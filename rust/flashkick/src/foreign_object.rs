use anyhow::{anyhow, Result};
use std::{any::TypeId, collections::HashMap, ffi::c_void, sync::Mutex};

use crate::{err::ResultToScm, ffi, Scm};

/// Allows converting between Rust structs and Scheme objects.
pub trait ForeignObjectType: 'static + Sized {
    /// The name of the type to show in Scheme.
    const NAME: &'static str;

    /// Initialize the type so that it is usable in Guile.
    ///
    /// # Safety
    /// Calls C code.
    unsafe fn init() {
        let name = Scm::new_symbol(Self::NAME);
        let slots = Scm::with_list(std::iter::once(Scm::new_symbol("ptr")));
        let scm_type =
            ffi::scm_make_foreign_object_type(name.0, slots.0, Some(drop_scm_object::<Self>));
        TYPE_ID_TO_SCM_TYPE.insert::<Self>(scm_type.into());
    }

    /// Convert the Rust object to a Guile object.
    ///
    /// # Safety
    /// Calls C code.
    unsafe fn to_scm(v: Box<Self>) -> Scm {
        let scm_type = TYPE_ID_TO_SCM_TYPE.get::<Self>().scm_unwrap();
        let ptr = Box::into_raw(v);
        let obj = ffi::scm_make_foreign_object_1(scm_type.0, ptr as *mut c_void);
        obj.into()
    }

    /// Get the underlying pointer to Self.
    ///
    /// # Safety
    /// Calls C code.
    unsafe fn ptr_from_scm(obj: Scm) -> *mut Self {
        let scm_type = TYPE_ID_TO_SCM_TYPE.get::<Self>().scm_unwrap();
        ffi::scm_assert_foreign_object_type(scm_type.0, obj.0);
        let ptr: *mut c_void = ffi::scm_foreign_object_ref(obj.0, 0);
        ptr.cast()
    }

    /// Gets the immutable reference from scheme.
    ///
    /// # Safety
    /// Calls C code. Additionally, the lifetime is static, but Scheme can't actually guarantee that
    /// there are no mutable references to the object.
    unsafe fn from_scm(obj: Scm) -> Option<&'static Self> {
        let scm_type = TYPE_ID_TO_SCM_TYPE.get::<Self>().scm_unwrap();
        ffi::scm_assert_foreign_object_type(scm_type.0, obj.0);
        let ptr: *const Self = ffi::scm_foreign_object_ref(obj.0, 0).cast();
        ptr.as_ref()
    }

    /// Gets the mutable reference from scheme.
    ///
    /// # Safety
    /// Calls C code. Additionally, the lifetime is static, but Scheme can't actually guarantee that
    /// there are no other mutable references to the object.
    unsafe fn from_scm_mut(obj: Scm) -> Option<&'static mut Self> {
        let scm_type = TYPE_ID_TO_SCM_TYPE.get::<Self>().scm_unwrap();
        ffi::scm_assert_foreign_object_type(scm_type.0, obj.0);
        let ptr: *mut Self = ffi::scm_foreign_object_ref(obj.0, 0).cast();
        ptr.as_mut()
    }

    /// Drop the Scheme object.
    ///
    /// # Safety
    /// Calls C code.
    unsafe fn scm_drop(obj: Scm) {
        drop_scm_object::<Self>(obj.0);
    }
}

unsafe extern "C" fn drop_scm_object<T: ForeignObjectType>(obj: ffi::SCM) {
    let ptr = T::ptr_from_scm(obj.into());
    if ptr.is_null() {
        return;
    }
    let b = Box::from_raw(ptr);
    ffi::scm_foreign_object_set_x(obj, 0, std::ptr::null_mut());
    drop(b);
}

static TYPE_ID_TO_SCM_TYPE: TypeIdToScmType = TypeIdToScmType::new();

#[derive(Debug)]
pub struct TypeIdToScmType {
    inner: Mutex<Option<HashMap<TypeId, Scm>>>,
}

impl TypeIdToScmType {
    const fn new() -> TypeIdToScmType {
        TypeIdToScmType {
            inner: Mutex::new(None),
        }
    }

    fn insert<T: 'static>(&self, scm_type: Scm) {
        let type_id = TypeId::of::<T>();
        let mut inner = self.inner.lock().unwrap();
        let mapping = inner.get_or_insert_with(HashMap::new);
        mapping.insert(type_id, scm_type);
    }

    fn get<T: 'static>(&self) -> Result<Scm> {
        let type_id = TypeId::of::<T>();
        let mut inner = self.inner.lock().unwrap();
        let mapping = inner.get_or_insert_with(HashMap::new);
        mapping.get(&type_id).cloned().ok_or_else(|| {
            anyhow!(
                "{type_name}::init() not called before attempting to use Scheme type. Type id was {type_id:?} and valid types are {mapping:?}",
                type_name = std::any::type_name::<T>()
            )
        })
    }
}

unsafe impl Send for TypeIdToScmType {}
unsafe impl Sync for TypeIdToScmType {}
