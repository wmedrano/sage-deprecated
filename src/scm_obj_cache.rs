use std::sync::OnceLock;

use flashkick::Scm;

static CACHE_CELL: OnceLock<ScmObjCache> = OnceLock::new();

/// An object containing many common scheme objects.
pub struct ScmObjCache {
    pub error_sym: Scm,
}

unsafe impl Send for ScmObjCache {}
unsafe impl Sync for ScmObjCache {}

impl ScmObjCache {
    /// Get the singleton reference to all the objects.
    pub unsafe fn singleton() -> &'static ScmObjCache {
        CACHE_CELL.get_or_init(|| unsafe { Self::init() })
    }

    unsafe fn init() -> ScmObjCache {
        ScmObjCache {
            error_sym: Scm::new_symbol("willy-error"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn can_init_singleton() {
        unsafe { ScmObjCache::singleton() };
    }
}
