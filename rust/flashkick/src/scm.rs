use crate::ffi;

#[derive(Copy, Clone, Debug)]
#[repr(transparent)]
pub struct Scm(pub ffi::SCM);

impl Scm {
    /// Equivalent to `#t` in Scheme.
    pub const TRUE: Scm = Scm(crate::scm_pack!(crate::scm_makiflag_bits!(4)));

    /// Equivalent to `#f` in Scheme.
    pub const FALSE: Scm = Scm(crate::scm_pack!(crate::scm_makiflag_bits!(0)));

    /// Equivalent to ELisp Nil.
    pub const ELISP_NIL: Scm = Scm(crate::scm_pack!(crate::scm_makiflag_bits!(1)));

    /// Equivalent to `'()` in Scheme.
    pub const EOL: Scm = Scm(crate::scm_pack!(crate::scm_makiflag_bits!(3)));

    /// Undefined.
    pub const UNDEFINED: Scm = Scm(crate::scm_pack!(crate::scm_makiflag_bits!(9)));

    pub fn is_undefined(&self) -> bool {
        self.0 == Self::UNDEFINED.0
    }

    pub fn is_eol(&self) -> bool {
        self.0 == Self::EOL.0
    }

    /// Equivalent to (string? self).
    ///
    /// # Safety
    /// Makes calls to C.
    pub unsafe fn is_string(&self) -> bool {
        Scm::from(ffi::scm_string_p(self.0)).is_true()
    }

    /// Equivalent to (equal? self other).
    ///
    /// # Safety
    /// Makes calls to C.
    pub unsafe fn is_equal(&self, other: Scm) -> bool {
        Scm::from(ffi::scm_equal_p(self.0, other.0)).is_true()
    }

    /// Equivalent to (eq? self other).
    ///
    /// # Safety
    /// Makes calls to C.
    pub unsafe fn is_eq(&self, other: Scm) -> bool {
        Scm::from(ffi::scm_eq_p(self.0, other.0)).is_true()
    }

    /// Equivalent to (eqv? self other).
    ///
    /// # Safety
    /// Makes calls to C.
    pub unsafe fn is_eqv(&self, other: Scm) -> bool {
        Scm::from(ffi::scm_eqv_p(self.0, other.0)).is_true()
    }

    /// Equivalent to (number? self).
    ///
    /// # Safety
    /// Makes calls to C.
    pub unsafe fn is_number(&self) -> bool {
        Scm::from(ffi::scm_number_p(self.0)).is_true()
    }

    /// Equivalent to (char? self).
    ///
    /// # Safety
    /// Makes calls to C.
    pub unsafe fn is_char(&self) -> bool {
        Scm::from(ffi::scm_char_p(self.0)).is_true()
    }

    /// # Safety
    /// Makes calls to C.
    pub unsafe fn new_bool(b: bool) -> Scm {
        if b {
            Scm::TRUE
        } else {
            Scm::FALSE
        }
    }

    /// # Safety
    /// Makes calls to C.
    pub unsafe fn new_i8(x: i8) -> Scm {
        ffi::scm_from_int8(x).into()
    }

    /// # Safety
    /// Makes calls to C.
    pub unsafe fn new_u8(x: u8) -> Scm {
        ffi::scm_from_uint8(x).into()
    }

    /// # Safety
    /// Makes calls to C.
    pub unsafe fn new_i16(x: i16) -> Scm {
        ffi::scm_from_int16(x).into()
    }

    /// # Safety
    /// Makes calls to C.
    pub unsafe fn new_u16(x: u16) -> Scm {
        ffi::scm_from_uint16(x).into()
    }

    /// # Safety
    /// Makes calls to C.
    pub unsafe fn new_i32(x: i32) -> Scm {
        ffi::scm_from_int32(x).into()
    }

    /// # Safety
    /// Makes calls to C.
    pub unsafe fn new_u32(x: u32) -> Scm {
        ffi::scm_from_uint32(x).into()
    }

    /// # Safety
    /// Makes calls to C.
    pub unsafe fn new_i64(x: i64) -> Scm {
        ffi::scm_from_int64(x).into()
    }

    /// # Safety
    /// Makes calls to C.
    pub unsafe fn new_u64(x: u64) -> Scm {
        ffi::scm_from_uint64(x).into()
    }

    /// # Safety
    /// Makes calls to C.
    pub unsafe fn new_f64(x: f64) -> Scm {
        ffi::scm_from_double(x).into()
    }

    /// # Safety
    /// Makes calls to C.
    pub unsafe fn new_string<S: AsRef<str>>(s: S) -> Scm {
        let s = s.as_ref();
        ffi::scm_from_utf8_stringn(s.as_ptr() as _, s.len() as _).into()
    }

    /// # Safety
    /// Makes calls to C.
    pub unsafe fn new_symbol(s: &str) -> Scm {
        ffi::scm_from_utf8_symboln(s.as_ptr() as _, s.len() as _).into()
    }

    /// # Safety
    /// Makes calls to C.
    pub unsafe fn new_keyword(s: &str) -> Scm {
        // The Guile C API only provides a scm_from_utf8_keyword (NOT keywordn) so we have to
        // convert to a symbol. To work around this, we convert to a symbol. Note that Guile's
        // keyword implementation does the same thing anyways.
        ffi::scm_symbol_to_keyword(Self::new_symbol(s).0).into()
    }

    /// # Safety
    /// Makes calls to C.
    pub unsafe fn cons(x: Scm, y: Scm) -> Scm {
        ffi::scm_cons(x.0, y.0).into()
    }

    /// # Safety
    /// Makes calls to C.
    pub unsafe fn new_char(ch: char) -> Scm {
        ffi::scm_c_make_char(u32::from(ch) as i32).into()
    }

    /// # Safety
    /// Makes calls to C.
    pub unsafe fn to_char(self) -> Option<char> {
        let ch_scm_int: Scm = ffi::scm_char_to_integer(self.0).into();
        let ch_int = ch_scm_int.to_u32();
        char::from_u32(ch_int)
    }

    /// # Safety
    /// Makes calls to C.
    pub unsafe fn with_list<I>(iter: I) -> Scm
    where
        I: IntoIterator<Item = Scm>,
    {
        Scm::with_list_impl(iter.into_iter())
    }

    /// # Safety
    /// Makes calls to C.
    pub unsafe fn with_list_impl(mut iter: impl Iterator<Item = Scm>) -> Scm {
        match iter.next() {
            Some(head) => Scm::cons(head, Self::with_list_impl(iter)),
            None => Scm::EOL,
        }
    }

    /// # Safety
    /// Makes calls to C.
    pub unsafe fn with_alist<I>(iter: I) -> Scm
    where
        I: IntoIterator<Item = (Scm, Scm)>,
    {
        Self::with_list(iter.into_iter().map(|(x, y)| Self::cons(x, y)))
    }

    /// Returns true if `self` is truthy. In Guile, all values are truthy except for `#f` and Emacs
    /// Lisp's variant of `nil`.
    ///
    /// # Safety
    /// Makes calls to C.
    pub unsafe fn is_true(&self) -> bool {
        let is_false = crate::scm_matches_bits_in_common!(self.0, Scm::ELISP_NIL.0, Scm::FALSE.0);
        !is_false
    }

    /// Returns the value of the bool. If `self` is not a bool, then an error is raised.
    ///
    /// # Safety
    /// Makes calls to C.
    pub unsafe fn to_bool(&self) -> bool {
        ffi::scm_to_bool(self.0) != 0
    }

    /// # Safety
    /// Makes calls to C.
    pub unsafe fn to_i8(&self) -> i8 {
        ffi::scm_to_int8(self.0)
    }

    /// # Safety
    /// Makes calls to C.
    pub unsafe fn to_u8(&self) -> u8 {
        ffi::scm_to_uint8(self.0)
    }

    /// # Safety
    /// Makes calls to C.
    pub unsafe fn to_i16(&self) -> i16 {
        ffi::scm_to_int16(self.0)
    }

    /// # Safety
    /// Makes calls to C.
    pub unsafe fn to_u16(&self) -> u16 {
        ffi::scm_to_uint16(self.0)
    }

    /// # Safety
    /// Makes calls to C.
    pub unsafe fn to_i32(&self) -> i32 {
        ffi::scm_to_int32(self.0)
    }

    /// # Safety
    /// Makes calls to C.
    pub unsafe fn to_u32(&self) -> u32 {
        ffi::scm_to_uint32(self.0)
    }

    /// # Safety
    /// Makes calls to C.
    pub unsafe fn to_i64(&self) -> i64 {
        ffi::scm_to_int64(self.0)
    }

    /// # Safety
    /// Makes calls to C.
    pub unsafe fn to_u64(&self) -> u64 {
        ffi::scm_to_uint64(self.0)
    }

    /// # Safety
    /// Makes calls to C.
    pub unsafe fn to_f64(&self) -> f64 {
        ffi::scm_to_double(self.0)
    }

    /// # Safety
    /// Makes calls to C.
    #[allow(clippy::inherent_to_string)]
    pub unsafe fn to_string(&self) -> String {
        let mut length = 0;
        let ptr: *mut u8 = ffi::scm_to_utf8_stringn(self.0, &mut length).cast();
        String::from_raw_parts(ptr, length as _, length as _)
    }

    /// # Safety
    /// Makes calls to C.
    pub unsafe fn to_symbol(&self) -> String {
        let s: Scm = ffi::scm_symbol_to_string(self.0).into();
        s.to_string()
    }

    /// # Safety
    /// Makes calls to C.
    pub unsafe fn to_keyword(&self) -> String {
        let s: Scm = ffi::scm_keyword_to_symbol(self.0).into();
        s.to_symbol()
    }

    /// # Safety
    /// Makes calls to C.
    pub unsafe fn car(self) -> Scm {
        ffi::scm_car(self.0).into()
    }

    /// # Safety
    /// Makes calls to C.
    pub unsafe fn cdr(self) -> Scm {
        ffi::scm_cdr(self.0).into()
    }

    /// Get the length of the list. Equivalent to calling `(length self)` in Scheme.
    ///
    /// # Safety
    /// Makes calls to C.
    pub unsafe fn length(self) -> usize {
        let scm_len = Scm(unsafe { ffi::scm_length(self.0) });
        scm_len.to_u64() as usize
    }

    /// Iterate through elements if this is a list.
    ///
    /// # Safety
    /// Makes calls to C.
    pub unsafe fn iter(self) -> impl ExactSizeIterator + Iterator<Item = Scm> {
        let len = self.length();
        let mut lst = self;
        (0..len).map(move |_| {
            let v = lst.car();
            lst = lst.cdr();
            v
        })
    }

    /// Iterate through pair elements.
    ///
    /// # Safety
    /// Makes calls to C.
    pub unsafe fn iter_pairs(self) -> impl ExactSizeIterator + Iterator<Item = (Scm, Scm)> {
        self.iter().map(|pair| (pair.car(), pair.cdr()))
    }

    /// Get the give slot for the struct.
    ///
    /// # Safety
    /// Makes calls to C.
    pub unsafe fn struct_ref(self, field_idx: usize) -> Scm {
        let v = ffi::scm_struct_ref(self.0, Scm::new_u16(field_idx as u16).0);
        Scm(v)
    }

    /// Return the `k`th element of the list. Equivalent to calling `(list-ref self k)` in Scheme.
    ///
    /// # Safety
    /// Makes calls to C.
    pub unsafe fn list_ref(self, k: usize) -> Scm {
        let v = ffi::scm_list_ref(self.0, Scm::new_u32(k as u32).0);
        Scm(v)
    }

    /// Mark the SCM object as permanent and return the same object. The GC will never deallocate
    /// the object.
    ///
    /// # Safety
    /// Makes calls to C. Additionally, this should only be called at most once per object.
    pub unsafe fn permanent(self) -> Scm {
        ffi::scm_permanent_object(self.0);
        self
    }
}

impl From<ffi::SCM> for Scm {
    fn from(value: ffi::SCM) -> Self {
        Scm(value)
    }
}

#[cfg(test)]
mod tests {
    use crate::boot_guile;

    use super::*;

    #[test]
    fn car_returns_first_cell() {
        unsafe {
            boot_guile(std::iter::empty(), || {
                let a = Scm::new_i32(1);
                let b = Scm::new_i32(2);
                let c = Scm::cons(a, b);
                assert_eq!(c.car().to_i32(), 1i32);
            })
        };
    }

    #[test]
    fn cdr_returns_second_cell() {
        unsafe {
            boot_guile(std::iter::empty(), || {
                let a = Scm::new_i32(1);
                let b = Scm::new_i32(2);
                let c = Scm::cons(a, b);
                assert_eq!(c.cdr().to_i32(), 2i32);
            })
        }
    }

    #[test]
    fn type_conversions_are_equal_before_and_after() {
        unsafe {
            boot_guile(std::iter::empty(), || {
                // Bool
                assert!(Scm::new_bool(true).is_true());
                assert!(!Scm::new_bool(false).is_true());

                // Numbers
                assert_eq!(Scm::new_u8(1).to_u8(), 1);
                assert_eq!(Scm::new_i8(-2).to_i8(), -2);
                assert_eq!(Scm::new_u16(3).to_u16(), 3);
                assert_eq!(Scm::new_i16(-4).to_i16(), -4);
                assert_eq!(Scm::new_u32(5).to_u32(), 5);
                assert_eq!(Scm::new_i32(-6).to_i32(), -6);
                assert_eq!(Scm::new_u64(7).to_u64(), 7);
                assert_eq!(Scm::new_i64(-8).to_i64(), -8);
                assert_eq!(Scm::new_f64(9.5).to_f64(), 9.5);
                assert_eq!(Scm::new_f64(-10.5).to_f64(), -10.5);

                // Strings/Chars
                assert_eq!(
                    Scm::new_string("my string").to_string(),
                    "my string".to_string()
                );
                assert_eq!(Scm::new_char('a').to_char(), Some('a'));
                assert_eq!(Scm::new_char('\n').to_char(), Some('\n'));
                assert_eq!(Scm::new_char(char::MAX).to_char(), Some(char::MAX));
                assert_eq!(
                    Scm::new_char(char::REPLACEMENT_CHARACTER).to_char(),
                    Some(char::REPLACEMENT_CHARACTER)
                );

                // Symbols/Keywords
                assert_eq!(
                    Scm::new_symbol("my symbol").to_symbol(),
                    "my symbol".to_string()
                );
                assert_eq!(
                    Scm::new_keyword("my keyword").to_keyword(),
                    "my keyword".to_string()
                );
                assert_eq!(
                    Scm::with_list([100, 101, 102, 103, 104, 105].map(|i| { Scm::new_u32(i) }))
                        .iter()
                        .map(|scm| scm.to_u32())
                        .collect::<Vec<u32>>(),
                    vec![100, 101, 102, 103, 104, 105],
                );
            })
        }
    }

    #[test]
    fn scheme_falsey_values() {
        unsafe {
            boot_guile(std::iter::empty(), || {
                assert!(!Scm::new_bool(false).is_true());
                assert!(!Scm::FALSE.is_true());
                assert!(!Scm::ELISP_NIL.is_true());
            });
        }
    }

    #[test]
    fn scheme_truthy_values() {
        unsafe {
            boot_guile(std::iter::empty(), || {
                assert!(Scm::TRUE.is_true());
                assert!(Scm::EOL.is_true());
                assert!(Scm::UNDEFINED.is_true());
                assert!(Scm::new_u8(1).is_true());
                assert!(Scm::new_i8(-2).is_true());
                assert!(Scm::new_u16(3).is_true());
                assert!(Scm::new_i16(-4).is_true());
                assert!(Scm::new_u32(5).is_true());
                assert!(Scm::new_i32(-6).is_true());
                assert!(Scm::new_u64(7).is_true());
                assert!(Scm::new_i64(-8).is_true());
                assert!(Scm::new_f64(9.5).is_true());
                assert!(Scm::new_f64(-10.5).is_true());
                assert!(Scm::new_string("").is_true());
                assert!(Scm::new_char(char::MAX).is_true());
            });
        }
    }

    #[test]
    fn with_list_on_empty_iter_returns_eol() {
        unsafe {
            boot_guile(std::iter::empty(), || {
                assert!(Scm::with_list(std::iter::empty()).is_eol());
            })
        }
    }
}
