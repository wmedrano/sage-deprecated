include!(concat!(env!("OUT_DIR"), "/bindings.rs"));

#[macro_export]
macro_rules! scm_pack {
    ($x:expr) => {
        $x as $crate::ffi::SCM
    };
}

#[macro_export]
macro_rules! scm_make_itag8_bits {
    ($n:expr, $tag:expr) => {
        ($n << 8) + $tag
    };
}

#[macro_export]
macro_rules! scm_makiflag_bits {
    ($n:expr) => {
        $crate::scm_make_itag8_bits!($n, $crate::ffi::scm_tc8_tags_scm_tc8_flag)
    };
}

#[macro_export]
macro_rules! scm_unpack {
    // # if defined __DECC || defined __HP_cc
    ($x:expr) => {
        $x as $crate::ffi::scm_t_bits
    }; // Figure out what to do with the other path.
}

#[macro_export]
macro_rules! scm_matches_bits_in_common {
    ($x:expr, $a:expr, $b:expr) => {
        (($crate::scm_unpack!($x) & !($crate::scm_unpack!($a) ^ $crate::scm_unpack!($b)))
            == ($crate::scm_unpack!($a) & $crate::scm_unpack!($b)))
    };
}
