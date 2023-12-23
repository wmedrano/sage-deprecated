use std::sync::OnceLock;

use flashkick::Scm;

static CACHE_CELL: OnceLock<ScmObjCache> = OnceLock::new();

/// An object containing many common scheme objects.
pub struct ScmObjCache {
    pub symbols: Symbols,
}

pub struct Symbols {
    pub char: Scm,
    pub error: Scm,
    pub event_type: Scm,
    pub key_press: Scm,
}

unsafe impl Send for Symbols {}
unsafe impl Sync for Symbols {}

impl ScmObjCache {
    /// Get the singleton reference to all the objects.
    pub unsafe fn singleton() -> &'static ScmObjCache {
        CACHE_CELL.get_or_init(|| unsafe { Self::init() })
    }

    unsafe fn init() -> ScmObjCache {
        ScmObjCache {
            symbols: Symbols {
                char: Scm::new_symbol("char"),
                error: Scm::new_symbol("willy-error"),
                event_type: Scm::new_symbol("event-type"),
                key_press: Scm::new_symbol("key-press"),
            },
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
