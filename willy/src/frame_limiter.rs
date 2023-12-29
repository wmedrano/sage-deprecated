use std::time::{Duration, Instant};

use flashkick::foreign_object::ForeignObjectType;

pub struct FrameLimiter {
    previous_time: Instant,
    target_duration: Duration,
}

impl ForeignObjectType for FrameLimiter {
    const NAME: &'static str = "willy-frame-limiter";
}

impl FrameLimiter {
    /// Create a new terminal UI with the given backend.
    pub fn new(target_fps: u16) -> FrameLimiter {
        let previous_time = Instant::now();
        let target_duration = if target_fps == 0 {
            Duration::ZERO
        } else {
            Duration::from_nanos(1_000_000_000 / target_fps as u64)
        };
        FrameLimiter {
            previous_time,
            target_duration,
        }
    }

    /// Limits the framerate. Returns if any frame rate limiting occurred.
    pub fn limit(&mut self) -> bool {
        let current_time = Instant::now();
        let target_time = self.previous_time + self.target_duration;
        if current_time < target_time {
            std::thread::sleep(target_time.duration_since(current_time));
            self.previous_time = Instant::now();
            true
        } else {
            self.previous_time = current_time;
            false
        }
    }
}
