use anyhow::Result;
use app::App;

pub mod app;
pub mod buffer;
pub mod widgets;

fn main() -> Result<()> {
    let mut app = App::new()?;
    loop {
        app.render()?;
        app.handle_events()?;
    }
}
