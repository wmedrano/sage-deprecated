use ratatui::style::Color;

/// Contains a collection of colors used for theming.
#[derive(Copy, Clone)]
pub struct Theme {
    pub black1: Color,
    pub black2: Color,
    pub black3: Color,
    pub red: Color,
    pub green: Color,
    pub yellow: Color,
    pub blue: Color,
    pub purple: Color,
    pub cyan: Color,
    pub white1: Color,
    pub white2: Color,
    pub white3: Color,
}

/// Based on https://onedarktheme.com/.
pub const ONEDARK_THEME: Theme = Theme {
    black1: Color::Rgb(33, 37, 43),
    black2: Color::Rgb(40, 44, 52),
    black3: Color::Rgb(50, 56, 66),
    red: Color::Rgb(224, 108, 117),
    green: Color::Rgb(152, 195, 121),
    yellow: Color::Rgb(229, 192, 123),
    blue: Color::Rgb(97, 175, 239),
    purple: Color::Rgb(198, 120, 221),
    cyan: Color::Rgb(86, 182, 194),
    white1: Color::Rgb(171, 178, 191),
    white2: Color::Rgb(212, 216, 223),
    white3: Color::Rgb(246, 247, 249),
};
