#![allow(unstable_name_collisions)]
use css::Color;
use style::Pixels;

pub struct Canvas {
    pub pixels: Vec<Color>,
    pub width: usize,
    pub height: usize,
}

/// Paint a display list to an array of pixels.
pub fn paint_display_list(display_list: &DisplayList, width: usize, height: usize) -> Canvas {
    let mut canvas = Canvas::new(width, height);
    for item in display_list {
        canvas.paint_item(&item);
    }
    canvas
}

#[derive(Debug)]
pub enum DisplayCommand {
    SolidColor {
        color: Color,
        x: Pixels,
        y: Pixels,
        width: Pixels,
        height: Pixels
    },
}

pub type DisplayList = Vec<DisplayCommand>;

impl Canvas {
    /// Create a blank canvas
    fn new(width: usize, height: usize) -> Canvas {
        let white = Color { r: 255, g: 255, b: 255, a: 255 };
        Canvas {
            pixels: vec![white; width * height],
            width: width,
            height: height,
        }
    }

    fn paint_item(&mut self, item: &DisplayCommand) {
        match *item {
            DisplayCommand::SolidColor { color, x, y, width, height } => {
                // Clip the rectangle to the canvas boundaries.
                let x0 = x.clamp(0.0, self.width as f32) as usize;
                let y0 = y.clamp(0.0, self.height as f32) as usize;
                let x1 = (x + width).clamp(0.0, self.width as f32) as usize;
                let y1 = (y + height).clamp(0.0, self.height as f32) as usize;
                for y in y0 .. y1 {
                    for x in x0 .. x1 {
                        let i = y * self.width + x;
                        self.pixels[i] = color.over(&self.pixels[i]);
                    }
                }
            }
        }
    }
}

trait Clamp {
    fn clamp(self, lower: Self, upper: Self) -> Self;
}
impl Clamp for Pixels {
    fn clamp(self, lower: Pixels, upper: Pixels) -> Pixels {
        self.max(lower).min(upper)
    }
}
