use crate::utility::{Color, Edge, Pixels, Rect};
use image;

#[derive(Clone, PartialEq, Debug)]
pub enum DisplayCommand {
    SolidColor(Color, Rect<Pixels>),
}
use DisplayCommand::SolidColor;

#[derive(Clone, PartialEq, Debug)]
pub struct DisplayList(Vec<DisplayCommand>);

impl AsRef<Vec<DisplayCommand>> for DisplayList {
    fn as_ref(&self) -> &Vec<DisplayCommand> {
        &self.0
    }
}

impl AsMut<Vec<DisplayCommand>> for DisplayList {
    fn as_mut(&mut self) -> &mut Vec<DisplayCommand> {
        &mut self.0
    }
}

impl std::ops::Deref for DisplayList {
    type Target = Vec<DisplayCommand>;

    fn deref(&self) -> &Self::Target {
        self.as_ref()
    }
}

impl DisplayList {
    pub fn new() -> Self {
        DisplayList(Vec::new())
    }

    pub fn push_command(&mut self, command: DisplayCommand) {
        self.as_mut().push(command);
    }

    pub fn display_block(&mut self, color: Color, block: Rect<Pixels>) {
        self.push_command(SolidColor(color, block));
    }

    pub fn display_frame(&mut self, color: Color, frame: Edge<Rect<Pixels>>) {
        self.push_command(SolidColor(color, frame.left));
        self.push_command(SolidColor(color, frame.right));
        self.push_command(SolidColor(color, frame.top));
        self.push_command(SolidColor(color, frame.bottom));
    }
}

pub struct Canvas {
    pub pixels: Vec<Color>,
    pub width: usize,
    pub height: usize,
}

impl Canvas {
    /// Create a blank canvas.
    pub fn new(width: usize, height: usize) -> Canvas {
        let white = Color {
            r: 255,
            g: 255,
            b: 255,
            a: 255,
        };
        Canvas {
            pixels: vec![white; width * height],
            width: width,
            height: height,
        }
    }

    /// Get the canvas size in both dimensions.
    pub fn size(&self) -> (usize, usize) {
        (self.width, self.height)
    }

    /// Calculate the index into the canvas buffer for a pixel co-ordinate.
    pub fn index_in_buffer(&self, x: usize, y: usize) -> usize {
        y * self.width + x
    }
}

impl std::ops::Index<(usize, usize)> for Canvas {
    type Output = Color;

    fn index(&self, (x, y): (usize, usize)) -> &Self::Output {
        let i = self.index_in_buffer(x, y);
        self.pixels.index(i)
    }
}

impl std::ops::IndexMut<(usize, usize)> for Canvas {
    fn index_mut(&mut self, (x, y): (usize, usize)) -> &mut Self::Output {
        let i = self.index_in_buffer(x, y);
        self.pixels.index_mut(i)
    }
}

impl Canvas {
    /// Draw the output of a `DisplayCommand` onto this canvas.
    pub fn paint_command(&mut self, command: &DisplayCommand) {
        let clip = |px: Pixels| px.max(0.0).min(self.width as f32).round() as usize;
        match command {
            SolidColor(color, rect) => {
                // Clip the rectangle to the canvas boundaries.
                let (origin, bound) = rect.transform(clip).diagonal();
                for y in origin.y..=bound.y {
                    for x in origin.x..=bound.x {
                        self[(x, y)] = color.over(&self[(x, y)]);
                    }
                }
            }
        }
    }

    /// Fill a raster image buffer with the current canvas buffer.
    pub fn as_image(&self) -> image::DynamicImage {
        let (w, h) = (self.width as u32, self.height as u32);
        let buffer = image::ImageBuffer::from_fn(w, h, |x, y| {
            let pixel = self[(x as usize, y as usize)];
            image::Pixel::from_channels(pixel.r, pixel.g, pixel.b, pixel.a)
        });
        image::ImageRgba8(buffer)
    }
}

/// Paint a display list to an array of pixels.
pub fn paint_canvas(display_list: &DisplayList, width: usize, height: usize) -> Canvas {
    let mut canvas = Canvas::new(width, height);
    for command in display_list.iter() {
        canvas.paint_command(&command);
    }
    canvas
}

/// Fill a raster image buffer from the painted canvas.
pub fn buffer_image(canvas: &Canvas) -> image::DynamicImage {
    canvas.as_image()
}
