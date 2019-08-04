extern crate getopts;
extern crate itertools;
extern crate image;

use std::io::BufWriter;
use std::fs::{self, File};

pub mod css;
pub mod dom;
pub mod html;
pub mod layout;
pub mod style;
pub mod paint;

const DEFAULT_WIDTH: usize = 1920;
const DEFAULT_HEIGHT: usize = 980;
const DEFAULT_DOCUMENT: &str = "examples/test.html";
const DEFAULT_STYLE: &str = "examples/test.css";
const DEFAULT_OUTPUT: &str = "output.png";

fn main() {
    // Parse command-line options:
    let mut opts = getopts::Options::new();
    opts.optopt("d", "html", "HTML document", "FILENAME");
    opts.optopt("s", "css", "CSS stylesheet", "FILENAME");
    opts.optopt("o", "out", "PNG viewport", "FILENAME");
    opts.optopt("w", "width", "Viewport width", "N");
    opts.optopt("h", "height", "Viewport height", "N");

    let matches = opts.parse(std::env::args().skip(1)).unwrap();
    let str_arg = |flag: &str, default: &str| -> String {
        matches.opt_str(flag).unwrap_or(String::from(default))
    };
    let num_arg = |flag: &str, default: usize| -> usize {
        matches.opt_get_default::<usize>(flag, default).unwrap()
    };

    // Read input files:
    let html = fs::read_to_string(str_arg("d", DEFAULT_DOCUMENT)).unwrap();
    let css  = fs::read_to_string(str_arg("s", DEFAULT_STYLE)).unwrap();

    // Configure viewport size:
    let width  = num_arg("w", DEFAULT_WIDTH);
    let height = num_arg("h", DEFAULT_HEIGHT);

    // Parsing and rendering:
    let root_node = html::parse(html);
    let stylesheet = css::parse(css);
    let style_root = style::style_tree(&root_node, &stylesheet);
    let layout_root = layout::layout_tree(&style_root, width, height);
    let display_list = layout::display_list(&layout_root);

    // Create the output file:
    let filename = str_arg("o", DEFAULT_OUTPUT);
    let mut file = BufWriter::new(File::create(&filename).unwrap());

    // Write to the file:
    let canvas = paint::paint_display_list(&display_list, width, height);
    let (w, h) = (canvas.width as u32, canvas.height as u32);
    let img = image::ImageBuffer::from_fn(w, h, move |x, y| {
        let color = canvas.pixels[(y * w + x) as usize];
        image::Pixel::from_channels(color.r, color.g, color.b, color.a)
    });
    if image::ImageRgba8(img).save(&mut file, image::PNG).is_ok() {
        println!("Saved output as {}", filename)
    } else {
        println!("Error saving output as {}", filename)
    }
}
