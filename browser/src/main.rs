//extern crate app_units;
extern crate getopts;
extern crate image;
extern crate itertools;

use std::fs;

pub mod css;
pub mod dom;
pub mod html;
pub mod layout;
pub mod lazy;
pub mod paint;
pub mod style;
pub mod utility;
pub mod user_agent;

const COMMAND_NAME: &str = "browser";
const COMMAND_HELP: &str = "
Expects that a benchmark is located at \"examples/bug/$NAME.{{html,css}}\".
Expects that a testcase is located at \"examples/sanity/$NAME.{{html,css}}\".

Since benchmark (--bench), testcase (--test), and manual (--html, --css) are
mutually exclusive modes of input, the program checks for each following a
priority order: first benchmark (--bench), second testcase (--test), third
manual (--html, --css), and fourth the default (\"examples/test.{{html,css}}\").
";

const DEFAULT_VIEWPORT_WIDTH: usize = 1280;
const DEFAULT_VIEWPORT_HEIGHT: usize = 703;
const DEFAULT_SCROLLBAR_WIDTH: usize = 0;
const DEFAULT_FONT_SIZE: usize = 16;
const DEFAULT_DOCUMENT: &str = "examples/test.html";
const DEFAULT_STYLESHEET: &str = "examples/test.css";
const DEFAULT_OUTPUT: &str = "output.png";


/// Initialize a recognizer for the command-line interface.
fn command_options() -> getopts::Options {
    let mut opts = getopts::Options::new();
    opts.optopt("t", "test", "Use inputs for named testcase", "BASENAME");
    opts.optopt("b", "bench", "Use inputs for named benchmark", "BASENAME");
    opts.optopt("d", "html", "HTML document", "FILENAME");
    opts.optopt("s", "css", "CSS stylesheet", "FILENAME");
    opts.optopt("o", "out", "PNG viewport", "PATH");
    opts.optopt("", "width", "Viewport width", "PIXELS");
    opts.optopt("", "height", "Viewport height", "PIXELS");
    opts.optopt("", "scrollbar", "Scrollbar width", "PIXELS");
    opts.optopt("", "font-size", "Font size", "POINTS");
    opts.optflag("", "dump-layout-tree", "Print Cassius layout tree");
    opts.optflag("h", "help", "Print this usage summary");
    opts
}

/// Indicate user's chosen inputs as a pair (HTML, CSS) of filenames.
fn select_input_filenames(args: &getopts::Matches) -> (String, String) {
    if let Some(benchmark) = args.opt_str("bench") {
        let html = format!("examples/bug/{}.html", benchmark);
        let css = format!("examples/bug/{}.css", benchmark);
        (html, css)
    } else if let Some(testcase) = args.opt_str("test") {
        let html = format!("examples/sanity/{}.html", testcase);
        let css = format!("examples/sanity/{}.css", testcase);
        (html, css)
    } else {
        let html = args.opt_str("html").unwrap_or(DEFAULT_DOCUMENT.into());
        let css = args.opt_str("css").unwrap_or(DEFAULT_STYLESHEET.into());
        (html, css)
    }
}

/// Get the user's chosen output filename.
fn select_output_filename(args: &getopts::Matches) -> String {
    args.opt_str("out").unwrap_or(DEFAULT_OUTPUT.into())
}

fn select_layout_parameters(args: &getopts::Matches) -> layout::Parameters {
    layout::Parameters {
        viewport_width:
            args.opt_get_default("width", DEFAULT_VIEWPORT_WIDTH)
                .expect("Viewport width (--width) is malformed"),
        viewport_height:
            args.opt_get_default("height", DEFAULT_VIEWPORT_HEIGHT)
                .expect("Viewport height (--height) is malformed"),
        scrollbar_width:
            args.opt_get_default("scrollbar", DEFAULT_SCROLLBAR_WIDTH)
                .expect("Scrollbar width (--scrollbar) is malformed"),
        font_size:
            args.opt_get_default("font-size", DEFAULT_FONT_SIZE)
                .expect("Font size (--font-size) is malformed"),
    }
}

fn main() {
    // Parse command-line options:
    let opts = command_options();
    let args = opts.parse(std::env::args().skip(1)).unwrap();

    // Print help message and quit, if so requested:
    if args.opt_present("help") {
        println!(
            "{}\nNotes:{}",
            opts.usage(&opts.short_usage(COMMAND_NAME)),
            COMMAND_HELP
        );
        return;
    }

    // Gather requisite command-line arguments:
    let (html_filename, css_filename) = select_input_filenames(&args);
    let png_filename = select_output_filename(&args);
    let layout_params = select_layout_parameters(&args);

    // Read input files:
    let html = fs::read_to_string(html_filename).unwrap();
    let css = fs::read_to_string(css_filename).unwrap();

    // Parse, style, layout, paint and raster:
    let document = html::parse_document(html);
    let stylesheet = css::parse(css);
    let style_tree = style::style_tree(&document, &stylesheet);
    let layout_tree = layout::layout_tree(&style_tree, layout_params);
    if args.opt_present("dump-layout-tree") {
        layout::print_layout(&layout_tree);
    }
    let display_list = layout::display_list(&layout_tree);
    let canvas = paint::paint_canvas(
        &display_list,
        layout_params.viewport_width,
        layout_params.viewport_height
    );
    let image = paint::buffer_image(&canvas);

    // Write image to output:
    if let Err(err) = image.save_with_format(png_filename, ::image::PNG) {
        println!("Error saving output image: {}", err)
    }
}
