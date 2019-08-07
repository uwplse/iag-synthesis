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

const COMMAND_NAME: &str = "browser";

const DEFAULT_WIDTH: usize = 1280;
const DEFAULT_HEIGHT: usize = 703;
const DEFAULT_FONT_SIZE: usize = 16;
const DEFAULT_SCROLLBAR: usize = 0;
const DEFAULT_DOCUMENT: &str = "examples/test.html";
const DEFAULT_STYLESHEET: &str = "examples/test.css";
const DEFAULT_OUTPUT: &str = "output.png";

/// Build the recognizer for the program's command-line options.
fn program_options() -> getopts::Options {
    let mut opts = getopts::Options::new();
    opts.optopt(
        "",
        "test",
        "Run on input files of so-named testcase",
        "NAME",
    );
    opts.optopt(
        "",
        "bench",
        "Run on input files of so-named benchmark",
        "NAME",
    );
    opts.optflag("l", "dump-layout-tree", "Print Cassius layout tree");
    opts.optopt("d", "html", "HTML document", "PATH");
    opts.optopt("s", "css", "CSS stylesheet", "PATH");
    opts.optopt("o", "out", "PNG viewport", "PATH");
    opts.optopt("", "width", "Viewport width", "PX");
    opts.optopt("", "height", "Viewport height", "PX");
    opts.optopt("", "scrollbar", "Scrollbar thickness", "PX");
    opts.optopt("", "font-size", "Font size", "PT");
    opts.optflag("h", "help", "Print this usage summary");
    opts
}

/// Print a help message for the command-line interface.
fn print_help(opts: &getopts::Options) {
    println!("{}", opts.usage(&opts.short_usage(COMMAND_NAME)));
    println!(
        "Notes:

Expects that a benchmark is located at \"examples/bug/$NAME.{{html,css}}\".
Expects that a testcase is located at \"examples/sanity/$NAME.{{html,css}}\".

Since benchmark (--bench), testcase (--test), and manual (--html, --css) are
mutually exclusive modes of input, the program checks for each following a
priority order: first benchmark (--bench), second testcase (--test), third
manual (--html, --css), and fourth the default (\"examples/test.{{html,css}}\").
"
    );
}

/// Return user's selected (HTML, CSS) filenames as such a pair.
fn select_input(args: &getopts::Matches) -> (String, String) {
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

fn main() {
    // Parse command-line options:
    let opts = program_options();
    let args = opts.parse(std::env::args().skip(1)).unwrap();

    // Print help message and quit, if so requested:
    if args.opt_present("help") {
        print_help(&opts);
        return;
    }

    // Read input files:
    let (html_filename, css_filename) = select_input(&args);
    let html = fs::read_to_string(html_filename).unwrap();
    let css = fs::read_to_string(css_filename).unwrap();

    // Configure viewport dimensions:
    let width = args.opt_get_default("width", DEFAULT_WIDTH).unwrap();
    let height = args.opt_get_default("height", DEFAULT_HEIGHT).unwrap();
    let font_size = args
        .opt_get_default("font-size", DEFAULT_FONT_SIZE)
        .unwrap();
    let scrollbar = args
        .opt_get_default("scrollbar", DEFAULT_SCROLLBAR)
        .unwrap();

    // Parse, style, layout, paint and raster:
    let document = html::parse_document(html);
    let stylesheet = css::parse(css);
    let style_tree = style::style_tree(&document, &stylesheet);
    let layout_tree = layout::layout_tree(&style_tree, width, height, font_size, scrollbar);
    if args.opt_present("dump-layout-tree") {
        layout::print_layout(&layout_tree);
    }
    let display_list = layout::display_list(&layout_tree);
    let canvas = paint::paint_canvas(&display_list, width, height);
    let image = paint::buffer_image(&canvas);

    // Write image to output:
    let png_filename = args.opt_str("out").unwrap_or(DEFAULT_OUTPUT.into());
    if let Err(err) = image.save_with_format(png_filename, ::image::PNG) {
        println!("Error saving output image: {}", err)
    }
}
