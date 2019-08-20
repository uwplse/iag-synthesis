//extern crate app_units;
extern crate getopts;
extern crate image;
extern crate itertools;

use std::fs;
use std::path::{Path, PathBuf};

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
Expects that any benchmark is located at \"examples/bug/$NAME.{{html,css}}\".
Expects that any testcase is located at \"examples/sanity/$NAME.{{html,css}}\".

The three input modes --- benchmark (--bench), testcase (--test), and manual
(--html,--css) --- are all mutually exclusive. If no input mode is given, the
default document and stylesheet (\"examples/test.{{html,css}}\") are used.

Cassius output is only supported in the benchmark and testcase input modes.
";

const DEFAULT_VIEWPORT_WIDTH: usize = 1280;
const DEFAULT_VIEWPORT_HEIGHT: usize = 703;
const DEFAULT_SCROLLBAR_WIDTH: usize = 0;
const DEFAULT_FONT_SIZE: usize = 16;

const CASSIUS_READ_ERR: &str = "Error reading Cassius input.";
const CASSIUS_WRITE_ERR: &str = "Error writing Cassius output.";
const CASSIUS_PROBLEM: &str = "

(define-problem doc-2
  :sheets default doc-1
  :fonts doc-1
  :documents doc-1
  :layouts doc-2)
";

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
    opts.optflag("v", "cassius", "Output Cassius file");
    opts.optflag("h", "help", "Print this usage summary");
    opts
}

fn path_to_file(args: &getopts::Matches, ext: &str) -> PathBuf {
    if let Some(manual) = args.opt_str(ext) {
        return PathBuf::from(manual);
    }

    let mut path = PathBuf::from("examples");
    if let Some(benchmark) = args.opt_str("bench") {
        path.push("bug");
        path.push(benchmark);
    } else if let Some(testcase) = args.opt_str("test") {
        path.push("sanity");
        path.push(testcase);
    } else { // The default case
        path.push("test");
    }
    path.set_extension(ext);
    path
}

fn path_to_html(args: &getopts::Matches) -> PathBuf {
    path_to_file(args, "html")
}

fn path_to_css(args: &getopts::Matches) -> PathBuf {
    path_to_file(args, "css")
}

fn path_to_png(args: &getopts::Matches) -> PathBuf {
    let mut path = path_to_file(args, "out");
    path.set_extension("png");
    path
}

fn layout_parameters(args: &getopts::Matches) -> layout::Parameters {
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

fn output_cassius_layout(html_path: &Path, layout_tree: &layout::LayoutTree) {
    let mut path = html_path.with_extension("rkt"); // TODO: Use ".cassius"
    let mut buffer = fs::read_to_string(&path).expect(CASSIUS_READ_ERR);

    // Append the layout entry and then the problem definition.
    buffer.push_str(layout_tree.to_string().as_str());
    buffer.push_str(CASSIUS_PROBLEM);

    path.set_extension("out.cassius");
    fs::write(&path, buffer).expect(CASSIUS_WRITE_ERR);
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
    let html_path = path_to_html(&args);
    let css_path = path_to_css(&args);
    let png_path = path_to_png(&args);
    let layout_params = layout_parameters(&args);

    // Read input files:
    let html = fs::read_to_string(&html_path).unwrap();
    let css = fs::read_to_string(&css_path).unwrap();

    // Parse, style, layout, paint and raster:
    let document = html::parse_document(html);
    let stylesheet = css::parse(css);
    let style_tree = style::style_tree(&document, &stylesheet);
    let layout_tree = layout::layout_tree(&style_tree, layout_params);
    let display_list = layout::display_list(&layout_tree);
    let canvas = paint::paint_canvas(
        &display_list,
        layout_params.viewport_width,
        layout_params.viewport_height
    );
    let image = paint::buffer_image(&canvas);

    if args.opt_present("dump-layout-tree") {
        println!("{}", layout_tree);
    }

    if args.opt_present("cassius") {
        output_cassius_layout(&html_path, &layout_tree);
    }

    // Write image to output:
    if let Err(err) = image.save_with_format(png_path, ::image::PNG) {
        eprintln!("Error saving output image: {}", err)
    }
}
