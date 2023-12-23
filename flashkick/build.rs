extern crate bindgen;
extern crate pkg_config;

use std::env;
use std::path::PathBuf;

fn main() {
    let conf = pkg_config::probe_library("guile-3.0").expect("Could not find guile3 dev library.");
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rustc-link-lib=guile-3.0");
    println!("cargo:rerun-if-changed=wrapper.h");
    let mut builder = bindgen::Builder::default()
        .header("wrapper.h")
        .generate_inline_functions(true)
        .generate_comments(true);
    for p in conf.include_paths {
        builder = builder.clang_arg(format!("-I{}", p.to_str().unwrap()));
    }
    let bindings = builder
        .parse_callbacks(Box::new(bindgen::CargoCallbacks))
        .generate()
        .expect("Unable to generate bindings");
    let out_path = PathBuf::from(env::var("OUT_DIR").unwrap());
    bindings
        .write_to_file(out_path.join("bindings.rs"))
        .expect("Couldn't write bindings!");
}
