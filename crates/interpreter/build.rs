// use std::path::{Path, PathBuf};
// use std::{env, fs};
// use type_sitter_gen::{generate_nodes, generate_queries, super_nodes};
//
// fn main() {
//     // Common setup
//     let out_dir = PathBuf::from(env::var_os("OUT_DIR").unwrap());
//     println!("cargo::rerun-if-changed=build.rs");
//
//     let parser_path = "../tree-sitter-command-cad-model";
//
//     // Obligatory: in this and future lines, replace `vendor/path/to/tree-sitter-foobar-lang`
//     // with the path to your grammar's folder, relative to the folder containing `Cargo.toml`
//     println!("cargo::rerun-if-changed={parser_path}");
//
//     let parser_path = Path::new(parser_path);
//
//     // To generate nodes
//     let path = parser_path.join("src/node-types.json");
//     fs::write(
//         out_dir.join("nodes.rs"),
//         generate_nodes(path).unwrap().into_string(),
//     )
//     .unwrap();
//
//     // To generate queries
//     fs::write(
//         out_dir.join("queries.rs"),
//         generate_queries(
//             parser_path.join("queries"),
//             parser_path,
//             // Replace with a different `syn::Path` if the nodes don't exist in a subling to `dest_path` named `nodes`
//             &super_nodes(),
//             // Replace with `true` if you are using the `yak-sitter` feature (by default, no)
//             false,
//         )
//         .unwrap()
//         .into_string(),
//     )
//     .unwrap();
// }
use std::path::PathBuf;
use std::{env, fs};
use type_sitter_gen::generate_nodes;

fn main() {
    // Common setup. Same as before
    let out_dir = PathBuf::from(env::var_os("OUT_DIR").unwrap());
    println!("cargo::rerun-if-changed=build.rs");

    // Obligatory: in this and future lines, replace `vendor/path/to/tree-sitter-foobar-lang`
    // with the path to your grammar's folder, relative to the folder containing `Cargo.toml`
    println!("cargo::rerun-if-changed=vendor/path/to/tree-sitter-foobar-lang");

    // To generate nodes
    fs::write(
        out_dir.join("nodes.rs"),
        generate_nodes(tree_sitter_command_cad_model::NODE_TYPES)
            .unwrap()
            .into_string(),
    )
    .unwrap();
}
