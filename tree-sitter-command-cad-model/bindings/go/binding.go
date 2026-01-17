package tree_sitter_command_cad_model

// #cgo CFLAGS: -std=c11 -fPIC
// #include "../../src/parser.c"
// // NOTE: if your language has an external scanner, add it here.
import "C"

import "unsafe"

// Get the tree-sitter Language for this grammar.
func Language() unsafe.Pointer {
	return unsafe.Pointer(C.tree_sitter_command_cad_model())
}
