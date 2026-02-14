package tree_sitter_command_cad_model_test

import (
	"testing"

	tree_sitter "github.com/smacker/go-tree-sitter"
	"github.com/tree-sitter/tree-sitter-command_cad_model"
)

func TestCanLoadGrammar(t *testing.T) {
	language := tree_sitter.NewLanguage(tree_sitter_command_cad_model.Language())
	if language == nil {
		t.Errorf("Error loading CommandCadModel grammar")
	}
}
