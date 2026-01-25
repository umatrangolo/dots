# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This is a dotfiles repository containing personal configuration files for development tools. The repository is intentionally minimal and stores configuration for:

- **Emacs**: Complete editor configuration with LSP support (via eglot)
- **Helix**: Modern terminal editor configuration
- **Git**: User-specific git settings
- **Tree-sitter**: Syntax highlighting and parsing configuration

## File Structure

- `dot.el.init` - Emacs initialization file (symlinked to `~/.emacs` or `~/.emacs.d/init.el`)
- `dot.gitconfig` - Git configuration (symlinked to `~/.gitconfig`)
- `helix/` - Helix editor configuration directory (symlinked to `~/.config/helix/`)
  - `config.toml` - Editor settings and keybindings
  - `languages.toml` - Language-specific tree-sitter grammar sources
  - `themes/simple.toml` - Custom "simple" color theme
- `tree-sitter/` - Tree-sitter CLI configuration
  - `config.json` - Parser directories and syntax highlighting theme

## Key Configuration Details

### Emacs (dot.el.init)

The Emacs configuration is set up for multi-language development with:

- **Tree-sitter modes** for: Go, Python, TypeScript, Scala, Java
- **LSP via eglot** with special configuration for Metals (Scala LSP server)
- **Auto-formatting on save** for Go (formats buffer and organizes imports)
- **Package management** via MELPA with key packages:
  - `scala-ts-mode` - Scala support with tree-sitter
  - `magit` - Git integration
  - `company` - Auto-completion
  - `eldoc-box` - Documentation in child frames
  - `diff-hl` - Git changes in fringe
  - `vertico` - Enhanced minibuffer completion
  - `git-timemachine` - Navigate through git history

**Metals LSP configuration**: Located at lines 105-107, uses specific Java options for optimal performance:
```elisp
(add-to-list 'eglot-server-programs
  `(scala-ts-mode . ("metals", "--java-opt", "-XX:+UseG1GC", "--java-opt", "-XX:+UseStringDeduplication", "--java-opt", "-Xss4m", "--java-opt", "-Xms100m", "--java-opt", "-Dmetals.client=emacs")))
```

### Helix (helix/)

The Helix configuration uses:
- Custom "simple" theme with minimal color palette
- Tree-sitter for indentation heuristics
- LSP with progress messages enabled, inlay hints disabled
- Editor preferences: 132 character text width, trailing whitespace trimming, cursorline highlighting

### Tree-sitter Grammars

Language grammars are pinned to specific versions in `helix/languages.toml`:
- Java: v0.23.5
- Python: v0.25.0
- Go: v0.25.0
- Scala: commit 97aead18d97708190a51d4f551ea9b05b60641c9

## Working with This Repository

**Installation**: These dotfiles are meant to be symlinked to their target locations:
- `ln -s ~/Development/dots/dot.el.init ~/.emacs`
- `ln -s ~/Development/dots/dot.gitconfig ~/.gitconfig`
- `ln -s ~/Development/dots/helix ~/.config/helix`

**Editing configurations**: Files should be edited in place. Changes take effect:
- Emacs: Requires restart or `M-x eval-buffer`
- Helix: Most settings apply immediately, some require `:config-reload`
- Git: Changes apply immediately to new commands

**Testing Emacs changes**: After editing `dot.el.init`, test by opening Emacs and checking for errors in the `*Messages*` buffer. The configuration assumes tree-sitter grammars are already installed.

**Color theme consistency**: The "simple" theme in both Helix and tree-sitter config.json share similar color choices (science blue for types, office green for strings, vivid orange for function calls).
