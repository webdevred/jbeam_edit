# JBeam Tool in Haskell

[![Build and test on Windows](https://github.com/webdevred/jbeam_edit/actions/workflows/build-and-test-windows.yaml/badge.svg?branch=master)](https://github.com/webdevred/jbeam_edit/actions/workflows/build-and-test-windows.yaml)
[![Build and test on Ubuntu](https://github.com/webdevred/jbeam_edit/actions/workflows/build-and-test-ubuntu.yaml/badge.svg?branch=master)](https://github.com/webdevred/jbeam_edit/actions/workflows/build-and-test-ubuntu.yaml)

<!--toc:start-->
- [JBeam Tool in Haskell](#jbeam-tool-in-haskell)
  - [Features](#features)
  - [Configuration](#configuration)
  - [Usage](#usage)
  - [Examples](#examples)
  - [Explanation of Source Code](#explanation-of-source-code)
  - [Planned Features / TODO](#planned-features-todo)
  - [Why Haskell + Megaparsec?](#why-haskell-megaparsec)
  - [Future Considerations](#future-considerations)
  - [Prerequisites](#prerequisites)
  - [Contributing & License](#contributing-license)
<!--toc:end-->

A fast, robust command-line parser, formatter, and editor for JBeam files, the JSON-like format used by BeamNG to define vehicles and physics structures.

## Features

- **Complete Parsing:**
  Parses entire `.jbeam` files, including comments and irregular whitespace, without breaking on manual edits.

- **Consistent Formatting:**
  Uniform indentation, spacing, and layout for improved readability.

- **Automatic Node Management:**
  Renames nodes sequentially (e.g., `["bf1", ...]` → `["bf0", "bf1", "bf2"]`) and updates all references automatically.

- **Configurable Formatting with JBFL:**
  Customize formatting rules using JBFL, a mini-language to specify padding, decimals, indentation, and more with wildcard targeting.

  Example JBFL snippet:

  ```jbfl
  .*.nodes[*][*] {
      PadDecimals: 3;
      PadAmount: 8;
  }

  .*.flexbodies[*] {
      NoComplexNewLine: true;
  }
  ```

## Configuration

Generate a default formatting config with:

```bash
jbeam-edit -cminimal   # simple config
jbeam-edit -ccomplex   # detailed config
```

Config files are saved in:

- Linux/macOS: `$HOME/.config/jbeam-edit/rules.jbfl`
- Windows: `%APPDATA%\jbeam-edit\rules.jbfl`

Override per project by placing `.jbeam_edit.jbfl` in your project root.

## Usage

Build and run:

```bash
git clone https://github.com/webdevred/jbeam-tool.git
cd jbeam-tool
stack build
stack exec jbeam-tool -- [options] <input-file>
```

Typical workflow:

- Parses and formats the file.
- Sorts and renames nodes, updating references.
- Writes output back with a `.bak` backup by default.

In-place editing (no backup):

```bash
jbeam-edit -i example.jbeam
```

## Examples

For sample `.jbeam` files and JBFL rule files, see the [Examples Directory README](examples/README.org).

## Explanation of Source Code

For an in-depth walkthrough of the implementation and design decisions, see [EXPLANATION_OF_SOURCE_CODE.org](EXPLANATION_OF_SOURCE_CODE.org).

## Planned Features / TODO

- Language Server Protocol (LSP) support
- Indent control in JBFL
- Automatic grouping of support vertices
- Meta node-aware sorting
- Prefix replacement via CLI arguments
- Expanded tests and example files
- Improved documentation

## Why Haskell + Megaparsec?

- Strong static typing for safer, reliable parsing and transformations.
- Elegant, composable parser combinators.
- Immutable data handling ensures predictable editing.

## Future Considerations

- Potential C/C++ port for easier Windows distribution.
- Trade-off: loss of Haskell’s expressiveness and safety vs. broader accessibility.

## Prerequisites

- GHC (The Glasgow Haskell Compiler)
- Stack build tool

## Contributing & License

Contributions and bug reports welcome. Licensed under BSD 3-Clause.

Happy parsing and formatting!

