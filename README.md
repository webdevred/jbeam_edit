# JBeam Tool in Haskell

[![Build & test (dev)](https://github.com/webdevred/jbeam_edit/actions/workflows/build-and-test.yaml/badge.svg)](https://github.com/webdevred/jbeam_edit/actions/workflows/build-and-test.yaml)
[![Build & test (latest GHC & deps)](https://github.com/webdevred/jbeam_edit/actions/workflows/future-proofing.yaml/badge.svg?branch=master&event=schedule)](https://github.com/webdevred/jbeam_edit/actions/workflows/future-proofing.yaml)
[![Build for release](https://github.com/webdevred/jbeam_edit/actions/workflows/build-and-release.yaml/badge.svg?branch=master)](https://github.com/webdevred/jbeam_edit/actions/workflows/build-and-release.yaml)
[![Lint & format](https://github.com/webdevred/jbeam_edit/actions/workflows/lint-and-format.yaml/badge.svg?branch=master)](https://github.com/webdevred/jbeam_edit/actions/workflows/lint-and-format.yaml)

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
  Renames nodes sequentially (e.g., `["bf1", ...]` → `["bf0", "bf1", "bf2"]`) and updates all references automatically. Feature currently unstable, enabled by build flag `transformation`.

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

### 1. Download the latest release
Go to the [Releases page](https://github.com/webdevred/jbeam_edit/releases) and download the most recent **`.zip`** file.

### 2. Unzip the file
Extract the contents of the downloaded `.zip` archive to a folder of your choice.

### 3. Run the setup as Administrator

Inside the extracted folder, right-click on **setup.exe** and choose **Run as administrator**.

## 4. Allow in Windows Defender if needed
If Windows shows a warning such as:

> *Windows protected your PC*

Click **More info** → **Run anyway**.

### 5. Path refresh
The installer adds `jbeam-edit` to your **PATH**, but:
- You must **open a new Command Prompt or PowerShell window** after installation.
- In some cases, you may need to **log out or restart Windows** for the PATH change to take effect.
- If it still doesn’t work, you can run it directly using the full path, e.g.:

```powershell
"C:\Program Files (x86)\jbeam_edit\jbeam-edit.exe" your-file.jbeam
```

### 6. Open Command Prompt or PowerShell
Press **`Win + R`**, type `cmd` or `powershell`, and hit **Enter**.

### 7. Run jbeam-edit on a file
Navigate to your project folder in CMD/PowerShell and run:

```powershell
jbeam-edit your-file.jbeam
```

Replace `your-file.jbeam` with the path to your JBeam file.

#### Typical workflow:
- Parses and formats the file.
- Sorts and renames nodes, updating references.
- Writes the output back with a `.bak` backup (default).

#### In-place editing (no backup):
```powershell
jbeam-edit -i example.jbeam
```

### From source (Linux or development)

Clone and build with Cabal:

```bash
git clone https://github.com/webdevred/jbeam_edit.git
cd jbeam_edit
cabal update
cabal install
jbeam-edit your-file.jbeam
```

## Examples

For sample `.jbeam` files and JBFL rule files, see the [Examples Directory README](examples/README.md).

## Planned Features / TODO

- Language Server Protocol (LSP) support
- Update specific ranges of vertices
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

- **Windows users (BeamNG players):** none, just download the installer from [Releases](https://github.com/webdevred/jbeam_edit/releases/latest).
- **Developers / Linux users:**
  - [GHC](https://www.haskell.org/ghc/) (Glasgow Haskell Compiler)
  - [Cabal](https://www.haskell.org/cabal/) build tool (comes with GHCup)

## Contributing & License

Contributions and bug reports welcome. Licensed under BSD 3-Clause.

Happy parsing and formatting!

