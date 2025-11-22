<!--toc:start-->
- [Configuration Documentation: Value Formatting Rules](#configuration-documentation-value-formatting-rules)
- [Overview](#overview)
- [Pattern Syntax](#pattern-syntax)
- [Properties Overview](#properties-overview)
- [How Matching Works](#how-matching-works)
- [Detailed Rules Examples](#detailed-rules-examples)
  - [Pattern: `.*.nodes[*][*]`](#pattern-nodes)
  - [Pattern: `.*.beams[*][*]`](#pattern-beams)
- [Padding Behavior on Scalar Values](#padding-behavior-on-scalar-values)
- [Examples](#examples)
- [Summary Table](#summary-table)
- [Notes and Tips](#notes-and-tips)
<!--toc:end-->

# Configuration documentation: value formatting rules

This documentation describes the rule-based system for formatting values inside JBeam. It explains how to use pattern matching to target specific nodes and apply formatting settings during export.

- Example ruleset file: [minimal.jbfl](examples/jbfl/minimal.jbfl)
- Example input: [minimal.jbeam](examples/jbeam/minimal.jbeam)
- Example result after formatting: [fender-minimal-jbfl.jbeam](examples/formatted_jbeam/fender-minimal-jbfl.jbeam)

# Overview

This configuration system allows users to define formatting rules for values inside nested objects and arrays. Rules consist of:

- Patterns: that specify which data elements to match.
- Properties: that define how matched values are formatted.

Typical use cases include:

- Fixed-width data export
- Consistent floating-point number formatting
- Flattening nested arrays for output

# Pattern Syntax

| Pattern  | Description                                                                      |
|----------|----------------------------------------------------------------------------------|
| `.*`     | Matches **any key** at the current object level, regardless of name.             |
| `[*]`    | Matches **all elements** in a list (1D array).                                   |
| `[*][*]` | Matches **all elements in the innermost lists** of 2D arrays (arrays of arrays). |
| `.test`  | Matches the value with key `test` in an object.                                  |
| `[4]`    | Matches the value at index 4 in an array.                                        |

# Properties Overview

| Setting Name          | Description                                                                                                                                                              | Applies To                     |
|-----------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------|--------------------------------|
| `PadDecimals`         | Adds trailing zeros only if the fractional part is shorter than `PadDecimals`, leaving existing extra decimals untouched. Guarantees a minimum number of decimal digits. | Numeric values                 |
| `PadAmount`           | Specifies the **total length** (number of characters) the formatted value should occupy.                                                                                 | Any scalar except for comments |
| `NoComplexNewLine`    | When true, disables multiline or indented formatting for arrays, outputting values inline.                                                                               | Any complex data structure     |
| `ForceComplexNewLine` | Forces complex structures (arrays or objects) to always use multiline and indented formatting, even if NoComplexNewLine is not set. Overrides inline formatting.         | Any complex data structure     |
| `Indent`              | When set, controls the the amount of indentation. Defaults to 2 spaces.                                                                                                  | Any complex data structure     |

# How Matching Works

- Patterns traverse nested objects and arrays.
- `.*` matches all keys at the current level.
- `[*]` matches all elements of an array.
- Combinations like `.*.nodes[*][*]` match all elements inside inner lists under `nodes` keys.
- Properties apply **to each matched value individually**.
- Matching is agnostic to the data type; however, settings may behave differently based on type.
- ForceComplexNewLine ensures that matched complex structures are always output in multiline format with indentation.
- If both NoComplexNewLine and ForceComplexNewLine are set, ForceComplexNewLine takes precedence.

# Detailed Rules Examples

## Pattern: `.*.nodes[*][*]`

```jbfl
.*.nodes[*][*] {
    PadDecimals: 3;
    PadAmount: 8;
    NoComplexNewLine: true;
    Indent: 4;
}
```

- Matches all values in the innermost arrays under the key `nodes`, where:
  - The top-level element is an object with arbitrary keys.
  - Each key references an object containing a key named `nodes`.
  - The value of `nodes` is a 2D array (array of arrays).
  - The rule applies to all numeric values in the innermost arrays of this 2D array.
- Properties:
  - `PadDecimals: 3`
  - `PadAmount: 8`
  - `ForceComplexNewLine: true`
  - `Indent: 4`
- Behavior: Format floats as fixed-width strings of length 8, padding with trailing zeros after the decimal point, and always output nested arrays in multiline format with 4-space indentation.

Examples

| Original Value | Initial width | Exported String |
|----------------|---------------|-----------------|
| 1.2            | 3             | 1.200000        |
| 3.14           | 3             | 3.140000        |
| 12.0           | 3             | 12.00000        |

## Pattern: `.*.beams[*][*]`

```jbfl
.*.beams[*][*] {
    PadAmount: 8;
}
```

- Matches values in the innermost arrays under the key beams.
- Properties:
  - `PadAmount: 8`
- Behavior: Format floats as fixed-width strings of length 8, padded with leading spaces to align right.

Examples:

| Original Value | Initial width | Exported String                |
|----------------|---------------|--------------------------------|
| 5.0            | 3             | 5.0 with 7 spaces before       |
| 0.1234         | 6             | 0.1234 with 2 spaces before    |
| 7.89           | 4             | 7.89 with 5 spaces before      |

# Padding Behavior on Scalar Values

- Padding applies to **all scalar types** (numbers, strings, booleans).
- If the length of the representation of the scalar is **less than `PadAmount`**, the value is padded:
  - With trailing zeros **only if the fractional part has fewer digits than `PadDecimals`**. Existing extra decimals are left intact. This guarantees a **minimum number** of decimal digits.
  - With leading spaces otherwise.
- If the length is **equal to or greater than `PadAmount`**, **no padding or truncation occurs**; the full string is output as-is.

# Examples

| Value       | Initial width | `PadDecimals` | `PadAmount` | Output                     |
|-------------|---------------|---------------|-------------|----------------------------|
| 3.14        | 3             | 3             | 8           | 3.140                      |
| 3.14        | 3             | 0             | 8           | 3.14 with 4 spaces before  |
| "abc"       | 5             | 3             | 8           | "abc" with 3 spaces before |
| "abc"       | 5             | 0             | 8           | "abc" with 3 spaces before |
| true        | 4             | 0             | 6           | true with 2 spaces before  |
| 123456789.0 | 11            | 0             | 5           | 123456789.0                |

# Summary Table

| Pattern          | Targeted Data                   | Properties       | Padding Behavior                                       |
|------------------|---------------------------------|------------------|--------------------------------------------------------|
| `.*.nodes[*][*]` | Innermost float values in nodes | `PadDecimals: 3` | Trailing zeros so fractional part is at least 3 digits |
| `.*.beams[*][*]` | Innermost float values in beams | `PadAmount: 8`   | Leading spaces                                         |

# Notes and Tips

- Patterns are powerful and flexible; combine `.*`, `[*]`, and object keys to precisely target values.
- `PadDecimals` applies only to numbers, while `PadAmount` applies to all non-comment scalar values.
- String values receive space padding regardless of `PadDecimals`.
- Use `NoComplexNewLine` to simplify output layout when working with complex structures like lists and objects.
