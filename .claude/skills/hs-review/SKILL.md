---
name: hs-review
description: Code review skill for Haskell. Reviews changed or specified files for correctness, style, types, error handling, and idiomatic Haskell. Aware of this project's toolchain (fourmolu, HLint, Cabal).
argument-hint: "[file or directory]"
allowed-tools: Bash, Glob, Grep, Read
---

# /hs-review

You are performing a Haskell code review. Be direct and specific. Reference file paths and line numbers. Focus on things that matter — skip cosmetic nitpicks that fourmolu or HLint would catch automatically.

**Project context:**
- Cabal, fourmolu for formatting, HLint for linting
- Extensions and warnings defined in `package.yaml`
- Flag-gated modules under `src-extra/` (transformation, language-server)
- Tests: HSpec, run with `cabal test --project-file=cabal.project.dev`

---

## Step 1 — Determine scope

If `$ARGUMENTS` is non-empty, review those files/directories.

Otherwise, find what has changed:
```bash
git diff --name-only HEAD
git diff --name-only --cached
```

Filter to `.hs` files only. If nothing changed, ask the user what to review.

---

## Step 2 — Run the tools

```bash
fourmolu --mode check src/ src-extra/ test/
hlint src/ src-extra/
cabal build --project-file=cabal.project.dev 2>&1
```

Note any failures. A build warning is a review finding.

---

## Step 3 — Read the files

Read each file under review. For context, also read files it imports from within the project if they are relevant to understanding the changes.

When a file defines or modifies a function, type, or constructor: **find all call sites** (grep for the name across the codebase). A change to a type or constructor is incomplete until every construction and consumption site is verified.

---

## Step 4 — Review

Evaluate each file against these categories. Only report findings that apply — skip categories with nothing to say.

### Correctness
- Partial functions (`head`, `tail`, `fromJust`, `error`, `undefined`) — are they safe here? Is there a total alternative?
- Pattern match coverage — are all cases handled, or is exhaustiveness suppressed with a wildcard that hides bugs?
- Off-by-one, wrong order of arguments, flipped comparisons
- `IO` actions that can fail silently (ignoring `Left`, discarding exceptions)
- Unsafe coercions or `unsafePerformIO`
- **`show` for output**: `show` on types like `Scientific`, `Double`, `Float` produces Haskell-native representations (e.g. `2.0e-3`) that are wrong for user-facing or serialized output. When `show` is used to produce `Text` that ends up in formatted output, verify the representation matches the domain (e.g. JBeam expects `0.002`, not `2.0e-3`). Prefer explicit formatting functions like `formatScientific Fixed Nothing`.

### Types and abstractions
- Is the type precise enough? Could a `Text` be a newtype? Could a `Bool` be a sum type?
- Are invariants enforced by the type, or only by convention?
- Unnecessary use of `Any`, `Dynamic`, or overly general constraints

### Consistency
- When a type has smart constructors (`mk*` functions), verify **all construction sites** use them and that the constructors maintain invariants. A smart constructor that silently violates the invariant it's supposed to enforce is worse than no smart constructor.
- When multiple functions produce the same kind of output (e.g. two functions that both produce a normalized text representation of a number), verify they use the **same logic**. Duplicated formatting logic will diverge. If one is the canonical implementation, the other should call it or share its core.
- When a type's `Show` output is used for round-tripping (via `Read`), verify that the `Show`/`Read` instances still round-trip correctly after the change. This project uses `Show`/`Read` for AST fixtures in `examples/ast/`.

### Error handling
- `Either`/`Maybe` chains — are errors propagated or swallowed?
- `fail` in `IO` vs structured errors — is the choice intentional?
- Are error messages informative (include context, not just "error")?

### Performance
- Unnecessary `String` where `Text` or `ByteString` fits
- `O(n)` operations in tight loops (list indexing, repeated `length`)
- Lazy vs strict — is the default appropriate here? Any risk of space leaks?

### Idiomatic Haskell
- Can a manual recursion be replaced with a fold, traverse, or standard combinator?
- Redundant `do`-notation where `<$>`/`<*>`/`>>=` is cleaner
- `where` clauses that are large enough to deserve top-level definitions
- Imports — are qualified imports consistent with the rest of the codebase?

### Tests
- For code in `src/` or `test/`: are new functions covered by unit tests?
- For code in `src-extra/` (flag-gated, experimental): do not flag missing unit tests — note it is experimental code. Integration/end-to-end tests (fixture-based, via `cabal test`) are still relevant and worth mentioning if clearly absent.
- Do existing tests still make sense after the change?
- Are test descriptions accurate?
- **New properties/features**: when a new JBFL property or formatting feature is added, check whether the dynamic integration tests in `FormattingSpec.hs` cover it (they pair every `examples/jbeam/*.jbeam` with every `examples/jbfl/*.jbfl`). If no example file exercises the new feature, flag the gap.

---

## Step 5 — Report

Structure your output as follows. Omit sections with no findings.

```
## Haskell code review

### Build / tooling
<fourmolu/HLint/build issues>

### Findings

**[Severity] file.hs:LINE — Short title**
Description. What the problem is, why it matters, and a concrete suggestion.

...

### Summary
<1–3 sentences: overall assessment and most important thing to address>
```

Severity levels: **Critical** (correctness bug), **Warning** (likely problem), **Suggestion** (improvement worth considering), **Note** (observation, no action needed).

If there are no findings, say so clearly.
