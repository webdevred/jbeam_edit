# AI Agent Instructions

## Critical Rules for AI Agents

### Commit and Pull Request Permissions

**IMPORTANT: AI agents working on this repository MUST follow these rules:**

1. **NEVER create commits without explicit user permission**
   - Do not use `git commit` commands
   - Do not use `git push` commands
   - Do not use any tools that automatically commit changes
   - Always wait for the user to explicitly grant permission before committing

2. **NEVER create pull requests without explicit user permission**
   - Do not use `gh pr create` or similar commands
   - Do not use any tools that automatically create PRs
   - Always wait for the user to explicitly grant permission before creating a PR

3. **NEVER use tools like `report_progress` that commit and push changes unless the user has explicitly granted permission**
   - These tools should only be used when the user has clearly stated they want changes committed
   - If uncertain whether permission has been granted, ask the user first

### When Permission IS Granted

Permission is considered granted only when the user:
- Explicitly says "commit these changes" or "create a PR"
- Explicitly says "yes, commit" or "yes, push"
- Explicitly instructs you to use tools that commit/push changes
- Provides clear, unambiguous approval to proceed with commits/PRs

### When Permission IS NOT Granted

Permission is NOT granted when:
- The user simply asks you to "make changes" or "fix an issue"
- The user asks you to "work on" or "implement" something
- The user provides a problem statement without explicitly mentioning commits/PRs
- There is any ambiguity about whether commits should be made

### Recommended Workflow

1. **Make changes** as requested by the user
2. **Show the changes** to the user (using `git diff`, `view`, etc.)
3. **Ask for permission** explicitly: "I've made the following changes. Would you like me to commit and push them?"
4. **Wait for explicit approval** before using any commit/push tools
5. **Only after approval**: Use tools to commit and push changes

### Protected Files - NEVER Modify Without Explicit Permission

**CRITICAL: The following build configuration files must NEVER be modified without explicit user permission:**

- **Cabal files**: `*.cabal`, `cabal.project*` (including `cabal.project`, `cabal.project.ci`, `cabal.project.dev`, `cabal.project.release`)
- **Stack files**: `stack.yaml`, `stack.yaml.lock`
- **Package configuration**: `package.yaml`

**Why these files are protected:**
- These files define the build system, dependencies, and project structure
- Incorrect modifications can break the build for all developers
- Changes require careful testing and review by project maintainers
- Dependency updates must be coordinated across the team

**Note:** The suggested GHC version for this project can be found in the `tested-with` key in the `jbeam-edit.cabal` file.

**If you need to modify these files:**
1. **STOP** and explicitly ask the user for permission first
2. Explain what changes you want to make and why
3. Wait for explicit approval before making any modifications
4. After approval, make minimal, surgical changes only

**Do NOT:**
- Add, remove, or update dependencies without permission
- Change GHC versions or resolver versions
- Modify build flags or compilation options
- Update Cabal file versions
- Regenerate `*.cabal` files from `package.yaml` without permission

### Protected Directory - examples/

**CRITICAL: The `examples/` directory must NEVER be modified without explicit user permission.**

- **Do NOT** edit, add, or remove any files in `examples/` unless the user has explicitly instructed you to do so.
- **Do NOT** make changes to `examples/` automatically, even to fix test failures or resolve build issues.

**If a test failure is caused by missing or out-of-date files in `examples/`:**
1. **STOP** — do not modify `examples/` unprompted.
2. Identify and report to the user which file(s) in `examples/` are causing the test failures.
3. Recommend possible solutions (e.g., regenerating files, updating specific entries).
4. **Wait for explicit user approval** before making any changes inside `examples/`.

**The only acceptable reasons to modify `examples/` are when the user explicitly asks to:**
- Improve documentation or onboarding materials
- Add new example files
- Update existing examples

**Do NOT:**
- Auto-regenerate or overwrite files in `examples/` during routine bug fixes or refactoring
- Silently update `examples/` as a side effect of any other change
- Propose or stage changes to `examples/` without user consent

### Exception Handling

- If you are working in a sandboxed environment specifically designed for automated commits (e.g., during automated testing), these rules may be relaxed
- If the repository has specific CI/CD workflows that require automated commits, document them here

## Summary

**Default behavior: DO NOT commit or push anything without explicit user permission.**

When in doubt, always ask the user first.
