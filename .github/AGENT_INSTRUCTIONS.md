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

### Exception Handling

- If you are working in a sandboxed environment specifically designed for automated commits (e.g., during automated testing), these rules may be relaxed
- If the repository has specific CI/CD workflows that require automated commits, document them here

## Summary

**Default behavior: DO NOT commit or push anything without explicit user permission.**

When in doubt, always ask the user first.
