# Common Lisp Project Guidelines

## MCP Integration

This project uses **cl-mcp** for REPL-driven development. Available tools:

| Tool | Purpose |
|------|---------|
| `repl-eval` | Evaluate Lisp expressions |
| `lisp-read-file` | Read Lisp files with folding |
| `lisp-edit-form` | Structure-aware editing |
| `code-find` | Find symbol definitions |
| `code-describe` | Get symbol metadata |

### Important Rules
- Use `lisp-read-file` instead of `fs-read-file` for `.lisp` files
- Use `lisp-edit-form` instead of `fs-write-file` for existing code
- Always specify package context in `repl-eval`

## Required SubAgents

Use the following SubAgents for their corresponding tasks in this project:

| Task | SubAgent |
|------|----------|
| Codebase exploration and analysis | `cl-codebase-analyst` |
| Macro creation and verification | `cl-macro-verifier` |
| Test execution and analysis | `cl-test-runner` |
| Dependency management and research | `cl-dependency-resolver` |
| Code review and quality checks | `cl-linter` |

## Workflow Rules

### When Modifying Code
- Before changes: Use `cl-codebase-analyst` to assess the impact scope
- After changes: Use `cl-linter` to verify code quality

### Before Creating a PR
Run the following checks in parallel:
1. `cl-linter` - Quality check with mallet
2. `cl-test-runner` - Run Rove tests
3. `cl-codebase-analyst` - Verify dependency integrity
