# Google Common Lisp Style Guide

Source: https://google.github.io/styleguide/lispguide.xml
Revision: 1.28

## Overview

The Google Common Lisp Style Guide provides recommendations for formatting and stylistic choices designed to enhance code readability and maintainability.

## Meta-Guide

### Guideline Levels (RFC 2119)

| Term | Meaning |
|------|---------|
| MUST/REQUIRED/SHALL | Absolute requirements |
| MUST NOT/SHALL NOT | Absolute prohibitions |
| SHOULD/RECOMMENDED | Valid reasons may exist to ignore |
| SHOULD NOT | Valid reasons may exist to do otherwise |
| MAY/OPTIONAL | Truly optional |

## General Guidelines

### Core Principles

1. Code must be easily understood by other developers
2. All code should follow consistent style
3. Be precise in implementation
4. Be concise in expression
5. Keep It Simple (KISS)
6. Use the smallest appropriate tool
7. Keep related code together

### Priority Order

1. Customer usability
2. Debuggability/testability
3. Readability/comprehensibility
4. Extensibility/modifiability
5. Lisp runtime efficiency

## Formatting

### Line Length

Keep lines under 100 characters.

### Indentation

- Use GNU Emacs with cl-indent (SLIME)
- Maintain consistent indentation throughout projects

```lisp
;; Good
(do-something first-argument
              second-argument
              #'(lambda (x) (frob x))
              fourth-argument
              last-argument)

;; Bad
(do-something first-argument second-argument (lambda (x)
    (frob x)) fourth-argument last-argument)
```

### Vertical White Space

- One blank line between top-level forms
- Use blank lines to separate logical sections

### Horizontal White Space

- No extra whitespace before/after parentheses
- Never place closing parentheses alone on a line
- Single space between forms
- Use 2-space indentation for bodies
- 4-space indentation for binding data

```lisp
;; Good
(defun factorial (limit)
  (let ((product 1))
    (loop for i from 1 upto limit
          do (setf product (* product i)))
    product))

;; Bad - closing parentheses separated
(defun factorial (limit)
  (let ((product 1))
    (loop for i from 1 upto limit
          do (setf product (* product i)))
    product
  )
)
```

### Tabs

Never use tabs. Configure editor: `(setq-default indent-tabs-mode nil)`

## Documentation

### Docstrings

Supply documentation strings for all visible functions, types, classes, variables, and macros.

```lisp
(defun small-prime-number-p (n)
  "Return T if N, an integer, is a prime number. Otherwise, return NIL."
  (cond ((or (< n 2)) nil)
        ((= n 2) t)
        ((divisorp 2 n) nil)
        (t (loop for i from 3 upto (sqrt n) by 2
                 never (divisorp i n)))))
```

### Comment Semicolons

| Count | Purpose |
|-------|---------|
| `;;;;` | File headers, large sections |
| `;;;` | Single top-level form or small groups |
| `;;` | Comments between lines within a form |
| `;` | End-of-line remarks |

Always include a space after semicolons.

### TODO Comments

Format: `TODO(identifier):` with YYYY-MM-DD dates.

```lisp
;;--- TODO(username): Refactor this after release 2025-01-15.
```

## Naming Conventions

### Symbols

- Use lowercase for all symbols
- Place hyphens between words
- Follow standard Common Lisp conventions

### Global Variables and Constants

```lisp
;; Constants: +plus-signs+
(defconstant +hash-results+ #xbd49d10d10cbee50)

;; Global variables: *earmuffs*
(defvar *maximum-search-depth* 100)
```

### Predicates

- Single word: `-P` suffix (e.g., `validp`)
- Multiple words: `-P` suffix (e.g., `valid-input-p`)

### Denote Intent, Not Content

Name variables according to their high-level concept, not implementation details.

```lisp
;; Good
(defun process-users (users) ...)

;; Bad - exposes implementation
(defun process-user-list (user-list) ...)
```

## Language Usage

### Mostly Functional Style

- Avoid modifying local variables; rebind instead
- Initialize slots during object creation
- Make classes as immutable as possible

### Special Variables

Use sparingly. Good candidates have "the current" as natural prefix.

### Assignment

Use `SETF` everywhere (not `SETQ`).

### Assertions and Conditions

| Construct | Use Case |
|-----------|----------|
| `ASSERT` | Internal bugs only |
| `CHECK-TYPE` | Type assertions |
| `ERROR` | User data/permission problems |

Always call `ERROR` with explicit condition types, never bare strings.

### Type Checking

- Use `CHECK-TYPE` in API functions
- Avoid `(declare (type ...))` for API functions
- Use declarations in internal low-level functions

### CLOS

- `DEFGENERIC` forms are mandatory for module entry points
- Avoid `SLOT-VALUE` and `WITH-SLOTS`; use accessors
- Do not use generics for overloading
- Never use MOP intercessory operations at runtime

```lisp
;; Good - explicit DEFGENERIC
(defgeneric process (object)
  (:documentation "Process the object."))

(defmethod process ((obj user))
  ...)

;; Bad - implicit generic
(defmethod process ((obj user))
  ...)
```

## Macros

### When to Use

- Never use a macro where a function will do
- Never transform functions to macros for performance (use `inline` instead)

### CALL-WITH Style

Limit macro to syntax processing; keep semantics in normal functions.

```lisp
;; Macro is thin wrapper
(defmacro with-foo (() &body body)
  `(call-with-foo (lambda () ,@body)))

;; Semantics in function
(defun call-with-foo (thunk)
  (setup-foo)
  (unwind-protect
      (funcall thunk)
    (teardown-foo)))
```

### Parameter Naming

- Use `-form` suffix for parameters that are Lisp forms to evaluate
- Exception: `body` and `end` are conventional

### WITH-xxx Macros

Always have parameter space for future extension:

```lisp
;; Good - allows future parameters
(defmacro with-lights-on (() &body body) ...)

;; Bad - no room for parameters
(defmacro with-lights-on (&body body) ...)
```

### Multiple Evaluation Prevention

Use `ALEXANDRIA:ONCE-ONLY` and `ALEXANDRIA:WITH-GENSYMS`.

## Prohibited Practices

### Never Use at Runtime

- `EVAL`
- `INTERN` / `UNINTERN`
- New reader macros (without consensus and `named-readtables`)

### Avoid

- `THROW` / `CATCH` (use restart facility)
- `SIGNAL` (use `ERROR` or `ASSERT`)
- Handling all conditions (type `T` or `IGNORE-ERRORS`)
- `NCONC` (use `APPEND`)
- `&ALLOW-OTHER-KEYS` (blurs function contracts)
- `&AUX` arguments
- `EQ` for numbers or characters (use `EQL`)

## Control Flow

### Conditional Expressions

```lisp
;; Single alternative
(when (valid-p x) (process x))
(unless (empty-p list) (first list))

;; Two alternatives
(if (ready-p) (start) (wait))

;; Several alternatives
(cond
  ((< n 0) 'negative)
  ((= n 0) 'zero)
  (t 'positive))
```

### CASE Forms

- Prefer `ECASE` and `ETYPECASE` to catch errors early
- Avoid `CCASE` and `CTYPECASE`
- Use `otherwise` instead of `t`
- Do not use gratuitous quotes

```lisp
;; Good
(ecase x
  ((bar) :bar)
  ((baz) :baz))

;; Bad - catches QUOTE, not bar
(case x
  ('bar :bar)
  ('baz :baz))
```

## Data Representation

### NIL Usage

| Meaning | Representation | Test |
|---------|----------------|------|
| False | `NIL` | `NOT` or `NULL` |
| Empty list | `'()` | `ENDP` or `NULL` |
| Unspecified | `NIL` or explicit symbol | - |

### Do Not Abuse Lists

Use lists only for sequential iteration over entire contents. For other cases:

| Need | Use |
|------|-----|
| Random access | Arrays |
| Sets | Balanced trees |
| Multiple values | `DEFSTRUCT` or `DEFCLASS` |

### Lists vs. Pairs

| Use Case | Functions |
|----------|-----------|
| Proper lists | `FIRST`, `REST`, `ENDP` |
| Pairs | `CAR`, `CDR`, `NULL` |

## Equality

| Predicate | Use Case |
|-----------|----------|
| `EQL` | Objects and symbols (default) |
| `CHAR=` | Case-dependent characters |
| `CHAR-EQUAL` | Case-ignoring characters |
| `STRING=` | Case-dependent strings |
| `STRING-EQUAL` | Case-ignoring strings |
| `=` | Numbers |
| `ZEROP`, `PLUSP`, `MINUSP` | Zero comparison |

Never use exact comparison on floating-point numbers.

## Iteration

- Use `DOLIST` or `DOTIMES` for simple cases
- Use `LOOP` when facilities like bindings, collection, or block return are needed

## Optimization

### Avoid Allocation

- Do not leave objects reachable after no longer needed
- Do not cons unnecessarily

### Unsafe Operations

Only use when:
1. Profiling indicates optimization need
2. Careful documentation explains safety

### DYNAMIC-EXTENT

Use only if:
1. Good reason for noticeable performance effect
2. Absolutely clear that assertions are true
3. Unlikely that code changes cause assertions to become false

### REDUCE vs APPLY

```lisp
;; Good
(reduce #'+ frobs :key #'acc :initial-value 0)

;; Bad - conses unnecessarily
(apply #'+ (mapcar #'acc frobs))
```

Never use `REDUCE` or `APPLY` with `STRCAT` or `APPEND` (O(n²)).

## Pathnames

- Use ASDF 3's `UIOP` for portable pathname handling
- Use `UIOP:MERGE-PATHNAMES*` or `UIOP:SUBPATHNAME` instead of `MERGE-PATHNAMES`
- Avoid `LOGICAL-PATHNAME` in portable code

## SATISFIES Types

Functions in `SATISFIES` clauses MUST:
- Accept any-type arguments
- Be defined within `EVAL-WHEN`

```lisp
;; Good
(deftype prime-number ()
  (and integer (satisfies prime-number-p)))

;; Bad - no type constraint
(deftype prime-number ()
  (satisfies prime-number-p))
```

## File Structure

```lisp
;;;; variable-length-encoding.lisp
;;;; Variable length encoding for integers and floating point numbers.

(in-package #:varint)
(declaim #.*optimize-default*)

;;; Section comment
;;; for related definitions

(defun encode-integer (n)
  "Encode N as a variable-length integer."
  ...)
```

## Package Design

- Each system typically has its own package namespace
- Never access internal symbols (`::`) in production code
- New packages should not be intended for `:use`

```lisp
(defpackage #:my-project
  (:use #:cl)
  (:import-from #:alexandria
                #:if-let
                #:when-let)
  (:export #:main
           #:run))
```
