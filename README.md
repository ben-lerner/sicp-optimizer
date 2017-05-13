This is an optimizer for the scheme compiler described in chapter 5 of
[Structure and Interpretation of Computer
Programs](https://mitpress.mit.edu/sicp/full-text/book/book.html). The target
language is the SICP register machine, also described in chapter 5.

**Status**: In Progress


## Usage

**Optimization**: Main function TBD

**Testing**: You can run tests with `test.scm`

The code for the compiler and register machine is in `/compiler`.

## Installation
This uses MIT/GNU Scheme, available here: https://www.gnu.org/software/mit-scheme/


## Design

### Architecture

The optimizer consists of four main parts:

1. Constant folding for builtin operations
2. In-lining constants for the registers `arg1`, `arg2`, `argl`, and `test`
3. Removing no-ops, unused labels, and unreachable code
4. Type/value inferencing and branch analysis to help with the above

### Runtime

Each run of the optimizer performs multiple passes on the source code. Every
pass transforms the code into valid, equivalent instructions. The optimizer is
run repeatedly until the code reaches a fixed-point.

### Passes

Each pass is numbered, referring to the high level goals above. 

- [x] Branch analysis (4)
- [x] Drop unreachable code following gotos (3)
- [x] Drop tests not followed by a branch (3)
- [x] Drop unused labels (3)
- [x] Drop gotos and branches that skip no code (3)
- [ ] Drop unread register assignments (3)
- [ ] Constant folding (1)
- [ ] Inline constants (2)
- [ ] Type and value inferencing (4)
