This is an optimizer for the scheme compiler in chapter 5 of
[Structure and Interpretation of Computer Programs](
https://mitpress.mit.edu/sicp/full-text/book/book.html). The target
language is the SICP register machine, also described in chapter 5.

## Usage

**Optimization**: Main function TBD

**Testing**: You can run tests with `test.scm`

The code for the compiler and register machine is in `/compiler`.

## Installation
This uses MIT/GNU Scheme, available here: https://www.gnu.org/software/mit-scheme/

To run a package, open scheme (`scheme` on the command line, or `M-x run-gesier` in emacs), and execute `load "<package.scm>"`.

## Design

### Architecture

The optimizer has four parts:

1. Constant folding for builtin operations
2. In-line constants for the registers `arg1`, `arg2`, `argl`, and `test`
3. Remove no-ops, unused labels, and unreachable code
4. Type inferencing, value inferencing, and branch analysis

### Runtime

Each run of the optimizer performs multiple passes on the source code. Every
pass transforms the code into an equivalent instruction set. The optimizer runs
repeatedly until the code reaches a fixed-point.

### Passes

Each pass is numbered, referring to the optimizer parts.

- [x] Branch analysis (4)
- [x] Drop unreachable code following gotos (3)
- [x] Drop tests not followed by a branch (3)
- [ ] Drop unused labels (3)
- [x] Fuse consecutive labels (3)
- [x] Drop gotos and branches that skip no code (3)
- [ ] Drop unread register assignments (3)
- [ ] Constant folding (1)
- [ ] Inline constants (2)
- [ ] Type inferencing (4)
- [ ] Value inferencing (4)
- [ ] Remove unused restore/save pairs (3)
- [ ] if -> goto for deterministic branches (1)
