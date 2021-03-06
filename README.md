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
2. Remove no-ops, unused labels, and unreachable code
3. Type inferencing, value inferencing, and branch analysis
4. In-line constants for the registers `arg1`, `arg2`, `argl`, and `test`

### Runtime

Each run of the optimizer performs multiple passes on the source code. Every
pass transforms the code into an equivalent instruction set. The optimizer runs
repeatedly until the code reaches a fixed-point.

### Passes

Each pass is numbered, referring to the optimizer parts.

- [x] Branch analysis (3)
- [x] Drop unreachable code following gotos (2)
- [x] Drop tests not followed by a branch (2)
- [x] Drop unused labels (2) - not run in debug
- [x] Fuse consecutive labels (2) - not run in debug
- [x] Drop gotos and branches that skip no code (2)
- [ ] Drop unread register assignments and tests (2)
- [x] Constant folding (1)
- [ ] Inline constants (3)
- [ ] Type inferencing (3)
- [ ] Value inferencing (3)
- [ ] Remove unused save/restore pairs (2)
- [ ] branch -> goto for deterministic branches (1)


#### Debug Mode
Certain passes are skipped in debug mode. We assume that any label can be the
entry point for a debugger, so unreachable labels and the code after them aren't
considered dead code. We also assume that any register can be set to any value
after any label.

You should change 'important-registers if you need to inspect the contents of
some register during debugging. Writes to registers not in that list will be
elided if unused.

### Additional Optimizations

This doesn't do:

- Reordering of ops. We can analyze valid reorderings, but without a target CPU
  architecture there's no benefit to doing so.
- Constant folding for multiple inferred values. For example, ((op >) (reg a)
  (const 0)) can be folded if we know that (reg a) contains either 1
  or 2. Checking all possible values  will usually not work.
- Performance tuning of the optimizer. Scheme's built-in data structures leave a
  lot to be desired, and writing new ones is out of scope.
