# SatLite

SatLite is a purely functional CDCL-based SAT solver implemented in Haskell.

It was developed as my Master's research project at the University of Melbourne.

## Features

- Watched literals
- First-UIP conflict analysis
- Non-chronological backjumping
- VSIDS-like branching heuristic
- Restart mechanism
- Built-in DIMACS CNF parser

## Usage

```bash
cabal build
cabal run haskell-sat-solver -- Examples/*.cnf```

```markdown
Example CNF instances are provided in the `Examples/` directory.```