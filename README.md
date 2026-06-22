# Full Bi-Abduction Prototype

This repository contains a reference implementation of the full bi-abduction algorithm for symbolic heap, described in Compositional backward shape analysis by means of bi-abduction.

## Structure of the project

The project is composed of two main modules:

### `symbolic_heap`
Defines all the necessary operations to work with symbolic heaps.
- **types**: defines the structure of symbolic heaps.
- **substitution**: provides all the operations regarding variables and their substitution.
- **symb_heap_ops**: provides all the useful operations to work with symbolic heaps.
- **consistency**: provides operations to reason about equalities in symbolic heaps.
- **formatting**: provides operations to pretty-print the previously defined structures.
- **utils**: provides useful function not regarding symbolic heaps.

### `full_biabduction`
Implements the main full bi-abductive procedure.
- **rules**: defines the rules of the full bi-abduction proof system.
- **fullbiabduction**: defines the central full bi-abductive procedure, executing the aforementioned rules.

The examples presented throughout the paper can be found in the **main** file.

## Prerequisites

The Ocaml compiler, along with its standard build utilities, are required.

- OCaml: 4.14.1 (or higher)
- OPAM: Package Manager
- Dune: 3.23.0 (or higher)

## Installation

First, clone the repository:
```bash
git clone https://github.com/cla25ber/full_biabduction.git
cd full_biabduction
```

Then, it is recommended to create a local sandbox to avoid conflicts with the global environment:
```bash
opam switch create . 4.14.1
eval $(opam env)
```

Using OPAM, it is possible to install automatically all the required project libraries:
```bash
opam install . --deps-only
```

### Compilation
```bash
dune build
```

### Running the example
```bash
dune exec main.exe
```

### Testing
```bash
dune test
```

## Citation

If you use this software in academic work, please cite:
*Yet to be published*
