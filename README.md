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
- **random_heaps**: provides functions to generate random symbolic heaps.

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

## Running the examples

The examples present in the paper can be executed as follows:
```bash
dune exec main
```

### Generating random examples

It is possible to execute two types of random experiments:

* `random1` generates two random symbolic heaps independently and applies full bi-abduction to them.
* `random2` generates two related symbolic heaps with a specified degree of similarity. This makes it possible to produce larger examples while controlling the amount of structure shared by the two heaps.

The experiments can be executed as follows:

```bash
dune exec random1 -- <v_size> <p_size1> <s_size1> <p_size2> <s_size2> [Options]
```

and

```bash
dune exec random2 -- <v_size> <p_size> <s_size> <similarity> [Options]
```

The parameters have the following meaning:

* `v_size`: number of variables available when generating the heaps;
* `p_size1`, `p_size2`: number of pure predicates in the first and second heap, respectively;
* `s_size1`, `s_size2`: number of spatial predicates in the first and second heap, respectively;
* `p_size`: number of pure predicates in each heap;
* `s_size`: number of spatial predicates in each heap;
* `similarity`: degree of similarity between the two generated heaps. It must be a value between `0` and `1`, where larger values result in a greater amount of shared structure.

For example,

```bash
dune exec random2 -- 100 5 5 0.5
```

generates a pair of related heaps using 100 variables, 5 pure predicates and 5 spatial predicates, with a similarity value of 0.5, and then applies the full bi-abduction procedure to them.

#### Options

The `--time` option can be used with both ```random1```and ```random2```to display the execution time of the full bi-abduction procedure:

The `--verbose` option can be used with both ```random1```and ```random2```to display the antiframe and frame inferred by the full bi-abduction procedure. This option is particularly useful for small examples, where the generated heaps and the inferred solution can be inspected directly. For larger examples, the random experiments are primarily intended to exercise the prototype on symbolic heaps of increasing size, for which displaying the complete result may be impractical.

The `--original` option is available only for ```random2```. It displays the original symbolic heap from which the two input heaps used by the full bi-abduction procedure are generated.


## Testing
```bash
dune test
```

## Citation

If you use this software in academic work, please cite:
*Yet to be published*
