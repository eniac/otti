# Compiler: Name of compiler or some descriptor

# Install

## Install dependencies

- [z3](https://github.com/Z3Prover/z3)
- The Haskell tool [Stack](https://docs.haskellstack.org/en/stable/README/)

## Build project

Once the dependencies are installed, you should be able to build the compiler:

```
stack build
```

# Test

```
stack test
```

To run an individual test (e.g., C value test), use:

```
stack test --ta '-p C value test'
```


# Run

Eventually, you will run the compiler with some command line options using the command:

```
stack exec compiler
```

# Directory structure

