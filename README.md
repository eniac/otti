# Compiler from C to circuit representations

Currently, the compiler goes from C to:
- R1CS
- SMT

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

# Format

We use `brittany`. You can format all files (slow) with `make fmt`.


You can format all files changed since a git-ref `REF` in a directory using
```
./scripts/format.bash REF DIR
```

You can format all files in a directory using

```
./scripts/format.bash all DIR
```

The formatting script will not format files which have unstaged changes.

# Run

For usage information:

```
stack exec compiler-exe -- -h
```

# Directory structure

```
├── app            -- Compiler executable 
├── src     
│   ├── AST        -- Circom AST and C AST helpers 
│   ├── Codegen    -- Machinery for generating circuits 
│   ├── IR         -- The typed SMT intermediate representation
│   ├── Parser     -- Machinery for parsing source files 
│   ├── Targets    -- TBD: Alex after code changes
│   └── Util       -- Utilities (e.g., logging)
└── test           -- Tests
```

