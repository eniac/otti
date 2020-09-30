# Compiler to circuit representations

Currently we have frontends for:
- C
- circom

We have backends for:
- R1CS (proofs via libsnark)
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

# Configuration

There is a configuration system in `src/Util/Cfg.hs`. You can add new
configuration options there. Configuration options can be set by environmental
variables, like so:

```
C_cfg_with_underscores_instead_of_dashes=value stack run -- ...
```

One particularly useful configuration options is `C_streams`, which enables
logging streams. You can log to named streams using `logIf` in
`src/Util/Log.hs`, and the output will only appear if the specified stream is
enabled.

# Directory structure

```
├── app               -- Compiler executable
├── src
│   ├── AST           -- Circom AST and C AST helpers 
│   ├── Codegen       -- Machinery for generating circuits 
│   │   ├── Circify   -- Language-indended machinery (branches, fns, scopes)
│   │   │   └── Memory-- Stack allocations and accesses
│   │   ├── Circom    -- Circom
│   │   └── C         -- C
│   ├── IR            -- The typed SMT intermediate representation
│   │   ├── SMT       -- Logging
│   │   │   ├── TySmt -- Sort-typed SMT terms
│   │   │   ├── Assert-- Monad for accumulating SMT assertions
│   │   │   ├── Opt   -- Optimizations over SMT
│   │   │   └── ToPf  -- Converting SMT to R1cs
│   │   └── R1cs      -- R1cs
│   │       └── Opt   -- Optimizations over R1cs
│   ├── Parser        -- Machinery for parsing source files 
│   ├── Targets       -- TBD: Alex after code changes
│   └── Util          -- Utilities (e.g., logging)
│       ├── Log       -- Logging
│       └── Cfg       -- Configuration
└── test              -- Tests
```

