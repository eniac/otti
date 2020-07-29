# Compiler: Name of compiler or some descriptor

# Install

## Install dependencies

-

## Build project

Once the dependencies are installed, you should be able to build the compiler:

```
stack build
```

# Test

```
stack test
```

# Run

Eventually, you will run the compiler with some command line options using the command:

```
stack exec compiler
```

# Directory structure

```
├── app            -- Executable (checker or proof system compiler)
├── src
│   ├── AST        -- Syntax of input langauges
│   ├── Codegen    -- Semantics of input languages
│   ├── IR         -- Intermediate representations
│   │   ├── SMT    -- Typed SMT, including field equations
│   │   └── R1cs   -- rank-1 field equations
│   ├── Targets    -- SMT solver bindings
│   └── Util       -- General utilities (e.g. data structures)
└── test           -- Tests
```

