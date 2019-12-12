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
├── app            -- Executable used to run the checkers
├── src
│   ├── Checkers   -- Static and symbolic checkers
│   ├── Control    -- Logging helpers
│   ├── LLVMAST    -- LLVM AST interface
│   ├── InternalIR -- Internal IR used to represent paths for both static and symex
│   ├── Static     -- Static checker DSL
│   └── Symex      -- Symbolic DSL and execution engine
└── test           -- Tests
```

