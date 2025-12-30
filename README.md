# sample-clojure-make-git

A minimal Git implementation written in Clojure, designed as a learning project for understanding Git internals.

## Overview

This project implements core Git functionality from scratch in Clojure, providing a hands-on approach to learning how Git works under the hood. The codebase is organized into progressive sections, each building upon the previous one to add more features.

## Features

- **init** - Initialize a new Git repository
- **add** - Stage files to the index
- **commit** - Create commits with messages
- **ls-files** - List files in the staging area
- **hash-object** - Compute and store object hashes
- **cat-file** - Display contents of Git objects
- **update-index** - Manually update the index
- **write-tree** - Create tree objects from the index

## Requirements

- [Clojure](https://clojure.org/) 1.12+
- [GraalVM](https://www.graalvm.org/) (for native compilation)
- [Nix](https://nixos.org/) (optional, for development environment)

## Getting Started

### Using Nix (Recommended)

If you have Nix with flakes enabled:

```bash
# Enter the development shell
nix develop

# Navigate to a section
cd sections/section99

# Start a REPL
make repl
```

### Manual Setup

Ensure you have Clojure and GraalVM installed, then:

```bash
cd sections/section99

# Start a REPL
make repl

# Build a native binary
make native
```

## Project Structure

```
sections/
  section01/   # Basic hello world setup
  section02/   # Core Git operations (add, commit, ls-files)
  section99/   # Full implementation with all commands
```

Each section contains:
- `deps.edn` - Clojure dependencies
- `Makefile` - Build automation
- `src/mgit/mgit.clj` - Main implementation

## Usage

After building, you can use the `mgit` command:

```bash
# Initialize a repository
./target/mgit init

# Stage a file
./target/mgit add myfile.txt

# Create a commit
./target/mgit commit -m "Initial commit"

# List staged files
./target/mgit ls-files --stage

# Show help
./target/mgit help
```

## Dependencies

- [babashka/fs](https://github.com/babashka/fs) - File system utilities
- [clj-commons/digest](https://github.com/clj-commons/digest) - SHA-1 hashing
- [graal-build-time](https://github.com/clj-easy/graal-build-time) - GraalVM native image support

## License

This project is available as open source.
