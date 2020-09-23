Benchmark the same async program across Rust, OCaml, and FSharp.

# Benchmark overview

This is a benchmark to test what's the best language for implementing Dark in.
Dark already has [an
implementation](https://github.com/darklang/dark/blob/main/backend/libexecution/ast.ml),
but we are looking for improvements, especially around async.

The benchmark has two parts:

- fizzbuzz: using an interpreter connected to web server, dynamically calculate
  fizzbuzz and return it as a JSON response. This is to test the raw speed of
  the HTTP server and interpreter.

- fizzboom: like fizzbuzz, except instead of printing "fizzbuzz", the program makes
  a HTTP call that takes 1 second to complete. In Dark, users can write code
  that makes HTTP calls to arbitrary servers, and we must handle pessimistic
  cases like this.

In both cases, the most important metric is _requests/second_.

# Contributing

No-one likes to see their favorite language lose at benchmarks, so please feel
free to submit pull requests to improve existing benchmarks, or add new
variations (different web servers, new languages/frameworks, etc). Some rules:

- the interpreter must be easy to update, add to, and improve. As such, no
  microoptimizations, assembly code, JITs, etc. However, it is fine to:
  - add one-off fixes that for example, improve the compiler optimization
    settings, the webserver configuration, etc. Whatever you'd use for best
    performance in production is fine.
  - if the code had bad performance that's unfairly penalizing your language
    (eg due to a compiler bug), it's fine to propose alternatives
  - fix existing bad code (eg if data is being copied unnecessarily)
  - provide code review for existing implementations
- I can't imagine all the ways that people will try to game this, so I'm
  definitely going to reject things that don't support how we'd actually want
  to write Dark's backend. New rules will come as this happens.

## Overview of codebase

The benchmark is implemented in [measure.py](measure.py).

Each benchmark candidate is in its own directory, which has some known files:

- `./install.sh` - installs dependencies
- `./build.sh` - builds the server. This should use release configuration
- `./exe` - a symlink to the compiled executable
- `PORT` - the port to use to connect to the server
- `BROKEN` - if this file exists, skip the implementation in this directory

## Code of Conduct

Dark's community is held to the Dark [Code of Conduct](./CODE-OF-CONDUCT.md).
Benchmarks can be contentious, please be kind to all people involved.

## License

MIT
