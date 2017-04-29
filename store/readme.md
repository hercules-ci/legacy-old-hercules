# Hercules Nix Store API Implementation

This directory contains an implementation of the nix-store API. This
implementation takes as a parameter a FIFO upon which to communicate.

reports when it is registered and when `buildPaths` is called.

## Building

Run `nix-build` in the repo root.

## Developing

- Run `nix-shell` in the repo root to drop to a shell with the required libraries present.
- Run `mkdir build` to create a directory for the build products.
- Run `cmake -Bbuild -H.` to generate the MakeFile.
- Run `cmake --build build` to build the library.

## Using

### On Linux

Assuming `libhercules-store.so` is in the current directory: run
`LD_PRELOAD=libhercules-store.so NIX_REMOTE=hercules:// nix-build`.

### On OS X

Assuming `libhercules-store.dylib` is in the current directory: run
`DYLD_INSERT_LIBRARIES=libhercules-store.dylib NIX_REMOTE=hercules:// nix-buld`.

### Forcing a local or remote store

- To force a local store run with `NIX_REMOTE=hercules://local`.
- To force a daemon accessed store run with `NIX_REMOTE=hercules://daemon`
