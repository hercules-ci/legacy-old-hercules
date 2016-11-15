# Hercules

A program to query a hydra database.

## Getting started

You'll need Nix installed and [Hydra database loaded into Postgresql]
(https://github.com/peti/hydra-tutorial) for hydra user.

   $ nix-build
   $ ./result/bin/hercules -c hercules.conf.sample

## API

The API is described in [API.hs](src/Hercules/API.hs).
