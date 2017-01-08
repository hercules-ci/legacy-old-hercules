# Hercules

[![Build Status](https://travis-ci.org/hercules-ci/hercules.svg?branch=master)](https://travis-ci.org/hercules-ci/hercules)

Continuous Integration for Nix projects.

Hercules uses the same DB schema as [Hydra](http://www.nixos.org/hydra/),
but a new Haskell backend with a RESTful API and Elm as new frontend.

The goal of 1.0 milestone is to run Hercules as a CI for Github.

## Background

Nix needs better tooling for building, testing and deploying of Nix expressions.

Hydra has gone through many iterations, but it has become big and
hard to maintain (not many Nix developers do Perl).

Hercules goes quite far by using Servant as contract between the API,
docs and the frontend.

There should be minimal configuration to host Hercules and to
build Nix projects.

## Backend

### Getting started

You'll need Nix installed and [Hydra database loaded into Postgresql]
(https://github.com/peti/hydra-tutorial) for hydra user.

    $ cd backend
    $ nix-build
    $ ./result/bin/hercules -c example-config.yaml

### API

The API is described in [API.hs](backend/src/Hercules/API.hs).

## Frontend

### Getting started

Hot reloading is used for development, so you can start your development server:

    $ cd frontend
    $ nix-shell --run "npm i && npm run dev"

And open your browser and point it to http://localhost:3000

## License

Backend ([BSD3](backend/LICENSE)) / Frontend ([BSD3](frontend/elm-package.json))
