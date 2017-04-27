# Hercules

[![Build Status](https://travis-ci.org/hercules-ci/hercules.svg?branch=master)](https://travis-ci.org/hercules-ci/hercules)

Continuous Integration for Nix projects.

Hercules uses the same DB schema as [Hydra](http://www.nixos.org/hydra/),
but a new Haskell backend with a RESTful API and Elm as new frontend.

The goal of the 1.0 milestone is to run Hercules as a CI for Github Pull Requests.

## Background

Nix needs better tooling for building, testing and deploying of Nix expressions.

Hydra has gone through many iterations, but it has become big and
hard to maintain (not many Nix developers do Perl).

Hercules goes quite far by using Servant as contract between the API,
docs and the frontend.

There should be minimal configuration to host Hercules and to
build Nix projects.

## Documentation

- [Introduction](http://hercules-ci.s3-website-us-west-2.amazonaws.com/)
- [Getting started](http://hercules-ci.s3-website-us-west-2.amazonaws.com/getting-started.html)
- [FAQ](http://hercules-ci.s3-website-us-west-2.amazonaws.com/faq.html)
- [HTTP API](http://hercules-ci.s3-website-us-west-2.amazonaws.com/api.html)

## Status

Very WIP - not usable yet. See [#5](https://github.com/hercules-ci/hercules/issues/5) for progress report.

![Status](https://cloud.githubusercontent.com/assets/126339/21887274/9b0eabd4-d8bf-11e6-9aeb-5f87f54e002c.png)

## License

Backend ([BSD3](backend/LICENSE)) / Frontend ([BSD3](frontend/elm-package.json))
