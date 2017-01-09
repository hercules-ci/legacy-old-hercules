Getting Started
===============

Backend
*******

You'll need:

- `Nix installed <http://nixos.org/nix/download.html>`_
- `Hydra database loaded into Postgresql <https://github.com/peti/hydra-tutorial>`_ for hydra user


To build::

    $ cd backend
    $ nix-build
    $ ./result/bin/hercules -c example-config.yaml


Frontend
********

Hot reloading is used for development, so you can start your development server::

    $ cd frontend
    $ nix-shell --run "npm i && npm run dev"

And open your browser and point it to http://localhost:3000

