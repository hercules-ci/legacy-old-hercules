.. Hercules documentation master file, created by
   sphinx-quickstart on Thu Jan  5 19:53:43 2017.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

Welcome to Hercules's documentation!
====================================

:Author: Domen Kožar <domen@enlambda.com>, Joe Hermaszewski <git@monoid.al>
:Source code: `github.com/hercules-ci/hercules <https://github.com/hercules-ci/hercules.git>`_
:License: BSD3
:Version: |release|


.. sidebar:: Features

    - RESTful API `Haskell Servant <http://haskell-servant.readthedocs.io/en/stable/>`_
    - Separate frontend written in `Elm <http://elm-lang.org/>`_
    - Github integration (Authentication and Pull Requests) 
    - Declarative configuration using a YAML file


.. topic:: Introduction

   Continuous Integration for Nix projects.

   Hercules uses the same DB schema as Hydra, but a new Haskell backend with a RESTful API and Elm as new frontend.
   
   The goal of the MVP is to run Hercules as a CI for Github.


Documentation
=============

.. toctree::
   :maxdepth: 2

   getting-started.rst
   faq.rst
   api.rst


Indices and tables
==================

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`

