FAQ
===

Q: What problems is it going to solve?
**************************************

Continuous Integration for `Nix <https://nixos.org/nix/>`_.

Q: Is it going to replace travis on nixpkgs?
********************************************

On the long run this might be a logical step,
but current focus is providing Pull Requests testing
for Nix projects in general.

Q: How is task scheduling going to work?
********************************************

For 1.0 same as in Hydra.

Q: Is it going to be able to merge PRs?
********************************************

Not in 1.0, maybe later on.

Q: Is it going to simplify the deployment of the CI?
****************************************************

Yes. There will be a NixOS module with releases.

Q: It there a mode that merges the code before running the tests?
*****************************************************************

That will be the default.

Q: How is the DB provisioned without hydra?
********************************************

Not determined yet, there `will be a command <https://github.com/hercules-ci/hercules/issues/13>`_ eventually.

Q: What does Eelco think of this project, how likely is it that hydra will be replaced by it?
*********************************************************************************************

No official word out yet. It's too soon.
