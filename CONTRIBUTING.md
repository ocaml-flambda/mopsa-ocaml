Contributing
============

Contributions to MOPSA are welcome.

This document describes the technical aspects of how to contribute: the branching model of MOPSA and how contributions are merged into the code base.
It also describes the legal aspects (licensing and contributor agreement).


License
--------

Unless explicitly specified, the components of the MOPSA software are distributed under the terms of the GNU Lesser General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
See the accompanying LICENSE file, or [http://www.gnu.org/licenses/](http://www.gnu.org/licenses/).

The documentation and example files of the MOPSA software are distributed under a Creative Commons Attribution-ShareAlike 4.0 International License. See [https://creativecommons.org/licenses/by-sa/4.0/](https://creativecommons.org/licenses/by-sa/4.0/).

The authors of the contributions must obey the term of our non-exclusive Contributor License Agreement, generated from [ContributorAgreements.org](http://contributoragreements.org/) and locally available as [CLA-Individual](CLA-Individual.pdf) (for individual contributors) and [CLA-Entity](CLA-Entity.pdf) (for entities).
For non-trivial contributions to be included in the main Mopsa repository, we require the authors to sign the relevant agreement. See the [instructions to sign the forms](http://mopsa.lip6.fr/contributing.html).

In any contribution, the license notice at the head of each file must remain intact, and new files should include the license notice.
Please keep the [AUTHORS](AUTHORS) file up to date.



Stable Branches
---------------
The repository of MOPSA has two protected and stable branches: 
- There is one stable branch `master` containing the latest public release of MOPSA.
- The development branch `dev` contains the latest stable features of the analyzer that are planned for the next release.


External Contributions
----------------------
To contribute to MOPSA, it is adviced to fork https://gitlab.com/mopsa/mopsa in a separate repository and work on a separate branch `<project>/dev`.

To remain synchronized with the upstream MOPSA repository, add a new remote to your local repository:
```shell
$ git remote add upstream git@gitlab.com:mopsa/mopsa.git
```
The branch `dev` should be read-only, fetched regularly from upstream and merged into local project branches:
```shell
$ git checkout dev
$ git pull upstream dev
$ git checkout my-project/dev
$ git merge dev
```

To integrate your contributions into the upstream project branch `<project>/dev`, create a merge request using GitLab web interface and select `mopsa/mopsa/<project>/dev` as the target branch.


Feature Branches
----------------
Daily work is generally done in *feature branches*. For internal contributions, features will likely be merged into `dev` and are named `feature/<category>/<feature name>`. For example:
```shell
$ git checkout dev
$ git checkout -b feature/c/handle-union-assignment
```

After committing code and testing it, create a merge request to add the feature. Alternatively, the feature can be merged directly into `dev`:
```shell
$ git checkout dev
$ git pull origin dev
$ git merge --no-ff feature/c/handle-union-assignment
$ git push origin dev
$ git branch -d feature/c/handle-union-assignment
$ git push origin :feature/c/handle-union-assignment
```

Sometimes, one would like to clean the commit history of feature branch before merging into `dev`. This can be done using `git rebase -i` **before** checking out `dev` and merging into it.


Remain Synchronized
--------------------
When features are merged into `dev`, **all existing** branches should pull these changes to remain up to date:
```shell
$ git checkout dev
$ git pull origin dev
$ git checkout my-project/dev
$ git merge dev
$ git checkout feature/my-feature
$ git merge dev
```

For external contributions, replace `origin` with `upstream`.

Release Branch
---------------
When features in `dev` are ready to be released, a new branch `release/<release id>` is created:
```shell
$ git checkout dev
$ git checkout -b release/1.2
```

Work is done in `release/<release id>` to polish the release (late bug fixes, documentation). When code is ready for the public release, the branch is merged into `master`:
```shell
$ git checkout master
$ git merge --no-ff release/1.2
$ git tag -a 1.2
$ git push origin master
$ git checkout dev
$ git merge master
$ git push origin dev
$ git branch -d release/1.2
$ git push origin :release/1.2
```

The last two commands are optional. They ensure that releases are immutable as they delete the brunch and only a tag is kept.
