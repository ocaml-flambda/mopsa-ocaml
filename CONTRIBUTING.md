Contributing
============

Contributions to MOPSA are welcome.

This document describes the technical aspects of how to contribute: the branching model of MOPSA and how contributions are merged into the code base.
It also describes the legal aspects (licensing).


License
--------

Unless explicitly specified, the components of the MOPSA software are distributed under the terms of the GNU Lesser General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
See the accompanying LICENSE file, or [http://www.gnu.org/licenses/](http://www.gnu.org/licenses/).

The documentation and example files of the MOPSA software are distributed under a Creative Commons Attribution-ShareAlike 4.0 International License. See [https://creativecommons.org/licenses/by-sa/4.0/](https://creativecommons.org/licenses/by-sa/4.0/).

In any contribution, the license notice at the head of each file must remain intact, and new files should include the license notice.
Please keep the [AUTHORS](AUTHORS.md) file up to date and in alphabetical order. For each contributor include all the affiliations from the times of contribution and an up-to-date email contact.


Main Branch
---------------

The repository of MOPSA has a stable branch, `main`, containing the latest public release of MOPSA.


External Contributions
----------------------
To contribute to MOPSA, it is advised to fork https://gitlab.com/mopsa/mopsa-analyzer in a separate repository and work on a separate branch `<project>/main`.

To remain synchronized with the upstream MOPSA repository, add a new remote to your local repository:
```shell
$ git remote add upstream git@gitlab.com:mopsa/mopsa-analyzer.git
```
The branch `main` should be read-only, fetched regularly from upstream and merged into local project branches:
```shell
$ git checkout main
$ git pull upstream main
$ git checkout my-project/main
$ git merge main
```

To integrate your contributions into the upstream project branch `<project>/main`, create a merge request using GitLab web interface and select `mopsa/mopsa-analyzer/<project>/main` as the target branch.


Feature Branches
----------------
Daily work is generally done in *feature branches*. For internal contributions, features will likely be merged into `main` and are named `feature/<category>/<feature name>`. For example:
```shell
$ git checkout main
$ git checkout -b feature/c/handle-union-assignment
```

After committing code and testing it, create a merge request to add the feature. Alternatively, the feature can be merged directly into `main`:
```shell
$ git checkout main
$ git pull origin main
$ git merge --no-ff feature/c/handle-union-assignment
$ git push origin main
$ git branch -d feature/c/handle-union-assignment
$ git push origin :feature/c/handle-union-assignment
```

Sometimes, one would like to clean the commit history of feature branch before merging into `main`. This can be done using `git rebase -i` **before** checking out `main` and merging into it.


Remain Synchronized
--------------------
When features are merged into `main`, **all existing** branches should pull these changes to remain up to date:
```shell
$ git checkout main
$ git pull origin main
$ git checkout my-project/main
$ git merge main
$ git checkout feature/my-feature
$ git merge main
```

For external contributions, replace `origin` with `upstream`.
