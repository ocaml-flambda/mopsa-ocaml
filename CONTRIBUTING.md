Contributing
============

This document describes the branching model followed in MOPSA and how contributions are merged into the code base.


Branch Naming Convention
------------------------
The repository of MOPSA has two protected and stable branches: 
- There is one stable branch `master` containing the latest public release of MOPSA.
- The development branch `dev` contains the latest stable features of the analyzer that are planned for the next release.

For each sub-project `<project>` (a supported language, the web interface, a parser, etc.), there is one development branch `<project>/dev` containing stable features. Ongoing features are maintained in (timely limited) branches `<project>/feature/<feature name>`.

To contribute to MOPSA, it is adviced to fork https://gitlab.com/mopsa/mopsa in a separate repository. Branch names should follow the same conventions described above

Feature Branches
----------------
To work on a feature for some sub-project, create a new branch:
```shell
$ git checkout -b c/feature/handle-union-assignment
```

After committing code and testing it, merge into the sub-project's `dev` branch:
```shell
$ git checkout c/dev
$ git pull origin c/dev
$ git merge --no-ff c/feature/handle-union-assignment
$ git push origin c/dev
$ git branch -d c/feature/handle-union-assignment
$ git push origin :c/feature/handle-union-assignment
```

Sometimes, one would like to clean the commit history of feature branch before merging into `<project>/dev`. This can be done using `git rebase -i` **before** checking out `<project>/dev` and merging into it.


External Contributions
----------------------
To integrate your new features into the upstream project branch `<project>/dev`, create a merge request using GitLab web interface and select `mopsa/mopsa/<project>/dev` as the target branch. Ensure to `git pull upstream/web/dev` in order to synchronize with the latest updates from upstream before merging.


The Development Branch `dev`
---------------------------
When features in a project branch are stable enough and planned for the next release, they are merged into the main `dev` branch:
```shell
$ git checkout dev
$ git pull origin dev
$ git merge --no-ff python/dev
$ git push origin dev
```

The flag --no-ff ensures that a new commit is always created when merging. In case `<project>/dev` contains advanced feature not yet completely stable, one could merge specific stable commits:
```shell
$ git checkout dev
$ git pull origin dev
$ git cherry-pick <commit hash>
$ git push origin dev
```

After updating `dev`, **all existing** `<project>/dev` should pull these changes to remain up to date:
```shell
$ git checkout c/dev
$ git pull origin dev
$ git checkout python/dev
$ git pull origin dev
```

Note here that we fast forward the merges in order to have a cleaner commit history.


Release Branch
---------------
When features in `dev` are ready to be released, a new branch `release/<release id>` is created:
```shell
$ git checkout dev
$ git checkout -b release/1.2
```

Work is done in `release/<release id>` to polish the release (late bug fixes, documentation, version bumping). When code is ready for the public release, the branch is merged into `master`:
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
