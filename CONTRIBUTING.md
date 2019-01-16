Contributing
============

This document describes the branching model followed in MOPSA and how contributions are merged into the code base.


Branch Naming Convention
------------------------
The repository of MOPSA is organized into several branches: 
- There is one stable branch `master` containing the latest public release of MOPSA.
- The development branch `dev` contains the latest stable features of the analyzer that are planned for the next release.
- For each supported language `<lang>`, there is one development branch `<lang>/dev` containing stable features for that language.
- Features are maintained in (timely limited) branches `feature/<feature name>` or `<lang>/feature/<feature name>`. The former is for features for the common parts of the analyzer (the framework and the Universal language), and the latter is for language-specific features.
- Issue branches can follow a similar naming convention `issue/<issue id>` and `<lang>/issue/<issue id>`.


Feature Branches
----------------
To work on a feature for some language, we create a new branch:
```shell
$ git checkout -b c/feature/handle-union-assignment
```

After committing code and testing it, we can merge into the language `dev` branch:
```shell
$ git checkout c/dev
$ git merge --no-ff c/feature/handle-union-assignment
$ git push origin c/dev
$ git branch -d c/feature/handle-union-assignment
$ git push origin :c/feature/handle-union-assignment
```

Sometimes, one would like to clean the commit history of feature branch before merging into `<lang>/dev`. This can be done using `git rebase -i` **before** checking out `<lang>/dev` and merging into it.


The Development Branch `dev`
---------------------------
When features in a language branch are stable enough and planned for the next release, they are merged into the main `dev` branch:
```shell
$ git checkout dev
$ git merge --no-ff python/dev
$ git push origin dev
```

The flag --no-ff ensures that a new commit is always created when merging. In case `<lang>/dev` contains advanced feature not yet completely stable, one could merge specific stable commits:
```shell
$ git checkout dev
$ git cherry-pick <commit hash>
```

After updating `dev`, all existing `<lang>/dev` should pull these changes to remain up to date:
```shell
$ git checkout c/dev
$ git merge dev
$ git checkout python/dev
$ git merge dev
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
