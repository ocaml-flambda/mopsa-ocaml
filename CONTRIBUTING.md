Contributing
============

This document describes the branching model followed in Mopsa and how contributions are merged into the code base.

Branch Naming Convention
------------------------
The repository of Mopsa is organized into several branches: 
- There is one stable branch `master` containing the last public release of Mopsa.
- The development branch `dev` contains the last stable features of the analyzer that are planned for the next release.
- For each supported language `<lang>`, there is one development branch `<lang>/dev` containing stable features for that language.
- Features are maintained in (timely limited) branches `features/<feature name>` or `<lang>/features/<feature name>`. The former is for features for the common parts of the analyzer (the framework and the Universal language), and the latter is for language-specific features.
- Issue branches can follow a similar naming convention `issues/<issue id>` and `<lang>/issues/<issue id>`.

Feature Branches
----------------
To work on a feature `F` in language `Lang`, we create a new branch `Lang/features/F`:
```bash
$ git checkout -b Lang/features/F
```

After committing code and testing it, we can begin merge into `Lang/dev`:
```bash
$ git checkout Lang/dev
$ git merge --no-ff Lang/features/F
$ git branch -d Lang/features/F
$ git push origin Lang/dev
```

Sometimes, one would like to clean the commit history of `Lang/features/F` before merging into `Lang/dev`. This can be done using `git rebase -i` before the previous commands and smash/reword the commits.

The Development Branch `dev`
---------------------------
When a set of features in language branch `Lang/dev` are stable enough and planned for the next release, they can merged into `dev` branch:
```bash
$ git checkout dev
$ git merge --no-ff Lang/dev
$ git push origin dev
```

The flag --no-ff ensures that a new commit is always created when merging. In case `Lang/dev` contains advanced features not yet completely stable, one could merge specific stable commits:
```bash
$ git checkout dev
$ git cherry-pick <commit hash>
```

After updating `dev`, all existing `Lang'/dev` should pull these changes to remain up to date:
```bash
$ git checkout Lang'/dev
$ git merge dev
```

Release Branch
---------------
When features in `dev` are ready to be released, a new branch `release/<release id>` is created:
```bash
$ git checkout dev
$ git checkout -b release/1.2
```

When commits in `release/<release id>` reach a stable state, we are ready to publish the release in `master`:
```bash
$ git checkout master
$ git merge --no-ff release/1.2
$ git tag -a 1.2
$ git checkout dev
$ git merge --no-ff release/1.2
$ git branch -d release/1.2
$ git push origin master dev
```
