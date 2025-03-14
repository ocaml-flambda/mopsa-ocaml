Release workflow:
- update the VERSION file, using semantic versioning
- change the "working version" in the CHANGELOG.md to this new VERSION. List other major changes. Check that we have kept at least a one-line entry for each Merge Request and reference the corresponding MR number
- make a Merge Request for the new version and merge it; check CI
- go into directory release and run the ./make_release.py script and check carefully the result
- run the docker and opam release commands output by the make_release.py script
- edit the Release Notes from the new release on GitLab
  + include the contents of the CHANGELOG.md entry for that version
  + include binary generated by CI
  + include link to docker image
