# Releases
All changes to this project will be documented in this file. This version
format adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.3.0] - 2025-11-01

- feat: add support for entrypoint override in jobs e.g.
  `my_job.image.entrypoint` and setting the image name by `my_job.image.name`
  instead of directly setting it from `my_job.image`.

## [0.2.0] - 2025-10-25

- fix: Prevent panic when the main branch of the repo is not equal to master.
- fix: Prevent quoting issues in script entries in the .gitlab-ci.yaml file.

## [0.1.0] - 2024-02-22

- feat: Initial release
