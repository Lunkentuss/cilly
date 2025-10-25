# Run your CI locally, cilly

![CI](https://github.com/lunkentuss/cilly/actions/workflows/main.yml/badge.svg)

Cilly is a CLI and TUI application used to run [Gitlab
CI](https://docs.gitlab.com/ee/ci/) pipelines on your local machine.

## Usage

Run `cilly` to run the application with a CLI interface:

![cilly cli](/docs/cli.gif)

Run `cilly --ui tui` to run the application with a TUI interface:

![cilly cli](/docs/tui.gif)

## Develop

Nix can used to build a statically linked binary by running

```bash
nix build .
```

which only works on *linux*. *Mac* users can build a dynamically linked binary
by running

```bash
nix build .#dynamic
```

## Missing Features

Cilly is currently missing some core functionality but is planned to be
implemented in the future. The following is a non exhaustive list of missing
functionality:

- Override environment variables to mock Gitlab CI secrets
- Include
- Matrix
- Passing environment variables with `artifacts:reports:dotenv`
- Child pipelines

## Similar tools

- [gitlab-ci-local](https://github.com/firecow/gitlab-ci-local): CLI tool to
  run Gitlab CI pipelines locally
- [act](https://github.com/nektos/act): CLI tool to run Github actions locally
