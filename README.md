hpc-codecov-action
==================

[![ci](https://github.com/8c6794b6/hpc-codecov-action/workflows/ci/badge.svg)](https://github.com/8c6794b6/hpc-codecov-action/actions?query=workflow%3Aci)
[![codecov](https://codecov.io/gh/8c6794b6/hpc-codecov-action/branch/main/graph/badge.svg?token=P8DDZGTB74)](https://codecov.io/gh/8c6794b6/hpc-codecov-action)


GitHub action to generate [Codecov](https://codecov.io) report for
[Haskell](https://haskell.org) codes with
[hpc-codecov](https://github.com/8c6794b6/hpc-codecov).

Supports Haskell projects using
[cabal-install](https://www.haskell.org/cabal) and
[stack](https://docs.haskellstack.org/en/stable/README/).


QuickStart
----------

Following shows steps to checkout, run tests, then generate and send
coverage report for a test suite named ``my-test-suite`` in a Haskell
cabal package in repository root:

```yaml
name: Main

on: push

jobs:
  cabal_test_and_send_coverage:
    runs-on: ubuntu-latest
    steps:
      - name: Checkout
        uses: actions/checkout@v2

      - name: Build and test
        run: cabal test --enable-coverage

      - name: Generate coverage report
        uses: 8c6794b6/hpc-codecov-action@v1
        with:
          build-tool: cabal
          test-suite: my-test-suite

      - name: Send coverage report
        uses: codecov/codecov-action@v1
```

Usage
-----

See the [github
workflow](https://github.com/8c6794b6/hpc-codecov-action/blob/main/.github/workflows/ci.yml)
YAML of this repository for working examples.
