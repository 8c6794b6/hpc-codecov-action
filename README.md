hpc-codecov-action
==================

[![ci](https://github.com/8c6794b6/hpc-codecov-action/workflows/ci/badge.svg)](https://github.com/8c6794b6/hpc-codecov-action/actions?query=workflow%3Aci)
[![codecov](https://codecov.io/gh/8c6794b6/hpc-codecov-action/branch/main/graph/badge.svg?token=P8DDZGTB74)](https://codecov.io/gh/8c6794b6/hpc-codecov-action)


GitHub action to generate [Codecov](https://codecov.io) report for
[Haskell](https://haskell.org) codes with
[hpc-codecov](https://github.com/8c6794b6/hpc-codecov).


QuickStart
----------

Following shows simple example, assuming that the repository root
contains a Haskell cabal package named ``my-package``, and the package
contains a test suite named ``my-test-suite``:

```yaml
name: Main

on: push

jobs:
  cabal_test_and_send_coverage:
    runs-on: ubuntu-latest
    steps:
      - name: Checkout
        uses: actions/checkout@v3

      - name: Build and test
        run: |
          cabal update
          cabal test --enable-coverage

      - name: Generate coverage report
        uses: 8c6794b6/hpc-codecov-action@v2
        with:
          target: cabal:my-test-suite
          excludes: Main,Paths_my_package

      - name: Send coverage report
        uses: codecov/codecov-action@v3
```

Inputs
------

| Name | Required | Default | Description |
|------|----------|---------|-------------|
|``target``|**Yes**|N/A|Target to generate test coverage. Either a path to ``.tix`` file, or ``TOOL:TEST_SUITE`` style string value. |
|``mix``|No|N/A|Comma separated directory names containing ``.mix`` files. |
|``src``|No|N/A|Comma separated directory names for source code lookup. |
|``excludes``|No|N/A|Comma separated module names to exclude from coverage report, E.g.: ``Main,Paths_project1,Foo,Bar``. |
|``out``|No|``./codecov.json``|Output path to write the report.|
|``root``|No|``.``|Project root directory, typically the directory containing ``stack.yaml`` or ``cabal.project``. |
|``build``|No|N/A|Name of the directory made by the build tool. Default is ``.stack-work`` for stack, and ``dist-newstyle`` for cabal. |
|``skip``|No|N/A|Comma separated directory names to skip when searching files for TOOL. |
|``verbose``|No|``true``|Show verbose output. |


Outputs
-------

| Name | Description |
|------|-------------|
|``exe``|Path of hpc-codecov executable.|
|``report``|Path of generated coverage report JSON file.|


Examples
--------

See the [github
workflow](https://github.com/8c6794b6/hpc-codecov-action/blob/main/.github/workflows/ci.yml)
YAML of this repository for working examples.
