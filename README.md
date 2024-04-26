hpc-codecov-action
==================

[![ci](https://github.com/8c6794b6/hpc-codecov-action/workflows/ci/badge.svg)](https://github.com/8c6794b6/hpc-codecov-action/actions?query=workflow%3Aci)
[![codecov](https://codecov.io/gh/8c6794b6/hpc-codecov-action/branch/main/graph/badge.svg?token=P8DDZGTB74)](https://codecov.io/gh/8c6794b6/hpc-codecov-action)


GitHub action to generate [Codecov](https://codecov.io),
[LCOV](https://github.com/linux-test-project/lcov), and
[Cobertura](https://cobertura.github.io/cobertura/) report for
[Haskell](https://haskell.org) codes with
[hpc-codecov](https://github.com/8c6794b6/hpc-codecov).


QuickStart
----------

The following shows a simple example, assuming that the
repository-root contains a Haskell cabal package named ``my-package``,
and the package contains a test suite named ``my-test-suite``:

```yaml
name: Main

on: push

jobs:
  cabal_test_and_send_coverage:
    runs-on: ubuntu-latest
    steps:
      - name: Checkout
        uses: actions/checkout@v4

      - name: Build and test
        run: |
          cabal update
          cabal configure --enable-coverage
          cabal build
          cabal test

      - name: Generate coverage report
        uses: 8c6794b6/hpc-codecov-action@v4
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
|``target``|**Yes**|N/A|Target to generate test coverage. Either a path to a ``.tix`` file or a ``TOOL:TEST_SUITE`` style string value. |
|``mix``|No|N/A|Comma-separated directory names containing ``.mix`` files. |
|``src``|No|N/A|Comma-separated directory names for source code lookup.|
|``excludes``|No|N/A|Comma-separated module names to exclude from coverage report, E.g.: ``Main,Paths_project1,Foo,Bar``. |
|``skip``|No|N/A|Comma separated directory names to skip when searching files for ``TOOL``.|
|``format``|No|``codecov``|Format of the output report, ``codecov``, ``lcov``, or ``cobertura``|
|``out``|No|``./codecov.json`` when the ``format`` is ``codecov``, ``./lcov.info`` when the ``format`` is ``lcov``, or ``./coverage.xml`` when the ``format`` is ``cobertura``|Output path to write the report.|
|``root``|No|``./``|Project root directory, usually the directory containing ``stack.yaml`` or ``cabal.project``. |
|``build``|No|``.stack-work`` when the ``TOOL`` is ``stack``, or ``dist-newstyle`` when the ``TOOL`` is  ``cabal``|Name of the directory made by the build tool.|
|``expr_only``|No|``false``|Count expressions only.|
|``ignore_dittos``|No|``false``|Ignore consecutive entries with the same source code positions.|
|``verbose``|No|``true``|Show verbose output. |


Outputs
-------

| Name | Description |
|------|-------------|
|``exe``|Path of ``hpc-codecov`` executable.|
|``report``|Path of generated coverage report file.|


Examples
--------

See the [github
workflow](https://github.com/8c6794b6/hpc-codecov-action/blob/main/.github/workflows/ci.yml)
YAML of this repository for working examples.
