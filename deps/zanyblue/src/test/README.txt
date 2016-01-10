The ZanyBlue tests are divided into two major categories:

  * Unit tests in the "unittest" directory
  * System, or functional, tests in the "system" directory

Each of these directories, in turn contain directory trees with tests being
more specific deeper in the tree.  To run the tests use

  $ make check

either at this level to run both the unit and system tests, or in any
particular sub-directory to run the more specific targetted tests.

The system tests use the "zbtest" utility to repeatedly execute commands
("zbmcompile" and "zbtest" for this release) with various input and options.
The tests are run in a test area (the "test-area" subdirectory created by
the "zbtest" command) by making this directory the current directory for
the "zbtest" command.  Any input files needed are copied into this directory,
any output files are created in this directory.  Output files are compared
with reference file and generate either "ok" files (successful test) or
"fail" files (failed tests).
