This directory contains the ZanyBlue system tests:  these are more "black box"
style tests where the ZB commands are exercised for various command line
options and various inputs.

The standard test is simply

	make check

If the ZB library and command were build for a coverage run, the tests should
be run using

	make gcheck

which sets the ZBTest "build_opts" parameter to include the coverage compiler
options.
