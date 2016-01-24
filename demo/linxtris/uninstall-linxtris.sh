#!/bin/sh
echo -e "Uninstalling LinXtris 0.1a2 ...\n"
prefix=`cut -f1 installed-dir`
bindir=`cut -f2 installed-dir`
rm $bindir"/linxtris"
rm -r $prefix"/linxtris"
rm installed-dir
echo "Removed LinXtris"