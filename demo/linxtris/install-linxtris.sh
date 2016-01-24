#!/bin/sh
echo -e "LinXtris 0.1a2 Installation\nCopyright (C) 2003 Dulio Matos Leite de C. e Silva\n"
echo -e "LinXtris is released under the GNU General Public License\nand comes WITHOUT ANY WARRANTY.\n"
echo -n "Data Directory Prefix [/usr/local]: "
read prefix
echo
echo -n "Bin Directory [/usr/local/bin]: "
read bindir
echo
if [ -z $prefix ]; then
	prefix="/usr/local"
fi
if [ -z $bindir ]; then
	bindir="/usr/local/bin"
fi
binfilename=$bindir"/linxtris"
echo -e "#!/bin/bash\n"$prefix"/linxtris/linxtris -data_dir "$prefix"/linxtris/" > $binfilename
chmod 755 $binfilename
mkdir -p $bindir
mkdir -p $prefix"/linxtris"
cp linxtris $prefix"/linxtris"
cp scores $prefix"/linxtris"
cp preferences $prefix"/linxtris"
cp -r pixmaps $prefix"/linxtris"
chmod 755 $prefix"/linxtris/linxtris"
chmod 666 $prefix"/linxtris/scores"
chmod 666 $prefix"/linxtris/preferences"
echo -e $prefix"\t"$bindir > installed-dir
echo -e "LinXtris installed. Enjoy!"
