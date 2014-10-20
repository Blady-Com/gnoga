all: @@data.App_Name_Lower@@ 

@@data.App_Name_Lower@@:
	cd src && gprbuild -p -P@@data.App_Name_Lower@@.gpr

clean:
	cd src && gprclean -P@@data.App_Name_Lower@@.gpr
