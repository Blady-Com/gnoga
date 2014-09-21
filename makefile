all:
	gprbuild -XLIBRARY_TYPE=static -XAWS_BUILD=default

clean:
	gprclean -XLIBRARY_TYPE=static -XAWS_BUILD=default
