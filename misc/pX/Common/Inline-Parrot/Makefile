all: libparrotside.so loadlib.pbc
clean:
	rm *.pbc
	rm *.so
loadlib.pbc: loadlib.pir
	parrot -o loadlib.pbc loadlib.pir
libparrotside.so: parrotside.c
	cc -shared -fpic `perl config` parrotside.c -o libparrotside.so -g
