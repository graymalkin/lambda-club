TARGETS = ls.byte
CFLAGS = -Wall -pedantic -Werror -g -O2


%.byte:
	ocamlbuild -use-ocamlfind $@

%.o : %.c
	gcc $(CFLAGS) 

clean:
	ocamlbuild -clean

