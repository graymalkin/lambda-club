TARGETS = ls.byte
BINARIES = ls

CFLAGS = -Wall -pedantic -Werror -g -O2

all: $(TARGETS)


%.byte:
	ocamlbuild -use-ocamlfind $@

%.o : %.c
	gcc $(CFLAGS) -o

clean:
	ocamlbuild -clean

