tmp/%.cmo : src/%.ml
	ocamlc -c $<

tmp/%.cmi : src/%.mli
	ocamlc -c $<

.PHONY : all clean

all : tmp
	mkdir -p tmp

.PHONY : clean
clean :
	rm -f tmp/*