OBJS = utils.cmo scanner.cmo parser.cmo analyzer.cmo \
interpret.cmo pumpkin.cmo

pmkn : $(OBJS)
	ocamlc -o pmkn $(OBJS)

scanner.ml : scanner.mll
	ocamllex scanner.mll

parser.ml parser.mli : parser.mly
	ocamlyacc parser.mly

scanner.cmo: scanner.ml parser.cmo utils.cmo
parser.cmo: ast.cmi parser.cmi
parser.cmi: parser.mli
ast.cmi: ast.mli
analyzer.cmo: analyzer.ml sast.cmo
sast.cmo: sast.ml ast.cmi
pumpkin.cmo: pumpkin.ml interpret.cmo \
	parser.cmo scanner.cmo utils.cmo analyzer.cmo
interpret.cmo: interpret.ml sast.cmo
utils.cmo: utils.ml ast.cmi parser.cmi parser.cmo sast.cmo

%.cmo : %.ml
	ocamlc -w A -c $<

%.cmi : %.mli
	ocamlc -w A -c $<

.PHONY : clean
clean :
	rm -f pmkn parser.ml parser.mli scanner.ml \
	*.cmo *.cmi *.out *.diff
