OBJS = utils.cmo codegen.cmo scanner.cmo parser.cmo analyzer.cmo processor.cmo pumpkin.cmo

pmkn : $(OBJS)
	ocamlc -o pmkn $(OBJS)

scanner.ml : scanner.mll
	ocamllex scanner.mll

parser.ml parser.mli : parser.mly
	ocamlyacc -v parser.mly

scanner.cmo: scanner.ml parser.cmo utils.cmo
parser.cmo: ast.cmi parser.cmi
parser.cmi: parser.mli ast.cmi
ast.cmi: ast.mli
analyzer.cmo: analyzer.ml sast.cmo
sast.cmo: sast.ml ast.cmi
pumpkin.cmo: pumpkin.ml\
	parser.cmo scanner.cmo utils.cmo processor.cmo analyzer.cmo codegen.cmo
interpret.cmo: interpret.ml sast.cmo
utils.cmo: utils.ml ast.cmi parser.cmi sast.cmo
codegen.cmo: sast.cmo ast.cmi
processor.cmo: processor.ml parser.cmo scanner.cmo

%.cmo : %.ml
	ocamlc -c $<

%.cmi : %.mli
	ocamlc -c $<

.PHONY : clean
clean :
	rm -f pmkn parser.ml parser.mli scanner.ml \
	*.cmo *.cmi *.out *.diff *.orig *.output
