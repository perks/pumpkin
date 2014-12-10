OBJS = utils.cmo codegen.cmo scanner.cmo parser.cmo analyzer.cmo processor.cmo pumpkin.cmo

pmkn : $(OBJS)
	ocamlc -o pmkn $(OBJS)

scanner.ml : scanner.mll
	ocamllex scanner.mll

parser.ml parser.mli : parser.mly
	ocamlyacc -v parser.mly

analyzer.cmo : utils.cmo sast.cmo ast.cmi
analyzer.cmx : utils.cmx sast.cmx ast.cmi
codegen.cmo : sast.cmo ast.cmi
codegen.cmx : sast.cmx ast.cmi
interpret.cmo : sast.cmo
interpret.cmx : sast.cmx
parser.cmo : ast.cmi parser.cmi
parser.cmx : ast.cmi parser.cmi
processor.cmo : scanner.cmo parser.cmi
processor.cmx : scanner.cmx parser.cmx
pumpkin.cmo : utils.cmo processor.cmo codegen.cmo analyzer.cmo
pumpkin.cmx : utils.cmx processor.cmx codegen.cmx analyzer.cmx
sast.cmo : ast.cmi
sast.cmx : ast.cmi
scanner.cmo : utils.cmo parser.cmi
scanner.cmx : utils.cmx parser.cmx
utils.cmo : sast.cmo parser.cmi ast.cmi
utils.cmx : sast.cmx parser.cmx ast.cmi
ast.cmi :
parser.cmi : ast.cmi

%.cmo : %.ml
	ocamlc -c $<

%.cmi : %.mli
	ocamlc -c $<

.PHONY : clean
clean :
	rm -f pmkn parser.ml parser.mli scanner.ml \
	*.cmo *.cmi *.out *.diff *.orig *.output
