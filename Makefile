SRC = src/

OBJS = \
	$(SRC)utils.cmo \
	$(SRC)scanner.cmo \
	$(SRC)parser.cmo \
	$(SRC)analyzer.cmo \
	$(SRC)processor.cmo \
	$(SRC)pumpkin.cmo

pmkn :
	make -C $(SRC)
	ocamlc -o pmkn $(OBJS)

.PHONY : clean
clean :
	make -C $(SRC) clean
	rm -f pmkn *.diff *.orig *.output
