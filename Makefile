TMP_DIR=tmp
SRC_DIR=src

MKDIR_P=mkdir -p

$(TEMP_DIR)/%.cmo : $(SRC_DIR)/%.ml
	ocamlc -c $<

$(TEMP_DIR)/%.cmi : $(SRC_DIR)%.mli
	ocamlc -c $<

.PHONY : all clean

all : $(TEMP_DIR)

$(TEMP_DIR): 
	echo "THIS"
	$(MKDIR_P) $@

.PHONY : clean
clean :
	rm -f tmp/*