SRC = src/

.PHONY : all
all : 
	@make -C $(SRC)
	@cp src/pmkn .

.PHONY : clean
clean :
	@make -C $(SRC) clean
	@rm -f pmkn
	rm -f *.diff *.orig *.output tests/*.orig

.PHONY : test
test :
	@echo "Begin Tests"
	@echo "End Tests"
