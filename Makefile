SRC = src/

.PHONY : all
all : 
	@make -C $(SRC)
	@cp src/pmkn .

.PHONY : clean
clean :
	@make -C $(SRC) clean
	@rm pmkn
	rm -f *.diff *.orig *.output

.PHONY : test
test :
	@echo "Begin Tests"
	@echo "End Tests"
