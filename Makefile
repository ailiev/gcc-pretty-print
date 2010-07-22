all:
	ghc -o funcdef --make -i.. FuncDef.hs

install: all
	install funcdef $(HOME)/bin
