all: lexer.x parser.y main.hs
	alex lexer.x 
	happy parser.y
	ghc main.hs

run:
	./main $(filename)

clean: 
	rm -rf *.hi *.o ./main.exe ./parser.hs ./lexer.hs
