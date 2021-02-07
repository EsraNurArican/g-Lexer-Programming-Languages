flex:	gpp_lexer.l
	flex -o gpp_lexer.c gpp_lexer.l
	gcc  gpp_lexer.c -lfl -o gpp_lexer.out


clean:
	rm *.out
