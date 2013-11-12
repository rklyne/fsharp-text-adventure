
all : game.exe

clean :
	rm -f *.exe parser.dll
game.exe : core.fs game.fs
	fsharpc -o game.exe -r FParsecCS.dll -r FParsec.dll $^

#%.dll: %.fs
#	fsharpc -I ${LIBS} -a -o %@ %^

