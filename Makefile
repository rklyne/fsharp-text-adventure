
FPARSEC_PATH=/usr/lib/mono/4.0
LIBS=${FPARSEC_PATH}

all : game.exe

clean :
	rm -f *.exe parser.dll
game.exe : game.fs
	fsharpc -o game.exe -r ${FPARSEC_PATH}/FParsecCS.dll -r ${FPARSEC_PATH}/FParsec.dll $<

#%.dll: %.fs
#	fsharpc -I ${LIBS} -a -o %@ %^

