
FPARSEC_PATH=/usr/lib/mono/4.0
LIBS=${FPARSEC_PATH}

all : game.exe parser.dll parser.exe

clean :
	rm -f *.exe parser.dll
parser.exe : parser.fs
	fsharpc -r ${FPARSEC_PATH}/FParsecCS.dll -r ${FPARSEC_PATH}/FParsec.dll -o $@ $<
parser.dll : parser.fs
	fsharpc -I ${LIBS} -r ${FPARSEC_PATH}/FParsecCS.dll -r ${FPARSEC_PATH}/FParsec.dll -a -o parser.dll parser.fs
game.exe : game.fs parser.dll
	fsharpc -o game.exe -r parser.dll $<

#%.dll: %.fs
#	fsharpc -I ${LIBS} -a -o %@ %^

