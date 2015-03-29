HC=ghc
HFLAGS=--make

all: simple_parser
clean:
	rm simple_parser main.hi main.o

simple_parser: main.hs
	$(HC) -o simple_parser $(HFLAGS) main.hs
	rm main.o main.hi
