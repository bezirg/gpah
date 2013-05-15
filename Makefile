CABAL-CONFIGURE-FLAGS := --user
CABAL-BUILD-FLAGS     :=

all : haskell

haskell: 
	runhaskell Setup.lhs configure $(CABAL-CONFIGURE-FLAGS)
	runhaskell Setup.lhs build $(CABAL-BUILD-FLAGS)

get-hackage:
	wget http://hackage.haskell.org/packages/archive/log -O hackage.log
	wget http://hackage.haskell.org/packages/archive/00-archive.tar -O hackage.tar	
	mkdir hackage
	tar -xf hackage.tar -C hackage/
	python hackage.py hackage

readme:
	pandoc README.md -o README.html

clean:
	cabal clean

.PHONY : haskell get-hackage clean readme
