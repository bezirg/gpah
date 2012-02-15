CABAL-CONFIGURE-FLAGS := --user
CABAL-BUILD-FLAGS     :=

all : haskell

haskell: 
	runhaskell Setup.lhs configure $(CABAL-CONFIGURE-FLAGS)
	runhaskell Setup.lhs build $(CABAL-BUILD-FLAGS)

get-hackage:
	wget http://hackage.haskell.org/packages/archive/log -O hackage.log
	wget http://hackage.haskell.org/cgi-bin/hackage-scripts/archive.tar -O hackage.tar
	mkdir hackage
	tar -xf hackage.tar -C hackage/
	python hackage.py hackage

deriving:
	./dist/build/DerivingParsing/DerivingParsing +RTS -K50m -RTS > results/d_analysis.txt
	cat results/d_analysis.txt | ./dist/build/DerivingMining/DerivingMining

function:
	./dist/build/FunctionParsing/FunctionParsing > results/f_analysis.txt
	cat results/f_analysis.txt | ./dist/build/FunctionMining/FunctionMining

error:
	./dist/build/ErrorParsing/ErrorParsing +RTS -K50m -RTS > results/e_analysis.txt
	cat results/e_analysis.txt | ./dist/build/ErrorMining/ErrorMining

uni:
	./dist/build/UniParsing/UniParsing > results/uni_analysis.txt
	cat results/uni_analysis.txt | ./dist/build/UniMining/UniMining

misc-hackage:
	./dist/build/MiscHackage/MiscHackage

misc-date:
	./dist/build/MiscDate/MiscDate

cpp:
	./dist/build/Cpp/Cpp

readme:
	pandoc README.md -o README.html

clean:
	cabal clean

.PHONY : haskell get-hackage deriving function error uni misc-hackage misc-date clean readme
