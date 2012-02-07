# GP analysis of Hackage README

For the report and documentation of the project, check the file `report.pdf`

There is also the presentation we gave at the file `presentation.pdf`

## Getting the Hackage repository

### The easy way:

~~~
make get-hackage
~~~

### The hard way:

* Open up a terminal and go in the root directory of this project.
* Download the Hackage log:

    ~~~
    wget http://hackage.haskell.org/packages/archive/log -O hackage.log

    ~~~

* Download a recent snapshot of Hackage by doing:

    ~~~
   	wget http://hackage.haskell.org/cgi-bin/hackage-scripts/archive.tar -O hackage.tar
    ~~~

* Untar it:

    ~~~
    mkdir hackage
	tar -xf hackage.tar -C hackage/
    ~~~

* Run the script to extract each package:

    ~~~
    python hackage.py hackage
    ~~~

* If you gave different paths for the archive and the log, please
update the `src/Analysis/Conf.hs`

## Compiling the source code

~~~
make
~~~

## Before running

- Edit `hackage/bytestring/0.9.2.1/bytestring-0.9.2.1/bytestring.cabal` and remove the tests

- Remove `hackage/hsc3/0.11/hsc3-0.11/Help` directory


## Running the program

### The easy way

There are some make commands for easier running the executables:

~~~
make (deriving | function | error | uni | misc-hackage | misc-date)
~~~

### The manual way

The executables are under `dist/build`: 

~~~
DerivingParsing, DerivingMining,
FunctionParsing,FunctionMining,
ErrorParsing, ErrorMining,
UniParsing, UniMining,
MiscHackage, MiscDate
~~~

Select the analysis you want to run (let's say FunctionAnalysis) and do:

~~~
./dist/build/FunctionParsing/FunctionParsing | ./dist/build/FunctionMining/FunctionMining
~~~

Alternatively you can store the results for later consumption:

~~~
./dist/build/DerivingParsing/DerivingParsing > deriving_results.txt
~~~

And later:

~~~
cat deriving_results.txt | ./dist/build/DerivingMining/DerivingMining
~~~




