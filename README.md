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
   	wget http://hackage.haskell.org/packages/archive/00-archive.tar -O hackage.tar
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

## Compiling the source code

~~~
make
~~~

## Before running

- Remove `hackage/hsc3/0.11/hsc3-0.11/Help` directory

- (optional) To make the analysis run in-memory (faster and less disk-intensive),
  do `cp -r ./hackage/ /tmp` . This command will copy the hackage archive
  to RAM, providing you have enough memory allocated to `tmpfs`.


## Running the program

After building the program, run it with

`dist/build/Run/Run args`

For a detailed description of the accepted arguments run:

`dist/build/Run/Run --help`
