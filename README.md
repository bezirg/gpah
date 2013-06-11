# GP analysis of Hackage README

For the report and documentation of the project, check the file `report.pdf`

There is also the presentation we gave at the file `presentation.pdf`

## Building

~~~
runghc Setup.lhs configure
runghc Setup.lhs build
runghc Setup.lhs install
~~~


# Before running

You have to fetch the the entire hackage repository and its activity log. You can automatically do that with:

~~~
gpah --fetch
~~~


## Running the analysis

After building the program, run it with

`gpah args`

~~~
Generic Programming Use in Hackage Project v0.0.1

gpah [OPTIONS]

Common flags:
     --fetch                Fetch the Hackage archive tarball and Hackage
                            activity log
  -d --deriving[=FILE]      Run the deriving analysis and output it to the
                            specified file
  -f --function[=FILE]      Run the function analysis and output it to the
                            specified file
  -u --uniplate[=FILE]      Run the uniplate analysis and output it to the
                            specified file
  -h --misc_hackage[=FILE]  Print general analysis info or output it to the
                            specified file
  -t --misc_date[=FILE]     Get syb+uniplate date info and output it to the
                            specified file (implies --misc_hackage)
     --hackage-dir=DIR      Set the hackage archive directory
     --hackage-log=FILE     Set the hackage archive log file
  -c --cpp[=FILE]           Run preprocessing in hackage that yields an
                            analysis with less failed-to-parse modules
  -i --interpret[=FILE]     Do type-checking interpretation on modules that
                            depend on SYB or Uniplate
  -e --derive[=FILE]        Do analysis on packages that build on derive
  -? --help                 Display help message
  -V --version              Print version information
~~~

For a detailed description of the accepted arguments run:

`gpah --help`
