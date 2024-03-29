0.2.0.0
-------
* Support some features described in “General OPB Format”
  https://www.cril.univ-artois.fr/PB24/OPBgeneral.pdf
  * allow both min and max keywords in the objective function
  * allow any usual relational operator in constraints
  * allow to use Unicode characters and the UTF-8 encoding for relational operators

0.1.11.0
-------
* some minor clean-up

0.1.10.0
-------
* allow parsing empty `<sum>`. https://github.com/msakai/pseudo-boolean/pull/5

0.1.9.0
-------
* fix compilation error with `megaparsec <4.4` due to the lack of MonadPlus requirement

0.1.8.0
-------
* make types to be instances of `Read` class

0.1.7.0
-------
* introduce `ParserError` type
* support megaparsec `7.*`

0.1.6.0
-------
* include #mincost=, #maxcost=, #sumcost= in the hint line of WBO files

0.1.5.0
-------
* support megaparsec-5.*

0.1.4.0
-------
* add Megaparsec-based parsers

0.1.3.0
-------
* relax the grammer of OPB/WBO files to allow ommitng spaces at the end of weighted terms.

0.1.2.0
-------
* relax the grammer of OPB/WBO files to allow various use of space characters.

0.1.1.0
-------
* parse* functions fails if the parser does not consume all of the inputs
* generate '#product=', 'sizeproduct=' and '#soft=' in header line of OPB/WBO files
* sort literals in a non-linear term when generating OPB/WBO files
