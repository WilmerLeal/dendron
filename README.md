# dendron
A simple tool for calculating cluster frequency within dendrograms. Such calculation uses input patterns, stored in a file, and a set of dendrograms (replicas), stored in another file using Newick format. Dendron's output is a table showing input patterns and their frequencies.

Clusters may be sets, dendrograms or branches (dendrons). Such clusters are compared with those from the set of replicas using two methods: crispy and relaxed. A 'crispy' dendron search extracts matches for the given patterns throughout the replicas (set cluster contrast or graph cluster contrast). When a relaxed search is selected, then a relaxed measure of similarity is used for finding the most similar subpattern within each replica. For more information regarding the contrast methods, read [Leal W., et al](http://www.jcheminf.com/).

## Installation

Dendron is written in Common Lisp; source code needs to be compiled into an executable image file. A Lisp interpreter must be installed. Dendron runs nicely on [SBCL](http://www.sbcl.org). Regarding a Debian system, SBCL is installed typing on a shell terminal:

> sudo apt-get install sbcl

If SBCL is properly installed, it can be launched by writing `sbcl` on the terminal

> sbcl

Type (quit) or CTRL-d for quitting. 

Next, `buildapp` must be installed, as this creates an executable file.

> sudo apt-get install buildapp

Then the library manager for Lisp [quicklisp](http://www.quicklisp.org) must be installed by getting the file:
[http://beta.quicklisp.org/quicklisp.lisp](http://beta.quicklisp.org/quicklisp.lisp). Then you should type:

> sbcl --load quicklisp.lisp

Once you are in SBCL, type:
> *(quicklisp-quickstart:install)

and then:

> *(ql:add-to-init-file)

After that, the library [CFFI](https://common-lisp.net/project/cffi/) must also be installed by typing in the sbcl REPL:

> *(ql:quickload "cffi")

Then type (quit) to exit SBCL. Now we are ready to compile our code. Type on the bash terminal:

> buildapp --load app.lisp --output dendron --entry dendron:support-patterns-from-file


## Synopsis     

> dendron --patterns FILE --replicas REP_FILE --output PREFIX --method METHOD <br/>&nbsp;&nbsp;--include-subdendrons --dendrons-as TYPE

### Options
`--dendron-as TYPE` 

Use TYPE for comparison when a dendrogram or a branch is found in the cluster input file. TYPE might be one of the following values: `set`, `graph` or `both`. If the value of TYPE is `set`, then dendrons are treated as sets. If TYPE is `graph`, then they are treated as graphs (compare as drawings). If TYPE is `both` (default), then frequencies are calculated both as sets and as graphs.                                                   

`--help`
 
 Display all of this help.

`--include-subdendrons`

Recursively calculate frequency of subdendrons of each dendron in FILE. Useful for calculating the frequency of a whole dendrogram and all its branches. Might be time consuming for large dendrograms, especially using graph comparison.

`--method METHOD`

METHOD may be one of:
* `crispy`, calculate frequency of patterns using the crispy comparison.
* `relaxed`, calculate frequency of patterns using a relaxed measure.
* `all`, calculate frequency of patterns using all measures (default).

`--output OUTPUT`

OUTPUT is the name of the output file. It stores all input patterns (see `--patterns`) and their corresponding frequency in a table. Fields are separated by a TAB character.

`--patterns FILE`

Patterns are read from file FILE, one per line. Patterns might be of the type `set` or `dendron`. Sets are lists of elements separated by spaces and enclosed by `(` and `)`. Dendrons are Newick formatted dendrograms or branches (see examples). Elements should not have any of the following characters: SPACE, TAB, LINEFEED, CARRIAGE_RETURN, COLON, COMMA, SEMICOLON, APOSTROPHE and DOUBLE_QUOTES.

`--replicas REP_FILE`

Read REP_FILE to load dendrograms to search for clusters. One dendrogram per line in Newick format.           

## Examples:   
> dendron --patterns example.tre --output example-output --method crispy-set --replicas rep.tre

to calculate pattern frequency in example.tre using a crispy set comparison whith dendrograms
from the file rep.tre.

> dendron --replicas rep.tre --patterns example2.tre --output example2-out --method all --include-subdendrons

to calculate the frequency of patterns in the file example.tre, including their branches and using all methods.
