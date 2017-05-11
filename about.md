## How datelife works

### Datelife the R package

https://github.com/phylotastic/datelife

Goals
* Users give input
    * List of names
    * Topology
    * Tree with branch lengths
* They get back one of
    * A single chronogram
        * With matching taxa only (done)
        * With all taxa (ish)
    * The depth of the split
    * A patristic distance matrix
* Or any of the above, but one per reference tree rather than an aggregate
* There are also relatively untested approaches to use, like using BOLD or supertrees

Technologies
* R, `rotl`, `geiger`, `ape` packages

### Datelife.org the website

* Server is omearalab19 in my lab. Currently dedicated to this. Trash can Mac Pro.
* A dockerfile (https://github.com/phylotastic/datelifedocker) is used to set up the server. See its readme for how to do this, as well as the contents of the Dockerfile itself.
* The website itself (https://github.com/phylotastic/datelifeweb) is a mixture of HTML and R Shiny
* The `query` and `citations` directories contain different Shiny apps.
