# datelifeweb
Contains the code for running the datelife website.

Ideally, the directories here go inside /srv/shiny-server. See the Dockerfile for [datelifedocker](https://github.com/phylotastic/datelifedocker).





OLDER

To test (you need to have a recent version of `devtools` installed in R):

```
devtools::install_github("phylotastic/datelifeweb")
```

To start the server:

```
library(opencpu)
opencpu$browse("library/datelifeweb/www")
```

To test once it's running, you can use the GUI in the web browser that opens and play with it. If you want to use the API:

```
curl http://localhost:5802/ocpu/library/datelifeweb/R/run -d "input='Rhinoceros%20unicornis%2CEquus%20caballus%2CMus%20musculus'&format='mrca' "
```

[changing the URL as appropriate]

This will give you back something like
```
/ocpu/tmp/x0ea8fe25ca/R/.val
/ocpu/tmp/x0ea8fe25ca/stdout
/ocpu/tmp/x0ea8fe25ca/source
/ocpu/tmp/x0ea8fe25ca/console
/ocpu/tmp/x0ea8fe25ca/info
/ocpu/tmp/x0ea8fe25ca/files/DESCRIPTION
```

You can then get values:

```
curl http://localhost:5802/ocpu/tmp/x0ea8fe25ca/R/.val/print
```

returns
```
$message
Bininda-Emonds, Olaf R. P., Marcel Cardillo, Kate E. Jones, Ross D. E. MacPhee, Robin M. D. Beck, Richard Grenyer, Samantha A. Price, Rutger A. Vos, John L. Gittleman, Andy Purvis. 2007. The delayed rise of present-day mammals. Nature 446 (7135): 507-512
101.3
Nyakatura, Katrin, Olaf RP Bininda-Emonds. 2012. Updating the evolutionary history of Carnivora (Mammalia): a new species-level supertree complete with divergence time estimates. BMC Biology 10 (1): 12
105.7
```


We also have a test version running with opencpu using github hooks:

```
https://phylotastic.ocpu.io/datelifeweb
```

This is ONLY for testing. Do not use for real analysis.
