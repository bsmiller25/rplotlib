# rplotlib

This package contains functions that expand on R's base plotting features.

These functions were originally written while I was an employee at the
Federal Reserve Board and are therefore in the public domain. These functions were used in order to produce "Board Style" exhibits. 

To test the package, clone into your local git repo and install by running at the command line (appropriately adjusted, and for Linux of course):

R --no-init-file CMD INSTALL -l /your/local/R/library /your/git/repo/rplotlib

Then, in R:

library(rplotlib, lib.loc = "/your/local/R/library")
