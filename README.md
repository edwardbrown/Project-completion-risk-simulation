# Project-completion-risk-simulation
Project completion risk simulation

This code can be used to simulate project completion failure risk.
The first section uses deSolve as an exemplar and provides a chart, though it can be extended.
The second section provides the bootsrapping and stores the number of project tasks not completed
and estimates the proportion of failures that are probable. 

The third section to perform importance sampling to test for extreme values in the tail(s).

There is also a shiny app in the folder PCRS_shiny_app, which displays one run at a time, using deSolve. It is customizable via slider input.

PCRS-AR uses accept reject methods rather than sampling from uniform to construct several sampling distributions which move the mean to the right of observed for outcome comparisons.

Edward G. Brown 2025

https://creativecommons.org/publicdomain/zero/1.0/
