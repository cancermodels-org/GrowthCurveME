## CRAN Package Updates (version 0.1.1)
This is a re-submission addressing the ERROR from the previous CRAN approved
version (version 0.1.0). A test that previously used exact equality comparison 
was updated to allow for numerical tolerance in floating point computations. 
All CRAN checks now pass with previous comments from first submission still 
applying for wrapping specific function in the the 'donttest{}' function due 
to their run exceeding the CPU (user + system) or elapsed time > 5s.

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

Some function examples are wrapped in the 'donttest{}' function due to their 
run exceeding the CPU (user + system) or elapsed time > 5s, 
but the elapsed time remains minimal (a few seconds over). The package 
functions that employ methods from other R packages include 
hyperlinks directly to the relevant source functions within those packages to 
make user review and interpretation easier.

I have addressed the comments from Benjamin Altmann 
(benjamin.altmann@wu.ac.at) from 12/16/2024 and hope they will be found to be
satisfactory.
