# traffic light - Tools for data validation

R package **trafficlight**, version **1.15.0**

[![CRAN status](https://www.r-pkg.org/badges/version/trafficlight)](https://cran.r-project.org/package=trafficlight)  [![R build status](https://github.com/pik-piam/trafficlight/workflows/check/badge.svg)](https://github.com/pik-piam/trafficlight/actions) [![codecov](https://codecov.io/gh/pik-piam/trafficlight/branch/master/graph/badge.svg)](https://app.codecov.io/gh/pik-piam/trafficlight) [![r-universe](https://pik-piam.r-universe.dev/badges/trafficlight)](https://pik-piam.r-universe.dev/builds)

## Purpose and Functionality

The package contains tools for data validation and aggregation of
    validation results.


## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made available permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r 
install.packages("trafficlight")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Questions / Problems

In case of questions / problems please contact Jan Philipp Dietrich <dietrich@pik-potsdam.de>.

## Citation

To cite package **trafficlight** in publications use:

Dietrich J, Bonsch M (2023). _trafficlight: traffic light - Tools for data validation_. R package version 1.15.0.

A BibTeX entry for LaTeX users is

 ```latex
@Manual{,
  title = {trafficlight: traffic light - Tools for data validation},
  author = {Jan Philipp Dietrich and Markus Bonsch Bonsch},
  year = {2023},
  note = {R package version 1.15.0},
}
```
