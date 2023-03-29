# caliperR

Use Caliper software from R

## Installation

```r
if (!require("devtools")) install.packages("devtools")
devtools::install_github("https://github.com/Caliper-Corporation/caliperR", build_vignettes = TRUE)
```

## Getting Started

```r
library(caliperR)
dk <- connect()
RunMacro("G30 Tutorial Folder")
#> "C:/Users/..."
```

For more details, see:

```r
vignette("using-caliperR")

# To list all package vignettes:
# vignette(package = "caliperR")
```
