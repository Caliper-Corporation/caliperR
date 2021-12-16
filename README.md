# caliperR

Use Caliper software from R

## Installation

```r
if (!require("devtools")) install.packages("devtools")
devtools::install_github('Caliper-Corporation/caliperR')
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
```
