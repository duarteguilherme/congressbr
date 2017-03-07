
congressbr
----------

congressbr is a package for extracting data from the APIs of the Brazilian Federal Senate and Chamber of Deputies, respectively. This package is under development and contributions are welcome.

### installation

As of yet, congressbr is not on CRAN. It can be installed with the devtools package:

``` r

devtools::install_github("RobertMyles/congressbr")
```

### function naming

We follow [googlesheets](https://github.com/jennybc/googlesheets) in using a naming convention for functions that facilitates tab completion. All Senate-related functions start with `sen_` and all Chamber-related ones start with `cam_` ('Câmara').
