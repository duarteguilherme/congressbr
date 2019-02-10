## ---- eval=F, echo=FALSE-------------------------------------------------
#  if(!require(devtools)){install.packages("devtools", repos = "http://cran.us.r-project.org")}
#  if(!require(ggplot2)){install.packages("ggplot2", repos = "http://cran.us.r-project.org")}

## ----eval = FALSE--------------------------------------------------------
#  install.packages("congressbr")

## ----eval=FALSE----------------------------------------------------------
#  library(congressbr)

## ---- message = F, warning = F, echo=FALSE-------------------------------
library(congressbr)
library(ggplot2)

## ------------------------------------------------------------------------
sen_votes(date = "20160908")

## ------------------------------------------------------------------------
sen_plenary_result(date = "20160303")

