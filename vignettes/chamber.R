## ---- eval=F-------------------------------------------------------------
#  if(!require(devtools)){install.packages("devtools", repos = "http://cran.us.r-project.org")}
#  if(!require(ggplot2)){install.packages("ggplot2", repos = "http://cran.us.r-project.org")}

## ---- message = F, warning=F---------------------------------------------
library(congressbr)
library(ggplot2)

## ------------------------------------------------------------------------
cham_votes(type = "PL", number = "1992", year = "2007")

## ------------------------------------------------------------------------
cham_bill_info(type = "PEC", number = "472", year = "1997")

## ------------------------------------------------------------------------
cham_bill_info(type = "PL", number = "3962", year = "2008")

## ------------------------------------------------------------------------
cham_bill_info_id(14784)

## ------------------------------------------------------------------------
cham_plenary_bills(year = 2008)

## ------------------------------------------------------------------------
cham_typeauthors_bills()

## ---- message = F, warning = F-------------------------------------------

party_orientation <- cham_votes(type = "PL", number = "1992", year = "2007")
party_orientation <- as.data.frame(t(party_orientation[1, c(7, 10:22)])) # select only one row and the columns of interest; transpose data
names(party_orientation)[1] <- "orientation"

ggplot2::ggplot(party_orientation, aes(x = orientation)) +
  geom_bar(aes(fill = orientation), colour = "white") +
  theme_classic() + 
  scale_fill_hue(l = 40) + 
  ggtitle("Party Orientation - PL 1992/2007") +
  labs(x = "Orientation", y = "Count") + 
  theme(legend.position = "none")

## ---- message = F, warning = F-------------------------------------------
if(!require(dplyr)){install.packages("dplyr", repos = "http://cran.us.r-project.org")}
library(dplyr)

party_orientation %>%
  group_by(orientation) %>%
  tally()

## ---- message = F, warning = F-------------------------------------------
if(!require(dplyr)){install.packages("dplyr", repos = "http://cran.us.r-project.org")}
library(dplyr)

party_orientation %>%
  group_by(orientation) %>%
  tally()

