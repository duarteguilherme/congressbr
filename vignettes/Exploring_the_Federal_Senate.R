## ---- eval = F-----------------------------------------------------------
#  if(!require(devtools)){install.packages("devtools")}
#  devtools::install_github("RobertMyles/congressbr")
#  library(congressbr)

## ---- echo=F, message=F, warning=F---------------------------------------
library(congressbr)

## ------------------------------------------------------------------------
sen_votes(date = "20160908")

## ------------------------------------------------------------------------
sen_senator_list()

## ------------------------------------------------------------------------
sen_coalition_info(code = 200)

## ------------------------------------------------------------------------
sen_commissions_senators(code = "CCJ")

## ------------------------------------------------------------------------
sen_parties()

## ------------------------------------------------------------------------
UF()

## ------------------------------------------------------------------------
sen_bills_passing(year = "2001", type = "MPV")

## ------------------------------------------------------------------------
sen_agenda(initial_date = "20161105", end_date = "20161125")

## ---- message = F, warning = F-------------------------------------------
library(ggplot2)
all_sens <- sen_senator_list()

ggplot(all_sens, aes(x = gender)) +
  geom_bar(aes(fill = gender), colour = "white") +
  theme_classic() +
  scale_fill_manual(values = c("#45C74A", "#FFFF00"))

## ------------------------------------------------------------------------
ggplot(all_sens, aes(x = status)) +
  geom_bar(aes(fill = status), colour = "black") +
  theme_classic() +
  scale_fill_manual(values = c("#45C74A", "navy")) +
  theme(legend.position = "none") +
  coord_flip() 

## ---- warning = F, message = F-------------------------------------------
library(dplyr)
all_sens %>% filter(status != "Titular") %>% group_by(state) %>% 
  summarise(totals = n()) %>% arrange(desc(totals))

