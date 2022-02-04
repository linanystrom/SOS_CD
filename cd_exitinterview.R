#Generate list of IDs to receive exit interview

## Packages

packages <- c("gtools", "readr", "tibble", "dplyr", "data.table", "tidyr", "readxl")

lapply(packages, library, character.only = TRUE)

## Seed

set.seed(540)

## Set up

coding <- read_xlsx("./data/coding.xlsx")

sample_size    <- 159  #Sample size
nr_exit        <- 32   # Amount of exit interviews (159*0.2)

set.seed(14)
exit           <- sample(c(1:sample_size), size = nr_exit, replace = F)

exit_coding <- coding %>% 
  mutate (exit = case_when(
    ID %in% exit ~ 1
  ))