#Generating MC permutations

## Packages

packages <- c("gtools", "readr", "tibble", "dplyr", "data.table", "tidyr")

lapply(packages, library, character.only = TRUE)

## Seed

set.seed(540)

## Study set up

nr_mcs    <- 3    #Number of mock crimes
nr_stages <- 2    #Number of stages per mock crime
nr_style  <- 3    #Number of styles
sample    <- 240  #Sample size
group     <- sample / nr_mcs

style     <- c("standard", "cool_off", "diversion") #interview style
stages    <- c("A","B")                               #stages

## Generate permutations for stages

perm_list <- permutations(n = nr_stages, r = nr_stages, v = stages)

colnames(perm_list) <- c("chunk_1","chunk_2")

perm_list <- data.frame(perm_list)

## Adding column for sequence of stages

perm_list$sequence <-
  paste(
    perm_list$chunk_1,
    perm_list$chunk_2
  )

## Calculating number of possible permutations

poss_permutations <- length(perm_list$sequence)

## Multiplying possible permutations with number of mock crimes

mc_x_poss_permutations <- poss_permutations*nr_mcs

## Creating data frame with all possible permutations * number of mock crimes

MCpermutations <- do.call("rbind", replicate(nr_mcs, perm_list, simplify = FALSE))

## Assigning mock crime to permutations

MCpermutations$mock_crime <- NA

MCpermutations$mock_crime <- c("MC_2")
MCpermutations$mock_crime[1:poss_permutations]<- c("MC_1")
MCpermutations$mock_crime[(mc_x_poss_permutations - (poss_permutations - 1)):mc_x_poss_permutations]<- c("MC_3")

sample_permutations <- do.call("rbind", replicate((sample/mc_x_poss_permutations), MCpermutations, simplify = FALSE))

sample_permutations$style[sample(1:nrow(sample_permutations), nrow(sample_permutations), FALSE)] <- rep(style,group)

style_count <- count(sample_permutations, style)
MC_count <- count(sample_permutations, mock_crime)

overall_count <- count(sample_permutations, mock_crime, sequence, style)

rows <- sample(nrow(sample_permutations))

sample_permutations_random <- sample_permutations[rows,]


write.csv(sample_permutations_random,"./permutations.csv", row.names = FALSE)

