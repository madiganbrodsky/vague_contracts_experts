library(tidyverse)
library(jsonlite)
library(Hmisc)
library(binom)
library(brms)
library(EnvStats)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

d <- read_csv("../../proliferate/1_falseconsensus/main-merged.csv")


# EXCLUSIONS

excludedWorkers <- (d %>%
                      filter((version == "unambiguous_uncovered" & individual_judgment == "yes") |
                               (version == "unambgiuous_covered" & individual_judgment == "no")) %>%
                      group_by(workerid) %>%
                      summarise(n = n()) %>%
                      filter(n > 1))$workerid

d <- d %>% 
  filter(!(workerid %in% excludedWorkers))

# DATA TRANSFORMATIONS

transformed <- d %>%
  select(workerid,title,version,individual_judgment,population_judgment,confidence) %>%
  group_by(version,title) %>%
  mutate(total = n()) %>%
  ungroup() %>%
  group_by(version,title,individual_judgment) %>%
  mutate(count = n()) %>%
  mutate(true_proportion = binom.confint(x = count, n  = total, methods = "exact")$mean * 100, 
         ci_low = binom.confint(x = count, n  = total, methods = "exact")$lower * 100,
         ci_high = binom.confint(x = count, n  = total, methods = "exact")$upper * 100) %>%
  mutate(difference = as.numeric(population_judgment) - true_proportion)