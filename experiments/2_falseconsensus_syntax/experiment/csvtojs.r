setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(jsonlite)

policy_wordings <- read.csv('policy_wordings.csv')

vignette_headers <- read.csv('vignette_headers.csv')

vignette_continuations <- read.csv('vignette_continuations.csv')

stimuli <- vignette_headers %>%
  right_join(vignette_continuations) %>%
  right_join(policy_wordings, relationship =
               "many-to-many")

write_file(paste("var stimuli =", toJSON(stimuli), sep = " "), 
           'js/stimuli.js')






