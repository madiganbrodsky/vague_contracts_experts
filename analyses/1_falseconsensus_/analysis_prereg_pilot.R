library(tidyverse)
library(jsonlite)
library(binom)
library(brms)
library(DescTools)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("../helpers.R")

d <- read_csv("../../data/1_falseconsensus_/vague_contracts_syntax-merged.csv")

# EXCLUSIONS

# # EXCLUDE WORKERS WHO MAKE MORE THAN ONE ERROR ON THE FILLER TRIALS 

excludedWorkers <- (d %>%
                      filter((version == "filler_uncovered" & individual_judgment == "yes") |
                               (version == "filler_covered" & individual_judgment == "no")) %>%
                      group_by(workerid) %>%
                      summarise(n = n()) %>%
                      filter(n > 0))$workerid

# # TODO: EXCLUDE WORKERS WHO SELF-REPORT A NATIVE LANGUAGE OTHER THAN ENGLISH

d <- d %>% 
  filter(!(workerid %in% excludedWorkers))

# DATA TRANSFORMATIONS

transformed <- d %>%
  filter(version %in% c("uncovered","covered","controversial")) %>% 
  select(workerid,item,version,center_embedding,passive,individual_judgment,population_judgment,confidence) %>%
  group_by(version,item,center_embedding,passive) %>%
  mutate(total = n()) %>%
  ungroup() %>%
  group_by(version,item,center_embedding,passive,individual_judgment) %>%
  mutate(count = n()) %>%
  mutate(true_proportion = binom.confint(x = count, n  = total, methods = "exact")$mean * 100, 
         ci_low = binom.confint(x = count, n  = total, methods = "exact")$lower * 100,
         ci_high = binom.confint(x = count, n  = total, methods = "exact")$upper * 100) %>%
  mutate(difference = as.numeric(population_judgment) - true_proportion) %>% 
  mutate(syntax_condition = case_when(center_embedding == "yes" & passive == "yes" ~ "CEP",
                                             center_embedding == "yes" & passive == "no" ~ "CEA",
                                             center_embedding == "no" & passive == "yes" ~ "NCEP",
                                             TRUE ~ "NCEA")) %>%
  mutate(majority_vs_minority_response = case_when(true_proportion >= 50 ~ "majority",
                                                   TRUE ~ "minority")) %>%
  ungroup() %>%
  mutate(center_embedding = relevel(as.factor(center_embedding), ref = "no"),
         passive = relevel(as.factor(passive), ref = "no"),
         individual_judgment = relevel(as.factor(individual_judgment), ref = "no"))

props <- transformed %>%
  filter(version %in% c("uncovered","covered", "controversial")) %>% 
  group_by(item, version, center_embedding, passive, syntax_condition) %>%
  mutate(yes = case_when(individual_judgment == "yes" ~ 1,
                         TRUE ~ 0),
         no = case_when(individual_judgment == "no" ~ 1,
                        TRUE ~ 0)) %>%
  summarise(nYes = sum(yes), nNo =sum(no),
            propYes = sum(yes)/n(), propNo = sum(no)/n(), meanError = mean(difference),
            entropy = (propYes * log2(1/propYes)) + (propNo * log2(1/propNo))) %>%
  mutate(entropy = case_when(is.nan(entropy) ~ 0,
                             TRUE ~ entropy)) %>%
  ungroup() %>%
  mutate(version = relevel(as.factor(version), ref = "uncovered")) 

# REGRESSIONS

# # MODEL 1: Q1 ENTROPY FROM CENTERED FIXED EFFECTS OF CTR. EMBEDDING, PASSIVE, CONDITION, AND THEIR INTERACTION, W/ MAX RANEF. STRUCTURE JUSTIFIED BY DESIGN. 

options(mc.cores = parallel::detectCores())

centered_m1 <- cbind(props, myCenter(data.frame(props[,c("passive","center_embedding")])))   

m1 <- brm(entropy ~ cpassive * ccenter_embedding * version + (1 + (cpassive * ccenter_embedding * version)|item),
          data = centered_m1)

# # MODEL 2: Q2 RESPONSE FROM CENTERED FIXED EFFECTS OF CTR. EMBEDDING, PASSIVE, INDIVIDUAL RESPONSE TYPE, EMPIRICAL RESPONSE PROPORTION, AND THEIR INTERACTION, W/ MAX RANEF. STRUCTURE JUSTIFIED BY DESIGN. 

options(mc.cores = parallel::detectCores())

centered_m2 <- cbind(transformed, myCenter(data.frame(transformed[,c("passive","center_embedding","individual_judgment","true_proportion")])))   

m2 <- brm(population_judgment ~ cpassive * ccenter_embedding * cindividual_judgment * ctrue_proportion + (1 + (cpassive * ccenter_embedding * cindividual_judgment * ctrue_proportion)|item) + (1 + (cpassive * ccenter_embedding * cindividual_judgment * ctrue_proportion)|workerid),
          data = centered_m2)

#################################PLOTS#########################################
# plot 1a: proportion of yes responses (y), syntax condition (x), color (condition), facet (maj vs min in non-DPL case)
ggplot(data = props, 
       mapping = aes(x = syntax_condition, 
                     y = propYes)) + 
  geom_jitter(aes(color = version), alpha = 0.5) # + 
  # facet_wrap(~majority_vs_minority_response)

# plot 1a: proportion of yes responses (y), syntax condition (x), color (condition), facet (yes vs no response in DPL case)
ggplot(data = props, 
       mapping = aes(x = syntax_condition, 
                     y = propYes)) + 
  geom_jitter(aes(color = version), alpha = 0.5) # + 
  # facet_wrap(~individual_judgment)

# plot 2a: mean agreement estimate (y), syntax condition (x), color (condition), facet (maj vs min in non-DPL case)
ggplot(data = props, 
       mapping = aes(x = syntax_condition, 
                     y = population_judgment)) + 
  geom_jitter(aes(color = version), alpha = 0.5) # + 
  # facet_wrap(~majority_vs_minority_response)

# plot 2a: mean agreement estimate (y), syntax condition (x), color (condition), facet (yes vs no in non-DPL case)
ggplot(data = props, 
       mapping = aes(x = syntax_condition, 
                     y = population_judgment)) + 
  geom_jitter(aes(color = version), alpha = 0.5) # + 
  # facet_wrap(~individual_judgment)

#plot 3: mean agreement estimate (y), prop yes (x), facet by syntax condition, color (condition )
ggplot(data = props, 
       mapping = aes(x = population_judgment, 
                     y = propYes)) + 
  geom_jitter(aes(color = version), alpha = 0.5) + 
  facet_wrap(~syntax_condition)

#plot 4: error (y), syntax condition (x)
ggplot(data = props, 
       mapping = aes(x = syntax_condition, 
                     y = difference)) + 
  geom_jitter(aes(color = version), alpha = 0.5) 

#plot 5: 
ggplot(data = props, 
       mapping = aes(x = confidence, 
                     y = propYes)) + 
  geom_jitter(aes(color = individual_judgment), alpha = 0.5) + 
  facet_wrap(~syntax_condition)



