library(tidyverse)
library(jsonlite)
library(Hmisc)
library(binom)
library(brms)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

theme_set(theme_bw())
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

source("../helpers.R")

d <- read_csv("../../data/2_falseconsensus_syntax/pilot/vague_contracts_syntax_2-merged.csv")

# EXCLUSIONS: 

# # EXCLUDE WORKERS WHO MAKE AN ERROR ON THE FILLER TRIALS 

excludedWorkers <- (d %>%
                      filter((version == "filler_uncovered" & individual_judgment == "yes") |
                               (version == "filler_covered" & individual_judgment == "no")) %>%
                      group_by(workerid) %>%
                      summarise(n = n()) %>%
                      filter(n > 1))$workerid

# # EXCLUDE WORKERS WHO SELF-REPORT A NATIVE LANGUAGE OTHER THAN ENGLISH

# # # todo: populate this with answers that aren't "English" or variants thereof
nonEnglishNativeLanguages <- c()

excludedWorkers_nonNative <- unique((d %>%
                                       filter(subject_information.language %in% nonEnglishNativeLanguages))$workerid)


d <- d %>% 
  filter(!(workerid %in% excludedWorkers)) %>%
  filter(!(workerid %in% excludedWorkers_nonNative))

# DATA TRANSFORMATIONS: 

# # d_bytrial: for every trial, compute participant's 'error' (operationalized as their population judgment minus true proportion of participants providing response)

d_bytrial <- d %>%
  # filter on critical trials
  filter(version %in% c("controversial","covered","uncovered")) %>%
  select(workerid,item,version,center_embedding,passive,individual_judgment,population_judgment,confidence) %>%
  # for each trial type (item/version/center_embedding/passive), get total # of participants who provided any type of response:
  group_by(item,version,center_embedding,passive) %>%
  mutate(total = n()) %>%
  ungroup() %>%
  # then count responses by type ('yes' or 'no') for each trial type:
  group_by(item,version,center_embedding,passive,individual_judgment) %>%
  mutate(count = n()) %>%
  # get true response proportions (+ confidence intervals), plus mean population estimates
  mutate(true_proportion = binom.confint(x = count, n  = total, methods = "exact")$mean * 100, 
         proportion_ci_low = binom.confint(x = count, n  = total, methods = "exact")$lower * 100,
         proportion_ci_high = binom.confint(x = count, n  = total, methods = "exact")$upper * 100) %>%
  # get individual 'error' per trial 
  mutate(error = as.numeric(population_judgment) - true_proportion) %>%
  mutate(versionPretty = factor(version))  %>%
  # numeric coding for passive and center_embedding (to facilitate centering)
  mutate(passive_numeric = case_when(passive == "yes" ~ 1,
                                     TRUE ~ 0),
         center_embedding_numeric = case_when(center_embedding == "yes" ~ 1,
                                              TRUE ~ 0),
         individual_judgment_numeric = case_when(individual_judgment == "yes" ~ 1,
                                                 TRUE ~ 0)) %>%
  # syntax condition coding 
  mutate(syntax_condition = case_when(center_embedding == "yes" & passive == "yes" ~ "CEP",
                                      center_embedding == "yes" & passive == "no" ~ "CEA",
                                      center_embedding == "no" & passive == "yes" ~ "NCEP",
                                      TRUE ~ "NCEA"))

levels(d_bytrial$versionPretty)
levels(d_bytrial$versionPretty) <- c("Controversial", "Covered", "Not\ncovered")

# # d_bytrialtype: for every trial type (combination of item, version, center_embedding, passive), 
# # compute the shannon entropy of response

props_bytrialtype <- d_bytrial %>%
  group_by(item, version, center_embedding, passive) %>% 
  mutate(yes = case_when(individual_judgment == "yes" ~ 1,
                         TRUE ~ 0),
         no = case_when(individual_judgment == "no" ~ 1,
                        TRUE ~ 0)) %>% 
  summarise(
            nYes = sum(yes), 
            nNo =sum(no),
            propYes = sum(yes)/n(), propNo = sum(no)/n(), 
            
            meanAgreementEstimate = mean(population_judgment),
            ciLowAgreementEstimate = ci.low(population_judgment),
            ciHighAgreementEstimate = ci.high(population_judgment),
            meanAgreeYMin = as.numeric(meanAgreementEstimate - ci.low(meanAgreementEstimate)), 
            meanAgreeYMax = as.numeric(meanAgreementEstimate + ci.high(meanAgreementEstimate)),
            
            ciLowError = ci.low(error),
            ciHighError = ci.high(error),
            meanError = mean(error),
            
            entropy = (propYes * log2(1/propYes)) + (propNo * log2(1/propNo))
            
            ) %>%
  mutate(entropy = case_when(is.nan(entropy) ~ 0,
                             TRUE ~ entropy)
         ) %>% 
  ungroup() %>% 
  mutate(version = relevel(as.factor(version), ref = "uncovered")) %>%
  # numeric coding for passive and center_embedding (to facilitate centering)
  mutate(passive_numeric = case_when(passive == "yes" ~ 1,
                                     TRUE ~ 0),
         center_embedding_numeric = case_when(center_embedding == "yes" ~ 1,
                                              TRUE ~ 0)) %>%
  ungroup()

# MEAN AND BOOTSTRAPPED 95% CI OF ERROR FOR EACH SYNTAX CONDITION, 'CONTROVERSIAL' STIMS ONLY:

error_CEyes_Pyes = 
  (d_bytrial %>% 
     filter(version == "controversial",
            center_embedding == "yes",
            passive == "yes"))$error

error_CEno_Pyes = 
  (d_bytrial %>% 
     filter(version == "controversial",
            center_embedding == "no",
            passive == "yes"))$error

error_CEyes_Pno = 
  (d_bytrial %>% 
     filter(version == "controversial",
            center_embedding == "yes",
            passive == "no"))$error

error_CEno_Pno = 
  (d_bytrial %>% 
     filter(version == "controversial",
            center_embedding == "no",
            passive == "no"))$error

# COMPUTE 95% CI OF INDIVIDUAL ERROR FOR EACH DATA SUBSET

mean_CEyes_Pyes = mean(error_CEyes_Pyes)
YMin_CEyes_Pyes = mean_CEyes_Pyes - ci.low(error_CEyes_Pyes)
YMax_CEyes_Pyes = mean_CEyes_Pyes + ci.high(error_CEyes_Pyes)

mean_CEyes_Pno = mean(error_CEyes_Pno)
YMin_CEyes_Pno = mean_CEyes_Pno - ci.low(error_CEyes_Pno)
YMax_CEyes_Pno = mean_CEyes_Pno + ci.high(error_CEyes_Pno)

mean_CEno_Pno = mean(error_CEno_Pno)
YMin_CEno_Pno = mean_CEno_Pno - ci.low(error_CEno_Pno)
YMax_CEno_Pno = mean_CEno_Pno + ci.high(error_CEno_Pno)

mean_CEno_Pyes = mean(error_CEno_Pyes)
YMin_CEno_Pyes = mean_CEno_Pyes - ci.low(error_CEno_Pyes)
YMax_CEno_Pyes = mean_CEno_Pyes + ci.high(error_CEno_Pyes)

# REGRESSION ANALYSIS: USE 95% CI OF INTERCEPT TO DETERMINE WHETHER MEAN ERROR > 0: 

options(mc.cores = parallel::detectCores())

m1_CEP <- brm(error ~ relevel(as.factor(syntax_condition), ref = "CEP") + 
                (1 + relevel(as.factor(syntax_condition), ref = "CEP")|item) +
                (1|workerid),
          data = d_bytrial %>% 
            filter(version == "controversial"))

m1_CEA <- brm(error ~ relevel(as.factor(syntax_condition), ref = "CEA") + 
                (1 + relevel(as.factor(syntax_condition), ref = "CEA")|item) +
                (1|workerid),
              data = d_bytrial %>% 
                filter(version == "controversial"))

m1_NCEP <- brm(error ~ relevel(as.factor(syntax_condition), ref = "NCEP") + 
                (1 + relevel(as.factor(syntax_condition), ref = "NCEP")|item) +
                (1|workerid),
              data = d_bytrial %>% 
                filter(version == "controversial"))

m1_NCEA <- brm(error ~ relevel(as.factor(syntax_condition), ref = "NCEA") + 
                (1 + relevel(as.factor(syntax_condition), ref = "NCEA")|item) +
                (1|workerid),
              data = d_bytrial %>% 
                filter(version == "controversial"))

summary(m1_CEP)
hypothesis(m1_CEP, "Intercept > 0")

summary(m1_CEA)
hypothesis(m1_CEA, "Intercept > 0")

summary(m1_NCEP)
hypothesis(m1_NCEP, "Intercept > 0")

summary(m1_NCEA)
hypothesis(m1_NCEA, "Intercept > 0")

# REGRESSION ANALYSES: Q1 ENTROPY AND Q2 RESPONSE FROM SYNTAX CONDITIONS

# # MODEL 1: Q1 ENTROPY FROM CENTERED FIXED EFFECTS OF CTR. EMBEDDING, PASSIVE, CONDITION, AND THEIR INTERACTION, W/ MAX RANEF. STRUCTURE JUSTIFIED BY DESIGN. 

centered_m2 <- cbind(props_bytrialtype, myCenter(data.frame(props_bytrialtype[,c("passive_numeric","center_embedding_numeric")])))

m2 <- brm(entropy ~ cpassive_numeric * ccenter_embedding_numeric * version + (1 + (cpassive_numeric * ccenter_embedding_numeric * version)|item),
          data = centered_m2)

summary(m3)

# # MODEL 2: Q2 RESPONSE FROM CENTERED FIXED EFFECTS OF CTR. EMBEDDING, PASSIVE, INDIVIDUAL RESPONSE TYPE, EMPIRICAL RESPONSE PROPORTION, AND THEIR INTERACTION, W/ MAX RANEF. STRUCTURE JUSTIFIED BY DESIGN. 

centered_m3 <- cbind(d_bytrial, myCenter(data.frame(d_bytrial[,c("passive_numeric","center_embedding_numeric","individual_judgment_numeric","true_proportion")])))

m3 <- brm(population_judgment ~ cpassive_numeric * ccenter_embedding_numeric * cindividual_judgment_numeric * ctrue_proportion + (1 + (cpassive_numeric * ccenter_embedding_numeric * cindividual_judgment_numeric * ctrue_proportion)|item) + (1 + (cpassive_numeric * ccenter_embedding_numeric * cindividual_judgment_numeric * ctrue_proportion)|workerid),
          data = centered_m3)

summary(m3)



fisherTest <- oneSamplePermutationTest((transformed %>% filter(version == "controversial"))$difference, 
                                       alternative = "greater", mu = 0, exact = FALSE, 
                                       n.permutations = 10000, seed = NULL)
fisherTest


mean = mean((transformed %>% filter(version == "controversial"))$difference)
ciLow = mean - ci.low((transformed %>% filter(version == "controversial"))$difference)
ciHigh = mean + ci.high((transformed %>% filter(version == "controversial"))$difference)
