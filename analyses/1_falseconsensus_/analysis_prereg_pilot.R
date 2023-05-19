library(tidyverse)
library(jsonlite)
library(binom)
library(brms)
library(DescTools)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("../helpers.R")

theme_set(theme_bw())

d <- read_csv("../../data/1_falseconsensus_/vague_contracts_syntax-merged.csv")

#################################EXCLUSIONS#################################

#EXCLUDE WORKERS WHO MAKE AN ERROR ON THE FILLER TRIALS 

excludedWorkers <- (d %>%
                      filter((version == "filler_uncovered" & individual_judgment == "yes") |
                               (version == "filler_covered" & individual_judgment == "no")) %>%
                      group_by(workerid) %>%
                      summarise(n = n()) %>%
                      filter(n > 0))$workerid

#EXCLUDE DUPLICATE WORKERS 
  duplicateworkerids <- c(2, 3, 4, 5, 6, 7, 8, 9)
  
  excludedWorkers_duplicate <- unique((d %>%
                                         filter(workerid %in% duplicateworkerids))$workerid)

#EXCLUDE WORKERS WHO SELF-REPORT A NATIVE LANGUAGE OTHER THAN ENGLISH
nonEnglishNativeLanguages <- c("Chinese", "Spanish", "spanish", "Cantonese", "Korean")

excludedWorkers_nonNative <- unique((d %>%
                                       filter(subject_information.language %in% nonEnglishNativeLanguages))$workerid)


d <- d %>% 
  filter(!(workerid %in% excludedWorkers)) %>%
  filter(!(workerid %in% excludedWorkers_duplicate)) %>% 
  filter(!(workerid %in% excludedWorkers_nonNative))

#################################DATA TRANSFORMATIONS#################################

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

################################# REGRESSIONS#################################

# # MODEL 1: Q1 ENTROPY FROM CENTERED FIXED EFFECTS OF CTR. EMBEDDING, PASSIVE, CONDITION, AND THEIR INTERACTION, W/ MAX RANEF. STRUCTURE JUSTIFIED BY DESIGN. 

options(mc.cores = parallel::detectCores())

centered_m1 <- cbind(props, myCenter(data.frame(props[,c("passive","center_embedding")])))   

m1 <- brm(entropy ~ cpassive * ccenter_embedding * version + (1 + (cpassive * ccenter_embedding * version)|item),
          data = centered_m1)

##################################################################################################

# # MODEL 2: Q2 RESPONSE FROM CENTERED FIXED EFFECTS OF CTR. EMBEDDING, PASSIVE, INDIVIDUAL RESPONSE TYPE, EMPIRICAL RESPONSE PROPORTION, AND THEIR INTERACTION, W/ MAX RANEF. STRUCTURE JUSTIFIED BY DESIGN. 

options(mc.cores = parallel::detectCores())

centered_m2 <- cbind(transformed, myCenter(data.frame(transformed[,c("passive","center_embedding","individual_judgment","true_proportion")])))   

m2 <- brm(population_judgment ~ cpassive * ccenter_embedding * cindividual_judgment * ctrue_proportion + (1 + (cpassive * ccenter_embedding * cindividual_judgment * ctrue_proportion)|item) + (1 + (cpassive * ccenter_embedding * cindividual_judgment * ctrue_proportion)|workerid),
          data = centered_m2)

#################################PLOTS#########################################
# plot 1: proportion of yes responses (y), syntax condition (x), facet (condition)
ggplot(data = props, 
       mapping = aes(x = syntax_condition, 
                     y = propYes)) + 
  geom_jitter(color = "darkmagenta", alpha = 0.5) + 
  stat_smooth(aes(group = version), fill = "lightblue", color = "darkmagenta", size = 1, alpha = 0.3, method = "lm") +
  facet_wrap(~version) + 
  ylab("Proportion of 'Yes' Responses for each item") + 
  xlab("Difficult-to-Process Language Presence")

##################################################################################

# plot 2: PROP YES VS. SYNTAX FOR 12 ITEMS (collapsed across coverage conditions) ??? 
 # ggplot(data = props,
 #        mapping = aes(x = syntax_condition,
 #                      y = propYes)) +
 #   geom_path(aes(color = item), alpha = 0.4) +
 #   geom_point(aes(color = item), alpha = 0.4)  
   
#################################################################################
   

# plot 3: mean agreement estimate (y), syntax condition (x), color (condition), facet (maj vs min in non-DPL case)
ggplot(data = transformed, 
       mapping = aes(x = syntax_condition, 
                     y = population_judgment)) + 
  geom_jitter(aes(color = individual_judgment), alpha = 0.4) + 
  stat_smooth(aes(group = version), fill = "lightblue", color = "darkmagenta", size = 1, alpha = 0.3, method = "lm") +
  facet_wrap(~version) + 
  ylab("Mean Agreement Estimates for each item") + 
  xlab("Difficult-to-Process Language Presence")
   
##################################################################################

# plot 4: entropy(y), syntax(x), (one with facet (y/n) and one collapsed)

entropy_plot <- props %>% 
  group_by(syntax_condition) 


  ggplot(data = entropy_plot, 
        mapping = aes(x = syntax_condition,
                     y = entropy)) +
  geom_bar() # +
  # facet_wrap(~individual_judgment)
   
##################################################################################

#plot 5: mean agreement estimate (y), prop of response (x), color (syntax), facet (yes vs no  & condition)
ggplot(data = transformed, 
       mapping = aes(x = true_proportion, #???? 
                     y = population_judgment)) + 
  geom_jitter(aes(color = syntax_condition), alpha = 0.5) + 
  stat_smooth(aes(group = syntax_condition, color = syntax_condition), size = 1, alpha = 0.25, method = "lm") +
  facet_wrap(~individual_judgment +version) + 
  xlab("Proportion of Responses for each item") + 
  ylab("Population Agreement Estimates for each item")

##################################################################################
#PLOT 6: Histogram of error / condition, facet by response type
   transformed %>%
     ggplot(aes(x = difference, fill = version)) + 
     geom_histogram(position="identity", alpha = 0.3) +
     xlab("Participant error") +
     ylab("Count") +
     labs(fill='Condition') +
     theme_bw() +
     theme(legend.position = "top",
           text = element_text(size=8)) + 
     guides(fill=guide_legend(nrow=1,byrow=TRUE)) + 
     facet_wrap(~individual_judgment)
   
##################################################################################
#PLOT 7: Histogram of error / condition, facet by syntax
   transformed %>%
     ggplot(aes(x = difference, fill = version)) +
     geom_histogram(position="identity", alpha = 0.3) +
     xlab("Participant error") +
     ylab("Count") +
     labs(fill='Condition') +
     theme_bw() +
     theme(legend.position = "top",
           text = element_text(size=8)) + 
     guides(fill=guide_legend(nrow=1,byrow=TRUE)) + 
     facet_wrap(~syntax_condition)
   
##################################################################################
#plot 8: CONFIDENCE vs. PROP OF RESPONSE
ggplot(data = transformed, 
       mapping = aes(x = confidence, 
                     y = true_proportion)) + 
  geom_jitter(aes(color = syntax_condition), alpha = 0.5) + 
  stat_smooth(aes(group = syntax_condition, color = syntax_condition), size = 1, alpha = 0.25, method = "lm") +
  facet_wrap(~individual_judgment +version) + 
  xlab("Confidence of Individual Interpretive Judgment") + 
  ylab("Proportion of Responses for each item") 
   
##################################################################################
#plot 9: CONFIDENCE vs. MEAN AGREEMENT ESTIMATE
ggplot(data = transformed, 
       mapping = aes(x = confidence, 
                     y = population_judgment)) + 
  geom_jitter(aes(color = syntax_condition), alpha = 0.5) + 
  stat_smooth(aes(group = syntax_condition, color = syntax_condition), size = 1, alpha = 0.25, method = "lm") +
  facet_wrap(~individual_judgment +version) + 
  xlab("Confidence of Individual Interpretive Judgment") + 
  ylab("Proportion Estimates of Agreement") 
   
##################################################################################



