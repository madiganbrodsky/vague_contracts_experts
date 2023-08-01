library(tidyverse)
library(jsonlite)
library(Hmisc)
library(binom)
library(brms)
library(EnvStats)
library(lme4)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

theme_set(theme_bw())
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

source("../helpers.R")

# load core dataset
d <- read_csv("../../data/1_falseconsensus_/vague_contracts_syntax-merged.csv")

# load extra information
metadata = read_csv("../../experiments/2_falseconsensus_syntax/pilot/stimuli.csv") %>% 
  select(item, version, center_embedding, passive)

d = d %>% 
  left_join(metadata,by=c("item","version", "center_embedding", "passive")) %>% 
  filter(grepl('Escape Of Oil|Vehicle Glass|Emergency Damages|Malicious Acts Or Vandalism|Ground Heave|Storm Damage|Vehicle Fire|Personal Accident|House Removal', item)) %>% 
  filter(center_embedding == "no")


#################################EXCLUSIONS#################################

#EXCLUDE WORKERS WHO MAKE AN ERROR ON THE FILLER TRIALS 

excludedWorkers <- (d %>%
                      filter((version == "filler_uncovered" & individual_judgment == "yes") |
                               (version == "filler_covered" & individual_judgment == "no")) %>%
                      group_by(workerid) %>%
                      summarise(n = n()) %>%
                      filter(n > 1))$workerid

#EXCLUDE WORKERS WHO SELF-REPORT A NATIVE LANGUAGE OTHER THAN ENGLISH
nonEnglishNativeLanguages <- c()

excludedWorkers_nonNative <- unique((d %>%
                                       filter(subject_information.language %in% nonEnglishNativeLanguages))$workerid)


d <- d %>% 
  filter(!(workerid %in% excludedWorkers)) %>%
  filter(!(workerid %in% excludedWorkers_nonNative))

########## DATA TRANSFORMATIONS ######### 

transformed <- d %>%
  select(workerid,item,version,individual_judgment,population_judgment,confidence,center_embedding, passive) %>%
  group_by(version,item) %>%
  mutate(total = n()) %>%
  ungroup() %>%
  group_by(version,item,individual_judgment, center_embedding, passive) %>%
  mutate(count = n()) %>%
  mutate(true_proportion = binom.confint(x = count, n  = total, methods = "exact")$mean * 100, 
         ci_low = binom.confint(x = count, n  = total, methods = "exact")$lower * 100,
         ci_high = binom.confint(x = count, n  = total, methods = "exact")$upper * 100,
         agreement_mean = mean(population_judgment)) %>%
  mutate(difference = as.numeric(population_judgment) - true_proportion) %>%
  mutate(versionPretty = factor(version))

levels(transformed$versionPretty)
levels(transformed$versionPretty) <- c("Controversial", "Covered", "Not\ncovered")

transformed %>%
  group_by(version) %>%
  mutate(yes = case_when(individual_judgment == "yes" ~ 1,
                         TRUE ~ 0),
         no = case_when(individual_judgment == "no" ~ 1,
                        TRUE ~ 0)) %>%
  summarise(nYes = sum(yes), nNo =sum(no),
            propYes = sum(yes)/n(), propNo = sum(no)/n())

################################# REGRESSIONS#################################

# # MODEL 1: Q1 ENTROPY FROM CENTERED FIXED EFFECTS OF CTR. EMBEDDING, PASSIVE, CONDITION, AND THEIR INTERACTION, W/ MAX RANEF. STRUCTURE JUSTIFIED BY DESIGN. 

options(mc.cores = parallel::detectCores())

centered_m1 <- cbind(props, myCenter(data.frame(props[,c("passive","center_embedding")])))

m1 <- brm(entropy ~ cpassive * ccenter_embedding * version + (1 + (cpassive * ccenter_embedding * version)|item),
          data = centered_m1)
# 
# ##################################################################################################
# 
# # # MODEL 2: Q2 RESPONSE FROM CENTERED FIXED EFFECTS OF CTR. EMBEDDING, PASSIVE, INDIVIDUAL RESPONSE TYPE, EMPIRICAL RESPONSE PROPORTION, AND THEIR INTERACTION, W/ MAX RANEF. STRUCTURE JUSTIFIED BY DESIGN. 

options(mc.cores = parallel::detectCores())

centered_m2 <- cbind(transformed, myCenter(data.frame(transformed[,c("passive","center_embedding","individual_judgment","true_proportion")])))

m2 <- brm(population_judgment ~ cpassive * ccenter_embedding * cindividual_judgment * ctrue_proportion + (1 + (cpassive * ccenter_embedding * cindividual_judgment * ctrue_proportion)|item) + (1 + (cpassive * ccenter_embedding * cindividual_judgment * ctrue_proportion)|workerid),
          data = centered_m2)

##########################################################################

# FISHER ONE-SAMPLE TEST TO DETERMINE WHETHER (SUBJECTIVE JUDGMENT) - (TRUE PROPORTION) > 0

fisherTest <- oneSamplePermutationTest((transformed %>% filter(version == "controversial"))$difference, 
                                       alternative = "greater", mu = 0, exact = FALSE, 
                                       n.permutations = 10000, seed = NULL)
fisherTest


mean = mean((transformed %>% filter(version == "controversial"))$difference)
ciLow = mean - ci.low((transformed %>% filter(version == "controversial"))$difference)
ciHigh = mean + ci.high((transformed %>% filter(version == "controversial"))$difference)

#################################PLOTS#########################################

# bar plot of response proportions alongside agreement estimates
props = transformed %>% 
  mutate(Yes = case_when(individual_judgment == "yes" ~ 1,
                         TRUE ~ 0),
         No = case_when(individual_judgment == "no" ~ 1,
                        TRUE ~ 0),
         CantDecide = case_when(individual_judgment == "cantdecide" ~ 1,
                                TRUE ~ 0)) %>% 
  group_by(version,center_embedding, passive) %>% 
  summarise(ProportionYes = mean(Yes), YesCILow=ci.low(Yes), YesCIHigh=ci.high(Yes),
            ProportionNo = mean(No), NoCILow=ci.low(No), NoCIHigh=ci.high(No)) %>% 
  ungroup() %>% 
  mutate(YesYMin=ProportionYes-YesCILow,YesYMax=ProportionYes+YesCIHigh,
         NoYMin=ProportionNo-NoCILow,NoYMax=ProportionNo+NoCIHigh)

yes = props %>% 
  select(version,ProportionYes,YesYMax,YesYMin,center_embedding, passive) %>% 
  rename(Proportion=ProportionYes,YMax=YesYMax,YMin=YesYMin) %>% 
  mutate(Response="yes",ResponseType="individual")

no = props %>% 
  select(version,ProportionNo,NoYMax,NoYMin, center_embedding, passive) %>% 
  rename(Proportion=ProportionNo,YMax=NoYMax,YMin=NoYMin) %>% 
  mutate(Response="no",ResponseType="individual")

pop_judgments = transformed %>% 
  group_by(version,individual_judgment, center_embedding, passive) %>% 
  summarise(Proportion = mean(population_judgment/100), CILow=ci.low(population_judgment/100), CIHigh=ci.high(population_judgment/100)) %>% 
  ungroup() %>% 
  mutate(Response=individual_judgment,ResponseType="agreement_estimate", YMin=Proportion-CILow,YMax=Proportion+CIHigh) %>% 
  select(-individual_judgment)

responses = bind_rows(yes,no,pop_judgments) %>% 
  mutate(Condition = fct_relevel(version,"unambiguous_covered","controversial"),
         ResponseType = fct_relevel(ResponseType,"individual"),         
         Response = fct_relevel(Response,"yes","no")) 

dodge=position_dodge(.9)

levels(responses$ResponseType) <- c("Individual","Agreement estimate")

passive.labs <- c("Passive", "Not Passive")
names(passive.labs) <- c("yes", "no")

center_embedding.labs <- c("Center Embedding", "No Center Embedding")
names(passive.labs) <- c("yes", "no")

ggplot(responses, aes(x=Response,y=Proportion,fill=Condition,alpha=ResponseType)) +
  geom_bar(stat="identity",position=dodge) +
  facet_wrap(center_embedding~passive, labeller = labeller(passive = passive.labs)) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.2,position=dodge) +
  scale_fill_manual(values=cbPalette,labels=c("unambiguous_covered"="Covered","unambiguous_uncovered"="Not covered", "controversial" = "Controversial")) +
  scale_alpha_discrete(range = c(1, .4),name="Response type",labels=c("agreement_estimate"="agreement\nestimate")) +
  ylab("Proportion of responses") +
  scale_x_discrete(labels=c("yes"="Yes","no"="No")) +
  theme(legend.position="top",legend.direction="vertical",
        text=element_text(size=14))
ggsave("graphs/responses.pdf",width=5,height=3.5)

names(transformed)
nrow(transformed)

# plot agreement against response proportions
perfect_estimates = data.frame(x=c(0,.5,1),y=c(1,.5,1))

d_item_responses = as_tibble(unique(transformed[,c("true_proportion","agreement_mean","individual_judgment","item","version")])) %>% 
  mutate(Condition=fct_relevel(version,"unambiguous_covered"),
         individual_judgment = factor(individual_judgment))
levels(d_item_responses$individual_judgment) <- c("No","Yes")
ggplot(d_item_responses %>% filter(individual_judgment!="cantdecide")) +
  geom_point(aes(x=true_proportion/100,y=agreement_mean/100,color=Condition)) +
  geom_smooth(aes(x=true_proportion/100,y=agreement_mean/100,color=Condition)) +
  # geom_point(data=perfect_estimates, aes(x=x,y=y)) +
  # geom_line(data=perfect_estimates, aes(x=x,y=y,group=1),linetype="dashed") +
  geom_abline(intercept=0,slope=1,linetype="dashed") +
  scale_color_manual(values=cbPalette,name="Condition",labels=c("unambiguous_covered"="Covered","unambiguous_uncovered"="Not covered", "controversial" = "Controversial")) +
  xlim(0,1) +
  ylim(0,1) +
  xlab("Proportion of response") +
  ylab("Mean agreement estimate") +
  facet_wrap(~individual_judgment) + #facet by version 
  theme(text=element_text(size=15),
        legend.position = "bottom")
ggsave("graphs/props_vs_agreement.pdf",width=7.5,height=4)

#mean agreement vs. proportion of responses by conditions

perfect_estimates = data.frame(x=c(0,.5,1),y=c(1,.5,1))

d_item_responses = as_tibble(unique(transformed[,c("true_proportion","agreement_mean","individual_judgment","item","version", "center_embedding", "passive")])) %>% 
  mutate(Condition=fct_recode(version,"Not covered"="unambiguous_uncovered","Covered"="unambiguous_covered","Controversial"="controversial")) %>% 
  mutate(Condition=fct_relevel(Condition,"Not covered","Controversial"),
         individual_judgment = factor(individual_judgment))
levels(d_item_responses$individual_judgment) <- c("No","Yes")
ggplot(d_item_responses %>% filter(individual_judgment!="cantdecide")) +
  geom_point(aes(x=true_proportion/100,y=agreement_mean/100,color=individual_judgment)) +
  geom_smooth(method="lm",aes(x=true_proportion/100,y=agreement_mean/100,color=individual_judgment)) +
  # geom_point(data=perfect_estimates, aes(x=x,y=y)) +
  # geom_line(data=perfect_estimates, aes(x=x,y=y,group=1),linetype="dashed") +
  geom_abline(intercept=0,slope=1,linetype="dashed") +
  scale_color_manual(values=c(cbPalette[4],cbPalette[8]),name="Individual judgment type") +
  xlim(0,1) +
  ylim(0,1) +
  xlab("Proportion of Judgment") +
  ylab("Mean Agreement Estimate") +
  facet_grid(passive ~ version ~center_embedding, labeller = labeller(passive = passive.labs)) + #facet by version 
  theme(text=element_text(size=15),
        legend.position = "bottom")
ggsave("graphs/props_vs_agreement_bycondition.pdf",width=7,height=3.5)

