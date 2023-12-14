#Here's the code to run some multiple regression!
#Functions I'll be using today
#library(foreign)
#library(haven)
library(dplyr)
library(plyr)


#First, set wd
setwd("~/pos word-level")

#Let's load in our dataframe.
single_t_data <- read.csv('single_R_Data.csv', header = TRUE)
prior_t_data <- read.csv('prior_R_Data.csv', header = TRUE)


# Does "paired semantic-similarity" predict word reading miscues after controlling for
  #word length and word frequency?
paired_model1 <- lm(single_t_data$miscue ~ single_t_data$single_primer +
                      single_t_data$word_len + single_t_data$totallog10)
summary(paired_model1)

#Does "coherence semantic-similarity" predict word reading miscues after controlling for
  #word length and word frequency?
cohere_model1 <- lm(prior_t_data$miscue ~ prior_t_data$prior_sim_val+
                      prior_t_data$word_len + prior_t_data$totallog10)
summary(cohere_model1)

## ALL IN ONE MODEL
all_model1 <- lm(prior_t_data$miscue ~ prior_t_data$prior_sim_val +
                    prior_t_data$single_primer + prior_t_data$totallog10 +
                    prior_t_data$word_len)
summary(all_model1)

#Model without coherence similarity 
no_cohere_model1 <- lm(prior_t_data$miscue ~ 
                        prior_t_data$single_primer + prior_t_data$totallog10 +
                        prior_t_data$word_len) 
summary(no_cohere_model1)

#Model without paired similarity 
no_paired_model1 <- lm(prior_t_data$miscue ~ 
                         prior_t_data$prior_sim_val + prior_t_data$totallog10 +
                         prior_t_data$word_len) 

summary(no_paired_model1)
