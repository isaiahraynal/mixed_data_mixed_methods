## LOAD PACKAGES ##

library(knitr)
library(dplyr)
library(evaluate)
library(ggplot2)
library(cowplot)
library(readr)
library(float)
library(tidyverse)
library(xtable)
options(xtable.comment = FALSE)
library(psych)
library(corrplot)
library("psych")
library(car)

## IMPORT SURVEY DATA ##

setwd("~/Desktop/Unauthorized immigrant framing/mixed methods journal/") #set working directory to appropriate folder
#Tommy - you will have to change the working directory above for yourself
uaisurvey <- read_csv("survey.csv") #read in csv file
View(uaisurvey) #view survey results
uaisurvey <- uaisurvey[-c(1,2),] #remove unnecessary rows and/or columns
View(uaisurvey) #view survey results again

## RECODE VARIABLES ##

#1 Honest
uaisurvey$ill_honest <- uaisurvey$Q2.1_1 #rename question so we know which variable it refers to
uaisurvey$ill_honest[uaisurvey$ill_honest == "1 (not well at all)"] <- 1 #change to just say 1
uaisurvey$ill_honest[uaisurvey$ill_honest == "5 (very well)"] <- 5 #change to just say 5

uaisurvey$undoc_honest <- uaisurvey$Q2.2_1
uaisurvey$undoc_honest[uaisurvey$undoc_honest == "1 (not well at all)"] <- 1
uaisurvey$undoc_honest[uaisurvey$undoc_honest == "5 (very well)"] <- 5

uaisurvey$honest <- ifelse(is.na(uaisurvey$ill_honest), uaisurvey$undoc_honest, uaisurvey$ill_honest) #combine treatments into one
uaisurvey$honest <- as.numeric(uaisurvey$honest) #make numeric

#2 Hardworking

uaisurvey$ill_hardworking <- uaisurvey$Q2.1_2
uaisurvey$ill_hardworking[uaisurvey$ill_hardworking == "1 (not well at all)"] <- 1
uaisurvey$ill_hardworking[uaisurvey$ill_hardworking == "5 (very well)"] <- 5

uaisurvey$undoc_hardworking <- uaisurvey$Q2.2_2
uaisurvey$undoc_hardworking[uaisurvey$undoc_hardworking == "1 (not well at all)"] <- 1
uaisurvey$undoc_hardworking[uaisurvey$undoc_hardworking == "5 (very well)"] <- 5

uaisurvey$hardworking <- ifelse(is.na(uaisurvey$ill_hardworking), uaisurvey$undoc_hardworking, uaisurvey$ill_hardworking)
uaisurvey$hardworking <- as.numeric(uaisurvey$hardworking)

#3 Unlawful

uaisurvey$ill_unlawful <- uaisurvey$Q2.1_3
uaisurvey$ill_unlawful[uaisurvey$ill_unlawful == "1 (not well at all)"] <- 1
uaisurvey$ill_unlawful[uaisurvey$ill_unlawful == "5 (very well)"] <- 5

uaisurvey$undoc_unlawful <- uaisurvey$Q2.2_3
uaisurvey$undoc_unlawful[uaisurvey$undoc_unlawful == "1 (not well at all)"] <- 1
uaisurvey$undoc_unlawful[uaisurvey$undoc_unlawful == "5 (very well)"] <- 5

uaisurvey$unlawful <- ifelse(is.na(uaisurvey$ill_unlawful), uaisurvey$undoc_unlawful, uaisurvey$ill_unlawful)
uaisurvey$unlawful <- as.numeric(uaisurvey$unlawful)

#4 Violent

uaisurvey$ill_violent <- uaisurvey$Q2.1_4
uaisurvey$ill_violent[uaisurvey$ill_violent == "1 (not well at all)"] <- 1
uaisurvey$ill_violent[uaisurvey$ill_violent == "5 (very well)"] <- 5

uaisurvey$undoc_violent <- uaisurvey$Q2.2_4
uaisurvey$undoc_violent[uaisurvey$undoc_violent == "1 (not well at all)"] <- 1
uaisurvey$undoc_violent[uaisurvey$undoc_violent == "5 (very well)"] <- 5

uaisurvey$violent <- ifelse(is.na(uaisurvey$ill_violent), uaisurvey$undoc_violent, uaisurvey$ill_violent)
uaisurvey$violent <- as.numeric(uaisurvey$violent)

#5 Intelligent

uaisurvey$ill_intelligent <- uaisurvey$Q2.1_5
uaisurvey$ill_intelligent[uaisurvey$ill_intelligent == "1 (not well at all)"] <- 1
uaisurvey$ill_intelligent[uaisurvey$ill_intelligent == "5 (very well)"] <- 5

uaisurvey$undoc_intelligent <- uaisurvey$Q2.2_5
uaisurvey$undoc_intelligent[uaisurvey$undoc_intelligent == "1 (not well at all)"] <- 1
uaisurvey$undoc_intelligent[uaisurvey$undoc_intelligent == "5 (very well)"] <- 5

uaisurvey$intelligent <- ifelse(is.na(uaisurvey$ill_intelligent), uaisurvey$undoc_intelligent, uaisurvey$ill_intelligent)
uaisurvey$intelligent <- as.numeric(uaisurvey$intelligent)

#6 Uneducated

uaisurvey$ill_uneducated <- uaisurvey$Q2.1_6
uaisurvey$ill_uneducated[uaisurvey$ill_uneducated == "1 (not well at all)"] <- 1
uaisurvey$ill_uneducated[uaisurvey$ill_uneducated == "5 (very well)"] <- 5

uaisurvey$undoc_uneducated <- uaisurvey$Q2.2_6
uaisurvey$undoc_uneducated[uaisurvey$undoc_uneducated == "1 (not well at all)"] <- 1
uaisurvey$undoc_uneducated[uaisurvey$undoc_uneducated == "5 (very well)"] <- 5

uaisurvey$uneducated <- ifelse(is.na(uaisurvey$ill_uneducated), uaisurvey$undoc_uneducated, uaisurvey$ill_uneducated)
uaisurvey$uneducated <- as.numeric(uaisurvey$uneducated)

#7 Lazy

uaisurvey$ill_lazy <- uaisurvey$Q2.1_7
uaisurvey$ill_lazy[uaisurvey$ill_lazy == "1 (not well at all)"] <- 1
uaisurvey$ill_lazy[uaisurvey$ill_lazy == "5 (very well)"] <- 5

uaisurvey$undoc_lazy <- uaisurvey$Q2.2_7
uaisurvey$undoc_lazy[uaisurvey$undoc_lazy == "1 (not well at all)"] <- 1
uaisurvey$undoc_lazy[uaisurvey$undoc_lazy == "5 (very well)"] <- 5

uaisurvey$lazy <- ifelse(is.na(uaisurvey$ill_lazy), uaisurvey$undoc_lazy, uaisurvey$ill_lazy)
uaisurvey$lazy <- as.numeric(uaisurvey$lazy)

#8 Immoral

uaisurvey$ill_immoral <- uaisurvey$Q2.1_8
uaisurvey$ill_immoral[uaisurvey$ill_immoral == "1 (not well at all)"] <- 1
uaisurvey$ill_immoral[uaisurvey$ill_immoral == "5 (very well)"] <- 5

uaisurvey$undoc_immoral <- uaisurvey$Q2.2_8
uaisurvey$undoc_immoral[uaisurvey$undoc_immoral == "1 (not well at all)"] <- 1
uaisurvey$undoc_immoral[uaisurvey$undoc_immoral == "5 (very well)"] <- 5

uaisurvey$immoral <- ifelse(is.na(uaisurvey$ill_immoral), uaisurvey$undoc_immoral, uaisurvey$ill_immoral)
uaisurvey$immoral <- as.numeric(uaisurvey$immoral)

#9 Loyal

uaisurvey$ill_loyal <- uaisurvey$Q2.1_9
uaisurvey$ill_loyal[uaisurvey$ill_loyal == "1 (not well at all)"] <- 1
uaisurvey$ill_loyal[uaisurvey$ill_loyal == "5 (very well)"] <- 5

uaisurvey$undoc_loyal <- uaisurvey$Q2.2_9
uaisurvey$undoc_loyal[uaisurvey$undoc_loyal == "1 (not well at all)"] <- 1
uaisurvey$undoc_loyal[uaisurvey$undoc_loyal == "5 (very well)"] <- 5

uaisurvey$loyal <- ifelse(is.na(uaisurvey$ill_loyal), uaisurvey$undoc_loyal, uaisurvey$ill_loyal)
uaisurvey$loyal <- as.numeric(uaisurvey$loyal)

#10 Responsible

uaisurvey$ill_responsible <- uaisurvey$Q2.1_10
uaisurvey$ill_responsible[uaisurvey$ill_responsible == "1 (not well at all)"] <- 1
uaisurvey$ill_responsible[uaisurvey$ill_responsible == "5 (very well)"] <- 5

uaisurvey$undoc_responsible <- uaisurvey$Q2.2_10
uaisurvey$undoc_responsible[uaisurvey$undoc_responsible == "1 (not well at all)"] <- 1
uaisurvey$undoc_responsible[uaisurvey$undoc_responsible == "5 (very well)"] <- 5

uaisurvey$responsible <- ifelse(is.na(uaisurvey$ill_responsible), uaisurvey$undoc_responsible, uaisurvey$ill_responsible)
uaisurvey$responsible <- as.numeric(uaisurvey$responsible)

## MEAN STEREOTYPE RATINGS ##

mean(uaisurvey$hardworking, na.rm = TRUE) #take the mean rating for the hardworking variable and remove NAs
mean(uaisurvey$responsible, na.rm = TRUE)
mean(uaisurvey$intelligent, na.rm = TRUE)
mean(uaisurvey$loyal, na.rm = TRUE)
mean(uaisurvey$honest, na.rm = TRUE)
mean(uaisurvey$uneducated, na.rm = TRUE)
mean(uaisurvey$unlawful, na.rm = TRUE)
mean(uaisurvey$immoral, na.rm = TRUE)
mean(uaisurvey$violent, na.rm = TRUE)
mean(uaisurvey$lazy, na.rm = TRUE)

#Table 1 showing mean stereotype ratings
allwords_matrix <- matrix(c(4.29, 3.65, 3.55, 3.53, 3.39, 2.37, 2.33, 1.75, 1.74, 1.59), ncol = 1, byrow = TRUE)
colnames(allwords_matrix) <- "Mean rating"
rownames(allwords_matrix) <- c("Hardworking", "Responsible", "Intelligent", "Loyal", "Honest", "Uneducated", "Unlawful", "Immoral", "Violent", "Lazy")
allwords_t <- as.table(allwords_matrix)

allwords_table <- xtable(allwords_t, caption = "Overall mean ratings")
allwords_table

## TABLE 2 SHOWING CONTENT ANALYSIS RESULTS ##
  
#include most common topics
  
open_matrix_mini <- matrix(c(161, 144, 98, 68, 68, 54, 46, 0.97, 0.98, 0.98, 0.94, 0.94, 0.97, 0.96, 0.93, 0.96, 0.93, 0.80, 0.75, 0.89, 0.81), ncol = 3, byrow = FALSE)
colnames(open_matrix_mini) <- c("N", "Intercoder agreement", "Krippendorff's Alpha")
rownames(open_matrix_mini) <- c("Better life", "Hardworking", "Family-oriented", "Unlawful", "Fleeing/escaping hardship", "No typical unauthorized immigrant", "Criticize US immigration process")
o_table_mini <- as.table(open_matrix_mini)

open_table_mini <- xtable(o_table_mini, caption = "Content analysis results", digits = c(0,0,2,2))

open_table_mini
  
  
  \end{landscape}

## COMBINE CLOSED AND OPEN ENDED RESPONSES ##

openended <- read_csv("openended.csv", skip = 1) #load in openended coding
View(openended) 

#create unique respondent IDs
create_unique_ids <- function(n, seed_no = 1, char_len = 5){
  set.seed(seed_no)
  pool <- c(letters, LETTERS, 0:9)
  res <- character(n) # pre-allocating vector is much faster than growing it
  for(i in seq(n)){
    this_res <- paste0(sample(pool, char_len, replace = TRUE), collapse = "")
    while(this_res %in% res){ # if there was a duplicate, redo
      this_res <- paste0(sample(pool, char_len, replace = TRUE), collapse = "")
    }
    res[i] <- this_res
  }
  res
}

uaisurvey$ID <- create_unique_ids(412)

write_csv(uaisurvey, file = "uaisurvey.csv")

fullsurvey <- full_join(uaisurvey, openended, by = "ID") #attempt to join by ID
write_csv(fullsurvey, file = "fullsurvey.csv")

## DATA ANALYSIS COMPARING CLOSED- AND OPEN-ENDED RESPONSES


#unlawful lower case is closed, Unlawful upper case is open
mean(fullsurvey$unlawful[fullsurvey$Unlawful == 0], na.rm = TRUE) #mean unlawful rating for respondents who did not talk about unlawfulness in open-ended response
mean(fullsurvey$unlawful[fullsurvey$Unlawful == 1], na.rm = TRUE) #mean unlawful rating for respondents who did talk about unlawfulness in open-ended response
#2.15 versus 3.24, so we can see it is higher for people who discussed unlawfulness in response, but still close to the median

fullsurvey %>%
  ggplot(aes(x = unlawful)) +
  geom_bar() +
  facet_wrap(~Unlawful)

## HOMEWORK FOR TOMMY



\end{document}