## LOAD PACKAGES ##

library(dplyr)
library(evaluate)
library(ggplot2)
library(readr)
library(float)
library(tidyverse)
library(xtable)
options(xtable.comment = FALSE)
library(psych)
library(corrplot)
library("psych")
library(car)
library(ggpubr)
library(stats)

## IMPORT SURVEY DATA ##

setwd("~/Documents/GitHub/mixed_datamethods") #set working directory to appropriate folder
fullsurvey <- read.csv("fullsurvey.csv") #open relevant csv
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

#openended <- read_csv("openended.csv", skip = 1) #load in openended coding
#View(openended) 

#create unique respondent IDs
#create_unique_ids <- function(n, seed_no = 1, char_len = 5){
#  set.seed(seed_no)
#  pool <- c(letters, LETTERS, 0:9)
#  res <- character(n) # pre-allocating vector is much faster than growing it
#  for(i in seq(n)){
#    this_res <- paste0(sample(pool, char_len, replace = TRUE), collapse = "")
#    while(this_res %in% res){ # if there was a duplicate, redo
#      this_res <- paste0(sample(pool, char_len, replace = TRUE), collapse = "")
#    }
#    res[i] <- this_res
#  }
#  res
#}

#uaisurvey$ID <- create_unique_ids(412)

#write_csv(uaisurvey, file = "uaisurvey.csv")

#fullsurvey <- full_join(uaisurvey, openended, by = "ID") #attempt to join by ID
#write_csv(fullsurvey, file = "fullsurvey.csv")

#above is how I created fullsurvey but now that it is created we can upload it normally
fullsurvey <- read.csv("fullsurvey.csv")

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
# start this week, and we will finish next week
# no need to complete everything by Monday

## 1. Rename the following variables
fullsurvey <- fullsurvey %>% rename("age" = "Q4") # Q4 age
fullsurvey <- fullsurvey %>% rename("sex" = "Q5") # Q5 sex
fullsurvey <- fullsurvey %>% rename("race" = "Q6") # Q6 race
fullsurvey <- fullsurvey %>% rename("hispaniclatino" = "Q7") # Q7 hispaniclatino
fullsurvey <- fullsurvey %>% rename("party" = "Q8") # Q8 party
fullsurvey <- fullsurvey %>% rename("ideology" = "Q10") # Q10 ideology
fullsurvey <- fullsurvey %>% rename("immigrationstatus" = "Q12") # Q12 immigrationstatus

## 2. Rename the open-ended variables (see README.md file on GitHub for full list)
# have them be single word even if combined
# ex. rename `American dream` to americandream or AmericanDream
fullsurvey <- fullsurvey %>% rename("HispanicLatino" = "Hispanic.Latino")
fullsurvey <- fullsurvey %>% rename("NotHispanicLatino" = "Not.Hispanic.Latino")
fullsurvey <- fullsurvey %>% rename("LatinAmerica" = "Latin.America")
fullsurvey <- fullsurvey %>% rename("MiddleEast" = "Middle.East")
fullsurvey <- fullsurvey %>% rename("ThirdWorldPoorCountry" = "Third.world.poor.country")
fullsurvey <- fullsurvey %>% rename("JobAmericansDoNotWant" = "Job.Americans.Don.t.Want")
fullsurvey <- fullsurvey %>% rename("FamilyOriented" = "Family.oriented")
fullsurvey <- fullsurvey %>% rename("ReligiousChurchgoing" = "Religious.church.going")
fullsurvey <- fullsurvey %>% rename("PoorLowSocioeconomicStatus" = "Poor.low.socio.economic.status")
fullsurvey <- fullsurvey %>% rename("StealingAmericansJobs" = "Stealing.American.s.Jobs")
fullsurvey <- fullsurvey %>% rename("SeekingWelfare" = "Seeking.welfare")
fullsurvey <- fullsurvey %>% rename("BetterLife" = "Better.life")
fullsurvey <- fullsurvey %>% rename("FleeingEscapingHardship" = "Fleeing.escaping.hardship")
fullsurvey <- fullsurvey %>% rename("NoTypicalUnauthorizedImmigrant" = "No..typical..unauthorized.immigrant")
fullsurvey <- fullsurvey %>% rename("CriticizeUSimmigrationProcess" = "Criticize.US.immigration.process")
fullsurvey <- fullsurvey %>% rename("RiskTakingBrave" = "Risk.taking..brave")
fullsurvey <- fullsurvey %>% rename("ContributionsToUS" = "Contributions.to.U.S.")
fullsurvey <- fullsurvey %>% rename("AreTryingOrShouldTryToBecomeLegal" = "Are.trying.or.should.try.to.become.legal")
fullsurvey <- fullsurvey %>% rename("NobodyIllegalOnStolenLand" = "Nobody.illegal.on.stolen.land")
fullsurvey <- fullsurvey %>% rename("USdoesNotTreatWell" = "US.doesn.t.treat.well")
fullsurvey <- fullsurvey %>% rename("AmericanDream" = "American.dream")
fullsurvey <- fullsurvey %>% rename("UnfairDisrespectfulToLegalImmigrantsAndCitizens" = "Unfair.disrespectful.to.legal.immigrants.and.citizens")
fullsurvey <- fullsurvey %>% rename("AntiUnauthorizedImmigrationRacism" = "Anti.unauthorized.immigration...racism")
fullsurvey <- fullsurvey %>% rename("RespondentTalksAboutTheirConnectionExperience" = "Respondent.talks.about.their.connection.experience")
fullsurvey <- fullsurvey %>% rename("OppositionToTerminology" = "Opposition.to.terminology")
fullsurvey <- fullsurvey %>% rename("AntiUnauthorizedImmigration=Racism" = "AntiUnauthorizedImmigrationRacism")

## 3. Exploratory data analysis
# similar to what I did with unlawful and Unlawful
fullsurvey %>%
  ggplot(aes(x = hardworking)) +
  geom_histogram(y = after_stat(count/sum(count))) +
  stat_bin(binwidth = 0.25) +
  facet_wrap(~Family.oriented)
# try to get Y axis to change to percentage of total respondents (# of respondents that gave rating / # that didn't mention it)
# of respondents who gave rating / # who did mention
#some interesting ones I found
fullsurvey %>%
  ggplot(aes(x = unlawful)) +
  geom_bar() +
  facet_wrap(~CriticizeUSimmigrationProcess)

fullsurvey %>%
  ggplot(aes(x = hardworking)) +
  geom_bar() +
  facet_wrap(~FleeingEscapingHardship)

fullsurvey %>%
  ggplot(aes(x = lazy)) +
  geom_bar() +
  facet_wrap(~FleeingEscapingHardship)

#similar to Isaiah's unlawful vs Unlawful plot
fullsurvey %>%
  ggplot(aes(x = hardworking)) +
  geom_bar() +
  facet_wrap(~Hardworking)

fullsurvey %>%
  ggplot(aes(x = immoral)) +
  geom_bar() +
  facet_wrap(~BetterLife)

#thought this would be more negatively correlated
fullsurvey %>%
  ggplot(aes(x = unlawful)) +
  geom_bar() +
  facet_wrap(~BetterLife)

#Thought this would be more positively correlated
fullsurvey %>%
  ggplot(aes(x = loyal)) +
  geom_bar() +
  facet_wrap(~FamilyOriented)

fullsurvey %>%
  ggplot(aes(x = unlawful)) +
  geom_bar() +
  facet_wrap(~FamilyOriented)

##gives percentages instead of count
U0 <- fullsurvey %>%
  filter(Unlawful == 0) %>%
  ggplot() +
  geom_bar(aes(x = unlawful, y = after_stat(count/sum(count))))
U1 <- fullsurvey %>%
  filter(Unlawful == 1) %>%
  ggplot() +
  geom_bar(aes(x = unlawful, y = after_stat(count/sum(count))))
ggarrange(U0, U1)

#look at treatment differences (illegal vs. undocumented) between open-ended variables
ill_treatment_positive <- c(fullsurvey$ill_honest, fullsurvey$ill_hardworking, fullsurvey$ill_intelligent, fullsurvey$ill_loyal, fullsurvey$ill_responsible)
undoc_treatment_positive <- c(fullsurvey$undoc_honest, fullsurvey$undoc_hardworking, fullsurvey$undoc_intelligent, fullsurvey$undoc_loyal, fullsurvey$undoc_responsible)
ill_treatment_negative <- c(fullsurvey$ill_unlawful, fullsurvey$ill_violent, fullsurvey$ill_uneducated, fullsurvey$ill_lazy, fullsurvey$ill_immoral)
undoc_treatment_negative <- c(fullsurvey$undoc_unlawful, fullsurvey$undoc_violent, fullsurvey$undoc_uneducated, fullsurvey$undoc_lazy, fullsurvey$undoc_immoral)
#mean(treatment, na.rm = TRUE)
#do treatment difference for open-ended
#make appointment with CITL

#look to combine the open-ended responses into positive and negative and see how they compare
positive <- c(fullsurvey$honest, fullsurvey$hardworking, fullsurvey$intelligent, fullsurvey$loyal, fullsurvey$responsible)
mean(positive, na.rm = TRUE)
negative <- c(fullsurvey$unlawful, fullsurvey$violent, fullsurvey$uneducated, fullsurvey$lazy, fullsurvey$immoral)
mean(negative, na.rm = TRUE)
#condense positive and negative terms into one variable (open-ended) and then compare (difference b/n two means)
t.test(positive, negative)

#look at the means and the difference between two means (paired t-test, chi squared for closed-ended)
#potentially do a clustered bar chart, do percentages instead of count, add labels for the ratings (1 is NA, etc)
#focus on variables that appear more often, possibly combine some
fullsurvey$Positive <- ifelse(fullsurvey$Hardworking == 1 | fullsurvey$BetterLife == 1 | fullsurvey$FamilyOriented == 1, 1, 0)
fullsurvey %>%
  ggplot(aes(x = unlawful)) +
  geom_bar() +
  facet_wrap(~Positive)
fullsurvey$positive <- rowMeans(fullsurvey[,c("hardworking","honest","intelligent","loyal","responsible")],na.rm = TRUE)
fullsurvey %>%
  ggplot(aes(x = positive)) +
  geom_bar() +
  facet_wrap(~Positive)
fullsurvey$negative <- rowMeans(fullsurvey[,c("unlawful","violent","lazy","uneducated","immoral")],na.rm = TRUE)
fullsurvey %>%
  ggplot(aes(x = negative)) +
  geom_bar() +
  facet_wrap(~Positive)

#closed-ended positive vs open-ended top 3 positive
fullsurvey$positive <- rowMeans(fullsurvey[,c("hardworking","honest","intelligent","loyal","responsible")],na.rm = TRUE)
fullsurvey$Positive3 <- ifelse(fullsurvey$Hardworking == 1 | fullsurvey$BetterLife == 1 | fullsurvey$FamilyOriented == 1, 1, 0)
fullsurvey %>%
  ggplot(aes(x = positive)) +
  geom_bar() +
  facet_wrap(~Positive3)
#closed-ended positive vs same variables open-ended positive
fullsurvey$PositiveAll <- ifelse(fullsurvey$Hardworking == 1 | fullsurvey$Honest == 1 | fullsurvey$Intelligent == 1 | fullsurvey$Loyal == 1 | fullsurvey$Responsible == 1, 1, 0)
fullsurvey %>%
  ggplot(aes(x = positive)) +
  geom_bar() +
  facet_wrap(~PositiveAll)
#closed-ended negative vs same variables open-ended negative
fullsurvey$negative <- rowMeans(fullsurvey[,c("unlawful","violent","uneducated","lazy","immoral")],na.rm = TRUE)
fullsurvey$Negative <- ifelse(fullsurvey$Unlawful == 1 | fullsurvey$Violent == 1 | fullsurvey$Uneducated == 1 | fullsurvey$Lazy == 1 | fullsurvey$Immoral == 1, 1, 0)
fullsurvey %>%
  ggplot(aes(x = negative)) +
  geom_bar() +
  facet_wrap(~Negative)
#compare positive to negative
fullsurvey %>%
  ggplot(aes(x = negative)) +
  geom_bar() +
  facet_wrap(~Positive)
fullsurvey %>%
  ggplot(aes(x = positive)) +
  geom_bar() +
  facet_wrap(~Negative)

#work on formatting graphs to make them more aesthetically pleasing for the 5 graphs
#tried using percentages for the y-axis, it seems like it worked for the 'yes' category but no the 'no' category

fullsurvey$positive <- rowMeans(fullsurvey[,c("hardworking","honest","intelligent","loyal","responsible")],na.rm = TRUE)
fullsurvey$Positive3 <- ifelse(fullsurvey$Hardworking == 1 | fullsurvey$BetterLife == 1 | fullsurvey$FamilyOriented == 1, 1, 0)
fullsurvey$PositiveAll <- ifelse(fullsurvey$Hardworking == 1 | fullsurvey$Honest == 1 | fullsurvey$Intelligent == 1 | fullsurvey$Loyal == 1 | fullsurvey$Responsible == 1, 1, 0)
fullsurvey$negative <- rowMeans(fullsurvey[,c("unlawful","violent","uneducated","lazy","immoral")],na.rm = TRUE)
fullsurvey$Negative <- ifelse(fullsurvey$Unlawful == 1 | fullsurvey$Violent == 1 | fullsurvey$Uneducated == 1 | fullsurvey$Lazy == 1 | fullsurvey$Immoral == 1, 1, 0)
fullsurvey$Positive3[fullsurvey$Positive3 == 0] <- "No"
fullsurvey$Positive3[fullsurvey$Positive3 == 1] <- "Yes"
fullsurvey$PositiveAll[fullsurvey$PositiveAll == 0] <- "No"
fullsurvey$PositiveAll[fullsurvey$PositiveAll == 1] <- "Yes"
fullsurvey$Negative[fullsurvey$Negative == 0] <- "No"
fullsurvey$Negative[fullsurvey$Negative == 1] <- "Yes"
fullsurvey$Positive3 <- factor(fullsurvey$Positive3, levels = c("Yes","No"))
fullsurvey$PositiveAll <- factor(fullsurvey$PositiveAll, levels = c("Yes","No"))
fullsurvey$Negative <- factor(fullsurvey$Negative, levels = c("Yes","No"))
#positive vs Positive3
fullsurvey %>%
  ggplot(aes(x = positive, fill = Positive3)) +
  geom_bar() +
  facet_wrap(~Positive3) +
  scale_fill_manual(values = c("green","red")) +
  labs(title = "Top 3 Positive Descriptors and Their Applicability", subtitle = "Mention of Positive Descriptor", x = "Applicability of Positive Descriptor (1 is NA, 5 is VA)", y = "Count")
fullsurvey$Positive3[fullsurvey$Positive3 == 0] <- "No"
fullsurvey$Positive3[fullsurvey$Positive3 == 1] <- "Yes"
#positive vs PositiveAll
fullsurvey %>%
  ggplot(aes(x = positive, fill = PositiveAll)) +
  geom_bar() +
  facet_wrap(~PositiveAll) +
  scale_fill_manual(values = c("green","red")) +
  labs(title = "Relationship Between Positive Closed- and Open-ended Responses", subtitle = "Mention of Positive Descriptor in Open-ended Response", x = "Applicability of Positive Descriptor in Closed-ended Reponse (1 is NA, 5 is VA)", y = "Count", fill = NULL)
fullsurvey$PositiveAll[fullsurvey$PositiveAll == 0] <- "No"
fullsurvey$PositiveAll[fullsurvey$PositiveAll == 1] <- "Yes"
#negative vs Negative
fullsurvey %>%
  ggplot(aes(x = negative)) +
  geom_bar() +
  facet_wrap(~Negative) +
  scale_fill_manual(values = c("green","red")) +
  labs(title = "Negative Descriptors and Their Applicability", subtitle = "Mention of Negative Descriptor", x = "Applicability of Negative Descriptor (1 is Low Applicability, 5 is High Applicability)", y = "Count")
fullsurvey$Negative[fullsurvey$Negative == 0] <- "No"
fullsurvey$Negative[fullsurvey$Negative == 1] <- "Yes"

#negative vs Positive
fullsurvey %>%
  ggplot(aes(x = negative, y = 100*after_stat(count/sum(count)))) +
  geom_bar() +
  facet_wrap(~PositiveAll) +
  labs(title = "Relationship Between Negative Closed-ended and Positive Open-ended Responses", subtitle = "Mention of Positive Descriptor", x = "Applicability of Negative Descriptor (1 is NA, 5 is VA)", y = "% of Responses")
#positive vs Negative
fullsurvey %>%
  ggplot(aes(x = positive, y = 100*after_stat(count/sum(count)))) +
  geom_bar(color = "positive") +
  facet_wrap(~Negative) +
  labs(title = "Relationship Between Positive Closed-ended and Negative Open-ended Responses", subtitle = "Mention of Negative Descriptor", x = "Applicability of Positive Descriptor (1 is NA, 5 is VA)", y = "% of Responses")


fullsurvey$PositiveAll <- as.factor(fullsurvey$PositiveAll)
geom_bar(aes(x = `answering the question`, fill = side), position = position_dodge(preserve = 'single'), alpha = 0.5)
#find a way to switch the order of 'yes' and 'no' in the graphs
#alter legend either no title or new title
#include count and percentage graphs for each




\end{document}