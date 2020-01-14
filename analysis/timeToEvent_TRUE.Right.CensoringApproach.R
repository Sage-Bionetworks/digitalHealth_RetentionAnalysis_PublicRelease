#########################
# Retention Analysis
# Abhishek Pratap
#########################

#################
# Approach II - Survival analysis using Right censoring 
# Done mainly to assess the sensitivity of the main No-censoring based approach
################

rm(list=ls())
library("install.load")
install_load("data.table", "gdata", "jsonlite", "tableone")
install_load("tidyverse", "purrr", "doMC", "scales", "data.table")
install_load("gridExtra", "pheatmap", "printr", "ggthemes", "anytime")
install_load("survival", "survminer", "synapser")
synapser::synLogin()

# To be able to run this script you will need to run the "loadData.R" script in the same folder and save the output
load("tmp_digitalHealth_retentiondata.RData")



######
# UserStats - loaded from loadData // QA - QC
#######
dim(userStats)

#Number of users who leave within the 84 days 
sum(userStats$duration_in_study < 84, na.rm = T)

## this is the group that leaves the study <84 group but then comes back and does an activity in the next 8 weeks 
userStats %>% filter(duration_in_study <= 84, duration.in.study.first20Weeks > 84) %>% nrow()

## EDA Analysis
# ggplot(data=userStats %>% filter(TRUE.Duration.in.study > 85), aes(x=(duration.in.study.first20Weeks), y=duration_in_study)) + geom_point()
# ggplot(data=userStats %>% filter(TRUE.Duration.in.study > 85), aes(x=log10(TRUE.Duration.in.study-duration_in_study))) + geom_density()
# ggplot(data=userStats %>% filter(TRUE.Duration.in.study > 85), aes(x=log10(TRUE.Duration.in.study))) + geom_density()

###### Create censor variable
#### Censor variable 0=alive after 84 days in the app OR 1=dead 
userStats <- userStats %>% dplyr::mutate(censorStatus = ifelse(duration.in.study.first20Weeks <= 84, 1, 0))
userStats <- userStats %>% dplyr::mutate(duration_in_study.mod = ifelse(duration.in.study.first20Weeks <= 84, duration.in.study.first20Weeks, 84))


#######################
#1. Survival - Across ALL studies  w/Right Censoring 
#######################
fit.test <- survdiff(survival::Surv(time=duration_in_study.mod, event=censorStatus, type = "right") ~ study, data = userStats )
fit.test
fit.plot <- survfit(survival::Surv(time=duration_in_study.mod, event=censorStatus, type = "right") ~ study, data = userStats )

### Summary stats 
survival.stats.all.studies <- summary(fit.plot)$table
survival.stats.all.studies <- as.data.frame(survival.stats.all.studies)
survival.stats.all.studies
renameCols <- c(LCL='0.95LCL', UCL='0.95UCL')
survival.stats.all.studies <- survival.stats.all.studies %>% 
  tibble::rownames_to_column(var='strata') %>%
  dplyr::rename( !!renameCols) %>%
  dplyr::mutate(CI=paste0(LCL,'-',UCL)) %>%
  dplyr::rename(N=records) %>%
  dplyr::select(strata, N, median, CI) %>%
  dplyr::mutate(strata = gsub('study=','', strata, perl = T))
survival.stats.all.studies
write.table(survival.stats.all.studies, file="Figs_N_Tables/SurvivalStats_withRightCensoring_survivalStats_acrossAllStudies.tsv", sep="\t", quote=F)



################################
#2. Age -  Survival Analysis
################################
userStats_age <- userStats %>% filter(age_group != 'NA/Missing')
dim(userStats_age)

## 1. Logrank based
fit2.test <- survdiff(Surv(time=duration_in_study.mod, event=censorStatus) ~ age_group,  data = userStats_age)
fit2.plot <- survfit(Surv(time=duration_in_study.mod, event=censorStatus) ~ age_group, data = userStats_age)
summary(fit2.plot)

## Stratified log rank test 
fit2.stratified.test <- survdiff(Surv(time=duration_in_study.mod, event=censorStatus) ~ age_group + strata(study, na.group = T),  data = userStats_age)
fit2.stratified.test
fit2.stratified.plot <- survfit(Surv(time=duration_in_study.mod, event=censorStatus) ~ age_group + strata(study, na.group = T),  data = userStats_age)
fit2.stratified.plot
fit2.stratified.summary <- summary(fit2.stratified.plot)
fit2.stratified.summary <- as.data.frame(fit2.stratified.summary$table)
renameCols <- c(LCL='0.95LCL', UCL='0.95UCL')
fit2.stratified.summary <- fit2.stratified.summary %>% 
  tibble::rownames_to_column(var='strata') %>%
  dplyr::rename( !!renameCols) %>%
  dplyr::mutate(CI=paste0(LCL,'-',UCL)) %>%
  dplyr::rename(N=records) %>%
  dplyr::select(strata, N, median, CI) %>%
  dplyr::mutate(strata = gsub(',.*=','|', strata, perl = T))
fit2.stratified.summary
write.table(fit2.stratified.summary, file="Figs_N_Tables/SurvivalStats_withRightCensoring_survivalStats_byAgeGroup_stratified_by_study.tsv",
            sep="\t", quote=F)


################################
#3. Gender -  Survival Analysis
################################
userStats_gender <- userStats %>%  dplyr::filter(gender %in% c('Male', 'Female') )
nrow(userStats_gender)

## 1. Logrank based
fit3.test <- survdiff(Surv(time=duration_in_study.mod, event=censorStatus) ~ gender, data = userStats_gender)
fit3.plot <- survfit(Surv(time=duration_in_study.mod, event=censorStatus) ~ gender, data = userStats_gender)
fit3.stratified.test <- survdiff(Surv(time=duration_in_study.mod, event=censorStatus) ~ gender + strata(study, na.group = T), 
                                 data = userStats_gender )
fit3.stratified.test
fit3.stratified.plot <- survfit(Surv(time=duration_in_study.mod, event=censorStatus) ~ gender + strata(study, na.group = T), 
                                data = userStats_gender )
fit3.stratified.summary <- summary(fit3.stratified.plot)
fit3.stratified.summary <- as.data.frame(fit3.stratified.summary$table)
fit3.stratified.summary
renameCols <- c(LCL='0.95LCL', UCL='0.95UCL')
fit3.stratified.summary <- fit3.stratified.summary %>% 
  tibble::rownames_to_column(var='strata') %>%
  dplyr::rename( !!renameCols) %>%
  dplyr::mutate(CI=paste0(LCL,'-',UCL)) %>%
  dplyr::rename(N=records) %>%
  dplyr::select(strata, N, median, CI) %>%
  dplyr::mutate(strata = gsub(',.*=','|', strata, perl = T))
fit3.stratified.summary
write.table(fit3.stratified.summary, file="Figs_N_Tables/SurvivalStats_withRightCensoring_survivalStats_byGender_stratified_by_study.tsv", sep="\t", quote=F)


#######################
#4. Survival Analysis - Case vs Control
#######################
table(userStats$study, userStats$caseStatus )
userStats_caseControl <- userStats %>% 
  filter(study %in% c('ElevateMS', 'mPower', 'SleepHealth', 'Asthma', 'MyHeartCounts'), 
         caseStatus %in% c(T, F)) %>% as.data.frame()
dim(userStats_caseControl)
fit4.test <- survdiff(Surv(time=duration_in_study.mod, event=censorStatus) ~ caseStatus, data = userStats_caseControl )
fit4.test
fit4.plot <- survfit(Surv(time=duration_in_study.mod, event=censorStatus) ~ caseStatus, data = userStats_caseControl )
summary(fit4.plot)$table

fit4.stratified.test <- survdiff(Surv(time=duration_in_study.mod, event=censorStatus) ~ caseStatus + strata(study, na.group = T), 
                                 data = userStats_caseControl )
fit4.stratified.test
fit4.stratified.plot <- survfit(Surv(time=duration_in_study.mod, event=censorStatus) ~ caseStatus + strata(study, na.group = T), 
                                data = userStats_caseControl )
fit4.stratified.summary <- summary(fit4.stratified.plot)
fit4.stratified.summary <- as.data.frame(fit4.stratified.summary$table)
fit4.stratified.summary
renameCols <- c(LCL='0.95LCL', UCL='0.95UCL')
fit4.stratified.summary <- fit4.stratified.summary %>% 
  tibble::rownames_to_column(var='strata') %>%
  dplyr::rename( !!renameCols) %>%
  dplyr::mutate(CI=paste0(LCL,'-',UCL)) %>%
  dplyr::rename(N=records) %>%
  dplyr::select(strata, N, median, CI) %>%
  dplyr::mutate(strata = gsub(',.*=','|', strata, perl = T))
fit4.stratified.summary
write.table(fit4.stratified.summary, file="Figs_N_Tables/SurvivalStats_withRightCensoring_byCaseStatus_stratified_by_study.tsv", sep="\t", quote=F)




#######################################
#5. Clincal referral vs  In the wild
######################################
userStats_clinicalRef <- userStats %>%  dplyr::filter(study %in% c('ElevateMS', 'mPower'))
View(userStats_clinicalRef)
fit5.test <- survdiff(Surv(time=duration_in_study, event=censorStatus) ~ clinicalReferral + strata(study), data = userStats_clinicalRef )
fit5.test
fit5.plot <- survfit(Surv(time=duration_in_study, event=censorStatus) ~ clinicalReferral, data = userStats_clinicalRef )
summary(fit5.plot)$table

fit5.stratified.test <- survdiff(Surv(time=duration_in_study, event=censorStatus) ~ clinicalReferral + strata(study, na.group = T), data = userStats_clinicalRef )
fit5.stratified.test
fit5.stratified.plot <- survfit(Surv(time=duration_in_study, event=censorStatus) ~ clinicalReferral + strata(study, na.group = T), data = userStats_clinicalRef )
fit5.stratified.plot
fit5.stratified.summary <- summary(fit5.stratified.plot)
fit5.stratified.summary <- as.data.frame(fit5.stratified.summary$table)
fit5.stratified.summary
renameCols <- c(LCL='0.95LCL', UCL='0.95UCL')
fit5.stratified.summary <- fit5.stratified.summary %>% 
  tibble::rownames_to_column(var='strata') %>%
  dplyr::rename( !!renameCols) %>%
  dplyr::mutate(CI=paste0(LCL,'-',UCL)) %>%
  dplyr::rename(N=records) %>%
  dplyr::select(strata, N, median, CI) %>%
  dplyr::mutate(strata = gsub(',.*=','|', strata, perl = T))
fit5.stratified.summary

###Exploring WHY NA's are reported  for mPower cohort
### Looks like the after right censoring, the retention in the clinically-referred cohort in the mPower study 
### did not drop below 50%. Therefore the median(50%) retention time is NA 
#### For supplementary data we showed the lowest retention time(76 days)for 52.9 retention and made a remark in the legend. 
summary(fit5.stratified.plot)



