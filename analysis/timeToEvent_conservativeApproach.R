#########################
# Retention Analysis
# Abhishek Pratap
# We used the most conservative approach here where participants are considered dead(as per survival terminology) 
# even if they were active in app later >84 days
#########################

rm(list=ls())
library("install.load")
install_load("data.table", "gdata", "jsonlite", "tableone")
install_load("tidyverse", "purrr", "doMC", "scales", "data.table")
install_load("gridExtra", "pheatmap", "printr", "ggthemes", "anytime")
install_load("survival", "survminer", "synapser")
synapser::synLogin()


# To be able to run this script you will need to run the "loadData.R" script in the same folder and save the output
load("tmp_digitalHealth_retentiondata.RData")


#######################
#1. Survival - Across ALL studies 
#######################
nrow(userStats)
censor <- rep(1, nrow(userStats)) 
fit.test <- survdiff(survival::Surv(time=duration_in_study, event=censor, type = "right") ~ study, data = userStats )
fit.test
fit.plot <- survfit(survival::Surv(time=duration_in_study, event=censor, type = "right") ~ study, data = userStats )
fit.plot

### Attrition for people at day 1  (1- Survival)
day1_survival <- 1- summary(fit.plot, times=1)$surv
day1_survival
IQR(day1_survival)
summary(day1_survival)

### Attrition for people at day 7  (1- Survival)
day7_survival <- 1- summary(fit.plot, times=7)$surv
day7_survival
IQR(day7_survival)
summary(day7_survival)


### Attrition for people at day 14   (1- Survival)
day14_survival <- 1- summary(fit.plot, times=14)$surv
day14_survival
IQR(day14_survival)
summary(day14_survival)
summary(fit.plot)$table
quantile(fit.plot, probs=c(.25, .50, .75))


### Summary stats 
survival.stats.all.studies <- summary(fit.plot)$table
survival.stats.all.studies <- as.data.frame(survival.stats.all.studies) 
renameCols <- c(LCL='0.95LCL', UCL='0.95UCL')
survival.stats.all.studies <- survival.stats.all.studies %>% 
  tibble::rownames_to_column(var='strata') %>%
  dplyr::rename( !!renameCols) %>%
  dplyr::mutate(CI=paste0(LCL,'-',UCL)) %>%
  dplyr::rename(N=records) %>%
  dplyr::select(strata, N, median, CI) %>%
  dplyr::mutate(strata = gsub('study=','', strata))
#Median survial time across studies
median(survival.stats.all.studies$median)


write.table(survival.stats.all.studies, file="Figs_N_Tables/survivalStats_acrossAllStudies.tsv", sep="\t", quote=F)
p1 <- ggsurvplot(fit.plot, pval = F, conf.int = T, 
                 xlab = "Duration in study ",  
                 palette = STUDY_COLS$color,
                 risk.table = F,
                 risk.table.height = 0.3,
                 risk.table.y.text = FALSE,
                 legend = "none",
                 legend.labs = STUDY_COLS$study,
                 surv.median.line = "hv", ggtheme = theme_bw(base_size = 15))
p1
ggsave("Figs_N_Tables/metaAnalysis_survivalCurve_byStudy.png", plot=print(p1), height = 6, width = 6, units="in", dpi=250)
ggsave("Figs_N_Tables/metaAnalysis_survivalCurve_byStudy.tiff", plot=print(p1), height = 6, width = 6, units="in", dpi=250)


#### Survival curve - sliding scale cut-off in min (duration in study, 0, 1, 2,3,4,5,6,7,14,21)
medSurvival_by_minDuration <- plyr::ldply(c(1,2,4,8,16,32), function(minDuration){
  df <- userStats %>% dplyr::filter(duration_in_study >= minDuration)
  censor <- rep(1, nrow(df)) 
  fit1.plot <- survfit(Surv(time=duration_in_study, event=censor, type = "right") ~ study, data = df)
  res <- surv_median(fit1.plot)
  res['minDuration'] = minDuration
  res
})
medSurvival_by_minDuration <- medSurvival_by_minDuration %>% 
  dplyr::rename(Study = strata) %>%
  dplyr::mutate(minDuration = factor(as.character(minDuration), levels=c('1','2','4','8','16','32')),
                Study = gsub('study=', '', Study),
                Study = factor(as.character(Study), levels=STUDY_COLS$study))
#### Some studies reach saturation as all participants are ALIVE after a certain min number of day participation.
#### For these move fwd the last median survival time.
medSurvival_by_minDuration<- medSurvival_by_minDuration %>% 
  dplyr::group_by(Study) %>%
  dplyr::arrange(minDuration) %>%
  dplyr::mutate(median.mod = ifelse(is.na(median), max(na.omit(median)), median))
medSurvival_by_minDuration

write.table(medSurvival_by_minDuration %>% mutate(CI = paste0(lower,'-', upper)) %>% select(Study, minDuration, median, CI), 
            file="Figs_N_Tables/medianSurvivalStats_acrossAllStudies_varied_by_minTimeinStudy.tsv",
            sep="\t", quote=F, row.names = F)

#### Change in survival time between baseline and cohort that stayed for atleast 8 days
tmpRes <- medSurvival_by_minDuration %>% filter(minDuration %in% c(1,8)) %>%
  select(-lower, -upper, -median) %>%
  mutate(minDuration = paste0('minDay', minDuration)) %>%
  spread(minDuration, median.mod) %>%
  mutate(dayChange = (minDay8 - minDay1)) 
median(tmpRes$dayChange)
p2 <- ggplot(data=medSurvival_by_minDuration, 
             aes(x=minDuration, y=median.mod, color=Study, group=Study)) + geom_point(size=1.3) + geom_line()
p2 <- p2 + geom_errorbar(data=medSurvival_by_minDuration, width=0.05,
                         mapping=aes(ymax = upper, ymin=lower))
p2 <- p2 + scale_color_manual(values = STUDY_COLS$color)
p2 <- p2 + theme_bw(base_size = 15) + xlab('Minimum number of days in the study') + ylab('Median survival time(days)')
p2 <- p2 +  theme(legend.position="none")
p2
ggsave("Figs_N_Tables/metaAnalysis_survivalCurve_byStudy_byVaryingMinDaysInStudy.png", plot=print(p2), height = 6, width = 6, units="in", dpi=250)
ggsave("Figs_N_Tables/metaAnalysis_survivalCurve_byStudy_byVaryingMinDaysInStudy.tiff", plot=print(p2), height = 6, width = 6, units="in", dpi=250)



################################
#2. Age -  Survival Analysis
################################
userStats_age <- userStats %>% filter(age_group != 'NA/Missing')
dim(userStats_age)
censor <- rep(1, nrow(userStats_age)) 
## 1. Logrank based
fit2.test <- survdiff(Surv(time=duration_in_study, event=censor) ~ age_group,  data = userStats_age)
fit2.plot <- survfit(Surv(time=duration_in_study, event=censor) ~ age_group, data = userStats_age)
summary(fit2.plot)
### Survival time at different quantiles. Here .75 = .25 (cohort survival). In other words .75 of the cohort dies so .25 survives
quantile(fit2.plot, probs=c(.25, .50, .75))
summary(fit2.plot, times=7)
summary(fit2.plot)$table


## Stratified log rank test 
fit2.stratified.test <- survdiff(Surv(time=duration_in_study, event=censor) ~ age_group + strata(study, na.group = T), data = userStats_age)
fit2.stratified.test
fit2.stratified.plot <- survfit(Surv(time=duration_in_study, event=censor) ~ age_group + strata(study, na.group = T), data = userStats_age)
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
write.table(fit2.stratified.summary, file="Figs_N_Tables/survivalStats_byAgeGroup_stratified_by_study.tsv",
            sep="\t", quote=F)

#The plot is using a simple logrank test (summarizing the study level)
p <- ggsurvplot(fit2.plot, conf.int = TRUE, pval=F,
                palette = c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00'),
                xlab = "Duration in study ",
                risk.table = F,
                risk.table.height = 0.3,
                risk.table.y.text = FALSE,
                legend = c(.88,.83),
                legend.title = 'Age group',
                legend.labs = c('18-29', '30-39', '40-49', '50-50', '60+'),
                surv.median.line = "hv", ggtheme = theme_bw(base_size = 15))
p
ggsave("Figs_N_Tables/metaAnalysis_survivalCurve_by_ageGroup.png", plot=print(p), height = 6, width = 6, units="in", dpi=250)
ggsave("Figs_N_Tables/metaAnalysis_survivalCurve_by_ageGroup.tiff", plot=print(p), height = 6, width = 6, units="in", dpi=250)


################################
#3. Gender -  Survival Analysis
################################
userStats_gender <- userStats %>%  dplyr::filter(gender %in% c('Male', 'Female') )
nrow(userStats_gender)
censor <- rep(1, nrow(userStats_gender)) 

## 1. Logrank based
fit3.test <- survdiff(Surv(time=duration_in_study, event=censor) ~ gender, data = userStats_gender)
fit3.plot <- survfit(Surv(time=duration_in_study, event=censor) ~ gender, data = userStats_gender)

fit3.stratified.test <- survdiff(Surv(time=duration_in_study, event=censor) ~ gender + strata(study, na.group = T), 
                                 data = userStats_gender )
fit3.stratified.test
fit3.stratified.plot <- survfit(Surv(time=duration_in_study, event=censor) ~ gender + strata(study, na.group = T), 
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
write.table(fit3.stratified.summary, file="Figs_N_Tables/survivalStats_byGender_stratified_by_study.tsv",
            sep="\t", quote=F)
p <- ggsurvplot(fit3.plot, conf.int = TRUE, pval=F,
                palette = c('#d01c8b', '#377eb8'),
                xlab = "Duration in study ",
                risk.table = F,
                risk.table.height = 0.3,
                risk.table.y.text = FALSE,
                legend = "top",
                legend.title = 'Gender',
                legend.labs = c('Female', 'Male'),
                surv.median.line = "hv", ggtheme = theme_bw(base_size = 15))
p
ggsave("Figs_N_Tables/metaAnalysis_survivalCurve_by_gender.png", plot=print(p), height = 6, width = 6, units="in", dpi=250)


#######################
#4. Survival Analysis - Case vs Control
#######################
table(userStats$study, userStats$caseStatus)
userStats_caseControl <- userStats %>% 
  filter(study %in% c('ElevateMS', 'mPower', 'SleepHealth', 'Asthma', 'MyHeartCounts'), 
         caseStatus %in% c(T, F)) %>% as.data.frame()
dim(userStats_caseControl)
censor <- rep(1, nrow(userStats_caseControl)) 

#censor <- rep(1, nrow(userStats_caseControl))
fit4.test <- survdiff(Surv(time=duration_in_study, event=censor) ~ caseStatus, data = userStats_caseControl )
fit4.plot <- survfit(Surv(time=duration_in_study, event=censor) ~ caseStatus, data = userStats_caseControl )
summary(fit4.plot)$table

fit4.stratified.test <- survdiff(Surv(time=duration_in_study, event=censor) ~ caseStatus + strata(study, na.group = T), 
                                 data = userStats_caseControl )
fit4.stratified.test
fit4.stratified.plot <- survfit(Surv(time=duration_in_study, event=censor) ~ caseStatus + strata(study, na.group = T), 
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
write.table(fit4.stratified.summary, file="Figs_N_Tables/survivalStats_byDiseaseStatus_stratified_by_study.tsv",
             sep="\t", quote=F)
p4 <- ggsurvplot(fit4.plot, pval = F, conf.int = T, 
                 palette = c('#404040', '#ca0020'),
                 xlab = "Duration in study ",  
                 risk.table = F,
                 risk.table.height = 0.3,
                 risk.table.y.text = FALSE,
                 legend = c(.83,.90), legend.labs=c('False', 'True'), legend.title = 'Disease Status',
                 surv.median.line = "hv", ggtheme = theme_bw(base_size = 15))
p4
ggsave("Figs_N_Tables/metaAnalysis_survivalCurve_by_CaseControl.png", plot=print(p4), height = 6, width = 6, units="in", dpi=250)
ggsave("Figs_N_Tables/metaAnalysis_survivalCurve_by_CaseControl.tiff", plot=print(p4), height = 6, width = 6, units="in", dpi=250)



#######################################
#5. Clincal referral vs  In the wild
######################################
userStats_clinicalRef <- userStats %>%  dplyr::filter(study %in% c('ElevateMS', 'mPower'))
dim(userStats_clinicalRef)
View(userStats_clinicalRef)
censor = rep(1,nrow(userStats_clinicalRef))
fit5.test <- survdiff(Surv(time=duration_in_study, event=censor) ~ clinicalReferral + strata(study), data = userStats_clinicalRef )
fit5.test
fit5.plot <- survfit(Surv(time=duration_in_study, event=censor) ~ clinicalReferral, data = userStats_clinicalRef )
summary(fit5.plot)$table

fit5.stratified.test <- survdiff(Surv(time=duration_in_study, event=censor) ~ clinicalReferral + strata(study, na.group = T), 
                                 data = userStats_clinicalRef )
fit5.stratified.test
fit5.stratified.plot <- survfit(Surv(time=duration_in_study, event=censor) ~ clinicalReferral + strata(study,na.group = T), 
                data = userStats_clinicalRef )
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

write.table(fit5.stratified.summary, file="Figs_N_Tables/survivalStats_byClinicalReferral_stratified_by_study.tsv",
            sep="\t", quote=F)
p5 <- ggsurvplot(fit5.plot,
                 palette = c('#008837', '#5e3c99'),
                 pval = F, conf.int = TRUE, xlab = "Duration in study ",
                 risk.table = F,
                 risk.table.height = 0.2,
                 risk.table.y.text = FALSE,
                 legend = c(.80, .90),
                 legend.title = 'Clinically referred',
                 legend.labs = c('False', 'True'),
                 surv.median.line = "hv", ggtheme = theme_bw(base_size = 15))
p5
ggsave("Figs_N_Tables/metaAnalysis_survivalCurve_by_ClinicalReferral_DurationInStudy.png", plot=print(p5), height = 6, width = 6, 
       units="in", dpi=250)
ggsave("Figs_N_Tables/metaAnalysis_survivalCurve_by_ClinicalReferral_DurationInStudy.tiff", plot=print(p5), height = 6, width = 6, 
       units="in", dpi=250)


#######################################
#6. Region wise differences -NOT Interesting visually
######################################
userStats_stateRegion <- merge(userStats,
      us.pop.data %>% select(NAME, state.region),
      by.x="state", by.y="NAME", all.x=T) %>%
  filter(!is.na(state.region))
dim(userStats_stateRegion)
censor = rep(1,nrow(userStats_stateRegion))
fit6.test <- survdiff(Surv(time=duration_in_study, event=censor) ~ state.region, data = userStats_stateRegion )
fit6.test
fit6.plot <- survfit(Surv(time=duration_in_study, event=censor) ~ state.region, data = userStats_stateRegion)
fit6.plot

fit6.stratified.test <- survdiff(Surv(time=duration_in_study, event=censor) ~ state.region + strata(study), 
                                 data = userStats_stateRegion )
fit6.stratified.test
fit6.stratified.plot <- survfit(Surv(time=duration_in_study, event=censor) ~ state.region + strata(study), 
                                data = userStats_stateRegion )
fit6.stratified.plot
p6 <- ggsurvplot(fit6.plot,
                 #palette = c('#008837', '#5e3c99'),
                 pval = F, conf.int = TRUE, xlab = "Duration in study ",
                 risk.table = F,
                 risk.table.height = 0.2,
                 risk.table.y.text = FALSE,
                 #legend = c(.80, .90),
                 #legend.title = 'Clinically referred',
                 #legend.labs = c('False', 'True'),
                 surv.median.line = "hv", ggtheme = theme_bw(base_size = 15))
p6

#######################################
#7. RACE - RELATED ENGAGEMENT DIFFERENCES
######################################
table(userStats$study, userStats$race)
userStats_race <- userStats %>% 
  filter(!is.na(race)) %>%
  filter(race %in% c('Non-Hispanic White', 
                     'Hispanic/Latinos')) %>%
  filter(! study == 'Start')

userStats_race <- userStats %>%
  filter(! race == 'NA/Missing') %>%
  mutate(race = case_when(
      race %in% c('African-American/Black', 'AIAN', 'Asian', 'Other',
                  'Hawaiian or other Pacific Islander',
                  'Hispanic/Latinos', 'Native Hawaiian or other Pacific Islander') ~ 'Non-White',
      TRUE ~ race
  )) %>% dplyr::filter(! study == 'Start')

table(userStats_race$race, userStats_race$study)
censor = rep(1,nrow(userStats_race))
fit7.test <- survdiff(Surv(time=duration_in_study, event=censor) ~ race + strata(study), data = userStats_race )
fit7.test

fit7.plot <- survfit(Surv(time=duration_in_study, event=censor) ~ race, data = userStats_race )
fit7.plot

fit7.stratified.plot <- survfit(Surv(time=duration_in_study, event=censor) ~ race + strata(study), data = userStats_race )
summary(fit7.stratified.plot)$table

p7 <- ggsurvplot(fit7.plot,
                 pval = TRUE, conf.int = TRUE, xlab = "Duration in study ",
                 risk.table = F,
                 risk.table.height = 0.2,
                 risk.table.y.text = FALSE,
                 legend = "top",
                 surv.median.line = "hv", ggtheme = theme_bw(base_size = 15))
p7
ggsave("Figs_N_Tables/metaAnalysis_survivalCurve_by_Hispanic_vs_White.png", plot=print(p7), height = 8, width = 8, 
       units="in", dpi=250)



###############
##### Cox PH Models
###############
install_load('broom')
#1. Global model - senseless as studies violate CoXPH assumptions
# censor <- rep(1, nrow(userStats))
# res.cox <- coxph(Surv(time=duration_in_study, event=censor) ~ study, 
#                  data = userStats)
# res.cox %>% tidy
# ## Looks like the assumptions of coxPH are broken
# cox.zph(res.cox)
# ggforest(res.cox)
# install_load("car")
# car::Anova(res.cox)
# ggcoxzph(cox.zph(res.cox))
# ggcoxdiagnostics(res.cox, type = "dfbeta",
#                  linear.predictions = FALSE, ggtheme = theme_bw())


### Study-wise cox-PH models
run_coxPH_model <- function(df, covariate){
  censor <- rep(1, nrow(df))
  covars <- paste('~ ', paste(covariate, collapse=' + ') )
  formula <- paste('Surv(time=duration_in_study, event=censor)', covars )
  coxph(as.formula(formula), data = df)
}


#run coxPH models
studywise.coxPH.models <- userStats %>%
  dplyr::mutate(age_group = as.character(age_group)) %>%
  dplyr::filter(!age_group == 'NA') %>%
  dplyr::filter(!is.na(age_group) & gender %in% c('Male', 'Female')) %>%
  dplyr::mutate(gender = factor(as.character(gender), levels=c('Male', 'Female'))) %>%
  dplyr::group_by(study) %>%
  tidyr::nest() %>%
  dplyr::mutate(coxPH_model = map(data,run_coxPH_model, c('age_group', 'gender')),
                coxPH_model_test = map(coxPH_model,cox.zph))

names(studywise.coxPH.models$coxPH_model_test) <- studywise.coxPH.models$study
studywise.coxPH.models$coxPH_model_test

# table(sleepHealth$gender)
# View(userStats %>% count(study, age_group) %>% group_by(study) %>%
#   mutate(percent = (n / sum(n)) * 100))
# table(x$age_group)
# x <- userStats %>% filter(study == 'Start') %>%
#   mutate(age_group = relevel(age_group, ref='31-40'))
# censor <- rep(1, nrow(x))
# fit <- coxph(Surv(time=duration_in_study, event=censor) ~ age_group , data = x)
# fit
# cox.zph(fit)

#extract model details 
studywise.coxPH.model.details <- studywise.coxPH.models %>% 
  mutate(tidy_coxph =  coxPH_model %>% map(broom::tidy)) %>%
  select(-data, -coxPH_model, -coxPH_model_test) %>%
  unnest() %>%
  mutate(exp.estimate = exp(estimate),
         exp.conf.high = exp(conf.high),
         exp.conf.low = exp(conf.low),
         significant = ifelse(p.value < .05, T, F))


# CoxPH - across studies faceted by Age group
age.rows <- grepl('age_group', studywise.coxPH.model.details$term)
age <- studywise.coxPH.model.details[age.rows,] %>%
  dplyr::mutate(term  = gsub('age_group', '', term)) %>%
  dplyr::rename(age = term) %>%
  dplyr::mutate(age = factor(age, levels = c('61+', '51-60', '41-50', '31-40'))) %>%
  filter(!is.na(age))

p <- ggplot(data=age, aes(x=age, y=exp.estimate, color=significant)) + geom_point(size=1) 
p <- p + geom_errorbar(mapping = aes(ymax=exp.conf.high, ymin=exp.conf.low), width=0.2)
p <- p + coord_flip() + theme_bw() + facet_wrap( study ~ ., nrow = 2) + ylab('hazard ratio') 
p <- p + scale_color_manual(values=c('#636363', '#a50f15')) + geom_hline(linetype="dashed", yintercept = 1.0)
p
ggsave("Figs_N_Tables/acrosStudies_hazardRatios_DurationInStudy_by_Age.png", plot=print(p), height = 8, width = 12, units="in", dpi=250)


### CoxPH - gender
gender.rows <- grepl('gender', studywise.coxPH.model.details$term)
gender <- studywise.coxPH.model.details[gender.rows,]
p <- ggplot(data=gender, aes(x=study, y=exp.estimate, color=significant)) + geom_point(size=1) 
p <- p + geom_errorbar(mapping = aes(ymax=exp.conf.high, ymin=exp.conf.low), width=0.2)
p <- p + coord_flip() + theme_bw() + ylab('hazard ratio (gender = Female)') 
p <- p + scale_color_manual(values=c('#636363', '#a50f15')) + geom_hline(linetype="dashed", yintercept = 1.0)
p
ggsave("analysis/FIGURES/acrosStudies_hazardRatios_DurationInStudy_by_Gender.png", plot=print(p), height = 6, width = 6, units="in", dpi=250)


