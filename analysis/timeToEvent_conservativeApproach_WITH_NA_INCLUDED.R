#########################
# Retention Analysis - WITH MISSING DATA INCLUDED (For Sensitivity Analysis)
# Abhishek Pratap
#########################

rm(list=ls())
library("install.load")
install_load("data.table", "gdata", "jsonlite", "tableone")
install_load("tidyverse", "purrr", "scales", "data.table")
install_load("gridExtra", "pheatmap", "printr", "ggthemes", "anytime")
install_load("survival", "survminer", "synapser")
synapser::synLogin()

# To be able to run this script you will need to run the "loadData.R" script in the same folder and save the output
load("tmp_digitalHealth_retentiondata.RData")



################################
#1. Age -  Survival Analysis - WITH MISSING DATA INCLUDED
################################
userStats_age_withMissing <- userStats %>% 
  dplyr::mutate(missingAge = ifelse(age_group == 'NA/Missing', T, F), censor = 1) %>%
  dplyr::filter(age_group != 'redacted' )
dim(userStats_age_withMissing)

## 1. Logrank based
fit1 <- survdiff(Surv(time=duration_in_study, event=censor) ~ age_group,  data = userStats_age_withMissing)
fit1.plot <- survfit(Surv(time=duration_in_study, event=censor) ~ age_group, data = userStats_age_withMissing)

## 2. Stratified log rank test
fit2.stratified.test <- survdiff(Surv(time=duration_in_study, event=censor) ~ age_group + strata(study, na.group = T), data = userStats_age_withMissing)
fit2.stratified.test
fit2.stratified.plot <- survfit(Surv(time=duration_in_study, event=censor) ~ age_group + strata(study, na.group = T), data = userStats_age_withMissing)
fit2.stratified.summary <- summary(fit2.stratified.plot)
fit2.stratified.summary <- as.data.frame(fit2.stratified.summary$table)
fit2.stratified.summary


p <- ggsurvplot(fit1.plot, conf.int = TRUE, pval=T,
                palette = c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00', 'grey10'),
                xlab = "Duration in study ",
                risk.table = F,
                risk.table.height = 0.3,
                risk.table.y.text = FALSE,
                legend = c(.85,.78),
                title = "Across studies(with missing age group included)",
                legend.title = 'Age group',
                legend.labs = c('18-29', '30-39', '40-49', '50-50', '60+', 'NA/Missing'),
                surv.median.line = "hv", ggtheme = theme_bw(base_size = 12))
p
fileName  = 'Figs_N_Tables/metaAnalysis_survivalCurve_by_ageGroup_withNAs.png'
ggsave(fileName, plot=print(p), height = 6, width = 6, units="in", dpi=250)


### Per study
lapply(unique(userStats_age_withMissing$study), function(x){
  tmp <- userStats_age_withMissing %>% filter(study == x)
  fit1.plot <- survfit(Surv(time=duration_in_study, event=censor) ~ missingAge, data = tmp)
  p <- ggsurvplot(fit1.plot, conf.int = TRUE, pval=T,
                  palette = c('#e41a1c', 'grey10'),
                  xlab = "Duration in study ", risk.table = F, title = x,
                  risk.table.y.text = FALSE, legend = c(.80,.84),
                  legend.title = 'Age',
                  surv.median.line = "hv", ggtheme = theme_bw(base_size = 15))
  p
  fileName  = paste0('Figs_N_Tables/metaAnalysis_study_', x,  'survivalCurve_by_ageGroup_withNAs.png')
  ggsave(fileName, plot=print(p), height = 6, width = 6, units="in", dpi=250)
})



################################
#2. Gender -  Survival Analysis
################################
userStats_gender_withMissing <- userStats  %>%  
  dplyr::filter(gender %in% c('Male', 'Female', 'NA/Missing') ) %>%
  dplyr::filter(age_group != 'redacted' ) %>%
  dplyr::mutate(missingGender = ifelse(gender == 'NA/Missing', T, F),
                censor=1)

## 1. Logrank based
fit3.test <- survdiff(Surv(time=duration_in_study, event=censor) ~ gender, data = userStats_gender_withMissing)
fit3.test
fit3.plot <- survfit(Surv(time=duration_in_study, event=censor) ~ gender, data = userStats_gender_withMissing)
fit3.plot
p <- ggsurvplot(fit3.plot, conf.int = TRUE, pval=F,
                palette = c('#d01c8b', '#377eb8', 'grey10'),
                xlab = "Duration in study ",
                risk.table = F,
                risk.table.height = 0.3,
                risk.table.y.text = FALSE,
                legend = c(.80,.84),
                legend.title = 'Gender',
                title = "Across studies(with missing gender group included)",
                legend.labs = c('Female', 'Male', 'NA/Missing'),
                surv.median.line = "hv", ggtheme = theme_bw(base_size = 12))
p
ggsave("Figs_N_Tables/metaAnalysis_survivalCurve_by_gender_with_NAs.png", plot=print(p), height = 6, width = 6, units="in", dpi=250)


### Per study
lapply(unique(as.character(userStats_gender_withMissing$study)), function(x){
  tmp <- userStats_gender_withMissing %>% filter(study == x)
  
  if (n_distinct(tmp$missingGender) <=1) {
    print('Skipping study')
    print(x)
    return(NA)
  } else {
    fit1.plot <- survfit(Surv(time=duration_in_study, event=censor) ~ missingGender, data = tmp)
    p <- ggsurvplot(fit1.plot, conf.int = TRUE, pval=T,
                    palette = c('#e41a1c', 'grey10'),
                    xlab = "Duration in study ", risk.table = F, title = x,
                    risk.table.y.text = FALSE, legend = c(.75,.85),
                    legend.title = 'Gender',
                    surv.median.line = "hv", ggtheme = theme_bw(base_size = 15))
    fileName  = paste0('Figs_N_Tables/metaAnalysis_study_', x,  'survivalCurve_by_gender_withNAs.png')
    ggsave(fileName, plot=print(p), height = 6, width = 6, units="in", dpi=250)
    return('success')
  }
  
})

