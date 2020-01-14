rm(list=ls())
options(stringsAsFactors = F)
library("install.load")
install_load("data.table", "gdata", "synapser", "jsonlite", "stringr")
install_load("plyr", "tidyverse", "doMC", "scales", "data.table")
install_load("gridExtra", "pheatmap", "printr", "ggthemes", "anytime")

## will require synapse login setup - more info see here - 
synapser::synLogin() 


### HARD CODED STUDY SPECIFIC COLORS FOR PLOTS
STUDY_COLS = data.frame(study = c('SleepHealth', 'Brighten', 'Asthma', 'ElevateMS',  
                                  'mPower','Phendo','MyHeartCounts', 'Start'),
                        color = c('#4363D8', '#0B9FC1', '#E6194B', '#38A847',
                                  '#F032E6', '#f58231', '#800000', '#808000'))


#######
#Set State level meta data
#######
state.metadata = data.frame(state.name = state.name,
                            state.abb = state.abb,
                            state.region = state.region) %>%
  dplyr::mutate(state.name = as.character(state.name),
                state.abb = as.character(state.abb),
                state.region = as.character(state.region))
## DC
state.metadata <- rbind(state.metadata, c('District of Columbia', 'DC', 'South'))

#State Population data
## GET 2018 state population data - American Community Survey
## A copy is stored in the public portal 
us.pop.data <- fread(synGet('syn21517653')$path, data.table=F) %>%
  select(REGION, DIVISION, NAME, POPESTIMATE2018) 
us.pop.data <- merge(us.pop.data, state.metadata, all=T, by.x='NAME', by.y='state.name')
##US Regions
US_regions <- us.pop.data %>% filter(str_detect(NAME, 'Region')) %>%
  dplyr::mutate(RegionProp =  round( (POPESTIMATE2018 / sum(POPESTIMATE2018))*100, digits=2))

##Remove rows for regions from state data
us.pop.data <- us.pop.data %>% filter(!is.na(state.region))


##################
### mPower
##################
get_mpower_engagement_data <- function(){
  df <- fread(synGet("syn20929422")$path, data.table = F) %>%
    dplyr::rename(healthCode = uid)
}
get_mpower_mdata <- function() {
  fread(synGet("syn20929429")$path, data.table = F) %>%
    dplyr::rename(healthCode = uid)

}
mpower_mdata <- get_mpower_mdata()
mpower <- get_mpower_engagement_data()


#############
### Start
#############
get_start_mdata <-  function(){
  fread(synGet("syn20929455")$path, data.table = F) %>%
    dplyr::rename(healthCode = uid)
}
get_start_engagement_data <- function(){
  df <- fread(synGet("syn20929421")$path, data.table = F) %>%
    dplyr::rename(healthCode = uid)
}
start_mdata <- get_start_mdata() 
start <- get_start_engagement_data()


###################
### Sleep Health
###################
get_sleepHealth_engagement_data <- function(){
  df <- fread(synGet("syn20929424")$path, data.table = F) %>%
    dplyr::rename(healthCode = uid)
}
get_sleepHealth_mdata <- function(){
  df <- fread(synGet("syn20929441")$path, data.table = F) %>%
    dplyr::rename(healthCode = uid)
}
sleepHealth <- get_sleepHealth_engagement_data()
sleepHealth_mdata <- get_sleepHealth_mdata()


#################
#### ElevateMS
#################
get_elevateMS_data <- function(){
  df <- fread(synGet("syn20929415")$path, data.table = F) %>%
    dplyr::rename(healthCode = uid)
}

get_elevateMS_mdata <- function(){
  df <- fread(synGet("syn20929426")$path, data.table = F) %>%
    dplyr::rename(healthCode = uid)
}
elevateMS_mdata <- get_elevateMS_mdata()
elevateMS <- get_elevateMS_data()


##############
#### Brighten
##############
get_brighten_data <- function(){
  df <- fread(synGet("syn20929414")$path, data.table = F) %>%
    dplyr::rename(healthCode = uid)
}
get_brighten_mdata <- function(){
  df <- fread(synGet("syn20929425")$path, data.table = F) %>%
    dplyr::rename(healthCode = uid)
}
brighten_mdata_v1 <- get_brighten_mdata() 
brighten_v1 <- get_brighten_data()


#############
#Phendo
#############
get_phendo_data <- function(){
  df <- fread(synGet("syn20929423")$path, data.table = F) %>%
    dplyr::rename(healthCode = uid)
}
get_phendo_mdata <- function(){
  df <- fread(synGet("syn20929437")$path, data.table = F) %>%
    dplyr::rename(healthCode = uid)
}
phendo <- get_phendo_data()
phendo_mdata <- get_phendo_mdata()


##############
#### Asthma 
##############
### The user-level engagement data for the Asthma study is available directly from the Asthma portal - https://www.synapse.org/#!Synapse:syn8361748
#### To access Asthma dataset, will require a similar but separate Conditions for Use agreement. See the link above for more details
#### Step 1- Obtain data access for Asthma data set here - https://www.synapse.org/#!Synapse:syn8361748
#### Step 2 - Once the data access is obtained the following script will work to generate the copy of the data
source('curation_scripts/curate_asthma_public.R')
## Running this script will generate 'Asthma_engagement.tsv' and 'Asthma_metadata.tsv'
get_asthma_engagement_data <- function(){
  df <- fread("Asthma_engagement.tsv", data.table = F) %>%
    dplyr::rename(healthCode = uid)
}
get_asthma_mdata <- function() {
  fread("Asthma_metadata.tsv", data.table = F) %>%
    dplyr::rename(healthCode = uid)
}
asthma <- get_asthma_engagement_data()
asthma_mdata <- get_asthma_mdata()
unlink("Asthma_engagement.tsv")
unlink("Asthma_metadata.tsv")


#######################
#### MyHeartCounts
#######################
### The user-level engagement data for the MyHeartCounts study is available directly from the Asthma portal - https://www.synapse.org/#!Synapse:syn11269541/wiki/485633
#### To access MyHeartCounts dataset, will require a similar but separate Conditions for Use agreement. See the link above for more details
#### Step 1- Obtain data access for MyHeartCounts data set here - https://www.synapse.org/#!Synapse:syn11269541/wiki/485633
#### Step 2 - Once the data access is obtained the following script will work to generate the copy of the data
source('curation_scripts/curate_my_heart_counts_public.R')
## Running this script will generate 'MyHeartCounts_engagement.tsv' and 'MyHeartCounts_metadata.tsv'

get_myHeartCounts_data <- function(){
  df <- fread("MyHeartCounts_engagement.tsv", data.table = F) %>%
    dplyr::rename(healthCode = uid)
}
get_myHeartCounts_mdata <- function(){
  df <- fread("MyHeartCounts_metadata.tsv", data.table = F) %>%
    dplyr::rename(healthCode = uid)
}
myHeartCounts_mdata <- get_myHeartCounts_mdata()
myHeartCounts <- get_myHeartCounts_data()
unlink("MyHeartCounts_engagement.tsv")
unlink("MyHeartCounts_metadata.tsv")



############
### MetaData - Combine DF for all selected meta-data
############
METADATA <- rbind.fill(sleepHealth_mdata, start_mdata, mpower_mdata,
                       asthma_mdata, elevateMS_mdata, brighten_mdata_v1,
                       myHeartCounts_mdata,phendo_mdata) %>%
  dplyr::filter(! (is.na(healthCode) | healthCode == 'NA' | healthCode == '')) %>%
  dplyr::mutate(race_ethnicity = case_when(
    race_ethnicity %in% c('NULL') ~ 'NA',
    race_ethnicity %in% c('Prefer not to answer', 'More than one') ~ 'Other',
    race_ethnicity %in% c('Native Hawaiian or other Pacific Islander') ~ 'Hawaiian or other Pacific Islander',
    TRUE ~ race_ethnicity )) %>% 
  tidyr::gather(demog, demogVal, c('gender', 'age_group', 'state', 'diseaseStatus',
                                   'clinicalReferral', 'race_ethnicity', 'caseStatus')) %>%
  dplyr::mutate(demogVal = ifelse( (is.na(demogVal) | demogVal == 'NA'  | demogVal == ''), 'NA/Missing', demogVal))  %>%
  tidyr::spread(demog, demogVal)


#################################
# Generate User Engagement Stats
#################################

#The following rather complicated function generates summary stats (one row per participant) for each study
#Implementation per-study calls below
calc_user_participation_stats <- function(df, maxParticipationDay = 84, taskTypes = c('active-sensor', 'survey') ){
  
  ##Calculate true max duration in the study - needed for creating censoring vector (alive/dead) for survival analysis
  TRUE.duration.in.study = df %>% 
    dplyr::filter( !is.na(dayInStudy) & taskType %in% taskTypes ) %>%
    dplyr::group_by(healthCode) %>%  
    dplyr::summarise(TRUE.Duration.in.study = max(dayInStudy, na.rm = T))
  

  ##Calculate true  duration in the study in the first 20 weeks(140 days) - needed for creating censoring vector (alive/dead) for survival analysis
  duration.in.study_first_20Weeks = df %>% 
    dplyr::filter(!is.na(dayInStudy) & taskType %in% taskTypes ) %>%
    dplyr::filter(dayInStudy <=  140) %>% # HARD CODED = 20 weeks = 140 days
    dplyr::group_by(healthCode) %>%  
    dplyr::summarise(duration.in.study.first20Weeks = max(dayInStudy, na.rm = T))
  
  tmp <- df %>% 
    dplyr::filter(dayInStudy <=  maxParticipationDay  & !is.na(dayInStudy) & taskType %in% taskTypes ) %>%
    dplyr::mutate(participantWeek = (dayInStudy %/% 7) + 1) %>%
    dplyr::group_by(healthCode) %>%
    dplyr::summarise(mean_days_between_app_activity = mean(diff(sort(unique(dayInStudy)), lag=1)),
                     median_days_between_app_activity = median(diff(sort(unique(dayInStudy)), lag=1)),
                     SD_days_between_app_activity = median(diff(sort(unique(dayInStudy)), lag=1)),
                     lastWeekinStudy = max(participantWeek),
                     totalDaysActive = n_distinct(dayInStudy),
                     firstDay = min(dayInStudy, na.rm = T),
                     lastDay = max(dayInStudy, na.rm = T),
                     duration_in_study = as.numeric(max(dayInStudy) - min(dayInStudy) + 1),
                     regularity = round((totalDaysActive/duration_in_study) * 100, digits=2))
  
  ##Add the true duration event
  tmp <- merge(tmp,TRUE.duration.in.study, by="healthCode")
  
  ## Add the second duration event based on the first 20 weeks
  merge(tmp, duration.in.study_first_20Weeks, by="healthCode")
}

#Using active data (active sensor-based + survey tasks ) only 
mpower_user_stats <- mpower %>% calc_user_participation_stats() %>% mutate(study = 'mPower')
asthma_user_stats <- asthma %>% calc_user_participation_stats() %>% mutate(study = 'Asthma')
sleepHealth_user_stats <- sleepHealth %>% calc_user_participation_stats() %>% mutate(study = 'SleepHealth')
start_user_stats <- start %>% calc_user_participation_stats() %>% mutate(study = 'Start')
elevateMS_user_stats <- elevateMS %>% calc_user_participation_stats() %>% mutate(study = 'ElevateMS')
myHeartCounts_user_stats <- myHeartCounts %>% calc_user_participation_stats() %>% mutate(study = 'MyHeartCounts')
brighten_v1_user_stats <- brighten_v1 %>% calc_user_participation_stats() %>% mutate(study = 'Brighten')
phendo_userStats <-  phendo %>% calc_user_participation_stats() %>% mutate(study = 'Phendo')
userStats <- rbind.fill(sleepHealth_user_stats,
                        start_user_stats,
                        mpower_user_stats,
                        asthma_user_stats,
                        elevateMS_user_stats,
                        brighten_v1_user_stats,
                        myHeartCounts_user_stats,
                        phendo_userStats)

####################
##### Filtering out users who joined the study and left the study within the first 7 days
##### ref - https://www.apple.com/newsroom/2015/03/09Apple-Introduces-ResearchKit-Giving-Medical-Researchers-the-Tools-to-Revolutionize-Medical-Studies/
####################
# The list of healthCode(userid's) that need to be excluded are saved on the Synapse page - https://www.synapse.org/#!Synapse:syn21518240
REMOVE_HEALTHCODES = fread(synGet("syn21518240")$path, data.table=F)

#Filtering out 
userStats <- userStats %>% dplyr::filter( ! healthCode %in% REMOVE_HEALTHCODES$healthCode)
METADATA <- METADATA %>% dplyr::filter( ! healthCode %in% REMOVE_HEALTHCODES$healthCode)

#Merge engagement stats & metadata
userStats <- merge(userStats, METADATA, by=c("healthCode", "study"), all=T)
userStats <- userStats %>% 
  tidyr::gather(demog, demogVal, c('gender', 'age_group', 'state', 'clinicalReferral', 'race_ethnicity', 'caseStatus')) %>%
  dplyr::mutate(demogVal = ifelse( (is.na(demogVal) | demogVal == 'NA'  | demogVal == ''), 'NA/Missing', demogVal)) %>%
  tidyr::spread(demog, demogVal)
userStats <- userStats %>% 
  dplyr::mutate(study= factor(study,levels = STUDY_COLS$study),
                age_group = revalue(age_group,
                             replace=c('(17,29]' = '18-29',
                                       '(29,39]' = '30-39',
                                       '(39,49]' = '40-49',
                                       '(49,59]' = '50-59',
                                       '(59,120]' = '60+')))




nrow(userStats)

##Save a local copy of the data to avoid reloading the time-consuming data prep for every downstream scripts
save.image('tmp_digitalHealth_retentiondata.RData')


