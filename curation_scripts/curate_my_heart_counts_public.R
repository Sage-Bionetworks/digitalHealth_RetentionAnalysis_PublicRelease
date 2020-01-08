library(synapser)
library(tidyverse)
library(lubridate)
library(zipcode)
data(zipcode)

read_syn_table <- function(syn_id) {
  q <- synTableQuery(paste('select "healthCode","recordId","createdOn",',
                           '"appVersion","phoneInfo"',
                           'from', syn_id))
  return(q$asDataFrame())
}

curate_table <- function(synId, activity_name) {
  df <- read_syn_table(synId) %>%
        select(-ROW_ID, -ROW_VERSION) %>%
        mutate(activity = activity_name,
               originalTableId = synId)
}

curate_my_heart_counts <- function() {
  day_one_survey <- curate_table("syn16782072", "day_one_survey")
  par_q_survey <- curate_table("syn16782071", "par_q_survey")
  daily_check_survey <- curate_table("syn16782070", "daily_check_survey")
  a_and_s_survey <- curate_table("syn16782069", "activity_and_sleep_survey")
  risk_factor_survey <- curate_table("syn16782068", "risk_factor_survey")
  cardio_diet_survey <- curate_table("syn16782067", "cardio_diet_survey")
  satisfied_survey <- curate_table("syn16782066", "satisfied_survey")
  aph_heart_age_survey <- curate_table("syn16782065", "aph_heart_age_survey")
  six_minute_walk <- curate_table("syn16782064", "six_minute_walk_activity")
  demographics <- curate_table("syn16782063", "demographics")
  health_kit_data <- curate_table("syn16782062", "health_kit_data")
  health_kit_sleep <- curate_table("syn16782061", "health_kit_sleep")
  health_kit_workout <- curate_table("syn16782060", "health_kit_workout")
  motion_tracker <- synTableQuery("select * from syn16782059")$asDataFrame() %>%
    select(recordId, healthCode, createdOn) %>%
    mutate(activity = "motion_tracker",
           originalTableId = "syn16782059")
  my_heart_counts <- bind_rows(
    day_one_survey, par_q_survey, daily_check_survey, a_and_s_survey,
    risk_factor_survey, cardio_diet_survey, satisfied_survey,
    aph_heart_age_survey, six_minute_walk, demographics, health_kit_data,
    health_kit_sleep, health_kit_workout, motion_tracker) %>%
    as_tibble()
  return(my_heart_counts)
}

mutate_participant_week_day <- function(engagement) {
  first_activity <- engagement %>%
    group_by(healthCode) %>%
    summarise(first_activity_time = min(createdOn, na.rm=T))
  engagement <- inner_join(engagement, first_activity)
  engagement <- engagement %>%
    mutate(
      seconds_since_first_activity = createdOn - first_activity_time,
      participantWeek = as.integer(
          floor(as.numeric(
            as.duration(seconds_since_first_activity), "weeks"))),
      participantDay = as.integer(
        floor(as.numeric(
          as.duration(seconds_since_first_activity), "days"))) + 1
    ) %>%
    select(-first_activity_time, -seconds_since_first_activity)
  return(engagement)
}

mutate_task_type <- function(engagement_data) {
  engagement_data %>%
    mutate(taskType = case_when(
      endsWith(activity, "survey") ~ "survey",
      startsWith(activity, "health_kit") ~ "passive-sensor",
      activity == "demographics" ~ "survey",
      activity == "motion_tracker" ~ "passive-sensor",
      activity == "six_minute_walk_activity" ~ "active-sensor"))
}

mutate_task_frequency <- function(engagement_data) {
  engagement_data %>%
    mutate(taskFrequency = case_when(
      activity %in% c("daily_check_survey", "six_minute_walk_activity") ~ "daily",
      activity %in% c("cardio_diet_survey", "risk_factor_survey",
                      "activity_and_sleep_survey", "satisfied_survey",
                      "aph_heart_age_survey", "par_q_survey",
                      "day_one_survey", "demographics") ~ "baseline",
      startsWith(activity, "health_kit") ~ "continuous",
      activity == "motion_tracker" ~ "continuous"))
}

demographics_tz_from_zip_prefix <- function(demographics_synId) {
  demographics <- synTableQuery(paste(
    "select healthCode, zip3 from", demographics_synId))
  demographics <- demographics$asDataFrame() %>%
    as_tibble() %>%
    select(-ROW_ID, -ROW_VERSION) %>%
    distinct(healthCode, zip3)
  zip <- zipcode %>%
    mutate(zip3 = as.integer(str_sub(zip, 1, 3))) %>%
    group_by(zip3) %>%
    summarize(lat = median(latitude), long = median(longitude))
  zip_state <- zipcode %>%
    mutate(zip3 = as.integer(str_sub(zip, 1, 3))) %>%
    count(zip3, state) %>%
    group_by(zip3) %>%
    slice(which.max(n)) %>%
    select(zip3, state)
  timezones <- purrr::map2(
    zip$lat, zip$long, lutz::tz_lookup_coords, method = "fast") %>%
    unlist()
  zip <- zip %>% mutate(timezone = timezones)
  demographics <- demographics %>% left_join(zip, by = "zip3")
  demographics <- demographics %>% left_join(zip_state, by = "zip3")
  travelers <- demographics %>%
    group_by(healthCode) %>%
    summarize(n_tz = n_distinct(timezone)) %>%
    filter(n_tz > 1)
  demographics <- demographics %>%
    filter(!(healthCode %in% travelers$healthCode))
  return(demographics)
}

mutate_local_time <- function(engagement) {
  demographics <- demographics_tz_from_zip_prefix("syn16782066")
  engagement <- engagement %>%
    left_join(demographics, by="healthCode")
  local_time <- purrr::map2(engagement$createdOn, engagement$timezone, with_tz) %>%
    purrr::map(as.character) %>%
    unlist()
  engagement <- engagement %>% dplyr::mutate(
    createdOnLocalTime = local_time,
    createdOnLocalTime = ifelse(is.na(timezone), NA, createdOnLocalTime)) %>%
    select(-zip3, -lat, -long)
  return(engagement)
}

curate_my_heart_counts_metadata <- function(engagement) {

  df <- synTableQuery("select * from syn16782063")$asDataFrame() %>%
    dplyr::rename(age = patientCurrentAge,
                  gender = patientBiologicalSex) %>%
    dplyr::mutate( age_group = cut(age, breaks=c(17,29,39,49, 59, 120))) %>%
    arrange(desc(createdOn)) %>%
    distinct(healthCode, .keep_all = T) %>%
    dplyr::select(healthCode, age,age_group, gender, phoneInfo)

  #integrate the heartCondition
  #See - https://github.com/Sage-Bionetworks/mhealth-engagement-analysis/issues
  diseaseStatus <- synTableQuery("select * from syn16782071")$asDataFrame() %>%
    dplyr::mutate(createdOn = as.Date(lubridate::ymd_hms(createdOn))) %>%
    arrange(desc(createdOn)) %>%
    distinct(healthCode, .keep_all = T) %>%
    dplyr::select(healthCode, heartCondition) %>%
    dplyr::rename(caseStatus = heartCondition)
  df = merge(df, diseaseStatus, all=T)

  ##Add state info
  state.metadata = data.frame(state.name = state.name,
                              state.abb = state.abb,
                              state.region = state.region) %>%
    dplyr::mutate(state.name = as.character(state.name),
                  state.abb = as.character(state.abb),
                  state.region = as.character(state.region))
  state.metadata <- rbind(state.metadata, c('District of Columbia', 'DC', 'South'))
  my_heart_counts_states <- engagement %>%
    group_by(healthCode) %>%
    arrange(createdOn) %>%
    summarise(state = state[1]) %>%
    left_join(state.metadata, by = c("state" = "state.abb")) %>%
    mutate(state = stringr::str_to_title(state.name)) %>%
    select(-state.name)
  df <- merge(df, my_heart_counts_states, by="healthCode", all=T)


  tmp_find_last_val <- function(x){
    x <-  na.omit(as.character(x))
    ifelse(length(x) == 0, 'NA', x[length(x)])
  }

  #race
  additionDemogData <- synTableQuery("select * from syn16782065")$asDataFrame() %>%
    dplyr::transmute(healthCode = healthCode,
                     createdOn = lubridate::ymd_hms(createdOn),
                     race = heartAgeDataEthnicity,
                     age.fromHeartAgeSurvey = as.numeric(heartAgeDataAge),
                     gender.fromHeartAgeSurvey  = as.character(heartAgeDataGender)) %>%
    dplyr::distinct(healthCode, race, age.fromHeartAgeSurvey, gender.fromHeartAgeSurvey, .keep_all=T) %>%
    dplyr::group_by(healthCode) %>%
    dplyr::arrange(createdOn) %>%
    dplyr::summarise(race = tmp_find_last_val(race),
                     gender.fromHeartAgeSurvey = tmp_find_last_val(gender.fromHeartAgeSurvey),
                     age.fromHeartAgeSurvey = tmp_find_last_val(age.fromHeartAgeSurvey)) %>%
    dplyr::mutate(gender.fromHeartAgeSurvey = ifelse(gender.fromHeartAgeSurvey == 'NA', NA, gender.fromHeartAgeSurvey),
                  age.fromHeartAgeSurvey = ifelse(age.fromHeartAgeSurvey == 'NA', NA, age.fromHeartAgeSurvey),)

  #Fix race col
  additionDemogData <- additionDemogData %>%
    dplyr::mutate(race = case_when(
      race %in% c('Alaska Native', 'American Indian') ~ 'AIAN',
      race == 'Hispanic' ~ 'Hispanic/Latinos',
      race == 'White' ~ 'Non-Hispanic White',
      race == 'Black' ~ 'African-American/Black',
      race == 'I prefer not to indicate an ethnicity' ~ 'Prefer not to answer',
      race == 'Pacific Islander' ~ 'Hawaiian or other Pacific Islander',
      TRUE ~ race
  ))

  #merge race and other engagement data
  df <- merge(df, additionDemogData, all.x=T)

  #replace missing values from additionalDemogData
  to.replace <- is.na(df$gender)
  df$gender[to.replace] =  df$gender.fromHeartAgeSurvey[to.replace]
  df$gender.fromHeartAgeSurvey <- NULL

  to.replace <- is.na(df$age)
  df$age[to.replace] <-  df$age.fromHeartAgeSurvey[to.replace]
  df$age.fromHeartAgeSurvey <- NULL
  df['study'] = 'MyHeartCounts'

  return(as_tibble(df))
}

main <- function() {
  synLogin()
  my_heart_counts <- curate_my_heart_counts() %>%
    mutate_participant_week_day() %>%
    mutate_local_time() %>% # adds state from demographics file
    mutate_task_type()

  my_heart_counts_metadata <- curate_my_heart_counts_metadata(my_heart_counts)

  my_heart_counts %>%
    mutate(study = "MyHeartCounts", hourOfDayUTC = lubridate::hour(createdOn)) %>%
    select(study, uid = healthCode, dayInStudy = participantDay,
           hourOfDayUTC, taskType) %>%
    write_tsv("MyHeartCounts_engagement.tsv")

  my_heart_counts_metadata %>%
    select(study, uid = healthCode, age_group, gender, diseaseStatus = caseStatus,
           state, race_ethnicity = race) %>%
    write_tsv("MyHeartCounts_metadata.tsv")
}

main()
