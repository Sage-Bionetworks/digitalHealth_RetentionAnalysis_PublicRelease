library(install.load)
install_load("lutz")
library(synapser)
library(tidyverse)
library(lubridate)
library(zipcode)
data(zipcode)

read_syn_table <- function(synId) {
  q <- synTableQuery(paste('select recordId, healthCode, createdOn,',
                           'appVersion, phoneInfo', 'from', synId))
  t <- q$asDataFrame()
  return(t)
}

curate_table <- function(synId, activityName) {
  df <- read_syn_table(synId) %>%
    select(-ROW_ID, -ROW_VERSION) %>%
    mutate(originalTableName = activityName,
           originalTableId = synId)
}

curate_asthma <- function() {
  about_you <- curate_table("syn8466446", "About You Survey")
  asthma_history <- curate_table("syn8466443", "Asthma History Survey")
  asthma_medication <- curate_table("syn8466442", "Asthma Medication Survey")
  daily_prompt <- curate_table("syn8466439", "Daily Prompt Survey")
  demographics <- curate_table("syn8466435", "Demographics Survey")
  eq5d <- curate_table("syn8466434", "EQ5D Survey")
  medical_history <- curate_table("syn8466433", "Medical History Survey")
  milestone <- curate_table("syn8466432", "Milestone Survey")
  weekly_prompt <- curate_table("syn8466431", "Weekly Prompt Survey")
  your_asthma <- curate_table("syn8466429", "Your Asthma Survey")
  asthma_engagement <- bind_rows(
    about_you, asthma_history, asthma_medication, daily_prompt, demographics,
    eq5d, medical_history, milestone, weekly_prompt, your_asthma)
  return(asthma_engagement)
}

mutate_participant_week_day <- function(asthma_engagement) {
  first_activity <- asthma_engagement %>%
    group_by(healthCode) %>%
    summarise(firstActivity = min(createdOn, na.rm=T))
  asthma_engagement <- inner_join(asthma_engagement, first_activity)
  asthma_engagement <- asthma_engagement %>%
    mutate(
      seconds_since_first_activity = createdOn - firstActivity,
      participantWeek = as.integer(
        floor(as.numeric(
          as.duration(seconds_since_first_activity), "weeks"))),
      participantDay = as.integer(
        floor(as.numeric(
          as.duration(seconds_since_first_activity), "days"))) + 1
    ) %>%
    select(-seconds_since_first_activity)
  return(asthma_engagement)
}

demographics_tz_from_zip_prefix <- function(demographics_synId) {
  demographics <- synTableQuery(paste(
    "select healthCode, zip3 from", demographics_synId))
  demographics <- demographics$asDataFrame() %>%
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
  demographics <- demographics_tz_from_zip_prefix("syn9993530")
  engagement <- engagement %>%
    left_join(demographics, by="healthCode")
  local_time <- purrr::map2(engagement$createdOn, engagement$timezone, with_tz) %>%
    purrr::map(as.character) %>%
    unlist()
  engagement <- engagement %>% dplyr::mutate(
    createdOnLocalTime = local_time,
    createdOnLocalTime = ifelse(is.na(timezone), NA, createdOnLocalTime))
  return(engagement)
}

mutate_task_type <- function(asthma_engagement) {
  asthma_engagement %>% mutate(taskType = "survey")
}

curate_asthma_metadata <- function(engagement) {
  df <- synTableQuery("select * from syn8466435")$asDataFrame()
  state.metadata = data.frame(state.name = state.name,
                              state.abb = state.abb,
                              state.region = state.region) %>%
    dplyr::mutate(state.name = as.character(state.name),
                  state.abb = as.character(state.abb),
                  state.region = as.character(state.region))
  state.metadata <- rbind(state.metadata, c('District of Columbia', 'DC', 'South'))
  asthma_states <- engagement %>%
    group_by(healthCode) %>%
    arrange(createdOn) %>%
    summarise(state = state[1]) %>%
    left_join(state.metadata, by = c("state" = "state.abb")) %>%
    mutate(state = stringr::str_to_title(state.name)) %>%
    select(-state.name)

  about_you <- as_tibble(synTableQuery("select * from syn8466446")$asDataFrame()) %>%
    select(healthCode, ethnicity, race, Income, education)

  ethnicity_mapping <- c("Hispanic/Latino", "Non-Hispanic/Latino",
                         "I choose not to answer")
  race_mapping <- c("1" = "Black/African American",
                    "2" = "Asian",
                    "3" = "American Indian or Alaskan Native",
                    "4" = "Hawaiian or other Pacific Islander",
                    "5" = "White",
                    "6" = "Other",
                    "7" = "I choose not to answer")
  income_mapping <- c("<14,999", "15,000-21,999", "22,000−43,999", "44,000-60,000",
                      ">60,000", "I don't know", "I choose not to answer")
  education_mapping <- c("8th grade or less", "More than 8th grade but did not graduate high school",
                         "High school graduate or equivalent", "Some college",
                         "Graduate of Two Year College or Technical School",
                         "Graduate of Four Year College", "Post graduate studies",
                         "I choose not to answer")
  about_you$ethnicity <- recode(about_you$ethnicity, !!!ethnicity_mapping)
  for (i in 1:length(race_mapping)) {
    .x <- names(race_mapping)[[i]]
    .y <- race_mapping[[i]]
    about_you$race <- stringr::str_replace(about_you$race, .x, .y)
  }
  about_you$Income <- recode(about_you$Income, !!!income_mapping)
  about_you$education <- recode(about_you$education, !!!education_mapping)
  about_you <- about_you %>%
    rename(income = Income)
  df <- df %>%
    dplyr::mutate(gender = biologicalSex) %>% as.data.frame() %>%
    dplyr::select(healthCode, phoneInfo, age, gender) %>%
    dplyr::mutate(age_group = cut(age, breaks=c(17,29,39,49, 59, 120))) %>%
    left_join(asthma_states, by = "healthCode") %>%
    full_join(about_you, by = "healthCode") %>%
    as_tibble()
  #Update race to Hispanic/Latino here Hispanic = "Yes"
  df <- df %>% mutate(race = ifelse(ethnicity == 'Hispanic/Latino', 'Hispanic/Latinos', race))

  #Update other race
  df <- df %>% dplyr::mutate(race = case_when(
    race == 'Black/African American' ~ 'African-American/Black',
    race == 'American Indian or Alaskan Native' ~ 'AIAN',
    race == 'White' ~ 'Non-Hispanic White',
    race == 'Hispanic/Latinos' ~ 'Hispanic/Latinos',
    race == 'Asian' ~ 'Asian',
    grepl('Other', race, perl=T)  ~ 'Other',
    grepl('Hawaiian|Pacific', race, perl=T) ~ 'Hawaiian or other Pacific Islander',
    grepl('I choose not to answer', race, perl=T) ~ 'Prefer not to answer',
    grepl(',', race) ~ 'More than one',
    is.na(race) ~ 'NA',
    TRUE ~ race)) %>%
    dplyr::mutate(race = ifelse(race == 'NA', NA, race))

  #Update Education
  df <- df %>%
    dplyr::mutate(education = case_when(
    education %in%  c('8th grade or less', 'More than 8th grade but did not graduate high school') ~ 'Below High School',
    education %in% c('Some college', 'Graduate of Four Year College', 'Graduate of Two Year College or Technical School') ~ 'College',
    education %in% c('High school graduate or equivalent') ~ 'High School',
    grepl('College', education, perl=T, ignore.case = T)  ~ 'College',
    grepl('Post graduate', education, perl=T, ignore.case = T)  ~ 'Post graduate',
    grepl('High school', education, perl=T, ignore.case = T)  ~ 'High School',
    TRUE ~ education))

  #Disease Status
  diseaseStatus <- synTableQuery("select * from syn8466443")$asDataFrame() %>%
    select(healthCode, age_when_diagnosed) %>%
    mutate(caseStatus = ifelse(is.na(age_when_diagnosed), F, T)) %>%
    select(-age_when_diagnosed)
  df <- merge(df, diseaseStatus, all=T)
  df <- df %>% dplyr::mutate(study='Asthma')

  return(df)
}

main <- function() {
  synLogin()

  asthma_engagement <- curate_asthma() %>%
    mutate_participant_week_day() %>%
    mutate_local_time() %>% # adds state from demographics file
    mutate_task_type()

  asthma_metadata <- curate_asthma_metadata(asthma_engagement)

  asthma_engagement %>%
    mutate(study = "Asthma", hourOfDayUTC = lubridate::hour(createdOn)) %>%
    select(study, uid = healthCode, dayInStudy = participantDay,
           hourOfDayUTC, taskType) %>%
    write_tsv("Asthma_engagement.tsv")

  asthma_metadata %>%
    select(study, uid = healthCode, age_group, gender,
           diseaseStatus = caseStatus, state, race_ethnicity = race) %>%
    write_tsv("Asthma_metadata.tsv")
}

main()
