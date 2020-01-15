rm(list=ls())
options(stringsAsFactors = F)
library("install.load")
install_load("data.table", "gdata", "synapser", "jsonlite")
install_load("plyr", "tidyverse", "doMC", "scales", "data.table")
install_load("gridExtra", "pheatmap", "printr", "ggthemes", "anytime", "RCurl")
synapser::synLogin()

# To be able to run this script you will need to run the "loadData.R" script in the same folder and save the output
load("tmp_digitalHealth_retentiondata.RData")


### Filter out regions 
state.pop <- us.pop.data  %>% 
  dplyr::filter(!str_detect(NAME, 'Region')) %>%
  dplyr::filter(! NAME == 'United States') %>%
  dplyr::mutate(proportion = POPESTIMATE2018/sum(POPESTIMATE2018)) %>%
  dplyr::mutate(proportion = round(proportion*100, digits=2)) %>%
  dplyr::rename(state = NAME) %>%
  dplyr::arrange(proportion) %>% 
  dplyr::mutate(cum_proportion = cumsum(proportion))

study.state.wise <- rbind.fill(
  mpower_mdata %>% select(study, state),
  start_mdata %>% select(study, state),
  myHeartCounts_mdata %>% select(study, state),
  asthma_mdata %>% select(study, state),
  brighten_mdata_v1 %>% select(study, state),
  phendo_mdata %>% select(study, state)
  #elevateMS_mdata %>% select(study, state) (excluded due to % missing)
) %>%
  dplyr::mutate(state = gsub('Of', 'of', state))


##Only US states
study.state.wise <- study.state.wise %>% filter(state %in% state.pop$state)

### #users / state
users_per_state_per_study <- study.state.wise %>% 
  dplyr::count(study, state) %>% 
  group_by(study) %>%
  dplyr::mutate(totalN = sum(n)) %>%
  dplyr::mutate(percent = round( (n/totalN)*100, digits=3)) %>%
  as.data.frame() 

STUDY_COLS_FLT <- STUDY_COLS %>% filter(study %in% unique(users_per_state_per_study$study)) 

##US MAP - showing the percent higher/lower recruitment w.r.t US State population
install_load("plotly", "processx")
# give state boundaries a white border
l <- list(color = toRGB("white"), width = 2)
# specify some map projection/options
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)
tmp <- merge(per_state_summary, state.metadata, by.x='state', by.y='state.name', all=T)
p <-  tmp %>% filter(abs(deviation_percent) < 100) %>% 
  plot_geo(locationmode = 'USA-states') %>%
  add_trace(z = ~deviation_percent, locations = ~state.abb,
            color = ~deviation_percent, colors = 'YlOrBr') %>%
  colorbar(title = "Percentage") %>%
  layout(geo = g)
p
orca(p, "/Figs_N_Tables/USState_Map_percentRecruitment.png")
orca(p, "/Figs_N_Tables/USState_Map_percentRecruitment.svg")
orca(p, "/Figs_N_Tables/USState_Map_percentRecruitment.pdf")








