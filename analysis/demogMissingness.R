rm(list=ls())
library("install.load")
install_load("data.table", "gdata", "synapser", "jsonlite")
install_load("plyr", "tidyverse", "doMC", "scales", "data.table")
install_load("gridExtra", "ggthemes", "anytime")
synapser::synLogin()


#Load Data
load(synGet('syn18941815')$path)


#################################################
#Table - Missing proportions for each demog var in each study
#################################################

#Add the per study N
demogMissingness_summary <- userStats %>% 
  dplyr::select(healthCode,study,  age_group, gender, race, state) %>%
  tidyr::gather(demog, demogVal, c('age_group', 'gender', 'race', 'state')) %>%
  dplyr::group_by(study) %>% 
  dplyr::mutate(per_study_N = n_distinct(healthCode)) %>%
  dplyr::filter(demogVal == 'NA/Missing') %>%
  dplyr::count(study, demog, per_study_N) %>%
  dplyr::mutate(percentMissing = round( (n/per_study_N)*100, digits=2)) 

### Fill back things if a certain demog is not missing at all - it will not have a value in demog summary
tmp.df <- expand.grid(unique(demogMissingness_summary$study),
            unique(demogMissingness_summary$demog))
colnames(tmp.df) <- c('study', 'demog')
demogMissingness_summary <- merge(demogMissingness_summary, tmp.df, all=T) %>%
  dplyr::mutate(percentMissing = ifelse(is.na(percentMissing), 0, percentMissing))

# Fix two rows 
# SleepHealth - State info not available
# Start - Race not available
demogMissingness_summary <- demogMissingness_summary %>% 
  dplyr::mutate(percentMissing =ifelse( (study == 'Start' & demog == 'race'), NA,  percentMissing),
                percentMissing =ifelse( (study == 'SleepHealth' & demog == 'state'), NA,  percentMissing))


##Missingness plot
p <- ggplot(data=demogMissingness_summary, aes(x=demog, y=percentMissing)) + geom_boxplot(width=0.3) 
p <- p + geom_point(aes(color=study), size=1.5, alpha=0.7) 
p <- p + coord_flip() + theme_bw(base_size = 15) + scale_y_continuous(limits =c(0,100))
p <- p + xlab('Demographics') + ylab('Percent') 
p <- p +  scale_color_manual(breaks=STUDY_COLS$study, values=STUDY_COLS$color) 
p <- p + scale_x_discrete( breaks =c('state', 'race', 'gender', 'age_group'),
                      labels = c('State', 'Race/Ethnicity', 'Sex', 'Age'))
p <- p + theme(legend.title = element_blank())
p
ggsave("Figs_N_Tables/missingDemographics.png", plot=p, height = 5, width = 8, dpi = 200)
ggsave("Figs_N_Tables/missingDemographics.tiff", plot=p, height = 5, width = 8, dpi = 200)


demogMissingTable <- demogMissingness_summary %>%
  dplyr::mutate(value = paste0(percentMissing, ' (', n, ')' )) %>%
  dplyr::select(-per_study_N, -n, -percentMissing) %>%
  tidyr::spread(study, value, fill=0)
head(demogMissingTable)
write.table(demogMissingTable, file = "Figs_N_Tables/demogMissingness_stats.tsv", row.names = F, quote=F, sep="\t")
