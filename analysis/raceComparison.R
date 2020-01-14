rm(list=ls())
library("install.load")
install_load("data.table", "gdata", "synapser", "jsonlite")
install_load("plyr", "tidyverse", "doMC", "scales", "data.table")
install_load("gridExtra", "ggthemes", "anytime")

# To be able to run this script you will need to run the "loadData.R" script in the same folder and save the output
load("tmp_digitalHealth_retentiondata.RData")


race.flt <- METADATA %>%  
  dplyr::select(healthCode, study, race_ethnicity)  %>%
  filter(!race_ethnicity == 'NA/Missing') %>%
  dplyr::group_by(study) %>%
  dplyr::mutate(studyN = n()) %>%
  dplyr::count(study, race_ethnicity, studyN) %>%
  dplyr::mutate(prop =  round( (n/studyN)*100, digits=2)) %>%
  dplyr::filter(!race_ethnicity %in% c('Other'))


#ref1 - https://www.census.gov/quickfacts/fact/table/US/IPE120217
#ref2 - https://www.census.gov/prod/cen2010/briefs/c2010br-02.pdf (USED)
#ref3 - https://www.census.gov/mso/www/training/pdf/race-ethnicity-onepager.pdf
us.2010.census.race <- data.frame(race = c("African-American/Black", "AIAN", "Asian", 
                                           "Hawaiian/Pacific Islander", "Hispanic/Latinos",
                                           "Non-Hispanic White"),
                                  us.census.prop = c(12.6, 0.9, 4.8, 0.2, 16.3, 63.7), 
                                  col = '2010 Census')
race.order <- us.2010.census.race %>% arrange(us.census.prop) %>% .$race
  
race.flt.summary <- race.flt %>% group_by(race_ethnicity) %>%
  dplyr::summarise(median.prop = median(prop, na.rm = T),
                   iqr = IQR(prop),
                   sd.prop = sd(prop, na.rm=T),
                   col = 'Recruited population') %>%
  dplyr::mutate(race = gsub(' or other ', '/', race_ethnicity))


### Calculate the avg difference between recruited pop and census prop
head(race.flt.summary)
head(us.2010.census.race)


p <- ggplot(data=race.flt.summary, aes(x=factor(race,levels=race.order), y=median.prop, col=col)) + geom_point()
p <- p + theme_bw(base_size = 15) + xlab('Race/Ethnicity') + ylab('Percent') 
p <- p  + geom_errorbar(aes(ymin = median.prop-iqr/2, ymax = median.prop+iqr/2), width=0.1, color="grey40") 
p <- p  + geom_point(data=us.2010.census.race, aes(x=race, y=us.census.prop, col=col)) + coord_flip() 
p <- p + scale_color_manual(values=c("red", "black"))
p <- p + theme(legend.title = element_blank(), legend.position = "none")
p
# ggsave("Figs_N_Tables/raceComparison.png", plot = p, height = 5, width=6, units="in", dpi=250)
# ggsave("Figs_N_Tables/raceComparison.tiff", plot = p, height = 5, width=6, units="in", dpi=250)



### INSET -- zoomed in plot
p <- ggplot(data=race.flt.summary %>% 
              filter(race %in% c('AIAN', 'Hawaiian/Pacific Islander')),
            aes(x=factor(race,levels=race.order), y=median.prop, col=col)) + geom_point()
p <- p + theme_light(base_size = 15) + xlab('Race/Ethnicity proportion ') + ylab('Percent') 
p <- p  + geom_errorbar(aes(ymin = median.prop-iqr/2, ymax = median.prop+iqr/2), width=0.1, color="grey40") 
p <- p  + geom_point(data=us.2010.census.race %>% filter(race %in% c('AIAN', 'Hawaiian/Pacific Islander')),
                     aes(x=race, y=us.census.prop, col=col)) + coord_flip() 
p <- p + scale_color_manual(values=c("red", "black"))
p <- p + theme(legend.position = "none", axis.text.y = element_blank()) + xlab("") + ylab("")
p          
# ggsave("Figs_N_Tables/raceComparison_INSET.png", plot = p, height = 1, width=3, units="in", dpi=250)
# ggsave("Figs_N_Tables/raceComparison_INSET.tiff", plot = p, height = 1, width=3, units="in", dpi=250)






