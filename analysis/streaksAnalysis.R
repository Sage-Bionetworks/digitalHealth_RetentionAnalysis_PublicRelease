################################
# Longutidinal daily engagement patterns - unsupervised clustering
################################

rm(list=ls())
options(stringsAsFactors = F)
library("install.load")
install_load("data.table", "gdata", "synapser", "jsonlite")
install_load("plyr", "tidyverse", "doMC", "scales", "data.table")
install_load("gridExtra", "ggthemes", "anytime", "synapser")
synapser::synLogin()


# To be able to run this script you will need to run the "loadData.R" script in the same folder and save the output
load("tmp_digitalHealth_retentiondata.RData")


getStreaks <- function(df, mData, selectedUsers){
  df <- df %>%
    dplyr::filter(dayInStudy <= 84 & 
                    healthCode %in% selectedUsers &
                    taskType %in% c('active-sensor', 'survey')) %>%
    dplyr::group_by(healthCode, dayInStudy) %>%
    dplyr::summarise(n = n())
  tmp1 <- expand.grid(dayInStudy = seq(1,84, by=1), healthCode = selectedUsers)
  df <- merge(df, tmp1, all.x=T, all.y=T)
  df <- df %>% mutate(active = ifelse( is.na(n), 0, 1 ))
  df %>% dplyr::mutate(week = ( (dayInStudy -1) %/% 7) + 1,
                       study = unique(na.omit(mData$study)))
}


#mPower
mPower_selected_users <- mpower_user_stats %>% filter(duration_in_study > 7 ) %>% .$healthCode %>% unique()
mpower_streaks <- mpower %>% getStreaks(mpower_mdata, mPower_selected_users)

#SleepHealth
sleepHealth_selected_users <- sleepHealth_user_stats %>% filter(duration_in_study > 7 ) %>% .$healthCode %>% unique()
sleepHealth_streaks <- sleepHealth %>% getStreaks(sleepHealth_mdata, sleepHealth_selected_users)

#Brighten v1
brighten_v1_selected_users <-  brighten_v1_user_stats %>% filter(duration_in_study > 7 ) %>% .$healthCode %>% unique()
brighten_v1_streaks <- brighten_v1 %>% getStreaks(brighten_mdata_v1, brighten_v1_selected_users)


#Asthma
asthma_selected_users <- asthma_user_stats %>% filter(duration_in_study > 7 ) %>% .$healthCode %>% unique()
asthma_streaks <- asthma %>% getStreaks(asthma_mdata, asthma_selected_users)

#Start
start_selected_users <- start_user_stats %>% filter(duration_in_study > 7 ) %>%  .$healthCode %>% unique()
start_streaks <- start %>% getStreaks(start_mdata, start_selected_users)

#myHeartCounts
myHeartCounts_selected_users <- myHeartCounts_user_stats %>%  filter(duration_in_study > 7) %>%  .$healthCode %>% unique()
myHeartCounts_streaks <- myHeartCounts %>% getStreaks(myHeartCounts_mdata, myHeartCounts_selected_users)

#elevateMS
elevateMS_selected_users <- elevateMS_user_stats %>% filter(duration_in_study > 7) %>%  .$healthCode %>% unique()
elevateMS_streaks <- elevateMS %>% getStreaks(elevateMS_mdata, elevateMS_selected_users)

#Phendo
phendo_selected_users <- phendo_userStats %>% filter(duration_in_study > 7 ) %>%  .$healthCode %>% unique()
phendo_streaks <- phendo %>% getStreaks(phendo_mdata, phendo_selected_users)


streaks <- rbind.fill(elevateMS_streaks, myHeartCounts_streaks,
                      start_streaks, asthma_streaks, 
                      brighten_v1_streaks, sleepHealth_streaks,
                      mpower_streaks, phendo_streaks) %>%
  dplyr::filter(!healthCode %in% REMOVE_HEALTHCODES)
streaks %>% group_by(study) %>% summarise(n = n_distinct(healthCode))



##########
#Streaks - Clustering
##########
#1. Determine Optimal K for clustering 
install_load("factoextra")
optimalCluster_N <- function(x){
  cat('processing study', x, '\n')
  tmp <- streaks %>% filter(study == x) %>%
    select(-n, -week, -study) %>% spread(participantDay, active) %>%
    select(-healthCode)
  cat('dataSize-', dim(tmp), '\n')
  set.seed(23443)
  oFile = paste0('Figs_N_Tables/streaks_', x, '_optimalClusterN_WSS_method.png')
  png(oFile, height = 8, width = 8, units = "in", res =100)
  res.wss <- fviz_nbclust(tmp, cluster::pam, method = "wss", k.max = 10) + ggtitle(x)
  ggsave(file = oFile, plot=res.wss, width = 4, height = 4, dpi = 100)
  
  # oFile = paste0('Figs_N_Tables/streaks_', x, '_optimalClusterN_silhouette_method.png')
  # png(oFile, height = 8, width = 8, units = "in", res =100)
  # res.silhouette <- fviz_nbclust(tmp, cluster::pam, method = "silhouette", k.max = 10)
  # ggsave(file = oFile, plot=res.silhouette, width = 4, height = 4, dpi = 100)
  # 
  
  # oFile = paste0('Figs_N_Tables/streaks_', x, '_optimalClusterN_GapStat_method.png')
  # png(oFile, height = 8, width = 8, units = "in", res =100)
  # res.gapStat <- fviz_nbclust(tmp, cluster::pam, method = "gap_stat", k.max = 10, nboot=50)
  # ggsave(file = oFile, plot=res.gapStat, width = 4, height = 4, dpi = 100)
}

optimalCluster_N('ElevateMS')
optimalCluster_N('Brighten')
optimalCluster_N('Phendo')
optimalCluster_N('mPower')
optimalCluster_N('SleepHealth')
optimalCluster_N('Asthma')
optimalCluster_N('MyHeartCounts')
optimalCluster_N('Start')


######
#Cluster based on K=4
######
dailyStreaksHeatmap <- function(x, clusterOrderLevels = NA){
  cat('processing study', x, '\n')
  tmp <- streaks %>% filter(study == x) %>%
    select(-n, -week, -study) %>% spread(participantDay, active)
  healthCode_ordered <- tmp$healthCode
  tmp$healthCode <- NULL
  set.seed(23443)
  cols <- c('#e0e0e0', '#2ca25f')
  oFile = paste0('Figs_N_Tables/streaks_', x, '.tiff')
  tiff(oFile, height = 6, width = 6, units = "in", res =200)
  
  #pre-determine clusters
  km <- kmeans(as.matrix(tmp), centers = 4)
  km.cluster.allocation <- factor(km$cluster)
  
  ###Custom - after looking at the heatmap if the cluster ordering need to changed - 
  ### Mainly for keeping consistent pattern across plots for publication 
  if (is.na(clusterOrderLevels) ==F){
    km.cluster.allocation = factor(km.cluster.allocation, levels=clusterOrderLevels)
  }
  
  res <- ComplexHeatmap::Heatmap(tmp, show_heatmap_legend = F,
                                   split = km.cluster.allocation,
                                   show_row_dend = F, cluster_columns = F,
                                   show_column_names = F, show_row_names = T,
                                   col = circlize::colorRamp2(breaks = c(0,1), cols),
                                   column_title = x, km_title='',
                                   heatmap_legend_param = list(labels_gp = grid::gpar(fontsize = 1)))
  print(res)
  dev.off()

  rowOrderList <- ComplexHeatmap::row_order(res)
  ##convert the row order to original healthCode 
  healthCode_by_clusters <- sapply(rowOrderList, function(x) healthCode_ordered[x])
  #names(healthCode_by_clusters) <-  1:length(healthCode_by_clusters)
  names(healthCode_by_clusters) <- paste0('C', 1:length(healthCode_by_clusters))
  #list to df
  healthCode_by_clusters <- melt(healthCode_by_clusters)
  colnames(healthCode_by_clusters) = c('healthCode', 'cluster')
  healthCode_by_clusters %>% dplyr::mutate(study = x)
}

unique(streaks$study)
#Default heatmaps across all studies
studyWiseClusters <- ldply(unique(streaks$study), function(x){
  dailyStreaksHeatmap(x)
})

### NOW Inspect each study heatmap and determine the order manually and redraw
### The returned cluster allocation are assigned based on the explicit levels
### With the new levels by default the ComplexHeatmap will create clusters in the order 1-4 based on levels specified
Asthma_clusters <- dailyStreaksHeatmap('Asthma', clusterOrder = c('4', '3', '1', '2'))
Brighten_clusters <- dailyStreaksHeatmap('Brighten', clusterOrder = c('3', '4', '1', '2'))
ElevateMS_clusters <- dailyStreaksHeatmap('ElevateMS', clusterOrder = c('3', '1', '4', '2'))
MyHeartCounts_clusters <- dailyStreaksHeatmap('MyHeartCounts', clusterOrder = c('4', '1', '3', '2'))
Phendo_clusters <- dailyStreaksHeatmap('Phendo', clusterOrder = c('2', '1', '3', '4'))
SleepHealth_clusters <- dailyStreaksHeatmap('SleepHealth', clusterOrder = c('1', '4', '2', '3'))
mPower_clusters <- dailyStreaksHeatmap('mPower', clusterOrder = c('2', '3', '4', '1'))
Start_clusters <- dailyStreaksHeatmap('Start', clusterOrder = c('1', '2', '3', '4'))
ALL_STUDY_CLUSTERS <- rbind.fill(ElevateMS_clusters, Phendo_clusters, mPower_clusters,
                                 SleepHealth_clusters, MyHeartCounts_clusters, Asthma_clusters,
                                 Brighten_clusters, Start_clusters) 


### Save clustering results
dataObject = 'StreaksAnalysis_clustering_results.RData'
save.image(dataObject)



################################
### RELOAD CLUSTERING RESULTS
################################
rm(list=ls())
load('StreaksAnalysis_clustering_results.RData')

### HealthCodes <= 7 days
healthCodes.not.clustered <- userStats %>% filter(duration_in_study <= 7 ) %>%
  dplyr::select(healthCode, study) %>%
  dplyr::mutate(cluster = 'C5*')
ALL_STUDY_CLUSTERS <- rbind.fill(ALL_STUDY_CLUSTERS, healthCodes.not.clustered)

cluster.summary <- ALL_STUDY_CLUSTERS %>% 
  dplyr::group_by(study) %>%
  dplyr::mutate(studyN = n()) %>%
  dplyr::group_by(study, cluster, studyN)  %>%
  dplyr::summarise(clusterN = n()) %>%
  dplyr::mutate(clusterPercent = round( (clusterN / studyN)*100, digits=2))

### Median prop of users in each cluster
cluster.summary %>% group_by(cluster) %>%
  dplyr::summarise(median = median(clusterPercent),
                   IQR = IQR(clusterPercent),
                   max = max(clusterPercent),
                   min = min(clusterPercent))

p <- ggplot(data = cluster.summary, aes(x=cluster, y=clusterPercent)) + geom_boxplot(width = 0.5)
p <- p + geom_point(size=2, alpha=0.7, aes(color=study))  + scale_color_manual('', values=STUDY_COLS$color) 
p <- p + theme_bw(base_size = 15)  + ylab('Percent') + xlab('Cluster')
p <- p + theme(legend.title = element_blank(),
               legend.position="none",
               legend.background = element_blank(),
               legend.key = element_blank())
p
ggsave("Figs_N_Tables/PropUsers_in_clusters_perStudy.png", plot=p, units="in", height = 6, width = 6, dpi=200)
ggsave("Figs_N_Tables/PropUsers_in_clusters_perStudy.tiff", plot=p, units="in", height = 6, width = 6, dpi=200)




######################
#Table1-A // Comparing engagement in each cluster
######################
engagement.stats <- userStats %>% 
  dplyr::select(healthCode, 
                mean_days_between_app_activity, 
                median_days_between_app_activity,
                SD_days_between_app_activity, 
                duration_in_study, 
                totalDaysActive)
engagement.stats.by.cluster <- merge(ALL_STUDY_CLUSTERS, engagement.stats)

#Add regularity in a 84 day period
engagement.stats.by.cluster <- engagement.stats.by.cluster %>% 
  dplyr::mutate(regularity = round( (duration_in_study / 84)*100, digits=2))

### Comparison of participation by clusters
tmpSummary <- engagement.stats.by.cluster %>% 
  dplyr::group_by(cluster, study) %>%
  dplyr::summarise(medianDurationinStudy = median(duration_in_study),
                   medianDaysActiveinStudy = median(totalDaysActive))  %>%
  tidyr::gather(type, days, c('medianDurationinStudy', 'medianDaysActiveinStudy')) %>%
  dplyr::mutate(type = factor(type, levels=c('medianDurationinStudy', 'medianDaysActiveinStudy')))
p <- ggplot(data=tmpSummary, aes(x=cluster, y=days, fill=type) )
p <- p + theme_bw(base_size = 15) + geom_boxplot(width=0.6, outlier.colour = 'grey70')
p <- p + scale_fill_manual('', values=c('#e6ab02', '#66a61e'), 
                           labels = c('Total duration of app usage', 'Days active tasks performed'))
p <- p + theme(legend.title = element_blank(), legend.position=c(0.75,0.85),
               legend.background = element_blank(), legend.key = element_blank()) + xlab('Cluster') + ylab('Days')
p <- p + geom_hline(yintercept = 84, linetype="dashed") + scale_y_continuous(limits=c(0,85))
p
ggsave("Figs_N_Tables/Userparticipation_across_clusters_boxplot.png", plot=p, units="in", height = 6, width = 6, dpi=200)
ggsave("Figs_N_Tables/Userparticipation_across_clusters_boxplot.tiff", plot=p, units="in", height = 6, width = 6, dpi=200)



######################
#Table1-B // Comparing engagement in each cluster
######################
study.clusters.demog <- merge(ALL_STUDY_CLUSTERS, METADATA)
study.clusters.demog %>% apply(2, function(x) sum(is.na(x)))
study.clusters.demog %>% apply(2, function(x) sum(x == 'NA/Missing'))

study.clusters.demog.prop <- study.clusters.demog %>% 
  dplyr::select(-age, -healthCode, -clinicalReferral, -caseStatus, -state) %>%
  tidyr::gather(demog, demogVal, c('gender', 'age_group', 'race')) %>%
  dplyr::filter(!demogVal == 'NA/Missing') %>%
  dplyr::group_by(study, cluster, demog) %>%
  dplyr::mutate(per_demog_N = n()) %>%
  dplyr::count(study, cluster, demog, demogVal, per_demog_N) %>%
  dplyr::mutate(percent = round( (n/per_demog_N)*100, digits=2)) %>%
  dplyr::select(-per_demog_N, -n) 



###########################
#### generate table 3 
###########################

### Median duration in study across clusters
res.1 <- engagement.stats.by.cluster %>% 
  dplyr::group_by(cluster) %>%
  dplyr::summarise(medianDuration = round(median(duration_in_study), digits=1),
                   medianDays.between.app.activity = round(median(median_days_between_app_activity), digits=1),
                   IQRDays.between.app.activity = round(IQR(median_days_between_app_activity, na.rm = T), digits=1),
                   IQRDuration = round(IQR(duration_in_study), digits=1),
                   mediantotalDaysActive = round(median(totalDaysActive),digits=1),
                   IQRtotalDaysActive = round(IQR(totalDaysActive), digits=1),
                   medianRegularity = round(median(regularity),digits=1),
                   IQRRegularity = round(IQR(regularity), digits=1)) %>%
  dplyr::transmute(cluster  = cluster,
                   duration_in_study =  paste0(medianDuration, ' ± ', IQRDuration),
                   daysActive_in_study =  paste0(mediantotalDaysActive, ' ± ', IQRtotalDaysActive),
                   regularity =  paste0(medianRegularity, ' ± ', IQRRegularity),
                   duration.between.activeDays  =  paste0(medianDays.between.app.activity, ' ± ', IQRDays.between.app.activity))
  
res.1 <- res.1 %>% tidyr::gather(feature, val, -1) %>%
  tidyr::spread(cluster, val)

res.2 <- study.clusters.demog.prop %>% as.data.frame() %>%
  group_by(cluster, demog, demogVal) %>%
  dplyr::summarise(median = round(median(percent), digits=1),
                   IQR = round(IQR(percent, na.rm=T), digits=1)) %>%
  dplyr::mutate(value = paste0(median, ' ± ', IQR)) %>%
  select(feature=demogVal, cluster, value) %>%
  tidyr::spread(cluster, value) %>% as.data.frame()
res.2 <- res.2 %>% select(-demog)
res.2


res.3 <- cluster.summary %>% group_by(cluster) %>%
  dplyr::summarise( N = sum(clusterN),
                   median = round(median(clusterPercent),digits=1),
                   IQR = round(IQR(clusterPercent), digits=1),
                   max = max(clusterPercent),
                   min = min(clusterPercent),
                   proportion = paste0(median, ' ± ', IQR))
res.3 <- res.3 %>% select(cluster, proportion, N) %>% 
  tidyr::gather(feature, val, -cluster) %>%
  tidyr::spread(cluster, val)


table3 <- rbind.fill(res.3, res.1, res.2)
table3
write.table(table3, file="Figs_N_Tables/clusterCharacteristics.tsv", sep="\t",
            row.names = F, col.names = T)


#############################
### Age acorss clusters
#############################
lm.res <- lm(percent ~ cluster * demogVal + study, 
             data=study.clusters.demog.prop %>% 
               filter(demog == 'age_group'))

study.clusters.demog.prop %>%
  #dplyr::filter(demog == 'age_group', demogVal %in% c('(17,29]', '(59,120]')) %>%
  dplyr::group_by(cluster, demogVal) %>%
  dplyr::summarise(medPercent = median(percent))
summary(lm.res)
anova(lm.res)
p <- ggplot(data = study.clusters.demog.prop %>% filter(demog == 'age_group'),
            #demogVal %in% c('(17,29]', '(59,120]')),
            aes(x=cluster, y=percent, fill=demogVal)) + geom_boxplot()
p <- p + theme_bw(base_size = 15)  + ylab('Percent') + xlab('Cluster')
p <- p + theme(legend.position="right",
               legend.background = element_blank(), legend.key = element_blank())
p <- p + scale_fill_manual(name = 'Age group',  
                           values=rev(c('#ffffcc','#c7e9b4','#7fcdbb','#41b6c4','#2c7fb8')),
                           labels=c('17-29', '30-39', '40-49', '50-59', '60+'))
p <- p + geom_hline(yintercept = 10, linetype='dashed', color="gray40") 
p <- p + theme(legend.position = c(.90,.85)) + scale_y_continuous(limits=c(0,100))
p
ggsave("Figs_N_Tables/AgeAcross_clusters_boxplot.png", plot=p, units="in", height = 6, width = 6, dpi=200)
ggsave("Figs_N_Tables/AgeAcross_clusters_boxplot.tiff", plot=p, units="in", height = 6, width = 6, dpi=200)


#############################
### Race acorss clusters
#############################
lm.res <- lm(percent ~ cluster + study, 
             data=study.clusters.demog.prop %>% filter(demog == 'race'))
summary(lm.res)
anova(lm.res)
p <- ggplot(data = study.clusters.demog.prop %>% 
              filter(demog == 'race', demogVal %in% c('Hispanic/Latinos')),
            aes(x=cluster, y=percent)) + geom_boxplot(width=0.5)
p <- p + theme_bw(base_size = 15)  + ylab('Percent') + xlab('Cluster')
p
ggsave("Figs_N_Tables/HispanicLatinos_clusters_boxplot.png", plot=p, units="in", height = 6, width = 6, dpi=200)
ggsave("Figs_N_Tables/HispanicLatinos_clusters_boxplot.tiff", plot=p, units="in", height = 6, width = 6, dpi=200)

table(study.clusters.demog.prop$demogVal)


lm.res <- lm(percent ~ cluster*demogVal  + study, 
             data=study.clusters.demog.prop %>% 
               filter(demog == 'race', demogVal %in% c('Non-Hispanic White', 'Hispanic/Latinos')))
summary(lm.res)
anova(lm.res)
p <- ggplot(data = study.clusters.demog.prop %>% 
              filter(demog == 'race', demogVal %in% c('Hispanic/Latinos', 'African-American/Black', 'Asian')),
            aes(x=cluster, y=percent, fill=demogVal)) + geom_boxplot(width=0.5)
p <- p + theme_bw(base_size = 15)  + ylab('Percent') + xlab('Cluster')
p <- p + theme(legend.position = c(.23,.90), legend.title = element_blank()) 
p <- p + scale_fill_manual(values=c('#66c2a5','#fc8d62','#8da0cb'))
p
ggsave("Figs_N_Tables/NonHispanicWhite_clusters_boxplot.png", plot=p, units="in", height = 6, width = 6, dpi=200)
ggsave("Figs_N_Tables/NonHispanicWhite_clusters_boxplot.tiff", plot=p, units="in", height = 6, width = 6, dpi=200)





#############################
### Gender acorss clusters - No difference
#############################
lm.res <- lm(percent ~ cluster*demogVal  + study, 
             data=study.clusters.demog.prop %>% 
               filter(demog == 'gender', demogVal %in% c('Male', 'Female'),
                      !study %in% c('Phendo')))
summary(lm.res)
anova(lm.res)


#############################
### Missingness across clusters
#############################
study.clusters.demog.prop.with.missingness <- study.clusters.demog %>% 
  dplyr::select(-age, -healthCode, -clinicalReferral, -caseStatus, -state) %>%
  tidyr::gather(demog, demogVal, c('gender', 'age_group', 'race')) %>%
  dplyr::group_by(study, cluster, demog) %>%
  dplyr::mutate(per_demog_N = n()) %>%
  dplyr::count(study, cluster, demog, demogVal, per_demog_N) %>%
  dplyr::mutate(percent = round( (n/per_demog_N)*100, digits=2)) %>%
  dplyr::select(-per_demog_N, -n) %>%
  dplyr::filter(demogVal == 'NA/Missing')

View(study.clusters.demog.prop.with.missingness)
lm.res <- lm( percent ~ cluster + study,
              data = study.clusters.demog.prop.with.missingness %>% 
                dplyr::filter(demog == 'gender'))
summary(lm.res)
anova(lm.res)
