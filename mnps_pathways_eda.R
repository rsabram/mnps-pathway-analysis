setwd('/Users/rabram/Desktop/NSS/mnps-pathway-analysis')
library(tidyverse)
library(ggplot2)
library(fuzzyjoin)

#read in data
membership_1617 <- read_csv('./data/membership_school_2017-18.csv') 
membership_1213 <- read_csv('./data/membership_school_2012-13.csv')
profile_1718 <-  read_csv('./data/school_profile_2017-18.csv')
clusters <- read_csv('./data/mnps_clusters.csv')
tvaas_2018 <- read_csv('./data/TVAAS_2018.csv')
tcap_2018 <- read_csv('./data/assessment_2018.csv')

colnames(membership_1718) <- c('district_id','district_name','school_id','school_name','grade','race','gender','enrollment')
colnames(membership_1213) <- c('district_id','district_name','school_id','school_name','grade','race','gender','enrollment')

mnps_membership_1718 <- membership_1718 %>% 
  filter(district_id == 190)

mnps_membership_1213 <- membership_1213 %>% 
  filter(district_id == 190)

# crate df of pathways
# drop schools with unknown SIDs and those that feed to two middle schools
# Ida B. Wells Elementary, Smith Springs Elementary, Neely's Bend Middle, Eagle View Elementary

pathways <- clusters %>% 
  filter(Type == 'Elementary') %>% 
  filter(SCHOOL_NO != 'UNKNOWN') %>% 
  filter(ms_feeder_code != 'UNKNOWN') %>% 
  filter(SCHOOL_NAME != 'Smith Springs Elementary School') 

pathways$SCHOOL_NO <- as.numeric(pathways$SCHOOL_NO)
pathways$ms_feeder_code <- as.numeric(pathways$ms_feeder_code)
pathways$hs_feeder_code <- as.numeric(pathways$hs_feeder_code)

# 62 remaining pathways / pipelines of schools

# clean assessment data

table(mnps_tcap_2018$subject)

mnps_tcap_2018 <- tcap_2018 %>% 
  filter(system == 190)
  
limited_ms_tcap <- mnps_tcap_2018 %>% 
  filter(test == 'TNReady' | test == 'EOC') %>%
  filter(subject == 'Math' | subject == 'ELA') %>% 
  filter(grade == 'All Grades') %>% 
  filter(subgroup == 'All Students') %>% 
  select(school, school_name, subject, pct_on_mastered)

limited_ms_tcap <- spread(limited_ms_tcap, subject, pct_on_mastered)

merge1 <- left_join(pathways, limited_ms_tcap, by = c('SCHOOL_NO' = 'school')) %>% 
  select(-school_name) %>%
  rename(
    ela_elem = ELA,
    math_elem = Math
  )

merge2 <- left_join(merge1, limited_ms_tcap, by = c("ms_feeder_code" = 'school' )) %>% 
  select(-school_name) %>%
  rename(
    ela_ms = ELA,
    math_ms = Math
  )

# fix HS data because multiple grade levels take the test

limited_hs_tcap <- mnps_tcap_2018 %>% 
  filter(test == 'EOC') %>%
  filter(subgroup == 'All Students') %>% 
  filter(grade == 'All Grades') %>% 
  filter(subject == 'Integrated Math I' | subject == 'English I') %>% 
  select(school, school_name, subject, pct_on_mastered)

limited_hs_tcap <- spread(limited_hs_tcap, subject, pct_on_mastered)

scored_pathways <- left_join(merge2, limited_hs_tcap, by = c("hs_feeder_code" = 'school' )) %>% 
  select(-school_name) %>%
  rename(
   ela_hs = 'English I',
    math_hs = 'Integrated Math I'
  )

scored_pathways <- scored_pathways %>% 
  select('SCHOOL_NO', 'SCHOOL_NAME', 'ela_elem','math_elem','ms_feeder_code', 'ms_feeder_name','ela_ms', 'math_ms', 'hs_feeder_code','hs_feeder_name','ela_hs','math_hs','cluster')

## In these files, suppression also occurs where any individual proficiency level 
## is less than 5% or greater than 95% at the school level 
## (this is denoted by two asterisks**).

scored_pathways$ela_elem <- mapply(gsub, pattern = '\\*{2}', replacement = 5, scored_pathways$ela_elem)
scored_pathways$ela_ms <- mapply(gsub, pattern = '\\*{2}', replacement = 5, scored_pathways$ela_ms)
scored_pathways$ela_hs <- mapply(gsub, pattern = '\\*{2}', replacement = 5, scored_pathways$ela_hs)
scored_pathways$math_hs <- mapply(gsub, pattern = '\\*{2}', replacement = 5, scored_pathways$math_hs)
scored_pathways$math_ms <- mapply(gsub, pattern = '\\*{2}', replacement = 5, scored_pathways$math_ms)
scored_pathways$math_elem <- mapply(gsub, pattern = '\\*{2}', replacement = 5, scored_pathways$math_elem)

scored_pathways$ela_elem <- as.numeric(scored_pathways$ela_elem)
scored_pathways$ela_ms <- as.numeric(scored_pathways$ela_ms)
scored_pathways$ela_hs <- as.numeric(scored_pathways$ela_hs)
scored_pathways$math_elem <- as.numeric(scored_pathways$math_elem)
scored_pathways$math_ms <- as.numeric(scored_pathways$math_ms)
scored_pathways$math_hs <- as.numeric(scored_pathways$math_hs)

scored_pathways <- scored_pathways %>% 
  mutate('ela_elem_to_ms' = ela_ms - ela_elem) %>% 
  mutate('ela_ms_to_hs' = ela_hs - ela_ms) %>% 
  mutate('math_elem_to_ms' = math_ms - math_elem) %>% 
  mutate('math_ms_to_hs' = math_hs - math_ms) 

boxes <- scored_pathways %>% 
  select(ela_elem, math_elem, ela_ms, math_ms, ela_hs, math_hs)

boxplot(boxes, horizontal = TRUE, main = "Comparing Score Distribution",
        las = 1,
        col = c("blue","lightblue"),
        border = "black",
        notch = TRUE, 
        par(mar = c(4, 6, 4, 2)+ 0.1), 
        names = c('Elem. ELA','Elem. Math', 'Middle ELA', 'Middle Math','HS ELA','HS Math'),
        xlab = ('% of Students On Track or Mastered')
)

ggplot(data = boxes, aes(x = "", y = boxes)) + 
  geom_boxplot() 

ela_ms

# analyze
averages <- scored_pathways %>% 
  group_by(cluster) %>% 
  summarise(
    mean(ela_elem),
    mean(ela_ms),
    mean(ela_hs),
    mean(ela_elem_to_ms),
    mean(ela_ms_to_hs),
    mean(math_elem),
    mean(math_ms),
    mean(math_hs),
    mean(math_elem_to_ms),
    mean(math_ms_to_hs),
    median(ela_elem),
    median(ela_ms),
    median(ela_hs),
    median(ela_elem_to_ms),
    median(ela_ms_to_hs),
    median(math_elem),
    median(math_ms),
    median(math_hs),
    median(math_elem_to_ms),
    median(math_ms_to_hs),
  )

colnames(averages) <- c('cluster',
  'mean_ela_elem',
  'mean_ela_ms',
  'mean_ela_hs',
  'mean_ela_elem_to_ms',
  'mean_ela_ms_to_hs',
  'mean_math_elem',
  'mean_math_ms',
  'mean_math_hs',
  'mean_math_elem_to_ms',
  'mean_math_ms_to_hs',
  'median_ela_elem',
  'median_ela_ms',
  'median_ela_hs',
  'median_ela_elem_to_ms',
  'median_ela_ms_to_hs',
  'median_math_elem',
  'median_math_ms',
  'median_math_hs',
  'median_math_elem_to_ms',
  'median_math_ms_to_hs'
) 

reshape_medians <- gather(averages, outcome, value, median_ela_elem:median_ela_hs) %>% 
  select(cluster, outcome, value) %>% 
  filter(cluster != 'Stratford') %>% 
  mutate(outcome = factor(outcome, levels = c("median_ela_elem", "median_ela_ms","median_ela_hs")))

reshape_medians_math <- gather(averages, outcome, value, median_math_elem:median_math_hs) %>% 
  select(cluster, outcome, value) %>% 
  filter(cluster != 'Stratford') %>% 
  mutate(outcome = factor(outcome, levels = c("median_math_elem", "median_math_ms","median_math_hs")))


reshape_change_ela <- gather(averages, outcome, value, median_ela_elem_to_ms:median_ela_ms_to_hs) %>% 
  select(cluster, outcome, value) %>% 
  filter(cluster != 'Stratford') %>% 
  mutate(outcome = factor(outcome, levels = c("median_ela_elem_to_ms", "median_ela_ms_to_hs")))

reshape_change_math <- gather(averages, outcome, value, median_math_elem_to_ms:median_math_ms_to_hs) %>% 
  select(cluster, outcome, value) %>% 
  filter(cluster != 'Stratford') %>% 
  mutate(outcome = factor(outcome, levels = c("median_math_elem_to_ms", "median_math_ms_to_hs")))


reshape_medians %>% 
  ggplot(
  aes(x = cluster, y = value, group = outcome, fill = outcome)
) +
  geom_bar(
    stat = "identity",
    position = position_dodge()
  ) +
  labs(x = 'Cluster', y = '% of Students on Track or Mastered', title = 'Median ELA Student Mastery by Cluster') +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  scale_fill_brewer(name = 'School Type', palette = "Set1", labels = c("Elementary School", "Middle School", "High School")) +
  ylim(0, 100)

reshape_medians_math %>% 
  ggplot(
    aes(x = cluster, y = value, group = outcome, fill = outcome)
  ) +
  geom_bar(
    stat = "identity",
    position = position_dodge()
  ) +
  labs(x = 'Cluster', y = '% of Students on Track or Mastered', title = 'Median Math Student Mastery by Cluster') +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  scale_fill_brewer(name = 'School Type', palette = "Set2", labels = c("Elementary School", "Middle School", "High School")) +
  ylim(0, 100)


reshape_change_ela %>% 
  ggplot(
    aes(x = cluster, y = value, group = outcome, fill = outcome)
  ) +
  geom_bar(
    stat = "identity",
    position = position_dodge()
  ) +
  labs(x = 'Cluster', y = '% Change in ELA On Track or Masterd', title = 'Median ELA Student Mastery Change by Cluster') +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  scale_fill_brewer(name = 'Change Type', palette = "Set1", labels = c("Elementary to Middle", "Middle to High"))


reshape_change_math %>% 
  ggplot(
    aes(x = cluster, y = value, group = outcome, fill = outcome)
  ) +
  geom_bar(
    stat = "identity",
    position = position_dodge()
  ) +
  labs(x = 'Cluster', y = '% Change in Math On Track or Masterd', title = 'Median Math Student Mastery Change by Cluster') +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  scale_fill_brewer(name = 'Change Type', palette = "Set2", labels = c("Elementary to Middle", "Middle to High"))
