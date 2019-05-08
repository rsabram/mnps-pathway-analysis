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

#write_csv(scored_pathways, 'scored_pathways.csv')

## In these files, suppression also occurs where any individual proficiency level 
## is less than 5% or greater than 95% at the school level 
## (this is denoted by two asterisks**).

scored_pathways <- read_csv('./data/scored_pathways.csv')

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

reshape_medians_ela <- gather(averages, outcome, value, median_ela_elem:median_ela_hs) %>% 
  select(cluster, outcome, value) %>% 
  filter(cluster != 'Stratford') %>% 
  mutate(outcome = factor(outcome, levels = c("median_ela_elem", "median_ela_ms","median_ela_hs"))) %>% 
  mutate('Subject' = 'ELA') %>% 
  mutate('GraphType' = 1)

reshape_medians_math <- gather(averages, outcome, value, median_math_elem:median_math_hs) %>% 
  select(cluster, outcome, value) %>% 
  filter(cluster != 'Stratford') %>% 
  mutate(outcome = factor(outcome, levels = c("median_math_elem", "median_math_ms","median_math_hs"))) %>% 
  mutate('Subject' = 'Math') %>% 
  mutate('GraphType' = 1)


reshape_change_ela <- gather(averages, outcome, value, median_ela_elem_to_ms:median_ela_ms_to_hs) %>% 
  select(cluster, outcome, value) %>% 
  filter(cluster != 'Stratford') %>% 
  mutate(outcome = factor(outcome, levels = c("median_ela_elem_to_ms", "median_ela_ms_to_hs"))) %>% 
  mutate('Subject' = 'ELA') %>% 
  mutate('GraphType' = 2)

reshape_change_math <- gather(averages, outcome, value, median_math_elem_to_ms:median_math_ms_to_hs) %>% 
  select(cluster, outcome, value) %>% 
  filter(cluster != 'Stratford') %>% 
  mutate(outcome = factor(outcome, levels = c("median_math_elem_to_ms", "median_math_ms_to_hs"))) %>% 
  mutate('Subject' = 'Math') %>% 
  mutate('GraphType' = 2)

reshaped <- rbind(reshape_medians_ela, reshape_medians_math, reshape_change_ela, reshape_change_math)

write_rds(reshaped, "./cluster_graphs.RDS")

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

### pivot idk

limited_tcap <- tcap_2018 %>% 
  filter(test == 'TNReady' | test == 'EOC') %>%
  filter(subject == 'Math' | subject == 'ELA') %>% 
  filter(grade == 'All Grades') %>% 
  filter(subgroup == 'All Students') %>% 
  select(year, system, system_name, school, school_name, test, subject, grade, subgroup, pct_on_mastered)

limited_tcap$pct_on_mastered <- as.numeric(limited_tcap$pct_on_mastered)

limited_tcap$subject <- mapply(gsub, pattern = "English I", replacement = 'ELA', limited_tcap$subject)
limited_tcap$subject <- mapply(gsub, pattern = "Integrated Math I", replacement = 'Math', limited_tcap$subject)

limited_type <- limited_tcap %>% 
  mutate(type = case_when(
    grepl("Elementary", school_name) ~ "Elementary",
    grepl("Middle", school_name) ~ "Middle",
    grepl("High", school_name) ~ "High",
  )) %>% 
  select(-test, -grade, -subgroup, -year) %>% 
  drop_na() %>% 
  filter(type != 'High')

summ <- limited_type %>% 
  group_by(system_name,subject,type) %>% 
  summarise(
    mean(pct_on_mastered, na.rm = TRUE),
    median(pct_on_mastered, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  drop_na()

colnames(summ) <- c('district','subject','type','mean_mastery','median_mastery')

df <- summ %>% 
  mutate(change_me = mean_mastery - lag(mean_mastery, default = mean_mastery[1])) %>% 
  mutate(change_med = median_mastery - lag(median_mastery, default = median_mastery[1])) %>% 
  filter(type != 'Elementary')


mnps_tcap_2018 <- tcap_2018 %>% 
  filter(system == 190)

mnps_tcap_eda <- mnps_tcap_2018 %>% 
  select(school, school_name, test, subject, grade, subgroup, pct_on_mastered) %>% 
  filter(test == 'TNReady' | test == 'EOC')

mnps_tcap_eda$pct_on_mastered <- mapply(gsub, pattern = '\\*{2}', replacement = 2.5, mnps_tcap_eda$pct_on_mastered)
mnps_tcap_eda$pct_on_mastered <- as.numeric(mnps_tcap_eda$pct_on_mastered) 

mnps_tcap_eda <- mnps_tcap_eda %>% 
  drop_na() %>% 
  filter(subgroup == 'All Students' | 
         subgroup == 'Asian' |
           subgroup == 'Black or African American' |
           subgroup == 'Economically Disadvantaged' |
           subgroup == 'Hispanic' |
           subgroup == 'Students with Disabilities' |
           subgroup == 'White'
           ) %>% 
  filter(subject == 'ELA' |
           subject == 'Biology I' |
           subject == 'English I' |
           subject == 'Integrated Math I' |
           subject == 'Math' |
           subject == 'Science') %>% 
  select(-test)

mnps_tcap_eda %>% 
  arrange(desc(pct_on_mastered)) %>% 
  select(school_name[1])

first(h(x, 1))
nth(mnps_tcap_eda$school_name, 2)

mnps_tcap %>% 
  arrange(desc(pct_on_mastered)) %>% 
  select(nth(mnps_tcap_eda$school_name, 3))

locations <- read_csv('./data/mnps_locations.csv')

test <- left_join(mnps_tcap_eda, locations, by = c('school' = "School State ID")) %>% 
  filter(school != 260) %>% 
  select(school, school_name, subject, grade, subgroup, pct_on_mastered, 'School Level','Lowest Grade','Highest Grade', 'Cluster','Street Address','City','State','ZIP Code','Phone Number','Principal Full Name','Principal Email','School Website')

colnames(test) <- c('School ID','School Name','Subject','Grade Level','Subgroup','Percent Mastered', 'School Level','Lowest Grade','Highest Grade', 'Cluster','Address','City','State','ZIP','Phone Number','Principal','Principal Email','Website')

test$Cluster[is.na(test$Cluster)] <- 'Non-Zoned'

#saveRDS(test, 'mnps_tcap.RDS')

table(mnps_tcap_eda$subject)
table(mnps_tcap_eda$subgroup)

all_2018 <- read_csv('./data/all_2018.csv')

all_2018$`School ID` <- as.numeric(all_2018$`School ID`)

all <- left_join(all_2018, test, by = c('School ID' = 'School ID')) %>% 
  select(-`Grades Served`, -`School Name.y`)

all_ms <- all %>% 
  filter(Subgroup == "All Students") %>% 
  filter(`Grade Level` == 'All Grades') %>% 
  filter(Subject != "Science" & Subject != "Biology I") %>% 
  filter(`School Level` == 'Middle School' & (Subject == 'Math' | Subject == 'ELA'))

all_high <- all %>% 
  filter(Subgroup == "All Students") %>% 
  filter(`Grade Level` == 'All Grades') %>% 
  filter(Subject != "Science" & Subject != "Biology I") %>% 
  filter(`School Level` == 'High School')

all_elem <- all %>% 
  filter(Subgroup == "All Students") %>% 
  filter(`Grade Level` == 'All Grades') %>% 
  filter(Subject != "Science" & Subject != "Biology I") %>% 
  filter(`School Level` == 'Elementary School')

all_other <- all %>% 
  filter(Subgroup == "All Students") %>% 
  filter(`Grade Level` == 'All Grades') %>% 
  filter(Subject != "Science" & Subject != "Biology I") %>% 
  filter(`School Level` == 'Non-Traditional' | `School Level` == 'Charter' )

all_other <- all_other %>% 
  filter(!(Subject == 'Integrated Math I' & `Highest Grade` == 'Grade 8')) %>% 
  filter(!(Subject == 'English I' & `Highest Grade` == 'Grade 9')) %>% 
  filter(!(Subject == 'Integrated Math I' & `Highest Grade` == 'Grade 9')) %>% 
  filter(!(Subject == 'Integrated Math I' & `School ID` == 3)) %>% 
  filter(!(Subject == 'English I' & `School ID` == 3))

all_mnps <- all %>% 
  filter(`School Name.x` == 'All MNPS')



table(all_other$`School Name.x`)

all_high$Subject <- mapply(gsub, pattern = 'Integrated Math I', replacement = 'Math', all_high$Subject)
all_high$Subject <- mapply(gsub, pattern = 'English I', replacement = 'ELA', all_high$Subject)



all_scored <- rbind(all_ms, all_high, all_elem, all_other, all_mnps)

all_scored$Subject <- mapply(gsub, pattern = 'Integrated Math I', replacement = 'Math', all_scored$Subject)
all_scored$Subject <- mapply(gsub, pattern = 'English I', replacement = 'ELA', all_scored$Subject)


read_in <- read_csv('./data/all_scored.csv')
write_rds(read_in, 'school_data_compare.RDS')


#fix wonky charter / non traditional ones

