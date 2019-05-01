setwd('/Users/rabram/Desktop/NSS/mnps-pathway-analysis')
library(tidyverse)
library(ggplot2)
library(fuzzyjoin)

#read in data
membership_1718 <- read_csv('./data/membership_school_2017-18.csv')
membership_1213 <- read_csv('./data/membership_school_2012-13.csv')
profile_1718 <-  read_csv('./data/school_profile_2017-18.csv')
clusters <- read_csv('./data/mnps_clusters.csv')

#beginning analysis 
start <- membership_1718 %>% 
  filter(DISTRICT == '190') %>% 
  filter(RACE == 'All Race/Ethnic Groups') %>% 
  filter(GENDER == 'All Genders') %>% 
  filter(GRADE != 'All Grades')

profile_1718 <- profile_1718 %>% 
  filter(DISTRICT_ID == '190')

#change grades to numeric so they can be sorted
start <- start %>% 
  mutate(GRADE = case_when(
    GRADE == 'K' ~ 0,
    GRADE == '1' ~ 1,
    GRADE == '2' ~ 2,
    GRADE == '3' ~ 3,
    GRADE == '4' ~ 4,
    GRADE == '5' ~ 5,
    GRADE == '6' ~ 6,
    GRADE == '7' ~ 7,
    GRADE == '8' ~ 8,
    GRADE == '9' ~ 9,
    GRADE == '10' ~ 10,
    GRADE == '11' ~ 11,
    GRADE == '12' ~ 12,
    
  ))

start <- start %>% 
  arrange(GRADE) %>% 
  arrange(`SCHOOL NAME`)

# add column that is difference in enrollment from grade level to grade level
start <- start %>%
  group_by(`SCHOOL NAME`) %>%
  mutate(change_enrollment = ENROLLMENT - lag(ENROLLMENT, default = ENROLLMENT[1]))

# histogram of enrollment changes
ggplot(start, aes(x=change_enrollment)) +
         geom_histogram(binwidth = 20)

# descriptive statistics for population change
#      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# -142.0000   -6.0000    0.0000   -0.4496    5.0000  120.0000 

grade_level_change <- start %>%
  group_by(GRADE) %>% 
  summarize(
    sum(change_enrollment),
    mean(change_enrollment),
    median(change_enrollment))
  
colnames(grade_level_change) <- c('GRADE', 'change_sum', 'change_mean','change_median')

#total change in enrollment by grade level
grade_level_change %>% 
  ggplot(aes(x = GRADE, y = change_mean)) +
  geom_bar(stat = "identity") +
  scale_x_continuous(breaks=seq(0,12,1))

mnps_info <- membership_1718 %>% 
  filter(DISTRICT == '190')

# combine MNPS data with cluster and pathway information
mnps_merge <- merge(mnps_info, clusters, by.x = 'SCHOOL', by.y = 'SCHOOL_NO') %>% 
  select(-SCHOOL_NAME, -DISTRICT, -'DISTRICT NAME') %>% 
  mutate(GRADE = case_when(
    GRADE == 'All Grades' ~ 13,
    GRADE == 'K' ~ 0,
    GRADE == '1' ~ 1,
    GRADE == '2' ~ 2,
    GRADE == '3' ~ 3,
    GRADE == '4' ~ 4,
    GRADE == '5' ~ 5,
    GRADE == '6' ~ 6,
    GRADE == '7' ~ 7,
    GRADE == '8' ~ 8,
    GRADE == '9' ~ 9,
    GRADE == '10' ~ 10,
    GRADE == '11' ~ 11,
    GRADE == '12' ~ 12,
  )) %>% 
  filter(RACE == 'All Race/Ethnic Groups' | RACE == 'Black or African American' | RACE == 'Hispanic' | RACE == 'White')

general_merge_eda <- mnps_merge %>% 
  filter(RACE != 'All Race/Ethnic Groups') %>% 
  filter(GENDER == 'All Genders') %>% 
  filter(GRADE == 13) %>% 
  #filter(cluster != 'non-zoned') %>%
  arrange(GRADE) %>% 
  arrange(`RACE`) %>% 
  arrange(`SCHOOL NAME`) %>% 
  group_by(`SCHOOL NAME`) 
  #mutate(change_enrollment = ENROLLMENT - lag(ENROLLMENT, default = ENROLLMENT[1]))

test_group <- general_merge_eda %>%
  group_by(`cluster`, RACE) %>% 
  summarize(
    sum(ENROLLMENT),
    mean(ENROLLMENT),
    median(ENROLLMENT))

colnames(test_group) <- c('cluster', 'race', 'sum', 'mean','median')

overall <- general_merge_eda %>% 
  group_by(RACE) %>% 
  summarize(
    sum(ENROLLMENT),
    mean(ENROLLMENT),
    median(ENROLLMENT)) %>% 
  mutate(cluster = 'Overall') 

colnames(overall) <- c('race', 'sum', 'mean','median','cluster')

test_group <- bind_rows(test_group, overall)

test_group <- test_group %>% 
  group_by(cluster) %>% 
  mutate('total' = sum(sum)) %>% 
  ungroup()

# Race of Students Enrolled by Cluster in 2017-2018
test_group %>% 
  ggplot(
    aes(x = reorder(cluster,total,max), y = sum, group=race, fill = race)
  ) +
  geom_bar(
    stat = "identity",
    position = position_fill()
  )  +
  labs(x = 'Cluster', y = ' of Students Enrolled', title = 'Race of Students Enrolled by Cluster in 2017-2018', subtitle = 'Ordered by Cluster Size')  +
  scale_fill_brewer(name = 'Race', palette = 'Set2') + 
  coord_flip() +
  theme(axis.text.y = element_text(face = c('plain', 'plain', 'plain', 'plain', 'plain', 
                                            'plain', 'plain', 'plain', 'plain', 'plain',
                                            'plain', 'plain', 'plain', 'bold'))) 

# OK lets make the same graph BUT for enrollment by cluster in the earliest year I have data for

mnps_info_12 <- membership_1213 %>% 
  filter(district_id == '190')

# combine MNPS data with cluster and pathway information
mnps_merge_12 <- merge(mnps_info_12, clusters, by.x = 'school_id', by.y = 'SCHOOL_NO') %>% 
  select(-SCHOOL_NAME, -district_id, -district_name) %>% 
  mutate(Grade = case_when(
    Grade == 'All Grades' ~ 13,
    Grade == 'PK' ~ -1,
    Grade == 'KG' ~ 0,
    Grade == '1' ~ 1,
    Grade == '2' ~ 2,
    Grade == '3' ~ 3,
    Grade == '4' ~ 4,
    Grade == '5' ~ 5,
    Grade == '6' ~ 6,
    Grade == '7' ~ 7,
    Grade == '8' ~ 8,
    Grade == '9' ~ 9,
    Grade == '10' ~ 10,
    Grade == '11' ~ 11,
    Grade == '12' ~ 12,
  )) %>%  
  filter(Race_or_Ethnicity == 'All Race/Ethnic Groups' | Race_or_Ethnicity == 'Black or African American' | Race_or_Ethnicity == 'Hispanic/Latino' | Race_or_Ethnicity == 'White')

general_merge_eda_12 <- mnps_merge_12 %>% 
  filter(Grade == 13) %>% 
  filter(Race_or_Ethnicity != 'All Race/Ethnic Groups') %>% 
  group_by(schoolname, Race_or_Ethnicity) %>% 
  mutate('race_enrollment' = sum(enrollment)) %>% 
  ungroup() %>% 
  filter(Gender == 'F') %>% 
  select(-Gender, -enrollment)

test_group_12 <- general_merge_eda_12 %>%
  group_by(`cluster`, Race_or_Ethnicity) %>% 
  summarize(
    sum(race_enrollment),
    mean(race_enrollment),
    median(race_enrollment))

colnames(test_group_12) <- c('cluster', 'race', 'sum', 'mean','median')

overall_12 <- general_merge_eda_12 %>% 
  group_by(Race_or_Ethnicity) %>% 
  summarize(
    sum(race_enrollment),
    mean(race_enrollment),
    median(race_enrollment)) %>% 
  mutate(cluster = 'Overall') 

colnames(overall_12) <- c('race', 'sum', 'mean','median','cluster')

test_group_12 <- bind_rows(test_group_12,overall_12)

test_group_12 <- test_group_12 %>% 
  group_by(cluster) %>% 
  mutate('total' = sum(sum)) %>% 
  ungroup()

# Race of Students Enrolled by Cluster in 2017-2018
test_group_12 %>% 
  ggplot(
    aes(x = reorder(cluster,total,max), y = sum, group=race, fill = race)
  ) +
  geom_bar(
    stat = "identity",
    position = position_fill()
  )  +
  labs(x = 'Cluster', y = ' of Students Enrolled', title = 'Race of Students Enrolled by Cluster in 2012-2013', subtitle = 'Ordered by Cluster Size')  +
  scale_fill_brewer(name = 'Race', palette = 'Set1') + 
  coord_flip() +
  theme(axis.text.y = element_text(face = c('plain', 'plain', 'plain', 'plain', 'plain', 
                                            'plain', 'plain', 'plain', 'plain', 'plain',
                                            'plain', 'plain', 'plain', 'bold'))) 

## compare sizes from 2012-2013 to 2017-2018 (over 5 years)

colnames(test_group_12) <- c('cluster', 'race', 'sum_2012', 'mean_2012','median_2012','total_2012')
colnames(test_group) <- c('cluster', 'race', 'sum_2017', 'mean_2017','median_2017','total_2017')

test_group_12$race <- mapply(gsub, pattern = "Hispanic/Latino", replacement = 'Hispanic', test_group_12$race)

test_group_change <- merge(test_group_12, test_group)

test_group_change <- test_group_change %>% 
  mutate('change_total' = total_2017 - total_2012) %>% 
  mutate('change_sum' = sum_2017 - sum_2012) %>% 
  mutate('change_mean' = mean_2017 - mean_2012) %>% 
  mutate('change_median' = median_2017 - median_2012) 

  
# Race of Students Enrolled by Cluster in 2017-2018
test_group_change %>% 
  filter(cluster != 'Overall') %>% 
  ggplot(
    aes(x = reorder(cluster,change_total,max), y = change_sum, group=race, fill = race)
  ) +
  geom_bar(
    stat = "identity",
    position = position_dodge()
  )  +
  labs(x = 'Cluster', y = '# of Students Enrolled', title = 'Change in Subgroups from 2012 to 2017')  +
  scale_fill_brewer(name = 'Race', palette = 'Set1') + 
  coord_flip() +
  theme(axis.text.y = element_text(face = c('plain', 'plain', 'plain', 'plain', 'plain', 
                                            'plain', 'plain', 'plain', 'plain', 'plain',
                                            'plain', 'plain', 'plain', 'bold'))) 

membership_1617 <- read_csv('./data/membership_school_2016-17.csv')
colnames(membership_1617) <- c('district_id', 'district','school_id','school_name','grade','race','gender','enrollment')
colnames(membership_1718) <- c('district_id', 'district','school_id','school_name','grade','race','gender','enrollment')

mnps_1617 <- membership_1617 %>% 
  filter(district_id == 190 & race == 'All Race/Ethnic Groups' & gender == 'All Genders' & grade != 'All Grades') %>% 
  select(school_id, school_name, grade, enrollment) %>% 
  mutate(year = 2017) %>% 
  mutate(grade = case_when(
    grade == 'PK' ~ -1,
    grade == 'KG' ~ 0,
    grade == '1' ~ 1,
    grade == '2' ~ 2,
    grade == '3' ~ 3,
    grade == '4' ~ 4,
    grade == '5' ~ 5,
    grade == '6' ~ 6,
    grade == '7' ~ 7,
    grade == '8' ~ 8,
    grade == '9' ~ 9,
    grade == '10' ~ 10,
    grade == '11' ~ 11,
    grade == '12' ~ 12,
  )) %>% 
  mutate(next_grade = grade + 1)

mnps_1718 <- membership_1718 %>% 
  filter(district_id == 190 & race == 'All Race/Ethnic Groups' & gender == 'All Genders' & grade != 'All Grades') %>% 
  select(school_id, school_name, grade, enrollment) %>% 
  mutate(year = 2018) %>% 
  mutate(grade = case_when(
    grade == 'PK' ~ -1,
    grade == 'K' ~ 0,
    grade == '1' ~ 1,
    grade == '2' ~ 2,
    grade == '3' ~ 3,
    grade == '4' ~ 4,
    grade == '5' ~ 5,
    grade == '6' ~ 6,
    grade == '7' ~ 7,
    grade == '8' ~ 8,
    grade == '9' ~ 9,
    grade == '10' ~ 10,
    grade == '11' ~ 11,
    grade == '12' ~ 12,
  )) %>% 
  mutate(next_grade = grade)

mnps_both_years <- merge(mnps_1617, mnps_1718, by=c('school_id', 'next_grade')) %>% 
  select(school_id, school_name.x, grade.x, enrollment.x, grade.y, enrollment.y)

colnames(mnps_both_years) <- c('school_id','school_name','grade_1617','enrollment_1617','grade_1718','enrollment_1718')

#drop pre-k (optional) and add a column for value changes

mnps_both_years <- mnps_both_years %>% 
  mutate(change = enrollment_1718 - enrollment_1617) %>% 
  filter(grade_1617 != -1)

only_clusters <- clusters %>% 
  select(SCHOOL_NO, cluster)

only_clusters$SCHOOL_NO <- as.numeric(only_clusters$SCHOOL_NO)

mnps_both_years_cluster <- left_join(mnps_both_years, only_clusters, by = c("school_id" = "SCHOOL_NO"))

zoned_mnps <- mnps_both_years_cluster %>% 
  filter(cluster != 'Non-Zoned' | cluster != NaN)

non_zoned_mnps <- mnps_both_years_cluster %>% 
  filter(cluster == 'Non-Zoned' | cluster == NaN)

mnps_both_years_cluster$cluster[is.na(mnps_both_years_cluster$cluster)] <- 'Non-Zoned'

grade_levels <- mnps_both_years_cluster %>% 
  filter(cluster != 'Non-Zoned') %>% 
  group_by(grade_1617) %>% 
  summarise(
    sum(change),
    mean(change),
    median(change)
  )

## need to fix 4th and 8th grade to account for pathways
## oy

  
  
