setwd('/Users/rabram/Desktop/NSS/mnps-pathway-analysis')
library(tidyverse)
library(ggplot2)
library(fuzzyjoin)

membership_1718 <- read_csv('./data/membership_school_2017-18.csv')
profile_1718 <-  read_csv('./data/school_profile_2017-18.csv')
clusters <- read_csv('./data/mnps_clusters.csv')

start <- membership_1718 %>% 
  filter(DISTRICT == '190') %>% 
  filter(RACE == 'All Race/Ethnic Groups') %>% 
  filter(GENDER == 'All Genders') %>% 
  filter(GRADE != 'All Grades')

profile_1718 <- profile_1718 %>% 
  filter(DISTRICT_ID == '190')

#start$GRADE <- ifelse(start$GRADE == 'K',0,start$GRADE) %>% 
#  as.numeric()

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

start <- start %>%
  group_by(`SCHOOL NAME`) %>%
  mutate(change_enrollment = ENROLLMENT - lag(ENROLLMENT, default = ENROLLMENT[1]))

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
  filter(GRADE != 13) %>% 
  filter(cluster != 'non-zoned') %>%
  arrange(GRADE) %>% 
  arrange(`RACE`) %>% 
  arrange(`SCHOOL NAME`) %>% 
  group_by(`SCHOOL NAME`) %>% 
  mutate(change_enrollment = ENROLLMENT - lag(ENROLLMENT, default = ENROLLMENT[1]))

test_group <- general_merge_eda %>%
  group_by(`RACE`) %>% 
  summarize(
    sum(change_enrollment),
    mean(change_enrollment),
    median(change_enrollment))

colnames(cluster_level_size) <- c('cluster', 'sum', 'mean','median')

cluster_level_size %>% 
  ggplot(aes(x = cluster, y = sum)) +
  geom_bar(stat = "identity") +
  coord_flip()


