setwd('/Users/rabram/Desktop/NSS/mnps-pathway-analysis')
library(tidyverse)
library(ggplot2)

#read in data
membership_1718 <- read_csv('./data/membership_school_2017-18.csv')
membership_1213 <- read_csv('./data/membership_school_2012-13.csv')
profile_1718 <-  read_csv('./data/school_profile_2017-18.csv')
clusters <- read_csv('./data/mnps_clusters.csv')

clusters$SCHOOL_NO <- as.numeric(clusters$SCHOOL_NO)

# gender
mnps <- membership_1718 %>% 
  filter(DISTRICT == 190) %>% 
  filter(GRADE == 'All Grades') %>% 
  filter(GENDER != 'All Genders') %>% 
  filter(RACE == 'All Race/Ethnic Groups') %>% 
  select(SCHOOL, `SCHOOL NAME`, GENDER, ENROLLMENT)

foo <- spread(mnps, GENDER, ENROLLMENT)

fa <- left_join(foo, clusters, by = c(SCHOOL = "SCHOOL_NO")) %>% 
  select(SCHOOL, SCHOOL_NAME, F, M, Type, cluster)

write_csv(fa, 'gender_breakdown.csv')

write_csv(mnps, 'mnps.csv')

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

mnps_both_years_cluster <- left_join(mnps_both_years, only_clusters, by = c("school_id" = "SCHOOL_NO"))

zoned_mnps <- mnps_both_years_cluster %>% 
  filter(cluster != 'Non-Zoned' | cluster != NaN)

non_zoned_mnps <- mnps_both_years_cluster %>% 
  filter(cluster == 'Non-Zoned' | cluster == NaN)

mnps_both_years_cluster$cluster[is.na(mnps_both_years_cluster$cluster)] <- 'Non-Zoned'

elem_paths <- clusters %>% 
  filter(Type == 'Elementary') %>% 
  filter(hs_feeder_name != 'Hunters Lane High' & hs_feeder_name != 'Cane Ridge High School' & hs_feeder_name != 'Maplewood High') %>% 
  select(-Type, -cluster)

elem_paths$SCHOOL_NO <- as.numeric(elem_paths$SCHOOL_NO)
elem_paths$ms_feeder_code <- as.numeric(elem_paths$ms_feeder_code)
elem_paths$hs_feeder_code <- as.numeric(elem_paths$hs_feeder_code)

mnps_4 <- mnps_1617 %>% 
  filter(grade == 4) %>% 
  select(school_id, grade, enrollment)

mnps_5 <- mnps_1718 %>% 
  filter(grade == 5) %>% 
  select(school_id, grade, enrollment)

mnps_8 <- mnps_1617 %>% 
  filter(grade == 8) %>% 
  select(school_id, grade, enrollment)

mnps_9 <- mnps_1718 %>% 
  filter(grade == 9) %>% 
  select(school_id, grade, enrollment)

test <- left_join(elem_paths, mnps_4, by = c("SCHOOL_NO" = "school_id")) %>% 
  filter(SCHOOL_NO != 8095) %>% 
  select(SCHOOL_NO, SCHOOL_NAME, grade, enrollment, ms_feeder_code, ms_feeder_name, hs_feeder_code, hs_feeder_name)

grouped <- test %>% 
  group_by(ms_feeder_code) %>% 
  mutate(expected_5th = sum(enrollment)) %>% 
  ungroup() %>% 
  select(SCHOOL_NO, SCHOOL_NAME, grade, enrollment, expected_5th, ms_feeder_code, ms_feeder_name, hs_feeder_code, hs_feeder_name)

df <- left_join(grouped, mnps_5, by = c("ms_feeder_code" = "school_id")) %>% 
  select(SCHOOL_NO, SCHOOL_NAME, grade.x, enrollment.x, expected_5th, enrollment.y, ms_feeder_code, ms_feeder_name, hs_feeder_code, hs_feeder_name)

colnames(df) <- c('school_id', 'name', 'grade','4th_grade_enrollment','expected_5th_enrollment','actual_5th_enrollment', 'ms_feeder_code', 'ms_feeder_name','hs_feeder_code','hs_feeder_name')

change_4_to_5 <- df %>% 
  select(ms_feeder_code, ms_feeder_name, expected_5th_enrollment, actual_5th_enrollment) %>% 
  mutate('difference' = actual_5th_enrollment - expected_5th_enrollment) %>% 
  unique()

## 8 to 9

test <- left_join(elem_paths, mnps_8, by = c("ms_feeder_code" = "school_id")) %>% 
  select(ms_feeder_code, ms_feeder_name, grade, enrollment, hs_feeder_code, hs_feeder_name) %>% 
  unique()

grouped <- test %>% 
  group_by(hs_feeder_code) %>% 
  mutate(expected_9th = sum(enrollment)) %>% 
  ungroup() %>% 
  select(ms_feeder_code, ms_feeder_name, grade, enrollment, expected_9th, hs_feeder_code, hs_feeder_name)

df <- left_join(grouped, mnps_9, by = c("hs_feeder_code" = "school_id")) %>% 
  select(ms_feeder_code, ms_feeder_name, grade.x, enrollment.x, grade.y, expected_9th, enrollment.y, hs_feeder_code, hs_feeder_name) %>% 
  select(-grade.y, -grade.x)

colnames(df) <- c('ms_feeder_code', 'ms_feeder_name','8th_grade_enrollment','expected_9th_enrollment','actual_9th_enrollment', 'hs_feeder_code', 'hs_feeder_name')

change_8_to_9 <- df %>% 
  select(hs_feeder_code, hs_feeder_name, expected_9th_enrollment, actual_9th_enrollment) %>% 
  mutate('difference' = actual_9th_enrollment - expected_9th_enrollment) %>% 
  unique()

# grade_levels <- mnps_both_years_cluster %>% 
#   filter(cluster != 'Non-Zoned') %>% 
#   group_by(grade_1617) %>% 
#   summarise(
#     sum(change),
#     mean(change),
#     median(change)
#   )

## need to fix 4th and 8th grade to account for pathways
## oy

## Over Time-

# #colClasses = c("character", "character", "complex", 
# "factor", "factor", "character", "integer", 
# "integer", "numeric", "character", "character",
# "Date", "integer", "logical")

# size_2010 <- read_csv('./data/data_2010.csv')  %>% 
#   filter(district == 190)
# size_2011 <- read_csv('./data/data_2011.csv')%>% 
#   filter(district == 190)
# size_2012 <- read_csv('./data/data_2012.csv')%>% 
#   filter(district == 190)
# size_2013 <- read_csv('./data/data_2013.csv') %>%  
#   filter(DISTRICT == '00190')
# size_2014 <- read_csv('./data/data_2014.csv')%>% 
#   filter(district == 190) %>% 
#   filter(SCHOOL_NAME != 'All Schools')
# size_2015 <- read_csv('./data/data_2015.csv')%>% 
#   filter(DISTRICT == 190) %>% 
#   filter(SCHOOL_NAME != 'All Schools')
# size_2016 <- read_csv('./data/data_2016.csv')%>% 
#   filter(DISTRICT == 190) %>% 
#   filter(SCHOOL_NAME != 'All Schools')
# size_2017 <- read_csv('./data/data_2017.csv')%>% 
#   filter(DISTRICT_ID == 190) %>% 
#   filter(SCHOOL_NAME != 'All Schools')
size_2018 <- read_csv('./data/data_2018.csv')%>% 
  filter(DISTRICT_ID == 190)


ix <- 9:15
# size_2010[ix] <- lapply(size_2010[ix], as.numeric) 
# size_2011[ix] <- lapply(size_2011[ix], as.numeric) 
# size_2012[ix] <- lapply(size_2012[ix], as.numeric) 
# size_2013[ix] <- lapply(size_2013[ix], as.numeric) 
# size_2014[ix] <- lapply(size_2014[ix], as.numeric) 
# size_2015[ix] <- lapply(size_2015[ix], as.numeric) 
# size_2016[ix] <- lapply(size_2016[ix], as.numeric) 
# size_2017[ix] <- lapply(size_2017[ix], as.numeric) 
size_2018[ix] <- lapply(size_2018[ix], as.numeric) 

# total_2010 <- size_2010 %>% 
#   summarize(
#     sum(Total, na.rm = TRUE),
#     sum(White, na.rm = TRUE),
#     sum(`African American`, na.rm = TRUE),
#     sum(Hispanic, na.rm = TRUE),
#     sum(Asian, na.rm = TRUE),
#     sum(`Native American`, na.rm = TRUE)
#   ) %>% 
#   mutate(year = 2010) %>% 
#   mutate('Hawaiian or Pacific Islander' = 0)
# 
# total_2011 <- size_2011 %>% 
#   summarize(
#     sum(Total, na.rm = TRUE),
#     sum(White, na.rm = TRUE),
#     sum(`African American`, na.rm = TRUE),
#     sum(Hispanic, na.rm = TRUE),
#     sum(Asian, na.rm = TRUE),
#     sum(`Native American`, na.rm = TRUE)
#   ) %>% 
#   mutate(year = 2011)%>% 
#   mutate('Hawaiian or Pacific Islander' = 0)
# 
# total_2012 <- size_2012 %>% 
#   summarize(
#     sum(Total, na.rm = TRUE),
#     sum(White, na.rm = TRUE),
#     sum(`African American`, na.rm = TRUE),
#     sum(Hispanic, na.rm = TRUE),
#     sum(Asian, na.rm = TRUE),
#     sum(`Native American`, na.rm = TRUE)
#   ) %>% 
#   mutate(year = 2012)%>% 
#   mutate('Hawaiian or Pacific Islander' = 0)
# 
# total_2013 <- size_2013 %>% 
#   summarize(
#     sum(Total, na.rm = TRUE),
#     sum(White, na.rm = TRUE),
#     sum(`African American`, na.rm = TRUE),
#     sum(Hispanic, na.rm = TRUE),
#     sum(Asian, na.rm = TRUE),
#     sum(`Native American`, na.rm = TRUE)
#   ) %>% 
#   mutate(year = 2013)%>% 
#   mutate('Hawaiian or Pacific Islander' = 0)
# 
# total_2014 <- size_2014 %>% 
#   summarize(
#     sum(Total, na.rm = TRUE),
#     sum(White, na.rm = TRUE),
#     sum(`African_American`, na.rm = TRUE),
#     sum(Hispanic, na.rm = TRUE),
#     sum(Asian, na.rm = TRUE),
#     sum(`Native_American`, na.rm = TRUE)
#   ) %>% 
#   mutate(year = 2014)%>% 
#   mutate('Hawaiian or Pacific Islander' = 0)
# 
# total_2015 <- size_2015 %>% 
#   summarize(
#     sum(TOTAL, na.rm = TRUE),
#     sum(WHITE, na.rm = TRUE),
#     sum(`AFRICAN_AMERICAN`, na.rm = TRUE),
#     sum(HISPANIC, na.rm = TRUE),
#     sum(ASIAN, na.rm = TRUE),
#     sum(`NATIVE_AMERICAN`, na.rm = TRUE),
#     sum(HAWAIIAN_PACISLD, na.rm = TRUE)
#   ) %>% 
#   mutate(year = 2015)
# 
# total_2016 <- size_2016 %>% 
#   summarize(
#     sum(TOTAL, na.rm = TRUE),
#     sum(WHITE, na.rm = TRUE),
#     sum(`AFRICAN_AMERICAN`, na.rm = TRUE),
#     sum(HISPANIC, na.rm = TRUE),
#     sum(ASIAN, na.rm = TRUE),
#     sum(`NATIVE_AMERICAN`, na.rm = TRUE),
#     sum(HAWAIIAN_PACISLD, na.rm = TRUE)
#   ) %>% 
#   mutate(year = 2016)
# 
# total_2017 <- size_2017 %>% 
#   summarize(
#     sum(TOTAL, na.rm = TRUE),
#     sum(WHITE, na.rm = TRUE),
#     sum(`AFRICAN_AMERICAN`, na.rm = TRUE),
#     sum(HISPANIC, na.rm = TRUE),
#     sum(ASIAN, na.rm = TRUE),
#     sum(`NATIVE_AMERICAN`, na.rm = TRUE),
#     sum(HAWAIIAN_PACISLD, na.rm = TRUE)
#   ) %>% 
#   mutate(year = 2017)
# 
# total_2018 <- size_2018 %>% 
#   summarize(
#     sum(TOTAL, na.rm = TRUE),
#     sum(WHITE, na.rm = TRUE),
#     sum(`AFRICAN_AMERICAN`, na.rm = TRUE),
#     sum(HISPANIC, na.rm = TRUE),
#     sum(ASIAN, na.rm = TRUE),
#     sum(`NATIVE_AMERICAN`, na.rm = TRUE),
#     sum(HAWAIIAN_PACISLD, na.rm = TRUE)
#   ) %>% 
#   mutate(year = 2018)

# colnames(total_2010) <- c('Total','White','African American','Hispanic','Asian','Native American','Year','Hawaiian/Pacific Islander')
# colnames(total_2011) <- c('Total','White','African American','Hispanic','Asian','Native American','Year','Hawaiian/Pacific Islander')
# colnames(total_2012) <- c('Total','White','African American','Hispanic','Asian','Native American','Year','Hawaiian/Pacific Islander')
# colnames(total_2013) <- c('Total','White','African American','Hispanic','Asian','Native American','Year','Hawaiian/Pacific Islander')
# colnames(total_2014) <- c('Total','White','African American','Hispanic','Asian','Native American','Year','Hawaiian/Pacific Islander')
# 
# colnames(total_2015) <- c('Total','White','African American','Hispanic','Asian','Native American','Hawaiian/Pacific Islander','Year')
# colnames(total_2016) <- c('Total','White','African American','Hispanic','Asian','Native American','Hawaiian/Pacific Islander','Year')
# colnames(total_2017) <- c('Total','White','African American','Hispanic','Asian','Native American','Hawaiian/Pacific Islander','Year')
# colnames(total_2018) <- c('Total','White','African American','Hispanic','Asian','Native American','Hawaiian/Pacific Islander','Year')

#all_years <- rbind(total_2010, total_2011, total_2012, total_2013, total_2014, total_2015, total_2016, total_2017, total_2018)

#write_csv(all_years,'all_years.csv')

size_2018<-size_2018[3:29]

size_2018 <- size_2018 %>% 
  select(-AVERAGE_DAILY_MEMBERSHIP)

colnames(size_2018) <- c("School ID","School Name","Grades Served","African American","% African American",'Asian','% Asian','Economically Disadvantaged','% Economically Disadvantaged','Female','% Female','Hawaiian/PI','% Hawaiian/PI','Hispanic','% Hispanic','ELL','% ELL','Male','% Male','Native American','% Native American','Students with Disabilites','% Students with Disabilites','Total','White','% White')

total_2018 <- size_2018 %>% 
  summarize(
    sum(Total, na.rm = TRUE),
    sum(White, na.rm = TRUE),
    sum(`African American`, na.rm = TRUE),
    sum(Hispanic, na.rm = TRUE),
    sum(Asian, na.rm = TRUE),
    sum(`Native American`, na.rm = TRUE),
    sum(`Hawaiian/PI`, na.rm = TRUE), 
    sum(`Male`, na.rm = TRUE),
    sum(`Female`, na.rm = TRUE),
    sum(`Economically Disadvantaged`, na.rm = TRUE),
    sum(`ELL`, na.rm = TRUE),
    sum(`Students with Disabilites`, na.rm = TRUE),
  ) 

colnames(total_2018) <- c('Total','White','African American','Hispanic','Asian','Native American','Hawaiian/PI','Male','Female','Economically Disadvantaged','ELL','Students with Disabilites')

total_2018 <- data.frame(lapply(total_2018, function(x) as.numeric(as.character(x))))

colnames(total_2018) <- c('Total','White','African American','Hispanic','Asian','Native American','Hawaiian/PI','Male','Female','Economically Disadvantaged','ELL','Students with Disabilites')

total_2018 <- total_2018 %>% 
  mutate('% White'= (White / Total)*100) %>% 
  mutate('% African American'= (`African American`/ Total)* 100) %>% 
  mutate('% Hispanic'= (Hispanic / Total) * 100) %>%
  mutate('% Asian'= (Asian / Total) * 100) %>%
  mutate('% Native American'= (`Native American` / Total) * 100) %>%
  mutate('% Hawaiian/PI'= (`Hawaiian/PI` / Total) * 100) %>% 
  mutate('% Male'= (Male / Total) * 100) %>%
  mutate('% Female'= (Female / Total) * 100) %>%
  mutate('% Economically Disadvantaged'= (`Economically Disadvantaged`/ Total) * 100) %>%
  mutate('% ELL'= (ELL / Total) * 100) %>%
  mutate('% Students with Disabilites'= (`Students with Disabilites` / Total) * 100) %>% 
  mutate('School ID' = 'All MNPS') %>% 
  mutate('School Name' = 'All MNPS') %>% 
  mutate('Grades Served' = 'All MNPS') 

all_2018 <- rbind(size_2018, total_2018)

#write_csv(all_2018, 'all_2018.csv')
