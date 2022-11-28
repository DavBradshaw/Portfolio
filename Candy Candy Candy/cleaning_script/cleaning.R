#load libraries
library(tidyverse)
library(here)
library(janitor)
library(readxl)

#load data
raw_candy_2015 <- read_xlsx(here("raw_data/boing-boing-candy-2015.xlsx"))
raw_candy_2016 <- read_xlsx(here("raw_data/boing-boing-candy-2016.xlsx"))
raw_candy_2017 <- read_xlsx(here("raw_data/boing-boing-candy-2017.xlsx"))

#view data
#2015 - id, age,trick or treat, candy questions, others
#2016 - id, trick or treat, gender, age, country, province, candy questions, others
#2017 - id, trick or treat, gender, age, country, province, candy questions, others


#need to make column names the same across tables
#first use clean names to tidy them
#then rename columns desired to consistent format
#then remove columns not wanted
#then added a year for when data is combined
#chose not to str_extract for candy that had been manually entered as data in the gives you joy/despair columns.

candy_2015 <- raw_candy_2015 %>%
  clean_names() %>% 
  rename("ID" = "timestamp", "age" = "how_old_are_you", "trick_or_treating" = "are_you_going_actually_going_trick_or_treating_yourself") %>% 
  select(-c(1, 97:113,116:124)) %>%
  mutate(year = 2015)

candy_2016 <- raw_candy_2016 %>% 
  clean_names() %>% 
  rename("ID" = "timestamp", "age" = "how_old_are_you", "trick_or_treating" = "are_you_going_actually_going_trick_or_treating_yourself", "gender" = "your_gender", "country" = "which_country_do_you_live_in", "state" = "which_state_province_county_do_you_live_in") %>% 
  select(-c(1, 6, 107:123)) %>%
  mutate(year = 2016)

candy_2017 <- raw_candy_2017 %>% 
  clean_names() %>% 
  rename("ID" = "internal_id", "age" = "q3_age", "trick_or_treating" = "q1_going_out", "gender" = "q2_gender", "country" = "q4_country", "state" = "q5_state_province_county_etc") %>% 
  select(-c(1, 6, 110:120)) %>%
  mutate(year = 2017)

#Used this formaula (can adapt to any dataset and column) to count unique values in the column of the dataset to know which to recode as below

#candy %>% 
# group_by(age) %>% 
#summarise(count = n())
#age - 274 values(when done on pivoted data), needs to be sorted before it is pivoted

#fix data inconsistencies found when checking data after initial screen complete
#Age
#When turning the value in the age column into an integer 85 columns retain an NA value
#whilst having data in the age column.

#Used mutate mutate(age_numeric = as.integer(age)) and additional filter(is.na(age_numeric), !is.na(age))
#during test process to filter out undesired results

#had issues with recoding certain lines (more wordy, may have missed spaces or something), used a filter with regex and pull function 
#to identify those lines and pull the relevant data to use in expression

age_sorted_candy_2015 <- candy_2015 %>% 
  mutate(age = recode(age,
                      "37 (I'm taking a child)" = "37",
                      "46:" = "46",
                      "37," = "37",
                      "40. Deal with it." = "40",
                      "50 (despair)" = "50",
                      "50, taking a 13 year old." = "50",
                      "42 - I'm taking my kid" = "42",
                      "27^" = "27",
                      "45, but the 8-year-old Huntress and bediapered Unicorn give me political cover and social respectability.  However, I WILL eat more than they do combined." = "45",
                      "Good Lord!  I'm 43!" = "43")) %>% 
  mutate(age = as.integer(age))


age_sorted_candy_2016 <- candy_2016 %>% 
  mutate(age = recode(age,
                      "49 11/12ths" = "49")) %>% 
  mutate(age = as.integer(age))


age_sorted_candy_2017 <- candy_2017 %>% 
  mutate(age = recode(age,
                      "5u" = "5",
                      "46 Halloweens." = "46",
                      "59 on the day after Halloween"= "58",
                      "sixty-nine" = "69")) %>% 
  mutate(age = as.integer(age))

#candy %>% 
# group_by(trick_or_treating) %>% 
#summarise(count = n())
#trick_or_treating - 3 values, may be better as TRUE/FALSE logical but okay as is

#then sort countries


#candy %>% 
# group_by(country) %>% 
#summarise(count = n())

#need to include 1 "united states of americca" that isn't working right in recode
#to be usa, in this case search by contains "uni" as everything else containing this
#had been filtered already
#candy %>% 
#  filter(str_detect(country, "uni")) %>% 
#  pull()
#the pull provides data in exact form
#country - 169 rows but some show duplication e.g. America vs america - needs to be sorted before it is pivoted. Also potentially has some age data
#Only want USA, Canada, UK and other
#Used a group by country and count to find individual entires and recode to consolidate values.
#Will use -NULL/NA, -usa, -canada and -uk to deal with other.

country_sorted_candy_2016 <- age_sorted_candy_2016 %>% 
  mutate(country = str_to_lower(country)) %>% 
  mutate(country = recode(country,
                          "30.0" = "na",
                          "44.0" = "na",
                          "45.0" = "na",
                          "47.0" = "na", 
                          "51.0" = "na",
                          "54.0" = "na",
                          "denial" = "na",
                          "one of the best ones" = "na",
                          "neverland" = "na",
                          "the best one - usa" = "usa",
                          "see above" = "na", 
                          "the yoo ess of aayyyyyy" = "usa",
                          "united states" = "usa",
                          "us" = "usa",
                          "united states of america" = "usa",
                          "america" = "usa",
                          "merica" = "usa",
                          "england" = "uk",
                          "eua" = "usa",
                          "god's country" = "na",
                          "murica" = "usa",
                          "somewhere" = "na", 
                          "sub-canadian north america... 'merica" = "usa",
                          "the yoo ess of aaayyyyyy" = "usa",
                          "there isn't one for old men" = "na", 
                          "this one" = "na",
                          "trumpistan" = "na", 
                          "u.s." = "usa",
                          "not the usa or canada" = "na",
                          "u.s.a." = "usa",
                          "united states of america" = "usa",
                          "united kingdom" = "uk",
                          "united kindom" = "uk",
                          "united sates" = "usa", 
                          "united state" = "usa",
                          "united stetes" = "usa",
                          "units states" = "usa",
                          "united states of america" = "usa",
                          "usa (i think but it's an election year so who can really tell)" = "usa",
                          "usa usa usa" = "usa",
                          "usa usa usa usa" = "usa",
                          "usa!" =	"usa",
                          "usa! usa!" =	"usa",
                          "usa! usa! usa!"	= "usa",
                          "usa!!!!!!" =	"usa",
                          "ussa" = "usa",
                          "united states of america" = "usa",
                          "united states of america	" = "usa",
                          "unied states" = "usa",
                          "united  states of america" = "usa"
  )) %>%
  mutate(country = na_if(country, "na"))


country_sorted_candy_2017 <- age_sorted_candy_2017 %>% 
  mutate(country = str_to_lower(country)) %>% 
  mutate(country = recode(country,
                          "1" = "na",
                          "32" = "na",
                          "35" = "na",
                          "45" = "na",
                          "46" = "na", 
                          "a" = "na",
                          "united states of america" = "usa",
                          "united kingdom" = "uk",
                          "united states of america" = "usa",
                          "'merica" = "usa",
                          "ahem....amerca" = "usa",
                          "america" = "usa",
                          "atlantis" = "na",
                          "california" = "usa",
                          "can" = "canada",
                          "canada`" = "canada",
                          "earth" = "na",
                          "endland" = "uk",
                          "england" = "uk",
                          "fear and loathing" = "na",
                          "i don't know anymore" = "na",
                          "i pretend to be from canada, but i am really from the united states." = "usa",
                          "alaska" = "usa",
                          "insanity lately" = "na",
                          "muria" = "na",
                          "murrika" = "usa",
                          "murica" = "usa",
                          "n. america" = "na",
                          "narnia" = "na",
                          "new jersey" = "usa",
                          "new york" = "usa",
                          "north carolina" = "usa",
                          "scotland" = "uk",
                          "pittsburgh" = "usa",
                          "subscribe to dm4uz3 on youtube" = "na",
                          "the united states" = "usa",
                          "the united states of america" = "usa",
                          "trumpistan" = "na",
                          "u s" = "usa",
                          "u s a" = "usa",
                          "u.k." = "uk",
                          "u.s" = "usa",
                          "ussa" = "usa",
                          "usausausa" = "usa",
                          "usas" = "usa",
                          "usaa" = "usa",
                          "usa? hard to tell anymore.." = "usa",
                          "usa! usa! usa!" = "usa",
                          "usa usa usa!!!!" = "usa",
                          "us of a" = "usa",
                          "us" = "usa",
                          "unites states" = "usa",
                          "united ststes" = "usa",
                          "united statss" = "usa",
                          "united states" = "usa",
                          "united stated" = "usa",
                          "united statea" = "usa",
                          "united state" = "usa",
                          "united staes" = "usa",
                          "united sates" = "usa",
                          "unite states"= "usa",
                          "unhinged states" = "usa",
                          "u.s.a." = "usa", 
                          "u.s."= "usa",
                          "ud" = "uk",
                          "united states of america	" = "usa",
                          "unied states" = "usa",
                          "united states of america" = "usa",
                          "united  states of america" = "usa"
  )) %>% 
  mutate(country = na_if(country, "na"))


#Pivot data into long format

pivoted_2015 <- age_sorted_candy_2015 %>% 
  pivot_longer(cols = c(3:97),
               names_to = "candy_type",
               values_to = "rating")


pivoted_2016 <- country_sorted_candy_2016 %>% 
  pivot_longer(cols = c(5:104),
               names_to = "candy_type",
               values_to = "rating") 

pivoted_2017 <- country_sorted_candy_2017 %>% 
  pivot_longer(cols = c(5:107),
               names_to = "candy_type",
               values_to = "rating")
#check that all data is bound, should be same number of observations as full_candy_data (it is)
number_of_obs = 529220 + 125900 + 250920


#candy %>% 
#group_by(candy_type) %>% 
#summarise(count = n())
#candy_type - 124 types, total if present in all 3 years should be 9349, only need to look at values that do not equal that.
5630 + 1259 + 2460







#bind rows
full_candy_data <- pivoted_2015 %>% 
  bind_rows(pivoted_2016, pivoted_2017) 

#Use str_replace_all with regex to change wording in 2017 data to remove "q._"
re_named_data <- full_candy_data %>% 
  mutate(candy_type = str_replace_all(candy_type, "q._", "")) %>% 
  unnest() 


#Take a slice head here of data right before it is pivoted (carry out mutate above as well), pivot together and count candy bars names and make the same if possible (ideally 3 if present all years)
#introduced additional recode function below to overcome these for any I could spot
#added a line to convert ratings to lower case for ease when filtering later.


re_named_data <- re_named_data%>%
  mutate(candy_type = recode(candy_type,
                             "boxo_raisins" = "box_o_raisins",
                             "anonymous_brown_globs_that_come_in_black_and_orange_wrappers" = "anonymous_brown_globs_that_come_in_black_and_orange_wrappers_a_k_a_mary_janes",
                             "mary_janes" = "anonymous_brown_globs_that_come_in_black_and_orange_wrappers_a_k_a_mary_janes",
                             "sweetums_a_friend_to_diabetes" = "sweetums",
                             "dark_chocolate_hershey" = "hersheys_dark_chocolate",
                             "x100_grand_bar" = "100_grand_bar")) %>% 
  mutate(rating = str_to_lower(rating)) %>% 
  mutate(trick_or_treating = str_to_lower(trick_or_treating))


#lots of outlier ages included in the dataset, remove ages <3 and >100 using case.when so ages
#>2 and <101 retain their number and everything else changes to NA

impossible_age_filtered_data <- re_named_data %>% 
  mutate(age = case_when(
    age >2 & age < 101 ~ as.integer(age)
  ))

#write data


write_csv(impossible_age_filtered_data, here("clean_data/clean_candy_data.csv"))