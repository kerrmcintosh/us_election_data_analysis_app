
library(tidyverse)
library(shinythemes)
library(shinyWidgets)
library(urbnmapr)
library(ggiraph)
library(janitor)
library(reshape2)

# set_urbn_defaults(style = "map")


#------------kerr code START-------------------
#data brought in
election_data <- read_csv("data/master_data_final.csv") %>% 
  rename(state_name = state)
election_data <- election_data[-c(52,53), ]

pivoted_sf <- election_data %>%
  pivot_longer(cols = contains('party'), names_to = 'election_year', values_to = 'result')
# 

pivoted_sf <- pivoted_sf%>%
  mutate(result = ifelse(result == "republican", "Republican", result)) %>%
  mutate(result = ifelse(result == "democrat", "Democrat", result))  %>%
  mutate(election_year = ifelse(election_year == "party2016", 2016, 2020))

# get map and join date
states <- get_urbn_map(map = "states", sf = TRUE)
pivoted_sf <- states %>% 
  left_join(pivoted_sf , by = "state_name")

vote_share <- read_csv("data/result2016_2020/vote_share.csv") 
state_voting <- read_csv("data/state_voting.csv") 
state_voting <- state_voting %>% 
  mutate(candidate = ifelse(result =="Republican", "Donald Trump", "democrat")) 
state_voting <- state_voting %>% 
  mutate(candidate = ifelse(result =="Democrat" & election_year == 2016, "Hilary Clinton", candidate)) 
state_voting <- state_voting %>% 
  mutate(candidate = ifelse(result =="Democrat" & election_year == 2020, "Joe Biden", candidate))

average_state_vote <- state_voting %>% 
  group_by(election_year, result, candidate) %>% 
  summarise(votes = mean(votes)) %>% 
  mutate(votes = round(votes,1))

# statex <- pull(election_data, state_name)
key_indicators <- read_csv("data/key_indicators.csv") %>% 
  filter(indicator != "black" & indicator != "hispanic" & indicator != "unemployment_change_over_year")

#reorder data table as factor
desired_order <- c("2020", "2016", "college_votes_available", "per_capita_income_2020", "income_change_over_year",
   "health_insured", "unemployment_010920", "adults_65plus",
   "percent_high_school_or_higher", "percent_bachelors_or_higher", "net_int_migration",
    "covid_cases_perc_state_popn", "states_deaths_prop_cases", "adult_gun_at_home_perc")

key_indicators <- key_indicators %>% 
  mutate(indicator =  factor(indicator, levels = desired_order)) %>%
  arrange(indicator)  

key_indicators$indicator = as.character(key_indicators$indicator)

key_indicators <- key_indicators %>% 
  mutate(indicator =
           case_when(
             indicator  == "2020" ~ "2020 Winner",
             indicator  == "2016" ~ "2016 Winner",
             indicator  == "college_votes_available" ~ "Electoral College Votes Available",
             indicator  == "per_capita_income_2020" ~ "Per Capita Income 2020 ($)",
             indicator  == "income_change_over_year" ~ "Income Change Over Year (%)",
             indicator  == "health_insured" ~ "Have Health Insurance (%)",
             indicator  == "unemployment_010920" ~ "Unemployment Rate Sept 2020 (%)",
             indicator  == "adults_65plus" ~ "Proportion of Population Aged 65+ (%)",
             indicator  == "percent_high_school_or_higher" ~ "Proportion of Population with High School Diploma or Higher (%)",
             indicator  == "percent_bachelors_or_higher" ~ "Proportion of Population with Bachelors or Higher (%)",
             indicator  == "net_int_migration" ~ "Net International Migration (%)",
             indicator  == "states_deaths_prop_cases" ~ "Covid Cases as Proportion of Population (%)",
             indicator  == "covid_cases_perc_state_popn" ~ "Covid Deaths as Proportion of Cases (%)",
             indicator  == "adult_gun_at_home_perc" ~ "Proportion of Adults with atleast One Gun in Household (%)",
             TRUE ~ indicator))
# state_voting<- read_csv("data/state_voting.csv") %>% 
# mutate(election_year = ifelse(election_year == 2016, "2016 Winning Vote Share", "2020 Winning Vote Share"))
# state_voting %>%  write_csv("data/state_voting2.csv")
state_votingupd <- read_csv("data/state_voting2.csv") 
key_indicators <-rbind(key_indicators, state_votingupd)

#reorder data table as factor (again)
desired_order <- c("2020 Winner", "2016 Winner", "2020 Winning Vote Share","2016 Winning Vote Share", "Electoral College Votes Available", "Per Capita Income 2020 ($)",
"Income Change Over Year (%)","Have Health Insurance (%)","Unemployment Rate Sept 2020 (%)",
"Proportion of Population Aged 65+ (%)","Proportion of Population with High School Diploma or Higher (%)","Proportion of Population with Bachelors or Higher (%)","Net International Migration (%)",
"Covid Cases as Proportion of Population (%)", "Covid Deaths as Proportion of Cases (%)", "Proportion of Adults with atleast One Gun in Household (%)")

key_indicators <- key_indicators %>%
  mutate(indicator =  factor(indicator, levels = desired_order)) %>%
  arrange(indicator)

key_indicators$indicator = as.character(key_indicators$indicator)
key_indicators <- key_indicators %>%
mutate(indicator = ifelse(indicator == "2020 Winning Vote Share", "2020 Winning Vote Share (%)", indicator)) %>% 
mutate(indicator = ifelse(indicator == "2016 Winning Vote Share", "2016 Winning Vote Share (%)", indicator))

key_indicators %>%  write_csv("data/key_inidcators_bar.csv")

key_indicators_bar <- read_csv("data/key_inicat_mean.csv")

#----table_kerr

#------------kerr code END---------------------


#------------Conor code START-------------------

demo <- read_csv("data/master_data_final.csv")
head(demo %>% 
       select("male", "female"))
melt_demo <- demo[-c(52,53), ] %>% 
  mutate(totalvotes2020 = joe_biden + donald_trump) %>% 
  melt(id.vars = "state",
       measure.vars = c("joe_biden",
                        "donald_trump"))
demo2 <- demo[-c(52,53), ] %>%
  rename(state_name = state)
states <- get_urbn_map(map = "states", sf = TRUE)
demo2 <- states %>% 
  left_join(demo2 , by = "state_name")
ylab <- seq(from = 0.5, to = 20, by = 2)


#------------Conor code END---------------------


#------------David code START-------------------

dw_data_set <- read_csv("data/master_data_final.csv") %>% 
  select(state, total_popn, donald_trump, joe_biden, other_candidates, tot_death, tot_cases, female, male, party2020, college_votes_available) %>% 
  mutate(total_voters = (donald_trump + joe_biden + other_candidates)) %>% 
  mutate(perc_covid_cases = ((tot_cases/total_popn)*100/1)) %>% 
  mutate(perc_covid_deaths = ((tot_death/total_popn)*100/1))
covid <- dw_data_set %>%
  filter(state != "NA") %>%
  filter(state != "United States of America") %>%
  select(state, tot_cases, tot_death, perc_covid_cases, perc_covid_deaths, total_popn,  total_voters, party2020, college_votes_available)

#------------David code END---------------------