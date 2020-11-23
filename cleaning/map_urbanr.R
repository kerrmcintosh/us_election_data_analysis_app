library(tidyverse)
library(urbnmapr)
library(urbanthemes)
set_urbn_defaults(style = "map")

#data brought in
election_data <- read_csv("data/master_data_final.csv") %>% 
  rename(state_name = state)
election_data <- election_data[-c(52,53), ]

# get map and join date
states_sf <- get_urbn_map(map = "states", sf = TRUE)
states_sf <- states_sf %>% 
  left_join(election_data, by = "state_name")


# create tooltips and onclick events
states_ <- sprintf("<p>%s</p>",
                   as.character(states_sf$state) )  

states_sf %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = party2020),
          color = "#ffffff", size = 0.25) + 
  scale_fill_manual(values = c("#5390D9", "#D90429")) +
  labs(fill = "") +
  coord_sf(datum = NA) +
  geom_sf_text(data = get_urbn_labels(map = "states", sf = TRUE), 
               aes(label = state_abbv), 
               size = 3) + 
  theme_void()
View(vote_share$electoral_votes)

library(tidyverse)
vote_share <- read_csv("data/result2016_2020/vote_share.csv") 

vote_share %>%
  filter(elect_year == 2020) %>% 
  ggplot() +
  aes(x = candidate, y = electoral_votes, fill=party ) +
  geom_col(position = "dodge", width=0.75) +
  labs(x = "") +
  theme_light() +
  theme(axis.title.x = element_blank(),
        axis.text.y = element_text(size = 12,  margin=margin(0,0,0,0)),
        axis.text.x = element_blank(),
        legend.position = "none",
        line = element_blank(),
        panel.border = element_blank()) +
  scale_fill_manual(l=40)(values = c("#5390D9", "#D90429")) +
  coord_flip() +
  geom_text(aes(label = paste0(votes_perc,"%")), position = position_stack(vjust = 0.9), size = 8 ,colour = "#ffffff")

# % end of graph white  remove lines, set width
library(tidyverse)
state_voting <- read_csv("data/president_county_candidate.csv")
colnames(state_voting)

state_votingdtotal <- state_voting %>% 
group_by(state) %>% 
  summarise(all_votes = sum(votes))
state_votingdtotal 


state_votingdtjb <- state_voting %>% 
  group_by(state, candidate) %>% 
  summarise(sum = sum(votes)) %>% 
  filter(candidate == "Donald Trump" | candidate == "Joe Biden")
state_votingdtjb 

state_voting_full <-  state_votingdtjb %>% 
  pivot_wider(names_from = candidate, values_from = sum)


state_voting_full <- left_join(state_voting_full, state_votingdtotal)
View(state_voting_full)
state_voting_full
result <- read_csv("data/result2016_2020/2020result_party.csv")
state_voting_full <- left_join(state_voting_full, result)
state_voting_full <- state_voting_full %>% 
  janitor::clean_names() %>% 
  mutate(won_vote_share_2020 = ifelse(party2020 == "Democrat", (joe_biden / all_votes)*100,  (donald_trump / all_votes)*100)) %>% 
  mutate(won_vote_share_2020 = round(won_vote_share_2020, 1))



election_data <- read_csv("data/master_data_final.csv")
election_data <- election_data %>% 
  select(c("state", "party2020", "party2016", "candidatevotes2016", "totalvotes2016")) %>% 
  mutate(won_vote_share_2016 = (candidatevotes2016 / totalvotes2016)* 100) %>% 
  mutate(won_vote_share_2016 = round(won_vote_share_2016, 1)) %>% 
  select(-c("candidatevotes2016", "totalvotes2016"))
  
colnames(state_voting_full)
state_voting_full <- state_voting_full %>% 
select(-c("donald_trump", "joe_biden", "all_votes", "party2020", "college_votes_available"))
      
state_voting_full <- left_join(state_voting_full, election_data)  
View(state_voting_full)
state_voting_full <- state_voting_full %>% 
  mutate(party2016 = ifelse(party2016 == "republican", "Republican", "Democrat"))
state_voting_full %>%  write_csv("data/state_voting.csv")

pivoted_voting <- state_voting_full %>%
  pivot_longer(cols = contains('party'), names_to = 'election_year', values_to = 'result') %>% 
  pivot_longer(cols = contains('won_vote'), names_to = 'vote_year', values_to = 'votes') %>% 
  select(-"vote_year")
View(pivoted_voting)
pivoted_voting <- pivoted_voting %>% 
  mutate(election_year = ifelse(election_year == "party2016", 2016, 2020))
View(pivoted_voting)
pivoted_voting %>%  write_csv("data/state_voting.csv")

update <- read_csv("data/state_voting.csv")
distinct(update)
