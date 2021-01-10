rm(list=ls())
# import US murders data
library(tidyverse)
library(ggrepel)
library(dslabs)
ds_theme_set()
data(murders)
head(murders)

# import US election results data
data(polls_us_election_2016)
head(results_us_election_2016)
identical(results_us_election_2016$state, murders$state)

# join the murders table and US election results table
tab <- left_join(murders, results_us_election_2016, by = "state")
head(tab)

# plot electoral votes versus population
tab %>% ggplot(aes(population/10^6, electoral_votes, label = abb)) +
  geom_point() +
  geom_text_repel() + 
  scale_x_continuous(trans = "log2") +
  scale_y_continuous(trans = "log2") +
  geom_smooth(method = "lm", se = FALSE)

# make two smaller tables to demonstrate joins
tab1 <- slice(murders, 1:6) %>% select(state, population)
tab1
tab2 <- slice(results_us_election_2016, c(1:3, 5, 7:8)) %>% select(state, electoral_votes)
tab2

# experiment with different joins
left_join(tab1, tab2)
tab1 %>% left_join(tab2)
tab1 %>% right_join(tab2)
inner_join(tab1, tab2)
semi_join(tab1, tab2)
anti_join(tab1, tab2)

### Questions 5-7 

library(Lahman)
top <- Batting %>% 
  filter(yearID == 2016) %>%
  arrange(desc(HR)) %>%    # arrange by descending HR count
  slice(1:10)    # take entries 1-10
top %>% as_tibble()

#Also Inspect the Master data frame, which has demographic information for all players:
  
  Master %>% as_tibble()
  
  
  
#  Question 5
#  1 point possible (graded)
#  Use the correct join or bind function to create a combined table of the names and 
#  statistics of the top 10 home run (HR) hitters for 2016. This table should have the player ID,
# first name, last name, and number of HR for the top 10 players. Name this data frame top_names.
  
# Identify the join or bind that fills the blank in this code to create the correct table:
    
  top_names <- top %>% left_join(Master) %>%
    select(playerID, nameFirst, nameLast, HR)
  
  top_names
  
  
 # Question 6
#  1 point possible (graded)
#  Inspect the Salaries data frame. Filter this data frame to the 2016 salaries, then use 
#  the correct bind join function to add a salary column to the top_names data frame from the
#  previous question. Name the new data frame top_salary. Use this code framework:
    
    
    top_salary <- Salaries %>% filter(yearID == 2016) %>%
    right_join(top_names) %>%
    select(nameFirst, nameLast, teamID, HR, salary)
    
    top_salary
    
    AwardsPlayers
    arrange(desc(HR)) %>%    # arrange by descending HR count
      slice(1:10)    # take entries 1-10
    
    bat<-Batting %>% filter(yearID==2016) %>% select(playerID, yearID, HR)%>%arrange(desc(HR))
    bat
    
    ap<-AwardsPlayers %>% filter(yearID== 2016) %>% select(playerID, awardID)
    
    combined<-ap %>% left_join(bat)%>% arrange(desc(HR))
    combined<-combined %>% left_join(top_names)
    
combined<-combined%>%group_by(playerID)%>%summarize(n=n(), topten=!is.na(nameFirst))
combined