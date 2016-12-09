library(dplyr)
library(ggplot2)
library(plotly)
library(grid)
library(gridExtra)
library(DT)
library(GGally)
library(randomForest)
primary <- read.csv("/Users/nitinyadav/Desktop/visualization assignment/2016-us-election/primary_results.csv", stringsAsFactors = FALSE)
demographics <- read.csv("/Users/nitinyadav/Desktop/visualization assignment/2016-us-election/county_facts.csv", stringsAsFactors = FALSE)

votes <- primary %>%  #get the winners and the fraction of votes the won
  filter(party == "Democrat") %>% 
  group_by(state_abbreviation, county) %>% 
  summarize(winner = candidate[which.max(fraction_votes)],
            Vote = max(fraction_votes),
            votes = max(votes))


demographics %<>%
  filter(state_abbreviation %in% c("IA", "NV", "SC")) %>% 
  select(state_abbreviation = state_abbreviation, county = area_name, 
         income = INC110213, hispanic = RHI725214,
         white= RHI825214, college = EDU685213, density = POP060210) %>% 
  mutate(county = gsub(" County", "", county))

# make sure to join by state too since some county names overlap 
votes <- inner_join(votes, demographics, by = c("state_abbreviation","county"))
datatable(votes, class = 'compact')
votes %>% 
  group_by(winner) %>% 
  summarize(round(mean(income)), round(mean(white)), 
            round(mean(college),1),round(mean(density)),round(mean(hispanic),1))%>% 
  datatable( colnames = c(" ",  "Winner", "Income", "White (non-Hispanic)", "Colege", 
                          "Density (pop/sq m)", "Hispanic"), class = 'compact', caption = "Average County Demographics by Winner")
ggplotly(qplot(x =  white, y = college, data = votes, 
               color = winner, size = Vote) +
           ggtitle("Counties by Winner, Whiteness and Educational Attainment"))

ggplotly(qplot(x =  white, y = hispanic, data = votes, 
               color = winner, size = Vote) +
           ggtitle("Counties by Winner, Percentage of Whites and Non-Hispanic Whites"))
ggplotly(qplot(x =  income, y = college, data = votes, 
               color = winner, size = Vote) +
           ggtitle("Counties by Income, Educational Attainment colored by Winner"))

