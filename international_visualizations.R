library(tidyverse)
library(plotly)
#install.packages('ggthemes', dependencies = TRUE)
library(ggthemes)
library(ggsci)
# What does international tag mean? Not exclusive of USA, but that the title was
# either produced in a country other than the USA alone, includes labels like
# spanish language, korean, british tv 

dim(df_clean %>% filter(International_Tag==1,United.States==1) %>% select(title))
##[1] 237   1
a = dim(df_clean %>% filter(International_Tag==1,United.States!=1) %>% select(title))
##[1] 3559    1
#dim(df_clean %>% filter(International_Tag!=1,United.States!=1) %>% select(title))

#Years we are interested in 
years = c('2014','2015','2016','2017','2018','2019','2020')

#PLOT #1: Of all the titles in Netflix's library, how many international titles were added per year? Line chart 
#about 20% titles were international before 2015, since then it
#has increased to about 50% per year  
data = df_clean %>%
  select(year = date_added_year,type=type_mo_tv,International_Tag) %>%
  filter(year %in% years) %>%
  group_by(year,type) %>%
  summarise(count = sum(International_Tag),total=n()) %>%
  mutate(percentage = round(count/total,2)*100)
  

g = ggplot(data, aes(x = year,y=percentage,color=type,group=type))+
  geom_line(size=1) + geom_point(size=2)+
  labs(title = "Percent of International Titles Added Over Time",
       x = "Year Added to Netflix",
       y = "Percentage of Titles",
       size = "12") 
g + theme_fivethirtyeight()

#Of all the originals, how many international titles were added per year?
data = df_clean %>%
  filter(original_or_not=='Original') %>%
  select(year = date_added_year,type=type_mo_tv,International_Tag) %>%
  filter(year %in% years) %>%
  group_by(year,type) %>%
  summarise(count = sum(International_Tag),total=n()) %>%
  mutate(percentage = round(count/total,2)*100)
g = ggplot(data, aes(x = year,y=percentage,color=type,group=type))+
  geom_line(size=1) + geom_point(size=2)+
  labs(title = "Originals: Percent of Internatonal Titles Added Over Time",
       x = "Year Added to Netflix",
       y = "Percentage of Titles",
       size = "12") 
g + scale_color_tableau(palette = "Tableau 10")+ theme_fivethirtyeight()


#Of the originals, how do the imdb ratings compare for USA produced vs not 
years = c('2014','2015','2016','2017','2018','2019','2020')
data = df_clean %>%
  filter(original_or_not=='Original' & date_added_year %in% years) %>%
  select(USA_or_not = United_States,imdb_rating,num_votes) %>%
  mutate(USA_or_not = ifelse(USA_or_not==1,'USA','International'))

b = boxplot(imdb_rating~USA_or_not, data=data,
            xlab = "Country Produced",
            ylab = "IMDB Score",
            main = "Netflix Original IMDB Ratings")
library(gridExtra)
library(grid)
grid.table(data.frame(b$stats))


#num votes boxplot 
g = ggplot(data, aes(x = USA_or_not,y=num_votes))+
  geom_boxplot(outlier.shape=NA)+
  coord_cartesian(ylim=quantile(data$num_votes,c(0.1,0.9),na.rm=TRUE))
g

#Of all the international titles, how many were original vs liscensed 
#between 2015-2017, netflix has added more liscensed 
#1/4 International titles are netflix originals 
data = df_clean %>%
  select(year = date_added_year,type=original_or_not,International_Tag) %>%
  filter(year %in% years & International_Tag==1) %>%
  group_by(year,type) %>%
  summarise(count = sum(International_Tag)) %>%
  group_by(year)%>%
  mutate(total=sum(count),percentage = round(count/total,2)*100)
g = ggplot(data, aes(x = year,y=percentage,color=type,group=type))+
  geom_line(size=1) + geom_point(size=2)+
  labs(title = "International Titles: Licensed vs. Original",
       x = "Year Added to Netflix",
       y = "Percentage of Titles",
       size = "12") 
g + scale_color_tableau(palette = "Tableau 10")+ theme_fivethirtyeight()

### PLOT 2 (a,b,c): Look at countries (121 total) where movies are produced 
#In 2015, there are 2 players - over half movies produced in US. 
#57.7% produced in the US, only 8.5% in France, less than 5% 
#UK, Italy, Canada, Belgium and Australia 
#Dominated by western countries 
country_cols = colnames(df_clean)[32:148]
data = df_clean %>%
  select(year = date_added_year,type=type_mo_tv,country_cols) %>%
  filter(year =='2015' & type=='Movie') %>%
  group_by(year,type) %>%
  summarise_all(.funs=sum) %>%
  pivot_longer(!(year | type), names_to = "country", values_to = "count") %>%
  mutate(total = sum(count),percentage = (count/total)*100) %>%
  arrange(desc(percentage)) %>%
  slice_head(n=20)

g = ggplot(data, aes(x = reorder(country,percentage),y=percentage))+
  geom_bar(stat='identity') + 
  labs(title = "Top 20 Countries: 2015",
       x = "Country of Production",
       y = "Percentage of Movies",
       size = "12") 
g + coord_flip() + scale_fill_tableau(palette = "Tableau 20") +  
  geom_text(aes(label = paste0(round(percentage,1),"%")), hjust=-.1, vjust=0, colour = "black",size=3)+
  theme_classic() +
  theme(legend.position = "none")
#
#By 2017, there are 3 players - USA production reduces to a third of the total at 33.4%, and India emerges
#as the second most popular country of production with 13.6%, followed by the UK
#at 7.5%, all other 17 countries less than 5% 
#This is maintained in 2019 
data = df_clean %>%
  select(year = date_added_year,type=type_mo_tv,country_cols) %>%
  filter(year =='2017' & type=='Movie') %>%
  group_by(year,type) %>%
  summarise_all(.funs=sum) %>%
  pivot_longer(!(year | type), names_to = "country", values_to = "count") %>%
  mutate(total = sum(count),percentage = (count/total)*100) %>%
  arrange(desc(percentage)) %>%
  slice_head(n=20)

g = ggplot(data, aes(x = reorder(country,percentage),y=percentage))+
  geom_bar(stat='identity') + 
  labs(title = "Top 20 Countries: 2017",
       x = "Country of Production",
       y = "Percentage of Movies",
       size = "12") 
g + coord_flip() + scale_fill_tableau(palette = "Tableau 20") +  
  geom_text(aes(label = paste0(round(percentage,1),"%")), hjust=-.1, vjust=0, colour = "black",size=3)+
  theme_classic() +
  theme(legend.position = "none")

#In 2019, 40% US, 12.4% India, 7.5% UK
data = df_clean %>%
  select(year = date_added_year,type=type_mo_tv,country_cols) %>%
  filter(year =='2015' & type=='Movie') %>%
  group_by(year,type) %>%
  summarise_all(.funs=sum) %>%
  pivot_longer(!(year | type), names_to = "country", values_to = "count") %>%
  mutate(total = sum(count),percentage = (count/total)*100) %>%
  arrange(desc(percentage)) %>%
  slice_head(n=20)

g = ggplot(data, aes(x = reorder(country,percentage),y=percentage))+
  geom_bar(stat='identity') + 
  labs(title = "Top 20 Countries: Percent of Movies Added in 2017",
       x = "Country of Production",
       y = "Percentage of Movies",
       size = "12") 
g + coord_flip() + scale_fill_tableau(palette = "Tableau 20") +  
  geom_text(aes(label = paste0(round(percentage,1),"%")), hjust=-.1, vjust=0, colour = "black",size=3)+
  theme_classic() +
  theme(legend.position = "none")


### PLOT 3 (a,b,c): Look at countries (121 total) where TV shows are produced 
#In 2015, there are 2 players - over half movies produced in US. 
#57.7% produced in the US, only 8.5% in France, less than 5% 
#UK, Italy, Canada, Belgium and Australia 
country_cols = colnames(df_clean)[32:148]
data = df_clean %>%
  select(year = date_added_year,type=type_mo_tv,country_cols) %>%
  filter(year =='2015' & type=='TV Show') %>%
  group_by(year,type) %>%
  summarise_all(.funs=sum) %>%
  pivot_longer(!(year | type), names_to = "country", values_to = "count") %>%
  mutate(total = sum(count),percentage = (count/total)*100) %>%
  arrange(desc(percentage)) %>%
  slice_head(n=20)

g = ggplot(data, aes(x = reorder(country,percentage),y=percentage))+
  geom_bar(stat='identity') + 
  labs(title = "Top 20 Countries: Percent of TV Shows Added in 2015",
       x = "Country of Production",
       y = "Percentage of TV Shows",
       size = "12") 
g + coord_flip() + scale_fill_tableau(palette = "Tableau 20") +  
  geom_text(aes(label = paste0(round(percentage,1),"%")), hjust=-.1, vjust=0, colour = "black",size=3)+
  theme_classic() +
  theme(legend.position = "none")
#
#By 2017, there are 3 players - USA production reduces to a third of the total at 33.4%, and India emerges
#as the second most popular country of production with 13.6%, followed by the UK
#at 7.5%, all other 17 countries less than 5% 
#This is maintained in 2019 
data = df_clean %>%
  select(year = date_added_year,type=type_mo_tv,country_cols) %>%
  filter(year =='2017' & type=='TV Show') %>%
  group_by(year,type) %>%
  summarise_all(.funs=sum) %>%
  pivot_longer(!(year | type), names_to = "country", values_to = "count") %>%
  mutate(total = sum(count),percentage = (count/total)*100) %>%
  arrange(desc(percentage)) %>%
  slice_head(n=20)

g = ggplot(data, aes(x = reorder(country,percentage),y=percentage))+
  geom_bar(stat='identity') + 
  labs(title = "Top 20 Countries: Percent of TV Shows Added in 2017",
       x = "Country of Production",
       y = "Percentage of TV Shows",
       size = "12") 
g + coord_flip() + scale_fill_tableau(palette = "Tableau 20") +  
  geom_text(aes(label = paste0(round(percentage,1),"%")), hjust=-.1, vjust=0, colour = "black",size=3)+
  theme_classic() +
  theme(legend.position = "none")

#In 2019, 40% US, 12.4% India, 7.5% UK
data = df_clean %>%
  select(year = date_added_year,type=type_mo_tv,country_cols) %>%
  filter(year =='2019' & type=='TV Show') %>%
  group_by(year,type) %>%
  summarise_all(.funs=sum) %>%
  pivot_longer(!(year | type), names_to = "country", values_to = "count") %>%
  mutate(total = sum(count),percentage = (count/total)*100) %>%
  arrange(desc(percentage)) %>%
  slice_head(n=20)

g = ggplot(data, aes(x = reorder(country,percentage),y=percentage))+
  geom_bar(stat='identity') + 
  labs(title = "Top 20 Countries: Percent of TV Shows Added in 2019",
       x = "Country of Production",
       y = "Percentage of TV Shows",
       size = "12") 
g + coord_flip() + scale_fill_tableau(palette = "Tableau 20") +  
  geom_text(aes(label = paste0(round(percentage,1),"%")), hjust=-.1, vjust=0, colour = "black",size=3)+
  theme_classic() +
  theme(legend.position = "none")



