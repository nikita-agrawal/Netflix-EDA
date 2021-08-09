library(tidyverse)
library(plotly)
#install.packages('ggthemes', dependencies = TRUE)
library(ggthemes)
library(ggsci)


#Genre columns:colnames(df_clean)[18:31]
genre_cols = colnames(df_clean)[18:31]
#Years we are interested in 
years = c('2014','2015','2016','2017','2018','2019','2020')

#PLOT #1: Visualize the Distribution of Genre Each Year 
data = df_clean %>%
  filter(type_mo_tv=='Movie')%>%
  select(year = date_added_year,genre_cols) %>%
  filter(year %in% years) %>%
  group_by(year) %>%
  summarise_all(.funs=sum) %>%
  pivot_longer(!year, names_to = "genre", values_to = "count") %>%
  group_by(year) %>%
  mutate(percentage = round(count/sum(count),2)) 
g = ggplot(data, aes(x = year,y=percentage*100,fill=genre)) 

p = g + 
  geom_bar(stat='identity')+
  labs(title = "Percentage of Movies by Genre Per Year (2014-2020)",
         x = "Year Added to Netflix",
         y = "Percentage of Titles",
         size = "12") 
p+ scale_fill_tableau(palette = "Tableau 20") + coord_flip()+
  theme_fivethirtyeight()


### PLOT 2: Facet Grid per Genre #from 2015 onwards
#Years we are interested in 
years = c('2014','2015','2016','2017','2018','2019','2020')

data = df_clean %>%
  filter(type_mo_tv=='Movie' & original_or_not=='Original')%>%
  select(year = date_added_year,genre_cols) %>%
  filter(year %in% years) %>%
  group_by(year) %>%
  summarise_all(.funs=sum) %>%
  pivot_longer(!year, names_to = "genre", values_to = "count") %>%
  group_by(year) %>%
  mutate(percentage = count/sum(count)*100) 


g = ggplot(data, aes(x = year,y=percentage,fill=genre)) 
p = g + 
  geom_bar(stat='identity')+
  labs(title = "Netflix Original Movies By Genre:
Percent of Titles Added Over Time",
       x = "Year Added to Netflix",
       y = "Count of Titles",
       size = "12") 
p  +  scale_fill_tableau(palette = "Tableau 20")+
  facet_wrap(vars(genre),scales='free_y') + 
  theme_fivethirtyeight() + 
  theme(legend.position = "none", 
        axis.text=element_text(size=6),
        axis.text.x = element_text(angle = 45))

###Originals-Avg Number of Votes per genre 

years = c('2013','2014','2015','2016','2017','2018','2019','2020')
data = df_clean %>%
  filter(original_or_not=='Original' & date_added_year %in% years) %>%
  select(genre_cols,num_votes,) %>%
  pivot_longer(!(num_votes), 
               names_to = "genre", values_to = "val") %>%
  filter(val==1) %>% 
  select(-val) %>%
  group_by(genre) %>%
  summarise(median_num_votes=median(num_votes))

data[is.na(data)] = 0


g = ggplot(data, aes(x = reorder(genre,median_num_votes),
                     y=median_num_votes,fill=genre)) +
      labs(title = "Originals: Average Number of Votes Per Genre",
       x = "Average Number of Votes",
       y = "Genre",
       size = "12") +
    geom_bar(stat='identity') + 
    coord_flip() +
    scale_fill_tableau(palette = "Classic 20") +  
    geom_text(aes(label = floor(median_num_votes)),
              hjust=-.1, vjust=0, colour = "black",size=3) +
    theme_fivethirtyeight() +
    theme(legend.position = "none")
g

###Originals-Avg IMDB rating per genre 

data = df_clean %>%
  filter(original_or_not=='Original' & date_added_year %in% years) %>%
  select(genre_cols,imdb_rating,) %>%
  pivot_longer(!(imdb_rating), 
               names_to = "genre", values_to = "val") %>%
  filter(val==1) %>% 
  select(-val) %>%
  group_by(genre) %>%
  summarise(avg_IMDB_rating=mean(imdb_rating))

data[is.na(data)] = 0
g = ggplot(data, aes(x = reorder(genre,avg_IMDB_rating),
                     y=avg_IMDB_rating,fill=genre)) +
  labs(title = "Originals: Average IMDB Rating Per Genre",
       x = "Average IMDB Rating",
       y = "Genre",
       size = "12") +
  geom_bar(stat='identity') + 
  coord_flip() +
  scale_fill_tableau(palette = "Tableau 20") +  
  geom_text(aes(label = (round(avg_IMDB_rating,2))), hjust=-.1, vjust=0, colour = "black",size=3)+
  theme_fivethirtyeight() +
  theme(legend.position = "none")
g

###Originals-Histogram of number of titles per imdb rating bin
data = df_clean %>%
  filter(original_or_not=='Original' & date_added_year %in% years) %>%
  select(imdb_rating) 
  
p <- ggplot(data, aes(x=imdb_rating)) + 
  geom_histogram()+scale_x_continuous(breaks=c(0:10))+
  labs(title = "Frequency distribution of titles by rating")


p

###Originals-Grid of Histograms of number of titles per imdb rating bin per genre 
data = df_clean %>%
  filter(original_or_not=='Original' & type_mo_tv=='TV Show'& date_added_year %in% years) %>%
  select(genre_cols,imdb_rating) %>%
  pivot_longer(!(imdb_rating), 
               names_to = "genre", values_to = "val") %>%
  filter(val==1) %>% 
  select(-val) 

ggplot(data, aes(imdb_rating)) + geom_histogram() + facet_wrap(~genre)+
  labs(title = "Frequency Distibution of IMDB Ratings (0-10) for each Genre",
       x = "Average IMDB Rating",
       y = "Number of Titles",
       size = "10") + theme_fivethirtyeight()

###Originals-Grid of Histograms of number of titles per imdb rating TV vs Movie 
data = df_clean %>%
  filter(original_or_not=='Original' & date_added_year %in% years) %>%
  select(type_mo_tv,imdb_rating) 

ggplot(data, aes(imdb_rating, fill=type_mo_tv)) + 
  geom_histogram(bins=25) + 
  labs(title = "Original Movies vs. TV Shows: Distribution of IMDB Ratings",
       x="IMDB Rating",
       y = "Count",
       size = "12") +
  facet_wrap(~type_mo_tv) +
  theme_fivethirtyeight()
  theme(legend.position = "none") 
  
###MOVIES - Originals-Avg IMDB rating per genre 

data = df_clean %>%
  filter(original_or_not=='Original' & date_added_year %in% years 
         & type_mo_tv=='Movie') %>%
  select(genre_cols,imdb_rating,) %>%
  pivot_longer(!(imdb_rating), 
               names_to = "genre", values_to = "val") %>%
  filter(val==1) %>% 
  select(-val) %>%
  group_by(genre) %>%
  summarise(avg_IMDB_rating=mean(imdb_rating))
reorder(position, desc(position))

g = ggplot(data, aes(x = reorder(genre,desc(genre)), #reorder(genre,avg_IMDB_rating
                     y=avg_IMDB_rating)) +
  labs(title = "Original Movies: Average IMDB Rating Per Genre",
       x = "Average IMDB Rating",
       y = "Genre",
       size = "12") +
  geom_bar(stat='identity') + 
  coord_flip() +  
  geom_text(aes(label = (round(avg_IMDB_rating,2))), hjust=-.1, vjust=0, colour = "black",size=3)+
  theme_fivethirtyeight() +
  theme(legend.position = "none")
g

###TV SHOWS - Originals-Avg IMDB rating per genre 

data = df_clean %>%
  filter(original_or_not=='Original' & date_added_year %in% years &
           type_mo_tv='TV Show') %>%
  select(genre_cols,imdb_rating) %>%
  pivot_longer(!(imdb_rating), 
               names_to = "genre", values_to = "val") %>%
  filter(val==1) %>% 
  select(-val) %>%
  group_by(genre) %>%
  summarise(avg_IMDB_rating=mean(imdb_rating))

data[is.na(data)] = 0
g = ggplot(data, aes(x = reorder(genre,avg_IMDB_rating),
                     y=avg_IMDB_rating,fill=genre)) +
  labs(title = "Original Movies: Average IMDB Rating Per Genre",
       x = "Average IMDB Rating",
       y = "Genre",
       size = "12") +
  geom_bar(stat='identity') + 
  coord_flip() +
  scale_fill_tableau(palette = "Tableau 20") +  
  geom_text(aes(label = (round(avg_IMDB_rating,2))), hjust=-.1, vjust=0, colour = "black",size=3)+
  theme_fivethirtyeight() +
  theme(legend.position = "none")
g

#Scatterplot originals - imdb score vs number ratings
#color by genre? by movie vs show?

#option 1 
data = df_clean %>% filter(original_or_not=='Original' & 
           type_mo_tv=='TV Show' & date_added_year %in% years) %>%
  arrange(desc(num_votes))

data = head(data,15)

p<-ggplot(data, aes(x=imdb_rating, y=num_votes,fill=genre,color=genre)) +
  geom_point(size=2) +
  labs(title = "ToNetflix Original TV Shows",
       x = "IMDB Rating",
       y = "Number of Votes",
       size = "12") 
  theme_fivethirtyeight()
p

data2 = df_clean %>%
  filter(original_or_not=='Original' & type_mo_tv=="TV Show" & date_added_year %in% years)%>% 
  select(title,imdb_rating,num_votes,all_genres,genre_cols) %>%
  group_by(title,imdb_rating,num_votes,all_genres) %>%
  summarise_all(.funs=sum) %>%
  pivot_longer(!(title|imdb_rating|num_votes|all_genres), names_to = "genre", values_to = "val") %>%
  filter(val!=0) %>%

data_merged = merge(data2,data,title)

#option 2 - color by movie/tv show - can hover and find top titles 
library(plotly)
fig <- plot_ly(
  data, x = ~imdb_rating, y = ~num_votes,
  # Hover text:
  text = ~paste("Title: ",str_to_title(title),"<br>Rating: ", imdb_rating, '/10<br># Votes: ', num_votes),
  color = ~type_mo_tv
)
fig 

# fig <- fig %>% layout(
#   xaxis = list(range = c(0, 10)),
#   yaxis = list(range = c(0, 500000)))
# fig

#option 3 - color by genre- can hover and find top titles. A 
# title can have multiple genres so will overlap (3 genres, 3 points)

data = df_clean %>%
  filter(original_or_not=='Original' & date_added_year %in% years)%>% 
  select(title,imdb_rating,num_votes,type=type_mo_tv,all_genres,genre_cols) %>%
  group_by(title,imdb_rating,num_votes,type,all_genres) %>%
  summarise_all(.funs=sum) %>%
  pivot_longer(!(title|imdb_rating|num_votes|type|all_genres), names_to = "genre", values_to = "val") %>%
  filter(val!=0)

library(plotly)
fig <- plot_ly(
  data, x = ~imdb_rating, y = ~num_votes,
  # Hover text:
  text = ~paste("Title: ",str_to_title(title),"<br>Genres: ", all_genres,"<br>Rating: ", imdb_rating, '/10<br># Votes: ', num_votes),
  color = ~genre
)
fig <- fig %>% layout(
  xaxis = list(range = c(0, 10)),
  yaxis = list(range = c(0, 00000)))
fig




