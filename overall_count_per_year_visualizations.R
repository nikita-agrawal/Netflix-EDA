
#reference: https://r-graphics.org/recipe-bar-graph-labels
#Graph 1: Total titles per year 
data = df_clean %>% 
  group_by(date_added_year) %>% 
  summarise(count = n()) %>%
  rename(year=date_added_year) 

g = ggplot(data, aes(x=year, y=count))+ geom_col() + 
 geom_text(aes(label = count), vjust=-.5, colour = "black")
g 

#Graph 2: Total Titles per year SPLIT by Netflix original vs. not 
data = df_clean %>% 
  group_by(date_added_year, original_or_not) %>% 
  summarise(count = n()) %>%
  rename(year=date_added_year,Netflix_original=original_or_not) 

ggplot(data, aes(x = year, y = count, fill = Netflix_original)) +
  geom_col(position = "dodge") +
  geom_text(
    aes(label = count),
    colour = "black", size = 3,
    vjust = -.5, position = position_dodge(.9)
  )

#Graph 3: Total Titles per year SPLIT by TV vs Movies 
data = df_clean %>% 
  group_by(date_added_year, type_mo_tv) %>% 
  filter(!(is.na(date_added_year)))%>%
  summarise(count = n()) %>%
  rename(year=date_added_year, type=type_mo_tv) 

ggplot(data, aes(x = year, y = count, fill = type)) +
  geom_col(position = "dodge") + 
  labs(title = "Number of Titles Added* To Netflix Library Per Year (2008-2021)",
       x = "Year Added to Netflix",
       y = "Number of Titles",
       size = "12") +
  theme_fivethirtyeight()+
  geom_text(aes(label = count), colour = "black", size = 3,
            vjust = -.5, position = position_dodge(.9)
  )

#Graph 4: Total Titles per year SPLIT by TV vs Movies SPLIT by Original vs Non
data = df_clean %>% 
  group_by(date_added_year, type_mo_tv, original_or_not) %>% 
  summarise(count = n()) %>%
  rename(year=date_added_year, type=type_mo_tv,Netflix_original=original_or_not) 

ggplot(data, aes(x = year, y = count, fill = paste(type,Netflix_original))) +
  geom_col(position = "dodge") +
  geom_text(
    aes(label = count),
    colour = "black", size = 3,
    vjust = -.5, position = position_dodge(.9)
  )


