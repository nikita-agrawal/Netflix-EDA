library(tidyverse)
library(dplyr)

###Load in all Netflix titles (includes data in 2021) - Kaggle dataset 
df_all = read.csv('./Netflix_EDA/Kaggle_Netflix_Titles.csv',encoding = 'UTF-8')

summary(df_all)
colnames(df_all)

dateFormat = '%B %d, %Y'
df_all$date_added = as.Date(trimws(df_all$date_added, "both"), dateFormat)
df_all$date_added_year = format(df_all$date_added,'%Y')



###Load in all Netflix Originals (as of 2021) - IMDB exported dataset 
df_originals = read.csv('./Netflix_EDA/IMDB_Neftlix_Originals_Edited.csv',encoding = 'UTF-8')
#df_originals = read.csv('./Netflix_EDA/IMDB_Neftlix_Originals_All.csv',encoding = 'UTF-8')
summary(df_originals)
colnames(df_originals)

#add column to label titles as Netflix Originals (will be used to differentiate once merged)
temp = data.frame(rep('Original',dim(df_originals)[1]))
colnames(temp)='original_or_not'
df_originals = cbind(df_originals,temp)

#Rename columns 
names(df_originals) = tolower(names(df_originals))
df_originals = rename(df_originals, 
                        "title_type"="title.type",
                        "imdb_rating"="imdb.rating",
                        "runtime"="runtime..mins.",
                        "num_votes"="num.votes",
                        "date_added"="release.date",
                        "release_year"="year")
colnames(df_originals)
dateFormat = '%Y-%m-%d'
df_originals$date_added = as.Date(trimws(df_originals$date_added, "both"), dateFormat)
#df_originals$release_date_year = format(df_originals$release_date,'%Y')



#format titles 
#convert latin characters to utf-8, trim leading and trailing spaces, lower case 
df_originals$title = tolower(trimws(iconv(df_originals$title,"LATIN2","UTF-8"),"both")) 

df_all$title = tolower(trimws(iconv(df_all$title,"LATIN2","UTF-8"),"both"))
#check for NA's
sum(!is.na(df_all$title))
sum(!is.na(df_originals$title))

#clarify what dates are (date added and release date are same)
# 600-900 Mismatches between Kaggle All Title vs. IMDB Originals Title:
# Marvel's Jessica Jones vs Jessica Jones
# #blackAF vs #BlackAF 
# Narcos:Mexico vs 	Narcos: M<e9>xico
# Project Mc² vs Project Mc<b2>

#fix marvel's titles (helps 6 match)
#check: (gsub("^marvel's *","",df_all$title))[which(grepl("^marvel's*", df_all$title, ignore.case=TRUE))]
df_all$title = gsub("^marvel's *","",df_all$title)
df_all$title = gsub("[:,!,?,/]","",df_all$title)
df_originals$title = gsub("[:,!,?,/]","",df_originals$title)
df_all$title = gsub("[-]"," ",df_all$title)
df_originals$title = gsub("[-]"," ",df_originals$title)

### merging on just the name
df_merged_left = merge(df_all,df_originals,by =c('title'), all.x=TRUE)

#check out excluded and included rows:
#df_originals_excluded = anti_join(df_originals,df_all, by=c('title')) 
#df_originals_included = semi_join(df_originals,df_all, by=c('title')) 

#check out duplicates
dups= df_merged_left[which(duplicated(df_merged_left$title)),]
dups$title 
df_merged_left = df_merged_left[-2141,]
df_merged_left = df_merged_left[-6146,]
df_merged_left[7298,]$title = "u-turn"
#[1] "fearless" "malibu rescue"     "the bleeding edge" "u turn"           
#investigate these and delete duplicate rows 
# 2141 - delete fearless tvseries as it does not exist in df_all  
# 6148 - delete the bleeding edge duplicate 
# u turn is actually a duplicate title, 2 different non-original movies 7301,7302

#update column original_or_not with 'license' label for the non-originals 
df_merged_left$original_or_not[which(is.na(df_merged_left$original_or_not))] = "Licensed"

#clean up df_merged_left 
df_clean = 
df_merged_left %>%
select(original_or_not, 
       title,
       type_mo_tv=type,
       type_specific=title_type,
       director,cast,country,
       date_added.x,release_year.x,
       all_duration=duration,date_added_year,
       originals_duration_min=runtime,
       original_genres = genres, all_genres = listed_in, rating,
       num_votes,imdb_rating)

rm('df_all','df_originals','df_originals_excluded','df_originals_included',
   'dups','temp','df_merged_left','dateFormat')

source(file="/Users/nikiagrawal/Desktop/RFiles/Netflix_EDA/column_cleaning_functions.R")
df_clean_dummy = df_clean %>% select(-original_genres)

df = wrangleInternationalTag('dummy')
df_clean_dummy= merge(df_clean_dummy,df,by =c('title'), all.x=TRUE)

df = wrangleGenres('dummy')
df_clean_dummy= merge(df_clean_dummy,df,by =c('title'), all.x=TRUE)

df = wrangleCountries('dummy')
df_clean_dummy= merge(df_clean_dummy,df,by =c('title'), all.x=TRUE)

#check out summary of which countries and genres are included
#summaryCountries= wrangleCountries('summary')
#summaryGenres = wrangleGenres('summary')

rm('df','df_clean')
df_clean = df_clean_dummy %>% select(-NA.x,-NA.y)
rm('df_clean_dummy')

dim(df_clean)
