library(DataExplorer)


wrangleGenres = function(returnType='summary'){
###Investigate unique elements listed in'all_genres' and 'original genres' columns 
# split = data.frame(str_split_fixed(df_clean$all_genres, ", ", 3))
# all_genres = data.frame(unique(as.vector(as.matrix(split))))
# split2 = data.frame(str_split_fixed(df_clean$original_genres, ", ", 9))
# originals_genres = data.frame(unique(as.vector(as.matrix(split2))))

#Transform all_genres into separate rows (each title has multiple)
x = df_clean %>% select('title','all_genres')
x = separate_rows(x,sep=", ",all_genres)

#Cleaning function to simplify genre labels (only use top categories with improved names)
clean_genre = function(temp){
  
  temp= gsub('^Docu.*','Documentary',temp)
  temp=  gsub('Sports Movies','Sport',temp)
  temp=  gsub('.*Horror.*','Horror',temp)
  temp=  gsub('.*Comed.*','Comedy',temp)
  temp=  gsub('.*Dramas.*','Drama',temp)
  temp=  gsub('.*Action.*','Action_and_Adventure',temp)
  temp=  gsub('.*Crime.*','Crime',temp)
  temp=  gsub('.*Sci-Fi & Fantasy.*','SciFi_and_Fantasy',temp)
  temp= gsub('.*Kids.*','Kids_and_Family',temp)
  temp= gsub('.*Children.*','Kids_and_Family',temp)
  temp=  gsub('.*Anime.*','Anime',temp)
  temp=  gsub('.*Thrillers.*','Thriller',temp)
  temp=  gsub('.*Romantic.*','Romance',temp)
  temp=  gsub('.*Music.*','Music_and_Musicals',temp)
  temp=  gsub('.*Reality TV.*','Reality_TV',temp)
  
  genVec = c('Documentary','Sport','Horror','Comedy','Drama',
             'Action_and_Adventure','Crime','SciFi_and_Fantasy','Kids_and_Family',
             'Anime','Thriller','Romance','Music_and_Musicals','Reality_TV')
  
  temp = ifelse(!(temp %in% genVec),rep(NA,length(temp)),temp)
}

#Simplify genre titles 
x$all_genres = (clean_genre(x$all_genres)) 
#delete duplicates
x = x[!duplicated(x), ]

#Summary dataframe with counts per genre 
xsummary = x %>% group_by(all_genres) %>% summarise(count = n()) %>% mutate(freq = floor(count/sum(count)*100))

#Dummy dataframe - Turn each genre to a distinct column with binary values
xdummy = dummify(x,select='all_genres')
#Condense into one dummy row per title 
xdummy = xdummy %>% group_by(title) %>% summarise_all(.funs = sum) 
#Rename columns to remove the "all_genres_" prefix 
names(xdummy) <- gsub("all_genres_", "", names(xdummy))

if(returnType=='dummy'){
  return(xdummy)
  }else{
  return(xsummary)
  }
}

wrangleCountries = function(returnType='summary'){
  ###Investigate unique elements listed in 'country' column
  #temp = data.frame(str_split_fixed(df_clean$country, ", ", 10))
  #all_countries = data.frame(unique(as.vector(as.matrix(temp))))
  #There are 123 differnt countries  
  
  #Transform country into separate rows (each title has multiple)
  x = df_clean %>% select('title','country')
  x = separate_rows(x,sep=", ",country)
  #Some titles do not have a country listed. Replace "" with NA
  x$country = trimws(x$country)
  x$country = gsub(' ','_',x$country)
  x$country = gsub(',','',x$country)
  
  x$country = ifelse(x$country=="",rep(NA,length(x$country)),x$country)  
  
  #Summary dataframe with counts per country 
  xsummary = x %>% group_by(country) %>% summarise(count = n()) %>% mutate(freq = floor(count/sum(count)*100))
  
  #Dummify country - Turn each country to a distinct column with binary values
  xdummy = dummify(x,select='country', maxcat = 125) #set maxcat to accomodate entire set of countries 
  #Condense into one dummy row per title 
  xdummy = xdummy %>% group_by(title) %>% summarise_all(.funs = sum) 
  #Rename columns to remove the "country_" prefix 
  names(xdummy) <- gsub("country_", "", names(xdummy))
  
  if(returnType=='dummy'){
    return(xdummy)
  }else{
    return(xsummary)
  }
}


wrangleDirectors = function(returnType='summary'){
  #Transform director into separate rows (each title has multiple)
  x = df_clean %>% select('title','director')
  x = separate_rows(x,sep=", ",director)
  #Some titles do not have a director listed. Replace "" with NA
  x$director = ifelse(x$director=="",rep(NA,length(x$director)),x$director)  
  
  #Summary dataframe with counts per country 
  xsummary = x %>% group_by(director) %>% summarise(count = n()) %>% mutate(freq = floor(count/sum(count)*100))
  
  #Dummify director - Turn each director to a distinct column with binary values
  xdummy = dummify(x,select='director', maxcat = 4480) #set maxcat to accomodate entire set of countries 
  #Condense into one dummy row per title 
  xdummy = xdummy %>% group_by(title) %>% summarise_all(.funs = sum) 
  #Rename columns to remove the "country_" prefix 
  names(xdummy) <- gsub("director_", "", names(xdummy))
  
  if(returnType=='dummy'){
    return(xdummy)
  }else{
    return(xsummary)
  }
}

wrangleActors = function(returnType='summary'){
  #Transform cast into separate rows (each title has multiple)
  x = df_clean %>% select('title','cast')
  x = separate_rows(x,sep=", ",cast)
  #Some titles do not have cast listed. Replace "" with NA
  x$cast = ifelse(x$cast=="",rep(NA,length(x$cast)),x$cast)  
  
  #Summary dataframe with counts per actor  
  xsummary = x %>% group_by(cast) %>% summarise(count = n()) 

    return(xsummary)
}


wrangleInternationalTag = function(returnType='summary'){
  ###Investigate International-related tags listed in 'all_genres' to create new feature  
  # split = data.frame(str_split_fixed(df_clean$all_genres, ", ", 3))
  # all_genres = data.frame(unique(as.vector(as.matrix(split))))

  #Transform all_genres into separate rows (each title has multiple)
  x = df_clean %>% select('title','international_tag' = 'all_genres')
  x = separate_rows(x,sep=", ",international_tag)
  
  #Cleaning function to label International titles 
  clean_intl_genre = function(temp){
    
    temp= gsub('^International.*','International_Tag',temp)
    temp=  gsub('.*Spanish-Language.*','International_Tag',temp)
    temp=  gsub('.*Korean.*','International_Tag',temp)
    temp=  gsub('.*British.*','International_Tag',temp)
  
  
    temp = ifelse((temp != "International_Tag"),rep("NA",length(temp)),temp)
  }
  
  #Create international_tag based on genre labels 
  x$international_tag = (clean_intl_genre(x$international_tag)) 
  #delete duplicates
  x = x[!duplicated(x), ]
  #Summary dataframe with counts per genre 
  xsummary = x %>% group_by(international_tag) %>% summarise(count = n()) %>% mutate(freq = floor(count/sum(count)*100))
  
  
  #Dummy dataframe - Turn each international_tag to a distinct column with binary values
  xdummy = dummify(x,select='international_tag')
  #Condense into one dummy row per title 
  xdummy = xdummy %>% group_by(title) %>% summarise_all(.funs = sum) 
  #Rename columns to remove the "all_genres_" prefix 
  names(xdummy) <- gsub("international_tag_", "", names(xdummy))
  xdummy$International_Tag = if_else(xdummy$International_Tag>0, 1,0)
  
  if(returnType=='dummy'){
    return(xdummy[,1:2])
  }else{
    return(xsummary)
  }
}