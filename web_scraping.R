library(rvest)
library(dplyr)
library(tidyverse)
library(purrr)
library(tidyr)
library(stringr)
library(ggplot2)
library(webr)
library(lubridate)

### Create a storing dataframe named movies
movies = data.frame()
### Create imdb dataframe where all data will be stored
imdb = data.frame()

for(page_result in seq(from =1, to= 6951, by= 50)) {
  
  link= paste0("https://www.imdb.com/search/title/?companies=co0144901&start=",page_result,"&ref_=adv_nxt")
  
  page = read_html(link)
  
  movies <- page %>% 
    html_nodes('.lister-item-content') %>% 
    map_df(~list(rank = html_nodes(.x, '.text-primary') %>% 
                   html_text() %>% 
                   {if(length(.) == 0) NA else .},
                 title = html_nodes(.x, '.lister-item-header a') %>% 
                   html_text() %>% 
                   {if(length(.) == 0) NA else .},
                 year = html_nodes(.x, '.text-muted.unbold') %>% 
                   html_text() %>% 
                   {if(length(.) == 0) NA else .},
                 rating = html_nodes(.x, '.ratings-imdb-rating strong') %>% 
                   html_text() %>% 
                   {if(length(.) == 0) NA else .},
                 votes = html_nodes(.x, '.sort-num_votes-visible span:nth-child(2)') %>% 
                   html_text() %>% 
                   {if(length(.) == 0) NA else .},
                 genre = html_nodes(.x, '.genre') %>% 
                   html_text() %>% 
                   {if(length(.) == 0) NA else .},
                 tv_rating = html_nodes(.x, '.certificate') %>% 
                   html_text() %>% 
                   {if(length(.) == 0) NA else .},
                 runtime = html_nodes(.x, '.runtime') %>% 
                   html_text() %>% 
                   {if(length(.) == 0) NA else .},
                 director = html_nodes(.x, '.text-muted~ .text-muted+ p , .ratings-bar~ .text-muted+ p') %>% 
                   html_text() %>% 
                   {if(length(.) == 0) NA else .},
                 plot = html_nodes(.x, '.ratings-bar+ .text-muted') %>% 
                   html_text() %>% 
                   {if(length(.) == 0) NA else .}))
  
  imdb <- rbind(imdb, movies)
  
  print(paste("Page:", page_result))
}


### Service- Created a column with the type of streaming service.
imdb['Service'] = 'Netflix'


####### Disney+
movies = data.frame()
disney = data.frame()

for(page_result in seq(from =1, to= 2891., by= 50)) {
  
  link= paste0("https://www.imdb.com/search/title/?companies=co0721120&start=",page_result,"&ref_=adv_nxt")
  
  page = read_html(link)
  
  movies <- page %>% 
    html_nodes('.lister-item-content') %>% 
    map_df(~list(rank = html_nodes(.x, '.text-primary') %>% 
                   html_text() %>% 
                   {if(length(.) == 0) NA else .},
                 title = html_nodes(.x, '.lister-item-header a') %>% 
                   html_text() %>% 
                   {if(length(.) == 0) NA else .},
                 year = html_nodes(.x, '.text-muted.unbold') %>% 
                   html_text() %>% 
                   {if(length(.) == 0) NA else .},
                 rating = html_nodes(.x, '.ratings-imdb-rating strong') %>% 
                   html_text() %>% 
                   {if(length(.) == 0) NA else .},
                 votes = html_nodes(.x, '.sort-num_votes-visible span:nth-child(2)') %>% 
                   html_text() %>% 
                   {if(length(.) == 0) NA else .},
                 genre = html_nodes(.x, '.genre') %>% 
                   html_text() %>% 
                   {if(length(.) == 0) NA else .},
                 tv_rating = html_nodes(.x, '.certificate') %>% 
                   html_text() %>% 
                   {if(length(.) == 0) NA else .},
                 runtime = html_nodes(.x, '.runtime') %>% 
                   html_text() %>% 
                   {if(length(.) == 0) NA else .},
                 director = html_nodes(.x, '.text-muted~ .text-muted+ p , .ratings-bar~ .text-muted+ p') %>% 
                   html_text() %>% 
                   {if(length(.) == 0) NA else .},
                 plot = html_nodes(.x, '.ratings-bar+ .text-muted') %>% 
                   html_text() %>% 
                   {if(length(.) == 0) NA else .}))
  
  disney <- rbind(disney, movies)
  
  print(paste("Page:", page_result))
}

disney['Service'] = 'Disney'

### Combine Disney with IMDB (netflix)
imdb <- rbind(imdb, disney)


####### Amazon
movies = data.frame()
amazon = data.frame()

for(page_result in seq(from =1, to= 3641., by= 50)) {
  
  link= paste0("https://www.imdb.com/search/title/?companies=co0476953&start=",page_result,"&ref_=adv_nxt")
  
  page = read_html(link)
  
  movies <- page %>% 
    html_nodes('.lister-item-content') %>% 
    map_df(~list(rank = html_nodes(.x, '.text-primary') %>% 
                   html_text() %>% 
                   {if(length(.) == 0) NA else .},
                 title = html_nodes(.x, '.lister-item-header a') %>% 
                   html_text() %>% 
                   {if(length(.) == 0) NA else .},
                 year = html_nodes(.x, '.text-muted.unbold') %>% 
                   html_text() %>% 
                   {if(length(.) == 0) NA else .},
                 rating = html_nodes(.x, '.ratings-imdb-rating strong') %>% 
                   html_text() %>% 
                   {if(length(.) == 0) NA else .},
                 votes = html_nodes(.x, '.sort-num_votes-visible span:nth-child(2)') %>% 
                   html_text() %>% 
                   {if(length(.) == 0) NA else .},
                 genre = html_nodes(.x, '.genre') %>% 
                   html_text() %>% 
                   {if(length(.) == 0) NA else .},
                 tv_rating = html_nodes(.x, '.certificate') %>% 
                   html_text() %>% 
                   {if(length(.) == 0) NA else .},
                 runtime = html_nodes(.x, '.runtime') %>% 
                   html_text() %>% 
                   {if(length(.) == 0) NA else .},
                 director = html_nodes(.x, '.text-muted~ .text-muted+ p , .ratings-bar~ .text-muted+ p') %>% 
                   html_text() %>% 
                   {if(length(.) == 0) NA else .},
                 plot = html_nodes(.x, '.ratings-bar+ .text-muted') %>% 
                   html_text() %>% 
                   {if(length(.) == 0) NA else .}))
  
  amazon <- rbind(amazon, movies)
  
  print(paste("Page:", page_result))
}

amazon['Service'] = 'Amazon'

### Combine Amazon with IMDB
imdb <- rbind(imdb, amazon)

#original data saved
write.csv(imdb,"imdb_strm.csv")

### DATA CLEANING

### Year- separated year into two columns. Start year and end year.  
imdb <- imdb %>% separate(year, into= c("start_year", "end_year"), sep = '–')
imdb$start_year[imdb$start_year==""] <- NA
imdb$start_year <- gsub("(I)","",imdb$start_year,ignore.case=F)
imdb$start_year <- gsub("(II)","",imdb$start_year,ignore.case=F)
imdb$start_year <- gsub("(III)","",imdb$start_year,ignore.case=F)
imdb$start_year <- gsub("(X)","",imdb$start_year,ignore.case=F)
imdb$start_year <- gsub("(XL)","",imdb$start_year,ignore.case=F)
imdb$start_year <- gsub("(XX)","",imdb$start_year,ignore.case=F)
imdb$start_year <- gsub("(L)","",imdb$start_year,ignore.case=F)
imdb$start_year <- gsub("(TV Movie)","",imdb$start_year,ignore.case=F)
imdb$start_year <- gsub("(TV Special)","",imdb$start_year,ignore.case=F)
imdb$start_year <- gsub("(Video)","",imdb$start_year,ignore.case=F)
imdb$start_year <- gsub(" Podcast Series","",imdb$start_year,ignore.case=F)
imdb$start_year <- gsub(" TV Short","",imdb$start_year,ignore.case=F)
imdb$start_year <- gsub("(V)","",imdb$start_year,ignore.case=F)
imdb$start_year <- gsub("\\()","",imdb$start_year,ignore.case=F)
imdb$start_year <- gsub("\\(I)","",imdb$start_year,ignore.case=F)
imdb$start_year <- gsub("\\(","",imdb$start_year,ignore.case=F)
imdb$start_year <- gsub("\\)","",imdb$start_year,ignore.case=F)
imdb$start_year <- trimws(imdb$start_year)

### Utilizing gsub to substitute certain characters with blank
imdb$end_year <- gsub("(I)","",imdb$end_year,ignore.case=F)
imdb$end_year <- gsub("(II)","",imdb$end_year,ignore.case=F)
imdb$end_year <- gsub("(III)","",imdb$end_year,ignore.case=F)
imdb$end_year <- gsub("(X)","",imdb$end_year,ignore.case=F)
imdb$end_year <- gsub("(XL)","",imdb$end_year,ignore.case=F)
imdb$end_year <- gsub("(XX)","",imdb$end_year,ignore.case=F)
imdb$end_year <- gsub("(L)","",imdb$end_year,ignore.case=F)
imdb$end_year <- gsub("(TV Movie)","",imdb$end_year,ignore.case=F)
imdb$end_year <- gsub("(TV Special)","",imdb$end_year,ignore.case=F)
imdb$end_year <- gsub("(Video)","",imdb$end_year,ignore.case=F)
imdb$end_year <- gsub(" Podcast Series","",imdb$end_year,ignore.case=F)
imdb$end_year <- gsub(" TV Short","",imdb$end_year,ignore.case=F)
imdb$end_year <- gsub("(V)","",imdb$end_year,ignore.case=F)
imdb$end_year <- gsub("\\()","",imdb$end_year,ignore.case=F)
imdb$end_year <- gsub("\\(I)","",imdb$end_year,ignore.case=F)
imdb$end_year <- gsub("\\(","",imdb$end_year,ignore.case=F)
imdb$end_year <- gsub("\\)","",imdb$end_year,ignore.case=F)
imdb$end_year <- trimws(imdb$end_year)
imdb$end_year[imdb$end_year==""] <- NA

### Genre- Created three columns to divide the type of genres.
imdb <- imdb %>% separate(genre, into= c("genre", "genre_two", "genre_three"), sep = ',')
imdb$genre <- trimws(imdb$genre)
imdb$genre_two <- trimws(imdb$genre_two)
imdb$genre_three <- trimws(imdb$genre_three)

### Runtime - Deleted min from runtime to transfrom the column into a numeric column. 
imdb$runtime <- gsub(" min", "", imdb$runtime, ignore.case=F)
imdb$runtime <- as.numeric(imdb$runtime)

### Votes - Transformed the votes column from character to a numeric column.
imdb$votes <- as.numeric(gsub("," ,"", imdb$votes))

### Rating - Transformed the rating column into a double data type
imdb$rating <- as.double(imdb$rating)

### Movie or Series- Created a column where if the runtime is greated than 60 min
### the type will be Movie.
imdb<- imdb %>% mutate(type =
                                case_when(runtime <= 60 ~ "Series", 
                                          runtime > 60 ~ "Movie"))

### Director - Divided the column into Director and Stars 
imdb$director <- gsub("Star:","Stars:",imdb$director,ignore.case=F)
imdb <- imdb %>% separate(director, into= c("director", "stars"), sep = 'Stars:')
imdb$director <- gsub("Director:","",imdb$director,ignore.case=F)
imdb$director <- gsub("Directors:","",imdb$director,ignore.case=F)
imdb$director <- gsub("\\|","",imdb$director,ignore.case=F)
imdb$director <- trimws(imdb$director)
imdb$director[imdb$director==""] <-NA

### Stars - Stars were divided into three different columns.
imdb <- imdb %>% separate(stars, into= c("leading_star", "supp_star","third_star"), sep = ',')
imdb$supp_star <- trimws(imdb$supp_star)
imdb$third_star <- trimws(imdb$third_star)

### Plot
imdb$plot <- trimws(imdb$plot)
imdb$plot[imdb$plot=="Add a Plot"] <-NA

### Rank
imdb <- imdb[imdb$rank != "Episode:",]

write.csv(imdb,"imdb_strm_clean.csv")
