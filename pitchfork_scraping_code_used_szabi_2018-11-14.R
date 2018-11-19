rm(list=ls())

library(rvest)
library(data.table)
library(jsonlite)
library(httr)
library(ggplot2)

page_count <- 1000
album_count <- page_count * 12

get_index_search_review <- function(url){
  
  my_page <- read_html(url)
  
  review_link<- 
    my_page%>%
    html_nodes('.review__link')%>%
    html_attr('href')

    return(review_link)
}

page_number <- c(seq(1,page_count,1))

page_link <- rep("https://pitchfork.com/reviews/albums/?page=", length(page_number))

all_page_links <- paste0(page_link, page_number)

##pitchfork_data_links <- get_index_search_review(all_page_links[1])

pitchfork_final_table <- NULL

for(i in 1:page_count){
  pitchfork_data_links <- get_index_search_review(all_page_links[i])
  pitchfork_final_table <- c(pitchfork_final_table, pitchfork_data_links)
}

full_link_list <- unique(pitchfork_final_table)

link_begin <- rep("https://pitchfork.com", length(full_link_list))

indiv_links <- paste0(link_begin, full_link_list)

get_index_search_working <- function(url){
  
  my_page <- read_html(url)

  artist<-
    my_page%>%
    html_nodes('.review__title-artist')%>%
    html_text()

  album_name<-
    my_page%>%
    html_nodes('.review__title-album')%>%
    html_text()
  
  date<-
    my_page%>%
    html_nodes('.pub-date')%>%
    html_text()
  
  t <- data.frame('artist'= artist, 'album_name' = album_name, 'date' = date, stringsAsFactors = F)
  return(t)
}

pitchfork_data_aggreg <- NULL

for(i in 1:page_count){
  pitchfork_data_aggreg_prep <- get_index_search_working(all_page_links3[i])
  pitchfork_data_aggreg <- rbind(pitchfork_data_aggreg, pitchfork_data_aggreg_prep)
}

page_count_bnm <- 50
page_number_bnm <- c(seq(1,page_count_bnm,1))

page_link_bnm <- rep("https://pitchfork.com/reviews/best/albums/?page=", length(page_number_bnm))

all_page_links_bnm <- paste0(page_link_bnm, page_number_bnm)

pitchfork_data_aggreg_bnm <- NULL

for(i in 1:page_count_bnm){
  pitchfork_data_aggreg_prep_bnm <- get_index_search_working(all_page_links_bnm[i])
  pitchfork_data_aggreg_bnm <- rbind(pitchfork_data_aggreg_bnm, pitchfork_data_aggreg_prep_bnm)
}
head(pitchfork_data_aggreg_bnm)




?left_join
#get_index_search_2 <- function(url){
  
#  my_page <- read_html(url)
  
#  p_score<-
 #   my_page%>%
  #  html_node('.score')%>%
   # html_text()
  
 # t <- data.frame('p_score'= p_score, stringsAsFactors = F)

 # return(t)
# }

# pitchfork_score <-data.frame(matrix(1:album_count,ncol=1))

# for(i in 1:album_count){
# pitchfork_score [i] <- get_index_search_2(c[i])
# }
get_index_search_3 <- function(url){
  
  my_page <- read_html(url)
  
  artist<-
    my_page%>%
    html_node('.single-album-tombstone__artist-links')%>%
    html_text()  
  
  album_name<-
    my_page%>%
    html_node('.single-album-tombstone__review-title')%>%
    html_text()  
  
  date<-
    my_page%>%
    html_node('.pub-date')%>%
    html_text() 

  p_score<-
    my_page%>%
    html_node('.score')%>%
    html_text()
  
  genre<-
    my_page%>%
    html_node('.genre-list__link')%>%
    html_text()

  release_year<-
    my_page%>%
    html_node('.single-album-tombstone__meta-year')%>%
    html_text()   
  
  author_name<-
    my_page%>%
    html_node('.authors-detail__display-name')%>%
    html_text()
  
  author_title<-
    my_page%>%
    html_node('.authors-detail__title')%>%
    html_text()
  
  label_name<-
    my_page%>%
    html_node('.labels-list__item')%>%
    html_text
  
  t <- data.frame('artist' = artist, 'album_name' = album_name, 'date' = date,  'p_score'= p_score, 'genre' = genre, 'release_year' = release_year, 'author_name' = author_name, 'author_title' = author_title, 'label_name' = label_name, stringsAsFactors = F)
  
  return(t)
}

start_time <- Sys.time()
pitchfork_score_post <- NULL
pitchfork_score_prep <- NULL

head(all_page_links)

for(i in 1:album_count){
  pitchfork_score_prep <- tryCatch(get_index_search_3(indiv_links[i]), error = function(e) c(0,0,0,0,0,0))
  pitchfork_score_post <- rbind(pitchfork_score_post, pitchfork_score_prep)
}
end_time <- Sys.time()
end_time - start_time
#tryCatch(get_index_search_3(list_3to4_5[1]), error = function(e) c(0,0,0,0,0,0,0,0,0))

#list_3to4_5[1]

results_matrix <- cbind(pitchfork_score_post, indiv_links)

colnames(results_matrix) <- c("artist", "album", "date", "score", "genre", "release_year", "author_name", "author_title", "label_name", "link")

pitchfork_data_aggreg <- as.data.table(pitchfork_data_aggreg)

pitchfork_data_aggreg$id <- c(seq(1,length(pitchfork_data_aggreg$artist),1))
results_matrix$id <- c(seq(1,length(results_matrix$link),1))

pitchfork_final <- merge(pitchfork_data_aggreg, results_matrix, by = 'id')

######################

pitchfork_final$score <- as.numeric(pitchfork_final$score)
pitchfork_final$genre <- as.factor(pitchfork_final$genre)
pitchfork_final$release_year <- as.numeric(sub("• ","", pitchfork_final$release_year))
pitchfork_final[, quality := cut(score, c(0, 7, 8, Inf), labels = c('bad', 'average', 'good'))]

###############################################################################################
pitchfork_final[score == 0, ,]
head(pitchfork_final)

length(unique(pitchfork_data_aggreg$artist))

head(pitchfork_data_aggreg[, .N, by = artist][order(-N)], 20)

tail(pitchfork_final)
pitchfork_final[, .N, by = author_name]

ggplot(pitchfork_final, aes(x = score)) + geom_histogram(binwidth = 0.2) + facet_wrap(~genre, nrow = 1)

ggplot(pitchfork_final, aes(x = genre, fill = quality)) + 
  geom_bar(position = 'fill') + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(pitchfork_final, aes(x = genre)) + 
  geom_bar() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(pitchfork_final, aes(x = score)) + 
  geom_histogram(binwidth = 0.5) + 
  geom_vline(xintercept = mean(pitchfork_final$score), color = 'red')

ggplot(pitchfork_final, aes(x = score, fill = genre)) + 
  geom_density(alpha = 0.25)

mean(pitchfork_final$score)
median(pitchfork_final$score)

#summary(pitchfork_final$score)
#pitchfork_final[id == 240, link, ]
pitchfork_extract <- pitchfork_final[, .N, by = .(genre, quality)][order(genre, quality)]

pitchfork_extract <- pitchfork_extract[, sub_count := sum(N), by = genre]

p_right_table <- pitchfork_extract[, sum(N), by = genre]

p_extract_final <- merge(pitchfork_extract, p_right_table, by = 'genre')

p_extract_final$percentage <- p_extract_final$N / p_extract_final$sub_count

p_extract_final

pitchfork_final[score < 7, .N, ]
pitchfork_final[score > 8, .N, ]
pitchfork_final[score == 10, ,]

write.csv(pitchfork_final, 'pitchfork_final.csv',row.names = FALSE)

pitchfork_final[score >= 9, .(artist, album_name)]

#.single-album-tombstone__meta-year


head(c)
list_1to3k <- c[1:3000]
list_3kto6k <- c[3001:6000]
list_6kto9k <- c[6001:9000]
list_9kto12k <- c[9001:12000]

all_page_links <- read.csv('~/ggplot_tutorial/pitchfork_pagelist.csv')
all_page_links <- unlist(all_page_links)

alpha <- read.csv('~/ggplot_tutorial/pitchfork_1to3k.csv')
beta <- read.csv('~/ggplot_tutorial/pitchfork_3kto6k.csv')
gamma <- read.csv('~/ggplot_tutorial/pitchfork_6kto9k.csv')
delta <- read.csv('~/ggplot_tutorial/pitchfork_9kto12k.csv')
indiv_links <- rbind(c(alpha, beta, gamma, delta))
indiv_links <- unlist(indiv_links)
indiv_links <- as.character(indiv_links)

list_0to1_5 <- indiv_links[1:1500]
list_1_5to3 <- indiv_links[1501:3000]
list_3to4_5 <- indiv_links[3001:4500]
list_4_5to6 <- indiv_links[4501:6000]
list_6to7_5 <- indiv_links[6001:7500]
list_7_5to9 <- indiv_links[7501:9000]
list_9to10_5 <- indiv_links[9001:10500]
list_10_5to12 <- indiv_links[10501:12000]

all_page_links1 <- all_page_links[1:125]
all_page_links2 <- all_page_links[126:250]
all_page_links3 <- all_page_links[251:375]
all_page_links4 <- all_page_links[376:500]
all_page_links5 <- all_page_links[501:625]
all_page_links6 <- all_page_links[626:750]
all_page_links7 <- all_page_links[751:875]
all_page_links8 <- all_page_links[876:1000]

all_page_links1 <- as.character(all_page_links1)
all_page_links2 <- as.character(all_page_links2)
all_page_links3 <- as.character(all_page_links3)
all_page_links4 <- as.character(all_page_links4)
all_page_links5 <- as.character(all_page_links5)
all_page_links6 <- as.character(all_page_links6)
all_page_links7 <- as.character(all_page_links7)
all_page_links8 <- as.character(all_page_links8)

list_0to1_5 <- as.character(list_0to1_5)
list_1_5to3 <- as.character(list_1_5to3)
list_3to4_5 <- as.character(list_3to4_5)
list_4_5to6 <- as.character(list_4_5to6)
list_6to7_5 <- as.character(list_6to7_5)
list_7_5to9 <- as.character(list_7_5to9)
list_9to10_5 <- as.character(list_9to10_5)
list_10_5to12 <- as.character(list_10_5to12)

write.csv(list_1to3k, 'pitchfork_1to3k.csv', row.names = FALSE)
write.csv(list_3kto6k, 'pitchfork_3kto6k.csv', row.names = FALSE)
write.csv(list_6kto9k, 'pitchfork_6kto9k.csv', row.names = FALSE)
write.csv(list_9kto12k, 'pitchfork_9kto12k.csv', row.names = FALSE)
write.csv(b, 'pitchfork_pagelist.csv', row.names = FALSE)
write.csv(pitchfork_final, 'pitchfork_final_1to3k.csv',row.names = FALSE)

head(list_3kto6k)
head(list_6kto9k)
tail(full_link_list)
pitchfork_final_1to3k <- pitchfork_final


## 43 to 148
?tryCatch

tail(results_matrix)
write.csv(results_matrix, 'pitchfork_final_results.csv', row.names = FALSE)
results_matrix_dt <- data.table(results_matrix)
results_matrix_dt <- results_matrix_dt[score != 0, ,]
results_matrix_dt$release_year <- as.numeric(sub("• ","", results_matrix_dt$release_year))
unique(results_matrix_dt$release_year)
results_matrix_dt[, .N, by = release_year][order(-release_year)]
results_matrix_dt <- results_matrix_dt[release_year >= 2008]
str(results_matrix_dt)
results_matrix_dt$score <- as.numeric(results_matrix_dt$score)
results_matrix_dt[, mean(score), by = .(release_year, genre)][order(-release_year)]
results_matrix_dt[, .N, by = genre]
length(unique(results_matrix_dt$artist))


ggplot(results_matrix_dt, aes(x = score, fill = genre)) + geom_density()

head(results_matrix_dt)
results_matrix_dt[album == "Honey", ,]
bnm_stats <- data.table(pitchfork_data_aggreg_bnm)
bnm_stats[, bnm := TRUE,]

head(bnm_stats)

bnm_stats[, id := paste0(artist, album_name, date), ]
results_matrix_dt[, id := paste0(artist, album, date), ]

results_matrix_bnm <- left_join(results_matrix_dt, bnm_stats, by = 'id')

str(results_matrix_bnm)
results_matrix_bnm <- data.table(results_matrix_bnm)
results_matrix_bnm[, c("artist.y", "album_name", "date.y") := NULL, ]
colnames(results_matrix_bnm) <- c("artist", "album", "review_date", "score", "genre", "release_year", "author_name", "author_title", "label_name", "link", "id", "bnm")

results_matrix_bnm[, .N, by = bnm]
results_matrix_bnm[, number_of_albums := .N, by = .(artist)]

write.csv(results_matrix_bnm, 'pitchfork_final_bnm_pre_clean.csv', row.names = FALSE)

results_matrix_bnm <- results_matrix_bnm[release_year >= 2009]
results_matrix_bnm <- results_matrix_bnm[genre != "NA", ,]
results_matrix_bnm[, number_of_albums := .N, by = .(artist)]

results_matrix_bnm2 <- results_matrix_bnm
results_matrix_bnm[is.na(bnm) == TRUE, bnm := FALSE,]

head(results_matrix_bnm2)

install.packages('lubridate')
library(lubridate)
mdy('October 29 2018')
results_matrix_bnm[, review_date := mdy(review_date), ]
head(results_matrix_bnm)
write.csv(results_matrix_bnm, 'pitchfork_final_bnm_post_clean.csv', row.names = FALSE)

results_matrix_bnm[score > 9, .N, by = artist]

results_matrix[which(results_matrix$artist == "Nirvana"),]
results_matrix_dt[album %like% "Edition", ,]
results_matrix_bnm2[artist == "Nirvana"]
results_matrix_bnm3 <- results_matrix_bnm[!(album %like% "Edition"), ,]
results_matrix_bnm3 <- results_matrix_bnm3[!(album %like% "Reissue"), ,]
write.csv(results_matrix_bnm3, 'pitchfork_final_bnm_post_clean3.csv', row.names = FALSE)

results_matrix_bnm3 <- results_matrix_bnm3[!(album %like% "Anniversary"), ,]
results_matrix_bnm3 <- results_matrix_bnm3[!(album %like% "Edit"), ,]

pwd()
