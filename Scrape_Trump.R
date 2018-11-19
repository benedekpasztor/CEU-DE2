#my keys
keyTable <- read.csv("your access key location in csv", header = T)
AWS_ACCESS_KEY_ID <- as.character(keyTable$Access.key.ID)
AWS_SECRET_ACCESS_KEY <- as.character(keyTable$Secret.access.key)

#activate
Sys.setenv("AWS_ACCESS_KEY_ID" = AWS_ACCESS_KEY_ID,
           "AWS_SECRET_ACCESS_KEY" = AWS_SECRET_ACCESS_KEY,
           "AWS_DEFAULT_REGION" = "us-west-2") 
############################################################################

library('rvest')

dir <-  paste0(getwd(), "/")
source(paste0(dir, "theme_bg.R"))

url_fox <- 'https://www.foxnews.com/politics/trump-wins-presidency-defeats-clinton-in-historic-election-upset'
url_cnn <- 'https://edition.cnn.com/2016/11/08/politics/election-day-2016-highlights/index.html'

#Reading the HTML code from the website
webpage_fox <- read_html(url_fox)
webpage_cnn <- read_html(url_cnn)

text_data_html_fox <- html_nodes(webpage_fox,'.article-body')
text_data_html_cnn <- html_nodes(webpage_cnn,'.zn-body__paragraph')

#Converting to text
text_data_fox <- html_text(text_data_html_fox)
text_data_cnn <- html_text(text_data_html_cnn)

#Let's have a look at the text we got
head(text_data_fox)
head(text_data_cnn)


#There are still strange characters in our text like "\n\t\tAll newsletters\n\t".

#Data-Preprocessing: removing unwanted garbage
text_data_fox <- gsub("[\r\n\t]", "", text_data_fox)
text_data_cnn <- gsub("[\r\n\t]", "", text_data_cnn)

text_data_fox <- paste(text_data_fox, collapse =" ")
text_data_cnn <- paste(text_data_cnn, collapse =" ")

library(aws.comprehend)
library(dplyr)
library(ggplot2)

text_data_fox_1 <- substring(text_data_fox, 1, nchar(text_data_fox)/2)
text_data_fox_2 <- substring(text_data_fox, nchar(text_data_fox)/2, nchar(text_data_fox))

text_data_cnn_1 <- substring(text_data_cnn, 1, nchar(text_data_cnn)/2)
text_data_cnn_2 <- substring(text_data_cnn, nchar(text_data_cnn)/2, nchar(text_data_cnn))


sentiments_fox_1 <- detect_sentiment(text_data_fox_1)
sentiments_fox_2 <- detect_sentiment(text_data_fox_2)

sentiments_cnn_1 <- detect_sentiment(text_data_cnn_1)
sentiments_cnn_2 <- detect_sentiment(text_data_cnn_2)

sentiments_fox <- rbind(sentiments_fox_1, sentiments_fox_2)
sentiments_fox$Sentiment <- NULL
sentiments_fox <- colMeans(data.matrix(sentiments_fox))

sentiments_cnn <- data.frame(rbind(sentiments_cnn_1, sentiments_cnn_2))
sentiments_cnn$Sentiment <- NULL
sentiments_cnn <- colMeans(data.matrix(sentiments_cnn))

sentiments <- rbind(sentiments_fox, sentiments_cnn)

library(ggrepel)
ggplot(mds, aes(x = V1, y = V2, label = car)) + geom_text_repel()

ggplot(data.frame(sentiments), aes(Negative, Positive, label = c("Fox News", "CNN"))) +
  geom_text_repel(size = 6) +
  coord_cartesian(ylim = c(0, 0.14)) +
  ggtitle("Sentiments of articles on Trump's election from November 2016") +
  xlab("Negative sentiments") +
  ylab("Positive sentiments")+
  theme_bg()
  ggsave(paste0(dir, "Trump_CNNFox_Comparison.png"), width=10, height=7.5)

