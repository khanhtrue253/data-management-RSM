##Part 1
if (!requireNamespace("tidyRSS", quietly = TRUE)) {
  install.packages("tidyRSS")
}
library(tidyRSS)

if (!requireNamespace("sentimentr", quietly = TRUE)) {
  install.packages("sentimentr")
}
library(sentimentr)

if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
library(dplyr)

final_news <- data.frame()


##Part 2
#Repeat the following part with all search keywords (hereby referred to as X)

#Current search keyword is global economy (global%20economy%20). If you want to change keyword, change the above phrase in adress to "key%20word%20change".
#Note that words are separated by "%20" sign. So if you look for jet fuel it will be "jet%20fuel"
#For changing how long ago the news should be published, note the "when%3A30d". Change 30 to the number of previous days you prefer.
keyword <- "https://news.google.com/rss/search?q=global%20economy%20when%3A30d&hl=nl&gl=NL&ceid=NL%3Anl"

#Change X to your current keyword
news_about_keyword_X <- tidyfeed(
  keyword,
  clean_tags = TRUE,
  parse_dates = TRUE
)

#Change X to your current keyword
selected_news_from_keyword_X <- subset(news_for_keyword_X, select = c("item_title", "item_pub_date"))
View(selected_news_from_keyword_X)

#Found an article you do not like when scrapping keyword X? Look for the row index (number N).
#Change X to your current keyword
#Repeat this step until you are happy with your currated list of articles
selected_news_from_keyword_X <- selected_news_from_keyword_X[-N, ]

#Change X to your current keyword
selected_news_from_keyword_X$item_title <- sub(" - .*|\\| .*", "", selected_news_from_keyword_X$item_title)

#Add articles from current keyword X to final list of articles
#Change X to your current keyword
final_news <- rbind(final_news, selected_news_from_keyword_X)

##End of part 2


##Part 3
#Get sentiment score. The algorithm first calculates sentiment for each sentence in each title.
sentence_news_sentiment <- sentiment(final_news$item_title)

#We then group them together for all articles' sentiment score
news_sentiment <- sentence_news_sentiment |>
       group_by(element_id) |>
       summarise(avg_sentiment = mean(sentiment))

#And calculate aggregated sentiment score in the news
average_sentiment <- news_sentiment |>
  summarize(sentiment = mean(avg_sentiment))
average_sentiment
