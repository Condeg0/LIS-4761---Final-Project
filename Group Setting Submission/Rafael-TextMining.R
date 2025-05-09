
# ===== Load libraries =====
library(quantmod)
library(dplyr)
library(lubridate)
library(tm)
library(slam)
library(wordcloud)
library(syuzhet)
library(ggplot2)
library(lmtest)
library(ggcorrplot)
library(quanteda)


# ===== Load & Read Data =====
# Get tweet data 
tweet_data <- read.csv("Data/Nvidia-Tweets.csv")
tweet_data <- na.omit(tweet_data)  # Remove NA values

# Remove duplicate tweets based on text
tweet_data <- tweet_data %>%
  distinct(Text, .keep_all = TRUE)

# Format date column
tweet_data$Day <- as.Date(tweet_data$Datetime)
tweet_data$Time <- format(as.POSIXct(tweet_data$Datetime), "%H:%M:%S")
tweet_data$Hour <- hour(ymd_hms(tweet_data$Datetime))


# Get stock price data 
start <- min(tweet_data$Day)
end <- max(tweet_data$Day)
nvda_data <- getSymbols("NVDA", from = start, to = end, auto.assign = F)

nvda_data$LogReturn <- dailyReturn(nvda_data, type = "log")
nvda_data$AReturn <- dailyReturn(nvda_data, type = "arithmetic")
nvda_data <- as.data.frame(nvda_data)
nvda_data$Date <- as.Date(rownames(nvda_data))





# Text processing & text mining
# Remove unecessary columns from tweet data
tweet_data <- tweet_data[ , c("Text", "Day")]

# Create corpus and tokens
corpus <- corpus(tweet_data$Text)
tokens <- tokens(corpus, remove_punct = TRUE, remove_symbols = TRUE)
tokens <- tokens_tolower(tokens)
tokens <- tokens_remove(tokens, stopwords("en"))

# Create document-feature matrix (DFM)
dfm <- dfm(tokens)
word_freq <- data.frame(topfeatures(dfm, 1000))

clean_text <- sapply(tokens, function(x) paste(x, collapse = " "))
tweet_data$CleanText <- clean_text


# ===== Sentiment Analysis =====
# Give a score to each tweet
tweet_data$Sentiment <- get_sentiment(tweet_data$CleanText, method = "afinn")

# Get sentiment score and tweet volume
df <- tweet_data %>%
  mutate(SentimentType = case_when(
    Sentiment > 0 ~ "positive",
    Sentiment < 0 ~ "negative",
    TRUE ~ "neutral"
  )) %>%
  group_by(Day) %>%
  summarize(
    MeanSentiment = mean(Sentiment),
    TweetVolume = n(),
    Positive = sum(SentimentType == "positive"),
    Negative = sum(SentimentType == "negative"),
    Neutral = sum(SentimentType == "neutral"),
    NegPosRatio = ifelse(Positive > 0, Negative / Positive, NA),
    PosRatio = ifelse(Positive > 0, Positive / TweetVolume, NA),
    NegRatio = ifelse(Negative > 0, Negative / TweetVolume, NA)) 

# Merge text mining data and stock data
merged_data <- merge(x = nvda_data, y = df, by.x = "Date", by.y = "Day", all.y = T)

merged_data <- merged_data %>%
  mutate(
    LaggedSentiment = lag(MeanSentiment, 1),
    Volatility = abs(AReturn),  # absolute return as proxy
    LaggedAReturn = lag(AReturn, 1),
    LaggedVolatility = lag(Volatility, 1),
    LaggedNVDA_Volume = lag(NVDA.Volume, 1),
    LaggedTVolume = lag(TweetVolume, 1)
  )
