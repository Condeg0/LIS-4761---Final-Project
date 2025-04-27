library(tidyverse)
library(lubridate)
library(tidytext)
library(e1071)
library(caret)
library(quantmod)

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

# Preprocess Stock Data
nvda_data <- nvda_data %>%
  mutate(Date = ymd(Date)) %>%
  filter(Date >= ymd("2022-11-21"), Date <= ymd("2023-02-03")) %>%
  arrange(Date)



# Create target variable: direction of next-day closing price
nvda_data <- nvda_data %>%
  mutate(Close_lead = lead(NVDA.Close),
         Direction = if_else(Close_lead > NVDA.Close, "Up", "Down")) %>%
  na.omit()

## By creating a next-day target variable (Direction = Up/Down, comparing each day's close to the following day's close; Close_lead),
## this frames the problem as a binary classification: is it possible to predict tomorrow's move by the prior day's trends?


# Preprocess Tweet Data
# Convert datetime and filter range
tweet_data <- tweet_data %>%
  mutate(Datetime = ymd_hms(Datetime),
         Date = as_date(Datetime)) %>%
  filter(Date >= ymd("2022-11-21"), Date <= ymd("2023-02-03"))

## By filtering both stock and tweet data to the same date range (Nov 21, 2022 to Feb 3, 2023), it is ensured that
## every observation (each trading day) may carry its corresponding social-media sentiment.



# Sentiment Analysis using Bing lexicon
bing_lex <- get_sentiments("bing")

tweet_words <- tweet_data %>%
  unnest_tokens(word, Text) %>%
  anti_join(get_stopwords())

sentiment_scores <- tweet_words %>%
  inner_join(bing_lex, by = "word") %>%
  count(Datetime, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(sent_score = positive - negative)

# Aggregate sentiment per Date
daily_sentiment <- sentiment_scores %>%
  mutate(Date = as_date(Datetime)) %>%
  group_by(Date) %>%
  summarize(
    Pos = sum(positive, na.rm = TRUE),
    Neg = sum(negative, na.rm = TRUE),
    Sentiment = sum(sent_score, na.rm = TRUE)
  )

## By using the Bing lexicon, it is possible to tag each individual word as Positive or Negative
## then we can count these positive or negatives words by timestamp, then aggregating to a daily total of
## positive (pos) words, negative (neg) words, and the net sentiment (sentiment = Pos - Neg), allowing for
## day-by-day sentiment analysis.

# Merge Stock and Sentiment Data
model_data <- nvda_data %>%
  left_join(daily_sentiment, by = "Date") %>%
  replace_na(list(Pos = 0, Neg = 0, Sentiment = 0))

## We then merge the two datasets (NVDA stock data and Tweet Sentiment data) to allow for the creation of
## training and test sets for SVM modelling and analysis.

# Prepare training and test sets
set.seed(123)
train_index <- createDataPartition(model_data$Direction, p = 0.8, list = FALSE)
train <- model_data[train_index, ]
test  <- model_data[-train_index, ]

## The standard training/testing split for SVM modelling is 80/20 respectively, which is used here within
## the createDataPartition() function.

# Select features for SVM
features <- c("NVDA.Open", "NVDA.High", "NVDA.Low", "NVDA.Close", "NVDA.Volume", "NVDA.Adjusted", "Pos", "Neg", "Sentiment")

## This feature set allows for the SVM models to see both raw price and volume data as well as crowd sentiment to draw conclusions.

# Train SVM Model
svm_model <- svm(
  as.factor(Direction) ~ ., 
  data = train[, c(features, "Direction")],
  kernel = "linear",
  cost = 1,
  probability = TRUE
)

# Evaluate on Test Set
pred <- predict(svm_model, test[, features])
conf_mat <- confusionMatrix(pred, as.factor(test$Direction))
print(conf_mat)

## The linear kernal model is often used for when there is high-dimesnional data or when classes are linearly separable,
## making it a valid choice for this sentiment analysis (as the model data can see NDVA stock data when tied to public perception
## simplied into a binary system (positive or negative sentiment).

## The model's accurary can be summarized into four different attributes: Accurary, Sensitivity (Down), Senitivity (Up), and Balanced Accuracy.
## The data is as follows: 
# Accuracy : 0.4444
# Sensitivity (Down) : 0.00
# Specificity (Up) : 0.80
# Balanced Accuracy : 0.40

## As Accuracy is below the no-information rate of 56%, this means the model is doing worse than always guessing the majority class (Up).
## A Sensitivity of 0 shows that the model never predicts 'Down' correctly.
## A Specificity of 0.80 means that when the model goes Up, the model correctly predicts it 80% of the time; however, this can easily be explained by the model being heavily biased towards predicting Up.

# Tune Hyperparameters (optional)
tune_out <- tune(
  svm, as.factor(Direction) ~ ., data = train[, c(features, "Direction")],
  ranges = list(cost = c(0.1, 1, 10), gamma = c(0.01, 0.1, 1)),
  kernel = "radial"
)

best_model <- tune_out$best.model

# Re-evaluate with best model
best_pred <- predict(best_model, test[, features])
best_conf_mat <- confusionMatrix(best_pred, as.factor(test$Direction))
print(best_conf_mat)

## The radial kernel model is used when one wants to project into a larger dimensional space, allowing for
## analysis that stems beyond the finite range of the original data set and attempts to draw future conclusions
## from present data.

## As is the Linear model, the Radial model can be summarized into the same four attributes, with the data being as follows:
# Accuracy : 0.5556
# Sensitivity (Down) : 0.00
# Specificity (Up) : 1.00
# Balanced Accuracy : 0.50

## Unfortunately for this model, the Accuracy equals the no-information rate (56%), meaning that the model is simply predicting the majority class (Up) each time.
## The model never correctly predicts Down (Sensitivity = 0), but can predict Up 100% of the time (Specificity = 1.0); however, the model never chooses the Down label at all.

### What can be gathered from this preliminary analysis?
## As the tweet sentiments never have a Negative day, it is impossible for the model to correctly predict how NVDA stocks may trend, even if the stocks can be turbulent in some periods;
## constantly shifting between gaining and losing value as of their closing time.

## SVM modelling also treats every day as independent; however, in real world scenarios, stock prices typically follow momentum and volatility trends that may not be conceptualized by static predictions,
## especially when the main predictor is public perception of a stock that is notoriously garnered as "premier" by both normal everyday individuals and those who follow the stock markets for a living.

