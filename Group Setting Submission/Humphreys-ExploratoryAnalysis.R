# ====Libraries====
library(GGally) # Exploratory Analysis
library(zoo) # Exploratory Analysis
library(lubridate) # Exploratory Analysis
library(forecast) # Exploratory Analysis

# ===== Exploratory Analysis =====
  
# Histogram of Mean Sentiment
ggplot(merged_data, aes(x = MeanSentiment)) +
  geom_histogram(
    binwidth = 0.2,
    color = 'black',
    fill = 'lightblue',
    boundary = 0
  ) +
  labs(
    title = "Distribution of Daily Mean Sentiment",
    x = "Mean Sentiment Score",
    y = "Number of Days"
  ) +
  theme_minimal()


# Histogram of Daily Returns
ggplot(merged_data, aes(x = AReturn)) +
  geom_histogram(
    binwidth = 0.005,
    color = 'black',
    fill = 'lavender',
  ) +
  labs(
    title = "Distribution of Daily Arithmetic Returns",
    x = "Daily Returns",
    y = "Number of Days"
  ) +
  theme_minimal()

# Partial Autocorrelation Plot
ggPacf(
  merged_data$AReturn,
  lag.max = 20,
) +
  labs(
    title = "Partial Autocorrelation of Returns",
    x = "Lag (days)",
    y = "PACF"
  )


# Test Pairwise Correlations
# Tweets vs NVDA volumes
g_volumes <- cor(cor_data$TweetVolume, cor_data$NVDA.Volume)
print(cor.test(cor_data$TweetVolume, cor_data$NVDA.Volume))

#Mean Sentiment vs AReturn
g_sent_ret <- cor(cor_data$MeanSentiment, cor_data$AReturn)
print(cor.test(cor_data$MeanSentiment, cor_data$AReturn))


# Scatterplot Matrix
#Build a view of all the relationships between the numerical inputs
GGally::ggpairs(cor_data,
                title = "Scatterplot Matrix of Returns, Sentiment, Volume, etc.")


# Rolling Correlation
# Create zoo object
z <- zoo(
  cor_data[, c("MeanSentiment","AReturn")],
  order.by = merged_data$Date[ match( rownames(cor_data), rownames(merged_data) ) ]
)

# Create a 20 day pearson correlation coefficient
roll_g <- rollapply(z, width = 20,
                    FUN = function(x) cor(x[,1], x[,2]),
                    by.column = FALSE, align = "right", fill = NA)

# Compute the total amount of points
len <- length(roll_g)

# Choose 6 indeces
pos <- floor(seq(1, len, length.out = 6))

# Plot
plot(coredata(roll_r), type="l", xaxt="n",
     ylab="Corr(MeanSentiment, AReturn)",
     main="20-Day Rolling Correlation")
axis(1, at = pos,
     labels = format(index(roll_g)[pos], "%Y-%m"))

# Cross Correlation Function
# Use Cross Correlation Function to discover if sentiment leads returns
ccf(cor_data$MeanSentiment, cor_data$AReturn,
    lag.max = 5, main = "Cross-correlation: Sentiment vs Return (±5 days)")

# By Day of The Week
# Show the days of week for dates
merged_data$Wday <- wday(merged_data$Date, label=TRUE)

# Plot a boxplot of sentiment by weekdays
ggplot(merged_data, aes(x = Wday, y = MeanSentiment)) +
  geom_boxplot() +
  labs(title="Mean Sentiment by Weekday",
       y="Mean Sentiment")

# Plot a boxplot of returns by weekdays
ggplot(merged_data, aes(x = Wday, y = AReturn)) +
  geom_boxplot() +
  labs(title="AReturn by Weekday",
       y="Daily Return")

# Compute correlation matrix
cor_matrix <- cor(cor_data)

# Visualize correlations
ggcorrplot(cor_matrix, 
           method = "circle", 
           type = "lower", 
           lab = TRUE, 
           tl.cex = 10,
           colors = c("darkred", "white", "darkblue"),
           title = "Correlation Matrix of Sentiment, Volume & Returns")
