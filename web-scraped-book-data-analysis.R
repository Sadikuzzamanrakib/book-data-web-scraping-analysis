library(rvest)
url <- "https://books.toscrape.com/"
webpage <- read_html(url)

titles <- webpage %>%
  html_elements("h3 a") %>%
  html_attr("title")
titles


prices <- webpage %>%
  html_elements(".price_color") %>%
  html_text2()
prices


availability <- webpage %>%
  html_elements(".availability") %>%
  html_text2()
availability


books_df <- data.frame(
  Title = titles,
  Price = prices,
  Availability = availability,
  stringsAsFactors = FALSE
)

# View first few rows
head(books_df)


library(rvest)

all_titles <- c()
all_prices <- c()
all_availability <- c()

https://books.toscrape.com/catalogue/page-1.html
https://books.toscrape.com/catalogue/page-2.html
...
https://books.toscrape.com/catalogue/page-50.html



for (page in 1:50) {
  
  # Create page URL
  url <- paste0(
    "https://books.toscrape.com/catalogue/page-",
    page,
    ".html"
  )
  
  # Read HTML
  webpage <- read_html(url)
  
  # Extract titles
  titles <- webpage %>%
    html_elements("h3 a") %>%
    html_attr("title")
  
  # Extract prices
  prices <- webpage %>%
    html_elements(".price_color") %>%
    html_text2()
  
  # Extract availability
  availability <- webpage %>%
    html_elements(".availability") %>%
    html_text2()
  
  # Store data
  all_titles <- c(all_titles, titles)
  all_prices <- c(all_prices, prices)
  all_availability <- c(all_availability, availability)
}


books_df <- data.frame(
  Title = all_titles,
  Price = all_prices,
  Availability = all_availability,
  stringsAsFactors = FALSE
)


# Number of books (should be 1000)
nrow(books_df)

# View first few rows
head(books_df)

# View last few rows
tail(books_df)



library(ggplot2)
library(dplyr)

#Convert Price to Numeric
books_df$Price <- as.numeric(
  gsub("£", "", books_df$Price)
)
#Clean Availability Column
books_df$Availability <- trimws(books_df$Availability)


# Dataset structure
str(books_df)

# First few rows
head(books_df)

# Dimensions of data
dim(books_df)

summary(books_df)


#Price‑Specific Statistics
mean(books_df$Price)
median(books_df$Price)
min(books_df$Price)
max(books_df$Price)
sd(books_df$Price)


#Histogram of Book Prices
ggplot(books_df, aes(x = Price)) +
  geom_histogram(
    binwidth = 5,
    fill = "skyblue",
    color = "black"
  ) +
  labs(
    title = "Distribution of Book Prices",
    x = "Price (£)",
    y = "Frequency"
  )

#Boxplot of Book Prices
ggplot(books_df, aes(y = Price)) +
  geom_boxplot(fill = "lightgreen") +
  labs(
    title = "Boxplot of Book Prices",
    y = "Price (£)"
  )

#Availability Count (Bar Chart)
ggplot(books_df, aes(x = Availability)) +
  geom_bar(fill = "orange") +
  labs(
    title = "Book Availability Status",
    x = "Availability",
    y = "Count"
  )


#Top 10 Most Expensive Books
books_df %>%
  arrange(desc(Price)) %>%
  head(20) %>%
  ggplot(aes(
    x = reorder(Title, Price),
    y = Price
  )) +
  geom_col(fill = "red") +
  coord_flip() +
  labs(
    title = "Top 20 Most Expensive Books",
    x = "Book Title",
    y = "Price (£)"
  )


# Count of books per availability category
table(books_df$Availability)

# Price distribution summary
quantile(books_df$Price)




library(stringr)

ratings_raw <- c()

for (page in 1:50) {
  
  url <- paste0(
    "https://books.toscrape.com/catalogue/page-",
    page,
    ".html"
  )
  
  webpage <- read_html(url)
  
  ratings <- webpage %>%
    html_elements(".star-rating") %>%
    html_attr("class")
  
  ratings_raw <- c(ratings_raw, ratings)
}


books_df$Star_Rating <- ratings_raw

books_df$Star_Rating <- str_replace(
  books_df$Star_Rating,
  "star-rating ",
  ""
)

books_df$Star_Rating <- recode(
  books_df$Star_Rating,
  "One"   = 1,
  "Two"   = 2,
  "Three" = 3,
  "Four"  = 4,
  "Five"  = 5
)

books_df$Star_Rating <- as.numeric(books_df$Star_Rating)


str(books_df$Star_Rating)
summary(books_df$Star_Rating)

#Summary Statistics for Star Ratings
mean(books_df$Star_Rating)
median(books_df$Star_Rating)
sd(books_df$Star_Rating)


#Bar Chart of Star Ratings Distribution
ggplot(books_df, aes(x = factor(Star_Rating))) +
  geom_bar(fill = "steelblue") +
  labs(
    title = "Distribution of Star Ratings",
    x = "Star Rating",
    y = "Number of Books"
  )


#Pie Chart of Star Ratings
rating_counts <- books_df %>%
  count(Star_Rating)

ggplot(rating_counts, aes(
  x = "",
  y = n,
  fill = factor(Star_Rating)
)) +
  geom_col(width = 1) +
  coord_polar("y") +
  labs(
    title = "Proportion of Star Ratings",
    fill = "Stars"
  )


#Boxplot of Price vs Star Rating
ggplot(books_df, aes(
  x = factor(Star_Rating),
  y = Price
)) +
  geom_boxplot(fill = "lightblue") +
  labs(
    title = "Book Price by Star Rating",
    x = "Star Rating",
    y = "Price (£)"
  )


#Average Price by Star Rating
books_df %>%
  group_by(Star_Rating) %>%
  summarise(
    Avg_Price = mean(Price)
  ) %>%
  ggplot(aes(
    x = factor(Star_Rating),
    y = Avg_Price
  )) +
  geom_col(fill = "darkgreen") +
  labs(
    title = "Average Book Price by Star Rating",
    x = "Star Rating",
    y = "Average Price (£)"
  )

#Cross‑Analysis (Ratings vs Availability)
table(books_df$Star_Rating, books_df$Availability)


#Summary Statistics (Price by Star Rating)

library(dplyr)

price_by_rating <- books_df %>%
  group_by(Star_Rating) %>%
  summarise(
    Count = n(),
    Mean_Price = mean(Price),
    Median_Price = median(Price),
    Min_Price = min(Price),
    Max_Price = max(Price)
  )

price_by_rating

#Boxplot: Price vs Star Rating

library(ggplot2)

ggplot(books_df, aes(
  x = factor(Star_Rating),
  y = Price
)) +
  geom_boxplot(fill = "lightblue") +
  labs(
    title = "Price Distribution by Star Rating",
    x = "Star Rating",
    y = "Price (£)"
  )

#Average Price per Star Rating
ggplot(price_by_rating, aes(
x = factor(Star_Rating),
y = Mean_Price
)) +
  geom_col(fill = "darkgreen") +
  labs(
    title = "Average Book Price by Star Rating",
    x = "Star Rating",
    y = "Average Price (£)"
  )


#Summary Statistics (Price by Availability)

price_by_availability <- books_df %>%
  group_by(Availability) %>%
  summarise(
    Count = n(),
    Mean_Price = mean(Price),
    Median_Price = median(Price)
  )

price_by_availability


#Boxplot: Price vs Availability

ggplot(books_df, aes(
  x = Availability,
  y = Price
)) +
  geom_boxplot(fill = "orange") +
  labs(
    title = "Book Price by Availability",
    x = "Availability",
    y = "Price (£)"
  )

#Cross‑Tabulation

rating_availability_table <- table(
  books_df$Star_Rating,
  books_df$Availability
)

rating_availability_table


#Stacked Bar Chart

ggplot(books_df, aes(
  x = factor(Star_Rating),
  fill = Availability
)) +
  geom_bar() +
  labs(
    title = "Availability Distribution Across Star Ratings",
    x = "Star Rating",
    y = "Number of Books"
  )

#Correlation Analysis (Numeric Attributes)

numeric_data <- books_df %>%
  select(Price, Star_Rating)

cor(numeric_data)


#Pairwise Visualization (Price & Rating)

pairs(
  numeric_data,
  main = "Pairwise Comparison of Numeric Attributes"
)

-------------------------------------------------------------------------------------------------------
  

  
#Data Preparation
#Before analysis, price and star rating values were cleaned and converted into numeric format.

  
library(dplyr)
library(ggplot2)
library(stringr)

# Clean price
books_df$Price <- as.numeric(gsub("£", "", books_df$Price))

# Clean availability
books_df$Availability <- trimws(books_df$Availability)


#Dataset Overview
str(books_df)
summary(books_df)
dim(books_df)
#>The dataset consists of 1000 observations.
#>Price and star rating are numeric variables.
#>Availability is a categorical variable.
#>No missing values were observed in key attributes.



#Analysis of Book Prices
#Summary Statistics
mean(books_df$Price)
median(books_df$Price)
min(books_df$Price)
max(books_df$Price)
sd(books_df$Price)
#>Most book prices fall between £20 and £50.
#>The mean and median are close, indicating moderate skewness.
#>A few high‑priced books create mild right‑skewness.



#Price Distribution
ggplot(books_df, aes(x = Price)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(
    title = "Distribution of Book Prices",
    x = "Price (£)",
    y = "Frequency"
  )
#>Prices are concentrated in the mid‑range.
#>Very few books are extremely cheap or expensive.



#Star Rating Analysis
#Star Rating Distribution
ggplot(books_df, aes(x = factor(Star_Rating))) +
  geom_bar(fill = "steelblue") +
  labs(
    title = "Distribution of Star Ratings",
    x = "Star Rating",
    y = "Number of Books"
  )
#>Most books receive 3 or 4 stars.
#>1‑star books are rare, suggesting generally positive ratings.



#Availability Analysis
ggplot(books_df, aes(x = Availability)) +
  geom_bar(fill = "orange") +
  labs(
    title = "Book Availability Status",
    x = "Availability",
    y = "Count"
  )
#>The vast majority of books are in stock.
#>Availability does not vary significantly across the dataset.



#Comparative Analysis of Attributes
#Price vs Star Rating
ggplot(books_df, aes(
  x = factor(Star_Rating),
  y = Price
)) +
  geom_boxplot(fill = "lightblue") +
  labs(
    title = "Book Price by Star Rating",
    x = "Star Rating",
    y = "Price (£)"
  )
#>Price ranges overlap heavily across ratings.
#>Higher star ratings do not guarantee higher prices.



#Average Price by Star Rating
books_df %>%
  group_by(Star_Rating) %>%
  summarise(Avg_Price = mean(Price)) %>%
  ggplot(aes(x = factor(Star_Rating), y = Avg_Price)) +
  geom_col(fill = "darkgreen") +
  labs(
    title = "Average Book Price by Star Rating",
    x = "Star Rating",
    y = "Average Price (£)"
  )
#>Average prices remain relatively stable across ratings.
#>Rating has weak influence on pricing.



#Price vs Availability
ggplot(books_df, aes(
  x = Availability,
  y = Price
)) +
  geom_boxplot(fill = "lightcoral") +
  labs(
    title = "Price Distribution by Availability",
    x = "Availability",
    y = "Price (£)"
  )
#>Availability status shows no strong price difference.
#>Stock availability is independent of price.



#Star Rating vs Availability
ggplot(books_df, aes(
  x = factor(Star_Rating),
  fill = Availability
)) +
  geom_bar() +
  labs(
    title = "Availability Across Star Ratings",
    x = "Star Rating",
    y = "Number of Books"
  )
#>Books of all ratings are mostly in stock.
#>No rating‑based bias in availability.



#Correlation Analysis (Numeric Variables)
cor(books_df %>% select(Price, Star_Rating))
#>Correlation is close to zero.
#>Confirms lack of linear relationship between price and rating.





#######Modeling########
library(dplyr)

#Encode Availability as Binary
books_df$Available_Binary <- ifelse(
  books_df$Availability == "In stock", 1, 0
)

#Train–Test Split
set.seed(123)

sample_index <- sample(
  1:nrow(books_df),
  size = 0.7 * nrow(books_df)
)

train_data <- books_df[sample_index, ]
test_data  <- books_df[-sample_index, ]


#Model 1: Linear Regression (Price)
#Model Training
price_model <- lm(
  Price ~ Star_Rating,
  data = train_data
)

#Model Summary
summary(price_model)
#The coefficient of Star_Rating measures how much price changes per unit increase in rating.
#A low R² indicates star rating alone explains a small portion of price variation.


#Model Testing & Predictions
price_predictions <- predict(
  price_model,
  newdata = test_data
)

#Performance Metrics (Regression)
rmse <- sqrt(mean((test_data$Price - price_predictions)^2))
mae  <- mean(abs(test_data$Price - price_predictions))

rmse
mae
#Metrics Used:
#RMSE (Root Mean Squared Error) – penalizes large errors.
#MAE (Mean Absolute Error) – average prediction error.

#Interpretation:
#High RMSE indicates substantial unexplained price variation.
#Confirms weak predictive power of star ratings alone.


#Logistic Regression (Availability)
#Model Training
availability_model <- glm(
  Available_Binary ~ Star_Rating + Price,
  data = train_data,
  family = "binomial"
)

# Model Summary
ummary(availability_model)


#Model Testing & Predictions
availability_prob <- predict(
  availability_model,
  newdata = test_data,
  type = "response"
)

availability_pred <- ifelse(
  availability_prob > 0.5, 1, 0
)

#Performance Metrics (Classification)
confusion_matrix <- table(
  Predicted = availability_pred,
  Actual = test_data$Available_Binary
)

confusion_matrix

#Accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
accuracy



















str(books_df$Price)

books_df$Price <- as.numeric(
  gsub("£", "", books_df$Price)
)
str(books_df$Price)


library(ggplot2)

ggplot(books_df, aes(x = Price)) +
  geom_histogram(
    binwidth = 5,
    fill = "skyblue",
    color = "black"
  ) +
  labs(
    title = "Distribution of Book Prices",
    x = "Book Price (£)",
    y = "Frequency"
  )




library(ggplot2)

ggplot(books_df, aes(x = Price)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(
    title = "Distribution of Book Prices",
    x = "Book Price (£)",
    y = "Frequency"
  )


ggplot(books_df, aes(y = Price)) +
  geom_boxplot(fill = "lightgreen") +
  labs(
    title = "Boxplot of Book Prices",
    y = "Book Price (£)"
  )


ggplot(books_df, aes(x = factor(Star_Rating))) +
  geom_bar(fill = "steelblue") +
  labs(
    title = "Distribution of Star Ratings",
    x = "Star Rating",
    y = "Number of Books"
  )



url <- paste0(
  "https://books.toscrape.com/catalogue/page-",
  page,
  ".html"
)

webpage <- read_html(url)

ratings <- webpage %>%
  html_elements(".star-rating") %>%
  html_attr("class")

ratings_raw <- c(ratings_raw, ratings)
}


books_df$Star_Rating <- ratings_raw

books_df$Star_Rating <- str_replace(
  books_df$Star_Rating,
  "star-rating ",
  ""
)

books_df$Star_Rating <- recode(
  books_df$Star_Rating,
  "One"   = 1,
  "Two"   = 2,
  "Three" = 3,
  "Four"  = 4,
  "Five"  = 5
)

books_df$Star_Rating <- as.numeric(books_df$Star_Rating)


library(rvest)
library(stringr)

ratings_raw <- c()

for (page in 1:50) {
  
  url <- paste0(
    "https://books.toscrape.com/catalogue/page-",
    page,
    ".html"
  )
  
  webpage <- read_html(url)
  
  ratings <- webpage %>%
    html_elements(".star-rating") %>%
    html_attr("class")
  
  ratings_raw <- c(ratings_raw, ratings)
}

length(ratings_raw)

books_df$Star_Rating <- ratings_raw

books_df$Star_Rating <- str_replace(
  books_df$Star_Rating,
  "star-rating ",
  ""
)

books_df$Star_Rating <- recode(
  books_df$Star_Rating,
  "One"   = 1,
  "Two"   = 2,
  "Three" = 3,
  "Four"  = 4,
  "Five"  = 5
)

books_df$Star_Rating <- as.numeric(books_df$Star_Rating)

colnames(books_df)
str(books_df$Star_Rating)


library(ggplot2)

ggplot(books_df, aes(x = factor(Star_Rating))) +
  geom_bar(fill = "steelblue") +
  labs(
    title = "Distribution of Star Ratings",
    x = "Star Rating",
    y = "Number of Books"
  )


# Linear regression predictions
price_predictions <- predict(
  price_model,
  newdata = test_data
)

# Regression metrics
RMSE <- sqrt(mean((test_data$Price - price_predictions)^2))
MAE  <- mean(abs(test_data$Price - price_predictions))
R2   <- summary(price_model)$r.squared

# Create results table
linear_results <- data.frame(
  Metric = c("RMSE", "MAE", "R-squared"),
  Value  = c(RMSE, MAE, R2)
)

linear_results


# Predict probabilities
availability_prob <- predict(
  availability_model,
  newdata = test_data,
  type = "response"
)

# Convert probabilities to class labels
availability_pred <- ifelse(
  availability_prob > 0.5, 1, 0
)


confusion_matrix <- table(
  Predicted = availability_pred,
  Actual = test_data$Available_Binary
)

confusion_matrix


# Extract values
TP <- confusion_matrix[2,2]
TN <- confusion_matrix[1,1]
FP <- confusion_matrix[2,1]
FN <- confusion_matrix[1,2]

accuracy  <- (TP + TN) / sum(confusion_matrix)
precision <- TP / (TP + FP)
recall    <- TP / (TP + FN)
f1_score  <- 2 * (precision * recall) / (precision + recall)

# Create results table
logistic_results <- data.frame(
  Metric = c("Accuracy", "Precision", "Recall", "F1-Score"),
  Value  = c(accuracy, precision, recall, f1_score)
)

logistic_results

