

################################################################
# Project 2
## Huyen Thi Thu Pham

library(dplyr)
library(tidytext)
library(textclean)
library(tm)
library(tidyr)
library(SnowballC)
library(fastcluster)
library(parallelDist)
library(wordcloud)
library(RColorBrewer)
library(textstem)
library(stringr)
library(tidyverse)
library(ggplot2)
library(corrplot)
library(GGally)

#---------------------------
load("property_data.RData")

# Inspect the dataset
summary(data)
colnames(data)

# Select relevant columns
data <- data %>%
  dplyr::select(-Page)

unique_types <- unique(data$type)
print(unique_types)

# Calculate the count of each unique property type
property_counts <- data %>%
  group_by(type) %>%       # Group by property type
  summarise(count = n())   # Count the number of occurrences of each type

# View the results
print(property_counts)

###  Distribution of Property Types ###
ggplot(property_counts, aes(x = reorder(type, -count), y = count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Distribution of Property Types", x = "Property Type", y = "Count") +
  theme_minimal()

unique_company <- unique(data$company_name)

# Calculate the count of each unique property type
company_counts <- data %>%
  group_by(company_name) %>%       # Group by property type
  summarise(count = n()) %>%    # Count the number of occurrences of each type
  arrange(desc(count)) 

# View the results
head(company_counts,10)

#-----------------------------

# Convert 'Price' to numeric and replace 'Price Undisclosed' prices with NA
data$price <- as.numeric(gsub("[^0-9]", "", data$price))

# handle prices where 'Price Undisclosed'
data$price[data$price == ""] <- NA  

# Check for missing values
sapply(data, function(x) sum(is.na(x)))

# Since price is crucial for analysis and prediction, remove rows with missing price
data <- data %>%
  dplyr::filter(!is.na(price))


# Calculate the median land_size by property type
median_land_size_by_type <- data %>%
  group_by(type) %>%
  summarise(median_land_size = median(land_size, na.rm = TRUE))
head(median_land_size_by_type)

# Impute missing land_size values based on the property type median
data <- data %>%
  left_join(median_land_size_by_type, by = "type") %>%
  mutate(
    land_size = if_else(is.na(land_size), median_land_size, land_size)
  ) %>%
  dplyr::select(-median_land_size)  # Remove the helper column

# verify
sapply(data, function(x) sum(is.na(x)))

# Format 'Sold time' to Date format and extract the year
library(lubridate)
data$sold_time <- dmy(data$sold_time)

## Extract the year and then order by year in increasing order
data <- data %>%
  mutate(
    month = month(sold_time, label = TRUE),
    year = year(sold_time)) %>%
  arrange(year)  # Order by year in ascending order

# Filter the dataset to include only House, Townhouse, Apartment and Unit
data_filtered <- data %>% filter(type %in% c("House", "Townhouse", "Unit", "Apartment"))


# Extract street name from the location field
data_filtered <- data_filtered %>%
  mutate(
    street = trimws(gsub("&", "and", 
                         sub(",.*", "", 
                             sub("^\\d+\\s*", "", location))))  # Remove house number and everything after the first comma
  ) %>%
  mutate(
    street = str_replace_all(street,  "^[a-z]\\s*|\\s*[\\d]+\\s*[-/]*\\s*|[A-Z]$", "") %>%  # Remove unit numbers and stray uppercase letters
      str_replace_all(., "^[abAB]\\s+", "") # Remove 'a ' or 'A ' at the beginning of street name
  ) %>%
  mutate(
    street = str_replace_all(street, "^[/-]+|[-/]+$", "") %>%  # Remove leading and trailing '-' or '/'
      str_replace_all(., "^nd|^\\s+", "") %>%  # Remove 'nd' at the beginning and leading spaces
      str_replace_all(., "\\s+", " ")  # Normalize multiple spaces to a single space
  )

str(data_filtered)

# Filter data by Property Type
house_data <- data_filtered %>% filter(type == "House")
unit_data <- data_filtered %>% filter(type == "Unit")
townhouse_data <- data_filtered %>% filter(type == "Townhouse")
apart_data <- data_filtered %>% filter(type == "Apartment")


# Calculating Median Price and Number of Sales
house_median_price <- median(house_data$price, na.rm = TRUE)
house_num_sales <- nrow(house_data)

townhouse_median_price <- median(townhouse_data$price, na.rm = TRUE)
townhouse_num_sales <- nrow(townhouse_data)

unit_median_price <- median(unit_data$price, na.rm = TRUE)
unit_num_sales <- nrow(unit_data)

apart_median_price <- median(apart_data$price, na.rm = TRUE)
apart_num_sales <- nrow(apart_data)

# Create a summary DataFrame
summary_df <- data.frame(
  `Key Market Data` = c('Median price', 'Number of Sales'),
  House = c(house_median_price, house_num_sales),
  Townhouse = c(townhouse_median_price, townhouse_num_sales),
  Unit = c(unit_median_price, unit_num_sales),
  Apartment = c(apart_median_price, apart_num_sales)
)

print(summary_df)

#----------------------------------------------
### Visualisation 
### 1. Distribution of Price ###
# Plot the distribution of price with histogram and density plot
ggplot(data_filtered, aes(x = price)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "skyblue", alpha = 0.7) +
  geom_density(color = "red") +
  labs(title = "Distribution of Property Prices", x = "Price", y = "Density") +
  theme_minimal()

### 2. Price Trends Over Time ###

price_trends <- data_filtered %>%
  group_by(year) %>%
  summarise(avg_price = mean(price, na.rm = TRUE),  # Calculate average price per year
            total_sales = n(),                      # Count total number of sales per year 
            .groups = "drop"   )                    # Drop all grouping after summarization                   


# Plot price trends
ggplot(price_trends, aes(x = year)) +
  geom_line(aes(y = avg_price, color = "Average Price")) +
  geom_bar(aes(y = total_sales * max(avg_price) / max(total_sales), fill = "Total Sales"),
           stat = "identity", alpha = 0.5) +
  labs(y = "Price", title = "Property Market Evolution in Prospect, SA 5082") +
  scale_y_continuous(sec.axis = sec_axis(~ . * max(price_trends$total_sales) / max(price_trends$avg_price), name = "Total Sales")) +
  theme_minimal()


### 3. Box Plots for Categorical Variables ###
# 3.1 Box plot of price by property type
ggplot(data_filtered, aes(x = type, y = price)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  labs(title = "Price Distribution by Property Type", x = "Property Type", y = "Price") +
  theme_minimal()

# 3.2 Top ten street have highest price
#  Calculate average price by street
average_price_by_street <- data_filtered %>%
  group_by(street) %>%  # Group by street
  summarise(avg_price = mean(price, na.rm = TRUE), .groups = 'drop') %>%  # Calculate average price
  arrange(desc(avg_price)) %>%  # Sort by average price in descending order
  head(10)  # Get top 10 streets

#  Create the plot
ggplot(average_price_by_street, aes(x = reorder(street, avg_price), y = avg_price)) +
  geom_bar(stat = "identity", fill = "skyblue") +  # Create bar plot
  coord_flip() +  # Flip coordinates for better readability
  labs(title = "Top 10 Streets by Average Price", 
       x = "Street", 
       y = "Average Price ($)") +  # Add labels
  theme_minimal()  # Apply minimal theme


### 4. Seasonal Trend Analysis 
# Group by month and summarize total sales for each month across all years
monthly_sales <- data_filtered %>%
  group_by(month) %>%
  summarise(total_sales = n())


# Plot total sales by month to visualize seasonal trends
ggplot(monthly_sales, aes(x = month, y = total_sales)) +
  geom_line(group = 1, color = "blue", linewidth = 1) +    # Line plot for trend
  geom_point(color = "darkred", size = 2) +           # Points to highlight each month
  labs(
    title = "Seasonal Property Sales Trends",
    x = "Month",
    y = "Total Sales"
  ) +
  theme_minimal()

### 5. Correlation Matrix ###
# Calculate correlation matrix for numerical variables
numeric_data <- data_filtered %>% dplyr::select(price, bedrooms, bathrooms, car, land_size, year)
cor_matrix <- cor(numeric_data, use = "complete.obs")


# Plot the correlation matrix
corrplot(cor_matrix, method = "color", addCoef.col = "black", number.cex = 0.7,
         tl.cex = 0.8, title = "Correlation Matrix of Numerical Features")


###-----------------
# Clustering: categorical nominal variable

library(klaR)

# Convert data to character to avoid factor issues
data_categorical <- data_filtered %>%
  dplyr::select(-c(price, bedrooms, bathrooms, car, land_size, year)) %>% # Exclude numeric columns
  mutate(across(everything(), as.character))


# Apply K-Modes clustering
kmodes_result <- kmodes(data_categorical, modes = 2, iter.max = 10)
print(kmodes_result$cluster)

# Add cluster labels to the original data
data_filtered$cluster <- kmodes_result$cluster

# Save the data
save(data_filtered, file ="data_filtered.RData")


# Summarize the key features of each cluster
cluster_summary <- data_filtered %>%
  group_by(cluster) %>%
  summarise(
    avg_price = mean(as.numeric(price), na.rm = TRUE),
    avg_bedrooms = mean(as.numeric(bedrooms), na.rm = TRUE),
    avg_bathrooms = mean(as.numeric(bathrooms), na.rm = TRUE),
    avg_car = mean(as.numeric(car), na.rm = TRUE),
    avg_land_size = mean(as.numeric(land_size), na.rm = TRUE)
  )
print(cluster_summary)

#------------------------------------
# Prepare Data for Modeling
data_model <- data_filtered %>%
  dplyr::select(ID, price, bedrooms, bathrooms, car, land_size, year, cluster)  

data_model$price <- as.numeric(data_model$price)

# Split the data into training and testing sets
set.seed(42)
trainIndex <- sample(seq_len(nrow(data_model)), size = 0.8 * nrow(data_model))
data_train <- data_model[ trainIndex,]
data_test  <- data_model[-trainIndex,]

# Linear Regression Model
lr_model <- lm(price ~ bedrooms + bathrooms + car + land_size + year + cluster, 
               data = data_train)
model_summary <- summary(lr_model)
print(model_summary)
lr_predictions <- predict(lr_model, newdata = data_test)

# Evaluate Linear Regression Model
lr_mse <- mean((data_test$price - lr_predictions)^2)
lr_r2 <- cor(data_test$price, lr_predictions)^2

print(paste("Linear Regression Model - Mean Squared Error:", lr_mse))
print(paste("Linear Regression Model - R^2 Score:", lr_r2))


#---------------------------------------
### Text Analysis for Property Descriptions

# Define the Vocabulary
voc <- data_filtered %>%
  unnest_tokens(word, description) %>%
  anti_join(stop_words, by = "word") %>%
  filter(!str_detect(word, "[[:digit:]]"),
         !str_detect(word, "[^\\u0001-\\u007F]+"),
         !str_detect(word, "\\b(.)\\b")) %>%
  count(word) %>%
  filter(n >= 3) %>%  # Keep only words with at least 3 occurrences
  arrange(desc(n))

# Check the vocabulary
dim(voc)

# Tokenize, clean, and process text data
data_text <- data_filtered %>%
  unnest_tokens(word, description) %>%
  
  # Apply stemming using SnowballC
  mutate(word = wordStem(word, language = "en")) %>%
  # Keep only words in the vocabulary
  filter(word %in% voc$word) %>%
  # Count word occurrences per property ID
  count(ID, word) %>%
  # Convert to binary presence (1 if the word appears in description, 0 if not)
  mutate(n = if_else(n > 0, 1, 0)) %>%
  # Convert long table into wide table
  pivot_wider(id_cols = ID, names_from = word, values_from = n) %>%
  # Replace NA by zeros
  map_df(replace_na, 0)

# Check the result
print(data_text[1:10, 1:8])


# Create a Word Cloud to Visualize the Most Common Words:

# Create a word frequency data frame
word_freq <- data_text %>%
  pivot_longer(cols = -ID, names_to = "word", values_to = "n") %>%
  filter(word != "infrequent") %>%  # Exclude 'infrequent'
  group_by(word) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  arrange(desc(n))


# Generate a word cloud
wordcloud(words = word_freq$word, 
          freq = word_freq$n, 
          min.freq = 5, 
          max.words = 100,  # Limit the word count to 100 most frequent words
          random.order = FALSE, # Arranges the most frequent words in the center.
          colors = brewer.pal(8, "Dark2"), 
          scale = c(2, 0.3)) # Adjusted range for word sizes



# Calculate Binary Distances for Clustering
# Remove the ID column and calculate binary distances.
binary_dist <- parallelDist::parDist(as.matrix(data_text[, -1]), method = "binary", diag = TRUE)
# check the result
round(as.matrix(binary_dist)[1:15, 1:15], 3) # most of the words are far from each other.

# select distances equal zero
identical_words <- data.frame(which(as.matrix(binary_dist) == 0, arr.ind = TRUE)) %>% 
  filter(row != col) # remove diagonal elements



# Perform Hierarchical Clustering
# Plot the dendrogram
plot(fastcluster::hclust(binary_dist, method = "ward.D2")) # choose k=2

# Cluster the properties into two clusters for simplicity.
clustering_result <- cutree(fastcluster::hclust(binary_dist, method = "ward.D2"), k = 2)


# cluster sizes
table(clustering_result)

# get most popular words for each cluster
data_text %>% mutate(cluster = clustering_result) %>%
  dplyr::select(-ID) %>% # remove id
  # pivot for all columns except cluster membership
  pivot_longer(col = head(names(.), -1), names_to = "word") %>% group_by(cluster, word) %>%
  summarise(n = sum(value)) %>%
  arrange(cluster, desc(n)) %>% # sort by word counts
  group_by(cluster) %>%
  mutate(n = n / n()) %>% # get proportion instead of counts 
  filter(row_number() <= 15) %>% # get top 15 observations in each group 
  print(n = 30) # print out all selected observations


# use clustering membership as a predictor for the predictive model
# clustering for 2 clusters
clustering_result <- cutree(fastcluster::hclust(binary_dist, method = "ward.D2"), k = 2)
# Analyze Cluster Membership with numerical Data
# Add cluster information to the main data and analyze with a linear model.
cluster_analysis_data <- data_filtered %>%
  dplyr::select(ID, price, bedrooms, bathrooms, car, land_size, year) %>%
  inner_join(data_text %>% 
               dplyr::select(ID), by = "ID") %>%
               mutate(cluster = as.factor(clustering_result))%>%
  dplyr::select(-ID) # Exclude ID column

# Check the resulting data to ensure it’s as expected
print(head(cluster_analysis_data))
print(summary(cluster_analysis_data))
# Run linear regression analysis using cluster labels as a predictor
fit <- lm(price ~ bedrooms + bathrooms + car + land_size + year + cluster, data = cluster_analysis_data)
fit_summary <- summary(fit)

# Output the model summary to assess significance of clusters
print(fit_summary)

# Evaluate Linear Regression Model
cl_predictions <- predict(fit, newdata = cluster_analysis_data)
cl_mse <- mean((cluster_analysis_data$price - cl_predictions)^2)
cl_r2 <- cor(cluster_analysis_data$price, cl_predictions)^2

print(paste("Linear Regression Model - Mean Squared Error:", cl_mse))
print(paste("Linear Regression Model - R^2 Score:", cl_r2))
