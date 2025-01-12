# import libraries
if(sessionInfo()['basePkgs']=="dplyr" | sessionInfo()['otherPkgs']=="dplyr"){
  detach(package:dplyr, unload=TRUE)
}
if(sessionInfo()['basePkgs']=="tm" | sessionInfo()['otherPkgs']=="tm"){
  detach(package:sentiment, unload=TRUE)
  detach(package:tm, unload=TRUE)
}
library(plyr)
library(arules)
library(arulesViz)
library(dplyr)
library(ggplot2)
library(lubridate)
# rm(list=ls())

# import dataset
groceries <- read.csv("Basket Analysis - Data.csv",sep = ",",fileEncoding = "UTF-8-BOM")
class(groceries)
# /////////////////////////////////////////////////////////////////////////////////////
# EDA (Exploratory Data Analysis)
# Basic dataset overview
summary(groceries)  # Overview of the data
dim(groceries)      # Dimensions of the dataset
str(groceries)      # Structure of the dataset 
# 43745 observations(obs.), it means there are 43745 rows of data recorded.
head(groceries, 10) # First 10 rows of the dataset

# Unique item count
# Count total unique product descriptions
total_unique_products <- groceries %>%
  summarise(UniqueProducts = n_distinct(Product.Description))
print(paste("Total Unique Product Descriptions:", total_unique_products$UniqueProducts))
# Number of unique items (berapa banyak product dgn nama berbeda)

# Generate Bill Number in copy dataset only for visualization
groceries_visualization <- groceries
groceries_visualization$BillNumber <- paste(groceries_visualization$Customer.ID, groceries_visualization$Transaction.Date, sep = "_")

# Ensure Transaction.Date is in correct Date format (if it's not already)
groceries_visualization$Transaction.Date <- as.Date(groceries_visualization$Transaction.Date, format = "%d/%m/%Y")

# Frequency analysis of items
item_counts <- table(groceries_visualization$Product.Description)
item_counts_sorted <- sort(item_counts, decreasing = TRUE)
head(item_counts_sorted, 10) # Top 10 most frequent items

# Visualization of top 10 items
barplot(head(item_counts_sorted, 10), 
        main = "Top 10 Most Frequent Items",
        las = 2, col = "steelblue", 
        xlab = "Items", ylab = "Frequency",
        cex.names = 0.6) 

groceries_visualization$Year <- format(groceries_visualization$Transaction.Date, "%Y")

# Calculate item frequencies for each year
item_counts_by_year <- groceries_visualization %>%
  group_by(Year, Product.Description) %>%
  summarise(Frequency = n(), .groups = "drop") %>%
  arrange(Year, desc(Frequency))

# Get the top 5 items for each year
top_items_2014 <- item_counts_by_year %>%
  filter(Year == 2014) %>%
  slice_max(order_by = Frequency, n = 5)

top_items_2015 <- item_counts_by_year %>%
  filter(Year == 2015) %>%
  slice_max(order_by = Frequency, n = 5)

# Plot for 2014
plot_2014 <- ggplot(top_items_2014, aes(x = reorder(Product.Description, -Frequency), y = Frequency, fill = Product.Description)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 5 Most Frequent Items in 2014",
       x = "Items", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set2") +
  guides(fill = "none")

# Plot for 2015
plot_2015 <- ggplot(top_items_2015, aes(x = reorder(Product.Description, -Frequency), y = Frequency, fill = Product.Description)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 5 Most Frequent Items in 2015",
       x = "Items", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set2") +
  guides(fill = "none")

# Display the plots
print(plot_2014)
print(plot_2015)

# Count unique transactions in the dataset
total_unique_transactions <- groceries_visualization %>%
  summarise(UniqueTransactions = n_distinct(BillNumber))
print(paste("Total Unique Transactions:", total_unique_transactions$UniqueTransactions))

# Extract year and month for analysis
groceries_visualization$Year <- year(groceries_visualization$Transaction.Date)
groceries_visualization$Month <- month(groceries_visualization$Transaction.Date, label = TRUE)

# Number of unique transactions by year
transactions_per_year <- groceries_visualization %>%
  group_by(Year) %>%
  summarize(UniqueTransactions = n_distinct(BillNumber))
print(transactions_per_year)

# Plot transactions per year
ggplot(transactions_per_year, aes(x = Year, y = UniqueTransactions)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Number of Unique Transactions Per Year",
       x = "Year", y = "Number of Transactions") +
  theme_minimal()

# Number of unique transactions by month and year
transactions_per_month_year <- groceries_visualization %>%
  group_by(Month, Year) %>%
  summarize(UniqueTransactions = n_distinct(BillNumber))
print(transactions_per_month_year)

# Plot transactions per month with different colors for years (stacked)
ggplot(transactions_per_month_year, aes(x = Month, y = UniqueTransactions, fill = factor(Year))) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Number of Unique Transactions Per Month by Year",
       x = "Month", y = "Number of Transactions", fill = "Year") +
  theme_minimal()

# /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
# PREPROCESSING
# Check for null (NA) values in each column
null_values <- colSums(is.na(groceries))
print(null_values)

# check for duplicated data
sum(duplicated(groceries))
groceries<-groceries[!duplicated(groceries),]

# check for anomalies or unwanted characters 
unique(groceries$Product.Description)
# Remove rows where Product Description contains "delete"
groceries <- groceries[groceries$Product.Description != "delete", ]

# Convert Transaction.Date from chr to Date type
groceries$Transaction.Date <- as.Date(groceries$Transaction.Date, format = "%d/%m/%Y")

# Sorts the dataset so that transactions are ordered by date and for each date by customer id
# Sorting prevents errors when 
# transforming the data into a basket format for analysis.
sortedGroceries <- groceries[order(groceries$Transaction.Date,groceries$Customer.ID),]
# Generate Bill Number to uniquely identify transactions by date and customer id
sortedGroceries$BillNumber <- paste(
  sortedGroceries$Transaction.Date, sortedGroceries$Customer.ID, sep = "_")

# ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
# Filter data for each year
groceries_2014 <- sortedGroceries[format(sortedGroceries$Transaction.Date, "%Y") == "2014",]
groceries_2015 <- sortedGroceries[format(sortedGroceries$Transaction.Date, "%Y") == "2015",]

# Group items bought together by the same customer on the same date based on bill number
grouped_data_2014 <- ddply(groceries_2014, c("BillNumber"),
                           function(df1) paste(df1$Product.Description, collapse = ","))
grouped_data_2015 <- ddply(groceries_2015, c("BillNumber"),
                           function(df1) paste(df1$Product.Description, collapse = ","))

# Remove BillNumber column
grouped_data_2014$BillNumber <- NULL
colnames(grouped_data_2014) <- c("Transactions")
grouped_data_2015$BillNumber <- NULL
colnames(grouped_data_2015) <- c("Transactions")

# Save grouped data to a CSV file for basket format conversion
write.csv(grouped_data_2014, "GroupedTransactions_2014.csv", quote = FALSE, row.names = TRUE)
write.csv(grouped_data_2015, "GroupedTransactions_2015.csv", quote = FALSE, row.names = TRUE)

# Convert the CSV file to basket format
txn_2014 <- read.transactions(file = "GroupedTransactions_2014.csv", 
                              rm.duplicates = TRUE, format = "basket", sep = ",", cols = 1)
txn_2014@itemInfo$labels <- gsub("\"", "", txn_2014@itemInfo$labels)  # Remove quotes
txn_2015 <- read.transactions(file = "GroupedTransactions_2015.csv", 
                              rm.duplicates = TRUE, format = "basket", sep = ",", cols = 1)
txn_2015@itemInfo$labels <- gsub("\"", "", txn_2015@itemInfo$labels)  # Remove quotes

# Run Apriori algorithm for both year
basket_rules_2014 <- apriori(txn_2014, parameter = list(sup = 0.002, conf = 0.5, target = "rules"))
basket_rules_2015 <- apriori(txn_2015, parameter = list(sup = 0.002, conf = 0.5, target = "rules"))

# Print number of rules and display the top rules sorted by lift
cat("Number of rules for year 2014:", length(basket_rules_2014), "\n")
rules_sorted_by_lift_2014 <- sort(basket_rules_2014, by = "lift", decreasing = TRUE)
inspect(rules_sorted_by_lift_2014)  

cat("Number of rules for year 2015:", length(basket_rules_2015), "\n")
rules_sorted_by_lift_2015 <- sort(basket_rules_2015, by = "lift", decreasing = TRUE)
inspect(rules_sorted_by_lift_2015)  

# Plot rules 
plot(basket_rules_2014, measure = c("support", "confidence"), shading = "lift", main = "Apriori Rules for Year 2014")
plot(basket_rules_2015, measure = c("support", "confidence"), shading = "lift", main = "Apriori Rules for Year 2015")

plot(basket_rules_2014, method = "grouped", main = "Grouped Rules for 2014")
plot(basket_rules_2015, method = "grouped", main = "Grouped Rules for 2015")

plot(basket_rules_2014, method = "graph", main = "Graph of Rules for 2014")
plot(basket_rules_2015, method = "graph", main = "Graph of Rules for 2015")

