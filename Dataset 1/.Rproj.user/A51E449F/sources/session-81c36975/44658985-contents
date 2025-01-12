library(arules) # Provides the infrastructure for representing
library(arulesViz) # Extends package 'arules' with various visualization.
library(plyr) # Tools for Splitting, Applying and Combining Data. -> for dataframe
library(dplyr) # Used for preprocessing
library(ggplot2)
library(igraph)

itemslist <- read.csv("Assignment-1_Data.csv", sep = ";", fileEncoding = "UTF-8-BOM", check.names = FALSE)
length(unique(itemslist$BillNo))

# EDA
#==========================================================================================================
### 1. Number of Unique Transactions per Month
# Extract year and month from the Date column
itemslist$Date <- as.Date(itemslist$Date, format = "%d.%m.%Y")
itemslist <- itemslist %>%
  mutate(YearMonth = format(Date, "%Y-%m"))

# Count unique transactions per month
transactions_per_month <- itemslist %>%
  group_by(YearMonth) %>%
  summarise(Unique_Transactions = n_distinct(BillNo)) %>%
  arrange(YearMonth)

# Plot the data as a bar plot
ggplot(transactions_per_month, aes(x = YearMonth, y = Unique_Transactions)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  labs(
    title = "Number of Unique Transactions Per Month",
    x = "Month",
    y = "Number of Transactions"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels

# Ensure the Date column is in Date format
itemslist$Date <- as.Date(itemslist$Date, format = "%d.%m.%Y")

# Extract Year and Month from the Date column
itemslist <- itemslist %>%
  mutate(YearMonth = format(Date, "%Y-%m"))  # Extract Year-Month as a single value

# Count the unique BillNo for each Year-Month
transactions_per_month <- itemslist %>%
  group_by(YearMonth) %>%
  summarise(Unique_Transactions = n_distinct(BillNo)) %>%
  arrange(YearMonth)

# View the result
print(transactions_per_month)

# Check if each CustomerID has multiple BillNo
itemslist %>%
  group_by(CustomerID) %>%
  summarise(NumBillNo = n_distinct(BillNo)) %>%
  filter(NumBillNo > 1)


###2. Boxplot of Purchase Quanitites
# Define the thresholds
q1 <- quantile(itemslist$Quantity, 0.25, na.rm = TRUE)
q3 <- quantile(itemslist$Quantity, 0.75, na.rm = TRUE)
iqr <- q3 - q1
lower_threshold <- q1 - 1.5 * iqr
upper_threshold <- q3 + 1.5 * iqr

# Create the boxplot with annotations as legends
ggplot(itemslist, aes(x = "", y = Quantity)) +
  geom_boxplot(fill = "skyblue", color = "darkblue") +
  labs(
    title = "Boxplot of Purchase Quantities with Thresholds",
    y = "Quantity"
  ) +
  theme_minimal() +
  annotate("text", x = 1.1, y = max(itemslist$Quantity, na.rm = TRUE), 
           label = paste("Upper Threshold:", round(upper_threshold, 1)), 
           color = "blue", size = 4, hjust = 0) +  # Place near top-right
  annotate("text", x = 1.1, y = max(itemslist$Quantity, na.rm = TRUE) - 5000, 
           label = paste("Lower Threshold:", round(lower_threshold, 1)), 
           color = "red", size = 4, hjust = 0) +  # Place slightly below the upper threshold annotation
  coord_cartesian(ylim = c(min(itemslist$Quantity, na.rm = TRUE), 
                           max(itemslist$Quantity, na.rm = TRUE)))  # Keep full data range


### 3. Visualizing Bulk vs Single Transactions
str(itemslist$Price)
itemslist$Price <- gsub(",", ".", itemslist$Price)
itemslist$Price <- as.numeric(itemslist$Price)

# Ensure PurchaseCategory is created and verify with str
itemslist <- itemslist %>%
  mutate(PurchaseCategory = ifelse(Quantity <= upper_threshold, "Normal", "Bulk"))

# Verify the structure of itemslist again to ensure PurchaseCategory exists
str(itemslist)

# Step 2: Count transactions by PurchaseCategory (Normal vs. Bulk)
purchase_category_counts <- itemslist %>%
  group_by(PurchaseCategory) %>%
  summarise(TransactionCount = n(), .groups = "drop")

# Step 3: Visualize the Count of Transactions (Normal vs Bulk)
ggplot(purchase_category_counts, aes(x = PurchaseCategory, y = TransactionCount, fill = PurchaseCategory)) +
  geom_bar(stat = "identity", color = "black") +
  labs(
    title = "Count of Transactions by Category (Normal vs Bulk)",
    x = "Purchase Category",
    y = "Transaction Count"
  ) +
  theme_minimal()

# Step 4: Identify the Most Common Items in each Category (Normal vs Bulk)
top_items_by_category <- itemslist %>%
  group_by(PurchaseCategory, Itemname) %>%
  summarise(ItemFrequency = n(), .groups = "drop") %>%
  group_by(PurchaseCategory) %>%
  slice_max(ItemFrequency, n = 10)

# Step 5: Visualize the Most Common Items in each Category (Normal vs Bulk)
ggplot(top_items_by_category, aes(x = reorder(Itemname, ItemFrequency), y = ItemFrequency, fill = PurchaseCategory)) +
  geom_bar(stat = "identity", color = "black") +
  coord_flip() +  # Flip the axes for readability
  labs(
    title = "Top 10 Most Frequently Purchased Items by Category (Normal vs Bulk)",
    x = "Item Name",
    y = "Frequency",
    fill = "Purchase Category"
  ) +
  theme_minimal()

# PREPROCESSING
#==========================================================================================================
### 1. Delete rows with negative quantity values
num_negative_quantity <- sum(itemslist$Quantity < 0, na.rm = TRUE)
print(paste("Number of rows with negative Quantity before filtering:", num_negative_quantity))

# Filter out rows with negative quantities
itemslist <- itemslist %>%
  filter(Quantity >= 0)
print(paste("Total Number of Rows Before Splitting:", nrow(itemslist)))

# Verify if negative quantities are removed
num_negative_quantity <- sum(itemslist$Quantity < 0, na.rm = TRUE)
print(paste("Number of rows with negative Quantity after filtering:", num_negative_quantity))

### 2. Delete rows with null or empty Itemname values
num_null_itemnames <- sum(is.na(itemslist$Itemname) | itemslist$Itemname == "")
print(paste("Number of rows with null or empty Itemname before filtering:", num_null_itemnames))

# Filter out null or empty Itemname values
itemslist <- itemslist %>%
  filter(!is.na(Itemname) & Itemname != "")

# Verify if null or empty Itemname values are removed
num_null_itemnames <- sum(is.na(itemslist$Itemname) | itemslist$Itemname == "")
print(paste("Number of rows with null or empty Itemname after filtering:", num_null_itemnames))

### 3. Delete rows with outlier items
# Define valid exceptions
valid_exceptions <- c(
  "BAG 500g SWIRLY MARBLES",
  "BAG 125g SWIRLY MARBLES",
  "BAG 250g SWIRLY MARBLES",
  "POLYESTER FILLER PAD 45x45cm",
  "POLYESTER FILLER PAD 45x30cm",
  "POLYESTER FILLER PAD 40x40cm",
  "FRENCH BLUE METAL DOOR SIGN No",
  "Dr. Jam's Arouzer Stress Ball",
  "3 TRADITIONAl BISCUIT CUTTERS  SET",
  "FOLK ART GREETING CARD,pack/12",
  "ESSENTIAL BALM 3.5g TIN IN ENVELOPE",
  "POLYESTER FILLER PAD 65CMx65CM",
  "Dad's Cab Electronic Meter",
  "NUMBER TILE VINTAGE FONT No",
  "*Boombox Ipod Classic",
  "NUMBER TILE COTTAGE GARDEN No",
  "*USB Office Mirror Ball",
  "POLYESTER FILLER PAD 30CMx30CM",
  "Dotcomgiftshop Gift Voucher Â£40.00",
  "Dotcomgiftshop Gift Voucher Â£50.00",
  "Dotcomgiftshop Gift Voucher Â£30.00",
  "Dotcomgiftshop Gift Voucher Â£20.00",
  "Dotcomgiftshop Gift Voucher Â£10.00",
  "POLYESTER FILLER PAD 60x40cm",
  "FLOWERS HANDBAG blue and orange",
  "THE KING GIFT BAG 25x24x12cm",
  "Dotcomgiftshop Gift Voucher Â£100.00"
)

# Identify outlier item names
outlier_itemnames <- itemslist %>%
  filter((Itemname == "?" | grepl("[a-z]", Itemname)) & !Itemname %in% valid_exceptions) %>%
  select(Itemname) %>%
  distinct()

print(outlier_itemnames)

# Filter out outlier Itemnames
itemslist <- itemslist %>%
  filter(!Itemname %in% outlier_itemnames$Itemname)

print(paste("Total Number of Rows Before Splitting:", nrow(itemslist)))

# Count the total number of unique BillNos
total_unique_bill_numbers <- itemslist %>%
  summarise(TotalUniqueBillNos = n_distinct(BillNo))

# Print the total number of unique BillNos
print(paste("Total number of unique BillNos:", total_unique_bill_numbers$TotalUniqueBillNos))

# Check if a BillNo contains both normal and bulk orders
mixed_bill_numbers <- itemslist %>%
  mutate(
    PurchaseType = case_when(
      Quantity >= 1 & Quantity <= upper_threshold ~ "Normal",
      Quantity > upper_threshold ~ "Bulk",
      TRUE ~ "Other"  # Catch-all for anything else
    )
  ) %>%
  group_by(BillNo) %>%
  summarise(
    HasNormal = any(PurchaseType == "Normal"),
    HasBulk = any(PurchaseType == "Bulk")
  ) %>%
  filter(HasNormal & HasBulk)

# Print the number of such BillNos
print(paste("Number of BillNos with both Normal and Bulk orders:", nrow(mixed_bill_numbers)))

# Inspect a few examples of such BillNos
head(mixed_bill_numbers)

# Categorize BillNos based on their PurchaseType
bill_number_categories <- itemslist %>%
  mutate(
    PurchaseType = case_when(
      Quantity >= 1 & Quantity <= upper_threshold ~ "Normal",
      Quantity > upper_threshold ~ "Bulk",
      TRUE ~ "Other"  # Catch-all for anything else
    )
  ) %>%
  group_by(BillNo) %>%
  summarise(
    HasNormal = any(PurchaseType == "Normal"),
    HasBulk = any(PurchaseType == "Bulk")
  ) %>%
  mutate(
    Category = case_when(
      HasNormal & HasBulk ~ "Mixed",
      HasNormal ~ "Only Normal",
      HasBulk ~ "Only Bulk",
      TRUE ~ "Other"
    )
  ) %>%
  count(Category)  # Count the number of BillNos in each category

# Create the pie chart
ggplot(bill_number_categories, aes(x = "", y = n, fill = Category)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  labs(
    title = "Distribution of BillNos by Order Type",
    fill = "Category",
    y = "",
    x = ""
  ) +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank()) +
  geom_text(aes(label = paste0(Category, " (", n, ")")), 
            position = position_stack(vjust = 0.5), size = 4)


### 4. Split the dataset based on Purchase Category
# Mixed :
bill_numbers_mixed <- itemslist %>%
  mutate(
    PurchaseType = case_when(
      Quantity >= 1 & Quantity <= upper_threshold ~ "Normal",
      Quantity > upper_threshold ~ "Bulk",
      TRUE ~ "Other"
    )
  ) %>%
  group_by(BillNo) %>%
  summarise(
    HasNormal = any(PurchaseType == "Normal"),
    HasBulk = any(PurchaseType == "Bulk")
  ) %>%
  mutate(
    Category = case_when(
      HasNormal & HasBulk ~ "Mixed",
      HasNormal ~ "Only Normal",
      HasBulk ~ "Only Bulk",
      TRUE ~ "Other"
    )
  ) %>%
  filter(Category == "Mixed") %>%
  pull(BillNo)

# Normal :
bill_numbers_only_normal <- itemslist %>%
  mutate(
    PurchaseType = case_when(
      Quantity >= 1 & Quantity <= upper_threshold ~ "Normal",
      Quantity > upper_threshold ~ "Bulk",
      TRUE ~ "Other"
    )
  ) %>%
  group_by(BillNo) %>%
  summarise(
    HasNormal = any(PurchaseType == "Normal"),
    HasBulk = any(PurchaseType == "Bulk")
  ) %>%
  mutate(
    Category = case_when(
      HasNormal & HasBulk ~ "Mixed",
      HasNormal ~ "Only Normal",
      HasBulk ~ "Only Bulk",
      TRUE ~ "Other"
    )
  ) %>%
  filter(Category == "Only Normal") %>%
  pull(BillNo)

# Bulk :
bill_numbers_only_bulk <- itemslist %>%
  mutate(
    PurchaseType = case_when(
      Quantity >= 1 & Quantity <= upper_threshold ~ "Normal",
      Quantity > upper_threshold ~ "Bulk",
      TRUE ~ "Other"
    )
  ) %>%
  group_by(BillNo) %>%
  summarise(
    HasNormal = any(PurchaseType == "Normal"),
    HasBulk = any(PurchaseType == "Bulk")
  ) %>%
  mutate(
    Category = case_when(
      HasNormal & HasBulk ~ "Mixed",
      HasNormal ~ "Only Normal",
      HasBulk ~ "Only Bulk",
      TRUE ~ "Other"
    )
  ) %>%
  filter(Category == "Only Bulk") %>%
  pull(BillNo)


mixed_transactions <- itemslist %>%
  filter(BillNo %in% bill_numbers_mixed)

only_normal_transactions <- itemslist %>%
  filter(BillNo %in% bill_numbers_only_normal)

only_bulk_transactions <- itemslist %>%
  filter(BillNo %in% bill_numbers_only_bulk)

write.csv(mixed_transactions, "mixed_transactions.csv", row.names = FALSE)
write.csv(only_normal_transactions, "only_normal_transactions.csv", row.names = FALSE)
write.csv(only_bulk_transactions, "only_bulk_transactions.csv", row.names = FALSE)

mixed_transactions <- read.csv("mixed_transactions.csv", fileEncoding = "UTF-8-BOM", check.names = FALSE)
only_normal_transactions <- read.csv("only_normal_transactions.csv", fileEncoding = "UTF-8-BOM", check.names = FALSE)
only_bulk_transactions <- read.csv("only_bulk_transactions.csv", fileEncoding = "UTF-8-BOM", check.names = FALSE)

unique_bill_mixed <- length(unique(mixed_transactions$BillNo))
unique_bill_only_normal <- length(unique(only_normal_transactions$BillNo))
unique_bill_only_bulk <- length(unique(only_bulk_transactions$BillNo))

print(paste("Unique BillNos in Mixed Transactions:", unique_bill_mixed))
print(paste("Unique BillNos in Only Normal Transactions:", unique_bill_only_normal))
print(paste("Unique BillNos in Only Bulk Transactions:", unique_bill_only_bulk))

total_unique_bill <- unique_bill_mixed + unique_bill_only_normal + unique_bill_only_bulk
print(paste("Total Unique BillNos across all datasets:", total_unique_bill))


### 5. Drop unnecessary columns for all three datasets
# Mixed Transactions
mixed_transactions_cleaned <- mixed_transactions %>%
  select(BillNo, Itemname) %>%
  distinct()

itemFrequencyPreTransaction_mixed <- mixed_transactions_cleaned %>%
  group_by(Itemname) %>%
  summarise(Frequency = n(), .groups = 'drop') %>%
  arrange(desc(Frequency))

print("Top 10 Frequent Items in Mixed Transactions:")
head(itemFrequencyPreTransaction_mixed, 10)

# Only Normal Transactions
only_normal_transactions_cleaned <- only_normal_transactions %>%
  select(BillNo, Itemname) %>%
  distinct()

itemFrequencyPreTransaction_only_normal <- only_normal_transactions_cleaned %>%
  group_by(Itemname) %>%
  summarise(Frequency = n(), .groups = 'drop') %>%
  arrange(desc(Frequency))

print("Top 10 Frequent Items in Only Normal Transactions:")
head(itemFrequencyPreTransaction_only_normal, 10)

# Only Bulk Transactions
only_bulk_transactions_cleaned <- only_bulk_transactions %>%
  select(BillNo, Itemname) %>%
  distinct()

itemFrequencyPreTransaction_only_bulk <- only_bulk_transactions_cleaned %>%
  group_by(Itemname) %>%
  summarise(Frequency = n(), .groups = 'drop') %>%
  arrange(desc(Frequency))

print("Top 10 Frequent Items in Only Bulk Transactions:")
head(itemFrequencyPreTransaction_only_bulk, 10)

# TRANSACTION CONVERSION
#===================================================================================================================
# Convert cleaned datasets to transactions for FP-Growth
convert_to_transactions <- function(cleaned_data) {
  transactions <- as(split(cleaned_data$Itemname, cleaned_data$BillNo), "transactions")
  print(summary(transactions))
  return(transactions)
}

# Create transactions for Mixed, Only Normal, and Only Bulk datasets
mixed_transactions_fp <- convert_to_transactions(mixed_transactions_cleaned)
only_normal_transactions_fp <- convert_to_transactions(only_normal_transactions_cleaned)
only_bulk_transactions_fp <- convert_to_transactions(only_bulk_transactions_cleaned)

# Summary of transactions for each dataset
print("Summary of Mixed Transactions:")
summary(mixed_transactions_fp)

print("Summary of Only Normal Transactions:")
summary(only_normal_transactions_fp)

print("Summary of Only Bulk Transactions:")
summary(only_bulk_transactions_fp)

# FP GROWTH IMPLEMENTATION
#===================================================================================================================
library(fim4r) # Ensure arules library is loaded

# Function to apply FP-Growth and generate association rules
generate_fpgrowth_rules <- function(transaction_data, supp, conf) {
  rules <- fim4r(transaction_data, method = "fpgrowth", target = "rules", supp = supp, conf = conf)
  if (length(rules) > 0) {
    rules_sorted <- sort(rules, by = "lift")
    print(paste("Number of rules generated:", length(rules)))
    inspect(head(rules_sorted, 10)) # Inspect top 10 rules sorted by lift
    return(rules_sorted)
  } else {
    print("No rules generated for this dataset.")
    return(NULL)
  }
}

# Apply FP-Growth to Mixed Transactions
print("FP-Growth Rules for Mixed Transactions:")
rules_mixed <- generate_fpgrowth_rules(mixed_transactions_fp, supp = 0.02, conf = 0.3)

# Apply FP-Growth to Only Normal Transactions
print("FP-Growth Rules for Only Normal Transactions:")
rules_only_normal <- generate_fpgrowth_rules(only_normal_transactions_fp, supp = 0.02, conf = 0.3)

# Apply FP-Growth to Only Bulk Transactions
print("FP-Growth Rules for Only Bulk Transactions:")
rules_only_bulk <- generate_fpgrowth_rules(only_bulk_transactions_fp, supp = 0.01, conf = 0.3)

# install.packages("https://mhahsler.github.io/arules/docs/fim4r/fim4r_latest.tar.gz", repos = NULL, type = "source")