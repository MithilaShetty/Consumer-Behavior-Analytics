library(ggplot2)
library(tidyr)
library(dplyr)
library(gridExtra)
library(knitr)
library(scales)
library(lubridate)
library(patchwork)
library(Metrics)
library(kableExtra)
library(reshape2)
library(Boruta)
library(ROSE)
library(caret)
library(rpart)
library(rpart.plot)
library(pROC)
library(ROCR)
library(base)
library(stats)
library(fastDummies)



############################## DATA COLLECTION #####################

#consumer dataset is loaded and its dimension and datatypes are retrieved
consumer.dt <- read.csv("/Users/mithilapapishetty/Downloads/Consumer_Data.csv")
dim(consumer.dt)


knitr::kable(head(consumer.dt[, 1:16]), caption = "First Half of Columns - consumer.dt")
knitr::kable(head(consumer.dt[, 17:29]), caption = "Second Half of Columns - consumer.dt")
str(consumer.dt)

################## DATA EXPLORATION AND PREPROCESSING ###############################

#renaming columns with more readable names

colnames(consumer.dt)
names(consumer.dt) <- c(
  "id", "birth_year", "education", "marital_status", "income",
  "kids_at_home", "teens_at_home", "join_date_customer", "days_since_last_purchase",
  "spending_on_wines", "spending_on_fruits", "spending_on_meat", "spending_on_fish", "spending_on_sweets", "spending_on_goldproducts",
  "num_of_deals_purchases", "num_of_web_purchases", "num_of_catalog_purchases", "num_of_store_purchases",
  "website_visits_per_month", "accepted_camp_3", "accepted_camp_4", "accepted_camp_5", "accepted_camp_1",
  "accepted_camp_2", "complain", "z_costcontact", "z_revenue", "last_campaign_response"
)
colnames(consumer.dt)

######### DATA TYPES CONVERSION
# Updating data types in consumer.dt: converting birth year and join date to Date, education and marital status to factor, 
#spending and revenue to numeric, and binary variables to factor(categorical).

consumer.dt$birth_year <- as.Date(paste0(consumer.dt$birth_year, "-01-01"), format = "%Y-%m-%d")
consumer.dt$join_date_customer <- as.Date(consumer.dt$join_date_customer, format = "%d-%m-%Y")
consumer.dt$education <- as.factor(consumer.dt$education)
consumer.dt$marital_status <- as.factor(consumer.dt$marital_status)
consumer.dt$spending_on_wines <- as.numeric(consumer.dt$spending_on_wines)
consumer.dt$spending_on_fruits <- as.numeric(consumer.dt$spending_on_fruits)
consumer.dt$spending_on_fish <- as.numeric(consumer.dt$spending_on_fish)
consumer.dt$spending_on_sweets <- as.numeric(consumer.dt$spending_on_sweets)
consumer.dt$spending_on_goldproducts <- as.numeric(consumer.dt$spending_on_goldproducts)
consumer.dt$income <- as.numeric(consumer.dt$income)
consumer.dt$z_revenue <- as.numeric(consumer.dt$z_revenue)
consumer.dt$z_costcontact <- as.numeric(consumer.dt$z_costcontact)
binary_vars <- c("accepted_camp_1", "accepted_camp_2", "accepted_camp_3", 
                 "accepted_camp_4", "accepted_camp_5", "complain", "last_campaign_response")
consumer.dt[binary_vars] <- lapply(consumer.dt[binary_vars], factor)

############# MISSING AND ZERO VALUE COUNT

# Creating a kable with  missing and zero value count by creating its summary 
missing_zero_summary <- dplyr::tibble(
  Variable = names(consumer.dt),
  `Missing Values` = sapply(consumer.dt, function(x) sum(is.na(x))),
  `Zero Values` = sapply(consumer.dt, function(x) sum(x == 0, na.rm = TRUE))
)
knitr::kable(missing_zero_summary, caption = "Missing and Zero Value Counts for Each Variable")

#HANDLING MISSING VALUES - removing the missing value records
consumer.dt <- consumer.dt %>%
  dplyr::filter(!is.na(income))
############# OUTLIER DETECTION

# To ensure reliable analysis, numeric features are checked for extreme values using the IQR method.
# Outliers are defined as values falling outside 1.5 times the interquartile range.
# This helps detect unusually high or low values that could distort statistical summaries or model training.
# The same approach is applied to birth year after converting it to numeric for consistency in detection.
count_outliers <- function(x) {
  qnt <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
  iqr <- qnt[2] - qnt[1]
  lower_bound <- qnt[1] - 1.5 * iqr
  upper_bound <- qnt[2] + 1.5 * iqr
  sum(x < lower_bound | x > upper_bound, na.rm = TRUE)
}
numeric_vars <- consumer.dt %>% select(where(is.numeric))
outlier_summary <- dplyr::tibble(
  Variable = names(numeric_vars),
  `Outlier Count` = sapply(numeric_vars, count_outliers)
)

kable(outlier_summary, caption = "Outlier Counts for Numeric Variables Only")
birthyear_outliers <- count_outliers(as.numeric(format(consumer.dt$birth_year, "%Y")))
join_dates <- table(format(consumer.dt$join_date_customer, "%Y"))
table(format(consumer.dt$birth_year, "%Y"))
birthyear_outliers 
join_dates


##################### HANDLING OUTLIERS 
# Birth year outliers are identified using the IQR method to remove unusually old or incorrect dates.
# Records falling outside acceptable year bounds are excluded to improve data quality.
# Additionally, a known placeholder value in 'income' (666666) is removed as it likely represents missing or invalid data.
# These steps ensure that the dataset used for analysis reflects realistic and reliable consumer information.

birth_year_numeric <- as.numeric(format(consumer.dt$birth_year, "%Y"))
iqr <- stats::IQR(birth_year_numeric, na.rm = TRUE)
lower_bound <- stats::quantile(birth_year_numeric, 0.25, na.rm = TRUE) - 1.5 * iqr
upper_bound <- stats::quantile(birth_year_numeric, 0.75, na.rm = TRUE) + 1.5 * iqr

consumer.dt <- consumer.dt %>%
  dplyr::filter(birth_year_numeric >= lower_bound & birth_year_numeric <= upper_bound)

consumer.dt <- consumer.dt %>%
  dplyr::filter(income != 666666)

#############CLEANING CATEGORICAL VARIABLES: MARITAL STATUS AND EDUCATION

# Standardize 'marital_status' to lowercase and remove invalid entries like "absurd" and "yolo" to improve data consistency.
# Similar marital categories are grouped into broader, meaningful labels such as "Couple" or "Single" for simplified analysis.
# The cleaned variable is then converted to a factor for appropriate categorical handling in modeling.

# For 'education', raw values are standardized to lowercase for uniformity.
# Similar levels of formal education are grouped (e.g., "2n cycle", "graduation", "master" -> "graduate") to reduce category granularity.
# This improves interpretability and modeling efficiency by avoiding redundant distinctions.


consumer.dt$marital_status <- tolower(consumer.dt$marital_status)
consumer.dt <- consumer.dt %>%
  filter(!marital_status %in% c("absurd", "yolo"))

consumer.dt$marital_status <- dplyr::case_when(
  consumer.dt$marital_status %in% c("married", "together") ~ "Couple",
  consumer.dt$marital_status %in% c("single", "alone") ~ "Single",
  consumer.dt$marital_status == "divorced" ~ "Divorced",
  consumer.dt$marital_status == "widow" ~ "Widow",
  TRUE ~ "Other"
)
consumer.dt$marital_status <- as.factor(consumer.dt$marital_status)

consumer.dt$education <- tolower(consumer.dt$education)
consumer.dt$education <- dplyr::case_when(
  consumer.dt$education %in% c("basic") ~ "Basic",
  consumer.dt$education %in% c("2n cycle", "graduation", "master") ~ "graduate",
  consumer.dt$education %in% c("phd") ~ "PhD",
  TRUE ~ "Other"
)

##########SUMMARY STATISTICS FOR NUMERIC VARIABLES
# Select only numeric columns from the dataset for statistical summarization.
# Summary statistics such as mean, median, min, max, and quartiles help in understanding the distribution and scale of each numeric feature.
# This step is crucial for identifying skewness, possible data entry issues, or the need for transformations.
# The structure of the dataset is then reviewed to confirm data types and overall integrity after preprocessing.

consumer.dt %>%
  select(where(is.numeric)) %>%
  select(-z_costcontact, -z_revenue, -id) %>%
  summary()


################### VARIABLE DISTRIBUTION PLOTS 
##### DEMOGRAPHIC DISTRIBUTIONS
# Visualize key demographic features using boxplots and barplots to understand distribution, spread, and potential outliers.
# Helps in identifying skewness in income, birth year, and family structure variables.
# Barplots for categorical variables provide insight into the most common education levels and marital statuses.

#####DEMOGRAPHICS
#Year of Birth
p1 <- ggplot(consumer.dt, aes(y = birth_year)) +
  geom_boxplot(fill = "steelblue") +
  theme_minimal() +
  labs(title = "Boxplot: Year of Birth", y = "")

# Income
p2 <- ggplot(consumer.dt, aes(y = income)) +
  geom_boxplot(fill = "steelblue") +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  labs(title = "Boxplot: Income", y = "")

#Kids and Teens at Home

family_data <- consumer.dt %>% select(kids_at_home, teens_at_home) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")

p3 <- ggplot(family_data, aes(x = Variable, y = Value, fill = Variable)) +
  geom_boxplot(fill = "steelblue" ) +
  theme_minimal() +
  labs(title = "Boxplot: Kid and Teen Count", x = "", y = "") +
  theme(legend.position = "none")

# Education
p4 <- ggplot(consumer.dt, aes(x = education)) +
  geom_bar(fill = "steelblue") +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.3, size = 3.5) +
  theme_minimal() +
  labs(title = "Barplot: Education", x = "", y = "Count") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

#  Marital Status
p5 <- ggplot(consumer.dt, aes(x = marital_status)) +
  geom_bar(fill = "steelblue") +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.3, size = 3.5) +
  theme_minimal() +
  labs(title = "Barplot: Marital Status", x = "", y = "Count") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

(p1 | p2 | p3) 

grid.arrange(p4, p5)


##### BEHAVIORAL VARIABLES
# Plot the distribution of 'days_since_last_purchase' to understand recency of customer activity.
# A histogram is used to visualize frequency of customers by recency bands.

######days_since_last_purchase

ggplot(consumer.dt, aes(x = days_since_last_purchase)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "white") +
  theme_minimal() +
  labs(
    title = "Distribution of Recency",
    x = "Days Since Last Purchase",
    y = "Number of Customers"
  )

#############SPENDING DIRSTRIBUTIONS
# Examine spending patterns across different product categories using histograms.
# The faceted layout helps to compare distribution shapes and identify products with high or low engagement.

colnames(consumer.dt)
spend_vars <- consumer.dt %>%
  dplyr::select(spending_on_wines, spending_on_fruits, spending_on_meat, spending_on_fish, spending_on_sweets, spending_on_goldproducts)

spend_long <- spend_vars %>% pivot_longer(cols = everything(), names_to = "Product", values_to = "Amount")
ggplot(spend_long, aes(x = Amount)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  facet_wrap(~Product, scales = "free", ncol = 3) +
  theme_minimal() +
  labs(title = "Distribution of Spending by Product Category")



############PURCHASE CHANNEL DISTRIBUTION
# Histogram of purchasing mode usage (e.g., web, catalog, store) gives insight into preferred buying channels.
# Helps identify which channels are most and least utilized by the customer base.

purchasemode_vars <- consumer.dt %>%
  dplyr::select(num_of_deals_purchases, num_of_web_purchases, num_of_catalog_purchases, num_of_store_purchases, website_visits_per_month)

purchasemode_long <- purchasemode_vars %>% tidyr::pivot_longer(cols = everything(), names_to = "Channel", values_to = "Count")
ggplot(purchasemode_long, aes(x = Count)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "white") +
  facet_wrap(~Channel, scales = "free", ncol = 3) +
  theme_minimal() +
  labs(title = "Distribution of Purchasing Modes")

######CAMPAIGN RESPONSES
# Visualizing acceptance rates across multiple marketing campaigns.
# This identifies which campaigns had higher engagement and helps assess historical campaign performance.

campaign_vars <- consumer.dt %>%
  dplyr::select(accepted_camp_1, accepted_camp_2, accepted_camp_3, accepted_camp_4, accepted_camp_5, last_campaign_response)

campaign_long <- campaign_vars %>% tidyr::pivot_longer(cols = everything(), names_to = "Campaign", values_to = "Accepted")
colnames(consumer.dt)

campaign_counts <- campaign_long %>%
  dplyr::group_by(Campaign, Accepted) %>%
  dplyr::summarise(Count = n(), .groups = "drop")

ggplot(campaign_counts, aes(x = as.factor(Accepted), y = Count, fill = Campaign)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = Count), vjust = -0.5, size = 3.5) +
  facet_wrap(~Campaign, ncol = 3) +
  theme_minimal() +
  labs(title = "Distribution of Campaign Responses", x = "Accepted (0 = No, 1 = Yes)", y = "Count")


####################### FEATURE ENGINEERING AND TRANSFORMATION ###################

# Extracting the join year allows time-based insights and helps analyze enrollment trends over the years.

consumer.dt$join_year <- lubridate::year(consumer.dt$join_date_customer)
a <- ggplot(consumer.dt, aes(x = as.factor(join_year))) +
  geom_bar(fill = "steelblue") +
  theme_minimal() +
  labs(title = "Customer Enrollment by Year", x = "Join Year", y = "Count")

#########CREATING FLAG FEATURES FOR OUTLIERS EXISTING VARIABLES 

#Individual binary flags are created for numeric variables with high variability using the IQR method.
# These flags identify customers with extreme behavior (e.g., very high spending or visits).
# A combined 'special_customer' flag aggregates these individual outliers to capture overall exceptionality.
# Convert outlier indicators and the special_customer flag to categorical to support modeling and profiling.

# Age at enrollment is derived using the difference between join year and birth year — a meaningful demographic metric.
# The 'children' variable is created by summing kids and teens at home to reflect total household responsibility.

# Total spending and total purchase variables are aggregated from their respective components for simplicity and interpretability.

flag_outliers <- function(x) {
  if (!is.numeric(x)) return(rep(0, length(x)))
  qnt <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
  iqr <- qnt[2] - qnt[1]
  lower_bound <- qnt[1] - 1.5 * iqr
  upper_bound <- qnt[2] + 1.5 * iqr
  as.integer(x < lower_bound | x > upper_bound)
}

# Important numeric variables you selected
vars_to_flag <- c(
  "spending_on_wines",
  "spending_on_fruits",
  "spending_on_meat",
  "spending_on_fish",
  "spending_on_sweets",
  "spending_on_goldproducts",
  "num_of_deals_purchases",
  "num_of_web_purchases",
  "num_of_catalog_purchases",
  "website_visits_per_month"
)

for (var in vars_to_flag) {
  new_var <- paste0(var, "_outlier")
  consumer.dt[[new_var]] <- flag_outliers(consumer.dt[[var]])
}
consumer.dt$special_customer <- apply(
  consumer.dt[, paste0(vars_to_flag, "_outlier")],
  1,
  function(x) as.integer(any(x == 1))
)
consumer.dt <- consumer.dt %>%
  dplyr::mutate(across(ends_with("_outlier") | special_customer, as.factor))

consumer.dt$age_at_enrollment
consumer.dt <- consumer.dt %>%
  dplyr::mutate(
    birth_year_num = year(birth_year),  # extract year
    age_at_enrollment = join_year - birth_year_num
  )

consumer.dt <- dplyr::mutate(consumer.dt, age_at_enrollment = join_year - year(birth_year))
consumer.dt <- dplyr::mutate(consumer.dt, children = kids_at_home + teens_at_home)

consumer.dt <- dplyr::mutate(
  consumer.dt, 
  total_monetary_value = spending_on_wines + spending_on_fruits + spending_on_meat +
    spending_on_fish + spending_on_sweets + spending_on_goldproducts
)

consumer.dt <- dplyr::mutate(
  consumer.dt,
  total_purchase_count = num_of_deals_purchases + num_of_web_purchases +
    num_of_catalog_purchases + num_of_store_purchases
)


###########NEW RESPONSE VARIABLE BY AGGREGATING ALL THE CAMPIAGN REPONSES 
#A new binary target variable 'campaign_response' is created by aggregating acceptance across all campaigns.
# It captures whether a customer responded to any campaign (1) or not (0), useful for classification tasks.

consumer.dt <- consumer.dt %>%
  dplyr::mutate(
    campaign_response = if_else(
      rowSums(
        select(., accepted_camp_1, accepted_camp_2, accepted_camp_3, accepted_camp_4, accepted_camp_5, last_campaign_response) %>%
          mutate(across(everything(), ~ as.numeric(as.character(.))))
      ) > 0,
      1,
      0
    )
  )

table(consumer.dt$campaign_response)

######INCOME 
#Income is segmented into three quantile-based categories: Low, Medium, and High.
# This helps in simplifying the analysis of spending and response behavior across income levels.
# Final data prep step: convert newly created categorical variables (like income_bracket, join_year, complain, campaign_response) to factor type to prepare for modeling.

consumer.dt <- dplyr::mutate(consumer.dt,
                             income_bracket = dplyr::case_when(
                               income < quantile(income, 0.33, na.rm = TRUE) ~ "Low",
                               income < quantile(income, 0.67, na.rm = TRUE) ~ "Medium",
                               TRUE ~ "High"
                             )
)

income_quantiles <- quantile(consumer.dt$income, probs = c(0.33, 0.67), na.rm = TRUE)
income_quantiles
table(consumer.dt$income_bracket)

consumer.dt <- consumer.dt %>% dplyr:: mutate(
  income_bracket = as.factor(income_bracket),  
  join_year = as.factor(join_year),            
  complain = as.factor(complain)               
)
consumer.dt$campaign_response <- as.factor(consumer.dt$campaign_response)
str(consumer.dt)

###################### MISSING VALUES CHECK AFTER TRANSFORMATION ####################
missing_counts <- colSums(is.na(consumer.dt))
missing_counts

################################# PREDICTOR RELEVANCY ######################################

###########################CORRELATION MATRIX
# First, all numeric variables from the dataset are selected to evaluate their linear relationships.
# To ensure accurate results, non-informative constant variables such as 'z_revenue' and 'z_costcontact' are excluded.
# A Pearson correlation matrix is then computed using only rows without missing values to avoid skewed correlations.
# Finally, a heatmap is created from the matrix to visually assess the strength and direction of relationships between variables.
# This visualization aids in detecting multicollinearity and patterns among features for potential model improvements.

numeric_vars <- consumer.dt %>%
  dplyr::select(where(is.numeric))

numeric_vars_filtered <- numeric_vars %>%
  dplyr::select(-z_revenue, -z_costcontact)

cor_matrix <- stats::cor(numeric_vars_filtered, use = "complete.obs")
melted_cor <- reshape2::melt(cor_matrix)

ggplot(melted_cor, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 2)), size = 3, color = "black") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name = "Pearson\nCorrelation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10)) +
  coord_fixed() +
  labs(title = "Full Correlation Heatmap (excluding constant variables)", x = "", y = "")



######################## VARIABLE RELEVANCY FOR CLASSIFICATION
#Campaign response is explored against several features (demographics, purchasing behavior, etc.)
# to understand how different customer traits relate to their likelihood of responding to campaigns.

consumer.dt <- consumer.dt %>%
  dplyr::mutate(campaign_response = as.factor(campaign_response))

######### DEMOGRAPHICS VS CAMPPAIGN RESPONSE
# Education vs Campaign Response
p_edu <- ggplot(consumer.dt, aes(x = education, fill = campaign_response)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(
       values = c("0"  = "steelblue",
               "1" = "black")
  )+
  labs(title = "Campaign Response by Education", 
       x = "Education", 
       y = "Proportion", 
       fill = "Response")

# Marital Status vs Campaign Response
p_marital <- ggplot(consumer.dt, aes(x = marital_status, fill = campaign_response)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(
    values = c("0"  = "steelblue",
               "1" = "black")
  )+
  labs(title = "Campaign Response by Marital Status", 
       x = "Marital Status", 
       y = "Proportion", 
       fill = "Response")

# Income Bracket vs Campaign Response
p_income <- ggplot(consumer.dt, aes(x = income_bracket, fill = campaign_response)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(
    values = c("0"  = "steelblue",
               "1" = "black")
  )+
  labs(title = "Campaign Response by Income Bracket", 
       x = "Income Bracket", 
       y = "Proportion", 
       fill = "Response")

# Join Year vs Campaign Response
p_joinyear <- ggplot(consumer.dt, aes(x = join_year, fill = campaign_response)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(
    values = c("0"  = "steelblue",
               "1" = "black")
  )+
  labs(title = "Campaign Response by Join Year", 
       x = "Join Year", 
       y = "Proportion", 
       fill = "Response")

# Complain vs Campaign Response
p_complain <- ggplot(consumer.dt, aes(x = complain, fill = campaign_response)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(
    values = c("0"  = "steelblue",
               "1" = "black")
  )+
  labs(title = "Campaign Response by Complaint Status", 
       x = "Complaint", 
       y = "Proportion", 
       fill = "Response")

#Number of Children vs. Campaign Response
p_children <- ggplot(consumer.dt, aes(x = factor(children), fill = campaign_response)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(
    values = c("0"  = "steelblue",
               "1" = "black")
  )+
  labs(title = "Campaign Response by Number of Children",
       x = "Number of Children",
       y = "Proportion",
       fill = "Campaign Response")
p_age <- ggplot(consumer.dt, aes(x = factor(campaign_response), y = age_at_enrollment, fill = campaign_response)) +
  geom_boxplot(fill = "steelblue") +
  labs(title = "Age at Enrollment vs. Campaign Response",
       x = "Campaign Response",
       y = "Age at Enrollment") +
  theme_minimal() +
  theme(legend.position = "none")

gridExtra::grid.arrange(p_edu, p_marital, p_income, p_joinyear, p_complain, p_children, p_age,  ncol = 2)

######### PURCHASING MODES  vs CAMPAIGN REPSONSE

channels <- c("num_of_deals_purchases", "num_of_web_purchases", 
              "num_of_catalog_purchases", "num_of_store_purchases", 
              "website_visits_per_month")

# Reshape and summarize: calculate the mean count per mode for each campaign response group
mode_summary <- consumer.dt %>% 
  select(campaign_response, all_of(channels)) %>% 
  pivot_longer(
    cols      = all_of(channels),    
    names_to  = "mode", 
    values_to = "count"
  ) %>%
  group_by(campaign_response, mode) %>%
  summarise(
    mean_count = mean(count, na.rm = TRUE),
    .groups    = "drop"
  )

# Create a grouped bar chart
p_modes_simple <- ggplot(mode_summary, 
                         aes(x = mode, 
                             y = mean_count, 
                             fill = factor(campaign_response))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(
    name   = "Campaign Response",
    values = c("0" = "black",
               "1" = "steelblue"),
    labels = c("0" = "No Response",
               "1" = "Responded")
  ) +
  labs(title = "Mean Usage of Purchase Modes by Campaign Response",
       x     = "Mode of Interaction",
       y     = "Average Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p_modes_simple

########spending var vs campaign response

spending_vars <- c("spending_on_wines", "spending_on_fruits", "spending_on_meat", "spending_on_fish", "spending_on_sweets", "spending_on_goldproducts")
spending_data <- consumer.dt %>%
  select(campaign_response, all_of(spending_vars)) %>%
  pivot_longer(
    cols      = all_of(spending_vars),    # <-- wrap here
    names_to  = "spending_category",
    values_to = "amount"
  ) %>%
  group_by(campaign_response, spending_category) %>%
  summarise(
    mean_amount = mean(amount, na.rm = TRUE),
    .groups     = "drop"
  )

#mean spending by campaign response
p_spending <- ggplot(spending_data,
                     aes(x = spending_category,
                         y = mean_amount,
                         fill = factor(campaign_response))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(
    name   = "Campaign Response",
    values = c("0" = "black",
               "1" = "steelblue")
  ) +
  labs(title = "Mean Spending by Campaign Response",
       x     = "Spending Category",
       y     = "Mean Amount") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p_spending

# days_since_last_purchase by Campaign Response
my_fill_scale <- scale_fill_manual(
  name   = "Campaign Response",
  values = c(
    "0" = "steelblue",
    "1" = "black"
  )
)

p_recency_density <- ggplot(consumer.dt, 
                            aes(x = days_since_last_purchase, 
                                fill = factor(campaign_response))) +
  geom_density(alpha = 0.5) +
  my_fill_scale +
  labs(title = "Density of Days Since Last Purchase by Campaign Response",
       x     = "Days Since Last Purchase",
       y     = "Density") +
  theme_minimal()

p_monetary_density <- ggplot(consumer.dt, 
                             aes(x = total_monetary_value, 
                                 fill = factor(campaign_response))) +
  geom_density(alpha = 0.5) +
  my_fill_scale +
  labs(title = "Density of Total Monetary Value by Campaign Response",
       x     = "Total Monetary Value",
       y     = "Density") +
  theme_minimal()

p_purchase_density <- ggplot(consumer.dt, 
                             aes(x = total_purchase_count, 
                                 fill = factor(campaign_response))) +
  geom_density(alpha = 0.5) +
  my_fill_scale +
  labs(title = "Density of Total Purchase Count by Campaign Response",
       x     = "Total Purchase Count",
       y     = "Density") +
  theme_minimal()

grid.arrange(p_recency_density, p_monetary_density, p_purchase_density, ncol = 3)

# campaign response rate by special customer
ggplot(consumer.dt, aes(x = special_customer, fill = as.factor(campaign_response))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("steelblue", "black"), name = "Campaign Response", labels = c("No", "Yes")) +
  labs(
    title = "Campaign Response Rate by Special Customer Status",
    x = "Special Customer (0 = No, 1 = Yes)",
    y = "Percentage of Customers"
  ) +
  theme_minimal()



######################### PREDICTOR RELEVANCY FOR REGRESISON

###############NUMERICAL VARIABLES AS PREDICTORS - Retrieved from correlation matrix

################CATEGORICAL  VARIABLES AS PREDICTORS


#Education vs. Total Monetary Value
z1 <- ggplot(consumer.dt, aes(x = education, y = total_monetary_value)) +
  geom_jitter(width = 0.2, alpha = 0.5, color = "steelblue") +
  labs(title = "Total Monetary Value by Education",
       x = "Education",
       y = "Total Monetary Value") +
  theme_minimal()

# Marital Status vs. Total Monetary Value
z2 <- ggplot(consumer.dt, aes(x = marital_status, y = total_monetary_value)) +
  geom_jitter(width = 0.2, alpha = 0.5, color = "lightblue") +
  labs(title = "Total Monetary Value by Marital Status",
       x = "Marital Status",
       y = "Total Monetary Value") +
  theme_minimal()

# Income Bracket vs. Total Monetary Value
z3 <- ggplot(consumer.dt, aes(x = income_bracket, y = total_monetary_value)) +
  geom_jitter(width = 0.2, alpha = 0.5, color = "navy") +
  labs(title = "Total Monetary Value by Income Bracket",
       x = "Income Bracket",
       y = "Total Monetary Value") +
  theme_minimal()


# Special Customer vs Total Monetary Value
z4 <- ggplot(consumer.dt, aes(x = special_customer, y = total_monetary_value)) +
  geom_jitter(width = 0.2, alpha = 0.5, color = "blue") +
  labs(title = "Total Monetary Value by Special Customer Flag",
       x = "Special Customer (0 = No, 1 = Yes)",
       y = "Total Monetary Value") +
  theme_minimal()

grid.arrange(z1, z2, z3, z4)

############################# DIMENSION REDUCTION #############################
# Constant variables like 'z_costcontact' and 'z_revenue' are removed since they don't vary across observations 
# and offer no predictive power to the model.

# The 'id' column is dropped because it serves as a unique identifier with no analytical value and can interfere with modeling due to high cardinality.

# Outlier flags for individual variables (e.g., spending or purchases) are removed after creating 
# a consolidated 'special_customer' flag, to reduce redundancy and simplify the feature space.

# Date-related and campaign-specific variables are dropped after being transformed into meaningful 
# features (like 'age_at_enrollment', 'campaign_response'), so the original columns are no longer needed.

# The final dataset contains only the most informative and transformed variables for downstream modeling,
# improving efficiency and reducing noise.


consumer.dt <- consumer.dt %>%
  dplyr::select(-z_costcontact, -z_revenue)

consumer.dt <- consumer.dt %>%
  dplyr::select(-id)
colnames(consumer.dt)

columns_to_remove <- paste0(vars_to_flag, "_outlier")
consumer.dt <- consumer.dt %>% select(-all_of(columns_to_remove))

consumer.dt <- dplyr::select(consumer.dt, -join_date_customer, -birth_year, -accepted_camp_1, -accepted_camp_2, -accepted_camp_3, -accepted_camp_4, -accepted_camp_5 )
consumer.dt <- dplyr::select(consumer.dt, -join_year, -birth_year_num, -last_campaign_response)
str(consumer.dt)

####################### LABEL ENCODING FOR EDUCATION AND INCOME BRACKET AND DUMMY COLS FOR MARITAL STATUS
#Convert 'education' into numeric labels for modeling: Basic = 0, Graduate = 1, PhD = 2.
# This transformation helps algorithms interpret ordinal relationships.

# Encode 'income_bracket' with numeric levels: Low = 0, Medium = 1, High = 2.
# Numeric encoding maintains the order of income levels for linear models or tree-based models.

# Convert 'marital_status' into dummy variables for each category.
# To avoid the dummy variable trap (perfect multicollinearity), the first dummy is dropped automatically.
# This makes the variable suitable for regression models and other machine learning algorithms.


consumer.dt <- consumer.dt %>%
  dplyr::mutate(education = case_when(
    education == "Basic" ~ 0,
    education == "graduate" ~ 1,
    education == "PhD" ~ 2
  ))
consumer.dt <- consumer.dt %>%
  dplyr::mutate(income_bracket = case_when(
    income_bracket == "Low" ~ 0,
    income_bracket == "Medium" ~ 1,
    income_bracket == "High" ~ 2,
    TRUE ~ NA_real_  # catch any unexpected category
  ))

consumer.dt <- consumer.dt %>%
  fastDummies::dummy_cols(select_columns = "marital_status",
                          remove_selected_columns = TRUE,  
                          remove_first_dummy = TRUE)      
str(consumer.dt)


############################## FEATURE SELECTION ##############################

####### FOR SEGMENTATION
#all the individual variables are considered  for understanding patterns which includes 
#consumer demographics, spending habits, purchasing patterns, marketing campaign reponse
#Categorical variables like education, income bracket, and marital status dummies are converted to factor type
# to ensure correct handling in the classification model.

segmentation_data <- consumer.dt %>%
  dplyr::select(
    # Demographics
    education, marital_status_Divorced, marital_status_Single, marital_status_Widow,
    income_bracket, age_at_enrollment, kids_at_home, teens_at_home, complain, days_since_last_purchase, 
    # Product Spending Details
    spending_on_wines, spending_on_fruits, spending_on_meat, spending_on_fish, spending_on_sweets, spending_on_goldproducts,
    # Purchase mode 
    num_of_deals_purchases, num_of_web_purchases, num_of_catalog_purchases, num_of_store_purchases, website_visits_per_month,
    special_customer, campaign_response
  )
str(segmentation_data)

############################ FOR CLASSIFICATION 
#A random forest model is trained on the complete processed dataset using all available features 
# to evaluate feature importance for predicting marketing campaign response.
# The model is set with 300 trees and importance = TRUE to extract and visualize variable importance using varImpPlot().

set.seed(123)
rf_model_all <- randomForest::randomForest(campaign_response ~ ., 
                             data = consumer.dt,
                             importance = TRUE,
                             ntree = 300)
rf_model_all
randomForest::varImpPlot(rf_model_all)

str(consumer.dt)
classification_data <- consumer.dt %>%
  select(
    # Spending and Purchase
    total_monetary_value, total_purchase_count,
    
    # Demographic
    age_at_enrollment, children, education, income_bracket,
    
    # Behavior
    days_since_last_purchase, complain, special_customer,
    
    # Marital Status
    marital_status_Single, marital_status_Divorced, marital_status_Widow,
    
    # Target Variable
    campaign_response
  )

str(classification_data)
classification_data$children = as.factor(classification_data$children)
classification_data$income_bracket = as.factor(classification_data$income_bracket)
classification_data$marital_status_Single = as.factor(classification_data$marital_status_Single)
classification_data$marital_status_Divorced = as.factor(classification_data$marital_status_Divorced)
classification_data$marital_status_Widow = as.factor(classification_data$marital_status_Widow)
classification_data$education = as.factor(classification_data$education)


################################# FOR REGRESSION

#A Boruta algorithm is applied to identify relevant predictors for modeling total monetary value (regression target).
# Boruta uses a random forest backend to iteratively evaluate the importance of each variable, including shadow variables for robustness.
# The TentativeRoughFix function is applied to resolve variables with tentative importance status, finalizing the selection.

# Based on the Boruta output and correlation matrix analysis, a refined set of features is chosen for the regression model:
# - total_purchase_count, income, children, education, website_visits_per_month, special_customer, campaign_response
# These cover a mix of behavioral, demographic, and campaign-related predictors.

# The data is then subset into a new dataset (`regression_data`) including only the selected predictors and the target variable.

set.seed(123)
boruta_output <- Boruta::Boruta(
  total_monetary_value ~ .,
  data = consumer.dt,
  doTrace = 2,      
  maxRuns = 50     
)

boruta_output
boruta_output_final <- Boruta::TentativeRoughFix(boruta_output)
final_decision <- Boruta::attStats(boruta_output_final)
final_decision

regression_data <- consumer.dt %>%
  dplyr::select(
    total_purchase_count,
    income,
    children,
    education,
    website_visits_per_month,
    special_customer,
    total_monetary_value,
    campaign_response

  )

str(regression_data)

############# STANDARDIZATION FOR SEGMENTATION DATA
#the selected segmentation data is converted into numeric and then standardized using scale()

segmentation_data <- segmentation_data %>%
  dplyr::mutate(across(everything(), as.numeric))
segmentation_data <- as.data.frame(scale(segmentation_data))
head(segmentation_data)

################### DATA PARTITIONING #################
#############for classification
#The classification dataset (balanced using ROSE) is split using stratified sampling:
# 70% of the data is used for training the model to learn campaign response patterns.
# The remaining 30% is evenly split into:
#15% validation set – used to fine-tune and evaluate model performance before final testing.
#15% test set – used to assess the model's generalization on unseen data.
#Stratified sampling ensures the class distribution of the target variable is maintained across splits.
  

set.seed(123)  
trainIndex_class <- caret::createDataPartition(classification_data$campaign_response, p = 0.70, list = FALSE)
train_data_class <- classification_data[trainIndex_class, ]
temp_data_class <- classification_data[-trainIndex_class, ]

validationIndex_class <- caret::createDataPartition(temp_data_class$campaign_response, p = 0.5, list = FALSE)
validation_data_class <- temp_data_class[validationIndex_class, ]
test_data_class <- temp_data_class[-validationIndex_class, ]

cat("Classification Data Partition Sizes:\n")
cat("Train:", nrow(train_data_class), "\n")
cat("Validation:", nrow(validation_data_class), "\n")
cat("Test:", nrow(test_data_class), "\n")

table(classification_data$campaign_response)

############## OVERSAMPLING WIHH ROSE FOR TRAIN SET ############
set.seed(123)   

rose_train <- ROSE::ROSE(
  campaign_response ~ ., 
  data = train_data_class,
  seed  = 123,                   
  N     = nrow(train_data_class)  
  )$data                            

table(train_data_class$campaign_response)
table(rose_train$campaign_response)

prop.table(table(train_data_class$campaign_response))          
prop.table(table(rose_train$campaign_response))                



############### FOR REGRESSION
#The regression dataset is split into:
#70% training set – for learning relationships that predict total monetary value.
# 15% validation set and 15% test set – to tune and evaluate the model.
# Since regression targets are continuous, regular random partitioning is sufficient (no stratification needed).
set.seed(123) 
trainIndex_reg <- caret::createDataPartition(regression_data$total_monetary_value, p = 0.70, list = FALSE)
train_data_reg <- regression_data[trainIndex_reg, ]
remaining_data_reg <- regression_data[-trainIndex_reg, ]

validationIndex_reg <- caret::createDataPartition(remaining_data_reg$total_monetary_value, p = 0.5, list = FALSE)
validation_data_reg <- remaining_data_reg[validationIndex_reg, ]
test_data_reg <- remaining_data_reg[-validationIndex_reg, ]

cat("Regression Data Partition Sizes:\n")
cat("Train:", nrow(train_data_reg), "\n")
cat("Validation:", nrow(validation_data_reg), "\n")
cat("Test:", nrow(test_data_reg), "\n")


############################# MODEL FITITNG #############################
###################### CUSTOMER SEGMENTATION USING K-MEANS ######################

# Step 1: Optimal number of clusters was determined using the Elbow Method,
# which evaluates the Within-Cluster Sum of Squares (WSS) for different values of K.
# A noticeable "elbow" at K = 3 indicated the optimal number of clusters.

# Step 2: K-Means clustering with K = 3 was performed on the standardized segmentation data,
# grouping customers based on demographic, behavioral, and transactional patterns.

# Step 3: PCA (Principal Component Analysis) was applied for dimensionality reduction,
# and the first two principal components were used for visualizing clusters.

# Step 4: A convex hull was drawn around each cluster in the PCA plot to visually highlight
# cluster boundaries and separation in the reduced space.

# Step 5: Cluster centroids were extracted and visualized using a grouped bar plot,
# displaying the standardized mean of each feature per cluster, helping interpret
# dominant characteristics of each customer segment.
wss <- sapply(1:10, function(k){
  stats::kmeans(segmentation_data, centers = k, nstart = 25)$tot.withinss
})

ROCR::plot(1:10, wss, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters K", ylab = "Total Within-Cluster Sum of Squares",
     main = "Elbow Method For Optimal K")

set.seed(123)  
kmeans_result <- kmeans(segmentation_data, centers = 3, nstart = 25)

consumer.dt$cluster <- as.factor(kmeans_result$cluster)
table(consumer.dt$cluster)

pca <- stats::prcomp(segmentation_data)
pca_data <- data.frame(PC1 = pca$x[, 1],
                       PC2 = pca$x[, 2],
                       Cluster = factor(kmeans_result$cluster))
pca$rotation
pca$rotation[, 1:2]

head(pca$x)
summary(pca)

hull_data <- pca_data %>%
  group_by(Cluster) %>%
  slice(chull(PC1, PC2))  

ggplot(pca_data, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_polygon(data = hull_data, aes(x = PC1, y = PC2, fill = Cluster),
               color = "red", alpha = 0.2, show.legend = FALSE) +
  labs(title = "Customer Clusters with Borders (Convex Hulls)",
       subtitle = "K-Means Clusters Visualized in PCA Space",
       x = "Principal Component 1", y = "Principal Component 2") 

cluster_centers <- as.data.frame(kmeans_result$centers)
cluster_centers$cluster <- factor(rownames(cluster_centers))


cluster_centers_long <- tidyr::pivot_longer(cluster_centers, -cluster, names_to = "Variable", values_to = "Mean")

ggplot(cluster_centers_long, aes(x = Variable, y = Mean, fill = cluster)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Cluster Centroids by Feature (Standardized)",
       x = "Features", y = "Standardized Value") +
  theme_minimal() +
  scale_fill_manual(values = c("red", "lightgreen", "steelblue")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold", hjust = 0.5))

######################## DECISION TREE FOR CLASSIFICATION
# Step 1: Built two decision tree models:
# One with default parameters to establish a baseline.
# One with tuned parameters (minsplit = 5, cp = 0.0005, maxdepth = 5) to improve depth and control overfitting.

# Step 2: Visualized both tree structures to interpret decision paths and key predictors influencing the campaign response.

# Step 3: Evaluated both models on validation and test datasets using:
# Confusion Matrix: to assess accuracy, sensitivity, and specificity.
# AUC (Area Under Curve): to measure classification performance.

# Step 4: Compared models using a summary table of AUC scores.
# Higher AUC values indicate better discriminatory power.

# Step 5: Plotted ROC curves for visual comparison of the models on both validation and test datasets,
# highlighting performance differences between default and tuned models

# Fit default decision tree model
fit <- rpart::rpart(campaign_response ~ ., data = rose_train)

# Fit improved decision tree model with tuning
fit2 <- rpart::rpart(campaign_response ~ ., data = rose_train,
              control = rpart.control(minsplit = 5, cp = 0.0005, maxdepth = 4))

rpart.plot::rpart.plot(fit,
           main = "Decision Tree - Default Parameters",
           extra = 104,      # display prob + percentage
           type = 2,         # label all nodes
           box.palette = "Blues",
           cex = 0.8)        # adjust text size
rpart.plot::rpart.plot(fit2,
           main = "Decision Tree - Tuned Parameters",
           extra = 104,
           type = 2,
           box.palette = "Blues",
           cex = 0.6)


val_pred_fit <- stats::predict(fit, validation_data_class, type = "class")
val_pred_fit2 <- stats::predict(fit2, validation_data_class, type = "class")


caret::confusionMatrix(val_pred_fit, validation_data_class$campaign_response, positive = "1")
caret::confusionMatrix(val_pred_fit2, validation_data_class$campaign_response, positive = "1")

val_prob_fit <- stats::predict(fit, validation_data_class, type = "prob")[, 2]
val_prob_fit2 <- stats::predict(fit2, validation_data_class, type = "prob")[, 2]

auc_val_fit <-  ROCR::performance(prediction(val_prob_fit, validation_data_class$campaign_response), "auc")@y.values[[1]]
auc_val_fit2 <-  ROCR::performance(prediction(val_prob_fit2, validation_data_class$campaign_response), "auc")@y.values[[1]]

test_pred_fit <- stats::predict(fit, test_data_class, type = "class")
test_pred_fit2 <- stats::predict(fit2, test_data_class, type = "class")

caret::confusionMatrix(test_pred_fit, test_data_class$campaign_response, positive = "1")
caret::confusionMatrix(test_pred_fit2, test_data_class$campaign_response, positive = "1")

test_prob_fit <- stats::predict(fit, test_data_class, type = "prob")[, 2]
test_prob_fit2 <- stats::predict(fit2, test_data_class, type = "prob")[, 2]

auc_test_fit <- ROCR::performance(prediction(test_prob_fit, test_data_class$campaign_response), "auc")@y.values[[1]]
auc_test_fit2 <- ROCR::performance(prediction(test_prob_fit2, test_data_class$campaign_response), "auc")@y.values[[1]]


results <- data.frame(
  Model = c("Decision Tree (Default)", "Decision Tree (Tuned)"),
  Validation_AUC = c(auc_val_fit, auc_val_fit2),
  Test_AUC = c(auc_test_fit, auc_test_fit2)
)

kable(results, caption = "Validation and Test Performance for Decision Trees")

roc_val_fit <- pROC::roc(validation_data_class$campaign_response, val_prob_fit)
ROCR::plot(roc_val_fit, main = "ROC Curve - Default Tree (Validation)")

roc_val_fit2 <- pROC::roc(validation_data_class$campaign_response, val_prob_fit2)
ROCR::plot(roc_val_fit2, main = "ROC Curve - Tuned Tree (Validation)")

roc_test_fit <- pROC::roc(test_data_class$campaign_response, test_prob_fit)
ROCR::plot(roc_test_fit, main = "ROC Curve - Default Tree (Test)", col = "black")

roc_test_fit2 <- pROC::roc(test_data_class$campaign_response, test_prob_fit2)
ROCR::plot(roc_test_fit2, main = "ROC Curve - Tuned Tree (Test)", col = "black")

############################## MUTLIPLE LINEAR REGRESSION ##################################
#A multiple linear regression model was trained to predict customer spending ('total_monetary_value') 
# using a combination of demographic, behavioral, and purchase-related features.

# Model predictions were evaluated on both validation and test sets using RMSE, MAE, and R² metrics.
# This helps assess the average prediction error, absolute deviation, and how much variance the model explains.

# A summary table was generated to compare model performance across both datasets.

lm_model <- stats::lm(total_monetary_value ~ ., data = train_data_reg)
summary(lm_model)

lm_pred_val <- stats::predict(lm_model, newdata = validation_data_reg)
actual_val <- validation_data_reg$total_monetary_value

lm_pred_test <- stats::predict(lm_model, newdata = test_data_reg)
actual_test <- test_data_reg$total_monetary_value


#Validation Set 
rmse_val <- Metrics::rmse(actual_val, lm_pred_val)
mae_val <- Metrics::mae(actual_val, lm_pred_val)
rsq_val <- stats::cor(actual_val, lm_pred_val)^2

#Test Set 
rmse_test <- Metrics::rmse(actual_test, lm_pred_test)
mae_test <- Metrics::mae(actual_test, lm_pred_test)
rsq_test <- stats::cor(actual_test, lm_pred_test)^2

cat( "Linear Regression Performance:\n")
cat("Validation Set:\n")
cat("  RMSE:", round(rmse_val, 2), "\n")
cat("  MAE :", round(mae_val, 2), "\n\n")

cat("Test Set:\n")
cat("  RMSE:", round(rmse_test, 2), "\n")
cat("  MAE :", round(mae_test, 2), "\n")
cat("  R²  :", round(rsq_test, 4), "\n")


results_df <- data.frame(
  Dataset = c("Validation", "Test"),
  RMSE = c(round(rmse_val, 2), round(rmse_test, 2)),
  MAE  = c(round(mae_val, 2), round(mae_test, 2)),
  R_squared = c(round(rsq_test, 4), round(rsq_test, 4))
)
knitr::kable(results_df, caption = "Linear Regression Performance on Validation and Test Sets")














