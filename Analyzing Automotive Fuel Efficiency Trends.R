library(ggplot2)
library(readr)
library(tidyr)
library(dplyr)

setwd("/Users/snehitamoturu/Downloads")

# Read csv file
df <- read.csv("auto-mpg.csv")

# Check for missing values in the entire dataframe
missing_values <- is.na(df)

# Summarize the total number of missing values in each column
sum_missing_values <- colSums(missing_values)

# Print the summary
print(sum_missing_values)

table(rowSums(df == "?"))

# Replace '?' with '0' in the entire dataframe
df[df == '?'] <- '0'

# Convert the 'horsepower' column to numeric
df$horsepower <- as.numeric(df$horsepower)

df$horsepower

#Checking for  outliers

# Checking for outliers using boxplots for all attributes
options(repr.plot.width=10, repr.plot.height=6) # Set plot size

# Create a list to store the outlier detection plots
plots <- list()

# Iterate through each column and create boxplot for outlier detection
for (col in names(df)) {
  if (is.numeric(df[[col]])) {
    plot <- ggplot(df, aes(y = !!sym(col))) +
      geom_boxplot() +
      labs(title = paste("Boxplot for", col))
    plots[[col]] <- plot
  }
}

# Arrange and print the outlier detection plots
library(gridExtra)
grid.arrange(grobs = plots, ncol = 2)  # Change the number of columns if needed


summary(df)


# Calculate the IQR for the 'horsepower' column
q1 <- quantile(df$horsepower, 0.25)
q3 <- quantile(df$horsepower, 0.75)
iqr <- q3 - q1

# Define upper and lower bounds for outliers
lower_bound <- q1 - 1.5 * iqr
upper_bound <- q3 + 1.5 * iqr

# Identify outliers
outliers <- df$horsepower < lower_bound | df$horsepower > upper_bound

# Count the number of outliers
num_outliers <- sum(outliers)


# Calculate the IQR for the 'horsepower' column
#q1 <- quantile(df$horsepower, 0.25)
#q3 <- quantile(df$horsepower, 0.75)
#iqr <- q3 - q1

# Define upper and lower bounds for outliers
#lower_bound <- q1 - 1.5 * iqr
#upper_bound <- q3 + 1.5 * iqr

# Identify and remove outliers
outliers <- df$horsepower < lower_bound | df$horsepower > upper_bound
df_clean <- df[!outliers, ]

nrow(df_clean)
# Calculate the number of rows in the original dataframe 'df'
original_row_count <- nrow(df)



# Find the rows that were removed as outliers
removed_outliers <- df[which(outliers), ]

# View the rows that were removed as outliers
head(removed_outliers)

### identify any remaining outliers in the cleaned data.
# Checking for outliers in the cleaned data (df_clean)
options(repr.plot.width=10, repr.plot.height=6) # Set plot size

# Create a list to store the outlier detection plots for df_clean
cleaned_plots <- list()

# Iterate through each column and create boxplot for outlier detection in df_clean
for (col in names(df_clean)) {
  if (is.numeric(df_clean[[col]])) {
    plot <- ggplot(df_clean, aes(y = !!sym(col))) +
      geom_boxplot() +
      labs(title = paste("Boxplot for", col, "in df_clean"))
    cleaned_plots[[col]] <- plot
  }
}

# Arrange and print the outlier detection plots for df_clean
library(gridExtra)
grid.arrange(grobs = cleaned_plots, ncol = 2)  # Change the number of columns if needed

##Summary Statistics: You can calculate summary statistics for both dataframes and compare them. This will give you a sense of how the central tendency and spread of the data have changed after removing outliers.
# Summary statistics for 'df' with outliers
summary(df)

# Summary statistics for 'df_clean' without outliers
summary(df_clean)


library(ggplot2)
library(dplyr)
library(tidyr)

# Create histograms for numeric columns in 'df' with outliers
histogram_df <- df %>%
  select_if(is.numeric) %>%
  gather(key = "Variable", value = "Value") %>%
  ggplot(aes(x = Value)) +
  geom_histogram(bins = 20) +
  facet_wrap(~Variable, scales = "free_x") +
  labs(title = "Histograms for df with Outliers")

# Create histograms for numeric columns in 'df_clean' without outliers
histogram_df_clean <- df_clean %>%
  select_if(is.numeric) %>%
  gather(key = "Variable", value = "Value") %>%
  ggplot(aes(x = Value)) +
  geom_histogram(bins = 20) +
  facet_wrap(~Variable, scales = "free_x") +
  labs(title = "Histograms for df_clean without Outliers")

# Combine the histograms for comparison
library(gridExtra)
grid.arrange(histogram_df, histogram_df_clean, ncol = 2)


###exploratory analysis

library(ggplot2)
ggplot(df_clean, aes(x = mpg)) +
  geom_histogram(binwidth = 2, fill = "blue", color = "black") +
  labs(title = "Distribution of MPG", x = "MPG", y = "Frequency")


ggplot(df_clean, aes(y = mpg)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Boxplot of MPG", y = "MPG")


ggplot(df_clean, aes(x = origin, fill = factor(cylinders))) +
  geom_bar(position = "dodge") +
  labs(title = "Distribution of Cylinders by Origin", x = "Origin", y = "Count")


ggplot(df_clean, aes(x = horsepower, y = mpg)) +
  geom_point() +
  labs(title = "Scatter Plot of Horsepower vs. MPG", x = "Horsepower", y = "MPG")

##correlation analysis

correlation_matrix <- cor(df_clean[, c("mpg", "cylinders", "horsepower", "weight", "acceleration")])
corrplot::corrplot(correlation_matrix, method = "color")



install.packages("ggcorrplot")

# Load the ggcorrplot library
library(ggcorrplot)

# Calculate the correlation matrix
correlation_matrix <- cor(df_clean[, c("mpg", "cylinders", "horsepower", "weight", "acceleration")])

# Create a correlation plot with values displayed
ggcorrplot(correlation_matrix, 
           type = "lower",  # Display values in the lower part of the plot
           method.args = list(cex = 0.8),  # Set the size of the text labels
           colors = c("red", "white", "blue"),  # Set color scale
           title = "Correlation Matrix"  # Set the plot title
)








