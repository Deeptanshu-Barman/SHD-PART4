library(missForest)

#CHANGE THIS PATH
setwd("C:/Users/Deeptanshu Barman/Desktop/SHD+PART4")

file1 <- "files/Folder_1@871405@871405_v2@result_merge.csv@"
file2 <- "files/Folder_1@1545611@1545611_v1@Book1.csv@"

data1 <- read.csv(file1)
data2 <- read.csv(file2)

# Function to perform integer encoding
integer_encode <- function(data,non_numeric_cols) {
  for (col in names(data)[non_numeric_cols]) {
    unique_values <- unique(data[[col]])
    encoding <- as.integer(factor(data[[col]], levels = unique_values))
    data[[col]] <- encoding
  }
  return(data)
}

# Identify non-numeric columns (character or factor)
non_numeric_cols1 <- sapply(data1, function(x) !is.numeric(x))
non_numeric_cols2 <- sapply(data2, function(x) !is.numeric(x))
data1 <- integer_encode(data1,non_numeric_cols1)
data2 <- integer_encode(data2,non_numeric_cols2)

imputed_data1 <- missForest(data1)$ximp
imputed_data2 <- missForest(data2)$ximp

#sum(sapply(imputed_data1, is.na))
#sum(sapply(imputed_data1, is.infinite))
#sum(sapply(imputed_data1, is.nan))

library(arules)

discretize_columns <- function(data, method = "cluster") {
  for (col in names(data)) {
    distinct_values <- unique(data[[col]])
    num_distinct_values <- length(distinct_values)
    num_clusters <- max(as.integer(num_distinct_values / 2), 2)
    if (num_distinct_values > 1) {
      data[[col]] <- arules::discretize(data[[col]], method = method, breaks = num_clusters)
    }
  }
  return(data)
}
discretized_data1 <- discretize_columns(imputed_data1, method = "cluster")
discretized_data2 <- discretize_columns(imputed_data2, method = "cluster")


library(bnlearn)

convert_integers_to_factors <- function(data) {
  integer_cols <- sapply(data, is.integer)
  data[integer_cols] <- lapply(data[integer_cols], as.factor)
  return(data)
}
remove_columns_with_one_unique_value <- function(data) {
  one_unique_value_cols <- sapply(data, function(x) length(unique(x)) == 1)
  data <- data[, !one_unique_value_cols]
  return(data)
}
input1=remove_columns_with_one_unique_value(convert_integers_to_factors(discretized_data1))
input2=remove_columns_with_one_unique_value(convert_integers_to_factors(discretized_data2))
bn_structure1 <- hc(input1, score = "bic")
bn_structure2 <- hc(input2, score = "bic")

# Set up the plotting layout
par(mfrow = c(1, 2))

# Plot the first Bayesian network structure
plot(bn_structure1, main = "Bayesian Network 1")

# Plot the second Bayesian network structure
plot(bn_structure2, main = "Bayesian Network 2")

# Reset the plotting layout to the default
par(mfrow = c(1, 1))

shd_distance <- shd(bn_structure1, bn_structure2)

# Print the Structural Hamming Distance
cat("Structural Hamming Distance:", shd_distance, "\n")

print(bn_structure1)

