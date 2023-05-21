data = ("/Users/nikhilbiyyap/Downloads/data.csv")
HeatFlux <- read.csv(data,header = TRUE, stringsAsFactors = FALSE)

summary(HeatFlux)

sum(is.na(HeatFlux$x_e_out....))

# Install and load the necessary libraries for multiple imputation
library(mice)



# Create a temporary data frame with a dummy variable
temp_data <- data.frame(x_e_out = HeatFlux$x_e_out...., dummy = 1)

# Create an imputation model using the mice package
imp_model <- mice(temp_data, method = "norm", m = 20, maxit = 200, seed = 123)

# Generate the imputed values for the "x_e_out" column
imputed_values <- complete(imp_model)$x_e_out

# Optionally, compare the imputed values to the original missing values
original_missing <- is.na(HeatFlux$x_e_out)
imputed_missing <- is.na(imputed_values)

# Count the number of missing values before and after imputation
missing_before_imputation <- sum(original_missing)
missing_after_imputation <- sum(imputed_missing)

imputed_values <- complete(imp_model)$x_e_out

imputed_data <- HeatFlux[, c("id", "x_e_out....")]

missing_indices <- which(is.na(imputed_data$x_e_out....))

imputed_data$x_e_out....[missing_indices] <- imputed_values[missing_indices]

submission_data <- imputed_data[missing_indices, c("id", "x_e_out....")]

# Find the column index with the matching name
column_index <- grep("x_e_out....", colnames(submission_data))

# Rename the column from "x_e_out..." to "x_e_out[-]"
colnames(submission_data)[column_index] <- "x_e_out [-]"

# Save the dataset as a CSV file
write.csv(submission_data, "submission_data4.csv", row.names = FALSE)















