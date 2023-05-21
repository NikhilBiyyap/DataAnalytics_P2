# Set the path to the data file
data <- "/Users/nikhilbiyyap/Downloads/data.csv"

# Read the data
HeatFlux <- read.csv(data, header = TRUE, stringsAsFactors = FALSE)

# Remove the "author" column from the dataset
HeatFlux <- HeatFlux[, -which(names(HeatFlux) == "author")]
HeatFlux <- HeatFlux[, -which(names(HeatFlux) == "geometry")]

# Convert x_e_out column to numeric
HeatFlux$x_e_out.... <- as.numeric(HeatFlux$x_e_out....)

# Perform missing value imputation using missForest
imputed_data <- missForest(HeatFlux)

# Access the imputed data
imputed_values <- imputed_data$ximp$x_e_out

# Create a copy of HeatFlux data frame for imputed values
imputed_data1 <- HeatFlux[, c("id", "x_e_out....")]

missing_indices <- which(is.na(imputed_data1$x_e_out....))

imputed_data1$x_e_out....[missing_indices] <- imputed_values[missing_indices]

submission_data <- imputed_data1[missing_indices, c("id", "x_e_out....")]

# Find the column index with the matching name
column_index <- grep("x_e_out....", colnames(submission_data))

# Rename the column from "x_e_out..." to "x_e_out[-]"
colnames(submission_data)[column_index] <- "x_e_out [-]"

# Save the dataset as a CSV file
write.csv(submission_data, "submission_data5.csv", row.names = FALSE)
