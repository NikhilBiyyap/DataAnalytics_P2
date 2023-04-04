trainingData <- read.csv("/Users/nikhilbiyyap/Downloads/trainingData.csv")
str(trainingData)
library(ggplot2)
library(GGally)

trainingData <- subset(trainingData, select = -id)


trainingData$destination <- as.factor(trainingData$destination)
trainingData$passanger <- as.factor(trainingData$passanger)
trainingData$weather <- as.factor(trainingData$weather)
trainingData$temperature <- as.numeric(trainingData$temperature)
trainingData$time <- as.factor(trainingData$time)
trainingData$coupon <- as.factor(trainingData$coupon)
trainingData$expiration <- as.factor(trainingData$expiration)
trainingData$gender <- as.factor(trainingData$gender)
trainingData$maritalStatus <- as.factor(trainingData$maritalStatus)
trainingData$has_children <- as.factor(trainingData$has_children)
trainingData$education <- as.factor(trainingData$education)
trainingData$Y <- as.factor(trainingData$Y)
trainingData$direction_opp <- as.factor(trainingData$direction_opp)
trainingData$direction_same <- as.factor(trainingData$direction_same)
trainingData$toCoupon_GEQ25min <- as.factor(trainingData$toCoupon_GEQ25min)
trainingData$toCoupon_GEQ15min <- as.factor(trainingData$toCoupon_GEQ15min)
trainingData$toCoupon_GEQ5min <- as.factor(trainingData$toCoupon_GEQ5min)
trainingData$age <- ifelse(trainingData$age == "50plus", 51, trainingData$age)
trainingData$age <- ifelse(trainingData$age == "below21", 20, trainingData$age)
trainingData$age <- as.numeric(trainingData$age)
trainingData$occupation <- as.factor(trainingData$occupation)
trainingData$income <- as.factor(trainingData$income)


# Assign different values with weightage to the "car" column

# Determine non-blank values
non_blank <- trainingData$car[trainingData$car != ""]

# Calculate probability of each non-blank value
prob <- table(non_blank) / length(non_blank)

# Generate random indices for blank values
blank_indices <- which(trainingData$car == "")

# Assign each blank value with a non-blank value randomly selected based on their probabilities
trainingData$car[blank_indices] <- sample(names(prob), length(blank_indices), replace = TRUE, prob = prob)

trainingData$car <- as.factor(trainingData$car)


# Assign different values with weightage to the "Bar" column

# Determine non-blank values
non_blank <- trainingData$Bar[trainingData$Bar != ""]

# Calculate probability of each non-blank value
prob <- table(non_blank) / length(non_blank)

# Generate random indices for blank values
blank_indices <- which(trainingData$Bar == "")

# Assign each blank value with a non-blank value randomly selected based on their probabilities
trainingData$Bar[blank_indices] <- sample(names(prob), length(blank_indices), replace = TRUE, prob = prob)

trainingData$Bar <- as.factor(trainingData$Bar)

# Assign different values with weightage to the "CoffeeHouse" column

# Determine non-blank values
non_blank <- trainingData$CoffeeHouse[trainingData$CoffeeHouse != ""]

# Calculate probability of each non-blank value
prob <- table(non_blank) / length(non_blank)

# Generate random indices for blank values
blank_indices <- which(trainingData$CoffeeHouse == "")

# Assign each blank value with a non-blank value randomly selected based on their probabilities
trainingData$CoffeeHouse[blank_indices] <- sample(names(prob), length(blank_indices), replace = TRUE, prob = prob)

trainingData$CoffeeHouse <- as.factor(trainingData$CoffeeHouse)

# Assign different values with weightage to the "CarryAway" column

# Determine non-blank values
non_blank <- trainingData$CarryAway[trainingData$CarryAway != ""]

# Calculate probability of each non-blank value
prob <- table(non_blank) / length(non_blank)

# Generate random indices for blank values
blank_indices <- which(trainingData$CarryAway == "")

# Assign each blank value with a non-blank value randomly selected based on their probabilities
trainingData$CarryAway[blank_indices] <- sample(names(prob), length(blank_indices), replace = TRUE, prob = prob)

trainingData$CarryAway <- as.factor(trainingData$CarryAway)

# Assign different values with weightage to the "RestaurantLessThan20" column

# Determine non-blank values
non_blank <- trainingData$RestaurantLessThan20[trainingData$RestaurantLessThan20 != ""]

# Calculate probability of each non-blank value
prob <- table(non_blank) / length(non_blank)

# Generate random indices for blank values
blank_indices <- which(trainingData$RestaurantLessThan20 == "")

# Assign each blank value with a non-blank value randomly selected based on their probabilities
trainingData$RestaurantLessThan20[blank_indices] <- sample(names(prob), length(blank_indices), replace = TRUE, prob = prob)

trainingData$RestaurantLessThan20 <- as.factor(trainingData$RestaurantLessThan20)

# Assign different values with weightage to the "Restaurant20To50" column

# Determine non-blank values
non_blank <- trainingData$Restaurant20To50[trainingData$Restaurant20To50 != ""]

# Calculate probability of each non-blank value
prob <- table(non_blank) / length(non_blank)

# Generate random indices for blank values
blank_indices <- which(trainingData$Restaurant20To50 == "")

# Assign each blank value with a non-blank value randomly selected based on their probabilities
trainingData$Restaurant20To50[blank_indices] <- sample(names(prob), length(blank_indices), replace = TRUE, prob = prob)

trainingData$Restaurant20To50 <- as.factor(trainingData$Restaurant20To50)

summary(trainingData)

pairs(auto_df[,1:8], col="navy", upper.panel = NULL)


