trainingData <- read.csv("/Users/nikhilbiyyap/Downloads/trainingData.csv")
str(trainingData)
library(ggplot2)
library(GGally)

trainingData <- subset(trainingData, select = -id)

trainingData$age <- ifelse(trainingData$age == "50plus", 51, trainingData$age)
trainingData$age <- ifelse(trainingData$age == "below21", 20, trainingData$age)

# Assign different values with weightage to the "car" column
# Determine non-blank values
non_blank <- trainingData$car[trainingData$car != ""]
# Calculate probability of each non-blank value
prob <- table(non_blank) / length(non_blank)
# Generate random indices for blank values
blank_indices <- which(trainingData$car == "")
# Assign each blank value with a non-blank value randomly selected based on their probabilities
trainingData$car[blank_indices] <- sample(names(prob), length(blank_indices), replace = TRUE, prob = prob)



# Assign different values with weightage to the "Bar" column
# Determine non-blank values
non_blank <- trainingData$Bar[trainingData$Bar != ""]
# Calculate probability of each non-blank value
prob <- table(non_blank) / length(non_blank)
# Generate random indices for blank values
blank_indices <- which(trainingData$Bar == "")
# Assign each blank value with a non-blank value randomly selected based on their probabilities
trainingData$Bar[blank_indices] <- sample(names(prob), length(blank_indices), replace = TRUE, prob = prob)


# Assign different values with weightage to the "CoffeeHouse" column
# Determine non-blank values
non_blank <- trainingData$CoffeeHouse[trainingData$CoffeeHouse != ""]
# Calculate probability of each non-blank value
prob <- table(non_blank) / length(non_blank)
# Generate random indices for blank values
blank_indices <- which(trainingData$CoffeeHouse == "")
# Assign each blank value with a non-blank value randomly selected based on their probabilities
trainingData$CoffeeHouse[blank_indices] <- sample(names(prob), length(blank_indices), replace = TRUE, prob = prob)


# Assign different values with weightage to the "CarryAway" column
# Determine non-blank values
non_blank <- trainingData$CarryAway[trainingData$CarryAway != ""]
# Calculate probability of each non-blank value
prob <- table(non_blank) / length(non_blank)
# Generate random indices for blank values
blank_indices <- which(trainingData$CarryAway == "")
# Assign each blank value with a non-blank value randomly selected based on their probabilities
trainingData$CarryAway[blank_indices] <- sample(names(prob), length(blank_indices), replace = TRUE, prob = prob)


# Assign different values with weightage to the "RestaurantLessThan20" column
# Determine non-blank values
non_blank <- trainingData$RestaurantLessThan20[trainingData$RestaurantLessThan20 != ""]
# Calculate probability of each non-blank value
prob <- table(non_blank) / length(non_blank)
# Generate random indices for blank values
blank_indices <- which(trainingData$RestaurantLessThan20 == "")
# Assign each blank value with a non-blank value randomly selected based on their probabilities
trainingData$RestaurantLessThan20[blank_indices] <- sample(names(prob), length(blank_indices), replace = TRUE, prob = prob)



# Assign different values with weightage to the "Restaurant20To50" column
# Determine non-blank values
non_blank <- trainingData$Restaurant20To50[trainingData$Restaurant20To50 != ""]
# Calculate probability of each non-blank value
prob <- table(non_blank) / length(non_blank)
# Generate random indices for blank values
blank_indices <- which(trainingData$Restaurant20To50 == "")
# Assign each blank value with a non-blank value randomly selected based on their probabilities
trainingData$Restaurant20To50[blank_indices] <- sample(names(prob), length(blank_indices), replace = TRUE, prob = prob)

summary(trainingData)

#Model 1 = Linear Regression 
fit1<- lm(Y~., data = trainingData)
summary(fit1)

fit2<- lm(Y~ destination+passanger+weather+time+coupon+expiration+gender+maritalStatus+education+occupation+income+Bar+CoffeeHouse+RestaurantLessThan20+Restaurant20To50+toCoupon_GEQ15min+toCoupon_GEQ25min+direction_same, data = trainingData)
summary(fit2)

fit3<- lm(Y~ destination+passanger+weather+time+coupon+expiration+gender+maritalStatus+education+occupation+income+Bar+CoffeeHouse+Restaurant20To50+toCoupon_GEQ15min+toCoupon_GEQ25min+direction_same, data = trainingData)
summary(fit3)


destination <- trainingData$destination== "No Urgent Place"
passanger <- trainingData$passanger == "Kid(s)"
time <- trainingData$time == "6PM"
maritalStatus <- trainingData$maritalStatus == "Single"
education <- trainingData$education %in% c("Bachelors degree", "Some High School", "Graduate degree (Masters or Doctorate)")
occupation <- trainingData$occupation %in% c("Arts Design Entertainment Sports & Media","Business & Financial","Community & Social Services","Computer & Mathematical","Education&Training&Library","Food Preparation & Serving Related","Installation Maintenance & Repair","Legal","Management","Office & Administrative Support","Personal Care & Service","Production Occupations","Retired","Sales & Related","Student","Transportation & Material Moving","Unemployed")
income <- trainingData$income %in% c("$62500 - $74999","$75000 - $87499","$87500 - $99999")
Bar <- trainingData$Bar %in% c("gt8","less1","never")
CoffeeHouse <- trainingData$CoffeeHouse %in% c("gt8","less1","never")
Restaurant20To50 <- trainingData$Restaurant20To50 %in% c("less1","never")
weather <- trainingData$weather
coupon <- trainingData$coupon
expiration <- trainingData$expiration
gender <- trainingData$gender
toCoupon_GEQ15min <- trainingData$toCoupon_GEQ15min
toCoupon_GEQ25min <- trainingData$toCoupon_GEQ25min
direction_same <- trainingData$direction_same
Y <- trainingData$Y

fit4<- lm(Y~ destination+passanger+weather+time+coupon+expiration+gender+maritalStatus+education+occupation+income+Bar+CoffeeHouse+Restaurant20To50+toCoupon_GEQ15min+toCoupon_GEQ25min+direction_same)
summary(fit4)


