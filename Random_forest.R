setwd("C:/Users/91899/Downloads")
getwd()   #setting work directory properly
#--------------------------------------------------
# installing packages and calling libraries
install.packages("readxl") 
install.packages("ggplot2") 
install.packages("randomForest")
install.packages("tidyr")
library(readxl)
library(ggplot2)
library(randomForest)
library(tidyr)
#---------------------------------------------------------
data <- read_excel("C:/Users/91899/Downloads/1657875746_day.xlsx")
head(data,10)
#Structure of data
str(data)

#Missing values
missing_values <- data.frame(Missing_Values = sapply(data, function(x) sum(is.na(x))))
print(missing_values)

# Data types
variable_types <- sapply(data, function(x) class(x))
variable_types 
# Here we wont be changing the data type because all variables have numeric data type
#-------------Exploratory Data Analysis------------------------
# monthly distribution of the number of bikes rented
ggplot(data, aes(x = mnth, y = cnt, fill = factor(mnth))) +
  geom_bar(stat = "sum", position = "dodge") +
  labs(title = "Monthly Distribution of Bikes Rented",
       x = "Month",
       y = "Total Bikes Rented") +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  theme_minimal()

# Year wise distribution of the number of bikes rented
ActualYear <- ifelse(data$yr == 0, 2011, 2012)
yer=ActualYear
ggplot(data, aes(x = yer, y = cnt, fill = factor(yer))) +
  geom_bar(stat = "sum", position = "dodge") +
  labs(title = "Yearly Distribution of Bikes Rented",
       x = "Year",
       y = "Total Bikes Rented") +
	 scale_x_discrete(breaks = c("2011", "2012")) +
  theme_minimal()

#Standardization (Here standardization is done to get better visual of boxplots for numerical variables) 
normalize <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
df=data
data[c("casual","registered","cnt")] <- lapply(data[c("casual","registered","cnt")], normalize)


#Dropping the columns which are not required
data_new <- df[, -which(names(data) == "dteday")]
data_new_1 <- data_new[, -which(names(data) == "instant")]

#Creating a new data having no categorical variables
new_data <- data[, c("temp", "atemp", "registered","hum","windspeed","casual","registered","cnt")]

#Boxplot for each variable(After exluding categorical variable)
data_long <- pivot_longer(new_data, cols = everything(), names_to = "Variable", values_to = "Value")

# Create a boxplot using ggplot2
ggplot(data_long, aes(x = Variable, y = Value, fill = Variable)) +
  geom_boxplot() +
  labs(title = "Boxplots for Each Numeric Variable",
       x = "Variable",
       y = "Value") +
  theme_minimal()



#------------Splitting of data into train and test-----------------
set.seed(123)

split_ratio <- 0.8
index_train <- sample(seq_len(nrow(data_new_1)), size = split_ratio * nrow(data_new_1))
training_set <-data_new_1[index_train, ]
testing_set <- data_new_1[-index_train, ]

# ----------Creating a Random Forest model--------------------------
rf_model <- randomForest(cnt ~ ., data = training_set)


#---Making predictions and evaluating performance of the model----------
#Here we are giving the dataframe which includes predicted values by model and the required values using test data. 
predictions <- predict(rf_model, newdata = testing_set)
result_df <- data.frame(Actual = testing_set$cnt, Predicted = predictions)
(result_df)

