setwd("D:/LocalGit/Jigsaw-Assignment")
# Loading required packages
library(ggplot2)
library(reshape2)

# Reading data set from the source
hrs <- read.csv("D:/Jigsaw/Manish Singh/Assignment_Data Exploration/horse_racing-class10.csv")

# performing basic checks
str(hrs)
summary(hrs)
nrow(hrs)
ncol(hrs)
colnames(hrs)

# Converting Date format in data type date
hrs$race_date <- as.Date(hrs$race_date,"%d-%b-%y" )
str(hrs)

#Performing sumary statistics- five point summary for different numeric variables
quantile(hrs$handle)
min(hrs$handle)
max(hrs$handle)
mean(hrs$handle)
range(hrs$handle)

quantile(hrs$prize)
min(hrs$prize)
max(hrs$prize)
mean(hrs$prize)
range(hrs$prize)

quantile(hrs$minimum_claim_price)
min(hrs$minimum_claim_price)
max(hrs$minimum_claim_price)
mean(hrs$minimum_claim_price)
range(hrs$minimum_claim_price)

quantile(hrs$maximum_claim_price)
min(hrs$maximum_claim_price)
max(hrs$maximum_claim_price)
mean(hrs$maximum_claim_price)
range(hrs$maximum_claim_price)


quantile(hrs$number_of_runners)
min(hrs$number_of_runners)
max(hrs$number_of_runners)
mean(hrs$number_of_runners)
range(hrs$number_of_runners)


quantile(hrs$race_number)
min(hrs$race_number)
max(hrs$race_number)
mean(hrs$race_number)
range(hrs$race_number)

# Checking the distribution of frequency among different variables
hist(hrs$handle ~ hrs$race_date,breaks = 10, col = "grey", labels = T, main = "Handle Plots" , freq = T )
boxplot(hrs$handle, col = "grey", notch = T)

hist(hrs$prize,breaks = 10, col = "grey", labels = T, main = "Handle Plots" , freq = T )
boxplot(hrs$prize, col = "grey", notch = T)

hist(hrs$maximum_claim_price,breaks = 10, col = "grey", labels = T, main = "Handle Plots" , freq = T)
boxplot(hrs$maximum_claim_price, col = "grey", notch = T)

hist(hrs$minimum_claim_price,breaks = 10, col = "grey", labels = T, main = "Handle Plots" , freq = T )
boxplot(hrs$minimum_claim_price, col = "grey", notch = T)

hist(hrs$race_number,breaks = 10, col = "grey", labels = T, main = "Handle Plots" , freq = T )
boxplot(hrs$race_number, col = "grey", notch = T)

plot(hrs$handle ~ hrs$race_date, col = "red")
plot(hrs$prize ~ hrs$race_date, col = "red")
plot(hrs$maximum_claim_price ~ hrs$race_date, col = "red")
plot(hrs$minimum_claim_price ~ hrs$race_date, col = "red")


## Plot using GGPLOT for checking the distribution of hande for different tracks inrespect of #Race_Number
ggplot(hrs, aes(race_number,handle,colour=track_id)) + 
  geom_line() + 
  geom_point()

# Checking Missing values
sum(is.na(hrs))

# Data Transformation
# extracting month and year from the date attribute
data <- transform(hrs,month=as.numeric(format(as.Date(race_date),"%m")))
data <- transform(data,year=as.numeric(format(as.Date(race_date),"%Y")))
data$date1 <- as.yearmon(paste(data$year, data$month, sep = "-"))
data$date1 <- as.Date(data$date1)

# Aggregation for numeric variales to check the dependencies and frequencies within the relations or combinations of the different attributes.
# We can have the idea that which combination is most or least frequent in the data.
# We can also have the idea of the different attributes involvement with the data by using the aggregation. 
new.data <- aggregate(cbind(handle, prize ) ~  date1 + course_type + surface + weather + track_condition,
                     data=data,FUN=sum)

new.data1 <- aggregate(cbind(handle, prize ) ~  date1 +country + state +distance_id ,
                      data=data,FUN=sum)

new.data2 <- aggregate(cbind(minimum_claim_price) ~  date1 + course_type + surface + weather + track_condition,
                      data=data,FUN=sum)

new.data3 <- aggregate(cbind(minimum_claim_price) ~  date1 +country + state +distance_id ,
                       data=data,FUN=sum)

# Performing multiple Regresssion analysis to check the significant variable inrespect of variable of intrest
reg <- lm(handle ~ course_type + surface + weather + track_condition + race_type +date1, data = data)
summary(reg)

reg1 <- lm(prize ~ course_type + surface + weather + track_condition + date1, data = data)
summary(reg1)

# performing covariance and correlation operations among the variable.
cor(data[sapply(data, is.numeric)])
cov(data[sapply(data, is.numeric)])

#average by different attributes for variable of interest

plot(data$handle ~ data$weather, xlab = "WEATHER", ylab = "Avg_HANDLE", main = "Average Handle by Weather")


