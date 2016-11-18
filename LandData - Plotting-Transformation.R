
# #Reading Data -----------------------------------------------------------

housing  <- read.csv("C:/Users/Gaurav Kishore/Downloads/landdata-states-2016q1.csv", header = T)

# Structuring the data ----------------------------------------------------
# Structure of the data
str(housing)

# formatting data types ---------------------------------------------------
housing$Year <- as.numeric(substr(housing$Date, 1, 4))
housing$Qrtr <- as.numeric(substr(housing$Date, 6, 6))
housing$Date <- housing$Year + housing$Qrtr/4
housing$Home.Value <- as.character(housing$Home.Value)

# replacing unwanted character in different columns -----------------------
housing$Home.Value <-  gsub("[\\$,]" , "", housing$Home.Value)
housing$Structure.Cost <-  gsub("[\\$,]" , "", housing$Structure.Cost)
housing$Land.Value <-  gsub("[\\$,]" , "", housing$Land.Value)
housing$Land.Share..Pct. <-  gsub("[\\%,]" , "", housing$Land.Share..Pct.)
str(housing)

# converting multipe columns at one go ------------------------------------
for(i in c(3:ncol(housing))) {
  housing[,i] <- as.numeric(housing[,i])
}
housing[,i] <- as.numeric(housing[,i])
str(housing)
hist(housing$Home.Value) # Normal plot function


# plotting variable with pot and ggplot -----------------------------------
library(ggplot2) # Loading ggplot library
ggplot(housing, aes(Home.Value))+ geom_histogram() +
  geom_smooth()+ xlab("Home Value") +ylab("Frequency") +
     ggtitle("Home Value Plotting") # Plotting same using ggplot package

# Vs graph using plot and ggplot2 plotting
plot(Home.Value ~ Date,
     data=subset(housing, STATE == "MA"))
points(Home.Value ~ Date, col="red",
       data=subset(housing, STATE == "TX"))
legend(19750, 400000,
       c("MA", "TX"), title="State",
       col=c("black", "red"),
       pch=c(1, 1))

# Plotting same with ggplot
ggplot(subset(housing, STATE %in% c("MA", "TX")) , 
       aes(Date, Home.Value, color = STATE)) +
           geom_point()




