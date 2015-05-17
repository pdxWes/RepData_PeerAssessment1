# Set WD and load data
setwd(paste0("./DataSci_coursera/ReproducibleResearch/PeerAssessment1/",
             "RepData_PeerAssessment1"))
dta <- read.csv("activity.csv")

# 

part1 <- dta[!is.na(dta$steps),]
test <- tapply(part1$steps, part1$date, sum)
test2 <- tapply(dta$steps, dta$date, sum)
histogram()
mean(dta$steps, na.rm = TRUE)
median(dta$steps, na.rm = TRUE)

# for convenience replaced NAs with 0s
newdta<- dta
newdta[is.na(newdta)] <- 0
interval <- as.vector(subset(newdta, date == "2012-10-01", 
                             select = interval)$interval) # use unique
interval <- unique(newdta$interval)
test3 <- tapply(dta$steps, dta$interval, mean, na.rm=TRUE)
test4 <- tapply(part1$steps, part1$interval, mean)
plot(interval, test3, type = "l")
df <- cbind(interval, test3)
max_ave <- subset(df, df[,2] == max(df[,2]))[,1]



'''make a function that starts just calculating each intervals average and binds
it to the interval sequence and well as 1-288, then for the original data, do an
if test, and then get the index of the NA value, and match it to the table you 
made witt the interval averages'''

my_impute <- function(my_data) {
      impute_data <- my_data[!is.na(my_data$steps),]
      interval <- unique(impute_data$interval)
      int_means <- tapply(impute_data$steps, impute_data$interval, mean)
      lkup_table <- cbind(interval, int_means)
      for(i in 1:length(my_data$steps)){
            if(is.na(my_data$steps[i])) {
                  my_data$steps[i] <- lkup_table[lkup_table[,1] == my_data$interval[i]][2]
            }
      }
      return(my_data)
}

imputed_data <- my_impute(dta)

library(tidyr)
dayofweek <- weekdays(as.Date(imputed_data$date, "%Y-%m-%d"))
weekpart <- NULL
for(i in 1:length(dayofweek)){
      if(dayofweek[i] == "Monday" || dayofweek[i] == "Tuesday" || 
               dayofweek[i] == "Wednesday" || dayofweek[i] == "Thursday" ||
               dayofweek[i] == "Friday"){
            weekpart[i] <- "Weekday"
      } else {
            weekpart[i] <- "Weekend"
      }
}

imputed_data <- cbind(imputed_data, weekpart)
aves <- tapply(imputed_data$steps, list(imputed_data$interval, imputed_data$weekpart), mean)
aves <- cbind(unique(imputed_data$interval), aves)
aves <- as.data.frame(aves)
names(aves)[1] <- "Interval"

stocks <- data.frame(
      time = as.Date('2009-01-01') + 0:9,
      X = rnorm(10, 0, 1),
      Y = rnorm(10, 0, 2),
      Z = rnorm(10, 0, 4)
)

stocks %>% gather(stock, price, -time)

new_aves <- gather(aves, partofday, average, -Interval)
library(lattice)
xyplot(new_aves$average~new_aves$Interval|new_aves$partofday, type = "l",layout=c(1,2))



seq(0, 2355, by = 300)
