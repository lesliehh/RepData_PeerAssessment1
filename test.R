setwd("C:/Base/Education/MOOC/Johns Hopkins - Data Science Specialization/Reproducible Research/RepData_PeerAssessment1");



library("utils")

unzip("activity.zip", overwrite=TRUE)
data <- read.csv("activity.csv", header = TRUE, sep = ',')

head(data)
tail(data)


# What is mean total number of steps taken per day?

agg <- as.data.frame(tapply(data$steps, data$date, sum))
names(agg) <- c("total_steps")

summary(agg)

plot (
    x    = as.Date(rownames(agg)),
    y    = agg$total_steps,
    type = "h",
    xlab = "Date",
    ylab = "Total Steps"
)


# What is the average daily activity pattern?

avg <- as.data.frame(tapply(data$steps, data$date, mean))
names(avg) <- c("average_steps")

which.max(avg[,1])

plot (
    x    = as.Date(rownames(avg)),
    y    = avg$average_steps,
    type = "l",
    xlab = "Interval",
    ylab = "Average Steps"
)


# Imputing missing values

summary(data$steps)[7]

factor(data$date, rownames(agg))

library(plyr)
count <- count(data, 'date')
for (i in 1:nrow(count)) {
    # df with c(date, count)
    row <- count[i,]
    
    c <- count(data[data$date == row[1,1],])
    print(c)
}
