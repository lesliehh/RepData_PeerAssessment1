setwd("C:/Base/Education/MOOC/Johns Hopkins - Data Science Specialization/Reproducible Research/RepData_PeerAssessment1");

library("utils")

unzip("activity.zip", overwrite=TRUE)
data <- read.csv("activity.csv", header = TRUE, sep = ',')

head(data)
tail(data)

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


summary(data$steps)[7]

