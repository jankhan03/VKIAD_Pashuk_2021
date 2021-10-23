GetMode <- function(v){
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
# 1
data <- read.csv('avianHabitat.csv', header = TRUE)

AHtData <- data$AHt
AHtData <- AHtData[AHtData != 0]

HHtData <- data$HHt
HHtData <- HHtData[HHtData != 0]

# 2
max(AHtData)

min(AHtData)

mean(AHtData)

median(AHtData)

GetMode(AHtData)

var(AHtData)

sqrt(var(AHtData))

quantile(AHtData, c(0.25, 0.75))

#3
boxplot(AHtData)

#4
boxplot(AHtData,HHtData)

#5
plot.ecdf(AHtData)

#6
hist(AHtData,main = "AHt", prob =TRUE)
lines(density(AHtData), lwd = 5)

#7
qqnorm((AHtData-mean(AHtData))/sd(AHtData))
lines(c(-2,2), c(-2,2), lwd = 3)
