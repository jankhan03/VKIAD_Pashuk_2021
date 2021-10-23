
library(class)
library(klaR)
library(e1071)
library(markdown)

x1 <- rnorm(150, 2)
y1 <- rnorm(150, 2)
p1 <- data.frame(x = x1, y = y1 , class =seq(1, 1, 150))

x2 <- rnorm(150, 8)
y2 <- rnorm(150, 8)
p2 <- data.frame(x = x2, y = y2, class = seq(2, 2, 150))

x3 <- rnorm(30, 5)
y3 <- rnorm(30, 6)
p3 <- data.frame(x = x3, y = y3, class = seq(0, 0, 30))

dataframe <- rbind(p1,p2)
dataframew3 <- rbind(dataframe, p3)
plot(dataframew3$x, dataframew3$y, col = 5 + dataframew3$class, pch = 5 + dataframew3$class)



cl1 <- knn(train = dataframe, test = p3 ,cl = dataframe$class, k = 7)
cl1
dataframew3[301:330, ]$class = cl1
plot(dataframew3$x, dataframew3$y, col = 5 + dataframew3$class, pch = 5 + dataframew3$class)

dataframew3[301:330, 3] = seq(0, 0, 30)



dataframe$class = as.factor(dataframe$class)
cl2 <- NaiveBayes(dataframe$class ~ ., data = dataframe)
p <- predict(cl2, p3)
dataframew3[301:330, 3] = p$class
plot(dataframew3$x, dataframew3$y, col = 5 + dataframew3$class, pch = 5 + dataframew3$class)
table(p$class, p3$class)



cl3 <- svm(dataframe$class ~ ., data = dataframe)
p <- predict(cl3, p3)
dataframew3[301:330, 3] = p
plot(dataframew3$x, dataframew3$y, col = 5 + dataframew3$class, pch = 5 + dataframew3$class)
table(p, p3$class)

