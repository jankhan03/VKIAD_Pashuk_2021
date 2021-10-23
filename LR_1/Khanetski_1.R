#first part
vectr1 <- vector ("double", 9)
vectr1[1] <- 25
vectr1[length(vectr1)] <- 6.5
differential <- ((vectr1[length(vectr1)] - vectr1[1]) / (length(vectr1) - 1) )
for (i in 2 : (length(vectr1) - 1) ){
  vectr1[i] <- vectr1[i - 1] + differential
}

vectr2 <- vector ("double", 3)
vectr2[1] <- abs(13 - 25) + 2
vectr2[length(vectr2)] <- 100
geometric_diff <- (vectr2[length(vectr2)] / vectr2[1])^(1 /(length(vectr2) - 1))
for( i in 2 :(length(vectr2) - 1) ) {
  vectr2[i] <- vectr2[i - 1]*geometric_diff
}
print("V1")
vectr1
print("V2")
vectr2


if ((vectr1[1] + vectr2[1]) %% 2 == 0 ) {
  matr1 <- matrix(c(vectr1, vectr2), ncol = 2, byrow = FALSE)
} else {
  matr1 <- matrix(c(vectr2, vectr1), ncol = 2, byrow = FALSE)
}


print("Our matrix")
matr1

#second part

cars <- c("Geely", "Skoda", "AlfaRomeo", "Morgan")
price <- c(0, 2334, 100500, 3259)
fram1 <- data.frame(cars, price)

fram1 <- cbind(fram1, logic = c(F,T,F,T)) #связать



print("lines with TRUE")
subset(fram1, logic == TRUE)


#3#
iris <- read.csv(file = "iris.csv", header = TRUE)

#a#
print("Sepal.width <= 3")
print(nrow(subset(iris, Sepal.Width <= 3 & Species == "setosa")))
print(nrow(subset(iris, Sepal.Width <= 3 & Species == "versicolor")))
print(nrow(subset(iris, Sepal.Width <= 3 & Species == "virginica")))

#b#
print("Petal.Length >4.5")
print(nrow(subset(iris, Petal.Length > 4.5 & Species == "setosa")))
print(nrow(subset(iris, Petal.Length > 4.5 & Species == "versicolor")))
print(nrow(subset(iris, Petal.Length > 4.5 & Species == "virginica")))

#c#
print("Sepal.width <= 3 && Petal.Length >4.5")
print(nrow(subset(iris, Petal.Length > 4.5 & Sepal.Width <= 3 & Species == "setosa")))
print(nrow(subset(iris, Petal.Length > 4.5 & Sepal.Width <= 3 & Species == "versicolor")))
print(nrow(subset(iris, Petal.Length > 4.5 & Sepal.Width <= 3 & Species == "virginica")))

#d#
print("Mean by Petal.Width")
print(mean(subset(iris, Species == "setosa")$Petal.Width))
print(mean(subset(iris, Species == "versicolor")$Petal.Width))
print(mean(subset(iris, Species == "virginica")$Petal.Width))