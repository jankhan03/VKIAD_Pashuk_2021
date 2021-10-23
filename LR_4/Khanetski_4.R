#PART 1
mfile <-read.csv("regression_data.csv")
x <- mfile$x6[]
y <- mfile$y6[]

x
y
#PART 2  
b1 <- (mean(x*y)-mean(x)*mean(y)) / (mean(x^2)-mean(x)^2)
b0 = mean(y) - b1*mean(x)


# check = 0 
mnk2a <- sum  ((mean(y)- b0 - b1*mean(x))^2  ) 
mnk2b  <- lm(y~x,data=mfile)

b1
b0
mnk2a
mnk2b 

#PART 3
reg3 <- lm(y ~ I(x^2) ,data=mfile)
reg3 

#PART 4
reg4 <- nls(y ~ I(a * exp(b * x)), data = mfile, start = list(a = 1, b = 0), trace = T)
reg4 

#PART 5
plot(x,y, xlim = c(0, 11))
new.data <- data.frame(x = seq(min(x),max(x),len = 100))
abline(mnk2b) 
lines(new.data$x,predict(reg3, newdata = new.data))
lines(new.data$x, predict(reg4, newdata = new.data))

new.data <- data.frame(x = 11)
points(11, predict(mnk2b, newdata = new.data))
points(11, predict(reg3, newdata = new.data))
points(11, predict(reg4, newdata = new.data))
#PART 6

prznach2 <-fitted(mnk2b)
prznach2
prznach3 <-fitted(reg3)
prznach3
prznach4 <-fitted(reg4)
prznach4