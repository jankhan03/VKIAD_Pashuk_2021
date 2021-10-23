install.packages('fpc')
install.packages('party')
install.packages('NbClust')
library(cluster)
library(fpc)
library(party)
library(NbClust)


x<-c(rnorm(30,3),rnorm(20,7),rnorm(50,3))
y<-c(rnorm(50,1),rnorm(30,5),rnorm(20,9))

plot(x,y)
x0=c(x,y)

m=matrix(x0,ncol=2)
cl1 = kmeans(m, 3)

plot(m, col = cl1$cluster,pch=5 + cl1$cluster)
cl2 = clara(m, 3)
plot(m, col = cl2$clustering,pch=6 + cl2$cluster)
cl3 = pam(m, 3)
plot(m, col = cl3$clustering,pch=7 + cl3$cluster)
cl4 =  kmeansruns(m)
plot(m, col = cl4$cluster,pch=8 + cl4$cluster)
cl5 = pamk(m)
plot(m, col = cl5$pamobject$clustering,pch=9+cl5$pamobject$clustering)

k<-NbClust(m, method = "ward.D")

M <- dist(scale(x))^2
hclust1 <- hclust(M, method = "ward.D")
dendr <- as.dendrogram(hclust1)
plot(cut(dendr, h = 1)$upper)
group <- cutree(hclust1, k = 2)
plot(m, col = group ,pch = 5 + group)

mydataframe<-data.frame(x=x,y=y,cluster = cl1$cluster)
plot(ctree(cl1$cluster~.,mydataframe))

