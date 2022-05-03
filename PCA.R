###import the mean value data set
head(ALL)
summary(ALL)
#assign pca with output by using prcomp function, here ALL is the data and 2:22 shows the columns of response variable
pca=prcomp(ALL[,2:22], scale. = TRUE)
summary(pca)
#ploting the pca results
plot(pca,type='l')
biplot(pca,scale = 0)
pca$rotation
pca$x
# x have pc score
# rotation have eginen vectors
#sdev have standard deviation
# to get egien values just do square for stdev
#assign pcaf with coloumn bind command and bind the normal data and pc scores for which pca egien value should be greater than 1
#here i selected 1 to 8 coulumns because 8 are having above 1 egien values
pcaf=cbind(ALL,pca$x[,1:8])
head(pcaf)
View(pcaf)
##ploting pcaf out put
library(ggplot2)
ggplot(pcaf,aes(PC1,PC2, col=genotypes,fill=genotypes))+
  stat_ellipse(geom = 'polygon',col='black',alpha=0.5)+
  geom_point(shape=21,col='black')
###creating a correlation plot between raw data and pcs by using corrplot package
a=cor(ALL[-1],pcaf[,23:30])
library(corrplot)
corrplot(a,method = 'circle')
#printing the values from pca  results
a=pca$sdev
a=as.data.frame(a)
write.csv(pca$sdev,file = 'sdev1.csv',col.names = TRUE,row.names = TRUE)
b=pca$rotation
write.csv(b,file = 'rotation.csv',col.names = TRUE,row.names = TRUE)
a=summary(pca)
View(a)
write.csv(a$importance,file = 'imp.csv',col.names = TRUE,row.names = TRUE)
write.csv(a$x,file = 'scores.csv',col.names = TRUE,row.names = TRUE)
