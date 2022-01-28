#install.packages("HDclassif")
#install.packages("MBCbook")
library(HDclassif); library(MBCbook)

show.image <- function(x,np){
  image(t(matrix(X[(np*np):1],ncol=np,byrow=T)),col=palette(gray(0:255/255)))
}

data("usps358")
X = usps358[,-1]
cls = usps358$cls

image(
  t(matrix(t(X[1,]),ncol=16, byrow=TRUE)[16:1,]),
  col=gray(255:0/255),axes=F
); box()

# Clustering with GMM through MClust

library(mclust)
out.mclust = Mclust(X, 3, initialization=list(subset=sample(nrow(X), 300)))
table(cls, out.mclust$classification)

means = t(out.mclust$parameters$mean)
par(mfrow=c(1,3))
image(t(matrix(t(means[1,]),ncol=16, byrow=TRUE)[16:1,]),col=gray(255:0/255),axes=F); box()
image(t(matrix(t(means[2,]),ncol=16, byrow=TRUE)[16:1,]),col=gray(255:0/255),axes=F); box()
image(t(matrix(t(means[3,]),ncol=16, byrow=TRUE)[16:1,]),col=gray(255:0/255),axes=F); box()

# Clustering with HD-GMM through HDDC
out.hddc = hddc(X,3,model="AkjBkQkDk") #ABQD
table(cls, out.hddc$class)

means = out.hddc$mu
par(mfrow=c(1,3))
image(t(matrix(t(means[1,]),ncol=16, byrow=TRUE)[16:1,]),col=gray(255:0/255),axes=F); box()
image(t(matrix(t(means[2,]),ncol=16, byrow=TRUE)[16:1,]),col=gray(255:0/255),axes=F); box()
image(t(matrix(t(means[3,]),ncol=16, byrow=TRUE)[16:1,]),col=gray(255:0/255),axes=F); box()

# The blockcluster package implements the Latent Block Model (LBM) for several
# data types

#install.packages("blockcluster")
library(blockcluster)

data(amazonFineFoods)
X = as.matrix(amazonFineFoods)
image(X, main="original data matrix", xaxt="n", yaxt="n")
spartsity = 1 - sum(X>0) / (dim(X)[1] * dim(X)[2])
out = coclusterBinary(X, nbcocluster=c(5,8))
par(mfrow=c(1,2))
image(X, main="Original data matrix", xaxt="n", yaxt="n")
image(X[order(out@rowclass), order(out@colclass)],
      main = "Data matrix sorted by groups", 
      xaxt="n", yaxt="n")

