install.packages('blockcluster')
library(blockcluster)

library(MBCbook)
data(amazonFineFoods)

X = as.matrix(amazonFineFoods)
image(X,main='Original data matrix', xaxt='n',yaxt='n')
sparsity = 1 - sum(X>0) / (dim(X)[1] *dim(X)[2]) 

out = coclusterBinary(X,nbcocluster = c(5,8))

par(mfrow=c(1,2))
image(X,main='Original data matrix', xaxt='n',yaxt='n')
image(X[order(out@rowclass),order(out@colclass)],
      main='Data matrix sorted by groups',
      xaxt='n',yaxt='n')
