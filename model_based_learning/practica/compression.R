library(png)

show.image <- function(x,np){ 
  image(t(matrix(X[(np*np):1],ncol=np,byrow=T)),col=palette(gray(0:255/255)))
}

X = readPNG('image.png')
Z = matrix(NA,4096,64)

# Split the image as 8x8 patches
for (i in 1:64) {
  for(j in 1:64) {
    Z[64*(i-1)+j,] = as.vector(t(X[(8*(i-1)+1):(8*i),(8*(j-1)+1):(8*j)]))
  }
}

# Projection with PCA
d = 2
U = eigen(cov(Z))$vectors[,1:d]
z = Z %*% U
  
# Reconstruction of patches from PC scores (z and U)
Zb = z %*% t(U)
  
# Reconstruction of the image from reconstructed patches
Xb = matrix(NA,512,512)
for (i in 1:64) {
  for(j in 1:64) {
    Xb[(8*(i-1)+1):(8*i),(8*(j-1)+1):(8*j)] = matrix(Zb[64*(i-1)+j,],8,8,byrow=T)
  }
}

# Computation of the compression rate
comp.original = 4096 * 64
comp.pca = 4096 * d + 64 * d
comp.rate = comp.pca / comp.original
print(comp.rate)

# Visualization
split.screen(c(1,2))
screen(1)
image(t(X[512:1,]),col=palette(gray(0:255/255)))
screen(2)
image(t(Xb[512:1,]),col=palette(gray(0:255/255)))
title(main=paste("Compression rate:", 1 - round(comp.rate,3)))

# Homework: Use the BIC to select D


