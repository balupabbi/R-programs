#Author
#Bhargav Pabbisetti
#W1167792

# change to appropriate dir; the code expects the images in current dir 
setwd("yalefaces/bmps")

#
# you would need to install the following two packages
#
library(bmp)
library(pixmap)

# images matrix
#   there are 165 images, each is 243x320 pixels
#
imat <- matrix(nrow=165, ncol=243*320)

# This function reads all the image files and 
# constructs the image matrix, imat
#

readImages <- function() {
    i <- 1
    for (f1 in list.files()) {
        im <- read.bmp(f1)
        pr <<- pixmapGrey(im[,,2:4])
        imat[i,] <<- as.vector(attr(pr, "grey"))

        i<-i+1
     }
}

# this takes an array of 243x320, im, as input 
#  e.g., to display first image, do viewImage(imat[1,])
#
viewImage <- function(im,name) {    
    attr(pr, "grey") <- as.matrix(im, nrow=243, ncol=320)
    plot(pr,main=name)	 
}

### 
###   Main
###

readImages()

# now imat has the 165 images, each row is an image

# Here are the suggested steps:
#
# - center the images, i.e., subtract the mean image from all images
#   (the mean image is the mean of all the columns in imat); save the mean image
# - compute PCA e.g. using prcomp with center=F (since it is already centered)
# - find the projection of the images in the PC space
# - reconstruct the image in the original space using some number of PC's
# - add the mean image to the reconstructed images
# - the original values in the image matrix are between 0 and 1, and the values are
#   supposed to be in the [0,1] range. However, the reconstructed matrix can have values
#   outside that range. So set the values less than 0 as 0, and greater that 1 as 1.
# - view the image using viewImage function
#
#
mean_image <- matrix(nrow=165, ncol=243*320)
rows = nrow(imat)
col = ncol(imat)

means_col = colMeans(imat)

for(j in 1:col){
  temp_mean = means_col[[j]]
  for(i in 1:rows){
   mean_image[i,j] = imat[i,j]-temp_mean
  }
}


pc = prcomp(mean_image,center = FALSE)
summary(pc)#has variance information
eigen_vectors = pc[[2]]

#all eigen vector for all required components
t_ev20 = t(eigen_vectors[,1:20])
t_ev40 = t(eigen_vectors[,1:40])
t_ev60 = t(eigen_vectors[,1:60])
t_ev80 = t(eigen_vectors[,1:80])
t_ev100 = t(eigen_vectors[,1:100])

#corresponding x original points
orig = pc$x
x_20 = orig[,1:20]
x_40 = orig[,1:40]
x_60 = orig[,1:60]
x_80 = orig[,1:80]
x_100 = orig[,1:100]


#Computations for all PCs to project into PC space
pc_20 = x_20 %*% t_ev20
pc_40 = x_40 %*% t_ev40
pc_60 = x_60 %*% t_ev60
pc_80 = x_80 %*% t_ev80
pc_100 = x_100 %*% t_ev100

#Reconstruction of 3 images
viewImage(imat[1,],'Original')

pc1_20 <- matrix(nrow=165, ncol=243*320)
for(j in 1:77760){
  for(i in 1:165)
    pc1_20[i,j] <- pc_20[i,j]+means_col[j]
}
pc1_20[pc1_20 >1]<-1
pc1_20[pc1_20 <0]<-0
viewImage(pc1_20[1,],'PC_20')


pc1_40 <- matrix(nrow=165, ncol=243*320)
for(j in 1:77760){
  for(i in 1:165)
    pc1_40[i,j] <- pc_40[i,j]+means_col[j]
}
pc1_40[pc1_40 >1]<-1
pc1_40[pc1_40 <0]<-0
viewImage(pc1_40[1,],'PC_40')


pc1_60 <- matrix(nrow=165, ncol=243*320)
for(j in 1:77760){
  for(i in 1:165)
    pc1_60[i,j] <- pc_60[i,j]+means_col[j]
}
pc1_60[pc1_60 >1]<-1
pc1_60[pc1_60 <0]<-0
viewImage(pc1_60[1,],'PC_60')


pc1_80 <- matrix(nrow=165, ncol=243*320)
for(j in 1:77760){
  for(i in 1:165)
    pc1_80[i,j] <- pc_80[i,j]+means_col[j]
}
pc1_80[pc1_80 >1]<-1
pc1_80[pc1_80 <0]<-0
viewImage(pc1_80[1,],'PC_80')


pc1_100 <- matrix(nrow=165, ncol=243*320)
for(j in 1:77760){
  for(i in 1:165)
    pc1_100[i,j] <- pc_100[i,j]+means_col[j]
}
pc1_100[pc1_100 >1]<-1
pc1_100[pc1_100 <0]<-0
viewImage(pc1_100[1,],'PC_100')

#Image 2
viewImage(imat[5,],'Original')
viewImage(pc1_20[5,],'PC_20')
viewImage(pc1_40[5,],'PC_40')
viewImage(pc1_60[5,],'PC_60')
viewImage(pc1_80[5,],'PC_80')
viewImage(pc1_100[5,],'PC_100')

#Image 3
viewImage(imat[100,],'Original')
viewImage(pc1_20[100,],'PC_20')
viewImage(pc1_40[100,],'PC_40')
viewImage(pc1_60[100,],'PC_60')
viewImage(pc1_80[100,],'PC_80')
viewImage(pc1_100[100,],'PC_100')


# extra-credit problem
#
#

#
# write a function which returns the nearest neighbor of a query image. The
# function is passed two arguments: 1) an image matrix, with each row as an
# image; 2) the index of the query image (which is also present in the image
# matrix. The function returns the index of the nearest image. Euclidean
# distance is used as the distance measure.
#
nn <- function(images, q, d) {
  
  
  distance_matrix = as.matrix(d)
  r1 = ncol(distance_matrix)
  minimum_array = apply(distance_matrix, 1, FUN = function(x) {min(x[x > 0])}) 
  find_q = minimum_array[q]
  for(j in 1:r1){
    if(distance_matrix[q,j]==find_q){
      nearest_image = j
    }
  }
  return(nearest_image)
}

# use the above function to determine the nearest neighbors of the images
# in the original space and in 2-dimensional space (top two PC's). Determine
# the percentage of cases where the nearest neighbor is identical in the two
# cases.
#
d = dist(imat,method = 'euclidean',upper = TRUE, p=2)
total_images = rows
orig_nearest = c(1:165)
for(m in 1:total_images){
  orig_nearest[m] = nn(imat,m,d)  
}


t_ev2 = t(eigen_vectors[,1:2])
x_2 = orig[,1:2]
pc_2 = x_2 %*% t_ev2
d1 = dist(pc_2,method = 'euclidean',upper = TRUE, p=2)
pc_nearest = c(1:165)
for(m in 1:total_images){
  pc_nearest[m] = nn(pc_2,m,d1)  
}

count = 0 
for(m1 in 1:total_images){
  if(orig_nearest[m1]==pc_nearest[m1]){
    count = count+1
  }
}
match_percent = (count/165)*100
cat(sprintf("Matched images: %0.2f", count))
cat(sprintf("Percentage of mataching: %0.2f", match_percent))
