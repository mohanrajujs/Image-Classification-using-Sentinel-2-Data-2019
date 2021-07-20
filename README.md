# Image-Classification-using-Sentinel-2 | 2019
### Aim
To develop and apply a chain of image filtering, segmentation and classification operations along with developing a classification model using convolution neural netwok (CNN). The exercise also explore various image filtering approaches that includes setting the correct parameters & finally reflect on the results and mention the current limitations of the method used.

### Image Selectioin
The below screenshot shows the arial view of the Manhattan, New York and the highlighted region is a sub-set of the main image. The co-ordinates of the sub-set image are X-min: 628204.624870053, X-Max: 629655.9482847921, Y-Min: 680645.7086842884, Y-Max:682035.0097308763 ([EPSG:2260]).

![Capture](https://user-images.githubusercontent.com/5634888/122561358-659a1780-d05f-11eb-94ee-1db6e4cb7945.PNG)

### Image analysis procedure
Following are the methods used to perform Image Analysis:-
 -  Filtering (Decision Tree Algorithm)
 -  Segmentation (Mean Shift Algorithm)
 -  Classification (Fuzzy C means Algorithm)
 
 1. #### Decision Tree Algorithm
```
#==================================================================================
# R script for Image Analysis
#==================================================================================
#Download/add the required packages/libraries
library (raster)
library (rgdal)
library(rpart)
library(rpart.plot)
library(rasterVis)
library(dismo)

#==================================================================================
#define the working directory
#setwd ('your working directory')
#==================================================================================
#load the land cover data (polygon shapefile)
samples <- readOGR('Flevoland_Samples.shp')

# Generate  training samples points using Flevoland_Samples dataset
pt_samples <- spsample(samples, 800, type='regular')

# Add the land cover class to the points  
pt_samples$Class <- over(pt_samples, samples)$Class

#writeOGR(pt_samples, dsn='your output folder', layer = 'TS_Flevoland', driver="ESRI Shapefile")
#==================================================================================
#Load the Planet image and rename the spectral bands
planet_image <- stack('Flevoland_planet.tif')
names(planet_image) <- c('blue', 'green', 'red', 'NIR')
#==================================================================================
# Extracting spectral information from Planet images for the training samples generated in the previous step
sample_values <- extract(planet_image, pt_samples, df = TRUE)
sample_values <- sample_values[, -1]

# Combine the class information with extracted values
sample_data <- data.frame(classvalue = pt_samples@data$Class, sample_values)
#==================================================================================
#Decision Tree /CART using rpart package
# Step 1: trainign classification model
DT_training <- rpart(as.factor(classvalue)~., data=sample_data, method = 'class', minsplit = 5)
#Plot the trained decision tree model
rpart.plot (DT_training)

#Investigate the trained decision tree model
summary (DT_training)
#alternatively you can use print(DT_training)

#investigate the complexity table. This table will be used to prune the trained DT model
#printcp(DT_training)
#plotcp(DT_training)

#Pruning training DT model
#prune(DT_training, cp= 0.04) 
#rpart.plot (DT_training)
#==================================================================================
# Step 2: Predict the land cover classes of interest
DT_prediction <- predict(planet_image, DT_training, type='class')
#==================================================================================
#Display the classification results
DT_prediction <- ratify(DT_prediction)
LC_raster_classes <- levels(DT_prediction)[[1]]
planetclass <- c("Arable", "BuiltUp", "Forest", "Grassland", "Water")
LC_Classes <- data.frame(classvalue1 = c(1,2,3,4,5), classnames1 = planetclass)
LC_ClassesColor <- c("#FBF65D", "#D2CDC0", "#38814E", "#D1D182", "#5475A8")
LC_raster_classes$legend <- LC_Classes$classnames
levels(DT_prediction) <- LC_raster_classes
levelplot(DT_prediction, maxpixels = 1e6,
          col.regions = LC_ClassesColor,
          scales=list(draw=FALSE),
          main = "Decision Tree classification of land cover usign Planet Images")

# Investigating the classification results obtained by the decision tree classification model
set.seed(200)
DT_kfold <- kfold(sample_data, k = 3, by=sample_data$classvalue)
table(DT_kfold )
DT_preparation <- list()
for (k in 1:3) {
  training_phase <- sample_data[DT_kfold != k, ]
  testing_phase <- sample_data[DT_kfold  == k, ]
  DT_training <- rpart(as.factor(classvalue)~., data=training_phase, method = 'class', minsplit = 5)
  prediction_class <- predict(DT_training,  testing_phase, type='class')
  # create a data.frame using the reference and prediction
  DT_preparation[[k]] <- cbind( testing_phase$classvalue, as.integer(prediction_class))
}

DT_results <- do.call(rbind, DT_preparation)
DT_results <- data.frame(DT_results)
colnames(DT_results) <- c('observed', 'predicted')
DT_results_summary <- table(DT_results)
# Rename the LC classes
colnames(DT_results_summary ) <- LC_Classes$classnames
rownames(DT_results_summary ) <- LC_Classes$classnames
DT_results_summary 

# Total number of classes
Total_classes <- sum(DT_results_summary )
Total_classes

# Identify the total number of correctly classified cases per class
matrix_diagonal <- diag(DT_results_summary )
# Calculate the Overall Accuracy
Overall_Accuracy <- sum(matrix_diagonal) / Total_classes
Overall_Accuracy


```
2. #### Mean Shift Algorithm
```
#==================================================================================
# R script for practical class Basic Image Processing
# Split and merge segmentation
#
# Last modified: 26 April 2019
#
# This code is developed for educational purpose, 
# to be used in Q4 course "Image Analysis" of MSc course in Geoinformatics at ITC.
# The code is developed by Valentyn Tolpekin, v.a.tolpekin@utwente.nl.
# The code is optimized for clarity rather than for computational efficiency.
#
# Do not remove this announcement.
# The code is distributed "as is", WITH NO WARRANTY whatsoever!
#==================================================================================
rm(list=ls(all=TRUE))
graphics.off()

require(rgdal)
require(meanShiftR)
require(RColorBrewer)

StartPath <- getwd()

#Defining the root directory
Root <- StartPath

Path_in <- paste(Root, "/input",sep="")
Path_out <- paste(Root, "/output",sep="")

dir.create(Path_out,showWarnings=FALSE, recursive=TRUE)

setwd(Root)
source("BIP_lib.R")

convert_to_polygons <- TRUE
#======================================================================================
# Read image
#======================================================================================
image.fn <- "NewYork_Subset.tif"

A <- readGDAL(paste(Path_in,"/",image.fn,sep="")) 

# Image is too large. Subset.
d <- A@grid@cells.dim
nrows0 <- d[1]
ncols0 <- d[2]

#nrows <- 1451
#ncols <- 1371

nrows <- 500
ncols <- 490

if(nrows < nrows0 & ncols < ncols0) {
	start_row <- round(runif(1, min = 1, max=nrows0 - nrows))
	start_col <- round(runif(1, min = 1, max=ncols0 - ncols))
	
	start_row <- nrows
	start_col <- ncols

	end_row <- start_row + nrows - 1
	end_col <- start_col + ncols - 1

	A <- A[start_col:end_col, start_row:end_row]
}

n_b <- ncol(A@data)

A.image <- A

# Apply histogram strecth
for(k in 1:n_b) {
	A.image@data[, k]   <- histstretch(A.image@data[, k])
}

# Image dimensions
d <- A@grid@cells.dim
psize <- A@grid@cellsize

nrows <- d[1]
ncols <- d[2]
npix <- prod(d)

proj_image <- A@proj4string


nR <- 1
nG <- 2
nB <- 3

windows()
if(n_b>1) image(A.image,red=nR,green=nG,blue=nB,axes=TRUE) else image(A.image,col=gray((0:255)/255),axes=TRUE)

# The max number of iterations
iter <- 1000

x <- data.matrix(A@data)

# xy <- coordinates(A)
# sx <- xy[,1]
# sy <- xy[,2]

# sx <- sx - min(sx)
# sy <- sy - min(sy)

# x <- cbind(x, sx, sy)

# set the bandwidth
h <- 0.5 * rep(1, ncol(x))

res <- meanShift(x, x, nNeighbors=10, bandwidth=h, alpha=0, iterations = iter, epsilonCluster = 0.1)

# Display classified data

Seg <- SpatialGridDataFrame(A@grid, data.frame(class_id=res$assignment), proj4string = A@proj4string)

rf <- colorRampPalette(rev(brewer.pal(11,'Spectral')))
pal <- rf(32)

windows()
image(Seg, col=pal, axes=TRUE)

#======================================================================================
# Output raster image file
#======================================================================================
setwd(Path_out)

OUT <- Seg
imagefn.out <- paste("NewYork_Subset",".tif",sep="")
if(file.exists(imagefn.out))file.remove(imagefn.out)
# writeGDAL(OUT, imagefn.out, drivername = "GTiff", type = "Int32", mvFlag = -1, colorTables=list(pal))
writeGDAL(OUT, imagefn.out, drivername = "GTiff", type = "Byte", mvFlag = -1)

setwd(StartPath)
#======================================================================================
# The end
#======================================================================================

```
 #### 3. Fuzzy C means
```
#==================================================================================
# R code for practical class Fuzzy and sub-pixel classification
# Last modified: 10 May 2018
#
# This code is developed for educational purpose, 
# to be used in Q4 course "Image Analysis" of MSc course in Geoinformatics at ITC.
# The code is developed by Valentyn Tolpekin, v.a.tolpekin@utwente.nl.
# The code is optimized for clarity rather than for computational efficiency.
#
# Do not remove this announcement.
# The code is distributed "as is", WITH NO WARRANTY whatsoever!

#======================================================================================
# Block 1: variable definitions, data import, preparation
#======================================================================================
rm(list=ls(all=TRUE))
graphics.off()

require(rgdal)

# Show intermediate results? (slow)
ShowAll <- TRUE

# Fuzzy parameter
m <- 2.0

# Number of classes
n_cl <- 5

# Set path to directories
Root <- "C:/Users/csc/Desktop/Root"

setwd(Root)
source("BIP_lib.R")

# Input directory
Path_in <- paste(Root, "/Input", sep="")

# Output directory
Path_out <- paste(Root, "/Output", sep="")
dir.create(Path_out, recursive = TRUE, showWarnings=FALSE)

# Name of the input file (excluding file extenstion .img)
imagefn <- "test.tif"

# Combine path, file name and file extension, then read the file
MS <- readGDAL(paste(Path_in, "/", imagefn, sep="")) 

# Number of bands
n_b <- dim(MS@data)[2]

MS.image <- MS

# Set RGB composition
nR <- 3
nG <- 2
nB <- 1

MS.image@data[, nR] <-histstretch(MS.image@data[, nR])
MS.image@data[, nG] <-histstretch(MS.image@data[, nG])
MS.image@data[, nB] <-histstretch(MS.image@data[, nB])

windows(7, 7)
image(MS.image, red=nR, green=nG, blue=nB, axes=TRUE)
title(paste("l19991023sub, RGB=", nR, ":", nG, ":", nB, sep=""))

# Image dimensions
d <- MS@grid@cells.dim
psize <- MS@grid@cellsize

n_col <- d[1]
n_row <- d[2]

tmp <- as.matrix(MS@data)
x <- array(tmp, dim(tmp))
max_val <- max(x)
#======================================================================================
# 2. Initialiazation of membership values. 
# Function sample_frac produces random membership value vector with sum of elements = 1
#======================================================================================
npix <- nrow(MS)
V <- array(0, c(n_cl, n_b))
U <- array(0, c(npix, n_cl))

Uref <- U

V[, ] <- matrix(rep(colMeans(x), n_cl), byrow=TRUE, nrow=n_cl)

# Add random vedctor to the mean value of each class 
amplitude <- array(0, n_b)

for(i in 1:n_b) amplitude[i] <- sd(x[, i])

for(k in 1:n_cl)
  for(i in 1:n_b)
    V[k, i] <- V[k, i] + runif(1, min=-amplitude[i], max=amplitude[i])

Md <- array(0, c(n_cl, npix))
Mdk <- array(0, c(n_cl, n_cl, npix))

# Update pixel membership values
for(k in 1:n_cl) {
  #Md[l, , ] <- colSums((D-V[l, ])^2, 1)
  Md[k, ] <- rowSums((t(t(x)-V[k, ]))^2, 1)
}

for(l in 1:n_cl) for (k in 1:n_cl) Mdk[k, l, ] <- (Md[l, ]/Md[k, ])^(1/(m-1))

U <- t(1.0/colSums(Mdk, 1))
#======================================================================================
# 3. Display initial membership values
#======================================================================================
FCM <- MS
FCM@data <- data.frame(U)

if(ShowAll) {
  windows(title="Iinitial membership values")
  Nrow <- round(sqrt(n_cl))
  par(mfrow=c(Nrow, round(n_cl/Nrow)))
  
  for(k in 1:n_cl) {
    image(FCM, attr=k, col=gray((0:255)/255), axes=TRUE)
    title( main = paste("Class ", k, sep=""))
  }
}
"Initial mean values:"
V
#======================================================================================
# 4. Fuzzy C means computations
#======================================================================================
fcm_objfun <- function() {
  val <- 0
  
  for(k in 1:n_cl) {
    tmp <- rowSums((t(t(x)-V[k, ]))^2, 1)
    val <- val + sum(tmp*(U[, k]^m))/(npix*(max_val^2))
  }
  
  return(val)
}


Nit <- 1000
eps <- 1.0e-02

parr <- rep(18, n_cl)
#colarr <- 2:(1+n_cl)

col_arr <- rainbow(n_cl)

Md <- array(0, c(n_cl, npix))
Mdk <- array(0, c(n_cl, n_cl, npix))

if(ShowAll) {
  windows(12, 6)
  par(mfrow=c(1, 2))
}

Errcount <- 0

ind <- sample(npix, 5000, replace=FALSE)
Err_hist <- array(0, Nit)

Objfun_hist <- array(0, Nit)

Objfun_hist[1] <- fcm_objfun()

for(iter in 1:Nit) {
  Uref <- U
  
  # Update pixel membership values
  for(k in 1:n_cl) {
    #Md[l, , ] <- colSums((D-V[l, ])^2, 1)
    Md[k, ] <- rowSums((t(t(x)-V[k, ]))^2, 1)
  }
  
  for(l in 1:n_cl)
    for (k in 1:n_cl) {
      Mdk[k, l, ] <- (Md[l, ]/Md[k, ])^(1/(m-1))
    }
  
  U <- t(1.0/colSums(Mdk, 1))
  
  # Update cluster mean values
  for (l in 1:n_b)
    for (k in 1:n_cl) {
      V[k, l] <- sum((U[, k]^m)*x[, l])/sum(U[, k]^m)
    }
  
  Objfun_hist[iter] <- fcm_objfun()
  
  Err <- sum(abs(Uref-U))/npix
  
  if(Err<eps) Errcount<-Errcount+1 else Errcount=0
  if(Errcount>=3) break
  
  if(ShowAll) {
    FCM@data[, 1] <- U[, 1]
    image(FCM, attr=1, col=gray((0:255)/255), axes=TRUE)
    title(main = paste("Iteration=", iter, " Mean band 4=", round(V[1, 4], 3), sep=""))
    
    plot(x[ind, 4], x[ind, 2], xlab = paste("Band", 4, sep=""), xlim=c(0, max_val), asp=1, ylab = paste("Band", 2, sep=""), cex=0.3, pch=16)
    
    for(i in 1:n_cl) {
      points(V[i, 4], V[i, 2], col=col_arr[i], cex=1.5, pch=parr[i])
      points(V[i, 4], V[i, 2], col=col_arr[i], cex=3, pch=3)
    }
  }
}

# Convergence plot, for testing purpose
windows()
plot(Objfun_hist[1:iter], type="b", xlab="iterations", ylab="Objective function")
title(main=paste("convergence plot; the end value =", round(Objfun_hist[iter], 6), sep=""))
#======================================================================================
# 5. Display results
#======================================================================================
FCM <- MS
FCM@data <- data.frame(U)

cl_names <- "Class1"
for(j in 2:n_cl)cl_names <- c(cl_names, paste("Class", j, sep=""))

names(FCM) <- cl_names

windows(title="FCM result: membership values")
Nrow <- round(sqrt(n_cl))
par(mfrow=c(Nrow, ceiling(n_cl/Nrow)))

for(k in 1:n_cl) {
  #	FCM@data[, k] <- as.vector(Uold[k, , ])
  image(FCM, attr=k, col=gray((0:255)/255), axes=TRUE)
  title(main = paste("Class ", k, sep=""))
}

windows(title="Histograms of membership values")
Nrow <- round(sqrt(n_cl))
par(mfrow=c(Nrow, round(n_cl/Nrow)))

for(k in 1:n_cl) {
  hist(FCM@data[, k], main = paste("Class ", k, sep=""))
}


"Number of iterations:"
iter

"Mean values:"
V
#======================================================================================
# 6. Display feature space
#======================================================================================
k<-4
l<-2

windows(title="Feature space with legend")

plot(x[ind, k], x[ind, l], xlab = paste("Band", k, sep=""), ylab = paste("Band", l, sep=""), cex=0.1)

text_str <- "Class1"

for(i in 1:n_cl) {
  points(V[i, k], V[i, l], col=col_arr[i], cex=2, pch=parr[i])
  if(i>1) text_str <- c(text_str, paste("Class", i, sep=""))
}

legend("right", text_str, fill=col_arr)
#======================================================================================
# 7. Display confidence image
#======================================================================================
Win1 <- array(0, npix)
Win2 <- array(0, npix)

Win1p <- array(0, npix)
Win2p <- array(0, npix)

for(i in 1:npix) {
  k <- which.max(U[i, ])
  Win1[i] <- k
  Win1p[i] <- U[i, k]
  
  l <- which.max(U[i, -k])
  Win2[i] <- l
  Win2p[i] <- (U[i, -k])[l]
}

m12 <- 1 - (Win2p/Win1p)

Win <- FCM
Win$conf <- m12
Win$first <- Win1

col_arr <- rainbow(n_cl) 

windows()
par(mfrow=c(1, 2))
image(Win, attr="first", col=col_arr, axes=TRUE)
title("Winner 1")
plot.new()
legend("center", text_str, fill=col_arr)


windows()
par(mfrow=c(1, 2))
image(Win, attr="conf", col=gray((0:255)/255), axes=TRUE)
title(main="FCM result: confidence image")
hist(m12, main="Histogram of confidence image")
#======================================================================================
# 8. Output the results
#======================================================================================
setwd(Path_out)
# To ASCII text file
write.table(V, file = paste(Path_out, "/FCM_Mean.txt", sep=""), append=FALSE, quote=TRUE, sep =" ", eol="\n", na="NA", dec=".", row.names=FALSE, col.names=FALSE, qmethod=c("escape", "double"))
# To R data file
save(V, FCM, file=paste(Path_out, "/FCM_Results.RData", sep=""))

# To multiband raster image
# filename
imagefn.out <- "FCM.tif"

OUT <- FCM
OUT@data <- data.frame(FCM@data[, ])
OUT.tif<-create2GDAL(OUT, drivername="GTiff", type="Float32")
saveDataset(OUT.tif, imagefn.out)
GDAL.close(OUT.tif)

setwd(Root)
#======================================================================================
# The End
#======================================================================================

```

