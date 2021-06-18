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
