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
