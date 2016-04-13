# read data
setwd("/Users/haocabbage/Desktop/Letter Recognition")
letter_data <- read.table("letter_data.txt", header = FALSE)
letter_names <- read.table("letter_names.txt", header = FALSE)
colnames(letter_data) <- unlist(letter_names)

padding <- function(v) {
  m <- matrix(v, nrow = 16, ncol = 8, byrow = TRUE) 
  pad <- matrix(0, nrow = 16, ncol = 4)
  m <- cbind(pad, m, pad)
  v <- as.vector(t(m))
  return(v)
}

for (i in 1:52152) {
  temp <- padding(1)
  pixels[i,] <- padding(unlist(letter_data[i, 7:134]))
}

# ========feature extraction==========
# =========sobel===========
getSobel <- function(v) {
  # calculate the sobel gradient of an image
  m <- matrix(v, 16, 16, byrow = TRUE)
  m.Gx <- matrix(0, 16, 16)
  m.Gy <- matrix(0, 16, 16)
  
  for (i in 1:16) {
    for (j in 1:16) {
      neighbors <- rep(0, 8)
      # p0
      if (j+1 <= 16) {
        neighbors[1] <- m[i, j+1]}
      # p1
      if (i+1 <= 16 & j+1 <= 16) {
        neighbors[2] <- m[i+1, j+1]}
      # p2
      if (i+1 <= 16) {
        neighbors[3] <- m[i+1, j]}
      # p3
      if (i+1 <= 16 & j-1 >= 1) {
        neighbors[4] <- m[i+1, j-1]}
      # p4
      if (j-1 >= 1) {
        neighbors[5] <- m[i, j-1]}
      # p5
      if (i-1 >= 1 & j-1 >= 1) {
        neighbors[6] <- m[i-1, j-1]}
      # p6
      if (i-1 >= 1) {
        neighbors[7] <- m[i-1, j]}
      # p7
      if (i-1 >= 1 & j+1 <= 16) {
        neighbors[8] <- m[i-1, j+1]}
      
      # get Gx and Gy
      G.x <- neighbors[2] + 2*neighbors[3] + neighbors[4] -
             (neighbors[6] + 2*neighbors[7] + neighbors[8])
      G.y <- neighbors[2] + 2*neighbors[1] + neighbors[8] -
             (neighbors[4] + 2*neighbors[5] + neighbors[6])

      m.Gx[i,j] <- G.x
      m.Gy[i,j] <- G.y
    }
  }
  return (list(as.vector(t(m.Gx)), as.vector(t(m.Gy))))
}

Gx.sobel <- matrix(0, 52152, 256)
Gy.sobel <- matrix(0, 52152, 256)

for (i in 1:52152) {
  Gxy <- getSobel(pixels[i,])
  Gx.sobel[i,] <- Gxy[[1]]
  Gy.sobel[i,] <- Gxy[[2]]
}
  
# =========kirsh===========

# ===============plot=================
rotate <- function(m) {
  # rotate the square matrix 90 degree to the right
  rev <- t(m)
  for (i in 1:dim(rev)[1]) {
    rev[i,] <- rev(m[,i])
  }
  return(rev)
}

draw <- function(v, x, y) {
  temp <- matrix(v, nrow = x, ncol = y, byrow = TRUE)
  temp <- rotate(temp)
  image(temp, col = gray(255:0/255), axes = FALSE)
}

# plot data, 16 samples in one image
setwd("/Users/haocabbage/Desktop/Letter Recognition/image")
seq <- 16*c(1:floor(52152/16))
for (i in seq) {
  temp.m <- matrix()
  for (j in 1:4) {
    temp.row <- matrix()
    for (k in 1:4) {
      temp.v <- matrix(pixels[i-16+j*k, ], 16, 16, byrow = TRUE)
      if (k == 1) {
        temp.row <- temp.v
      } else {
        temp.row <- cbind(temp.row, temp.v)    
      }
    }
    if (j == 1) {
      temp.m <- temp.row
    } else {
      temp.m <- rbind(temp.m, temp.row)  
    }
  }     
  name <- paste(i-15, "_", i, ".png", sep = "")
  png(filename = name, width = 1280, height = 1280)
  draw(as.vector(t(temp.m)), 64, 64)
  dev.off()  
}

i = 5260
temp.m <- matrix()
for (j in 1:2) {
  temp.row <- matrix()
  for (k in 1:4) {
    temp.v <- matrix(pixels[i-16+j*k, ], 16, 16, byrow = TRUE)
    if (k == 1) {
      temp.row <- temp.v
    } else {
      temp.row <- cbind(temp.row, temp.v)    
    }
  }
  if (j == 1) {
    temp.m <- temp.row
  } else {
    temp.m <- rbind(temp.m, temp.row)  
  }
}     

temp.row <- matrix(0, 32, 64)
temp.m <- rbind(temp.m, temp.row)
png(filename = "52147_52152.png", width = 1280, height = 1280)
draw(as.vector(t(temp.m)), 64, 64)
dev.off()    


# =======testing=============
test.sobel <- getSobel(pixels[1,])
test.sample <- matrix(pixels[1,], 16, 16, byrow = TRUE)
test.gx <- matrix(test.sobel[[1]], 16, 16, byrow = TRUE)
test.gy <- matrix(test.sobel[[2]], 16, 16, byrow = TRUE)
