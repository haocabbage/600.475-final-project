# read data
setwd("/Users/haocabbage/Desktop/Letter Recognition")
letter_data <- read.table("letter_data.txt", header = FALSE)
letter_names <- read.table("letter_names.txt", header = FALSE)
colnames(letter_data) <- unlist(letter_names)
size <- dim(letter_data)[1]
dim.x <- 16
dim.y <- 8
dim.norm <- 25

# matrix containing the pixel info
pixels <- matrix(0, size, dim.x*dim.y)
for (i in 1:size) {
  pixels[i,] <- unlist(letter_data[i, 7:134])
}

# moment normalization with sine of aspect ratio
getMass <- function(m) {
  # return m10, m01 and m00
  dim.x <- dim(m)[1]
  dim.y <- dim(m)[2]
  mass.x <- 0
  mass.y <- 0
  mass <- 0
  
  for (i in 1:dim.x) {  
    for (j in 1:dim.y) {
      mass.x <- mass.x + j*m[i,j]
      mass.y <- mass.y + i*m[i,j]
      mass <- mass + m[i,j]
    }
  }
  
  return(c(mass.x, mass.y, mass))
}

getMu <- function(m, masses) {
  # return m20 and m02
  dim.x <- dim(m)[1]
  dim.y <- dim(m)[2]
  mass.x <- masses[1]
  mass.y <- masses[2]
  mu.x <- 0
  mu.y <- 0
  
  for (i in 1:dim.x) {  
    for (j in 1:dim.y) {
      mu.x <- mu.x + (j-mass.x)^2*m[i,j]
      mu.y <- mu.y + (i-mass.y)^2*m[i,j]
    }
  }
  
  return(c(mu.x, mu.y))
}

recenter <- function(m, center, x.new, y.new) {
  # put m on a [x.new, y.new] plane centered at its centroid 
  # construct a matrix containing both the original one
  # and the recentered one
  x <- dim(m)[2]
  y <- dim(m)[1]
  x.center <- center[1]
  y.center <- center[2]
  x.center.new <- round(x.new/2)
  y.center.new <- round(y.new/2)
  diff <- c()
  diff[1] <- max(x.center - 1, x - x.center)
  diff[2] <- max(y.center - 1, y - y.center)
  diff[3] <- max(x.center.new - 1, x.new - x.center.new)
  diff[4] <- max(y.center.new - 1, y.new - y.center.new)
  
  side <- max(diff)
  m.big <- matrix(0, 2*side + 1, 2*side + 1)
  center.new <- side + 1
  
  # put the original matrix into the big one
  start.x <- center.new - x.center + 1
  start.y <- center.new - y.center + 1
  end.x <- start.x + x - 1
  end.y <- start.y + y - 1
  m.big[start.y:end.y, start.x:end.x] <- m
  
  # carve out the recentered one
  start.x <- center.new - x.center.new + 1
  start.y <- center.new - y.center.new + 1
  end.x <- start.x + x.new - 1
  end.y <- start.y + y.new - 1
  m.recentered <- m.big[start.y:end.y, start.x:end.x]
  
  return(m.recentered)
}

normalize <- function(m, dim.norm) {
  dim.x <- dim(m)[1]
  dim.y <- dim(m)[2]
  masses <- getMass(m)
  mass.x <- masses[1]
  mass.y <- masses[2]
  mass <- masses[3]
  x.center <- round(mass.x/mass)
  y.center <- round(mass.y/mass)
  mus <- getMu(m, masses)
  mu.x <- mus[1]
  mu.y <- mus[2]
  
  # new dimension 
  W1 <- round(4*sqrt(mu.x/mass))
  H1 <- round(4*sqrt(mu.y/mass))
  m <- recenter(m, c(x.center, y.center), H1, W1)
  draw(as.vector(t(m)), H1, W1)
  
  W2 <- 0
  H2 <- 0
  R1 <- min(W1, H1)/max(W1, H1)
  R2 <- sqrt(sin(R1*pi/2))
  
  if (W1 > H1) {
    W2 <- dim.norm
    H2 <- round(R2*dim.norm)
  } else {
    H2 <- dim.norm
    W2 <- round(R2*dim.norm)
  }

  alpha <- W2/W1
  beta <- H2/H1
  xc <- round(W1/2)
  yc <- round(H1/2)
  xc.prime <- round(W2/2)
  yc.prime <- round(H2/2)
  m.norm <- matrix(0, H2, W2)
  
  for (i in 1:H2) {
    for (j in 1:W2) {
      x.temp <- min(W1, round((j - xc.prime)/alpha + xc))
      y.temp <- min(H1, round((i - yc.prime)/beta + yc))
      
      m.norm[i,j] <- m[y.temp, x.temp]
    }
  }
  
  masses.norm <- getMass(m.norm)
  mass.x.norm <- masses.norm[1]
  mass.y.norm <- masses.norm[2]
  mass.norm <- masses.norm[3]
  center.x.norm <- round(mass.x.norm/mass.norm)
  center.y.norm <- round(mass.y.norm/mass.norm)
  
  m.norm <- recenter(m.norm, c(center.x.norm, center.y.norm), 
                     dim.norm, dim.norm)
  
  return(m.nrom)
}

pixels.norm <- matrix(0, size, dim.norm^2)

for (i in 1:1) {
  temp.m <- matrix(pixels[i,], dim.x, dim.y, byrow=TRUE)
  temp.m <- normalize(temp.m, dim.norm)
  pixels.norm[i,] <- as.vector(t(temp.m))
}

# ========feature extraction==========
# =========sobel===========
getSobel <- function(v, dim.x, dim.y) {
  # calculate the sobel gradient of an image
  m <- matrix(v, dim.x, dim.y, byrow = TRUE)
  m.Gx <- matrix(0, dim.x, dim.y)
  m.Gy <- matrix(0, dim.x, dim.y)
  
  for (i in 1:dim.x) {
    for (j in 1:dim.y) {
      neighbors <- rep(0, 8)
      # p0
      if (j+1 <= dim.y) {
        neighbors[1] <- m[i, j+1]}
      # p1
      if (i+1 <= dim.x & j+1 <= dim.y) {
        neighbors[2] <- m[i+1, j+1]}
      # p2
      if (i+1 <= dim.x) {
        neighbors[3] <- m[i+1, j]}
      # p3
      if (i+1 <= dim.x & j-1 >= 1) {
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
      if (i-1 >= 1 & j+1 <= dim.y) {
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

Gx.sobel <- matrix(0, size, dim.x*dim.y)
Gy.sobel <- matrix(0, size, dim.x*dim.y)

for (i in 1:size) {
  Gxy <- getSobel(pixels[i,], dim.x, dim.y)
  Gx.sobel[i,] <- Gxy[[1]]
  Gy.sobel[i,] <- Gxy[[2]]
}

# decomposition into 8 orientations
# construct 8 empty images
subimages <- list()
for (i in 1:size) {
  subimages[[i]] <- list()
  for (k in 1:8) {
    subimages[[i]][[k]] <- rep(0, dim.x*dim.y) 
  }     
}

# update subimages
for (i in 1:2) {
  temp.Gx <- Gx.sobel[i,]
  temp.Gy <- Gy.sobel[i,]
  for (j in 1:(dim.x*dim.y)) {
    temp.gx <- temp.Gx[j]
    temp.gy <- temp.Gy[j]
    # x- and y-axis
    if (temp.gx == 0 | temp.gy == 0) {
      if (temp.gx == 0) {
        if (temp.gy > 0) {
          subimages[[i]][[3]][j] <- abs(temp.gy)   
        } else {
          subimages[[i]][[7]][j] <- abs(temp.gy)
        } 
      } 
      if (temp.gy == 0) {
        if (temp.gx > 0) {
          subimages[[i]][[1]][j] <- abs(temp.gx)
        } else {
          subimages[[i]][[5]][j] <- abs(temp.gx)
        } 
      }
    } 
    # other 4 orientations
    else if (temp.gx == temp.gy) {
      if (temp.gx > 0 & temp.gy > 0) {
        subimages[[i]][[2]][j] <- sqrt(temp.gy^2 + temp.gx^2)   
      }
      if (temp.gx > 0 & temp.gy < 0) {
        subimages[[i]][[8]][j] <- sqrt(temp.gy^2 + temp.gx^2)    
      }
      if (temp.gx < 0 & temp.gy > 0) {
        subimages[[i]][[4]][j] <- sqrt(temp.gy^2 + temp.gx^2)    
      }
      if (temp.gx < 0 & temp.gy < 0) {
        subimages[[i]][[6]][j] <- sqrt(temp.gy^2 + temp.gx^2)    
      }
    } 
    # between standard orientations
    else {
      if (temp.gx > 0 & temp.gy > 0) {
        if (abs(temp.gx) > abs(temp.gy)) {
          subimages[[i]][[1]][j] <- abs(temp.gx) - abs(temp.gy)
          subimages[[i]][[2]][j] <- sqrt(2)*abs(temp.gy)
        } else {
          subimages[[i]][[3]][j] <- abs(temp.gy) - abs(temp.gx)
          subimages[[i]][[2]][j] <- sqrt(2)*abs(temp.gx)  
        }      
      }
      if (temp.gx > 0 & temp.gy < 0) {
        if (abs(temp.gx) > abs(temp.gy)) {
          subimages[[i]][[3]][j] <- abs(temp.gy) - abs(temp.gx)
          subimages[[i]][[4]][j] <- sqrt(2)*abs(temp.gx)     
        } else {
          subimages[[i]][[5]][j] <- abs(temp.gx) - abs(temp.gy)
          subimages[[i]][[4]][j] <- sqrt(2)*abs(temp.gy) 
        }       
      }
      if (temp.gx < 0 & temp.gy > 0) {
        if (abs(temp.gx) > abs(temp.gy)) {
          subimages[[i]][[5]][j] <- abs(temp.gx) - abs(temp.gy)
          subimages[[i]][[6]][j] <- sqrt(2)*abs(temp.gy)     
        } else {
          subimages[[i]][[7]][j] <- abs(temp.gy) - abs(temp.gx)
          subimages[[i]][[6]][j] <- sqrt(2)*abs(temp.gx) 
        }     
      }
      if (temp.gx < 0 & temp.gy < 0) {
        if (abs(temp.gx) > abs(temp.gy)) {
          subimages[[i]][[7]][j] <- abs(temp.gy) - abs(temp.gx)
          subimages[[i]][[8]][j] <- sqrt(2)*abs(temp.gx)     
        } else {
          subimages[[i]][[1]][j] <- abs(temp.gx) - abs(temp.gy)
          subimages[[i]][[8]][j] <- sqrt(2)*abs(temp.gy) 
        }        
      }    
    }
  }
}

# =========kirsh===========

# ======gaussian mask======

# ===============plot=================
rotate <- function(m) {
  # rotate the square matrix 90 degree to the right
  rev <- t(m)
  for (i in 1:dim(rev)[1]) {
    rev[i,] <- rev(m[,i])
  }
  return(rev)
}

draw <- function(v, dim.x, dim.y) {
  temp <- matrix(v, nrow = dim.x, ncol = dim.y, byrow = TRUE)
  temp <- rotate(temp)
  image(temp, col = gray(255:0/255), axes = FALSE)
}
 
# =======testing=============
test.sobel <- getSobel(pixels[1,])
test.sample <- matrix(pixels[1,], 16, 16, byrow = TRUE)
test.gx <- matrix(test.sobel[[1]], 16, 16, byrow = TRUE)
test.gy <- matrix(test.sobel[[2]], 16, 16, byrow = TRUE)

for (i in 1:10) {
  draw(pixels[i,],16,8)
  draw(pixels.norm[i,],25,17)
}

