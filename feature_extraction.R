# read data
setwd("/Users/Haocabbage/Desktop/Letter_Recognition")
letter_data <- read.table("letter_data.txt", header = FALSE)
letter_names <- read.table("letter_names.txt", header = FALSE)
colnames(letter_data) <- unlist(letter_names)
size <- dim(letter_data)[1]
dim.x <- 16
dim.y <- 8
dim.norm <- 25
size.gaussian <- 5

# matrix containing the pixel info
pixels <- matrix(0, size, dim.x*dim.y)
for (i in 1:size) {
  pixels[i,] <- unlist(letter_data[i, 7:134])
}

# normalization with sine of aspect ratio
normalize <- function(m, dim.norm) {
  W1 <- dim(m)[2]
  H1 <- dim(m)[1]
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
  m.norm <- matrix(0, H2, W2)
  
  for (i in 1:H2) {
    for (j in 1:W2) {
      x.temp <- min(W1, max(1, round(j/alpha)))
      y.temp <- min(H1, max(1, round(i/beta)))
      m.norm[i,j] <- m[y.temp, x.temp]
    }
  }
  
  # zero padding
  if (W2 != H2) {
    if (H2 > W2) {
      left <- floor((H2 - W2)/2) 
      right <- ceiling((H2 - W2)/2)
      pad1 <- matrix(0, dim.norm, left)
      pad2 <- matrix(0, dim.norm, right)
      m.norm <- cbind(pad1, m.norm, pad2)
    } else {
      up <- floor((W2 - H2)/2) 
      down <- ceiling((W2 - H2)/2)
      pad1 <- matrix(0, up, dim.norm)
      pad2 <- matrix(0, down, dim.norm)
      m.norm <- rbind(pad1, m.norm, pad2)  
    } 
  }
  
  return(m.norm)
}

runtime <- proc.time()

pixels.norm <- matrix(0, size, dim.norm^2)

for (i in 1:size) {
  temp.m <- matrix(pixels[i,], dim.x, dim.y, byrow=TRUE)
  temp.m <- normalize(temp.m, dim.norm)
  pixels.norm[i,] <- as.vector(t(temp.m))
}

proc.time() - runtime

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

runtime <- proc.time()

Gx.sobel <- matrix(0, size, dim.norm^2)
Gy.sobel <- matrix(0, size, dim.norm^2)

for (i in 1:size) {
  Gxy <- getSobel(pixels.norm[i,], dim.norm, dim.norm)
  Gx.sobel[i,] <- Gxy[[1]]
  Gy.sobel[i,] <- Gxy[[2]]
  print(i)
}

proc.time() - runtime

# decomposition into 8 orientations
# construct 8 empty images
runtime <- proc.time()

subimages <- list()

for (i in 1:size) {
  subimages[[i]] <- list()
  for (k in 1:8) {
    subimages[[i]][[k]] <- rep(0, dim.norm^2) 
  }     
}

# update subimages
for (i in 1:size) {
  temp.Gx <- Gx.sobel[i,]
  temp.Gy <- Gy.sobel[i,]
  for (j in 1:(dim.norm^2)) {
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
      if (temp.gx < 0 & temp.gy > 0) {
        if (abs(temp.gx) > abs(temp.gy)) {
          subimages[[i]][[5]][j] <- abs(temp.gx) - abs(temp.gy)
          subimages[[i]][[4]][j] <- sqrt(2)*abs(temp.gy)     
        } else {
          subimages[[i]][[3]][j] <- abs(temp.gy) - abs(temp.gx)
          subimages[[i]][[4]][j] <- sqrt(2)*abs(temp.gx) 
        }       
      }
      if (temp.gx < 0 & temp.gy < 0) {
        if (abs(temp.gx) > abs(temp.gy)) {
          subimages[[i]][[5]][j] <- abs(temp.gx) - abs(temp.gy)
          subimages[[i]][[6]][j] <- sqrt(2)*abs(temp.gy)     
        } else {
          subimages[[i]][[7]][j] <- abs(temp.gy) - abs(temp.gx)
          subimages[[i]][[6]][j] <- sqrt(2)*abs(temp.gx) 
        }     
      }
      if (temp.gx > 0 & temp.gy < 0) {
        if (abs(temp.gx) > abs(temp.gy)) {
          subimages[[i]][[1]][j] <- abs(temp.gx) - abs(temp.gy)
          subimages[[i]][[8]][j] <- sqrt(2)*abs(temp.gy)     
        } else {
          subimages[[i]][[7]][j] <- abs(temp.gy) - abs(temp.gx)
          subimages[[i]][[8]][j] <- sqrt(2)*abs(temp.gx) 
        }        
      }    
    }
  }
  print(i)
}

proc.time() - runtime

# ======gaussian mask======
gau_blur <- function(m, loc, sigma) {
  # get blurred value at (loc[1], loc[2]) with gaussian filter   
  side <- dim(m)[1]
  loc.x <- loc[1]
  loc.y <- loc[2]
  value <- 0
  
  for (i in 1:side) {
    for (j in 1:side) {
      h <- exp(-((loc.x-i)^2+(loc.y-j)^2)/2/sigma^2)/2/pi/sigma^2
      value <- value + m[i,j]*h
    }
  }
  
  return(value)
}

gau_sample <- function(m, size) {
  # return a gaussian sample of size*size
  m.new <- matrix(0, size, size)
  side <- dim(m)[1]
  sigma <- sqrt(2)*(side/size)/pi
  step <- floor(side/(2*size))
  
  for (i in 1:size) {
    for (j in 1:size) {
      loc.x <- i*size - step
      loc.y <- j*size - step
      m.new[i,j] <- gau_blur(m, c(loc.x, loc.y), sigma)
    }
  }
  
  return(m.new)
}

# sampling subimages
runtime <- proc.time()

subgaussians <- list()

for (i in 1:size) {
  subgaussians[[i]] <- list()
  for (k in 1:8) {
    temp.sub <- subimages[[i]][[k]]
    temp.m <- matrix(temp.sub, dim.norm, dim.norm, byrow = TRUE)
    temp.m <- gau_sample(temp.m, size.gaussian)
    subgaussians[[i]][[k]] <- as.vector(t(temp.m))
  }
  print(i)
}

proc.time() - runtime

# organize gradient features


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
  temp <- matrix(v, dim.x, dim.y, byrow = TRUE)
  temp <- rotate(temp)
  image(temp, col = gray(255:0/255), axes = FALSE)
}

# ========-data export=======
write.csv(pixels, file="pixels.csv")
write.csv(pixels.norm, file="pixels_norm.csv")

sample.gaussian <- matrix(0, size, size.gaussian^2*8)
image.sub <- matrix(0, size, dim.norm^2*8)

for (i in 1:size) {
  for (k in 1:8) {
    temp.gau <- subgaussians[[i]][[k]]
    temp.sub <- subimages[[i]][[k]]
    
    end.gau <- k*size.gaussian^2 
    end.sub <- k*dim.norm^2
    start.gau <- end.gau - size.gaussian^2 + 1
    start.sub <- end.sub - dim.norm^2 + 1
    
    sample.gaussian[i, start.gau:end.gau] <- temp.gau
    image.sub[i, start.sub:end.sub] <- temp.sub
  }  
}

write.csv(sample.gaussian, file="sample_gaussian.csv")
write.csv(image.sub, file="subimages.csv")

# =======testing=============
test.sobel <- getSobel(pixels[1,])
test.sample <- matrix(pixels[1,], 16, 16, byrow = TRUE)
test.gx <- matrix(test.sobel[[1]], 16, 16, byrow = TRUE)
test.gy <- matrix(test.sobel[[2]], 16, 16, byrow = TRUE)

for (i in 1:100) {
  draw(pixels[1,],16,8)
  draw(pixels.norm[1,],25,25)
}

for (i in 1:10) {
  for (k in 1:8) {
    mi <- matrix(subimages[[i]][[k]], 25, 25)
    draw(as.vector(t(mi)), 25, 25)  
  }
}

for (i in 1:100) {
  for (k in 1:8) {
    draw(subimages[[i]][[k]],25, 25)  
    draw(subgaussians[[i]][[k]],5, 5)  
  }
}

m1 <- matrix(subimages[[1]][[1]], 25, 25)
m2 <- matrix(Gx.sobel[1,], 25, 25)
m3 <- matrix(Gy.sobel[1,], 25, 25)
m11 <- matrix(subgaussians[[1]][[1]], 5, 5)
draw(as.vector(t(m1)), 25, 25)
draw(as.vector(t(m11)), 5, 5)

