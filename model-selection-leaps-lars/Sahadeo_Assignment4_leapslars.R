# load packages
library(ISLR)
library(leaps)
library(lars)

head(Auto)

# get the original auto data and remove the name column
auto_data <- Auto[, -c(9)]

# define matrix
matrix.2ndorder.make <- function(x, only.quad = F) {
	x0<- x
	dimn <- dimnames(x)[[2]]
	num.col <- length(x[1, ])
	for(i in 1:num.col) {
		if(!only.quad){
			for(j in i:num.col){
				x0 <- cbind(x0, x[, i] * x[, j])
				dimn <- c(dimn, paste(dimn[i], dimn[j], sep = ""))
			}
		}else{
			x0 <- cbind(x0, x[, i] * x[, i])
			dimn <- c(dimn, paste(dim[i], "2", sep = ""))
		}
	}
	dimnames(x0)[[2]] <- dimn
	return(x0)
}

# create the US dataset
us_data <- auto_data[auto_data[, "origin"]==1,]

# store response varialbe 
us_mpg <- us_data[, 1]

# get predictor variable
us_predictors <- us_data[, -c(1,8)]

# create 2nd order model matrix for us
us_quad <- matrix.2ndorder.make(us_predictors)
us_quad
