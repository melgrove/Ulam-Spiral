# using my prime function from PolarPrimes.R
make_primes <- function(x) {
  prime_test <- function(n) {
    out <- TRUE
    if(n <= 1){return(FALSE)}
    if(n == 2){return(TRUE)}
    for(i in 2:floor(sqrt(n))) {
      if(n%%i == 0) {
        out <- FALSE
        break
      }
    }
    out
  }
  which(sapply(x,prime_test))
}

#first we need to get a list of coordinates in the order that they go
#then we will use a for loop to add a true or false to each element
#then we will plot all true points
#right = 1
#up = 2
#left = 3
#down = 4
ulam <- function(primes, squares) {
  repping <- rep(1:squares, each=2)
  vect <- numeric(0)
  dir <- 1
  for(n in repping){
    out <- rep(dir, n)
    vect <- append(vect, out)
    dir <- dir + 1
    if(dir == 5){dir <- 1}
  }
  direction <- vect
  coords <- list(c(0,0))
  for(n in 2:length(direction)){
    coord <- coords[[n-1]]
    if(direction[n-1] == 1){coord <- coord + c(1,0)}
    if(direction[n-1] == 2){coord <- coord + c(0,1)}
    if(direction[n-1] == 3){coord <- coord + c(-1,0)}
    if(direction[n-1] == 4){coord <- coord + c(0,-1)}
    coords[[n]] <- coord
  }
  #Use a list of primes as the index
  primecoords <- coords[] 
  #getting nested x and y coords
  x <- unlist(lapply(primecoords, `[[`, 1)) #this is a hack
  y <- unlist(lapply(primecoords, `[[`, 2)) #pretty cool that it works though
  return(list(x=x,y=y))
}

# A PNG can be created by running the functions and plotting 
myUlam <- ulam(make_primes(1:10000), 10000)

#png(filename = "ulam7", width = 5400, height = 5400, units = "px", bg="black") #file specifications
plot(myUlam[['x']], myUlam[['y']], pch=15, col="green") 
#dev.off() 









