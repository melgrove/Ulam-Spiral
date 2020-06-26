# this is not the fastest way to make primes in R but it
# is fast enough on my macbook for making these graphics
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

polar_primes <- function(primes) {
  # converting to polar coords
  theta <- primes%%(2*pi)
  x <- primes*cos(theta)
  y <- primes*sin(theta)
  return(list(x=x, y=y))
}

# A PNG can be created by running the functions and plotting 
# note that you may want to reuse myPrimes for plotting due to computation time
myPrimes <- polar_primes(make_primes(1:100000))
# Exporting to png
png(filename = "primeTest3", width = 2048, height = 2048, units = "px", bg="black") #file specifications
plot(myPrimes[['x']], myPrimes[['y']], pch=20, col="lightblue", main="Primes Under 100000 in Polor Coordinates") #plotting the points
dev.off() # closes connection and saves


















