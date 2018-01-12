# Assignment 2 for R programming coursera course

# This code provide a code that run the inverse of a matrix, only this inverse in not yet stored in the cache. 

# Why? Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse
# of a matrix rather than compute it repeatedly.

# note that the inputed matrix has to be invertible. 

# Catherine Pfeifer, 
# created on 12.01.2018
# last modified on 12.01.2018




## Write a short comment describing this function 

# the makeCacheMatrix function consists of 4 commands 
# set = set the dimensions of the matrix
# get = read in the matrix 
# setinverse = sets the computation for the inverse
# getinverse = computes the inverse



makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }# end of set function 
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}# end of makeCacheMatrix



## Write a short comment describing this function

# the cacheSolve function first checks if there is a cached matric, if so it returns the cached matrix. 
# Otherwise it computes the inverste and sets it into the cache. 
# setinverse function.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }# end of if clause
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}# end of cacheSolve function 




#################################let's test the functions here###########################


x = rbind(c(1, 2), c(2, 1))
m = makeCacheMatrix(x)
m$get()
# this return the right initial matrix 

# As there is no info in the cache at the beginning
cacheSolve(m)
#this command returns the inverse matrix 

#Lets run it a second time 
cacheSolve(m)
# this time we get the message "getting cached data"

# it works!!!