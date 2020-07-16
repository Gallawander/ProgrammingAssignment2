### These functions take a matrix and return its inverted form. ###

## Makes a cachable matrix

makeCacheMatrix <- function(orig.m = matrix()) {
  
  # Initialization of the inverse property
  inv.m <- NULL
  
  # Process to set the original matrix
  set <- function(m){
    orig.m <<- m
    inv.m <<- NULL
  }
  
  # Process to get the original matrix
  get <- function() orig.m
  
  # Process to set the inverted matrix
  set.inv <- function(solve) inv.m <<- solve
  
  # Process to get the inverted matrix
  get.inv <- function() inv.m
  
  list(set = set,
       get = get,
       set.inv = set.inv,
       get.inv = get.inv)
}


## Computes the inverse of the cachable matrix

cacheSolve <- function(cache.m, ...) {
  inv.m <- cache.m$get.inv()
  
  #Checks for the cachable matrix
  if(!is.null(inv.m)){
    message("Getting inverted matrix")
    return(inv.m)
  }
  
  # Computes inverted matrix from data
  data <- cache.m$get()
  inv.m<-solve(data, ...)
  cache.m$set.inv(inv.m)
  inv.m
}
