#The idea of these following functions is to save computation time.
#Calculating an inverse of large matrix might take a long time so it is better to save
#inverse calculation to be used again in the next computation rather than recomputing.

#Function makeCacheMatrix receives a matrix as its argument. It has 4 built in function (set, get, setmean and getmean).
#Those functions are self-explanatory and will be used on cacheSolve() function. makeCacheMatrix will return a list of 4
#elements which will be fed to cacheSolve() function.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


#Function cacheSolve receives a makeCacheMatrix object as its arguments. The argument is actually a list with 4 elements
#which each of them is a function. cacheSolve first check whether the list already has inverse pre-calculated before or not
#using the built-in getinverse() function. If it is already pre-calculated then there is no need to calculate the inverse
#of the matrix using solve(), rather simply return the cached data. If it has not been calculated before then it needs to be
#calculated and then the result will be saved so that the next call on the same matrix will need no calculation.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
