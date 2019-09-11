## makecacheMatrix creates a matrix mat and can cache inv=mat^(-1)
## whereas cacheSolve returns the inverse of mat; of course, if
## the inverse has been already calculated, cacheSolve finds the
## inverse in the cache

## Creating the matrix mat

makeCacheMatrix <- function(mat = matrix()) {
  #inv is the inverse. For now inv=NULL
  inv <- NULL
  #This function assigns a value to the matrix and inv=NULL
  set <- function(y) {
    mat <<- y
    #If we invoke set, then inv has to be NULL again. Otherwise,
    #the cached inverse would be wrong
    inv <<- NULL
  }
  #get returns the value of the matrix mat
  get <- function() mat
  #setinv assigns solve to inv
  setinv <- function(solve) inv <<- solve
  #getinv returns inv
  getinv <- function() inv
  #The output is a list with the defined functions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## Computing the inverse of x / finding it in the cache

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  #This checks whether inv exists and then returns it
  if(!is.null(inv)) {
    message("getting cached inverse")
    #Return inv
    return(inv)
  }
  #I store the matrix in data
  data <- x$get()
  #I compute the inverse of data and assign its value to inv
  inv <- solve(data, ...)
  #This caches the inverse
  x$setinv(inv)
  #Return inv
  inv
}
