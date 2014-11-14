## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
  
  invers <- NULL
  #setter
  set<- function(y){
    x <<- y
    invers<<-NULL
  }
  #getter
  get<- function() x
  
  #setter inverse
  setinvers <- function(inverse) invers<<- inverse
  #getter inverse
  getinvers <- function() invers
  
  list(set=set,get=get,setinvers=setinvers,getinvers=getinvers)
}
## Write a short comment describing this function
cacheSolve <- function(x, ...) {
  ## Get inverse matrix 
  inv <- x$getinvers()
  # If inverse is not null -> already calculated -> return inverse
  if (!is.null(inv)) {
    message("getting inverse matrix from cache")
    return(inv)
  }
  # else -> calculate inverse
  data <- x$get()
  inv <- solve(data, ...)
  # Cache the inverse
  x$setinvers(inv)
  # Return it
  inv
}
makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}
cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting inverse matrix from cache")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}