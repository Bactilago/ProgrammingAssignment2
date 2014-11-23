##The first function, makeVector creates a special "vector", 
##which is really a list containing a function to
## 1. set the value of the vector
## 2. get the value of the vector
## 3. set the value of the mean
## 4. get the value of the mean



makeVector <- function(x = numeric()) {
  # sets x equal to an empty numeric vector
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get, setmean = setmean      , getmean = getmean)
  
}

##The following function calculates the mean of the special "vector" created with the above function. 
##However, it first checks to see if the 
##mean has already been calculated. If so, it gets the mean from the cache and skips the computation.
##Otherwise, it calculates the mean of the data and sets the value of the mean in the cache 
##via the setmean function.


cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}





## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


## PART 1
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  a<- NULL
## matrix will start empty  and the inverse as null

  set <- function(y){
    x <<- y
    a<<- NULL
}
##returns the value of the original vector
get <- function() x

## this replaces the previous value and creates the inverse (solve) function
Inv <- function(solve) a <<- solve

##returns the inverse
getInv <- function() a

## list the functions
list(set = set, get = get, Inv = Inv, getInv = getInv)
}

## Write a short comment describing this function
##PART 2
cacheSolve <- function(x, ...) {

  ##returns the inverse of x
  a <- x$getInv()  
## If the value is of the inverse is not null returns the value and displays a message. 
 
  if(!is.null(a)){
    message("getting cached data")
    return(a)
  
}

## If null then calculate with solve
data <- x$get()
a <- solve(data, ...)
x$setInverse(a)
a ## returns the value of a
}
## end 