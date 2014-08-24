## Put comments here that give an overall description of what your
## functions do

##  makeCacheMatrix() is a function that takes a 2*2 matrix as input, computes 
## the inversed matrix and stores it in a variable called inversedmat 

makeCacheMatrix <- function(amat = matrix()) {
  inversedmat <- NULL
  set <- function(imat){
     amat <<- imat
     inversedmat <<- NULL
  }
  get <- function(){ amat }
  setInverse <- function(solve){ inversedmat <<- solve(amat) }
  getInverse <- function(){ inversedmat }
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

## cacheSolve() is afunction that computes the inverse of a matrix passed to it ## as an argument, for the 1st time or retreives the cached inversed matrix if  ## it had been called previously with the same argument 

cacheSolve <- function(a, ...) {
        ## Return a matrix that is the inverse of 'a'
    inversedmat <- a$getInverse()
    if(!is.null(inversedmat)){
        message("getting cached data")
	return(inversedmat)
    }
    data <-a$get()
    inversedmat <- solve(data, ...)
    a$setInverse(inversedmat)
    inversedmat
}
