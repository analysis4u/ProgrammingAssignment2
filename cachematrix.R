## Put comments here that give an overall description of what your
## functions do

## Creates a list from the given matrix to hold the cached value
makeCacheMatrix <- function(m = matrix()) {
  mOrg <- NULL
  mInv <- NULL
  set <- function(mx) {
    m <<- mx
    mOrg <<- NULL
    mInv <<- NULL
  }
  get <- function() m
  setBoth <- function(Org, Inv) {
    mOrg <<- Org 
    mInv <<- Inv
  }
  getOrg <- function() mOrg
  getInv <- function() mInv
  
  list(set = set, get = get,
       setBoth = setBoth,
       getOrg = getOrg,getInv = getInv)
}

## Returns a matrix that is an inverse of m
## from the cache, if exists and the cached matrix has not been changed
## using solve function, if it is not cached
cacheSolve <- function(m) {
  mOrg <- m$getOrg()
  mInv <- m$getInv()
  mdata <- m$get()
  if(!is.null(mInv) && identical(mdata, mOrg)) {
    message("getting cached data")
    return(mInv)
  }
  mInv <- solve(mdata)
  m$setBoth(mdata, mInv)
  mInv
}
