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

## Returns a matrix that is an inverse of a matrix stored in m_list
## from the cache, if exists and the cached matrix has not been changed
## using solve function, if it is not cached
cacheSolve <- function(m_list) {
  mOrg <- m_list$getOrg()
  mInv <- m_list$getInv()
  mdata <- m_list$get()
  if(!is.null(mInv) && identical(mdata, mOrg)) {
    message("getting cached data")
    return(mInv)
  }
  mInv <- solve(mdata)
  m_list$setBoth(mdata, mInv)
  mInv
}
