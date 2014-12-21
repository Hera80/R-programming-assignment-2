# Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
f<-makeCacheMatrix(matrix(1:4, nrow=2,ncol=2)) ##Storing a test matrix function in a variable
 
#creating the Cache Inverse Matrix
makeCacheMatrix <- function(x = matrix()) {
  a<-NULL
  set<-function(y){
    x<<-y
    a<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) a<<- solve
  getmatrix<-function() a
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

##The second function cacheSolve calls functions stored in the special "matrix" returned by makeCacheMatrix. If the inverse has already been calculated and the matrix has not changed, then cacheSolve retrieves the inverse from the cache. If the input is new, it calculates the inverse of the data and sets the inverse in the cache.

cacheSolve <- function(x=matrix(), ...) {
  a<-x$getmatrix()
  if(!is.null(a)){
    message("getting cached data")
    return(a)
  }
  matrix<-x$get()
  a<-solve(matrix, ...)
  x$setmatrix(a)
  a 
  }
