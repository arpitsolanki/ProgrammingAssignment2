#Coursera R Programming Assignment 2, Lexical Scoping
#Arpit Solanki


#Cache matrix creates a list of four functions namely-
#1. get
#2. set
#3. getmatrix
#4. setmatrix
#
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

#Cache solve returns a inverse of the matrix inputted 
#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    message("Result already computed, returning cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}

mat<-matrix(data=c(1:4),nrow=2,ncol=2)
a<-makeCacheMatrix()
a$set(matrix(1:4,2,2))
cacheSolve(a)
