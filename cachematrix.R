## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#Consists of a list with set, get, setInverse, getInverse
library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL    #Initializin inverse as NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
   get<-function(){x} #Get matrix x 
   setInverse<-function(inverse){inv<<-inverse}
   getInverse<-function() {
     inver<-ginv(x)
     inver%*%x
   }
   list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## Write a short comment describing this function
#Used to get cache data
cacheSolve <- function(x, ...) {
  inv<-x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
     mat<-x$get
     inv<-solve(mat, ...)
     x$setInverse(inv)
     inv ## Return a matrix that is the inverse of 'x'
}
