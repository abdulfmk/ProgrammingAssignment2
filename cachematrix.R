## This code has 2 functions to it to calulate the inverse of matrix and store it in cache for future use
## function makeCacheMatrix() takes input matrix through the argument x and set the inital values to null
## Also this function creates a list which has all the child functions as element so that it can be used to call it in cacheresolve() function


## Takes input matrix and set the inital values as NULL on parent environment thorugh lexical scoping concept
## List with each functions as element so that it can used in cacheresolve() function

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setresolve<-function(resolve) m<<-resolve
  getresolve<-function() m
  list(set=set,get=get,setresolve=setresolve,getresolve=getresolve)
  
}


## This function performs the following
## Check for the matrix inverse from Cache
## Print the value from Cache if there is value stored in the cache
## Otherwise calculate the inverse of matrix with solve function and update the value using set resolve function

cacheresolve<-function(x,...){
  m<-x$getresolve()
  if(!is.null(m)) {
    print("Printing value from cache")
    return(m)
  }
  data<-x$get()
  m<-solve(data)
  x$setresolve(m)
  m
  }
