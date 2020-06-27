## Cousera Assignment


##This function creates a matrix and gives us the option to store its inverse

makeCacheMatrix <- function(x = matrix()) {
     i <- NULL
     set <- function(y){
          x<<-y
          i<<-NULL
     }
     get <- function(){
          return(x)
     }
     setinverse <- function(inverse){
          i <<- inverse
     }
     getinverse <- function(){
          return(i)
     }
     list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


##calculates the inverse of the data and sets the value of the inverse in the cache via the setmean function.

cacheSolve <- function(x, ...) {
        i<-x$getinverse()
        if(!is.null(i)){
             return(i)
        }
        data <-x$get()
        i<- solve(data)
        x$setinverse(i)
        i
}

