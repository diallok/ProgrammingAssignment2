##the fuctions cache the inverse of a matrix
## the first funtion create a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
i<-NULL
		set<- function(y) {
			x<<-y
			i<<-NULL
		}
		get<- function() {
			x
		}
		setinv<- function(solve) {
			i<<-solve
		}
		getinv<- function() {
			i
		}
		list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## the second function compute the inverse of the matrix made by the first function,
## if the  inverse has already been made it retrieve it from the cache

cacheSolve <- function(x, ...) {
    i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}  

