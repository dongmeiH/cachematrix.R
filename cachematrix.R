########################## Programming assignment 2
###### caching  the inverse of a matrix 
## <<- is a operator which can be used to assign value to an object in an 
## environment that is different from the current
## makeCacheMatrix creates a special matrix than can cathe its inverse
## cacheSolve: computes the inverse of the special matrix returned by makeCatheMatrix
#1. set the value of the matrix
#2. get the value of the matrix
#3. set the value of the inverse
#4. get the value of the inverse

makeCacheMatrix <- function(x = numeric()){
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function()
	setinv <- function(solve) inv <<- solve
	getinv <- function() inv
	list(set = set, get = get, 
	    setinv = setinv
	    getinv = getinv)
}


# computes the inverse of the special matrix returned by makeCatheMatrix
# however, it first checks to see if the inverse has already been calculated. if so, it 
# gets the inverse from cache and skip the computation. otherwise, it calculates the inverse

cacheSolve <- function(x, ...) {
	inv <- x$getinv()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinv(inv)
	inv
}

# Test of calling the above function
m <- matrix(c(2, 4, 5, 3, 8, 4, 9, 1, 6), 3, 3)
# check if m is inversable
invtest <- det(m)
print(invtest)

catheSolve(m)
