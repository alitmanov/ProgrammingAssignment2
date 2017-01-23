############################################################################
##
## makeCacheMatrix
##
## Create a 'special' matrix object which can be cached 
## returns a list of four functions to store(set), retrieve(get), 
## set matrix inverse function and retireve(get) matrix inverse function 
## from the object
## Works with conjunction with subsequent calls to cacheSolve function
############################################################################
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse
        )
}
############################################################################
##
## cacheSolve function
##
## Compute inverse matrix for a given matrix
## Produce error message when a inverse matrix cannot be computed because 
## 1. input object is not a matrix
## 2. inverse matrix cannot be computed because matrix determinant is zero
## 3. matrix is not a square matrix (not n by n)
## determinant is computed with determinant det() function
##
## Test case 1:
##		myMatrix<-matrix(c(1,2,3,4), nrow=2, ncol=2)
##		myCachedMatrix<-makeCacheMatrix(myMatrix)
##		cacheSolve(myCachedMatrix) # first call will cache inverse matrix
##		cacheSolve(myCachedMatrix) # first call will retrieve cached inverse matrix
##
## Test case 2: - not a square matrix
##		myMatrix<-matrix(c(1,2,3), nrow=3, ncol=1)
##		myCachedMatrix<-makeCacheMatrix(myMatrix)
##		cacheSolve(myCachedMatrix) # will produce message
##
## Test case 3: - will produce message that matrix is not invertable becasue det() is zero
##		myMatrix<-matrix(c(1,2,3,4,5,6,7,8,9), nrow=3, ncol=3)
##		myCachedMatrix<-makeCacheMatrix(myMatrix)
##		cacheSolve(myCachedMatrix) # first call will cache inverse matrix
##		cacheSolve(myCachedMatrix) # first call will retrieve cached inverse matrix
## Test case 4: a 3 by 3 valid invertatble matrix
##		myMatrix<-matrix(c(4,45,6,7,88,10,12,3,11), nrow=3, ncol=3)
##		myCachedMatrix<-makeCacheMatrix(myMatrix)
##		cacheSolve(myCachedMatrix)
##		cacheSolve(myCachedMatrix)
############################################################################
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x' 
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    ## Check if object is a matrix 
    if (!class(data)=="matrix") {
        message("not a matrix")
        return(NULL)
    }
    ## Check if matrix is n by n - a square matrix
    if (nrow(data)>=1 && nrow(data)!=ncol(data)) {
        message("not a square matrix")
        return(NULL)
    }
    ## Check if matrix is invertable - det() not equals to zero
    if (det(data)==0) {
        message("matrix is not invertable")
        return(NULL)
    }
    ## Compute inverse matrix
    m <- solve(data, ...)
    x$setinverse(m)
    message("creating new data and caching it")
    m
}

