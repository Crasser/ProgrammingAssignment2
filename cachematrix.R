## Following code produces first a list containing functions that allow the user to store a matrix for inversion or store
## an inverted matrix for later use in the cache memory. The second function takes the list as input and returns an inverted matrix


## makeCacheMatrix, returns a list containing 
## 1) a function set(data) which ascribes a matrix for which the inverse can be computed
## 2) a function get() which returns the matrix for inversion
## 3) a function set_inverse(data) which sets the inverted matrix and, if already defined, 
##    overwrites the cached inverted matrix
## 4) a function get_inverse() returns the inverted matrix in cached memory





makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        set_inverse <- function(inverse) i<<-inverse
        get_inverse <- function() i
        
        list(   set = set,
                get = get,
                set_inverse = set_inverse,
                get_inverse = get_inverse)

}


## cacheSolve() takes as input the list created by makeCacheMatrix. If the list contains a
## cached inverted matrix, then this matrix is returned. If the list does not contain an inverted matrix,
## i.e. get_inverse==NULL, then cacheSolve computes the inverse by the standard solve function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                i<- x$get_inverse()      
# If inverse matrix has already been stored, then this inverse is loaded. Otherwise the inverse is computed
        if (!is.null(i)) {
                return(i)
        } else {
                matrix_to_invert<- x$get()
                i <- solve(matrix_to_invert)
                x$set_inverse(i)
                return(i)
        }
}
