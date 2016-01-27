## Assignment 2 by Avi G.
## Overview. Based on a template for another purpose, modify it for a new purpose.
## Other then some text changes for names of variables and functions, it was straightforward.

## Executive Summary: Do a hard job only once and cache it if wanted again.

## Wrote a function that creates a new kind of object that is an augmented
## square matrix. You use it by calling one function called "makeCacheMatrix"
## that accepts a matrix as an argument and creates a list object that not only stores the matrix
## in a hidden environment but creates 4 sub-objects that are functions to get and set the
## underlying matrix and to get and set a chached inverse of the matrix when needed.

## The second function is related. Once you have matrix-like objects created using the first,
## you can at any time call "cacheSolve" with that object to get the inverse of that matrix.
## The function uses the methods of the object to see if the calculation has already been
## done nad simply returns the already known result. If not, the calculation is done and stored
## away so next time it is called for, an expensive calculation can be avoided.

## The following function takes an square matrix and embeds it into an environment and returns
## a new object we can call a "cache matrix" as it may not formaly have a name. The function accepts
## one argument that is a standards (square) matrix. It creates a list of 4 functions and returns
## that list as a representation of a new kind of object. The object then has a variable that persists
## and holds the matrix. One access function returns this value and another re-sets it.

## Two other functions can be used to return an inverse (if the matrix has not beeen changed)
## or to calculate the inverse and cache it. If you make a matrix called Mat and call this function as
## "Ret <- makeCacheMatrix(Mat)"
## Then you can access the matrix as Ret$get() and use it like any matrix. You can re-set the
## matrix with a new matric such as Mat2 using "Ret$set(Mat2)" which will null out any previously
## stored inverse. The last two functions are called as Ret$getsolve() to return an inverse and
# another to set the calculated inverse.

makeCacheMatrix <- function(x = matrix()) {
        # New object has no calculated inverse but requires one to be instantiated
        # so it remains available in the environment. Initial value is NULL.
        inv <- NULL
        
        # Creating a function to store a matrix argument in a persistent variable
        # called "x" and since this happens only when the value is set again,
        # the stored inverse, if any, is nulled.
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        # This trivial function retrieves the matrix currently stored in "x"
        get <- function() x
        
        # This function is called when the other main function has been called the
        # first time on the currently stored matrix and thus used "solve()" expensively
        # so it asks to store the result.
        setsolve <- function(solve) inv <<- solve 
        
        # This final function simply returns what is stored for the inverse, perhaps NULL.
        getsolve <- function() inv
        
        # The following creates a list of the four access functions and names them
        # with the same names so a user can ask for them by putting "$get()" or one of the
        # other three right after a variable name of this kind.
        # Since this is the last statement in this overall function, it returns this "object"
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
        
}


## This function is used only on objects created by the one above. It returns the inverse
## of the matrix-like object as a regular matrix. It uses the internal functions stored
## within the object (three of them) and, if needed, calls the actual "solve()" function
## to perform an expensive matrix inversion. If that has already been done, it returns the
## cached copy. Note the "..." argument is not used by this function but is passed along
## to the "solve()" function if you want to tune it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        # A local variable gets what inverse is currently stored within the object, or a NULL.
        inv <- x$getsolve()
        
        # If the inverse matrix exists, a non-null value is available. The user is notified
        # that they are getting a copy from the cache and that is what is returned.
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        # If execution reaches this point, then this is the first time the inverse
        # is being requested for this instance of the matrix and thus a null was returned.
        # No problem. You use the method on the object to get a matrix copy of this object.
        data <- x$get()
        
        # Now you call the real "solve()" function to get an inverse and store it locally as a matrix.
        # Note the assignment says to assume all matrices used will be invertible so no errors are
        # being handled.
        inv <- solve(data, ...)
        
        # Call the method on the matrix object that stores the local inverse copy for any
        # future requests.
        x$setsolve(inv)
        
        # As a final statement for this function, declare the inverse so it gets returned as
        # an object of type matrix
        inv
}
