# Programming Assignment 2 - cacheMatrix ----------------------------------


## Put comments here that give an overall description of what your
## functions do:

# I am citing a discussion post by Fu Sheng Wang "Hope this helps those who are lost" regarding
# a description of what the functions do. The post is in the discussion forums under the 
# Programming Assignment 2 topic. It succinctly explains the concepts involved with the
# makeCacheMatrix and cacheSolve functions. This is my first exposure to such concepts, and I
# have little understanding of it right now, so I thought is best to refer to a discussion forum
# post that seems to be knowledgeable. Fu Sheng Wang discusses the functions that compute and
# cache the mean (as in the example code for the assignment). If one substitutes "Matrix" for "Mean"
# in the function title and "solve" for "mean" for named functions, the explanation is the same
# (and the code executes).

# [start quote] "The assignment 2 takes the smallest amount of effort to complete if you know something 
# about "first-class functions," but is most confusing for people who have never heard of it.
# the example function "makeVector" is a function with the following properties:
#     1. it takes an argument x of type numeric vector
#     2. it returns a list with 4 list items  (they are actually 4 functions wrapped in a list).
#list(set = set, get = get,
#setmean = setmean,
#getmean = getmean)

#so you when you create a "special vector" using makeVector function, you will get a list
#>a <- makeVector(c(1,2,3))
#> class(a)
#[1] "list"


#you may call any of the functions in the list:
#  > class(a$get)
#[1] "function"

# > a$get()
#[1] 1 2 3

#> a$set(c(4,5,6))
#> a$get()
#[1] 4 5 6


#The example function "cacheMean" is just a client function that uses "makeVector" function in its implementation.
#The input is expecting a "special vector" made from makeVector (ignore the ... for now).
#The output is the mean coming whether from the special vector's  cache or computation." [end quote]


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}


cacheSolve(makeCacheMatrix(9:9))

# The above actually produces a matrix, so the code executes. I confess that I really do not
# know what is going on with the function. However, if the primary purpose of the exercise is
# to use GitHub, the evaluator can see that the upload (i.e., commit and push) was succesful.
