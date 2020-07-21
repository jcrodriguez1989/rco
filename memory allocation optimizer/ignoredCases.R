## Problem1: Case where their is an expression instead of a symbol in place of index
v <- NULL
for(i in seq_len(5)) {
	v[i] <- i
	v[i+5] <- i*i 
}

## Problem2: We also have to take care of the case when the same vector name is being repeated at differnet places. For instance,

## Solution: I'll simply initialize the vector with the largest number that we find down the line
z <- NULL
v <- NULL
for(i in seq_len(5)) {
	v[i] <- i
}
z <- v
v <- c()
for(i in seq_len(10)) {
	v[i] <- i*i
}

## Problem3: The case when the vector with the same name is initialized at multiple places
v <- NULL
for(i in 1:5) {
  v[i] <- i
}
v <- NULL
for(j in 1:10) {
  v[j] <- j*j
}

## Another troublesome case is when before a `FOR` loop, the vector is not initialized because it had been initialized earlier. 
## For instance,

v <- NULL
for(i in 1:5) {
  v[i] <- i
}

for(j in 6:10) {
  v[j] <- j*j
}

v

## The cases that I've been checking this far
v <- NULL 
w <- c()
x <- list()  ##To-Do
for (i in 1:5) {
  v[i] <- i
}
for (i in 1:5) { ##To-Do
  w[[i]] <- i
}
for (i in 1:5) { ##To-Do
  x[[i]] <- i
}



## My approach thus far:
## > Since the following case does not throw any errors/warning but still is faster than initializing it as NULL: 
v <- vector(length = 2)
for(i in 1:3) {
  v[i] <- i
}
v

## Therefore, I will attempt to 


## Here we verify that even if we initialize a vector without the exact number of entries, it still is quite faster than just initializing it with NULL
library("microbenchmark")
a <- function() {
  v <- vector(length = 500)
  for(i in 1:100) {
    v[i] <- i
  }
}

b <- function() {
  v <- NULL
  for(i in 1:1000) {
    v[i] <- i
  }
}

microbenchmark(a(), b(), times = 1000L)

## I've avoided the case of list. For example:

v <- list()
for(i in 1:10) {
  v[[i]] <- i*i
}


x <- NULL
i <- 1
while(i < 10) {
  i <- i + 4
  x[i] <- i^2
}


## Test to see just how fast does also writing the type makes it:
library("microbenchmark")
library("ggplot2")
a <- function() {
  v <- vector(mode = "numeric",length = 500)
  for(i in 1:100) {
    v[i] <- i
  }
}

b <- function() {
  v <- vector(length = 500)
  for(i in 1:100) {
    v[i] <- i
  }
}

c <- function() {
  v <- NULL
  for(i in 1:1000) {
    v[i] <- i
  }
}

autoplot(microbenchmark(a(), b(), c(), times = 10000L))

