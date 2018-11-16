V <- round(runif(20) * 10)

#1. Insertion_sort

insertion_sort <- function(Vector, n){
  if(n == 0) stop("No element to sort")
  for(i in 2:length(Vector)){
    key <- Vector[i]
    j <- i - 1
    while(j >= 1 && key <= Vector[j]){
      Vector[j+1] <- Vector[j]
      j <- j - 1
    }
    Vector[j+1] <- key
  }
  return(Vector)
}

insertion_sort(V, 1)

#2. Bubble_sort
Buble_sort <- function(Vector, n){
  if(n == 0) stop("No element to sort")
  for(i in 1:length(Vector)){
    flag <- 0
    for(j in 1:(length(Vector) -1)){
      if(Vector[j] > Vector[j+1]){
        val <- Vector[j]
        Vector[j] <- Vector[j+1]
        Vector[j+1] <- val
        flag <- 1
      }
    }
    if(!flag) break
  }
  return(Vector)
}

Bubble_sort(V, 1)

#3. selection_sort
Selection_sort <- function(Vector, n){
  if(n == 0) stop("No element to sort")
  for(i in seq_along(Vector)){
    small_pos <- (i - 1) + which.min(Vector[i:length(Vector)])
    temp <- Vector[i]
    Vector[i] <- Vector[small_pos]
    Vector[small_pos] <- temp
  }
  return(Vector)
}

Selection_sort(V, 1)


#4. merge_sort
Merge_Sort <- function(V) {
  if(length(V) == 0) stop("Not enough elements to sort")
  
  ## Merge function to sort two halves or sub-vectors
  merge_fn <- function(first_half, second_half) {
    result <- c()
    while(length(first_half) > 0 && length(second_half) > 0) {
      if(first_half[1] <= second_half[1]) {
        result <- c(result, first_half[1])
        first_half <- first_half[-1]
      } else {
        result <- c(result, second_half[1])
        second_half <- second_half[-1]
      }         
    }
    if(length(first_half) > 0) result <- c(result, first_half)
    if(length(second_half) > 0) result <- c(result, second_half)
    return(result)
  }
  
  if(length(V) <= 1) V else {
    middle <- length(V) / 2
    first_half <- V[1:floor(middle)]
    second_half <- V[floor(middle+1):length(V)]
    first_half <- Merge_Sort(first_half)
    second_half <- Merge_Sort(second_half)
    if(first_half[length(first_half)] <= second_half[1]) {
      c(first_half, second_half)
    } else {
      merge_fn(first_half, second_half)
    } 
  }
}

Merge_Sort(V)
