bubblesort <- function(x){
  n <- length(x)
  
  for(j in 1:(n - 1)){
    for(i in 1:(n - j)){
      if(x[i] > x[i + 1]){
        temp <- x[i]
        x[i] <- x[i + 1]
        x[i + 1] <- temp
      }
    }
  }
  
  return(x)
}

selectionsort <- function(vec) {
  n <- length(vec)
  if (n <= 1) return(vec)
  
  for (i in n:1){
    min_el_index <- i 
    for (j in 1:i){
      if (vec[min_el_index] < vec[j]){
        min_el_index <- j
      }
    }
    tmp <- vec[min_el_index]
    vec[min_el_index] <- vec[i]
    vec[i] <- tmp
  }
  
  return(vec)
}

insertsort <- function(vec) {
  n <- length(vec)
  if (n <= 1) return(vec)
  
  for (i in 2:n) {
    value <- vec[i]
    j <- i
    while (j > 1 && vec[j - 1] > value) {
      vec[j] <- vec[j - 1]
      j = j - 1
    }
    vec[j] <- value
  }
  return(vec)
}

insertsort <- function(arr) {
  n <- length(arr)
  if (n <= 1) return(arr)  
  
  for (i in 2:n){
    key <- arr[i]  
    j <- i - 1
    while (j >= 1 && key < arr[j]){
      arr[j+1] <- arr[j]
      j <- j - 1
    }
      
    arr[j+1] <- key 
  }
  return(arr)
}


merge <- function(a, b) {
  N <- length(a)
  M <- length(b)
  sortedlist <- c()
  i <- 1
  j <- 1
  
  while (i <= N && j <= M) {
    if (a[i] <= b[j]){
      sortedlist <- c(sortedlist, a[i])
      i <- i + 1
    }
    else if (a[i] > b[j]) {
      sortedlist <- c(sortedlist, b[j])
      j <- j + 1
    }
  }
  if (i == N + 1) sortedlist <- c(sortedlist, b[j:length(b)])
  else sortedlist = c(sortedlist, a[i:length(a)])
  
  return(sortedlist)
}

mergesort <- function(a, n = NULL) {
  if (is.null(n)) n <- length(a)
  if (n <= 1) return(vec)
  
  n1 <- n %/% 2
  
  a1 <- a[(n1 + 1):length(a)]
  a2 <- a[1:n1]
  
  if (length(a1) > 1) a1 <- mergesort(a1, length(a1))
  if (length(a2) > 1) a2 <- mergesort(a2, length(a2))
  
  return(merge(a1, a2))
}