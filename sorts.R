bubblesort = function(vec) {
  n <- length(vec)
  if (n <= 1) return(vec)
  
  for (i in 1:n) {
    for (j in i:n) {
      if ((vec[i] > vec[j])) {
        tmp <- vec[i]
        vec[i] <- vec[j]
        vec[j] <- tmp      
      }
    }
  }
  
  return(vec)
}

selectionsort <- function(vec) {
  n <- length(vec)
  if (n <= 1) return(vec)
  
  for (i in n:1){
    max_el_index <- i 
    for (j in 1:i){
      if (vec[max_el_index] < vec[j]){
        max_el_index <- j
      }
    }
    tmp <- vec[max_el_index]
    vec[max_el_index] <- vec[i]
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