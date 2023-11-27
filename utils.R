get_random_list <- function(n = NULL) {
  #' Возвращает список случайных чисел от 1 до 100
  #' 
  #' @param n: int - размер списка
  #' @return _: numeric - случайный список
  
  if (is.null(n)) n = 10
  return(sample(1:100, n, replace = TRUE))
}


timer <- function(f) {
  #' Декоратор для замера времени работы функции
  #' 
  #' @param f: Function - функция, время которой замеряем
  #' @return _: Numeric - время выполнения функции
  
  wrapper <- function(...) {
    op <- options(digits.secs = 15) 
    start <- proc.time()
    res <- f(...)
    end <- proc.time()
    return((end - start)["elapsed"])
  }
  return(wrapper)
}
