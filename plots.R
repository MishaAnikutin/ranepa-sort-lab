source("utils.R")
source("sorts.R")


generate_data <- function(sorting_algorithm, x) {
  #' Генерация данных времени исполнения
  #' 
  #' @param sort_algorithm: Function - алгоритм сортировки
  #' @param x: Numeric - список размеров случайных списков, по которому будем делать замеры
  #' @return time: Numeric - список времени выполнения функции
  
  time <- c()
  for (n in x) {
    sa_timer <- timer(sorting_algorithm)
    time <- c(time, sa_timer(get_random_list(n)))
  }
  return(time)
}


build_time_plot <- function(xmin = 250, xmax = 3000, step = 250) {
  x <- seq(xmin, xmax, by = step)
  
  print("генерируем данные для bubblesort")
  y_bubblesort <- generate_data(bubblesort, x)
  
  print("генерируем данные для insertsort")
  y_insertsort <- generate_data(insertsort, x)
  
  print("генерируем данные для selectionsort")
  y_selectionsort <- generate_data(selectionsort, x)
  
  print("генерируем данные для mergesort")
  y_mergesort <- generate_data(mergesort, x)
  
  print("генерируем данные для встроенной сортировки")
  y_built_in <- generate_data(sort, x)
  
  plot(
    x, y_bubblesort,
    type = "l", 
    col = "red",
    xlab = "Количество элементов в списке",
    ylab = "Время сортировки",
  )
  
  lines(x, y_insertsort, type = "l", col = "blue")
  lines(x, y_selectionsort, type = "l", col = "green")
  lines(x, y_mergesort, type = "l", col = "orange")
  lines(x, y_built_in, type='l', col='black')
  
  nsquare <- (x)^2 / (5 * 10^7)
  nlogn <- x * log2(x) / (7 * 10^5)
  
  lines(x, nsquare, col = 'grey', lty = 2, type = "l")
  lines(x, nlogn, col = 'grey', lty = 2)
  
  title(main = "Время выполнения сортировок")
  
  legend(
    "topleft",
    legend = c(
      "Bubblesort",
      "Insert sort",
      "Selection sort",
      "Merge sort",
      "built-in sort",
      "y = O(n^2)",
      "y = O(nlog(n))"
      ), 
    col = c("red", "blue", "green", "orange", "black", "grey", "grey"), 
    lty = 1
  )
}
