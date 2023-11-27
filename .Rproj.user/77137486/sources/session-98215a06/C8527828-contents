#Тестирование функции сортировок
source("sorts.R")
source("utils.R")


test_sort <- function(sort_function) {
  #' Тестирует функцию сортировки 
  #' 
  #' На пустом списке, на списке из 1 элемента, на отсортированном списке
  #' на отсортированном в обратном порядке и случайных списках
  #' 
  #' @param sort_function: Function - функция сортировки 
  #' @return c(_, exc): List[bool, Exception] - прошла ли тест и исключение
  
  exc <- NULL
  
  #if (!identical(sort_function(c()), c())) exc <- "Пустой список не отсортировался\n"
  #if (!identical(sort_function(c(1)), c(1))) exc <- "Список длины 1 не отсортировался\n"
  
  random_list <- get_random_list()
  sorted_random_list <- sort(random_list)
  
  if (!identical(sort_function(sorted_random_list), sorted_random_list)) exc <- "\tотсортированный список не отсортировался\n"
  if (!identical(sort_function(sorted_random_list[length(random_list):1]), sorted_random_list)) exc <- "\tотсортированный в обратном порядке список не отсортировался\n"
  if (!identical(sort_function(random_list), sort(random_list))) exc <- "\tРандомный список не отсортировался"
  
  if (is.null(exc)) return(c(TRUE, exc))
  return(c(FALSE, exc))
}

test <- function() {
  #' Тестирование всех функций сортировок 
  #' 
  #' @return _: Union[bool, Exception] - все ли тесты прошли, иначе исключение
  
  exc <- NULL 
  
  print("Тестируем Bubblesort")
  data <- test_sort(bubblesort)
  if (!is.na(data[2])) exc <- c("\tbubblesort не прошла тест\n", data[2])
  
  print("Тестируем Insertsort")
  data <- test_sort(insertsort)
  if (!is.na(data[2])) exc <- c("\tinsertsort не прошла тест\n", data[2])
  
  print("Тестируем Selectionsort")
  data <- test_sort(selectionsort)
  if (!is.na(data[2])) exc <- c("\tselectionsort не прошла тест\n", data[2])
  
  print("Тестируем mergesort")
  data <- test_sort(mergesort)
  if (!is.na(data[2])) exc <- c("\tmergesort не прошла тест\n", data[2])
  
  return(c(is.null(exc), exc))
}

test()
