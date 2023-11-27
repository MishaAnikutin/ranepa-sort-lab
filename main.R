source("sorts.R")
source("unit_tests.R")
source("plots.R")


main <- function() {
  data <- test()
  
  if (is.na(data[2])) print("Все тесты прошли успешно!")
  
  else {
    print("Тестирование прервалось с ошибкой:", data[2])
    return()
  }
  
  print("Строим график")
  build_time_plot(xmin = 250, xmax = 10000, step = 1000)
  print("График построен")
}

main()
