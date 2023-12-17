source("sorts.R")
source("unit_tests.R")
source("plots.R")


main <- function() {
  data <- test()
  print(data)
  if (is.na(data[2])) print("Все тесты прошли успешно!")
  
  else {
    print("Тестирование прервалось с ошибкой:", data[2])
    return()
  }
  
  print("Строим график")
  build_time_plot(xmin = 100, xmax = 5000, step = 500)
  print("График построен")
}

main()

