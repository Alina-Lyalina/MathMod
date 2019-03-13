#Задание 1. Вычислить среднее для каждой колонки таблицы iris, за исключением колонки “Species” и соберите результат в список (list). 
data.frame(iris) 
mean(iris$Sepal.Length) 
mean(iris$Sepal.Width) 
mean(iris$Petal.Length) 
mean(iris$Petal.Width) 
list(mean(iris$Sepal.Length),mean(iris$Sepal.Width),mean(iris$Petal.Length),mean(iris$Petal.Width)) 
