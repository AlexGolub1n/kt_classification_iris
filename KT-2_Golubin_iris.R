#подключаем библиотеку
library(ggplot2)
library(gridExtra)
library(e1071)

#загружаем данные
data <- iris

#построим графики
a <- qplot(Petal.Length, Petal.Width,data = iris) +
  facet_grid(facets = ~ Species) +
  geom_smooth(color = "red", se = FALSE)

b <- qplot(Sepal.Length, Sepal.Width, data = iris) +
  facet_grid(facets = ~ Species) +
  geom_smooth(color = "red", se = FALSE)
grid.arrange(a, b, nrow = 2)

#создаем модель
model <- svm (Species ~., data = data)

print (model)
summary (model) 

#Создадим данные
x <- seq (0.1,1, by = 0.05) 
y <- log (x) + rnorm (x, sd = 0,2) 

#оценим модель
m <- svm (x, y) 
new <- predict(m, x) 
View(new)
        
        