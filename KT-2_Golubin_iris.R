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
       
#KNN
set.seed(4)#зададим зерно случайных чисел
names(data)#посмотрим переменные
data$Species <- factor(data$Species)# переведем отклик в фактор    

index <- sample(1:nrow(data), round(0.6*nrow(data))) # делим дата-сет на обучающую и тестовую выборки
train <- data[index, ]
test <- data[-index, ]


knn.iris <- knn(train = train[, -5], test = test[, -5], 
                       cl = train[, "Species"], k = 5, prob = TRUE)

table(Факт = test$Species, Прогноз = knn.iris) #построим конфьюжн-матрицу

        
