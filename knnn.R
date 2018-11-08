euclideanDistance <- function(u, v) {                
  sqrt(sum((u - v)^2))
}

sortObjectsByDist <- function(xl, z, metricFunction = euclideanDistance) 
{                                                                        
  l <- dim(xl)[1] 
  n <- dim(xl)[2] - 1 
}


distances <- matrix(NA, l, 2)             # матрица расстояний
for(p in 1:l) {
  distances[p, ] <- c(p, euclideanDistance(xl[p, 1:n], z))
}
orderedxl <- xl[order(distances[ , 2]), ] # сортируем расстояния
return (orderedXl);

kNN <- function(xl, z, k)               
{
  orderedXl <- sortObjectsByDist(xl, z) # Сортируем выборку согласно классифицируемого объекта
  n <- dim(orderedXl)[2] - 1
  classes <- orderedxl[1:k, n + 1]    # Получаем класс первых k соседей
  counts <- table(classes)            # Составляем таблицу встречаемости каждого класса
  classname <- which.max(counts)      # Находим класс, который доминирует среди первых k соседей
  return (class)
}                             


colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue") 
plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col = colors[iris$Species], asp = 1)

z <- c(2.7, 1)                      
xl <- iris[, 3:5]
class <- kNN(xl, z, k=6)
points(z[1], z[2], pch = 22, bg = colors[class], asp = 1)



plot(NULL, NULL, type = "l", xlim = c(0, 150), ylim = c(0, 1), main ="LOO для KNN(k)",xlab = 'k', ylab = 'LOO')
step <- 5
Ox <- seq(from = 1, to = 150, by = step) 
Oy <- c() 

LOOmin <- 1                     
kmin <- 1
for(k in Ox) {
  err <- 0                         
  for(i in 1:l) {
    iris_new <- iris[-i, ]        
    point <- iris[i, 3:4]
    if(knn(iris_new, point, k) != iris[i, 5]) {  
      err <- err + 1                                
    } 
  }
  LOO <- err/l
  Oy <- c(Oy, LOO)
  
  if(LOO < LOOmin) {           
    LOOmin <- LOO
    kmin <- k
  }
}

print(Ox)
print(Oy)
print(kmin)

lines(Ox, Oy, pch = 8, bg = "black", col = "black")
points(kmin, LOOmin, pch = 21, bg = "red", col = "red")
label = paste("   K = ", kmin, "\n", "   LOO = ", LOOmin, sep = "")
