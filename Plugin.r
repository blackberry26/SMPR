estimateMu <- function(objects)
{

  rows <- dim(objects)[1]
  cols <- dim(objects)[2]
  mu <- matrix(NA, 1, cols)
  for (col in 1:cols)
  {
    mu[1, col] = mean(objects[,col])
  }
  return(mu)
}
## Восст-е ковариационной матрицы нормального распред-я

estimateCovarianceMatrix <- function(objects, mu)
{
  rows <- dim(objects)[1]
  cols <- dim(objects)[2]
  sigma <- matrix(0, cols, cols)
  for (i in 1:rows)
  {
    sigma <- sigma + (t(objects[i,] - mu) %*%
                        (objects[i,] - mu)) / (rows - 1)
  }
  return (sigma)
}
## Получ-е коэфф.
getPlugInDiskriminantCoeffs <- function(mu1, sigma1, mu2,
                                        sigma2)
{
  ## Line equation: a*x1^2 + b*x1*x2 + c*x2 + d*x1 + e*x2

  invSigma1 <- solve(sigma1)
 
  invSigma2 <- solve(sigma2)
  f <- log(abs(det(sigma1))) - log(abs(det(sigma2))) +
    mu1 %*% invSigma1 %*% t(mu1) - mu2 %*% invSigma2 %*%
    t(mu2);
  alpha <- invSigma1 - invSigma2
  a <- alpha[1, 1]
  b <- 2 * alpha[1, 2]
  c <- alpha[2, 2]
  beta <- invSigma1 %*% t(mu1) - invSigma2 %*% t(mu2)
  d <- -2 * beta[1, 1]
  e <- -2 * beta[2, 1]
  return (c("x^2" = a, "xy" = b, "y^2" = c, "x" = d, "y"
            = e, "1" = f))
}
## Кол-во объектов в каждом классе
ObjectsCountOfEachClass <- 100
## библиот. для генерации многомерного норм. распред-я

library(MASS)
## Генерируем тестовые данные
Sigma1 <- matrix(c(10, 0, 0, 1), 2, 2)
Sigma2 <- matrix(c(8, 0, 0, 5), 2, 2)
Mu1 <- c(11, 3)
Mu2 <- c(3, 8)
xy1 <- mvrnorm(n=ObjectsCountOfEachClass, Mu1, Sigma1)
xy2 <- mvrnorm(n=ObjectsCountOfEachClass, Mu2, Sigma2)
## Собираем два класса в одну выборку
xl <- rbind(cbind(xy1, 1), cbind(xy2, 2))
## Рисуем обучающую выборку
colors <- c("red","blue","green")
plot(xl[,1], xl[,2], pch = 21, bg = colors[xl[,3]], asp = 1)
## Оценивание
objectsOfFirstClass <- xl[xl[,3] == 1, 1:2]
objectsOfSecondClass <- xl[xl[,3] == 2, 1:2]

mu1 <- estimateMu(objectsOfFirstClass)
mu2 <- estimateMu(objectsOfSecondClass)
sigma1 <- estimateCovarianceMatrix(objectsOfFirstClass,
                                   mu1)
sigma2 <- estimateCovarianceMatrix(objectsOfSecondClass,
                                   mu2)
coeffs <- getPlugInDiskriminantCoeffs(mu1, sigma1, mu2,
                                      sigma2)
## Рисуем дискриминантую функцию –зеленая линия
x <- y <- seq(-10, 20, len=100)
z <- outer(x, y, function(x, y) coeffs["x^2"]*x^2 +
             coeffs["xy"]*x*y
           + coeffs["y^2"]*y^2 + coeffs["x"]*x
           + coeffs["y"]*y + coeffs["1"])
contour(x, y, z, levels=0, drawlabels=FALSE, lwd = 3, col =
          "green", add = TRUE)
