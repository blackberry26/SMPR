euclideanDistance <- function(u, v)
{
return(sqrt(sum((u - v)^2)))
}
coreEP = function(r){
  return ((3/4*(1-r^2)*(abs(r)<=1)))
}
coreR = function(r){
  return ((0.5 * (abs(r) <= 1) ))
} 
coreT = function(r){
  return ((1 - abs(r)) * (abs(r) <= 1)) 
}
coreQ = function(r){
  return ((15 / 16) * (1 - r ^ 2) ^ 2 * (abs(r) <= 1)) 
}
coreG = function(r){
  return (((2*pi)^(-1/2)) * exp(-1/2*r^2))
}



PW= function(XL,y,h,metricFunction = euclideanDistance) # Оценка весовой функции по расстояниюу 
{
  n = dim(xl)[1]
  weights = rep(0,3)
  names(weights) = c("setosa", "versicolor", "virginica")
  for(i in 1:n) # Для всех объектов выборки
  {
    
    x=XL[i,1:2]
    class=XL[i,3] # Берём его класс
    
    r = metricFunction(x,y)/h
    weights[class]=coreG(r)+weights[class]; # И прибавляем его вес к общему весу его класса
  }
  
  class = names(which.max(weights))
  
  if(max(weights)==0){ # Если веса точки по классам равны 0 (точка не попала в окно)
    return (0) #вернуть 0
  }
  else{ # иначе
    return (class) #  Вернуть класс с максимальным весом
  }
}

plotWindows = function(h){
  for(i in seq(0, 7, 0.1)){
    for(j in seq(0,3,0.1)){
      z = c(i, j)
      class = PW(xl,z,h)
      if(class!=0){
        points(z[1], z[2], pch = 1,col=colors[class])
      }
    }
  }
}


LOO = function(xl,class) {
  n = dim(xl)[1]
  loo = rep(0, 20)
  
  for(i in 1:(n)){
    u=xl[i, 1:2]
    v=xl[-i,1:3]
    
    for(h in 1:20){
      H = h/10;
      test=PW(v,u,H)
      
      if(colors[test] != colors[class[i]]){
        loo[h] = loo[h]+1;
     }    
    } 
  }
  
  loo = loo / n
  x = seq(0.1,2,0.1)
  plot(x, loo,main ="LOO для PW(H)", xlab="h", ylab="LOO", type = "l")
  
  min=which.min(loo)
  lOOmin=round(loo[min],3)
  minX=min/10
  
  points(minX, loo[min], pch = 21, col = "red",bg = "red"
  label = paste("   H = ", minX, "\n", "   LOO = ", lOOmin, sep = "")
  text(minX, lOOmin, labels = label, pos=4, col = "red")
  
  
  text = paste("Кaрта классификации для PW(GS)")
  plot(iris[, 3:4],main=text, pch = 21, bg = colors[xl$Species], col = colors[xl$Species],asp='1')
  plotWindows(minX)
}



par(mfrow = c(1, 2))
colors = c("setosa" = "red", "versicolor" = "green", "virginica" = "blue", "No" = "No")
xl = iris[, 3:5] 
class = iris[, 5]
LOO(xl,class)
