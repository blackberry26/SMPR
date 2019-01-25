naiv <- function(xi, mu, sigma, lambda, P){
  n <- 2
  res <- log(lambda*P)
  for(i in 1 : n){
    pyj <- getPyj(sigma[i], xi[i], mu[i])
    res <- res + log(pyj)
  }
  return(res)
}

getPyj <- function(disp, xi, mu){
  return((1/(disp*sqrt(2*pi))) * exp(-1 * ((xi - mu)^2)/(2*disp^2)) )
}

mu_with_hat <- function(xl){
  l <- dim(xl)[1] 
  return(c(sum(xl[,1])/l, sum(xl[,2])/l))
}

sigma_with_hat <- function(xl, mu){
  l <- dim(xl)[1] 
  
  return(c(sum((xl[,1] - mu[1])^2)/l, sum((xl[,2] - mu[2])^2)/l))
}

quality <- function(xl, muh1, sigma1, muh2, sigma2){
  l <- dim(xl)[1] 
  mis <- 0
  for(i in 1 : l){      
    class <- 0;
    if(naiv(xl[i, -3], muh1, sigma1, 1, 0.5) > naiv(xl[i, -3], muh2, sigma2, 1, 0.5)){
      class <- 1
    } else {
      class <- 2
    }     
    if(class != xl[i, 3])
      err <- err + 1
  }
  return (err/l)
}

main <- function(objectCounter = 500){
  sigma1 <- matrix(c(2, 0, 0, 2),2,2)
  sigma2 <- matrix(c(1, 0, 0, 1),2,2)
  mu1 <- c(0,0)
  mu2 <- c(4,4)
  x1 <- mvrnorm(n = objectCounter, mu1, sigma1)
  x2 <- mvrnorm(n = objectCounter, mu2, sigma2)
  
  xy1 <- cbind(x1,1) 
  xy2 <- cbind(x2,2) 
  
  xl <- rbind(xy1,xy2)
  
  colors <- c("violet", "yellow")
  plot(xl[,1],xl[,2], pch = 21,main = "Наивный байесовский классификатор", xlab = 'Признак 1', ylab= 'Признак 2', col = colors[xl[,3]], asp = 1, bg=colors[xl[,3]])
  
  colors <- c("green", "red")
  muh1 <- mu_with_hat(x1)
  muh2 <- mu_with_hat(x2)         
  sigma1 <- get_sigma_with_hat(x1, muh1)
  sigma2 <- get_sigma_with_hat(x2, muh2)
  
  x1 <- -20;
  while(x1 < 30){
    x2 <- -10;
    while(x2 < 20){          
      class <- 0;
      if(naiv(c(x1,x2), muh1, sigma1, 1, 0.5) > naiv(c(x1,x2), muh2, sigma2, 1, 0.5)){
        class <- 1
      } else {
        class <- 2
      }
      #points(x1, x2, pch = 21, col=colors[class], asp = 1)
      x2 <- x2 + 0.1
    }
    x1 <- x1 + 0.1
  }
  print(quality(xl, muh1, sigma1, muh2, sigma2))
}
main()
