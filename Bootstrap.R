install.packages("boot")
library(boot)

set.seed(63334)
data <- c(373.35,455.39,622.72,626.89,390.65,770.73,919.38,579.43,2687.64,915.2,924.66,283.48,449.91,347.31,2120.06,437.38,414.77,1000.52,440.86,1836.86,895.74,1152.37,814.54,1137.79,2460.13,1538.32,1276.87,1125.86,686.13,801.46,994.04,1251.83)
data

plot(data)
hist(data)

# Distribución bootstrap

curve(ecdf(data)(x), ylab = "FD", type = "s", lwd = 2)

# Distribución teórica
abline(a = 0, b = 1, lty = 2)

# Muestra bootstrap
xboot <- sample(data, replace=TRUE)

# dat <- dat.sim[, 1]
ndat <- 100
datmed <- mean(data)
datsd <- sd(data)
dat <- rnorm(ndat, mean=datmed, sd=datsd)

#Consideramos 1000 réplicas bootstrap:

nboot <- 1000

#El valor del estadístico en la muestra es:

stat.dat <- mean(dat)
stat.dat

#Generamos las réplicas bootstrap:
stat.boot <- numeric(nboot)
for (i in 1:nboot) {
  dat.boot <- sample(dat, replace=TRUE)
  stat.boot[i] <- mean(dat.boot)
}
# Valor esperado bootstrap del estadístico
mean.boot <- mean(stat.boot)  
mean.boot

#Bootstrap percentil:

hist(stat.boot, freq=FALSE, ylim = c(0,.01))
abline(v=mean.boot, lwd=2)
# abline(v=stat.dat)

# Distribución poblacional
curve(dnorm(x, datmed, datsd/sqrt(ndat)), lty=2, add=TRUE)
abline(v=datmed, lwd=2, lty=2)

#Sesgo y error estándar bootstrap:
mean.boot - stat.dat 

# error estándar

estdev <- sd(stat.boot)

#Generación de las muestras
stat.boot <- boot(dat, boot.f, nboot)
stat.boot

boot(data = dat, statistic = boot.f, R = nboot)

hist(stat.boot, freq=FALSE)
plot(stat.boot)
jack.after.boot(stat.boot)

# Intervalos de confianza bootstrap

boot.ci(stat.boot, type=c("norm", "basic", "perc"))









###OTRO MÉTODO
#Bootstrap natural/básico:

hist(stat.boot-stat.dat, freq=FALSE, ylim = c(0,0.01))
abline(v=mean.boot-stat.dat, lwd=2)

# Distribución poblacional
# Distribución teórica de stat.dat - stat.teor
curve(dnorm(x, 0, datsd/sqrt(ndat)), lty=2, add=TRUE)   
abline(v=0, lwd=2, lty=2)

#Versión "optimizada" para R:
boot.strap <- function(dat, nboot=1000, statistic=mean)
{
  ndat <- length(dat)
  dat.boot <- sample(dat, ndat*nboot, replace=T)
  dat.boot <- matrix(dat.boot, ncol=nboot, nrow=ndat)
  stat.boot <- apply(dat.boot, 2, statistic)
}

fstatistic <- function(dat){
  #  mean(dat)
  mean(dat, trim=0.2)
  #  median(dat)
  #  max(dat)
}

stat.dat <- fstatistic(dat)
stat.boot <- boot.strap(dat, nboot, fstatistic)

res.boot <- c(stat.dat, mean(stat.boot)-stat.dat, sd(stat.boot))
names(res.boot) <- c("Estadístico", "Sesgo", "Err.Std")
res.boot

boot.f <- function(data, indices){
  # data[indices] será la muestra bootstrap
  mean(data[indices])
}