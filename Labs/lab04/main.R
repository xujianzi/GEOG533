### Author: Dr. Qiusheng Wu

### Question 1: The number of points scored by each team in a tournament is normally distributed, with mean μ = 32 and standard deviation σ = 7

## Q1a: The probability of a team scoring 11 points or fewer?
?dnorm
pnorm(20,mean = 32,sd = 7) 
curve(dnorm(x,32,7),from = 10,to = 54)
cord.x <- c(10,seq(10,20,0.01),20)
cord.y <- c(0,dnorm(seq(10,20,0.01),mean = 32,sd = 7),0)
polygon(cord.x,cord.y,density = 10)

## Q1b: The probability of a team scoring more than 35 points?
pnorm(35,mean = 32,sd = 7,lower.tail = FALSE)
curve(dnorm(x,32,7),from = 10,to = 54)
cord.x <- c(35,seq(35,54,0.01),54)
cord.y <- c(0,dnorm(seq(35,54,0.01),mean = 32,sd = 7),0)
polygon(cord.x,cord.y,density = 10)


## Q1c: The probability of a team scoring between 20 and 40 points?
pnorm(40,mean = 32,sd = 7) - pnorm(20,mean = 32,sd = 7)
diff(pnorm(c(20,40),mean = 32,sd = 7))
curve(dnorm(x,32,7),from = 10,to = 54)
cord.x <- c(20,seq(20,40,0.01),40)
cord.y <- c(0,dnorm(seq(20,40,0.01),mean = 32,sd = 7),0)
polygon(cord.x,cord.y,density = 10)


### Question 2: The number of comments per post on a social media site is exponentially distributed, with the average post receiving ten comments. What percentage of posts get fewer than three comments? More than 50? Between five and ten?
## Q2a: What percentage of posts get fewer than three comments?
?pexp
pexp(3,rate = 1/10)
curve(dexp(x,rate = 1/10),from = 0,to = 100)
cord.x <- c(0,seq(0,3,0.01),3)
cord.y <- c(0,dexp(seq(0,3,0.01),rate = 1/10),0)
polygon(cord.x,cord.y,density = 10)

## Q2b: What percentage of posts get more than 20 comments?
pexp(20,rate = 1/10,lower.tail = FALSE)
curve(dexp(x,rate = 1/10),from = 0,to = 100)
cord.x <- c(20,seq(20,100,0.01),100)
cord.y <- c(0,dexp(seq(20,100,0.01),rate = 1/10),0)
polygon(cord.x,cord.y,density = 10)

## Q2c: What percentage of posts get between five and ten comments?
diff(pexp(c(5,10),rate = 1/10))
curve(dexp(x,rate = 1/10),from = 0,to = 100)
cord.x <- c(5,seq(5,10,0.01),10)
cord.y <- c(0,dexp(seq(5,10,0.01),rate = 1/10),0)
polygon(cord.x,cord.y,density = 10)

### Q3a 
library(raster)
ras1 <- raster(nrows=30,ncols=30,xmn=0,xmx=30,ymn=0,ymx=30)
ras1[] = runif(ncell(ras1))
plot(ras1)
hist(ras1)

### Q3b 
ras2 <- raster(nrows=30,ncols=30,xmn=0,xmx=30,ymn=0,ymx=30)
ras2[] = rnorm(ncell(ras2))
plot(ras2)
hist(ras2)

### Q3c
ras3 <- ras1 + ras2
plot(ras3)
hist(ras3)

### Q3d
mean(ras3)
m <- cellStats(ras3,mean)
ras3[ras3 < m] <- 0
ras3[ras3 >= m] <- 1
plot(ras3)
hist(ras3)
writeRaster(ras3,filename = "test.tif")
r <- raster("test.tif")
plot(r)
hist(r)
