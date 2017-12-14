### Author: Dr. Qiusheng Wu

### Q1
## Q1a

income <- c(30,28,52,40,35)
edu <- c(12,13,18,16,17)
cor(x = edu,y = income,method = "pearson")
# r = 0.83577

## Q1b
cor.test(x = edu,y = income,method = "pearson")
# p-value = 0.0779, we fail to reject the null hypothesis

## Q1c
cor(x = edu,y = income,method = "spearman")
# r = 0.8

## Q1d
cor.test(x = edu,y = income,method = "spearman")
# p-value = 0.1333, we fail to reject the null hypothesis. The correlation coefficient is not significantly different than zero.


### Q3

n <- 36
r <- 2/sqrt(n)
# r <- 0.3333

t.critical <- qt(0.975,df = n - 2)  ## = 2.0322
t <- r*sqrt(n-2)/sqrt(1-r^2)  ## = 2.0615

n <- 80
r <- 2/sqrt(n)
# r <- 0.2236

t.critical <- qt(0.975,df = n - 2)  ## = 1.9908
t <- r*sqrt(n-2)/sqrt(1-r^2)  ## = 2.0261


### Q4
X <- c(2,8,9,7)
Y <- c(6,6,10,4)
cor(X,Y,method = "pearson")
cor.test(X,Y,method = "pearson")
# r = 0.3834129, however, it is not significant.


### Q6
income <- c(35165,35778,37027,37256,37512,37997,37343,36054,35593,35241,35486)
races <- c(399,469,429,450,474,598,364,430,433,410,317)

cor(x = races,y = income)
# r = 0.5583199
r.min <- 2/sqrt(11)
# r.min = 0.603

cor.test(x = races,y = income)
# p-value = 0.07425, we fail to reject the null hypothesis. 


### Q7
x <- c(1,2,5,6,11,12)
y <- c(8,4,12,3,10,7)

cor(x,y,method = "spearman")
# r = -0.02857143
cor.test(x,y,method = "spearman",conf.level = 0.9)
# p-value = 1. No corrlelation


### Q8
x <- c(3.2,2.4,1.6,8.3,7.2,5.1)
y <- c(6.2,7.3,8.1,2.6,6.3,4.3)

cor(x,y)
# r = -0.8073491
cor.test(x,y,conf.level = 0.95)
# p-value = 0.0521. We fail to reject the null hypothesis


### Q9
df <- read.csv("Milwaukee_Sales.csv",header = TRUE)
str(df)

cor(x = df$Bedrms,y = df$LotSize,method = "spearman")
# r = -0.06929767
cor.test(x = df$Bedrms,y = df$LotSize)
# p-value = 0.8567


### Q10
df <- read.csv("UK_Housing.csv",header = TRUE)
str(df)

cor(df$bedrooms,df$floorarea,method = "spearman")
# r = 0.6004428
cor.test(df$bedrooms,df$floorarea)
# p-value < 2.2e-16


## 
df <- cars
plot(cars)

n <- nrow(df)

df$speed.z <- scale(df$speed)
df$dist.z <- scale(df$dist)
df$product <- df$speed.z * df$dist.z
r <- sum(df$product) / (n-1)  # r = 0.8068949
t <- r*sqrt(n-2)/sqrt(1-r^2)  # t = 9.464
qt(0.95,df = n-2)


cor(df$speed,df$dist,method = "pearson")   # r = 0.8068949
cor.test(df$speed,df$dist)                 # t = 9.464


df$speed.r <- rank(df$speed,ties.method = "average")
df$dist.r <- rank(df$dist,ties.method = "average")

df$d <- (df$speed.r - df$dist.r)^2
r <- 1 - (6*sum(df$d)/(n^3-n))   # r = 0.8308282

cor(df$speed,df$dist,method = "spearman")  # r = 0.8303568
