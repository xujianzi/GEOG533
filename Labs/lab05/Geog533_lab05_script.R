### Author: Dr. Qiusheng Wu

### Ex1
q95 <- qnorm(0.975)
q95
# std.err = q95 * std /(sqrt(n)) = 0.2
n <- (q95 * 2 / 0.2)^2
n  # n>385
conf.95 <- q95 * (2 / sqrt(385))
conf.95 



### Ex3
## solution 1
n <- 50
m <- 18.5
std <- 7
q95 <- qnorm(0.975)
std.err <- std / sqrt(n)
conf.95 <- c(m - std.err,m + std.err)
conf.95
x <- 16
print(x >= conf.95[1] & x <= conf.95[2])

## solution 2
library(MASS)
x <- mvrnorm(n = 50,mu = 18.5,Sigma = 7*7,empirical = TRUE)
t.test(x,mu = 16)
# 95% confidence interval [16.51,, 20.49]
# p-value = 0.01484
# reject the null hypothesis, tolerlable level is not within the interval


### Ex5
prop.test(x=50*0.24,n = 50,p = 0.165,conf.level = 0.9)
# 90% confidence interval [0.148 0.362]
# p-value = 0.2156
# p-value >0.05, cannot reject the null hypothesis. No difference from the national average


### Ex7
## Ex7 a-d
library(MASS)
x1 <- mvrnorm(n = 20,mu = 4.1,Sigma = 14.3,empirical = TRUE)
x2 <- mvrnorm(n = 16,mu = 3.1,Sigma = 12.0,empirical = TRUE)
t.test(x1,x2,var.equal = TRUE)
t.test(x1,x2,var.equal = FALSE)

## https://www.youtube.com/watch?v=7GXnzQ2CX58 

t.var.equal <- t.test(x1,x2,var.equal = TRUE)
print(t.var.equal$p.value)
print(t.var.equal$conf.int)
t.var.unequal <- t.test(x1,x2,var.equal = FALSE)
print(t.var.unequal$p.value)
print(t.var.unequal$conf.int)

## Ex7 e

x1 <- mvrnorm(n = 24,mu = 4.1,Sigma = 14.3,empirical = TRUE)
x2 <- mvrnorm(n = 12,mu = 3.1,Sigma = 12.0,empirical = TRUE)
t.var.equal <- t.test(x1,x2,var.equal = TRUE)
print(t.var.equal$p.value)
print(t.var.equal$conf.int)
t.var.unequal <- t.test(x1,x2,var.equal = FALSE)
print(t.var.unequal$p.value)
print(t.var.unequal$conf.int)


### Ex9
library(MASS)
x <- mvrnorm(n = 17,mu = 6.4,Sigma = 4.4*4.4)
t.test(x,mu = 4.2)
# 95% confidence interval [5.56 10.69]
# p-value = 0.00504
# p-value < 0.05, reject the null hypothesis. We can conclude that the pollutant level exceeds the allowable limit


### Ex13
A <- mvrnorm(n = 52,mu = 3.4,Sigma = 1.1*1.1)
B <- mvrnorm(n = 62,mu = 2.8,Sigma = 0.8*0.8)
t.test(A,B,var.equal = FALSE)
# null hypothesis: the mean length of unemployment is equal
# p-value = 0.0384
# p-value < 0.05, reject the null hypothesis. the mean length of unemployment is not equal


### Ex15
x <- c(100,426,322,466,112,155,388,1155,234,324,556,221,18,133,177,441)
t.test(x,conf.level = 0.9)
# 90% confidence interval [208.8 444.7]
t.test(x,conf.level = 0.95)
# 95% confidence interval [183.3 470.1]


### Ex16
prop.test(x=c(50*0.3,40*0.22),n = c(50,40))
prop.test(x=c(50*0.3,40*0.22),n = c(50,40),conf.level = 0.9)


### Ex17
x1 <- mvrnorm(n = 15,mu = 12.4,Sigma = 3*3,empirical = TRUE)
x2 <- mvrnorm(n = 15,mu = 14.4,Sigma = 4*4,empirical = TRUE)
t.test(x1,x2,var.equal = TRUE)
t.test(x1,x2,var.equal = FALSE)


### Q20
prop.test(x = 50*0.15,n = 50,p = 0.1)
