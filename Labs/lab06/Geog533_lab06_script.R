### author: Dr. Qiusheng Wu

### Exercises
## EX2
n <- 50
k <- 5
WSS <- 2000
m.sq1 <- 116.3

df1 <- k - 1 
df2 <- n - k

BSS <- df1 * m.sq1
m.sq2 <- WSS/df2
TSS <- WSS + BSS

F <- m.sq1/m.sq2   ## 2.61675
F.critical <- qf(0.95,df1,df2)   ## 2.5787

## F > F.critical, we reject the null hypothesis that all means are equal


## Ex6

L <- c(5,7,9,11,13,8,10,34,17,50,17,25)
H <- c(25,24,8,2,11,10,10,66,113,1,3,5)
m1 <- mean(L)
m2 <- mean(H)
s1 <- var(L)
s2 <- var(H)

income <- c(L,H)
group <- c(rep("L",12),rep("H",12))
df <- data.frame(income,group)

F <- s1/s2  #0.157
F.ratio <- df(0.9,df1 = 11,df2 = 11)  #0.708

library(car)
leveneTest(income~group,data = df)

m <- aov(income~group, data = df)
m
summary(m)

kruskal.test(L,H)


### Ex9
library(MASS)
A <- mvrnorm(12,mu = 43.2,Sigma = 36.2^2,empirical = TRUE)
B <- mvrnorm(10,mu = 34.3,Sigma = 20.3^2,empirical = TRUE)
C <- mvrnorm(8,mu = 27.2,Sigma = 21.4^2,empirical = TRUE)
k <- 3
n <- 30

values <- c(A,B,C)
groups <- c(rep("A",12),rep("B",10),rep("C",8))
df <- data.frame(values,groups)
m <- aov(values~groups,data = df)
m
summary(m)
TukeyHSD(m)
plot(TukeyHSD(m))
## conclusion: cannot reject the null hypothesis that the means are equal

### Ex 10
c1 <- c(23.1,13.3,15.6,1.2)
c2 <- c(43.1,10.2,16.2,0.2)
c3 <- c(56.5,32.1,43.3,24.4)
c4 <- c(10002.3,54.4,8.7,54.4)

values <- c(c1,c2,c3,c4)
groups <- c(rep("c1",4),rep("c2",4),rep("c3",4),rep("c4",4))

df <- data.frame(values,groups)

kruskal.test(values~groups,data = df)

## p-value = 0.094 > 0.05
## conclusion: cannot reject the null hypothesis that the means are equal


### Ex 12
L <- c(5,4,1,2,3,10,6,6,4,12,11)
M <- c(10,10,8,6,5,3,16,20,7,3,2)
H <- c(8,11,15,19,21,7,7,4,3,17,18)

values <- c(L,M,H)
groups <- c(rep("L",11),rep("M",11),rep("H",11))
df <- data.frame(values,groups)

m <- aov(values~groups,data = df)
m
summary(m)
TukeyHSD(m)
plot(TukeyHSD(m))
## p-value = 0.043
## conclusion: reject the null hypothesis.

library(car)
leveneTest(values~groups,data = df)
## p-value = 0.131
## the assumption of homoscedasticity is satisfied. 

hist(values)
shapiro.test(values)
## p-value = 0.008 < 0.05
## the assumption of normality not satisfied


### Ex 13

city <- mvrnorm(10,mu = 1.5,Sigma = 1,empirical = TRUE)
suburb <- mvrnorm(15,mu = 2.6,Sigma = 1.1^2,empirical = TRUE)
rural <- mvrnorm(15,mu = 1.2,Sigma = 1.2^2,empirical = TRUE)

values <- c(city,suburb,rural)
groups <- c(rep("city",10),rep("suburb",15),rep("rural",15))
df <- data.frame(values,groups)

m <- aov(values~groups,data = df)
m
summary(m)
TukeyHSD(m)
plot(TukeyHSD(m))
### p-value = 0.004 < 0.05
### conclusion: reject the null hypothesis
## the suburbs-rural group are significantly different