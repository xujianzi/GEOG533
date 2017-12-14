
df <- read.csv("http://spatial.binghamton.edu/files/UK_Housing.csv",header = TRUE)
str(df)

df <- df[,c(2:9)]
str(df)


full.model <- lm(price ~ .,data = df)
summary(full.model)
confint(full.model)

new.model <- lm(price~garage + datebuilt + floorarea + detached + fireplace,data=df)
summary(new.model)
resid(new.model)
sd(resid(new.model))

sqrt(sum(resid(new.model)^2)/498)  # see textbook page 236 equation (8.24)

anova(full.model)
library(car)
vif(full.model)
## a common rule of thumb, if vif >5, this indicates potential multicollinearity
cor(df)

















## http://www.businessinsider.com/heres-median-income-in-the-us-by-race-2013-9
set.seed(100)
race <- c(rep("Asian",50),rep("White",50),rep("Hispanic",50),rep("Black",50))
income <- c(rnorm(50,68636,10000),rnorm(50,57009),rnorm(50,39005,10000),rnorm(50,33321,10000))
df <- data.frame(income,race)
df <- df[sample(1:nrow(df)),]
str(df)
tapply(df$income, df$race, median)

m <- lm(income~race,data = df)
summary(m)

## income = 69451 - 34909*Black - 31444*Hispanic - 12442*White

summary(df$race)
levels(df$race)

contrasts(df$race) <- contr.treatment(4,base = 2)
df$race

m <- lm(income~race,data = df)
summary(m)
## income = 34541 + 34909*Asian + 3465*Hispanic + 22467*White







df <- read.delim("GlastonburyFestivalRegression.dat",header = TRUE)
write.csv(df,"Festival.csv",row.names = FALSE)

df <- read.csv("Festival.csv",header = TRUE)
music <- df$music
summary(music)
levels(music)
contrasts(df$music) <- contr.treatment(4,base = 4)
df$music

m <- lm(change ~ music, data = df)
summary(m)



df <- read.csv("AlbumSales1.csv",header = TRUE)
plot(df)
m <- lm(sales ~ adverts, data = df)
summary(m)
anova(m)
abline(m,col="red",lwd=2)
attributes(m)
plot(m)


df <- read.csv("AlbumSales2.csv",header = TRUE)
m <- lm(sales ~ adverts + airplay + attract+0, data = df)
summary(m)
anova(m)
m <- lm(sales ~ ., data = df)
summary(m)
library(car)
vif(m)
outlierTest(m)


df <- read.csv("AlbumSales2.csv",header = TRUE)
df$new <- df$attract + df$airplay
m <- lm(sales ~ ., data = df)
summary(m)
anova(m)

m <- lm(sales ~ ., data = df)
summary(m)
library(car)
vif(m)
outlierTest(m)


df <- read.csv("AlbumSales2.csv",header = TRUE)
m <- lm(sales ~ adverts + airplay + attract + 0, data = df)
summary(m)
anova(m)
m <- lm(sales ~ ., data = df)
summary(m)
library(car)
vif(m)
outlierTest(m)


full.model <- lm(sales ~ .,data = df)
reduced.model <- step(full.model,direction = "backward")
summary(reduced.model)

min.model <- lm(sales ~ 1,data = df)
fwd.model <- step(min.model,direction = "forward",scope = (~adverts + airplay + attract))
summary(fwd.model)
df <- swiss
summary(lm1 <- lm(Fertility ~ ., data = swiss))
slm1 <- step(lm1)
summary(slm1)
slm1$anova


aov(m)
anova(m)
coef(m)
m$

cor(df$adverts,df$sales)


df <- cars
plot(df)
x <- cars$speed
y <- cars$dist
plot(y ~ x)
m <- lm(y~x)
summary(m)

abline(m,col="red",lwd=2)
segments(x,fitted(m),x,y,col="blue",lty = "dashed")

m


lm(y~x,data = dfrm)
m <- lm(dist~speed,data = df)
summary(m)
attributes(m)
plot(m,which = 1)
plot(m)

plot(dist~speed,data = df)
abline(m,col="red",lwd=2)



library(car)
outlier.test(m)

acf(m)

lm(y ~ u + v + w)
anova(m)
coefficients(m)
coef(m)
confint(m)
deviance(m)
effects(m)
fitted(m)
residuals(m)
resid(m)
summary(m)


# Multiple Linear Regression Example 
fit <- lm(y ~ x1 + x2 + x3, data=mydata)
summary(fit) # show results

m <- lm(dist~speed+0,data = df)
summary(m)

lm(y~u*v)
y~u*v*w

#y ~ u + v + w + u:v:w


full.model <- lm(y ~ x1 + x2 + x3 + x4)


spent <- c(120,68,35,60,100,91,44,71,89,113)
income <- c(65,35,30,44,80,77,32,39,44,77)
m <- lm(spent~income)
summary(m)
fitted(m)
resid(m)
plot(m)



x <- c(65,35,30,44,80,77,32,39,44,77)
y <- c(120,68,35,60,100,91,44,71,89,113)

n <- length(x)
mx <- mean(x)
my <- mean(y)
sx <- sd(x)
sy <- sd(y)

numerator <- sum((x-mx)*(y-my))
denominator <- sum((x-mx)*(x-mx))

slope <- numerator / denominator
intercept <- my - slope * mx

m <- lm(y~x)
summary(m)

MSS <- sum((fitted(m) - my)^2)
RSS <- sum((y - fitted(m))^2)
TSS <- RSS + MSS

df1 <- 1
df2 <- n-2 

msq1 <- MSS / df1
msq2 <- RSS / df2
F.value <- msq1 / msq2 

F.critical <- qf(0.95,df1 = 1,df2 = n-2)

rsq <- MSS / TSS

df <- data.frame(y,x)

y <- c(0,1,0,1,0,1,0,1,1,1,0,0)
x <- c(32,89,50,49,80,56,40,70,72,76,32,58)

m <- lm(y~x)
summary(m)

gm <- glm(y~x,family = binomial(logit))
summary(gm)
fitted(gm)
anova(gm)
