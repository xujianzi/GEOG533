### Author: Dr. Qiusheng Wu

##################### Ex7
## (a)
n <- 14
MSS <- 4234
RSS <- 3487
TSS <- MSS + RSS  ## TSS = 7721
df1 <- 3
df2 <- n-1-df1  ## df2=10
msq1 <- MSS/df1  ## msq1 = 1411.33
msq2 <- RSS/df2  ## msq2 = 348.7
F.value <- msq1/msq2  ## F.value = 4.047

## (b)
R2 <- MSS/TSS  ## R2=0.548

## (c)
# use the euqation 8.24 on textbook page 236: se = sqrt(RSS/(n-2))
se <- sqrt(RSS/(n-4))  ## se = 18.67
se2 <- sqrt(msq2)


## (d)
F.critical <- qf(0.95,df1,df2)  ## F.critical = 3.708
# F.value > F.critical, so we reject the null hypothesis. 

## (e)
## the coefficient of size of lot is not in the direction I would hypothesis. 

## (f)
## t.statistic = coefficient / standard error, see equation 8.25 on page 237
t.critical <- qt(0.975,df = df2)  ## t.critical = 2.228
t.critical2 <- qt(0.025,df = df2) ## t.critical2 = -2.228

t.income <- 1.57/0.34  ## t.income = 4.618
t.house <- 23.4/11.2  ## t.house = 2.089
t.lot <- -9.5/7.1 ## t.lot = -1.338
t.constant <- 40000/1000  ## t.constant = 40

## (g)
# The VIF of size of lot is 11.3 > 5, this indicates potential multicollinearity. I recommend remove this size of lot variable

## (h)
price <- 40000 +  1.57*40000 + 23.4*1500 + (-9.5)*6000  ## price = 80900


########################## Ex10
## (a)
df <- read.csv("UK_Housing.csv",header = TRUE)
str(df)

df <- df[,c(2:9)]
str(df)
full.model <- lm(price ~ .,data = df)
summary(full.model)
confint(full.model)
anova(full.model)
aov(full.model)

library(car)
vif(full.model)
# garage  bedrooms bathrooms datebuilt floorarea  detached fireplace 
# 1.366464  1.704548  1.040451  1.321745  1.866295  1.236362  1.167944 
## the VIF of all variables are <5, this indicates no multicollinearity

cor(df)
plot(full.model)
## plot the residual graph, we can see outlines 209, 374, 301

## weakness of the regression: bedrooms and bathrooms are insignificant.

### (b)
## improve the model by removing the insignificant variables found in (a)
new.model <- lm(price~garage + datebuilt + floorarea + detached + fireplace,data=df)
summary(new.model)


########################### Ex11
## (a)
df <- read.csv("Milwaukee_Sales_2012.csv", header = TRUE)
str(df)

full.model <- lm(SalePrice ~ Bedrms + LotSize + FinSqft + Age + Baths, data = df)
summary(full.model)

library(car)
vif(full.model)
# Bedrms  LotSize  FinSqft      Age    Baths 
# 2.020048 1.254748 3.110106 1.352311 2.087716 
# The VIFs of all variables are < 5, this indicates no multicollinearity
plot(full.model)
# by checking the residual plot, there are outliers 161, 145, 143

# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  2.624e+04  9.420e+03   2.786  0.00541 ** 
#   Bedrms      -2.876e+04  2.372e+03 -12.122  < 2e-16 ***
#   LotSize      3.580e+00  6.442e-01   5.557 3.26e-08 ***
#   FinSqft      1.007e+02  4.535e+00  22.210  < 2e-16 ***
#   Age         -1.306e+02  7.629e+01  -1.712  0.08711 .  
# Baths        2.302e+04  3.698e+03   6.225 6.29e-10 ***

## the vairable age of house is insignificant. 


##(b)
# improve the model by removing the insignificant variable age



# library(car)
# Prestige
# ?Prestige
# #https://rstudio-pubs-static.s3.amazonaws.com/65641_88a692252c6c4f2ab279d115e59e6767.html


new.model <- lm(SalePrice ~ Bedrms + LotSize + FinSqft + Baths, data = df)
summary(new.model)

# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  1.544e+04  6.995e+03   2.206   0.0275 *  
#   Bedrms      -2.870e+04  2.374e+03 -12.090  < 2e-16 ***
#   LotSize      4.058e+00  5.808e-01   6.987 4.28e-12 ***
#   FinSqft      9.849e+01  4.346e+00  22.659  < 2e-16 ***
#   Baths        2.401e+04  3.656e+03   6.568 7.08e-11 ***
