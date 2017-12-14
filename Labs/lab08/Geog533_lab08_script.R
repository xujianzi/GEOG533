### author: Dr. Qiusheng Wu

############################## Ex1
# 1a
n <- 24
intercept <- 0.46

slope <- 0.19

TSS <- 2.3  # Total sum of squares
RSS <- 1.7  # Residual sum of squares
MSS <- TSS - RSS # Model sum of squares

df1 <- 1 
df2 <- n-2

msq1 <- MSS/df1 # mean square of regression  0.6
msq2 <- RSS/df2  # mean square of residual   0.077

F.value <- msq1/msq2  # 7.76

# 1b
y <- intercept + slope * (50000/1000)  # y = 9.96


# 1c
# for every increase in annual income equal to $1000, the predicted weekly shopping trip frequency will increase by 0.19

# 1d
F.critical <- qf(0.95,df1,df2)  # 4.30
# since F.value > F.critical, we reject the null hypothesis. In other words, the regression coefficient is significantly different from zero

# 1e
r.sq <- MSS/TSS  # R2 = 0.26
r <- sqrt(r.sq)  # r = 0.51


########################################## Ex6

## option 1: using lm() function

# (a)
y <- c(36,78,11,45)
x <- c(400,800,200,675)
n <- length(y)

m <- lm(y~x)
summary(m)
# intercept = -7.63721
# slope = 0.09665

## (b)
# use the equation on page 236 of the textbook
se <- sqrt(sum(residuals(m)^2)/(n-2))  # 11.252

## (c)
summary(m)
anova(m)
# p-value: 0.05663
# p-value > 0.05, so we fail to reject the null hypothesis that the regression coefficient associated with the independent variables is equal to zero
confint(m)
#                     2.5 %     97.5 %
# (Intercept)   -66.487337152 51.2129252
# x             -0.006754067  0.2000541

## (d)
summary(m)
# R2 = 0.8899

## (e)
df <- data.frame(x,y)
df$predict <- fitted(m)
df$residual <- residuals(m)
View(df)

## (f)
anova(m)
# Analysis of Variance Table
# 
# Response: y
# Df  Sum Sq Mean Sq F value  Pr(>F)  
# x          1 2047.77 2047.77  16.173 0.05663 .
# Residuals  2  253.23  126.61                  
# ---
#   Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1

## (g)
plot(x,y)
abline(m,col="red",lwd=2)


########################## Ex 10
df <- read.csv("Milwaukee_Sales_2012.csv",header = TRUE)
m <- lm(SalePrice ~ LotSize,data = df)
summary(m)

# SalePrice = 92980 + 6.443 * Lotsize

# Call:
#   lm(formula = SalePrice ~ LotSize, data = df)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -144724  -42205  -14553   20987  808570 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 9.298e+04  5.249e+03  17.715  < 2e-16 ***
#   LotSize     6.443e+00  7.835e-01   8.223 4.37e-16 ***
#   ---
#   Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1
# 
# Residual standard error: 83090 on 1447 degrees of freedom
# Multiple R-squared:  0.04464,	Adjusted R-squared:  0.04398 
# F-statistic: 67.62 on 1 and 1447 DF,  p-value: 4.374e-16

anova(m)
# Analysis of Variance Table
# 
# Response: SalePrice
# Df     Sum Sq    Mean Sq F value    Pr(>F)    
# LotSize      1 4.6676e+11 4.6676e+11  67.615 4.374e-16 ***
#   Residuals 1447 9.9889e+12 6.9032e+09                      
# ---
#   Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1

plot(df$LotSize,df$SalePrice)
abline(m,col="red",lwd=2)



############################################ Ex 11
df <- read.csv("UK_Housing.csv",header = TRUE)

## (a)
m <- lm(df$price~df$bedrooms)
summary(m)

## price = 11254.1 + 11892.6 * bedrooms


# Call:
#   lm(formula = df$price ~ df$bedrooms)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -39324 -11511  -3539   8461 107568 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  11254.1     2312.7   4.866 1.53e-06 ***
#   df$bedrooms  11892.6      876.5  13.568  < 2e-16 ***
#   ---
#   Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1
# 
# Residual standard error: 17800 on 497 degrees of freedom
# Multiple R-squared:  0.2703,	Adjusted R-squared:  0.2688 
# F-statistic: 184.1 on 1 and 497 DF,  p-value: < 2.2e-16


## (b)
m <- lm(df$price~df$bathrooms)
summary(m)

## price = 32729 + 7113 * bathrooms

# Call:
#   lm(formula = df$price ~ df$bathrooms)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -34955 -15242  -5342  10908 114658 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     32729       3149  10.394  < 2e-16 ***
#   df$bathrooms     7113       2682   2.653  0.00824 ** 
#   ---
#   Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1
# 
# Residual standard error: 20690 on 497 degrees of freedom
# Multiple R-squared:  0.01396,	Adjusted R-squared:  0.01198 
# F-statistic: 7.036 on 1 and 497 DF,  p-value: 0.008245

## comments, although the model is significant, the R2 is too small, compared to the previous model using bedrooms. That means bedroom is a much better predictor than bathroom.