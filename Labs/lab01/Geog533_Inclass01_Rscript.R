df <- read.csv("students.csv",header = TRUE)


# Solution 1
str(df)
male <- df[df$Gender=="Male",]
female <- df[df$Gender=="Female",]
mean(male$SAT)
median(male$SAT)
quantile(male$SAT)["75%"]
sd(male$SAT)
var(male$SAT)

mean(female$SAT)
median(female$SAT)
quantile(female$SAT)["75%"]
sd(female$SAT)
var(female$SAT)


# Soltuion 2
summary(male$SAT)
summary(female$SAT)
sd(male$SAT)
var(male$SAT)
sd(female$SAT)
var(female$SAT)


# Solution 3
#install.packages("fBasics")
library(fBasics)
basicStats(male$SAT)
basicStats(female$SAT)


# Solution 4
tapply(df$SAT, df$Gender, summary)
tapply(df$SAT, df$Gender, sd)
tapply(df$SAT, df$Gender, var)




# misc
male.des <- basicStats(male$SAT)
female.des <- basicStats(female$SAT)
result <- cbind(male.des,female.des)
re <- as.data.frame(tapply(df$SAT, df$Gender, basicStats))
tapply(df$Age, list(df$Gender,df$Major), median)
aggregate(df$SAT,list(sex=df$Gender,major=df$Major),mean)
