A <- c(38,42,50,57,80,70,32,20)
B <- c(58,66,80,62,73,39,73,58)
C <- c(80,70,60,55,72,73,81,50)

swimming <- c(A,B,C)
group <- c(rep("Central city",8),rep("Suburbs",8),rep("Rural",8))

df <- data.frame(swimming,group)

m <- aov(swimming~group,data = df)
m
summary(m)

TukeyHSD(m)
plot(TukeyHSD(m))

qf(0.95,df1 = 2,df2 = 21)
pf(3.51,df1=2,df2=21,lower.tail = FALSE)

s <- oneway.test(swimming~group,data = df,var.equal = TRUE)
s
summary(s)
str(s)
