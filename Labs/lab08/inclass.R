x <- c(65,35,30,44,80,77,32,39,44,77)
y <- c(120,68,35,60,100,91,44,71,89,113)

df <- data.frame(x,y)
View(df)

mx <- mean(x)
my <- mean(y)

df$dx <- df$x - mx
df$dy <- df$y - my
df$pxy <- df$dx * df$dy

numerator <- sum(df$pxy)

df$dx2 <- (df$dx)^2

denominator <- sum(df$dx2)

slope <- numerator/denominator

intercept <- my - slope * mx

df$py <- intercept + slope * df$x

df$ry <- df$y - df$py
df$ry2 <- (df$ry)^2

RSS <- sum(df$ry2)
TSS <- sum((df$y - my)^2)
MSS <- TSS - RSS

n <- nrow(df)
df1 <- 1
df2 <- n-2

msq1 <- MSS/df1
msq2 <- RSS/df2
F.value <- msq1/msq2

F.critical <- qf(0.95,df1,df2)

pf(F.value,df1,df2,lower.tail = FALSE)


R2 <- MSS / TSS
m <- lm(y~x)
summary(m)
