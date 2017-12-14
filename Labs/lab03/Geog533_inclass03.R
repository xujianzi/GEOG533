
dbinom(40,size = 100,prob = 0.5)
dbinom(50,size = 100,prob = 0.5)

pbinom(40,size = 100,prob = 0.5)
pbinom(60,size = 100,prob = 0.5,lower.tail = FALSE)

diff(pbinom(c(40,60),size = 100,prob = 0.5))

a <- pbinom(40,size = 100,prob = 0.5)
b <- pbinom(60,size = 100,prob = 0.5,lower.tail = FALSE)

c <- diff(pbinom(c(40,60),size = 100,prob = 0.5))

a+b+c
