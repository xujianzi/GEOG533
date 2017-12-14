### Author: Dr. Qiusheng Wu

### Load the library and the dataset
library(MASS)
df <- Cars93
?Cars93

### check the metadata and compute the summary statistics
str(df)
summary(df)

### Question 1a: find out the cheapest car in each type
price.min <- tapply(df$Price, df$Type, min)     ### get the min price based on type
price.min
df$price.min <- price.min[df$Type]              ### write the minimum price to a new column
df$price.diff <- df$Price - df$price.min        ### write the price difference to a new column
cheapest <- df[df$price.diff == 0,]             ### select the rows with price.diff = 0
cheapest[,c("Manufacturer","Model","Type","Price")]


### Question 1b: find out the greatest fuel efficiency in each type
MPG.max <- tapply(df$MPG.highway, df$Type, max)   ### get the max MPG based on type
MPG.max
df$MPG.max <- MPG.max[df$Type]                    ### write the max MPG to a new column
df$MPG.diff <- df$MPG.highway - df$MPG.max        ### write the MPG difference to a new column
best <- df[df$MPG.diff==0,]                       ### select the rows with MPG.diff = 0
best[,c("Manufacturer","Model","Type","MPG.highway")]


### Question 2: compute the mean Hoursepower for each type, and the difference between the mean
power.mean <- tapply(df$Horsepower, df$Type, mean)  ### get the mean hoursepower based on group
power.mean
df$power.mean <- power.mean[df$Type]                ### write the mean hoursepower to a new column
df$power.diff <- df$Horsepower - df$power.mean      ### write the hoursepower diff to a new column
hist(df$power.diff)
library(moments)
skewness(df$power.diff)
kurtosis(df$power.diff)

### Question 3: create two new dataframes for USA and nonUSA cars
df.USA <- df[df$Origin == "USA",]
df.nonUSA <- df[df$Origin == "non-USA",]


### Question 4: save the USA car data to a file
write.table(df.USA,file = "carsUSA.txt",sep = ",")    ### write to a txt file
USA.txt <- read.table("carsUSA.txt",header = TRUE,sep = ",")  ### read the txt file
str(USA.txt)

write.csv(df.USA,"carsUSA.csv")    ### write to a csv file
USA.csv <- read.csv("carsUSA.csv")   ### read the csv file
str(USA.csv)
