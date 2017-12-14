### Author: Dr. Qiusheng Wu

### Q1
if (!"UScensus2010" %in% installed.packages()) install.packages("UScensus2010")
library(UScensus2010)
if(!"UScensus2010county" %in% installed.packages()) install.county("osx")
if(!"UScensus2010tract" %in% installed.packages()) install.tract("osx")
library(UScensus2010county)
library(UScensus2010tract)

### Q1a
data("new_york.county10")
county <- new_york.county10
plot(county)
county.df <- county@data

### Q1b
if(!"tmap" %in% installed.packages()) install.packages("tmap")
library(tmap)
qtm(shp = county,fill = "NAME10",fill.title="County")

### Q1c
## how many counties in New York
print(nrow(county.df))

### Q1d
## what's the fips and total population of Broome County?
Broome <- county.df[county.df$NAME10=="Broome",]
print(Broome$fips)

### Q1e
print(Broome$P0010001)
### descriptive statistics: 
pop <- county.df$P0010001
if(!"fBasics" %in% installed.packages()) install.packages("fBasics")
library(fBasics)
stat <- basicStats(pop)
stat
stat["Sum",1]
stat["Minimum",1]
stat["Maximum",1]
stat["Mean",1]
stat["Median",1]
stat["Skewness",1]

### Q1f
hist(pop,breaks = 20)
boxplot(pop)


### Q2a
library(UScensus2010tract)
data("new_york.tract10")
tract <- new_york.tract10
plot(tract)
tract.df <- tract@data

### Q2b
nrow(tract.df)
pop <- tract.df$P0010001
sum(pop)


### Q2c
Broome <- tract[tract.df$county=="007",]
plot(Broome)

### Q2d
## how many census tracts in Broome county, what's the total population
df <- Broome@data
nrow(df)
pop <- df$P0010001
sum(pop)

### Q2e
### 
hist(pop)
boxplot(pop)

### Q2f
Broome <- Broome[,1:5]
df <- Broome@data
df$pop.pct <- df$P0010001/sum(pop)
Broome@data <- df
getwd()
library(rgdal)
writeOGR(Broome,dsn = ".",layer = "Broome",driver = "ESRI Shapefile")


### Q3a
library(raster)
library(ncdf4)
ndvi <- brick("NDVI.nc")
ndvi

### Q3b
nrow(ndvi)
ncol(ndvi)
ncell(ndvi)
nlayers(ndvi)
res(ndvi)
extent(ndvi)
bbox(ndvi)
projection(ndvi)

### Q3c
ndvi.mean <- mean(ndvi)
plot(ndvi.mean)

ndvi.max <- max(ndvi)
plot(ndvi.max)

ndvi.range <- range(ndvi)
plot(ndvi.range)

writeRaster(ndvi.mean,filename = "ndvi.mean.tif")

### Q3d
plot(ndvi)
plot(ndvi,13:24)

### Q3e
ndvi2001 <- subset(ndvi,13:24)
ndvi2001
plot(ndvi2001)
hist(ndvi2001)

### Q3f
plot(ndvi,1)
values <- click(ndvi,n=1,xy=FALSE)
plot(as.vector(values),type="b")


hist(ndvi[[1:12]])
hist(pop,breaks = 20)
boxplot(pop)


