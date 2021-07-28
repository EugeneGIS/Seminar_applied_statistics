#Load R packages
library(tidyverse)
library(ggpubr)
library(MASS) # for AIC model selection
library(DAAG) 
library(bootstrap)
library(maps)
library(mapdata)  # country/continent outlines
library(fields)   # image.plot() function for nice legends
library(raster)

#read files
list.files("E:/FS 2021/Seminar in Applied Statistics in Climatology und Hydrology/Project - Multi regression/regression_daten")
station <- readRDS("E:/FS 2021/Seminar in Applied Statistics in Climatology und Hydrology/Project - Multi regression/regression_daten/stations.rds")
TabsY <- raster("E:/FS 2021/Seminar in Applied Statistics in Climatology und Hydrology/Project - Multi regression/regression_daten/TabsY-1985-2014.asc")

#Correlation analysis
#Draw scatter plots
p1 = ggplot(data = station) + geom_point(mapping = aes(x = xLV1903, y = tre200d0))
p2 = ggplot(data = station) + geom_point(mapping = aes(x = yLV1903, y = tre200d0))
p3 = ggplot(data = station) + geom_point(mapping = aes(x = alt, y = tre200d0))
p4 = ggplot(data = station) + geom_point(mapping = aes(x = asp, y = tre200d0))
p5 = ggplot(data = station) + geom_point(mapping = aes(x = slp, y = tre200d0))
p6 = ggplot(data = station) + geom_point(mapping = aes(x = tpi, y = tre200d0))
ggarrange(p1, p2, p3, p4, p5, p6, ncol = 3, nrow = 2) 
#Calculate correlation coefficient
cor(station[,3],station[,1:2])
cor(station[,3],station[,4:7])

#Using AIC to built the multiple regression model
full.model <- lm(station$tre200d0~station$xLV1903+station$yLV1903+station$alt+station$asp+station$slp+station$tpi, data = station)
require(MASS)
fit.fullmodel <- stepAIC(full.model, direction = "backward")
#Significance test for predictors
experiment <- lm(station$tre200d0~station$yLV1903+station$alt+station$slp+station$tpi, data = station)
summary(experiment)
fit <- lm(station$tre200d0~station$yLV1903+station$alt+station$slp, data = station)
summary(fit)
par(mfrow = c(2,2))
plot(fit)
#See importance of predictor
fit.main <- lm(station$tre200d0~station$alt, data = station)
summary(fit.main)

#Cross-validation
cv.lm(data = station,form.lm = formula(tre200d0~yLV1903+alt+slp), m=10) # 10 fold cross-validation
#calculate R-square for Cross-validation
theta.fit <- function(x,y){lsfit(x,y)}
theta.predict <- function(fit,x){cbind(1,x)%*%fit$coef}
X <- as.matrix(station[c("yLV1903","alt","slp")])
y <- as.matrix(station[c("tre200d0")])
results <- crossval(X,y,theta.fit,theta.predict,ngroup=10)
cor(y, fit$fitted.values)**2 # raw R2
cor(y, results$cv.fit)**2 # cross-validated R2

#calculate corresponding regression coefficient and interval estimation
confint(fit)

#Load the data
setwd("E:/FS 2021/Seminar in Applied Statistics in Climatology und Hydrology/Project - Multi regression/regression_daten")
alt <- raster("DHM200-z.asc")
# point map with color scale
str(fit)
diff1 = fit$fitted.values - station$tre200d0
lev <- pretty(diff1,5)
# create 13 categories that cover the range of data values
br <- length(lev)
# create color scale with number of levels
break_point = as.numeric(cut(diff1,breaks=br))
break_point 
max(break_point)
min(break_point) # see the max and min value in station difference to decide how much levels we will use
tepcol <- two.colors(n=7,start="blue", middle="lightyellow", end="red")
# draw the map to explain the difference in the stations of Switzerland
plot(alt) # the base map of Switzerland
points(station$xLV1903,station$yLV1903,        # x and y coordinates
       cex=1.2,                                # cex=point size~precip
       pch=15,                                 # point type: 15 is square with possible color filling
       col=tepcol,                             # point fill color 
       xlab="Longitude",ylab="Latitude")       # labels 
legend('bottomright',legend = c(1.2,0.8,0.4,0,-0.4,-0.8,-1.2),col=tepcol,lwd=c(1,2)) #Legend for the color of station

# Application for all Switzerland 1985-2014 calculate using TabsY-1985-2014.asc
setwd("E:/FS 2021/Seminar in Applied Statistics in Climatology und Hydrology/Project - Multi regression/regression_daten")
Temp <- raster("TabsY-1985-2014.asc")
ycor <- raster("DHM200-y.asc")
alt <- raster("DHM200-z.asc")
slp <- raster("DHM200-slope.asc")
T_estimate = fit$coefficients[1]+fit$coefficients[2]*ycor+fit$coefficients[3]*alt+fit$coefficients[4]*slp
T_estimate_resampled <- resample(T_estimate,Temp,method = 'ngb') # resample the projection of data
T_diff = T_estimate_resampled - Temp #calculate the difference of true and predicted data
tepcols <- two.colors(n=9,start="blue", middle="lightyellow", end="red") # make color scale with 9 colors from blue to red
plot(T_diff,zlim=c(-7,7),col=tepcols) # plot map with data values in the range of -7 to 7 and the color scale defined above
