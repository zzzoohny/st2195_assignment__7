# Section 2 - Graphics and Data Visualization in R

# setwd(): remember to set the working directory
# Menu: Session -> Set Working Directory -> To Source File Location

df = airquality     # assign built-in data set "airquality" to data frame
head(df)            # show the first parts

df$date <- ISOdate(1973, df$Month, df$Day)    # create date variable from month & day
str(df)                                       # show the structure


# Section 2 - Line and Scatter Plots

plot(df$date, df$Temp, type='l')    # x:date, y:Temp, lines only
plot(df$date, df$Temp, type='o')    # lines and points

install.packages("RColorBrewer")
library("RColorBrewer")

display.brewer.all(colorblindFriendly = TRUE)

colours <- brewer.pal(n=12, name="Paired")        # 12 colors in "Paired" palette
plot(df$date, df$Temp, type="l", col=colours[2])  # 2nd color

plot(df$date, df$Temp, type="o", ylim=c(1,180), col=colours[8])
lines(df$date, df$Ozone, type="o", col=colours[4])
legend("topright", legend=c("Temperature","Ozone"), col=colours[c(8,4)], pch=1)   # pch=1 => circle symbol

plot(df$date, df$Temp, type="l", lwd=2, lty=2,
     xlab="Date", ylab="Maximum daily temperature in Fahrenheit",
     main="Temperatures in New York City", col=colours[10])         # lty=2 => dashed

plot(df$Wind, df$Temp, pch=19, 
     xlab="Average wind in mph", ylab="Maximum daily temperature in Fahrenheit",
     main="Temperature vs Wind in New York", col=colours[2])

df$season[df$Month>6] <- 1    # create "season" variable as "1" for period after June
df$season[df$Month<=6] <- 2   # "season" variable as "2" for period before and include June
table(df$season)              # create frequency table for "season"

plot(df$Wind, df$Temp, pch=19, col=colours[c(2,6)],
     xlab="Average wind in mph", ylab="Maximum daily temperature in Fahrenheit",
     main="Temperature vs Wind in New York")
legend("topright", legend=c("post June","May or June"), 
       col=colours[c(2,6)], pch=19)

pairs(df[1:4], col=colours[c(2,6)],
      main="Matrix scatter plot of the air quality dataset",
      oma=c(5,5,5,15))                                        # set the margin
par(xpd=TRUE)                                                 # TRUE: all plotting is clipped to the figure region
legend("bottomright", legend=c("post June","May or June"),
       col=colours[c(2,6)], pch=1)


# Section 2 - Histograms and Density Plots

hist(df$Temp, breaks=20, col=colours[2])
hist(df$Temp, breaks=20, col=colours[2], freq=FALSE)    # density histogram

hist(df$Temp[df$season==1], breaks=20, xlim=c(55,100), col=colours[2],
     xlab="height", main="Distribution of Temperature before and after June")
hist(df$Temp[df$season==2], breaks=20, xlim=c(55,100), col=colours[6],
     add=TRUE)
legend("topright", legend=c("Post June","May or June"), col=colours[c(2,6)],
       pt.cex=2, pch=15)

d1 <- density(df$Temp[df$season==1])
d2 <- density(df$Temp[df$season==2])
plot(d1, xlim=c(45,100), col=colours[2], lwd=2,
     xlab="height", main="Distribution of Temperature before and after June")
lines(d2, xlim=c(45,100), col=colours[6], lwd=2)
legend("topright", legend=c("Post June","May or June"), col=colours[c(2,6)],
       lwd=2, pch=15)

boxplot(Temp ~ Month, data=df, col=colours[c(2,6,10,8,4)],
        names=c("May","Jun","Jul","Aug","Sep"))

install.packages("vioplot")
library(vioplot)
vioplot(Temp ~ Month, data=df, col=colours[c(2,6,10,8,4)],
        names=c("May","Jun","Jul","Aug","Sep"))


# Section 2 - Bar Plots

categories <- c("51-60","61-70","71-80","81-90","91-100")       # create categories
df$Temp_cat <- cut(df$Temp, seq(50,100,10), labels=categories)  # categorizing
Temp_counts <- table(df$Temp_cat)                               # create frequencies table
Temp_counts

barplot(height=Temp_counts, names=categories, col=colours[c(2,6,10,8,4)])

Temp_counts_2 <- table(df$Month, df$Temp_cat)     # two categorical variables
Temp_counts_2

barplot(height=Temp_counts_2, names=categories, col=colours[c(2,6,10,8,4)],
        legend=c("May","Jun","Jul","Aug","Sep"))

barplot(height=Temp_counts_2, names=categories, col=colours[c(2,6,10,8,4)],
        legend=c("May","Jun","Jul","Aug","Sep"), beside=TRUE)


# Section 2 - Heat Maps

head(mtcars)

data <- as.matrix(mtcars)                                 # convert data set into matrix
colblues <- colorRampPalette(brewer.pal(8,"Blues"))(25)   # set blue color with 25 expansions
heatmap(data, Rowv=NA, Colv=NA, scale="column",
        xlab="variable", main="heatmap", col=colblues)


