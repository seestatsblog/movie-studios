# Libraries
library(reshape2)
library(ggplot2)


# Mock Data

educattn<-matrix(c(90.4,90.3,75.7,78.9,66,71.8,70.5,70.4,68.4,67.9,
                   67.2,76.1,68.1,74.7,68.5,72.4,64.3,71.2,73.1,77.8),ncol=2,byrow=TRUE)
rownames(educattn)<-c("Anchorage AK","Boston MA","Chicago IL",
                      "Houston TX","Los Angeles CA","Louisville KY","New Orleans LA",
                      "New York NY","Philadelphia PA","Washington DC")
colnames(educattn)<-c(1990,2000)

data <- as.data.frame(educattn)
data$City <- rownames(data)

bumpData <- melt(data)
bumpData$Rank <- NA
bumpData$Rank[bumpData$variable == '1990'] <- rank(bumpData$value[bumpData$variable == '1990'])
bumpData$Rank[bumpData$variable == '2000'] <- rank(bumpData$value[bumpData$variable == '2000'])
bumpData$Upper <- bumpData$Rank + (bumpData$value/max(bumpData$value))/2
bumpData$Lower <- bumpData$Rank - (bumpData$value/max(bumpData$value))/2

# Mock Bump chart

ggplot(data = bumpData, mapping = aes(x = variable, group = City, fill = City, colour = City)) +
  geom_ribbon(data = bumpData, mapping = aes(x = variable, ymin = Lower, ymax = Upper, alpha = 0.3))

# Actual data

data.2016 <- read.csv('2016.csv', header = FALSE)[,1:6]
names(data.2016) <- c("Rank", "Studio", "Market.Share", "Total.Gross", "Movies.Tracked", "Year.Movies")

data.2015 <- read.csv('2015.csv', header = FALSE)[,1:6]
names(data.2015) <- c("Rank", "Studio", "Market.Share", "Total.Gross", "Movies.Tracked", "Year.Movies")

data.2014 <- read.csv('2014.csv', header = FALSE)[,1:6]
names(data.2014) <- c("Rank", "Studio", "Market.Share", "Total.Gross", "Movies.Tracked", "Year.Movies")

data.2013 <- read.csv('2013.csv', header = FALSE)[,1:6]
names(data.2013) <- c("Rank", "Studio", "Market.Share", "Total.Gross", "Movies.Tracked", "Year.Movies")

data.2012 <- read.csv('2012.csv', header = FALSE)[,1:6]
names(data.2012) <- c("Rank", "Studio", "Market.Share", "Total.Gross", "Movies.Tracked", "Year.Movies")

data.2011 <- read.csv('2011.csv', header = FALSE)[,1:6]
names(data.2011) <- c("Rank", "Studio", "Market.Share", "Total.Gross", "Movies.Tracked", "Year.Movies")

data.2010 <- read.csv('2010.csv', header = FALSE)[,1:6]
names(data.2010) <- c("Rank", "Studio", "Market.Share", "Total.Gross", "Movies.Tracked", "Year.Movies")

data.2009 <- read.csv('2009.csv', header = FALSE)[,1:6]
names(data.2009) <- c("Rank", "Studio", "Market.Share", "Total.Gross", "Movies.Tracked", "Year.Movies")

data.2008 <- read.csv('2008.csv', header = FALSE)[,1:6]
names(data.2008) <- c("Rank", "Studio", "Market.Share", "Total.Gross", "Movies.Tracked", "Year.Movies")

data.2007 <- read.csv('2007.csv', header = FALSE)[,1:6]
names(data.2007) <- c("Rank", "Studio", "Market.Share", "Total.Gross", "Movies.Tracked", "Year.Movies")

data.2006 <- read.csv('2006.csv', header = FALSE)[,1:6]
names(data.2006) <- c("Rank", "Studio", "Market.Share", "Total.Gross", "Movies.Tracked", "Year.Movies")

data.2005 <- read.csv('2005.csv', header = FALSE)[,1:6]
names(data.2005) <- c("Rank", "Studio", "Market.Share", "Total.Gross", "Movies.Tracked", "Year.Movies")

data.2004 <- read.csv('2004.csv', header = FALSE)[,1:6]
names(data.2004) <- c("Rank", "Studio", "Market.Share", "Total.Gross", "Movies.Tracked", "Year.Movies")

data.2003 <- read.csv('2003.csv', header = FALSE)[,1:6]
names(data.2003) <- c("Rank", "Studio", "Market.Share", "Total.Gross", "Movies.Tracked", "Year.Movies")

data.2002 <- read.csv('2002.csv', header = FALSE)[,1:6]
names(data.2002) <- c("Rank", "Studio", "Market.Share", "Total.Gross", "Movies.Tracked", "Year.Movies")

data.2001 <- read.csv('2001.csv', header = FALSE)[,1:6]
names(data.2001) <- c("Rank", "Studio", "Market.Share", "Total.Gross", "Movies.Tracked", "Year.Movies")

data.2000 <- read.csv('2000.csv', header = FALSE)[,1:6]
names(data.2000) <- c("Rank", "Studio", "Market.Share", "Total.Gross", "Movies.Tracked", "Year.Movies")

allData <- rbind(data.2016, data.2015, data.2014, data.2013, data.2012, data.2011, data.2010,
               data.2009, data.2008, data.2007, data.2006, data.2005, data.2004, data.2003,
               data.2002, data.2001, data.2000)

totalGross <- aggregate(allData$Total.Gross, by = list(Studio = allData$Studio), FUN = sum)

orderedData <- totalGross[order(totalGross$x, na.last = NA, decreasing = TRUE),]

top10 <- orderedData[1:10,]

topStudios <- as.vector(top10$Studio)

for(i in 1:10){
  studioName <- topStudios[i]
  table[i,1]
}
