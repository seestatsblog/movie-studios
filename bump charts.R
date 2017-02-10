# Libraries
library(reshape2)
library(ggplot2)


# Data

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

# Bump chart

ggplot(data = bumpData, mapping = aes(x = variable, group = City, fill = City, colour = City)) +
  geom_ribbon(data = bumpData, mapping = aes(x = variable, ymin = Lower, ymax = Upper, alpha = 0.3))
