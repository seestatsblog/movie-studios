# Libraries

library(reshape2)
library(ggplot2)
library(extrafont)
library(grid)
library(scales)


# Data

rankings <- read.csv('rankings.csv', header = TRUE, sep = ",")
names(rankings) <- c('Studios', 2000:2016)
rankings <- rankings[1:5,]

rankData <- melt(rankings)

gross <- read.csv('gross.csv', header = TRUE, sep = ",")
names(gross) <- c('Studios', 2000:2016)
gross <- gross[1:5,]

grossData <- melt(gross)

rankData$Gross <- grossData$value
rankData$Lower <- rankData$value - (rankData$Gross/max(rankData$Gross))/2
rankData$Upper <- rankData$value + (rankData$Gross/max(rankData$Gross))/2


# Bump chart

svg('movie studios.svg', width = 15, height = 5.48)

p1 <- ggplot(data = rankData, mapping = aes(x = variable, y = value, group = Studios, fill = Studios, colour = Studios)) +
  geom_line(size = 2, alpha = 0.6) +
  geom_linerange(data = rankData, mapping = aes(x = variable, ymin = Lower, ymax = Upper), size = 2) +
  geom_text(data = rankData[rankData$variable == 2016,], aes(x =  variable, y = value, label = Studios),
            hjust = 0, nudge_x = 0.2, family = "Segoe UI Light", color = "#434A54") +
  scale_y_reverse(breaks = c(1:7)) +
  scale_colour_manual(values = c("#EE6C4D", "#266DD3", "#F7CB15", "#3AB795", "#733988")) +
  xlab("") +
  ylab("Ranking based on market share") +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.background = element_blank(),
        plot.margin = margin(30,120,30,30),
        plot.title = element_text(hjust = 0.5, size = 20),
        legend.position = "none",
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 14, margin = margin(t =-10)),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 16, margin = margin(0,10,0,0)),
        text = element_text(family = "Segoe UI Light", color = "#434A54")) +
  ggtitle("Yearly box office rankings for the 5 highest earning movie studios")

gt <- ggplot_gtable(ggplot_build(p1))
gt$layout$clip[gt$layout$name == "panel"] <- "off"
grid.draw(gt)

dev.off()


# Disney

disney <- read.csv('disney.csv', header = TRUE, sep = ",")
disney <- disney[,1:7]

dollars <- function(x) {
  a <- rep(0, length(x))
  a[x>=1000000000] <-   paste("$", round(x[x>=1000000000]/1000000000), "bn", sep = "")
  a[x<1000000000] <- paste("$", round(x[x<1000000000]/1000000), "m", sep = "")
  a
}

svg('disney by movies.svg', width = 15, height = 5.48)

p3 <- ggplot() +
  geom_point(data = disney[disney$Type == "Other",], mapping = aes(x = as.factor(Year), y = Gross, fill = Type, colour = Type, size = Total), pch = 21, alpha = 0.3) +
  geom_point(data = disney[disney$Type != "Other",], mapping = aes(x = as.factor(Year), y = Gross, fill = Type, colour = Type, size = Total), pch = 21, alpha = 0.6) +
  geom_text(data = disney[disney$Type != "Other",], mapping = aes(x = as.factor(Year), y = Gross, label = Movie.title),
            size = 3.5, color = "#434A54", family = "Segoe UI Light") +
  scale_y_continuous(labels = dollars, breaks = pretty(disney$Gross), limits = range(pretty(disney$Gross))) +
  scale_size_area(name = "Disney domestic gross for entire year:", max_size = 10,
                  labels = dollars, breaks = c(1000000000,2000000000,3000000000), limits = range(c(1000000000,2000000000,3000000000))) +
  scale_fill_manual(name = "",
                    breaks = c("Star Wars", "Marvel", "Disney Animation", "Pixar", "Pirates of the Caribbean", "Other"),
                    values = c("#4FC1E9", "#FFCE54", "#c6cbd2", "#48CFAD", "#EC87C0", "#AC92EC")) +
  scale_colour_manual(name = "",
                      breaks = c("Star Wars", "Marvel", "Disney Animation", "Pixar", "Pirates of the Caribbean", "Other"),
                      values = c("#4FC1E9", "#FFCE54", "#c6cbd2", "#48CFAD", "#EC87C0", "#AC92EC")) +
  xlab("") +
  ylab("Domestic Gross ($)") +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.background = element_blank(),
        plot.margin = margin(30,30,10,30),
        plot.title = element_text(hjust = 0.5, size = 20),
        legend.title = element_text(size = 10),
        legend.position = "bottom",
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 14, margin = margin(t = 0)),
        axis.text.y = element_text(size = 14, margin = margin(t = 0)),
        axis.title.y = element_text(size = 16, margin = margin(0,10,0,0)),
        text = element_text(family = "Segoe UI Light", color = "#434A54")) +
  ggtitle("Domestic Gross for Disney films between 2010 - 2016")

gt <- ggplot_gtable(ggplot_build(p3))
gt$layout$clip[gt$layout$name == "panel"] <- "off"
grid.draw(gt)

dev.off()

