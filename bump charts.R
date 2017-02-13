# Libraries

library(reshape2)
library(ggplot2)
library(extrafont)
library(grid)


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


# Bump chart 1

svg('movie studios.svg', width = 15, height = 5.48)

p1 <- ggplot(data = rankData, mapping = aes(x = variable, y = value, group = Studios, fill = Studios, colour = Studios)) +
  geom_line(size = 2, alpha = 0.6) +
  geom_linerange(data = rankData, mapping = aes(x = variable, ymin = Lower, ymax = Upper), size = 2) +
  scale_y_reverse(breaks = c(1:7)) +
  scale_colour_manual(values = c("#4FC1E9", "#FFCE54", "#48CFAD", "#EC87C0", "#AC92EC", "#FC6E51", "#A0D468", "#ED5565","#5D9CEC")) +
  xlab("") +
  ylab("Ranking based on market share") +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.background = element_blank(),
        plot.margin = margin(10,10,10,20),
        legend.position = "none",
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 10, margin = margin(t =-10)),
        axis.title.y = element_text(size = 10, margin = margin(0,10,0,0)),
        text = element_text(family = "Segoe UI Light", color = "#434A54")) +
  ggtitle("Movie studios")

gt <- ggplot_gtable(ggplot_build(p1))
gt$layout$clip[gt$layout$name == "panel"] <- "off"
grid.draw(gt)

dev.off()


# Bump chart 2 

p1 <- ggplot(data = rankData, mapping = aes(x = variable, y = value, group = Studios, fill = Studios, colour = Studios)) +
  geom_ribbon(data = rankData, mapping = aes(x = variable, ymin = Lower, ymax = Upper, alpha = 0.3)) +
  scale_y_reverse() +
  scale_colour_manual(values = c("#4FC1E9", "#FFCE54", "#48CFAD", "#EC87C0", "#AC92EC", "#FC6E51", "#A0D468", "#ED5565","#5D9CEC")) +
  scale_fill_manual(values = c("#4FC1E9", "#FFCE54", "#48CFAD", "#EC87C0", "#AC92EC", "#FC6E51", "#A0D468", "#ED5565","#5D9CEC")) +
  xlab("") +
  ylab("Rank") +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.background = element_blank(),
        plot.margin = margin(10,10,10,20),
        legend.position = "none",
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 10, margin = margin(t =-10)),
        axis.title.y = element_text(size = 10, margin = margin(0,10,0,0)),
        text = element_text(family = "Segoe UI Light", color = "#434A54")) +
  ggtitle("Movie studios")
