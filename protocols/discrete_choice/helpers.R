library(ggplot2)
library(ggthemes)
theme_gg <- theme_economist() +
  theme(plot.title = element_text(size = 12),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        legend.text = element_text(size = 6))
