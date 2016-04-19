par(mar = c(0,0,0,0))
par(oma = c(0,0,0,0))
library(maps)
library(RColorBrewer)
world <- map('world',
             ylim = c(-35, 40),
             xlim = c(-20, 50))
world$country <- unlist(lapply(strsplit(world$names, ':'), function(x){x[[1]]}))
world$country_number <- as.numeric(factor(world$country))
world$val <- round(world$country_number * rnorm(mean = 10, sd = 2, n = length(world$country_number)))
cols <- colorRampPalette(brewer.pal(9, 'Spectral'))(max(world$val))

pdf('map.pdf')
map('world', fill = TRUE, col = cols[world$val],
    ylim = c(-35, 40),
    xlim = c(-20, 50))

legend('bottomleft',
       legend = c('x', 'y', 'z'),
       title = 'Cost',
       fill = brewer.pal(3, 'Spectral'))
dev.off()

costs <- data.frame(Country = c('Uganda', 'Kenya', 'Mozambique'),
                    Transporation = c(1.14, 0.86, 3.51),
                    Storage = c(0.14, 0.19, 0.19),
                    Loss = c(0.11, 0.31, 0.24),
                    Disposal = c(0.34, 0.32, 0.32),
                    Documentation = c(0.15, 0.26, 0.88),
                    Administration = c(1.41, 0.98, 1.61))

print(xtable(costs))
