library(dplyr)
library(ggplot2)

df <- expand.grid(Year = 1995:2016,
                  Software = c('R', 'STATA', 'SAS', 'Excel'))
df$value <- NA
df$value[df$Software == 'R'] <- jitter(seq(0, 8, length = 22)) ^2
df$value[df$Software == 'STATA'] <- jitter(seq(5, 10, length = 22)) ^2
df$value[df$Software == 'SAS'] <- jitter(seq(7, 4, length = 22)) ^2
df$value[df$Software == 'Excel'] <- jitter(seq(5, 8, length = 22)) ^2

ggplot(data = df,
       aes(x = Year,
           y = value,
           group = Software,
           color = Software)) +
  geom_line() +
  ylab('Value') +
  ggtitle('Health economists software preferences',
          'Should universities adapt their curriculum?')

df <- data.frame(Software = sort(unique(df$Software)),
                 publications = c(31, 40, 19, 10),
                 classes = c(15, 35, 45, 5))

label_df <- data.frame(x = c(45, 5),
                       y = c(5, 45),
                       label = c('Undertaught',
                                 'Overtaught'))

ggplot(data = df,
       aes(x = publications,
           y = classes)) +
  geom_point() +
  geom_label(aes(label = Software)) +
  xlab('Publication index') +
  ylab('Class index') +
  geom_abline(intercept = 0, slope = 1) +
  xlim(0, 50) +
  ylim(0, 50) +
  ggtitle('Health economists software preferences',
          'Should universities adapt their curriculum?') +
  geom_text(data = label_df,
            aes(x = x,
                y = y,
                label = label),
            color = 'darkred',
            size = 6)
###################

df <- expand.grid(Year = 1995:2016,
                  Term = c('Sustainability', 'Development', 'Third world', 'Climate change'))
df$value <- NA
df$value[df$Term == 'Sustainability'] <- jitter(seq(0, 8, length = 22)) ^2
df$value[df$Term == 'Development'] <- jitter(seq(5, 10, length = 22)) ^2
df$value[df$Term == 'Third world'] <- jitter(seq(7, 4, length = 22)) ^2
df$value[df$Term == 'Climate change'] <- c(rep(1, 11), jitter(seq(3, 6, length = 11)) ^2)


ggplot(data = df,
       aes(x = Year,
           y = value,
           group = Term,
           color = Term)) +
  geom_line() +
  ylab('Value') +
  ggtitle('Buzzwords',
          'Should universities adapt their curriculum?')
