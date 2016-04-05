library(ggplot2)

# Worker prestige and years to eradication
x <- seq(0, 100, length = 1000)
y <- seq(0, 50, length = 1000)
y <- y + runif(n = 1000) + sample(seq(-10, 10, length = 1000))
y[sample(1:1000, 400)] <- sample(seq(0, 50, length = 400))
y[y < 0] <- 0
df <- data.frame(quality = x,
                 years = y)
ggplot(data = df,
       aes(x = quality, 
           y = years)) +
  geom_point(alpha = 0.6) +
  geom_smooth() +
  xlab('Number of top-decile disease-related publications') +
  ylab('Perceived years to eradication') +
  ggtitle('Correlation between researcher prestige and perception of years to eradication')

# Obstacles by discipline and disease
disciplines <- 
  c('Biology',
    'Immunology',
    'Anthropology',
    'Pol. Science',
    'Economics',
    'Medicine')

diseases <- 
  c('Leish',
    'HAT',
    'LF',
    'Chagas',
    'Schisto',
    'Malaria')

df <- expand.grid(discipline = disciplines,
                  disease = diseases)
df$p <- sample(1:100,nrow(df))

ggplot(data = df,
       aes(x = discipline, y = p)) +
  geom_bar(stat = 'identity', alpha = 0.6) +
  facet_wrap(~disease) +
  xlab('') +
  ylab('Percent perceiving eradication in < 10 years') +
  ggtitle('Perception of likelihood of eradication by disease/discipline') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.5))

# Obstacles by disease and area
diseases <- 
  c('Leish',
    'HAT',
    'LF',
    'Chagas',
    'Schisto',
    'Malaria')

areas <- 
  c('Resources for research',
    'Resources for public health',
    'Partnerships',
    'Creation/improvement pharm',
    'Creation/improvement vacc',
    'Awareness/education',
    'Healthy systems infr.',
    'Knowledge sharing',
    'Interventions',
    'International coordination')


df <- expand.grid(disease = diseases,
                  area = areas)
df$p <- sample(1:100,nrow(df))

df <- df %>%
  group_by(area) %>%
  mutate(p = p / sum(p) * 100)

ggplot(data = df,
       aes(x = area, y = p)) +
  geom_bar(stat = 'identity', alpha = 0.6) +
  facet_wrap(~disease) +
  xlab('') +
  ylab('Percent pointing to area as most important') +
  ggtitle('Perception of area of greatest importance by disease') +
  theme(axis.text.x = element_text(angle = 65, hjust = 1, vjust = 1))




# Obstacles by discipline and disease and principal area of importance
disciplines <- 
  c('Biology',
    'Immunology',
    'Anthropology',
    'Pol. Science',
    'Economics',
    'Medicine')

diseases <- 
  c('Leish',
    'HAT',
    'LF',
    'Chagas',
    'Schisto',
    'Malaria')

areas <- 
  c('Resources for research',
    'Resources for public health',
    'Partnerships',
    'Creation/improvement pharm',
    'Creation/improvement vacc',
    'Awareness/education',
    'Healthy systems infr.',
    'Knowledge sharing',
    'Interventions',
    'International coordination')

df <- expand.grid(discipline = disciplines,
                  disease = diseases,
                  area = areas)
df$p <- sample(seq(0, 100, length = 10000),nrow(df))

library(dplyr)
library(RColorBrewer)
df <- 
  df %>%
  group_by(discipline, disease) %>%
  mutate(p = p / sum(p) * 100)

ggplot(data = df,
       aes(x = discipline, y = p, group = area, fill = area)) +
  geom_bar(stat = 'identity', alpha = 0.6) +
  facet_wrap(~disease) +
  xlab('') +
  ylab('Percent pointing to area as most important') +
  ggtitle('Perception of area of greatest importance by disease and discipline') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.5)) +
  scale_fill_manual(name = 'Area of priority',
                    values = colorRampPalette(brewer.pal(n = 9, name = 'Spectral'))(length(unique(df$area))))

