library(tidyverse)
library(here)
library(gtsummary)
library(gglm)

load(here::here('./dragons.RData'))
dragons <- as_tibble(dragons)

dragons |> names()

dragons |> 
  group_by(site, mountainRange) |> 
  tally()

count(dragons, site)

# distribution of testScore
dragons |> 
  ggplot(aes(x=testScore)) + 
  geom_histogram() +
  theme_bw()
# bell shaped distribution

# standardise the explanatory variable
dragons <- dragons |> 
  mutate(bodyLength2 = scale(bodyLength, center = TRUE, scale = TRUE) |> 
           as.vector())

dragons

# fitting all data in a linear model
basic.lm <- dragons |> 
  lm(testScore ~ bodyLength2, data = _)

basic.lm |> 
  tbl_regression() |> 
  add_glance_table()
# for a 1 standard deviation change of bodyLength, testScore increases by 9.0 unit

summary(basic.lm)

# scatter plot of testScore ~ bodyLength
(prelim_plot <- dragons |> 
  ggplot(aes(x = bodyLength, y = testScore)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  labs(title = 'Test score vs Body length',
       y = 'Test score',
       x = 'Body length') +
  theme_bw())
# large dragons on average have higher intelligence

# plotting residual of linear model
basic.lm |> 
  ggplot(aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_smooth() +
  geom_hline(yintercept = 0) +
  theme_bw() +
  labs(title = 'Residual plot',
       y = 'Residual',
       x = 'Fitted values')

# using gglm
gglm(basic.lm)

# box plots of testScore by mountainRange
dragons |> 
  ggplot(aes(x = mountainRange, y = testScore)) +
  geom_boxplot() +
  labs(title = 'testScore ~ mountainRange',
       x = "Mountain range",
       y = "Test score") +
  theme_bw()
# Some mountain ranges have higher test scores compared to others 4 vs 4

# scatterplot of testScore ~ bodyLength colored by mountainRange
dragons |> 
  ggplot(aes(x=bodyLength, y=testScore, col=mountainRange)) +
  geom_point() +
  labs(title = 'testScore vs bodyLength by mountainRange') +
  theme_bw() +
  theme(legend.position = "none")

# facet plot by mountainRange layered original data in background
dragons |> 
  ggplot(aes(x=bodyLength, y=testScore)) +
  geom_point(data=dragons |> select(-mountainRange), 
             aes(x=bodyLength, y=testScore, col="grey80")) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  labs(title = 'testScore vs bodyLength by mountainRange') +
  theme_bw() +
  facet_wrap(~mountainRange) +
  theme(legend.position = "none")

dragons |> 
  group_by(mountainRange, site) |> 
  tally() |> 
  View()

# adding mountain as a fixed effect
mountain.lm <- dragons |> 
  lm(testScore ~ bodyLength2 + mountainRange, data = _)

mountain.lm |> 
  tbl_regression() |> 
  add_glance_table()

# examining mixed effects
library(lme4)
# up to...
# https://ourcodingclub.github.io/tutorials/mixed-models/#what
#   - Further reading fr the keen
