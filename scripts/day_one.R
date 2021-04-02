library(tidyverse)
library(ggplot2)
library(cowplot)

tuesdata <- tidytuesdayR::tt_load('2021-03-30')

shades <- tuesdata$allShades

data <- shades %>% 
  mutate(darker_than_me = lightness < 0.54) %>% 
  select(brand, darker_than_me) %>% 
  group_by(brand) %>% 
  count(darker_than_me) %>% 
  mutate(total = sum(n)) %>% 
  filter(darker_than_me == TRUE) %>% 
  mutate(ratio = n/total) %>% 
  arrange(-total) %>% 
  head(20)

data$x <- c(1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5)
data$y <- c(1,1,1,1,1,3,3,3,3,3,5,5,5,5,5,7,7,7,7,7)

plot <- ggplot(data, aes(x = x, y = y)) +
  geom_moon(aes(ratio = ratio), size = 20, fill = "#A98969FF", colour = "#A98969FF") +
  geom_text(aes(y = y-0.8, label = brand), size = 2, colour = "#74430C", face = 'bold', hjust = 0.5) +
  theme_void() + 
  theme(text=element_text(family="Georgia",colour = "#74430C"),
        plot.title = element_text(colour = "#74430C", face = 'bold', size = 12),
        axis.text.x = element_text(colour = "#A98969FF", face = 'bold', size = 2, hjust = 0.5),
        plot.margin = margin(b=10, r=4, l=4,t=4)) +
  ggtitle("What is the proportion of shades that are at least as dark as my own skin colour?\n") +
  labs(caption = "#30DayChartChallenge by @SushGopalan") +
  lims(x = c(0.5, 5.5), y = c(0, 7.5)) +
  theme(plot.background = element_rect(fill="#fffbf5", color = NA))

