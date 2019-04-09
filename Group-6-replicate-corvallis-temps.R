# Data vis week 2 replicate plot group assignment
# replicating, then altering Option 4: The average mean, minimum, and maximum daily temperature in Corvallis based on ten years. (Hint: geom_pointrange)

library("tidyverse")
rm(list = ls())

# reading in via url didn't work with this one
corv_temps <- read_csv("corv_sum.csv")

copy_of <- ggplot(corv_temps, aes(yday, mean)) +
  geom_point() +
  geom_pointrange(aes(ymin = min, ymax = max)) +
  labs(x = "Day of Year", y = "Temperature (F)") +
  theme(axis.title.x = element_text(size = 20), 
        axis.title.y = element_text(size = 20))

copy_of
group_6_replicate_corv_temps <- copy_of
ggsave("group_6_replicate_corv_temps.pdf")

# now to alter the graph...
