# Data vis week 2 replicate plot group assignment
# replicating, then altering Option 4: The average mean, minimum, and maximum daily temperature in Corvallis based on ten years. (Hint: geom_pointrange)

library("tidyverse")
rm(list = ls())

# reading in via url didn't work with this one
corv_temps <- read_csv("corv_sum.csv")

copy_of <- ggplot(corv_temps, aes(yday, mean)) +
  geom_linerange(aes(ymin = min, ymax = max)) +
  geom_point() +
  labs(x = "Day of Year", y = "Temperature (F)") +
  theme(axis.title.x = element_text(size = 13), 
        axis.title.y = element_text(size = 13))

copy_of
group_6_replicate_corv_temps <- copy_of
ggsave("group_6_replicate_corv_temps.pdf", height=3, width = 8)

# now to alter the graph...

dblue = rgb(8/256,9/256,81/256)
mint = rgb(175/256,234/256,220/256)
sblue = rgb(8/256,30/256,66/256)

alt_graph <- ggplot(corv_temps, aes(yday, mean)) +
  
  geom_line(aes(y=max), color=sblue) +
  geom_line(aes(y=min), color=sblue) +
  geom_ribbon(aes(ymin=min,ymax=max), fill="blue", alpha="0.5") +
  geom_line(size=1, color=sblue) +
  labs(x = "Day of Year", y = "Temperature (F)") +
  theme(axis.title.x = element_text(size = 13), 
        axis.title.y = element_text(size = 13))
alt_graph
group_6_alternative_1_corv_temps <- alt_graph
ggsave("group_6_alternative_1_corv_temps.pdf", height=3, width = 8)

alt_graph2 <- ggplot(corv_temps, aes(yday, mean)) +
  
  geom_line(aes(y=max, colour="red"), size=1) +
  geom_line(aes(y=min, colour=sblue), size=1) +
  #geom_ribbon(aes(ymin=min,ymax=max), fill="grey", alpha="0.5") +
  geom_line(aes(colour="yellow2"), size=1) +
  scale_color_manual(name="" ,values= c(sblue, "red", "yellow2"), labels = c("Min", "Max", "Mean")) +
  labs(x = "Day of Year", y = "Temperature (F)") +
  theme(axis.title.x = element_text(size = 13), 
        axis.title.y = element_text(size = 13))
alt_graph2
group_6_alternative_2_corv_temps <- alt_graph2
ggsave("group_6_alternative_2_corv_temps.pdf", height=3, width = 8)
  

