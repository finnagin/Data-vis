library(dplyr)
library(hflights)
library(ggplot2)

hflights_df <- tbl_df(hflights)

hflights_df <- mutate(hflights_df, 
  DepHour = floor(DepTime/100),
  DayOfWeek = factor(DayOfWeek, 
    labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
)

day_hour <- group_by(hflights_df, Origin, DepHour, DayOfWeek)
day_hour_sum <- summarise(day_hour, 
  avg_delay = mean(DepDelay, na.rm = TRUE),
  avg_delay_delayed = mean(DepDelay[DepDelay > 0], 
    na.rm = TRUE),
  prop_over_15 = mean(DepDelay > 15, na.rm = TRUE),
  nflights  = n(),
  ndests = n_distinct(Dest)
)


#write.csv(day_hour_sum, file = "all-summary.csv", 
#  row.names = FALSE)


########################
# Our Code alterations #
########################

# Difference in proportion plot - Finn
# diff_plot1 <- 
# day_hour_sum %>% 
#   group_by(DepHour, DayOfWeek) %>%
#   summarise(avg_delay = avg_delay[which(Origin == "HOU")][1]-avg_delay[which(Origin == "IAH")][1],
#             avg_delay_delayed = avg_delay_delayed[which(Origin == "HOU")][1]-avg_delay_delayed[which(Origin == "IAH")][1],
#             prop_over_15 = (prop_over_15[which(Origin == "HOU")][1]-prop_over_15[which(Origin == "IAH")][1])*100,
#   ) %>%
#   ggplot(aes(DepHour, DayOfWeek)) +
#   geom_tile(aes(fill = prop_over_15)) +
#   scale_fill_gradient2(name = "Percent Difference", low="Red", mid="Yellow", high="Blue", midpoint=0, na.value="Black") +
#   labs(x="Hour", y="Day", title="Difference in percentage of delays over 15 minutes of HOU to IAH")

# Difference in proportion plot (Some polishing) - Sam
# diff_plot2 <- 
# day_hour_sum %>% 
#   group_by(DepHour, DayOfWeek) %>%
#   summarise(avg_delay = avg_delay[which(Origin == "HOU")][1]-avg_delay[which(Origin == "IAH")][1],
#             avg_delay_delayed = avg_delay_delayed[which(Origin == "HOU")][1]-avg_delay_delayed[which(Origin == "IAH")][1],
#             prop_over_15 = (prop_over_15[which(Origin == "HOU")][1]-prop_over_15[which(Origin == "IAH")][1])*100,
#   ) %>%
#   ggplot(aes(DepHour, DayOfWeek)) +
#   geom_tile(aes(fill = prop_over_15)) +
#   scale_fill_gradient2(name = "Percent Difference", low="Red", mid="Yellow", high="Blue", midpoint=0, na.value="white") +
#   labs(x="Hour", y="Day", 
#        title="How different can delays in IAH and HOU be ?",
#        subtitle = "Based on all departing flights from George Bush Intercontinental Airport (IAH) and 
#        William P. Hobby Airport (HOU) in 2011") + 
#   theme_classic() +
#   theme(axis.ticks.y = element_blank(), axis.line.y = element_blank()) + coord_equal()

# Difference in proportion plot (Some more polishing) - Lorne
diff_plot3 <- 
day_hour_sum %>% 
  group_by(DepHour, DayOfWeek) %>%
  summarise(prop_over_15 = (prop_over_15[which(Origin == "HOU")][1]-
                              prop_over_15[which(Origin == "IAH")][1])*100) %>%
  ggplot(aes(DepHour, DayOfWeek)) +
  geom_tile(aes(fill = prop_over_15)) +
  scale_x_continuous("Departure time", 
                     breaks = c(0, 3, 6, 9, 12, 15, 18, 21, 24),
                     labels = c("midnight", "3am", "6am", "9am", "12pm", "3pm", "6pm", "9pm", "midnight"),
                     expand = c(0, 0),
                     limits = c(0, 23)) +
  scale_fill_gradient2(name = "Percent difference \n HOU > IAH", low="Red", mid="Yellow", 
                       high="Blue", midpoint=0, na.value="white") +
  labs(x="Hour", y="Day", 
       title="Flight departure delays greater than 15 minutes out of HOU trounce those of IAH",
       subtitle = "Departing flights from William P. Hobby Airport (HOU) and George Bush Intercontinental Airport (IAH) in 2011") + 
  theme_classic() +
  theme(axis.ticks.y = element_blank(), axis.line.y = element_blank()) + coord_equal()

diff_plot3
ggsave("group6-polished.pdf", height = 3.5, width = 10)
ggsave("group6-polished.jpeg")
