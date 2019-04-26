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

# Difference in proportion plot
day_hour_sum %>% 
  group_by(DepHour, DayOfWeek) %>%
  summarise(avg_delay = avg_delay[which(Origin == "HOU")][1]-avg_delay[which(Origin == "IAH")][1],
            avg_delay_delayed = avg_delay_delayed[which(Origin == "HOU")][1]-avg_delay_delayed[which(Origin == "IAH")][1],
            prop_over_15 = prop_over_15[which(Origin == "HOU")][1]-prop_over_15[which(Origin == "IAH")][1],
  ) %>%
  ggplot(aes(DepHour, DayOfWeek)) +
  geom_tile(aes(fill = prop_over_15)) +
  scale_fill_gradient2(low="Red", mid="Yellow", high="Blue", midpoint=0, na.value="Black")
