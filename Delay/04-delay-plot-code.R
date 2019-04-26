library(ggplot2)
delays <- read.csv("http://vis.cwick.co.nz/data/all-summary.csv")
# see: 04-get-delay-summary.R for details of summarization

delays$DayOfWeek <- factor(delays$DayOfWeek, 
  levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))

# our plot for IAH
iah <- subset(delays, Origin == "IAH")
ggplot(iah, aes(DepHour, DayOfWeek)) +
  geom_tile(aes(fill = prop_over_15)) 

# but now there are also flights from HOU 