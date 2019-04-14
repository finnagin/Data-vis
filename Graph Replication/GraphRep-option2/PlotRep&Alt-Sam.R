# Option 2:


# Environment setting & Import data ---------------------------------------
install.packages("fivethirtyeight")
library(tidyverse)
library(fivethirtyeight)
library(scales)
?comic_characters

Comic <- comic_characters
Comic$align[is.na(Comic$align)] <- "Netural Characters"
Comic$align[Comic$align == "Reformed Criminals"] <- "Good Characters"
Comic$percent <- 1:length(Comic$align)
Comic$percent[Comic$align == "Good Characters"] <- 100*sum(Comic$align == "Good Characters")/length(Comic$align)
Comic$percent[Comic$align == "Netural Characters"] <- 100*sum(Comic$align == "Netural Characters")/length(Comic$align)
Comic$percent[Comic$align == "Bad Characters"] <- 100*sum(Comic$align == "Bad Characters")/length(Comic$align)
Comic$label = paste0(sprintf("%.0f", Comic$percent), "%")

# Replication -------------------------------------------------------------

ggplot(data = Comic %>% filter(sex == "Female Characters" | sex == "Male Characters")) + 
  geom_bar(mapping = aes(x = sex, fill = align), position = "fill", width = 0.3) +
  scale_fill_manual("legend", values = c("Good Characters" = "chartreuse4", 
                                         "Netural Characters" = "orange", 
                                         "Bad Characters" = "red")) +
  facet_wrap(~ publisher, nrow = 2) +
  coord_flip() +
  labs(title = "Good Girls Gone Meh", subtitle = "Character alignment by gender") +
  scale_size(trans = "sqrt", range = c(0.5, 10)) +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "grey95"),
        plot.background = element_rect(fill = "grey95"),
        strip.text.x = element_text(size = 10, hjust = 0,  color = "black", face = "bold"),
        strip.background = element_rect(fill = "grey95"),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.line = element_blank(),
        axis.title = element_blank(),
        legend.position = "top",
        legend.title = element_blank(),
        legend.background = element_rect(fill = "grey95"))
ggsave("replication-sam.pdf")

# Alternative -------------------------------------------------------------
ggplot(data = Comic %>% filter(sex == "Female Characters" | sex == "Male Characters")) + 
  geom_bar(mapping = aes(x = sex, fill = align), position = "dodge", width = 0.3) +
  scale_fill_manual("legend", values = c("Good Characters" = "chartreuse4", 
                                         "Netural Characters" = "orange", 
                                         "Bad Characters" = "red")) +
  facet_wrap(~ publisher, nrow = 2) +
  labs(title = "Good Girls Gone Meh", subtitle = "Character alignment by gender") +
  scale_size(trans = "sqrt", range = c(0.5, 10))
ggsave("alternative-sam.pdf")
