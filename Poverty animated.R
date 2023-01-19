library(gganimate)
library(ggplot2)
library(gifski)
library(av)
library(dplyr)
install.packages("transformr")
library(transformr)

# importing data

data <- read.csv("poverty.csv")

head(data)

# importing font

# library(showtext)
# font_add(family = "News Gothic Condensed", regular = "News Gothic Condensed Regular.ttf")
# showtext.auto()


# checking if we have all the years
unique(data$Year)



# checking all the states
length(unique(data$Name))

unique(data$Name)

# plotting poverty rate for the entire country


data %>% filter(Name == "United States") %>% ggplot() +
  geom_line(aes(Year, Percent.in.Poverty), size = 1.5, color="#3FA796") +
  scale_x_continuous(limits = c(2011, 2021), breaks = c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021)) +
  ggtitle("Poverty rate in the United States", subtitle = "Between years 2011 - 2021") +
  ylab("Poverty in percentage") +
  theme(plot.background = element_rect(fill="#F5EDDC"),
        panel.background = element_rect(fill = "#F5EDDC"),
        panel.grid.major = element_blank(),
        plot.title = element_text(size = 25, family = "News Gothic Condensed", margin=margin(10,0,10,0)),
        plot.subtitle = element_text(size = 15, family = "News Gothic Condensed", margin=margin(0,0,10,0)),
        axis.text.x = element_text(size = 10, family = "News Gothic Condensed"),
        axis.title.x =  element_text(size = 15, family = "News Gothic Condensed", margin=margin(10,0,10,0)),
        axis.text.y = element_text(size = 10, family = "News Gothic Condensed"),
        axis.title.y =  element_text(size = 15, family = "News Gothic Condensed", margin=margin(0,8,0,11)),
        plot.margin = margin(0,0.5,0,0, "cm"))

# ----------------------------------------------------------

# Checking the minimum and the maximum poverty rates
sel <- data %>% select(Percent.in.Poverty) %>% summary()
sel

# Finding the state with the lowest poverty rate

min <- which(data$Percent.in.Poverty == 7.00)
data$Name[min]


# Finding the state with the highest poverty rate

max <- which(data$Percent.in.Poverty == 23.90)
data$Name[max]



# Plotting result for the highest, lowest poverty rate and the poverty rate for the country

data%>% filter(Name %in% c("Mississippi", "New Hampshire", "United States")) %>% ggplot(aes(Year, Percent.in.Poverty)) +
  geom_line(aes(Year, Percent.in.Poverty, color = Name), size = 1.5) +
  scale_x_continuous(limits = c(2011, 2021), breaks = c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021)) +
  scale_y_continuous(limits = c(7, 24), breaks = c(7, 10, 15, 20, 24)) +
  ggtitle("Poverty rate in the United States", subtitle = "Between years 2011 - 2021") +
  ylab("Poverty in percentage") +
  theme(plot.background = element_rect(fill="#F5EDDC"),
        panel.background = element_rect(fill = "#F5EDDC"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 25, family = "News Gothic Condensed", margin=margin(10,0,10,0)),
        plot.subtitle = element_text(size = 15, family = "News Gothic Condensed", margin=margin(0,0,10,0)),
        axis.text.x = element_text(size = 10, family = "News Gothic Condensed"),
        axis.title.x =  element_text(size = 15, family = "News Gothic Condensed", margin=margin(10,0,10,0)),
        axis.text.y = element_text(size = 10, family = "News Gothic Condensed"),
        axis.title.y =  element_text(size = 15, family = "News Gothic Condensed", margin=margin(0,8,0,11)),
        plot.margin = margin(0,0.1,0,0, "cm"),
        legend.background =element_rect(fill="#F5EEDC"),
        legend.key =element_rect(fill="#F5EEDC")) +
  scale_color_manual(values = c("#ECB390", "#632626", "#1572A1")) +
  labs(color="State", family = "News Gothic Condensed", size = 15)


# creating animated plot for chosen states


states <- c("New York", "California", "District of Columbia", "Florida", "Alaska")

poverty <- data %>% filter(Name %in% states) %>% ggplot() +
  geom_line(aes(Year, Percent.in.Poverty, color = Name), size = 1) +
  scale_x_continuous(limits = c(2011, 2021), breaks = c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021)) +
  scale_y_continuous(limits = c(9, 20), breaks = c(9, 11, 13, 15, 17, 19, 20)) + 
  ggtitle("Poverty rate in the United States", subtitle = "Between years 2011 - 2021") +
  ylab("Poverty in percentage") +
  theme(plot.background = element_rect(fill="#F5EDDC"),
        panel.background = element_rect(fill = "#F5EDDC"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 25, family = "News Gothic Condensed", margin=margin(10,0,10,0)),
        plot.subtitle = element_text(size = 15, family = "News Gothic Condensed", margin=margin(0,0,10,0)),
        axis.text.x = element_text(size = 10, family = "News Gothic Condensed"),
        axis.title.x =  element_text(size = 15, family = "News Gothic Condensed", margin=margin(10,0,10,0)),
        axis.text.y = element_text(size = 10, family = "News Gothic Condensed"),
        axis.title.y =  element_text(size = 15, family = "News Gothic Condensed", margin=margin(0,8,0,11)),
        legend.background =element_rect(fill="#F5EEDC"),
        legend.key =element_rect(fill="#F5EEDC")) +
  scale_color_manual(values = c("#C0D8C0", "#DD4A48","#7897AB", "#D885A3", "#655D8A"))+
  labs(color="State", family = "News Gothic Condensed", size = 15) +
  transition_reveal(Year)

anim_save("poverty.gif", poverty)



# --- facetting all the states ---



plot <- data %>% filter((Name != "United States" & Name != "District of Columbia")) %>% ggplot() +
  geom_line(aes(Year, Percent.in.Poverty, color = Name), size = 1.5) +
  scale_x_continuous(limits = c(2011, 2021), breaks = c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021)) +
  ggtitle("Poverty rate in the United States", subtitle = "Between years 2011 - 2021") +
  ylab("Poverty in percentage") +
  facet_wrap(~Name, nrow=10, ncol=5) +
  theme(plot.background = element_rect(fill="#F5EDDC"),
        panel.background = element_rect(fill = "#F5EDDC"),
        panel.grid.major = element_blank(),
        plot.title = element_text(size = 25, family = "News Gothic Condensed", margin=margin(10,0,10,0)),
        plot.subtitle = element_text(size = 15, family = "News Gothic Condensed", margin=margin(0,0,10,0)),
        axis.text.x = element_blank(),
        axis.title.x =  element_text(size = 15, family = "News Gothic Condensed", margin=margin(10,0,10,0)),
        axis.text.y = element_blank(),
        axis.title.y =  element_text(size = 15, family = "News Gothic Condensed", margin=margin(0,8,0,11)),
        plot.margin = margin(0,0.5,0,0, "cm"),
        legend.position = "none")

ggsave(plot, filename = "test.png", height = 10, width = 5)
