#INFORMATION####

#Title: Gotta Plot 'Em All
#Project: PSY6422_Data_Management_&_Visualization
#Student Registration No.: 200256683

#Welcome to Data Analysis and Visualization Project for PSY6422. 

##Data Origins====

#Pokemon data was used for this project and can be found on:
#https://www.kaggle.com/abcsds/pokemon

##Research Question====

#My visualization attempts to find our how Pokemon's strength changed throughout 
#generations (1-6) and if not-legendary Pokemon can defeat legendary ones and 
#what are they

##Pokemon Information====

###generations----

#I Generation - (1996-1999) New Pokemon - 151
#II Generation - (1999-2001) New Pokemon - 100
#III Generation - (2002-2005) New Pokemon - 135
#IV Generation - (2006-2010) New Pokemon - 107
#V Generation - (2010-2013) New Pokemon - 156
#VI Generation - (2013-2016) New Pokemon - 72

###legendary----

#Legendary Pokemon are incredibly rare and often very powerful Pokemon,
#generally featured prominently in the legends and myths of the Pokemon world.

#information source: https://bulbapedia.bulbagarden.net/wiki/Legendary_Pok%C3%A9mon
#and https://bulbapedia.bulbagarden.net/wiki/Generation 

#PROJECT SCRIPT####

##Start====
###install packages----

if(!require("tidyverse"))install.packages("tidyverse") #data package
if(!require("dplyr"))install.packages("dplyr") #data package
if(!require("tidyr"))install.packages("tidyr") #data package
if(!require("ggplot2"))install.packages("ggplot2") #making beautiful graphs
if(!require("tibble"))install.packages("tibble") #wrangling data and making tibbles
if(!require("plotly"))install.packages("plotly") #interactive plots
if(!require("wesanderson"))install.packages("wesanderson") #colour palette
if(!require("gganimate"))install.packages("gganimate") #animating plots
if(!require("htmlwidgets"))install.packages("htmlwidgets") #saving interactive plots

###libraries----

library(tidyverse)
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tibble)
library(plotly)
library(wesanderson)
library(gganimate)
library(htmlwidgets)

###data origins----

data <- read.csv("data/pokemon_data.csv")

class(data)

head(data,10)

#there are 13 variables which are:# 
names(data) #list the names of variables

###tidy data----

#data is tidy as every observation is in one row, each column corresponds to 
#each variable.However, it can be more tidy.

df <- data %>% as_tibble() %>% 
  distinct(X., .keep_all = TRUE) %>% 
  select(Name, Type.1, Total, Generation, Legendary) %>% #selects wanted variables
  dplyr::rename(type = Type.1, #removes capital letters and extra signs
                name = Name, #and gives new names to variables
                total = Total,
                generation = Generation,
                legendary = Legendary) %>%
  arrange(type) %>% #lists Pokemon sorted by their type
  print() #shows how it looks like

df$legendary[df$legendary=="True"] <- "Legendary" #changes value from True to Legendary
df$legendary[df$legendary=="False"] <- "Not Legendary" #changes value from True to Legendary

class(df) #type of data
head(df,10)

## Visualisation====

###firs rainbow pokemon - g1----

g1 <- df %>%
  mutate(text=paste("Name: ",(df$name), "\nType: ",(df$type), "\nTotal: ", (df$total), sep="")) %>% #formats labes boxes
  ggplot(df,mapping = aes(x = total, y=type, col=type, text=text)) +
  geom_point(position = "jitter")+ #spreads out the points
  labs( x="Total strength", #labes of the plot
        y= NULL,
        title="Types of Pokemon")+
  theme_minimal()+ #theme with white background
  theme(axis.text.y=element_blank(), #removes y axis labels
        plot.title = element_text(size=20))+ #formats title
  guides(col=guide_legend("Types of Pokemon")) #legend title

p1 <- ggplotly(g1, tooltip="text") #makes interactive plot
p1

htmlwidgets::saveWidget(p1, file="graphs/rainbow_pokemon.html")

###legendary pokemon - g2 ----

names(wes_palettes) #list all the names of the palettes in Wes Anderson package

g2 <- df %>%
  mutate(text=paste("Name: ",(df$name), "\nType: ",(df$type), "\nTotal: ", 
                    (df$total), "\nGen: ", (df$generation), "\n", 
                    (df$legendary),sep="")) %>%
  ggplot(df,mapping = aes(x = generation, y=total, col=legendary, text=text)) +
  geom_point(position = "jitter")+
  labs( x=NULL,
        y= "Total strength",
        title="Legendary Pokemon")+
  theme_minimal()+
  theme(axis.text.x=element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(size=20),
        panel.grid.major.x = element_blank())+
  guides(col=guide_legend("Types of Pokemon"))+
  scale_colour_manual(values=wes_palette("Moonrise3")) #colours from Moon Rise by Wes Anderson

p2 <- ggplotly(g2, tooltip="text") 
p2 #interactive plot with visible comparison of 
#legendary and nonlegendary Pokemon

htmlwidgets::saveWidget(p2, file="graphs/legendary_pokemon.html")

###all pokemon - g3----

g3 <- df %>%
  mutate(text=paste("Name: ",(df$name), "\nType: ",(df$type), "\nTotal: ", 
                    (df$total), "\nGen: ", (df$generation), "\n", 
                    (df$legendary),sep="")) %>%
  ggplot(df,mapping = aes(x = generation, y=total, col=type, text=text)) +
  geom_point(position = "jitter")+
  labs( x=NULL,
        y= "Total strength",
        title="All Pokemon")+
  theme_minimal()+
  theme(axis.text.x=element_blank(),
        plot.title = element_text(size=20),
        panel.grid.major.y = element_line("#e6f2ff"),
        panel.grid.major.x = element_blank(), 
        axis.line = element_line(colour = "#003166"))+
  guides(col=guide_legend("Types of Pokemon"))+
  scale_color_manual(values = c("Bug"="tan4","Dark"="black","Dragon"="chartreuse4","Electric"="darkgoldenrod1","Fairy"="hotpink",
                                "Fighting"="darksalmon","Fire"="firebrick1","Flying"="lightskyblue","Ghost"="darkblue","Grass"="yellowgreen",
                                "Ground"="peru","Ice"="cadetblue1","Normal"="pink","Poison"="maroon","Psychic"="darkviolet",
                                "Rock"="wheat3","Steel"="gray","Water"="steelblue1")) #assigns colours to each type of Pokemon

p3 <- ggplotly(g3, tooltip="text") 
p3#interactive plot with all Pokemon

htmlwidgets::saveWidget(p3, file="graphs/all_types_interactive.html")

###animated - g4----

g4 <- ggplot(df,mapping = aes(x = generation, y=total, col=type)) +
  geom_point(position = "jitter", size=2)+
  labs(title="Now showing {closest_state} Pokemon", #shows Pokemon type visible 
       subtitle = 'Frame {frame} of {nframes}', #shows number of frames in GIF
       x=NULL,
       y= "Total strength")+
  theme_minimal()+
  theme(axis.text.x=element_blank(),
        axis.title.y = element_text(size=15),
        plot.title = element_text(size=20),
        panel.grid.major.y = element_line("#e6f2ff"),
        axis.line = element_line(colour = "#003166"),
        legend.position = "none")+ #removes legend
  transition_states(type,
                    transition_length = 20,
                    state_length = 10,
                    wrap = FALSE)+
  enter_fade()+ #style of showing new points
  exit_fade()+ #style of disappearing old points
  scale_color_manual(values = c("Bug"="tan4","Dark"="black","Dragon"="chartreuse4","Electric"="darkgoldenrod1","Fairy"="hotpink",
                                "Fighting"="darksalmon","Fire"="firebrick1","Flying"="lightskyblue","Ghost"="darkblue","Grass"="yellowgreen",
                                "Ground"="peru","Ice"="cadetblue1","Normal"="pink","Poison"="maroon","Psychic"="darkviolet",
                                "Rock"="wheat3","Steel"="gray","Water"="steelblue1"))

animate(g4 + ease_aes(),
        width=600, 
        height=600) #animates the plot into the GIF

anim_save(file="graphs/all_types_animated.gif") #saves g4 as GIF

#SUMMARY AND DISCUSSION####

Caveat #1: Type.2 of Popkemon were not look at, which might be interesting to do so in the future.

Caveat #2:  2 new generations (VII[2016-2019] and VIII [2019-2022]) were not included in the data and there are 177 new Pokemon.

Caveat #3:  Comparison of each attributes of Total Score such as HP and Attack might be worth looking at


Caveat #1: Type.2 of Popkemon were not look at, which might be interesting to do so in the future.

Caveat #2:  2 new generations (VII[2016-2019] and VIII [2019-2022]) were not included in the data and there are 177 new Pokemon.

Caveat #3:  Comparison of each attributes of Total Score such as HP and Attack might be worth looking at