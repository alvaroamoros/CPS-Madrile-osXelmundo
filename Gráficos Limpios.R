# TAbas prubea

# Alvaro y Piotr
# Madrileños X el mundo
# Tablas y gráficos  Limpios
# 10/28/20

library(foreign)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(gtools)
library(ggthemes)
library(haven)
library(ggrepel)
library(scales)


madrileños <- read_dta("MadxMundo + libres + recode.dta")
madrileños <- as_factor(madrileños)


#Edad en intervalos
edad_intervalos_g <- madrileños %>%
  filter(!is.na(edad_intervalos)) %>%
  ggplot(aes(edad_intervalos, fill = edad_intervalos)) +
  geom_bar(aes(y = (..count..)/sum(..count..), fill=factor(..x..)), stat= "count") +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..)),
                y= ((..count..)/sum(..count..))), stat="count",
            hjust = 0.5, size = 3, vjust= -1,
            inherit.aes = TRUE) +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  theme_excel_new() +
  theme(legend.position = "none",
        axis.text=element_text(size=10)) +
  ylab("") +
  xlab("") 

edad_intervalos_g + ggsave("edad_intervalos_g.png")


#Años fuera de España en intervalos
años_fuera_intervalo_g <- madrileños %>%
  filter(!is.na(años_fuera_intervalo)) %>%
  ggplot(aes(años_fuera_intervalo, fill = años_fuera_intervalo)) +
  geom_bar(aes(y = (..count..)/sum(..count..), fill=factor(..x..)), stat= "count") +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..)),
                y= ((..count..)/sum(..count..))), stat="count",
            hjust = 0.5, size = 3, vjust= -1,
            inherit.aes = TRUE) +
  theme_excel_new() +
  theme(legend.position = "none",
        axis.title=element_text(size=10, face="bold")) +
  ylab("") +
  xlab("") + 
  scale_x_discrete(labels = c("< 1","1-2","2-3","3-4","4-5","5-10","10-15","15-20","+20")) 

años_fuera_intervalo_g + ggsave("años_fuera_intervalo_g.png")


# Había estado anteriormente en su país de residencia
antes_pais_g <- madrileños %>%
  filter(!is.na(antes_pais)) %>%
  ggplot(aes(antes_pais, fill = antes_pais)) +
  geom_bar(aes(y = (..count..)/sum(..count..), fill=factor(..x..)), stat= "count") +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..)),
                y= ((..count..)/sum(..count..))), stat="count",
            hjust = 0.5, size = 3, vjust= -1,
            inherit.aes = TRUE) +
  theme_excel_new() +
  theme(legend.position = "none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold"),
        plot.title = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("") +
  xlab("")

antes_pais_g + ggsave
  


  
  
  