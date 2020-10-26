# Alvaro y Piotr
# Madrileños X el mundo
# Tablas y gráficos 
# 10/26/20

library(foreign)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(gtools)
library(ggthemes)
library(haven)

rm(list = ls())

madrileños <- read_dta("MadxMundo + libres + recode.dta")
madrileños <- as_factor(madrileños)


genero_g <- madrileños %>%
  filter(genero %in% c("Masculino", "Femenino")) %>%
  ggplot(aes(genero, fill = genero)) +
  geom_bar() +
  theme_excel_new()
genero_g


educacion_g <- madrileños %>%
  filter(!is.na(estudios)) %>%
  ggplot(aes(estudios, fill = estudios)) +
  geom_bar()  +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))  +
  scale_x_discrete(labels = c("Primaria", "Secundaria", "Bachillerato", "Formación profesional", "Grado universitario", "Postgrado", "Doctorado")) +
  ylab("") +
  xlab("") +
  ggtitle("Nivel educativo")
educacion_g 
  

años_fuera_intervalo_g <- madrileños %>%
  filter(!is.na(años_fuera_intervalo)) %>%
  ggplot(aes(años_fuera_intervalo, fill = años_fuera_intervalo)) +
  geom_bar() +
  theme(legend.position = "none") +
  ylab("") +
  xlab("") +
  ggtitle("Años fuera de España") 

años_fuera_intervalo_g 

