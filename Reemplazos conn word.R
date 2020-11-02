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
library(RColorBrewer)
library(viridis) 
library(comma_format)
library(ggpubr)


display.brewer.all()
scale_fill_brewer(palette = "YlGnBu")

madrileños <- read_dta("MAdxMundo + Libres + Actualizado intervalos fuera.dta")
madrileños <- as_factor(madrileños)


comma2 <- function(x, ...) {
  format(x, decimal.mark = ",", trim = TRUE, scientific = FALSE, ...)
}

edad_intervalos_g + scale_y_continuous(formatter=comma2)

point <- format_format(big.mark = ".", decimal.mark = ",", scientific = FALSE)





#Edad en intervalos
edad_intervalos_g <- madrileños %>%
  filter(!is.na(edad_intervalos)) %>%
  ggplot(aes(edad_intervalos)) +
  geom_bar(aes(y = (..count..)/sum(..count..), fill=factor(..x..)), stat= "count") +
  scale_fill_brewer(palette="GnBu", aesthetics = "fill") +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..), accuracy = 1),
                y= ((..count..)/sum(..count..))), stat="count",
            hjust = 0.5, size = 3, vjust= -1,
            inherit.aes = TRUE) +
  scale_y_continuous(labels=scales::percent) +
  theme_hc() +
  theme(legend.position = "none",
        axis.text=element_text(size=10)) +
  ylab("") +
  xlab("")
                                         
edad_intervalos_g + ggsave("edad_intervalos_g.png")

#Años fuera de España en intervalos
años_fuera_intervalo_g <- madrileños %>%
  filter(!is.na(años_fuera_intervalo)) %>%
  ggplot(aes(años_fuera_intervalo)) +
  geom_bar(aes(y = (..count..)/sum(..count..), fill=factor(..x..)), stat= "count") +
  scale_fill_brewer(palette="GnBu", aesthetics = "fill") +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..), accuracy = 1),
                y= ((..count..)/sum(..count..))), stat="count",
            hjust = 0.5, size = 3, vjust= -0.5,
            inherit.aes = TRUE) +
  scale_y_continuous(labels=scales::percent) + 
  theme_hc() +
  theme(legend.position = "none",
        axis.title=element_text(size=10, face="bold")) +
  ylab("") +
  xlab("") + 
  scale_x_discrete(labels = c("< 1","1-2","2-3","3-4","4-5","5-10","10-15","15-20","+20")) 

años_fuera_intervalo_g + ggsave("años_fuera_intervalo_g.png")


# Nacionalidad española
nacionalidad_g <- madrileños %>%
  filter(!is.na(nacionalidad)) %>%
  ggplot(aes(nacionalidad)) +
  geom_bar(aes(y = (..count..)/sum(..count..), fill=factor(..x..)), stat= "count") +
  scale_fill_brewer(palette="GnBu", aesthetics = "fill") +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..), accuracy = 1),
                y= ((..count..)/sum(..count..))), stat="count",
            hjust = 0.5, size = 3, vjust= -1,
            inherit.aes = TRUE) +  
  theme(legend.position = "none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold"),
        plot.title = element_text(size = 10, face = "bold")) +
  scale_y_continuous(labels=scales::percent) +
  xlab("") +
  ylab("") +
  scale_x_discrete(labels = c("Sí","No")) 

nacionalidad_g + ggsave("nacionalidad_g.png")

# Había estado anteriormente en su país de residencia
antes_pais_g <- madrileños %>%
  filter(!is.na(antes_pais)) %>%
  ggplot(aes(antes_pais)) +
  scale_fill_brewer(palette="GnBu", aesthetics = "fill", labels = c("De turismo", "Motivos laborales", "Visitando a familiares", "De erasmus",
                                                                    "Otra beca", "Vivido anteriormente", "No", "Otro") ) +
  geom_bar(aes(y = (..count..)/sum(..count..), fill=factor(..x..)), stat= "count") +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..), accuracy = 1),
                y= ((..count..)/sum(..count..))), stat="count",
            hjust = 0.5, size = 3, vjust= -1,
            inherit.aes = TRUE) +
  scale_y_continuous(labels=scales::percent) +
  theme_hc() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x=element_blank()) +
  ylab("") +
  xlab("")

antes_pais_g + guides(fill=guide_legend(nrow=3,byrow=TRUE, label.hjust = -0.1)) +ggsave("antes_pais_g.png")



# Dificultades con el idioma en el país de destino
dific_idioma_g <- madrileños %>%
  filter(!is.na(dific_idioma)) %>%
  ggplot(aes(dific_idioma, fill = dific_idioma)) +
  geom_bar(aes(y = (..count..)/sum(..count..), fill=factor(..x..)), stat= "count") +
  scale_fill_brewer(palette="GnBu", aesthetics = "fill")+
  geom_text(aes(label = scales::percent((..count..)/sum(..count..), accuracy = 1),
                y= ((..count..)/sum(..count..))), stat="count",
            hjust = 0.5, size = 3, vjust= -1,
            inherit.aes = TRUE) +
  scale_y_continuous(labels=scales::percent) +
  theme_hc() +  
  theme(legend.position = "none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold"),
        plot.title = element_text(size = 10, face = "bold")) +
  ylab("") +
  xlab("")


dific_idioma_g+ ggsave("dific_idioma_g.png")

# Dificultades financieras al mudarse al país de destino
dific_recursos_g <- madrileños %>%
  filter(!is.na(dific_recursos)) %>%
  ggplot(aes(dific_recursos, fill = dific_recursos)) +
  geom_bar(aes(y = (..count..)/sum(..count..), fill=factor(..x..)), stat= "count", fill = "paleturquoise3") +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..), accuracy = 1),
                y= ((..count..)/sum(..count..))), stat="count",
            hjust = 0.5, size = 3, vjust= -1,
            inherit.aes = TRUE) +
  scale_y_continuous(labels=scales::percent) +
  theme_hc() +
  theme(legend.position = "none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold"),
        plot.title = element_text(size = 10, face = "bold")) +
  ylab("") +
  xlab("")

dific_recursos_g + ggsave("dific_recursos_g.png")


# Dificultades con el idioma en país de destino
dific_idioma_g <- madrileños %>%
  filter(!is.na(dific_idioma)) %>%
  ggplot(aes(dific_idioma, fill = dific_idioma)) +
  geom_bar(aes(y = (..count..)/sum(..count..), fill=factor(..x..)), stat= "count") +
  scale_fill_brewer(palette="GnBu", aesthetics = "fill") +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..), accuracy = 1),
                y= ((..count..)/sum(..count..))), stat="count",
            hjust = 0.5, size = 3, vjust= -1,
            inherit.aes = TRUE) +
  scale_y_continuous(labels=scales::percent) +
  theme_hc() +
  theme(legend.position = "none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold"),
        plot.title = element_text(size = 10, face = "bold")) +
  ylab("") +
  xlab("")

dific_idioma_g + ggsave("dific_idioma_g.png")

# Dificultades con el alojamiento en el país de destino
dific_alojamiento_g <- madrileños %>%
  filter(!is.na(dific_alojamiento)) %>%
  ggplot(aes(dific_alojamiento, fill = dific_alojamiento)) +
  geom_bar(aes(y = (..count..)/sum(..count..), fill=factor(..x..)), stat= "count", fill = "paleturquoise3") +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..), accuracy = 1),
                y= ((..count..)/sum(..count..))), stat="count",
            hjust = 0.5, size = 3, vjust= -1,
            inherit.aes = TRUE) +
  scale_y_continuous(labels=scales::percent) +
  theme_hc() +
  theme(legend.position = "none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold"),
        plot.title = element_text(size = 10, face = "bold")) +  
  ylab("") +
  xlab("")

dific_alojamiento_g + ggsave("dific_alojamiento_g.png")

# Dificultades laborales en el país de destino
dific_laborales_g <- madrileños %>%
  filter(!is.na(dific_laborales)) %>%
  ggplot(aes(dific_laborales, fill = dific_laborales)) +
  scale_fill_brewer(palette="GnBu", aesthetics = "fill") +
  geom_bar(aes(y = (..count..)/sum(..count..), fill=factor(..x..)), stat= "count") +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..), accuracy = 1),
                y= ((..count..)/sum(..count..))), stat="count",
            hjust = 0.5, size = 3, vjust= -1,
            inherit.aes = TRUE) +
  scale_y_continuous(labels=scales::percent) +
  theme_hc() +
  theme(legend.position = "none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold"),
        plot.title = element_text(size = 10, face = "bold")) +
  ylab("") +
  xlab("")
  
  dific_laborales_g + ggsave("dific_laborales_g.png")


# Dificultades conocimiento trámites administrativos en el país de destino
dific_tramites_g <- madrileños %>%
  filter(!is.na(dific_tramites)) %>%
  ggplot(aes(dific_tramites)) +
  scale_fill_brewer(palette="GnBu", aesthetics = "fill") +
  geom_bar(aes(y = (..count..)/sum(..count..), fill=factor(..x..)), stat= "count") +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..), accuracy = 1),
                y= ((..count..)/sum(..count..))), stat="count",
            hjust = 0.5, size = 3, vjust= -1,
            inherit.aes = TRUE) +
  scale_y_continuous(labels=scales::percent) +
  theme_hc() +
  theme(legend.position = "none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold"), 
        plot.title = element_text(size = 10, face = "bold")) +
  ylab("") +
  xlab("")

dific_tramites_g + ggsave("dific_tramites_g.png")

# Dificultades conocimiento questiones sanitarias en el país de destino
dific_sanidad_g <- madrileños %>%
  filter(!is.na(dific_sanidad)) %>%
  ggplot(aes(dific_sanidad)) +
  scale_fill_brewer(palette="GnBu", aesthetics = "fill") +
  geom_bar(aes(y = (..count..)/sum(..count..), fill=factor(..x..)), stat= "count") +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..), accuracy = 1),
                y= ((..count..)/sum(..count..))), stat="count",
            hjust = 0.5, size = 3, vjust= -1,
            inherit.aes = TRUE) +
  scale_y_continuous(labels=scales::percent) +
  theme_hc() +
  theme(legend.position = "none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold"),
        plot.title = element_text(size = 10, face = "bold")) +
  ylab("") +
  xlab("")

dific_sanidad_g + ggsave("dific_sanidad_g.png")

#Dificultades para integrarse en la  sociedad en el país de destino sociedad en el país de destino
dific_integracion_g <- madrileños %>%
  filter(!is.na(dific_integracion)) %>%
  ggplot(aes(dific_integracion)) +
  scale_fill_brewer(palette="GnBu", aesthetics = "fill") +
  geom_bar(aes(y = (..count..)/sum(..count..), fill=factor(..x..)), stat= "count") +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..), accuracy = 1),
                y= ((..count..)/sum(..count..))), stat="count",
            hjust = 0.5, size = 3, vjust= -1,
            inherit.aes = TRUE) +
  scale_y_continuous(labels=scales::percent) +
  theme_hc() +
  theme(legend.position = "none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold"),
        plot.title = element_text(size = 10, face = "bold")) +
  ylab("") +
  xlab("")

dific_integracion_g + ggsave("dific_integracion_g.png")


# Dificultades psicológicas en el país de destino
dific_psico_g <- madrileños %>%
  filter(!is.na(dific_psico)) %>%
  ggplot(aes(dific_psico)) +
  scale_fill_brewer(palette="GnBu", aesthetics = "fill") +
  geom_bar(aes(y = (..count..)/sum(..count..), fill=factor(..x..)), stat= "count") +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..), accuracy = 1),
                y= ((..count..)/sum(..count..))), stat="count",
            hjust = 0.5, size = 3, vjust= -1,
            inherit.aes = TRUE) +
  scale_y_continuous(labels=scales::percent) +
  theme_hc() +
  theme(legend.position = "none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold"),
        plot.title = element_text(size = 10, face = "bold")) +
  ylab("") +
  xlab("")

dific_psico_g + ggsave("dific_psico_g.png")  


# Con que frecuencia viaja a Madrid
viajes_madrid_g <- madrileños %>%
  filter(!is.na(viajes_madrid)) %>%
  ggplot(aes(viajes_madrid)) +
  scale_fill_brewer(palette="GnBu", aesthetics = "fill", labels = c("Nunca", "Menos de una vez al año", "Una vez al año",
                                                                    "Varias veces al año", "Mensualmente o más") ) +
  geom_bar(aes(y = (..count..)/sum(..count..), fill=factor(..x..)), stat= "count") +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..), accuracy = 1),
                y= ((..count..)/sum(..count..))), stat="count",
            hjust = 0.5, size = 3, vjust= -1,
            inherit.aes = TRUE) +
  scale_y_continuous(labels=scales::percent) +
  theme_hc() +  
  theme(legend.position = "right",
        legend.title = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x=element_blank()) +
  ylab("") +
  xlab("")

viajes_madrid_g + ggsave("viajes_madrid_g.png")

# Con que frecuencia tiene contacto con gente de Madrid
frecuencia_gente_mad_g <- madrileños %>%
  filter(!is.na(frecuencia_gente_mad)) %>%
  ggplot(aes(frecuencia_gente_mad)) +
  scale_fill_brewer(palette="GnBu", aesthetics = "fill", labels = c("Nunca", "Menos de una vez al mes", "Mensualmente",
                                                                    "Semanalmente", "Diariamente") ) +
  geom_bar(aes(y = (..count..)/sum(..count..), fill=factor(..x..)), stat= "count") +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..), accuracy = 1),
                y= ((..count..)/sum(..count..))), stat="count",
            hjust = 0.5, size = 3, vjust= -1,
            inherit.aes = TRUE) +
  scale_y_continuous(labels=scales::percent) +
  theme_hc() +    
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x=element_blank()) +
  xlab("") +
  ylab("")

frecuencia_gente_mad_g + guides(fill=guide_legend(nrow=2,byrow=TRUE, label.hjust = -0.1)) +  ggsave("frecuencia_gente_mad_g.png")

# Grafico de barras con el % apilado si/no informa_esp miembro_aso manifestaciones reuniones (quitando ns/nc en los que queda) 
#####Faltan Porcentajes##########

informa_esp_g <- madrileños %>%
  filter(!is.na(informa_esp)) %>%
  ggplot(aes(informa_esp)) +
  scale_fill_brewer(palette="GnBu", aesthetics = "fill", labels = c("Nunca", "Menos de una vez al mes", "Mensualmente",
                                                                    "Semanalmente", "Diariamente") ) +
  geom_bar(aes(y = (..count..)/sum(..count..), fill=factor(..x..)), stat= "count") +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..), accuracy = 1),
                y= ((..count..)/sum(..count..))), stat="count",
            hjust = 0.5, size = 3, vjust= -0.3,
            inherit.aes = TRUE) +
  scale_y_continuous(labels=scales::percent) +
  theme_hc() + theme(legend.position = "bottom",
                     legend.title = element_blank(),
                     axis.text.x = element_blank(),
                     axis.ticks.x=element_blank()) +
  ylab("") +
  xlab("")

informa_esp_g + guides(fill=guide_legend(nrow=2,byrow=TRUE, label.hjust = -0.1)) +  ggsave("informa_esp_g.png")


"miembro_aso_g <- madrileños %>%
  filter(!is.na(miembro_aso)) %>%
  filter(miembro_aso %in% c("Sí","No")) %>%
  ggplot(aes(miembro_aso)) +
  scale_fill_brewer(palette="Blues", aesthetics = "fill") +
  geom_bar(aes(y = (..count..)/sum(..count..), fill=factor(..x..)), stat= "count") +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..), accuracy = 1),
                y= ((..count..)/sum(..count..))), stat="count",
            hjust = 0.5, size = 3, vjust= -0.3,
            inherit.aes = TRUE) +
  scale_y_continuous(labels=scales::percent) +
  theme_hc() + theme(legend.position = "none",
                     axis.text=element_text(size=10),
                     axis.title=element_text(size=10,face="bold"),
                     plot.title = element_text(size = 10, face = "bold")) +
  ylab("") +
  xlab("")

miembro_aso_g + ggsave("miembro_aso_g.png")

manifestaciones_g <- madrileños %>%
  filter(!is.na(manifestaciones)) %>%
  filter(manifestaciones %in% c("Sí","No")) %>%
  ggplot(aes(manifestaciones)) +
  scale_fill_brewer(palette="Blues", aesthetics = "fill") +
  geom_bar(aes(y = (..count..)/sum(..count..), fill=factor(..x..)), stat= "count") +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..), accuracy = 1),
                y= ((..count..)/sum(..count..))), stat="count",
            hjust = 0.5, size = 3, vjust= -0.3,
            inherit.aes = TRUE) +
  scale_y_continuous(labels=scales::percent) +
  theme_hc() + theme(legend.position = "none",
                     axis.text=element_text(size=10),
                     axis.title=element_text(size=10,face="bold"),
                     plot.title = element_text(size = 10, face = "bold")) +
  ylab("") +
  xlab("")

manifestaciones_g + ggsave("manifestaciones_g.png")

reuniones_g <- madrileños %>%
  filter(!is.na(reuniones)) %>%
  filter(reuniones %in% c("Sí","No")) %>%
  ggplot(aes(reuniones)) +
  scale_fill_brewer(palette="Blues", aesthetics = "fill") +
  geom_bar(aes(y = (..count..)/sum(..count..), fill=factor(..x..)), stat= "count") +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..), accuracy = 1),
                y= ((..count..)/sum(..count..))), stat="count",
            hjust = 0.5, size = 3, vjust= -0.3,
            inherit.aes = TRUE) +
  scale_y_continuous(labels=scales::percent) +
  theme_hc() + theme(legend.position = "none",
                     axis.text=element_text(size=10),
                     axis.title=element_text(size=10,face="bold"),
                     plot.title = element_text(size = 10, face = "bold")) +
  ylab("") +
  xlab("")

reuniones_g + ggsave("reuniones_g.png")

ggarrange(reuniones_g, manifestaciones_g)"

# Ha participado en las Elecciones generalesde España de noviembre 2019?
"elecciones_generales_esp_g <- madrileños %>%
  filter(!is.na(elecciones_generales_esp)) %>%
  ggplot(aes(elecciones_generales_esp, fill = elecciones_generales_esp)) +
  geom_bar(aes(y = (..count..)/sum(..count..), fill=factor(..x..)), stat= "count", fill = "paleturquoise3") +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..), accuracy = 1),
                y= ((..count..)/sum(..count..))), stat="count",
            hjust = 0.5, size = 3, vjust= -1,
            inherit.aes = TRUE) +
  scale_y_continuous(labels=scales::percent) +
  theme_hc() +      theme(legend.position = "none",
                          axis.text=element_text(size=10),
                          axis.title=element_text(size=10,face="bold"),
                          plot.title = element_text(size = 10, face = "bold"),
                          axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("") +
  ylab("")

elecciones_generales_esp_g + ggsave("elecciones_generales_esp_g.png")

# grafico de barras (%) elecciones_autonomicas_esp 
elecciones_autonomicas_esp_g <- madrileños %>%
  filter(!is.na(elecciones_autonomicas_esp)) %>%
  ggplot(aes(elecciones_autonomicas_esp, fill = elecciones_autonomicas_esp)) +
  geom_bar(aes(y = (..count..)/sum(..count..), fill=factor(..x..)), stat= "count", fill = "paleturquoise3") +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..), accuracy = 1),
                y= ((..count..)/sum(..count..))), stat="count",
            hjust = 0.5, size = 3, vjust= -0.5,
            inherit.aes = TRUE) +
  scale_y_continuous(labels=scales::percent) +
  theme_hc() +    
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold")) +
  xlab("") +
  ylab("")

elecciones_autonomicas_esp_g + ggsave("elecciones_autonomicas_esp_g.png")


# grafico de barras (%) elecciones_municipales_esp 
elecciones_municipales_esp_g <- madrileños %>%
  filter(!is.na(elecciones_municipales_esp)) %>%
  ggplot(aes(elecciones_municipales_esp, fill = elecciones_municipales_esp)) +
  geom_bar(aes(y = (..count..)/sum(..count..), fill=factor(..x..)), stat= "count", fill = "paleturquoise3") +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..), accuracy = 1),
                y= ((..count..)/sum(..count..))), stat="count",
            hjust = 0.5, size = 3, vjust= -0.5,
            inherit.aes = TRUE) +
  scale_y_continuous(labels=scales::percent) +
  theme_hc() +     theme(legend.position = "none",
                         axis.text.x = element_text(angle = 45, hjust = 1),
                         axis.text=element_text(size=10),
                         axis.title=element_text(size=10, face="bold")) +
  ylab("") +
  xlab("")

ggsave("elecciones_municipales_esp_g.png")

# grafico de barras (%) elecciones_municipales_reside 
elecciones_municipales_reside_g <- madrileños %>%
  filter(!is.na(elecciones_municipales_reside)) %>%
  ggplot(aes(elecciones_municipales_reside, fill = elecciones_municipales_reside)) +
  geom_bar(aes(y = (..count..)/sum(..count..), fill=factor(..x..)), stat= "count", fill = "paleturquoise3") +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..), accuracy = 1),
                y= ((..count..)/sum(..count..))), stat="count",
            hjust = 0.5, size = 3, vjust= -0.5,
            inherit.aes = TRUE) +
  scale_y_continuous(labels=scales::percent) +
  theme_hc() +     theme(legend.position = "none",
                         axis.text.x = element_text(angle = 45, hjust = 1),
                         axis.text=element_text(size=10),
                         axis.title=element_text(size=10,face="bold")) +
  ylab("") +
  xlab("")

elecciones_municipales_reside_g + ggsave("elecciones_municipales_reside_g.png")

# grafico de barras (%) elecciones_europeas_españoles 
elecciones_europeas_españoles_g <- madrileños %>%
  filter(!is.na(elecciones_europeas_españoles)) %>%
  ggplot(aes(elecciones_europeas_españoles, fill = elecciones_europeas_españoles)) +
  geom_bar(aes(y = (..count..)/sum(..count..), fill=factor(..x..)), stat= "count", fill = "paleturquoise3") +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..), accuracy = 1),
                y= ((..count..)/sum(..count..))), stat="count",
            hjust = 0.5, size = 3, vjust= -0.5,
            inherit.aes = TRUE) +
  scale_y_continuous(labels=scales::percent) +
  theme_hc() +     theme(legend.position = "none",
                         axis.text.x = element_text(angle = 45, hjust = 1),
                         axis.text=element_text(size=10),
                         axis.title=element_text(size=10,face="bold")) +
  ylab("") +
  xlab("")

elecciones_europeas_españoles_g + ggsave("elecciones_europeas_españoles_g.png")

# grafico de barras (%) elecciones_europeas_extranjeros
elecciones_europeas_extranjeros_g <- madrileños %>%
  filter(!is.na(elecciones_europeas_extranjeros)) %>%
  ggplot(aes(elecciones_europeas_extranjeros, fill = elecciones_europeas_extranjeros)) +
  geom_bar(aes(y = (..count..)/sum(..count..), fill=factor(..x..)), stat= "count", fill = "paleturquoise3") +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..), accuracy = 1),
                y= ((..count..)/sum(..count..))), stat="count",
            hjust = 0.5, size = 3, vjust= -0.5,
            inherit.aes = TRUE) +
  scale_y_continuous(labels=scales::percent) +
  theme_hc() +     
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold")) +
  ylab("") +
  xlab("")

elecciones_europeas_extranjeros_g + ggsave("elecciones_europeas_extranjeros_g.png")



# Grafico de barras con el % apilado con las 4 categorias para conoce_bolsa conoce_ayudas conoce_asociaciones conoce_consejo conoce_portal conoce_info_salud conoce_info_padron
conoce_bolsa_g <- madrileños %>%
  filter(!is.na(conoce_bolsa)) %>%
  ggplot(aes(conoce_bolsa, fill = conoce_bolsa)) +
  geom_bar(aes(y = (..count..)/sum(..count..), fill=factor(..x..)), stat= "count", fill = "paleturquoise3") +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..), accuracy = 1),
                y= ((..count..)/sum(..count..))), stat="count",
            hjust = 0.5, size = 3, vjust= -0.5,
            inherit.aes = TRUE) +
  scale_y_continuous(labels=scales::percent) +
  theme_hc() +       theme(legend.position = "none",
                           axis.text.x = element_text(angle = 45, hjust = 1),
                           axis.text=element_text(size=10),
                           axis.title=element_text(size=10,face="bold")) +
  ylab("") +
  xlab("")"

"conoce_bolsa_g + ggsave("conoce_bolsa_g.png")

conoce_ayudas_g <- madrileños %>%
  filter(!is.na(conoce_ayudas)) %>%
  ggplot(aes(conoce_ayudas, fill = conoce_ayudas)) +
  geom_bar(aes(y = (..count..)/sum(..count..), fill=factor(..x..)), stat= "count", fill = "paleturquoise3") +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..), accuracy = 1),
                y= ((..count..)/sum(..count..))), stat="count",
            hjust = 0.5, size = 3, vjust= -0.5,
            inherit.aes = TRUE) +
  scale_y_continuous(labels=scales::percent) +
  theme_hc() +       theme(legend.position = "none",
                           axis.text.x = element_text(angle = 45, hjust = 1),
                           axis.text=element_text(size=10),
                           axis.title=element_text(size=10,face="bold")) +
  ylab("") +
  xlab("")

conoce_ayudas_g + ggsave("conoce_ayudas_g.png")

conoce_asociaciones_g <- madrileños %>%
  filter(!is.na(conoce_asociaciones)) %>%
  ggplot(aes(conoce_asociaciones, fill = conoce_asociaciones)) +
  geom_bar(aes(y = (..count..)/sum(..count..), fill=factor(..x..)), stat= "count", fill = "paleturquoise3") +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..), accuracy = 1),
                y= ((..count..)/sum(..count..))), stat="count",
            hjust = 0.5, size = 3, vjust= -0.5,
            inherit.aes = TRUE) +
  scale_y_continuous(labels=scales::percent) +
  theme_hc() +       theme(legend.position = "none",
                           axis.text.x = element_text(angle = 45, hjust = 1),
                           axis.text=element_text(size=10),
                           axis.title=element_text(size=10,face="bold")) +
  ylab("") +
  xlab("")

conoce_asociaciones_g + ggsave("conoce_asociaciones_g.png")

conoce_consejo_g <- madrileños %>%
  filter(!is.na(conoce_consejo)) %>%
  ggplot(aes(conoce_consejo, fill = conoce_consejo)) +
  geom_bar(aes(y = (..count..)/sum(..count..), fill=factor(..x..)), stat= "count", fill = "paleturquoise3") +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..), accuracy = 1),
                y= ((..count..)/sum(..count..))), stat="count",
            hjust = 0.5, size = 3, vjust= -0.5,
            inherit.aes = TRUE) +
  scale_y_continuous(labels=scales::percent) +
  theme_hc() +       theme(legend.position = "none",
                           axis.text.x = element_text(angle = 45, hjust = 1),
                           axis.text=element_text(size=10),
                           axis.title=element_text(size=10,face="bold")) +
  ylab("") +
  xlab("")

conoce_consejo_g + ggsave("conoce_consejo.png")

conoce_info_salud_g <- madrileños %>%
  filter(!is.na(conoce_info_salud)) %>%
  ggplot(aes(conoce_info_salud, fill = conoce_info_salud)) +
  geom_bar(aes(y = (..count..)/sum(..count..), fill=factor(..x..)), stat= "count", fill = "paleturquoise3") +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..), accuracy = 1),
                y= ((..count..)/sum(..count..))), stat="count",
            hjust = 0.5, size = 3, vjust= -0.5,
            inherit.aes = TRUE) +
  scale_y_continuous(labels=scales::percent) +
  theme_hc() +       theme(legend.position = "none",
                           axis.text.x = element_text(angle = 45, hjust = 1),
                           axis.text=element_text(size=10),
                           axis.title=element_text(size=10,face="bold")) +
  ylab("") +
  xlab("")

conoce_info_salud_g + ggsave("conoce_info_salud_g.png")

conoce_info_padron_g <- madrileños %>%
  filter(!is.na(conoce_info_padron)) %>%
  ggplot(aes(conoce_info_padron, fill = conoce_info_padron)) +
  geom_bar(aes(y = (..count..)/sum(..count..), fill=factor(..x..)), stat= "count", fill = "paleturquoise3") +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..), accuracy = 1),
                y= ((..count..)/sum(..count..))), stat="count",
            hjust = 0.5, size = 3, vjust= -0.5,
            inherit.aes = TRUE) +
  scale_y_continuous(labels=scales::percent) +
  theme_hc() +      
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold")) +
  ylab("") +
  xlab("")

conoce_info_padron_g + ggsave("conoce_info_padron_g.png")"


# Grafico de barras con el % de registrado_consulado
registrado_consulado_g <- madrileños %>%
  filter(!is.na(registrado_consulado)) %>%
  filter(registrado_consulado %in% c("Sí, me he inscrito como residente (estancia permanente)",
                                     "Sí, me he inscrito como no residente (estancia temporal)",
                                     "No")) %>%
  ggplot(aes(registrado_consulado)) +
  scale_fill_brewer(palette="GnBu", aesthetics = "fill", labels = c("Sí, como residente", "Sí, como no residente", "No")) +
  geom_bar(aes(y = (..count..)/sum(..count..), fill=factor(..x..)), stat= "count") +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..), accuracy = 1),
                y= ((..count..)/sum(..count..))), stat="count",
            hjust = 0.5, size = 3, vjust= -0.5,
            inherit.aes = TRUE) +
  scale_y_continuous(labels=scales::percent) +
  theme_hc() +    theme(legend.position = "bottom",
                        legend.title = element_blank(),
                        axis.text.x = element_blank(),
                        axis.ticks.x=element_blank()) +
  ylab("") +
  xlab("")

registrado_consulado_g + guides(fill=guide_legend(nrow=2,byrow=TRUE, label.hjust = )) + ggsave("registrado_consulado_g.png")

# Grafico de barras con el % apilado si/no registro_votar registro_documentacion registro_contacto registro_prevencion registro_certificado registro_franquicia registro_transacciones registro_iva
# Define the number of colors you want
nb.cols <- 12
mycolors <- colorRampPalette(brewer.pal(8, "GnBu"))(nb.cols)

cnt <- plyr::count(madrileños$motivo_registro)
madrileños$motivo_registro <- factor(madrileños$motivo_registro,
                                     levels = cnt$x[order(cnt$freq, decreasing = TRUE)])

motivo_registro_g <- madrileños %>%
  filter(!is.na(motivo_registro)) %>%
  filter(!motivo_registro %in% c("NS/NC")) %>%
  ggplot(aes(x = motivo_registro)) +
  geom_bar(aes(y = (..count..)/sum(..count..), fill=factor(..x..)), stat= "count") +
  scale_fill_manual(values = mycolors, aesthetics = "fill", labels = c("Votar en España", "Renovar mi documentación", " Localizable durante COVID-19","Catástrofes / emergencias", "Certificado de emigrante retornado",
                                                                       "Franquicia aduanera", "Transacciones / cuentas bancarias", "Excención IVA", "Nacionalización / registro hijos", "Casarse", "Obligación legal / responsabilidad", "Otros") ) +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..), accuracy = 1),
                y= ((..count..)/sum(..count..))), stat="count",
            hjust = 0.5, size = 3, vjust= -0.5,
            inherit.aes = TRUE) +
  scale_y_continuous(labels=scales::percent) +
  theme_hc() +    theme(legend.position = "bottom",
                        legend.title = element_blank(),
                        axis.text.x = element_blank(),
                        axis.ticks.x=element_blank()) +
  ylab("") +
  xlab("")

motivo_registro_g + guides(fill=guide_legend(nrow=6,byrow=TRUE, label.hjust = -0.03 )) + ggsave("motivo_registro_g.png") 

# Grafico de barras con el % apilado si/no no_registro_conocia no_registro_sabia no_registro_tramites no_registro_padron no_registro_covid no_registro_admin no_registro_aporta


cnt <- plyr::count(madrileños$motivo_no_registro)
madrileños$motivo_no_registro <- factor(madrileños$motivo_no_registro,
                                     levels = cnt$x[order(cnt$freq, decreasing = TRUE)])

motivo_no_registro_g <- madrileños %>%
  filter(!is.na(motivo_no_registro)) %>%
  ggplot(aes(motivo_no_registro)) +
  geom_bar(aes(y = (..count..)/sum(..count..), fill=factor(..x..)), stat= "count") +
  scale_fill_brewer(palette="GnBu", aesthetics = "fill", labels = c("No sabía de 
esta posibilidad.", "No sabía 
como hacerlo.", "Trámites complicados/ 
desplazarme lejos.", "No causar baja 
en el padrón de España.", "Crísis sanitaria
COVID-19 lo ha impedido.", "Administración no 
conozca mi residencia.", "No me interesa", "NS/NC.", "Falta de tiempo.") ) +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..), accuracy = 1),
                y= ((..count..)/sum(..count..))), stat="count",
            hjust = 0.5, size = 3, vjust= -0.5,
            inherit.aes = TRUE) +
  scale_y_continuous(labels=scales::percent) +
  theme_hc() +    theme(legend.position = "bottom",
                        legend.title = element_blank(),
                        legend.text = element_text(size = 7),
                        axis.text.x = element_blank(),
                        axis.ticks.x=element_blank()) +
  ylab("") +
  xlab("")

motivo_no_registro_g + guides(fill=guide_legend(nrow=3,byrow=TRUE, label.hjust = -0.1)) + ggsave("motivo_no_registro.png")

# Grafico de barras con % prob_trabajo
prob_trabajo_g <- madrileños %>%
  filter(!is.na(prob_trabajo)) %>%
  filter(!registrado_consulado %in% c("Ns/Nc")) %>%
  ggplot(aes(prob_trabajo)) +
  scale_fill_brewer(palette="GnBu", aesthetics = "fill", labels = c("Muy probable", "Bastante probable", "Poco probable", "Nada probable") ) +
  geom_bar(aes(y = (..count..)/sum(..count..), fill=factor(..x..)), stat= "count") +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..), accuracy = 1),
                y= ((..count..)/sum(..count..))), stat="count",
            hjust = 0.5, size = 3, vjust= -0.5,
            inherit.aes = TRUE) +
  scale_y_continuous(labels=scales::percent) +
  theme_hc() +      theme(legend.position = "bottom",
                          legend.title = element_blank(),
                          legend.text = element_text(size = 8),
                          axis.text.x = element_blank(),
                          axis.ticks.x=element_blank()) +
  ylab("") +
  xlab("")

prob_trabajo_g + guides(fill=guide_legend(nrow=2,byrow=TRUE, label.hjust = -0.1)) + ggsave("prob_trabajo_g.png")

# Grafico de barras con % apilado de categorias emprender_habilidades emprender_experiencia emprender_conocido emprender_oportunidades emprender_fracaso emprender_idea emprender_capital emprender_impuestos emprender_apoyo emprender_innovador
"emprender_habilidades_g <- madrileños %>%
  filter(!is.na(emprender_habilidades)) %>%
  ggplot(aes(emprender_habilidades, fill = emprender_habilidades)) +
  geom_bar(aes(y = (..count..)/sum(..count..), fill=factor(..x..)), stat= "count", fill = "paleturquoise3") +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..), accuracy = 1),
                y= ((..count..)/sum(..count..))), stat="count",
            hjust = 0.5, size = 3, vjust= -0.5,
            inherit.aes = TRUE) +
  scale_y_continuous(labels=scales::percent) +
  theme_hc() +      theme(legend.position = "none",
                          axis.text=element_text(size=10),
                          axis.title=element_text(size=10,face="bold"),
                          axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("") +
  xlab("")
emprender_habilidades_g + ggsave("emprender_habilidades_g.png")

emprender_experiencia_g <- madrileños %>%
  filter(!is.na(emprender_experiencia)) %>%
  ggplot(aes(emprender_experiencia, fill = emprender_experiencia)) +
  geom_bar(aes(y = (..count..)/sum(..count..), fill=factor(..x..)), stat= "count", fill = "paleturquoise3") +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..), accuracy = 1),
                y= ((..count..)/sum(..count..))), stat="count",
            hjust = 0.5, size = 3, vjust= -0.5,
            inherit.aes = TRUE) +
  scale_y_continuous(labels=scales::percent) +
  theme_hc() +      theme(legend.position = "none",
                          axis.text=element_text(size=10),
                          axis.title=element_text(size=10,face="bold"),
                          plot.title = element_text(size = 10, face = "bold"), 
                          axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("") +
  xlab("")
emprender_experiencia_g + ggsave("emprender_experiencia_g.png")

emprender_conocido_g <- madrileños %>%
  filter(!is.na(emprender_conocido)) %>%
  ggplot(aes(emprender_conocido, fill = emprender_conocido)) +
  geom_bar(aes(y = (..count..)/sum(..count..), fill=factor(..x..)), stat= "count", fill = "paleturquoise3") +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..), accuracy = 1),
                y= ((..count..)/sum(..count..))), stat="count",
            hjust = 0.5, size = 3, vjust= -0.5,
            inherit.aes = TRUE) +
  scale_y_continuous(labels=scales::percent) +
  theme_hc() +      theme(legend.position = "none",
                          axis.text=element_text(size=10),
                          axis.title=element_text(size=10,face="bold"),
                          plot.title = element_text(size = 10, face = "bold"), 
                          axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("") +
  xlab("")

emprender_conocido_g + ggsave("emprender_conocido_g.png")

emprender_oportunidades_g <- madrileños %>%
  filter(!is.na(emprender_oportunidades)) %>%
  ggplot(aes(emprender_oportunidades, fill = emprender_oportunidades)) +
  geom_bar(aes(y = (..count..)/sum(..count..), fill=factor(..x..)), stat= "count", fill = "paleturquoise3") +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..), accuracy = 1),
                y= ((..count..)/sum(..count..))), stat="count",
            hjust = 0.5, size = 3, vjust= -0.5,
            inherit.aes = TRUE) +
  scale_y_continuous(labels=scales::percent) +
  theme_hc() +      theme(legend.position = "none",
                          axis.text=element_text(size=10),
                          axis.title=element_text(size=10,face="bold"),
                          plot.title = element_text(size = 10, face = "bold"), 
                          axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("") +
  xlab("")
emprender_oportunidades_g + ggsave("emprender_oportunidades_g.png")

emprender_fracaso_g <- madrileños %>%
  filter(!is.na(emprender_fracaso)) %>%
  ggplot(aes(emprender_fracaso, fill = emprender_fracaso)) +
  geom_bar(aes(y = (..count..)/sum(..count..), fill=factor(..x..)), stat= "count", fill = "paleturquoise3") +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..), accuracy = 1),
                y= ((..count..)/sum(..count..))), stat="count",
            hjust = 0.5, size = 3, vjust= -0.5,
            inherit.aes = TRUE) +
  scale_y_continuous(labels=scales::percent) +
  theme_hc() +      theme(legend.position = "none",
                          axis.text=element_text(size=10),
                          axis.title=element_text(size=10,face="bold"),
                          plot.title = element_text(size = 10, face = "bold"), 
                          axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("") +
  xlab("")
emprender_fracaso_g + ggsave("emprender_fracaso_g.png")

emprender_idea_g <- madrileños %>%
  filter(!is.na(emprender_idea)) %>%
  ggplot(aes(emprender_idea, fill = emprender_idea)) +
  geom_bar(aes(y = (..count..)/sum(..count..), fill=factor(..x..)), stat= "count", fill = "paleturquoise3") +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..), accuracy = 1),
                y= ((..count..)/sum(..count..))), stat="count",
            hjust = 0.5, size = 3, vjust= -0.5,
            inherit.aes = TRUE) +
  scale_y_continuous(labels=scales::percent) +
  theme_hc() +      theme(legend.position = "none",
                          axis.text=element_text(size=10),
                          axis.title=element_text(size=10,face="bold"),
                          plot.title = element_text(size = 10, face = "bold"), 
                          axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("") +
  xlab("")

emprender_idea_g + ggsave("emprender_idea_g.png")


emprender_capital_g <- madrileños %>%
  filter(!is.na(emprender_capital)) %>%
  ggplot(aes(emprender_capital, fill = emprender_capital)) +
  geom_bar(aes(y = (..count..)/sum(..count..), fill=factor(..x..)), stat= "count", fill = "paleturquoise3") +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..), accuracy = 1),
                y= ((..count..)/sum(..count..))), stat="count",
            hjust = 0.5, size = 3, vjust= -0.5,
            inherit.aes = TRUE) +
  scale_y_continuous(labels=scales::percent) +
  theme_hc() +      theme(legend.position = "none",
                          axis.text=element_text(size=10),
                          axis.title=element_text(size=10,face="bold"),
                          plot.title = element_text(size = 10, face = "bold"), 
                          axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("") +
  xlab("")

emprender_capital_g+ ggsave("emprender_capital_g.png")

emprender_impuestos_g <- madrileños %>%
  filter(!is.na(emprender_impuestos)) %>%
  ggplot(aes(emprender_impuestos, fill = emprender_impuestos)) +
  geom_bar(aes(y = (..count..)/sum(..count..), fill=factor(..x..)), stat= "count", fill = "paleturquoise3") +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..), accuracy = 1),
                y= ((..count..)/sum(..count..))), stat="count",
            hjust = 0.5, size = 3, vjust= -0.5,
            inherit.aes = TRUE) +
  scale_y_continuous(labels=scales::percent) +
  theme_hc() +      theme(legend.position = "none",
                          axis.text=element_text(size=10),
                          axis.title=element_text(size=10,face="bold"),
                          plot.title = element_text(size = 10, face = "bold"), 
                          axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("") +
  xlab("")

emprender_impuestos_g + ggsave("emprender_impuestos_g.png")

emprender_apoyo_g <- madrileños %>%
  filter(!is.na(emprender_apoyo)) %>%
  ggplot(aes(emprender_apoyo, fill = emprender_apoyo)) +
  geom_bar(aes(y = (..count..)/sum(..count..), fill=factor(..x..)), stat= "count", fill = "paleturquoise3") +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..), accuracy = 1),
                y= ((..count..)/sum(..count..))), stat="count",
            hjust = 0.5, size = 3, vjust= -0.5,
            inherit.aes = TRUE) +
  scale_y_continuous(labels=scales::percent) +
  theme_hc() +     
  theme(legend.position = "none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold"),
        plot.title = element_text(size = 10, face = "bold"), 
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("") +
  xlab("")

emprender_apoyo_g + ggsave("emprender_apoyo_g.png")

emprender_innovador_g <- madrileños %>%
  filter(!is.na(emprender_innovador)) %>%
  ggplot(aes(emprender_innovador, fill = emprender_innovador)) +
  geom_bar(aes(y = (..count..)/sum(..count..), fill=factor(..x..)), stat= "count", fill = "paleturquoise3") +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..), accuracy = 1),
                y= ((..count..)/sum(..count..))), stat="count",
            hjust = 0.5, size = 3, vjust= -0.5,
            inherit.aes = TRUE) +
  scale_y_continuous(labels=scales::percent) +
  theme_hc() +       theme(legend.position = "none",
                           axis.text=element_text(size=10),
                           axis.title=element_text(size=10,face="bold"),
                           plot.title = element_text(size = 10, face = "bold"), 
                           axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("") +
  xlab("")
emprender_innovador_g + ggsave("emprender_innovador_g.png")"

# Pie chart % genero
genero_g <- madrileños %>%
  filter(!is.na(genero)) %>%
  ggplot(aes(genero, fill = genero)) +
  geom_bar(aes(y = (..count..)/sum(..count..), fill=factor(..x..)), stat= "count") +
  scale_fill_brewer(palette="GnBu", aesthetics = "fill") +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..), accuracy = 1),
                y= ((..count..)/sum(..count..))), stat="count",
            hjust = 0.5, size = 3, vjust= -0.5,
            inherit.aes = TRUE) +
  scale_y_continuous(labels=scales::percent) +
  theme_hc() +       theme(legend.position = "none",
                           axis.text=element_text(size=10),
                           axis.title=element_text(size=10,face="bold"),
                           plot.title = element_text(size = 10, face = "bold")) +
  ylab("") +
  xlab("")

genero_g + ggsave("genero_g.png")

# Grafico de barras con  % estado_civil
cnt <- plyr::count(madrileños$estado_civil)
madrileños$estado_civil <- factor(madrileños$estado_civil,
                                          levels = cnt$x[order(cnt$freq, decreasing = TRUE)])

estado_civil_g <- madrileños %>%
  filter(!is.na(estado_civil)) %>%
  ggplot(aes(estado_civil)) +
  geom_bar(aes(y = (..count..)/sum(..count..), fill=factor(..x..)), stat= "count") +
  scale_fill_brewer(palette="Blues", aesthetics = "fill", labels = c("Casado/a", "Soltero/a", "Viudo/a", "Separado/a", "Divorciado/a") ) +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..), accuracy = 1),
                y= ((..count..)/sum(..count..))), stat="count",
            hjust = 0.5, size = 3, vjust= -0.5,
            inherit.aes = TRUE) +
  scale_y_continuous(labels=scales::percent) +
  theme_hc() +       
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        axis.text.x = element_blank(),
        axis.ticks.x=element_blank()) +
  ylab("") +
  xlab("")

estado_civil_g + ggsave("estado_civil_g.png")

# Grafico de barras con  % vive_solo
cnt <- plyr::count(madrileños$vive_solo)
madrileños$vive_solo <- factor(madrileños$vive_solo,
                                  levels = cnt$x[order(cnt$freq, decreasing = TRUE)])

vive_solo_g <- madrileños %>%
  filter(!is.na(vive_solo)) %>%
  filter(!vive_solo %in% c("NS/NC")) %>%
  ggplot(aes(vive_solo)) +
  geom_bar(aes(y = (..count..)/sum(..count..), fill=factor(..x..)), stat= "count") +
  scale_fill_brewer(palette="Blues", aesthetics = "fill", labels = c("Solo/a", "Mi pareja", "Mi pareja y algún familiar", "Con amigos/as", "Compañeros/as de piso", "NS/NC") ) +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..), accuracy = 1),
                y= ((..count..)/sum(..count..))), stat="count",
            hjust = 0.5, size = 3, vjust= -0.5,
            inherit.aes = TRUE) +
  scale_y_continuous(labels=scales::percent) +
  theme_hc() +       
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        axis.text.x = element_blank(),
        axis.ticks.x=element_blank()) +
  ylab("") +
  xlab("")


vive_solo_g + ggsave("vive_solo_g.png")

# Grafico de barras con  % hijos
hijos_g <- madrileños %>%
  filter(!is.na(hijos)) %>%
  filter(!hijos %in% c("Ns/Nc")) %>%
  ggplot(aes(hijos)) +
  geom_bar(aes(y = (..count..)/sum(..count..), fill=factor(..x..)), stat= "count") +
  scale_fill_brewer(palette="Blues", aesthetics = "fill", labels = c("Sí, vive conmigo", "Sí, vive en el mismo país", "Sí, vive en España", "Sí, vive en otro país", "Compañeros/as de piso") ) +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..), accuracy = 1),
                y= ((..count..)/sum(..count..))), stat="count",
            hjust = 0.5, size = 3, vjust= -0.5,
            inherit.aes = TRUE) +
  scale_y_continuous(labels=scales::percent) + 
  theme_hc() +       
  theme(legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        axis.text.x = element_blank(),
        axis.ticks.x=element_blank()) +
  ylab("") +
  xlab("")

hijos_g + ggsave("hijos_g.png")

# Grafico de barras con  % tamaño_municipio
cnt <- plyr::count(madrileños$tamaño_municipio)
madrileños$tamaño_municipio <- factor(madrileños$tamaño_municipio,
                                          levels = cnt$x[order(cnt$freq, decreasing = TRUE)])

tamaño_municipio_g <- madrileños %>%
  filter(!is.na(tamaño_municipio)) %>%
  ggplot(aes(tamaño_municipio)) +
  geom_bar(aes(y = (..count..)/sum(..count..), fill=factor(..x..)), stat= "count") +
  scale_fill_brewer(palette="Blues", aesthetics = "fill", labels = c("Aldea/pueblo", "Ciudad (1.001 a 20.000)", "Ciudad (20.001 a 100.000)", "Ciudad (100.001 a 500.000", "Ciudad(Más de 500.001") ) +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..), accuracy = 1),
                y= ((..count..)/sum(..count..))), stat="count",
            hjust = 0.5, size = 3, vjust= -0.5,
            inherit.aes = TRUE) +
  scale_y_continuous(labels=scales::percent) +  theme(legend.position = "none") +
  theme_hc() +       
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        axis.text.x = element_blank(),
        axis.ticks.x=element_blank()) +
  ylab("") +
  xlab("")

tamaño_municipio_g + guides(fill=guide_legend(nrow=3,byrow=TRUE, label.hjust = -0.1)) + ggsave("tamaño_municipio_g.png")

############### codigo_postal  queda por recodificar y hacer un grafico  ##########
# Grafico de barras con  % estudios
cnt <- plyr::count(madrileños$estudios)
madrileños$estudios <- factor(madrileños$estudios,
                                      levels = cnt$x[order(cnt$freq, decreasing = TRUE)])

estudios_g <- madrileños %>%
  filter(!is.na(estudios)) %>%
  ggplot(aes(estudios)) +
  scale_fill_brewer(palette="GnBu", aesthetics = "fill", labels = c("Primaria", "Secundaria", "Bachillerato", "Formación profesional", "Grado universitario", "Postgrado", "Doctorado")) +
  geom_bar(aes(y = (..count..)/sum(..count..), fill=factor(..x..)), stat= "count") +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..), accuracy = 1),
                y= ((..count..)/sum(..count..))), stat="count",
            hjust = 0.5, size = 3, vjust= -0.5,
            inherit.aes = TRUE) +
  scale_y_continuous(labels=scales::percent) +
  theme_hc() +       
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        axis.text.x = element_blank(),
        axis.ticks.x=element_blank()) +
  ylab("") +
  xlab("")

estudios_g + guides(fill=guide_legend(nrow=3,byrow=TRUE, label.hjust = -0.1))  +ggsave("estudios_g.png")

# Grafico de barras con  % actividad_principal  --- hay muchas categorias, igual habria que juntarlas 0; 1; 2; 3-5; 6; 7; 8-10; 11-13, 14-15 o algo asi, como veas oportuno

# Grafico de barras con  % tipo_contrato
tipo_contrato_g <- madrileños %>%
  filter(!is.na(tipo_contrato)) %>%
  filter(!tipo_contrato %in% "NS/NC") %>%
  ggplot(aes(tipo_contrato)) +
  geom_bar(aes(y = (..count..)/sum(..count..), fill=factor(..x..)), stat= "count") +
  scale_fill_brewer(palette="Blues", aesthetics = "fill") +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..), accuracy = 1),
                y= ((..count..)/sum(..count..))), stat="count",
            hjust = 0.5, size = 3, vjust= -0.5,
            inherit.aes = TRUE) +
  scale_y_continuous(labels=scales::percent) +
  theme_hc() +       
  theme(legend.position = "none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold"),
        axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  ylab("") +
  xlab("")

tipo_contrato_g+ ggsave("tipo_contrato_g.png")

# Grafico de barras con  % dedicacion
dedicacion_g <- madrileños %>%
  filter(!is.na(dedicacion)) %>%
  filter(!dedicacion %in% "NS/NC") %>%
  ggplot(aes(dedicacion)) +
  geom_bar(aes(y = (..count..)/sum(..count..), fill=factor(..x..)), stat= "count") +
  scale_fill_brewer(palette="Blues", aesthetics = "fill") +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..), accuracy = 1),
                y= ((..count..)/sum(..count..))), stat="count",
            hjust = 0.5, size = 3, vjust= -0.5,
            inherit.aes = TRUE) +
  scale_y_continuous(labels=scales::percent) + 
  theme_hc() +       
  theme(legend.position = "none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold"),
        plot.title = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  ylab("") +
  xlab("") +
  scale_x_discrete(labels = c("Jornada completa", "Jornada parcial", "Por horas")) 
  

dedicacion_g + ggsave("dedicacion_g.png")

# Grafico de barras con  % salario
nb.cols <- 10
mycolors <- colorRampPalette(brewer.pal(8, "GnBu"))(nb.cols)


salario_g <- madrileños %>%
  filter(!is.na(salario)) %>%
  ggplot(aes(salario)) +
  geom_bar(aes(y = (..count..)/sum(..count..), fill=factor(..x..)), stat= "count") +
  scale_fill_manual(values = mycolors, aesthetics = "fill", labels = c("Hasta 500", "501-1.000", "1.001-1.500", "1.501-2.000", "2.001-2.500", "2.501-3.000", "3.001-3.500", "3.501-4.000",  "Más de 4.000", "NS/NC")) +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..), accuracy = 1),
                y= ((..count..)/sum(..count..))), stat="count",
            hjust = 0.5, size = 3, vjust= -0.5,
            inherit.aes = TRUE) +
  scale_y_continuous(labels=scales::percent) +  
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        axis.text.x = element_blank(),
        axis.ticks.x=element_blank()) +
  ylab("") +
  xlab("")

salario_g +guides(fill=guide_legend(nrow=3,byrow=TRUE, label.hjust = -0.1)) +  ggsave("salario_g.png")

