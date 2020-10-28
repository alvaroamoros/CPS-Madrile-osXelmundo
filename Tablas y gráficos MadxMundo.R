# TAbas prubea

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
library(ggrepel)

madrileños <- read_dta("MadxMundo + libres + recode.dta")
madrileños <- as_factor(madrileños)



# Grafico de barras (en %) de edad_intervalos
edad_intervalos_g <- madrileños %>%
  filter(!is.na(edad_intervalos)) %>%
  ggplot(aes(edad_intervalos, fill = edad_intervalos)) +
  geom_bar() +
  theme(legend.position = "none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold")) +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  ylab("") +
  xlab("") +
  ggtitle("Edad")  + 
  scale_y_continuous(labels = scales::percent, 
                     limits = c(0,1000))
edad_intervalos_g 
edad_intervalos_g ggsave("edad_intervalos_g.png")


scale_y_continuous(breaks = seq(0, 1.2, 0.2)) + 
  coord_cartesian(ylim = c(0, 1.2))



#Grafico de barras (en %) de años_fuera_intervalo  // Ojo recodificado! mira arriba
años_fuera_intervalo_g <- madrileños %>%
  filter(!is.na(años_fuera_intervalo)) %>%
  ggplot(aes(años_fuera_intervalo, fill = años_fuera_intervalo)) +
  geom_bar() +
  theme(legend.position = "none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold")) +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  ylab("") +
  xlab("") +
  ggtitle("Años fuera de España")  +
  scale_y_continuous(labels=scales::percent) 
años_fuera_intervalo_g + ggsave("años_fuera_intervalo_g.png")

# Pie chart de nacido_españa (en %)  *** En ggplot no hay paquete para hacer Quesos, se pueden hacer con una opccion de cordenadas pero no es facil cuadrarlos.
# R si tiene la opcción Pie pero habría que transformar los vectores.
#Personalmente tambíen creo q los quesos son bastante poco intuitivos de interpretar no se si podríamos hacer simple gráfico de barras aqui.

# Pie chart de nacido_madrid (en %)
antes_pais_g <- madrileños %>%
  filter(!is.na(nacido_madrid)) %>%
  ggplot(aes(nacido_madrid, fill = nacido_madrid)) +
  geom_bar() +
  theme(legend.position = "none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold")) +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  ggtitle("Nacidos en Madrid") +
  scale_y_continuous(labels=scales::percent) 
antes_pais_g +  ylab("") +
  xlab("") + ggsave("antes_pais_g.png")

# Pie chart de nacionalidad (en %)
nacionalidad_g <- madrileños %>%
  filter(!is.na(nacionalidad)) %>%
  ggplot(aes(nacionalidad, fill = nacionalidad)) +
  geom_bar() +
  theme(legend.position = "none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold")) +
  theme(plot.title = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Nacionalidad española") +
  scale_y_continuous(labels=scales::percent) 
nacionalidad_g +  ylab("") +
  xlab("") + ggsave("nacionalidad_g.png")

# Grafico de barras con el % apilado si/no en antes_turismo antes_laboral antes_familia antes_erasmus antes_beca antes_vivido antes_no

antes_pais_g <- madrileños %>%
  filter(!is.na(antes_pais)) %>%
  ggplot(aes(antes_pais, fill = antes_pais)) +
  geom_bar() +
  theme(legend.position = "none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold")) +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Había anteriormente en su país de residencia") +
  scale_y_continuous(labels=scales::percent)  +
  xlab("") +
  ylab("")

antes_pais_g + ggsave("antes_pais_g.png")


# Grafico de barras situacion_españa (en %)

situacion_españa_g <- madrileños %>%
  filter(!is.na(situacion_españa)) %>%
  ggplot(aes(situacion_españa, fill = situacion_españa)) +
  geom_bar() +
  theme(legend.position = "none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold")) +
  theme(plot.title = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Situación en España") +
  scale_y_continuous(labels=scales::percent) 
situacion_españa_g +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("") +
  xlab("") + ggsave("situacion_españa_g.png")


#Graficos de barras (%) de todas las dificultades
# dific_idioma 
# dific_recursos 
# dific_alojamiento 
# dific_laborales 
# dific_tramites 
# dific_sanidad 
# dific_integracion 

# dific_psico 
dific_idioma_g <- madrileños %>%
  filter(!is.na(dific_idioma)) %>%
  ggplot(aes(dific_idioma, fill = dific_idioma)) +
  geom_bar() +
  theme(legend.position = "none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold")) +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  ggtitle("Dificultades con el idioma en el país de destino") +
  scale_y_continuous(labels=scales::percent) 
dific_idioma_g+ ggsave("dific_idioma_g.png")

dific_recursos_g <- madrileños %>%
  filter(!is.na(dific_recursos)) %>%
  ggplot(aes(dific_recursos, fill = dific_recursos)) +
  geom_bar() +
  theme(legend.position = "none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold")) +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  ggtitle("Dificultades financieras al mudarse al país de destino") +
  scale_y_continuous(labels=scales::percent) 
dific_recursos_g +
  ylab("") +
  xlab("")+ ggsave("dific_recursos_g.png")

dific_idioma_g <- madrileños %>%
  filter(!is.na(situacion_españa)) %>%
  ggplot(aes(dific_idioma, fill = dific_idioma)) +
  geom_bar() +
  theme(legend.position = "none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold")) +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  ggtitle("Dificultades con el idioma en país de destino") + 
  scale_y_continuous(labels=scales::percent) 
dific_idioma_g +  ylab("") +
  xlab("")+ ggsave("dific_idioma_g.png")

dific_alojamiento_g <- madrileños %>%
  filter(!is.na(dific_alojamiento)) %>%
  ggplot(aes(dific_alojamiento, fill = dific_alojamiento)) +
  geom_bar() +
  theme(legend.position = "none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold")) +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  ggtitle("Dificultades con el alojamiento en el país de destino") +
  scale_y_continuous(labels=scales::percent) 
dific_alojamiento_g +
  ylab("") +
  xlab("")+ ggsave("dific_alojamiento_g.png")

dific_laborales_g <- madrileños %>%
  filter(!is.na(dific_laborales)) %>%
  ggplot(aes(dific_laborales, fill = dific_laborales)) +
  geom_bar() +
  theme(legend.position = "none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold")) +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  ggtitle("Dificultades laborales en el país de destino") +
  scale_y_continuous(labels=scales::percent) 
dific_laborales_g +
  ylab("") +
  xlab("")+ ggsave("dific_laborales_g.png")

dific_tramites_g <- madrileños %>%
  filter(!is.na(dific_tramites)) %>%
  ggplot(aes(dific_tramites, fill = dific_tramites)) +
  geom_bar() +
  theme(legend.position = "none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold")) +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  ggtitle("Dificultades conocimiento trámites administrativos en el país de destino") +
  scale_y_continuous(labels=scales::percent) 
dific_tramites_g +
  ylab("") +
  xlab("")+ ggsave("dific_tramites_g.png")

dific_sanidad_g <- madrileños %>%
  filter(!is.na(dific_sanidad)) %>%
  ggplot(aes(dific_sanidad, fill = dific_sanidad)) +
  geom_bar() +
  theme(legend.position = "none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold")) +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  ggtitle("Dificultades conocimiento questiones sanitarias 
en el país de destino") +
  scale_y_continuous(labels=scales::percent) 
dific_sanidad_g +
  ylab("") +
  xlab("")+ ggsave("dific_sanidad_g.png")

dific_integracion_g <- madrileños %>%
  filter(!is.na(dific_integracion)) %>%
  ggplot(aes(dific_integracion, fill = dific_integracion)) +
  geom_bar() +
  theme(legend.position = "none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold")) +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  ggtitle("Dificultades para integrarse en la 
sociedad en el país de destino") + 
  scale_y_continuous(labels=scales::percent) 
dific_integracion_g +
  ylab("") +
  xlab("")+ ggsave("dific_integracion_g.png")



dific_psico_g <- madrileños %>%
  filter(!is.na(dific_psico)) %>%
  ggplot(aes(dific_psico, fill = dific_psico)) +
  geom_bar() +
  theme(legend.position = "none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold")) +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  ggtitle("Dificultades psicológicas en el país de destino") + 
  scale_y_continuous(labels=scales::percent) 
dific_psico_g +
  ylab("") +
  xlab("")+ ggsave("dific_psico_g.png")

# grafico de barras (%) viajes_madrid 

viajes_madrid_g <- madrileños %>%
  filter(!is.na(dific_psico)) %>%
  ggplot(aes(viajes_madrid, fill = viajes_madrid)) +
  geom_bar() +
  theme(legend.position = "none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold")) +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  ggtitle("Con que frecuencia viaja a Madrid") +
  scale_y_continuous(labels=scales::percent) 

viajes_madrid_g + theme(legend.position = "none",
                        axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("") +
  xlab("")+ ggsave("viajes_madrid_g.png")

# grafico de barras (%) frecuencia_gente_mad 

frecuencia_gente_mad_g <- madrileños %>%
  filter(!is.na(frecuencia_gente_mad)) %>%
  ggplot(aes(frecuencia_gente_mad, fill = frecuencia_gente_mad)) +
  geom_bar() +
  theme(legend.position = "none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold")) +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  ggtitle("Con que frecuencia tiene contacto con gente de Madrid") +
  scale_y_continuous(labels=scales::percent) 
frecuencia_gente_mad_g + theme(legend.position = "none",
                               axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("") +
  xlab("")+ ggsave("frecuencia_gente_mad_g.png")

# Grafico de barras con el % apilado si/no informa_esp miembro_aso manifestaciones reuniones (quitando ns/nc en los que queda) 
##Aqui no tengo claro si es esto lo que tenías en mente ((REVISAR))


miembro_aso_g <- madrileños %>%
  filter(!is.na(informa_esp),
         !is.na(miembro_aso)) %>%
  ggplot(aes(informa_esp, fill = miembro_aso)) +
  geom_bar(position = position_dodge()) +
  theme(legend.position = "none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold")) +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  labs(fill = "¿Es miembro de alguna asociación relacionada
                con España/Madrid el país donde reside?") +
  ggtitle("¿Con qué frecuencia se informa sobre 
           la actualidad en España?") +
  scale_y_continuous(labels=scales::percent) +
  xlab("") +
  ylab("")
miembro_aso_g + ggsave("miembro_aso_g.png")

manifestaciones_g <- madrileños %>%
  filter(!is.na(informa_esp),
         !is.na(manifestaciones)) %>%
  filter(manifestaciones %in% c("Sí", "No")) %>%
  ggplot(aes(informa_esp, fill = manifestaciones)) +
  geom_bar(position = position_dodge()) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold")) +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  labs(fill = "Participa en manifestaciones relacionadas
              con España / Madrid en el país donde reside?") +
  ggtitle("¿Con qué frecuencia se informa sobre 
           la actualidad en España?") +
  scale_y_continuous(labels=scales::percent) +
  xlab("") +
  ylab("")
manifestaciones_g + ggsave("manifestaciones.png")


reuniones_g <- madrileños %>%
  filter(!is.na(informa_esp),
         !is.na(reuniones)) %>%
  filter(reuniones %in% c("Sí", "No" )) %>%
  ggplot(aes(informa_esp, fill = reuniones)) +
  geom_bar(position = position_dodge()) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold")) +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  labs(fill = "¿Asiste a reuniones/fiestas con gran número de Españoles?") +
  ggtitle("¿Con qué frecuencia se informa sobre la actualidad
          en España?") +
  scale_y_continuous(labels=scales::percent) +
  xlab("") +
  ylab("")
reuniones_g + ggsave("reuniones_g.png")



#Model inverso, guardar por si hace falta  
"miembro_aso_g <- madrileños %>%  
      filter(!is.na(informa_esp),
             !is.na(miembro_aso)) %>%
      ggplot(aes(miembro_aso, fill = informa_esp)) +
      geom_bar(position = position_dodge()) +
      theme(legend.position = "bottom") +
      labs(fill = "¿Con qué frecuencia se informa sobre la actualidad en España?") +
      ggtitle("¿Es miembro de alguna asociación relacionada con España/Madrid el país donde reside?") +
      scale_y_continuous(labels=scales::percent) +
      xlab("") +
      ylab("")
    miembro_aso_g" 

# grafico de barras (%) elecciones_generales_esp 
elecciones_generales_esp_g <- madrileños %>%
  filter(!is.na(elecciones_generales_esp)) %>%
  ggplot(aes(elecciones_generales_esp, fill = elecciones_generales_esp)) +
  geom_bar() +
  theme(legend.position = "bottom") +
  ggtitle("¿Ha participado en las Elecciones generales
  de España de noviembre 2019?") + 
  scale_y_continuous(labels=scales::percent) 
elecciones_generales_esp_g + theme(legend.position = "none",
                                   axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold")) +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  ylab("") +
  xlab("")+ ggsave("elecciones_generales_esp_g.png")

# grafico de barras (%) elecciones_autonomicas_esp 
elecciones_autonomicas_esp_g <- madrileños %>%
  filter(!is.na(elecciones_autonomicas_esp)) %>%
  ggplot(aes(elecciones_autonomicas_esp, fill = elecciones_autonomicas_esp)) +
  geom_bar() +
  theme(legend.position = "bottom") +
  ggtitle("¿Ha participado en las Elecciones autonómicas
  de Madrid en mayo 2019?") +
  scale_y_continuous(labels=scales::percent) 
elecciones_autonomicas_esp_g  + theme(legend.position = "none",
                                      axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold")) +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  ylab("") +
  xlab("")+ ggsave("elecciones_autonomicas_esp_g.png")

# grafico de barras (%) elecciones_municipales_esp 
elecciones_municipales_esp_g <- madrileños %>%
  filter(!is.na(elecciones_municipales_esp)) %>%
  ggplot(aes(elecciones_municipales_esp, fill = elecciones_municipales_esp)) +
  geom_bar() +
  theme(legend.position = "bottom") +
  ggtitle("¿Ha participado en las Elecciones municipales
  de España en mayo del 2019?") +
  scale_y_continuous(labels=scales::percent) 
elecciones_municipales_esp_g  + theme(legend.position = "none",
                                      axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold")) +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  ylab("") +
  xlab("")+ ggsave("elecciones_municipales_esp_g.png")

# grafico de barras (%) elecciones_municipales_reside 
elecciones_municipales_reside_g <- madrileños %>%
  filter(!is.na(elecciones_municipales_reside)) %>%
  ggplot(aes(elecciones_municipales_reside, fill = elecciones_municipales_reside)) +
  geom_bar() +
  theme(legend.position = "bottom") +
  ggtitle("¿Ha participado en las Elecciones municipales
  del país en que reside?") +
  scale_y_continuous(labels=scales::percent)
elecciones_municipales_reside_g  + theme(legend.position = "none",
                                         axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold")) +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  ylab("") +
  xlab("")+ ggsave("elecciones_municipales_reside_g.png")

# grafico de barras (%) elecciones_europeas_españoles 
elecciones_europeas_españoles_g <- madrileños %>%
  filter(!is.na(elecciones_europeas_españoles)) %>%
  ggplot(aes(elecciones_europeas_españoles, fill = elecciones_europeas_españoles)) +
  geom_bar() +
  theme(legend.position = "bottom") +
  ggtitle("¿Ha participado en las Elecciones europeas de mayo 2019? 
- voto a candidatos españoles") +
  scale_y_continuous(labels=scales::percent)
elecciones_europeas_españoles_g + theme(legend.position = "none",
                                        axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold")) +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  ylab("") +
  xlab("")+ ggsave("elecciones_europeas_españoles_g.png")

# grafico de barras (%) elecciones_europeas_extranjeros
elecciones_europeas_extranjeros_g <- madrileños %>%
  filter(!is.na(elecciones_europeas_extranjeros)) %>%
  ggplot(aes(elecciones_europeas_extranjeros, fill = elecciones_europeas_extranjeros)) +
  geom_bar() +
  theme(legend.position = "bottom") +
  ggtitle("¿Ha participado en las Elecciones europeas de mayo 2019?
  - voto a candidatos del país en el que reside") +
  scale_y_continuous(labels=scales::percent)
elecciones_europeas_extranjeros_g + theme(legend.position = "none",
                                          axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold")) +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  ylab("") +
  xlab("")+ ggsave("elecciones_europeas_extranjeros_g.png")

# Grafico de barras con el % apilado con las 4 categorias para conoce_bolsa conoce_ayudas conoce_asociaciones conoce_consejo conoce_portal conoce_info_salud conoce_info_padron

conoce_bolsa_g <- madrileños %>%
  filter(!is.na(conoce_bolsa)) %>%
  ggplot(aes(conoce_bolsa, fill = conoce_bolsa)) +
  geom_bar() +
  theme(legend.position = "bottom") +
  ggtitle("Conoce la Bolsa de empleo de la 
  Administración Regional madrileña") +
  scale_y_continuous(labels=scales::percent) 
conoce_bolsa_g + theme(legend.position = "none",
                       axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold")) +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  ylab("") +
  xlab("")+ ggsave("conoce_bolsa_g.png")

conoce_ayudas_g <- madrileños %>%
  filter(!is.na(conoce_ayudas)) %>%
  ggplot(aes(conoce_ayudas, fill = conoce_ayudas)) +
  geom_bar() +
  theme(legend.position = "bottom") +
  ggtitle("Conoce las ayudas para emprendedores de la 
  Administración Regional madrileña") +
  scale_y_continuous(labels=scales::percent)
conoce_ayudas_g + theme(legend.position = "none",
                        axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold")) +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  ylab("") +
  xlab("")+ ggsave("conoce_ayudas_g.png")

conoce_asociaciones_g <- madrileños %>%
  filter(!is.na(conoce_asociaciones)) %>%
  ggplot(aes(conoce_asociaciones, fill = conoce_asociaciones)) +
  geom_bar() +
  theme(legend.position = "bottom") +
  ggtitle("¿Conoce las asociaciones y centros
  de madrileños en el extranjero?") +
  scale_y_continuous(labels=scales::percent)
conoce_asociaciones_g + theme(legend.position = "none",
                              axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold")) +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  ylab("") +
  xlab("")+ ggsave("conoce_asociaciones_g.png")

conoce_consejo_g <- madrileños %>%
  filter(!is.na(conoce_consejo)) %>%
  ggplot(aes(conoce_consejo, fill = conoce_consejo)) +
  geom_bar() +
  theme(legend.position = "bottom") +
  ggtitle("Conoce el Consejo de madrileños en el extranjero") +
  scale_y_continuous(labels=scales::percent)
conoce_consejo_g + theme(legend.position = "none",
                         axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold")) +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  ylab("") +
  xlab("")+ ggsave("conoce_consejo_g.png")

conoce_portal_g <- madrileños %>%
  filter(!is.na(conoce_portal)) %>%
  ggplot(aes(conoce_portal, fill = conoce_portal)) +
  geom_bar() +
  theme(legend.position = "bottom") +
  ggtitle("Conoce el Portal de emigración de la 
  Administración Regional madrileña") +
  scale_y_continuous(labels=scales::percent)
conoce_portal_g + theme(legend.position = "none",
                        axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold")) +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  ylab("") +
  xlab("")+ ggsave("conoce_portal_g.png")

conoce_info_salud_g <- madrileños %>%
  filter(!is.na(conoce_bolsa)) %>%
  ggplot(aes(conoce_info_salud, fill = conoce_info_salud)) +
  geom_bar() +
  theme(legend.position = "bottom") +
  ggtitle("Conoce  el servicio de información 
  oficial sobre salud en Madrid") +
  scale_y_continuous(labels=scales::percent)
conoce_info_salud_g + theme(legend.position = "none",
                            axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold")) +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  ylab("") +
  xlab("")+ ggsave("conoce_info_salud_g.png")

conoce_info_padron_g <- madrileños %>%
  filter(!is.na(conoce_info_padron)) %>%
  ggplot(aes(conoce_info_padron, fill = conoce_info_padron)) +
  geom_bar() +
  theme(legend.position = "bottom") +
  ggtitle("Conoce el servicio de información oficial 
  sobre el empadronamiento en Madrid") + 
  theme(legend.position = "none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold")) +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels=scales::percent) +
  xlab("") +
  ylab("")

conoce_info_padron_g+ ggsave("conoce_info_padron_g.png")

# Grafico de barras con el % de registrado_consulado
registrado_consulado_g <- madrileños %>%
  filter(!is.na(registrado_consulado)) %>%
  ggplot(aes(registrado_consulado, fill = registrado_consulado)) +
  geom_bar() +
  theme(legend.position = "bottom") +
  ggtitle("¿Se ha registrado en el consulado 
  de la región donde vive?") +
  scale_y_continuous(labels=scales::percent)+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold")) +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  ylab("") +
  xlab("")
registrado_consulado_g + ggsave("registrado_consulado_g.png")

# Grafico de barras con el % apilado si/no registro_votar registro_documentacion registro_contacto registro_prevencion registro_certificado registro_franquicia registro_transacciones registro_iva

motivo_registro_g <- madrileños %>%
  filter(!is.na(motivo_registro)) %>%
  ggplot(aes(motivo_registro, fill = motivo_registro)) +
  geom_bar() +
  theme(legend.position = "bottom") +
  ggtitle("¿Por qué ha registrado su residencia en 
  el consulado español?") +
  scale_y_continuous(labels=scales::percent)
motivo_registro_g + theme(legend.position = "none",
                          axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold")) +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  
  ylab("") +
  xlab("")+ ggsave("motivo_registro_g.png")

"registro_votar_g <- madrileños %>%
      filter(!is.na(registro_votar)) %>%
      ggplot(aes(registro_votar, fill = registro_votar)) +
      geom_bar() +
      theme(legend.position = "bottom") +
      ggtitle("registro_votar") +
      scale_y_continuous(labels=scales::percent)
    registro_votar_g
    
    registro_documentacion_g <- madrileños %>%
      filter(!is.na(registro_documentacion)) %>%
      ggplot(aes(registro_documentacion, fill = registro_documentacion)) +
      geom_bar() +
      theme(legend.position = "bottom") +
      ggtitle("registro_documentacion") +
      scale_y_continuous(labels=scales::percent)
    registro_documentacion_g
    
    registro_contacto_g <- madrileños %>%
      filter(!is.na(registro_contacto)) %>%
      ggplot(aes(registro_contacto, fill = registro_contacto)) +
      geom_bar() +
      theme(legend.position = "bottom") +
      ggtitle("registro_contacto") +
      scale_y_continuous(labels=scales::percent)
    registro_contacto_g
    
    registro_prevencion_g <- madrileños %>%
      filter(!is.na(registro_prevencion)) %>%
      ggplot(aes(registro_prevencion, fill = registro_prevencion)) +
      geom_bar() +
      theme(legend.position = "bottom") +
      ggtitle("registro_prevencion") +
      scale_y_continuous(labels=scales::percent)
    registro_prevencion_g
    
    registro_certificado_g <- madrileños %>%
      filter(!is.na(registrado_consulado)) %>%
      ggplot(aes(registro_certificado, fill = registro_certificado)) +
      geom_bar() +
      theme(legend.position = "bottom") +
      ggtitle("registro_certificado") +
      scale_y_continuous(labels=scales::percent)
    registro_certificado_g
    
    registro_franquicia_g <- madrileños %>%
      filter(!is.na(registrado_consulado)) %>%
      ggplot(aes(registro_franquicia, fill = registro_franquicia)) +
      geom_bar() +
      theme(legend.position = "bottom") +
      ggtitle("registro_franquicia") +
      scale_y_continuous(labels=scales::percent)
    registro_franquicia_g
    
    registro_transacciones_g <- madrileños %>%
      filter(!is.na(registro_transacciones)) %>%
      ggplot(aes(registro_transacciones, fill = registro_transacciones)) +
      geom_bar() +
      theme(legend.position = "bottom") +
      ggtitle("registro_transacciones") +
      scale_y_continuous(labels=scales::percent)
    registro_transacciones_g
    
    registro_iva_g  <- madrileños %>%
      filter(!is.na(registro_iva)) %>%
      ggplot(aes(registro_iva, fill = registro_iva)) +
      geom_bar() +
      theme(legend.position = "bottom") +
      ggtitle("registro_iva") +
      scale_y_continuous(labels=scales::percent)
    registro_iva_g"

# Grafico de barras con el % apilado si/no no_registro_conocia no_registro_sabia no_registro_tramites no_registro_padron no_registro_covid no_registro_admin no_registro_aporta
motivo_no_registro_g  <- madrileños %>%
  filter(!is.na(motivo_no_registro)) %>%
  ggplot(aes(motivo_no_registro, fill = motivo_no_registro)) +
  geom_bar() +
  theme(legend.position = "bottom") +
  ggtitle("¿Por qué no ha registrado su residencia 
  en el consulado español?") +
  scale_y_continuous(labels=scales::percent)
motivo_no_registro_g + theme(legend.position = "none",
                             axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold")) +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  ylab("") +
  xlab("")+ ggsave("motivo_no_registro_g.png")

# Grafico de barras con % prob_trabajo
prob_trabajo_g <- madrileños %>%
  filter(!is.na(registrado_consulado)) %>%
  ggplot(aes(prob_trabajo, fill = prob_trabajo)) +
  geom_bar() +
  theme(legend.position = "bottom") +
  ggtitle("En el caso de su hipotética vuelta a Madrid, ¿cree probable
  encontrara trabajo en un plazo de doce meses?") + 
  scale_y_continuous(labels=scales::percent)
prob_trabajo_g  + theme(legend.position = "none",
                        axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold")) +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  ylab("") +
  xlab("")+ ggsave("prob_trabajo_g.png")

# Grafico de barras con % apilado de categorias emprender_habilidades emprender_experiencia emprender_conocido emprender_oportunidades emprender_fracaso emprender_idea emprender_capital emprender_impuestos emprender_apoyo emprender_innovador
emprender_habilidades_g <- madrileños %>%
  filter(!is.na(emprender_habilidades)) %>%
  ggplot(aes(emprender_habilidades, fill = emprender_habilidades)) +
  geom_bar() +
  theme(legend.position = "bottom") +
  ggtitle("En relación con las posibilidades de emprender un negocio,
considera  que tiene las habilidades
y conocimientos necesarios") + 
  scale_y_continuous(labels=scales::percent)
emprender_habilidades_g + theme(legend.position = "none") +
  theme(legend.position = "none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold")) +
  theme(plot.title = element_text(size = 10, face = "bold"), 
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("") +
  xlab("")+ ggsave("emprender_habilidades_g.png")

emprender_experiencia_g <- madrileños %>%
  filter(!is.na(emprender_experiencia)) %>%
  ggplot(aes(emprender_experiencia, fill = emprender_experiencia)) +
  geom_bar() +
  theme(legend.position = "bottom") +
  theme(legend.position = "none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold")) +
  theme(plot.title = element_text(size = 10, face = "bold"), 
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("En relación con las posibilidades de emprender un negocio, 
considera que tiene experiencia necesaria ") + 
  scale_y_continuous(labels=scales::percent)
emprender_experiencia_g + theme(legend.position = "none") +
  ylab("") +
  xlab("")+ ggsave("emprender_experiencia_g.png")

emprender_conocido_g <- madrileños %>%
  filter(!is.na(emprender_conocido)) %>%
  ggplot(aes(emprender_conocido, fill = emprender_conocido)) +
  geom_bar() +
  theme(legend.position = "bottom") +
  ggtitle("En relación con las posibilidades de emprender un negocio, 
¿conoce a alguien que ha montado un negocio en los últimos 
dos años?") + 
  scale_y_continuous(labels=scales::percent)
emprender_conocido_g + 
  theme(legend.position = "none") +
  theme(legend.position = "none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold")) +
  theme(plot.title = element_text(size = 10, face = "bold"), 
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("") +
  xlab("")+ ggsave("emprender_conocido_g.png")

emprender_oportunidades_g <- madrileños %>%
  filter(!is.na(emprender_oportunidades)) %>%
  ggplot(aes(emprender_oportunidades, fill = emprender_oportunidades)) +
  geom_bar() +
  theme(legend.position = "bottom") +
  ggtitle("En relación con las posibilidades de emprender un negocio, 
¿observa buenas oportunidades para montar un negocio
en los próximos seis meses en el área donde vive? ") + 
  scale_y_continuous(labels=scales::percent)
emprender_oportunidades_g + theme(legend.position = "none") +
  theme(legend.position = "none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold")) +
  theme(plot.title = element_text(size = 10, face = "bold"), 
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("") +
  xlab("")+ ggsave("emprender_oportunidades_g.png")

emprender_fracaso_g <- madrileños %>%
  filter(!is.na(emprender_fracaso)) %>%
  ggplot(aes(emprender_fracaso, fill = emprender_fracaso)) +
  geom_bar() +
  theme(legend.position = "bottom") +
  theme(legend.position = "none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold")) +
  theme(plot.title = element_text(size = 10, face = "bold"), 
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("En relación con las posibilidades de emprender un negocio, 
¿descartó la idea debido al miedo al fracaso ?") + 
  scale_y_continuous(labels=scales::percent)
emprender_fracaso_g + theme(legend.position = "none") +
  ylab("") +
  xlab("")+ ggsave("emprender_fracaso_g.png")

emprender_idea_g <- madrileños %>%
  filter(!is.na(emprender_idea)) %>%
  ggplot(aes(emprender_idea, fill = emprender_idea)) +
  geom_bar() +
  theme(legend.position = "bottom") +
  theme(legend.position = "none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold")) +
  theme(plot.title = element_text(size = 10, face = "bold"), 
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("En relación con las posibilidades de emprender un negocio, 
considera que tiene una idea ") + 
  scale_y_continuous(labels=scales::percent)
emprender_idea_g  + theme(legend.position = "none") +
  ylab("") +
  xlab("")+ ggsave("emprender_idea_g.png")


emprender_capital_g <- madrileños %>%
  filter(!is.na(emprender_capital)) %>%
  ggplot(aes(emprender_capital, fill = emprender_capital)) +
  geom_bar() +
  theme(legend.position = "bottom") +
  theme(legend.position = "none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold")) +
  theme(plot.title = element_text(size = 10, face = "bold"), 
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("En relación con las posibilidades de emprender un negocio, 
¿considera que tiene capital/recursos financieros suficientes?") + 
  scale_y_continuous(labels=scales::percent)
emprender_capital_g  + theme(legend.position = "none") +
  ylab("") +
  xlab("")+ ggsave("emprender_capital_g.png")

emprender_impuestos_g <- madrileños %>%
  filter(!is.na(emprender_impuestos)) %>%
  ggplot(aes(emprender_impuestos, fill = emprender_impuestos)) +
  geom_bar() +
  theme(legend.position = "bottom") +
  theme(legend.position = "none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold")) +
  theme(plot.title = element_text(size = 10, face = "bold"), 
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("En relación con las posibilidades de emprender un negocio, 
¿considera que los impuestos son demasiado altos?") + 
  scale_y_continuous(labels=scales::percent)
emprender_impuestos_g  + theme(legend.position = "none") +
  ylab("") +
  xlab("")+ ggsave("emprender_impuestos_g.png")

emprender_apoyo_g <- madrileños %>%
  filter(!is.na(emprender_apoyo)) %>%
  ggplot(aes(emprender_apoyo, fill = emprender_apoyo)) +
  geom_bar() +
  theme(legend.position = "bottom") +
  theme(legend.position = "none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold")) +
  theme(plot.title = element_text(size = 10, face = "bold"), 
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("En relación con las posibilidades de emprender un negocio, 
¿considera que falta apoyo o programas públicos, 
o de las instituciones locales en el área donde vive?") + 
  scale_y_continuous(labels=scales::percent)
emprender_apoyo_g  + theme(legend.position = "none") +
  ylab("") +
  xlab("")+ ggsave("emprender_apoyo_g.png")

emprender_innovador_g <- madrileños %>%
  filter(!is.na(emprender_innovador)) %>%
  ggplot(aes(emprender_innovador, fill = emprender_innovador)) +
  geom_bar() +
  theme(legend.position = "bottom") +
  theme(legend.position = "none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold")) +
  theme(plot.title = element_text(size = 10, face = "bold"), 
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("En relación con las posibilidades de emprender un negocio, 
¿considera que tiene un proyecto innovador que 
he aprendido / extraido de su experiencia en el exterior?") + 
  scale_y_continuous(labels=scales::percent)
emprender_innovador_g  + theme(legend.position = "none") +
  ylab("") +
  xlab("")+ ggsave("emprender_innovador_g.png")

# Pie chart % genero
genero_g <- madrileños %>%
  filter(!is.na(genero)) %>%
  ggplot(aes(genero, fill = genero)) +
  geom_bar() +
  theme(legend.position = "none") +
  theme(legend.position = "none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold")) +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  ggtitle("¿Cuál es su género?") + 
  scale_y_continuous(labels=scales::percent)
genero_g +  ylab("") +
  xlab("")+ ggsave("genero_g.png")

# Grafico de barras con  % estado_civil

estado_civil_g <- madrileños %>%
  filter(!is.na(registrado_consulado)) %>%
  ggplot(aes(estado_civil, fill = estado_civil)) +
  geom_bar() +
  theme(legend.position = "bottom") +
  theme(legend.position = "none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold")) +
  theme(plot.title = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("¿Cuál es su estado civil?") + 
  scale_y_continuous(labels=scales::percent)
estado_civil_g + theme(legend.position = "none") +
  ylab("") +
  xlab("")+ ggsave("estado_civil_g.png")

# Grafico de barras con  % vive_solo

vive_solo_g <- madrileños %>%
  filter(!is.na(vive_solo)) %>%
  ggplot(aes(vive_solo, fill = vive_solo)) +
  geom_bar() +
  theme(legend.position = "bottom") +
  theme(legend.position = "none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold")) +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  ggtitle("¿Vive con algún adulto más o vive sólo?") + 
  scale_y_continuous(labels=scales::percent)
vive_solo_g  + theme(legend.position = "none",
                     axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("") +
  xlab("")+ ggsave("vive_solo_g.png")

# Grafico de barras con  % hijos
hijos_g <- madrileños %>%
  filter(!is.na(registrado_consulado)) %>%
  ggplot(aes(hijos, fill = hijos)) +
  geom_bar() +
  theme(legend.position = "bottom") +
  theme(legend.position = "none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold")) +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  ggtitle("¿Tiene hijos?") + 
  scale_y_continuous(labels=scales::percent)
hijos_g + theme(legend.position = "none",
                axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("") +
  xlab("")+ ggsave("hijos_g.png")

# Grafico de barras con  % tamaño_municipio
tamaño_municipio_g <- madrileños %>%
  filter(!is.na(tamaño_municipio)) %>%
  ggplot(aes(tamaño_municipio, fill = tamaño_municipio)) +
  geom_bar() +
  theme(legend.position = "none") +
  theme(legend.position = "none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold")) +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  ggtitle("Tamaño en el municipio en el que vive actualmente") + 
  scale_y_continuous(labels=scales::percent)
tamaño_municipio_g + theme(legend.position = "none",
                           axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("") +
  xlab("")+ ggsave("tamaño_municipio_g.png")

# codigo_postal  queda por recodificar y hacer un grafico   ********

# Grafico de barras con  % estudios
educacion_g <- madrileños %>%
  filter(!is.na(estudios)) %>%
  ggplot(aes(estudios, fill = estudios)) +
  geom_bar()  +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))  +
  scale_x_discrete(labels = c("Primaria", "Secundaria", "Bachillerato", "Formación profesional", "Grado universitario", "Postgrado", "Doctorado")) +
  theme(legend.position = "none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold")) +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  ylab("") +
  xlab("") +
  ggtitle("¿Cuál es su nivel de estudios terminados?")+ 
  scale_y_continuous(labels=scales::percent)
educacion_g+ ggsave("educacion_g.png")

# Grafico de barras con  % actividad_principal  --- hay muchas categorias, igual habria que juntarlas 0; 1; 2; 3-5; 6; 7; 8-10; 11-13, 14-15 o algo asi, como veas oportuno

# Grafico de barras con  % tipo_contrato
tipo_contrato_g <- madrileños %>%
  filter(!is.na(tipo_contrato)) %>%
  ggplot(aes(tipo_contrato, fill = tipo_contrato)) +
  geom_bar() +
  theme(legend.position = "bottom") +
  theme(legend.position = "none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold")) +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  ggtitle("¿Qué tipo de contrato tiene?") +
  scale_y_continuous(labels=scales::percent)
tipo_contrato_g + theme(legend.position = "none",
                        axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("") +
  xlab("")+ ggsave("tipo_contrato_g.png")

# Grafico de barras con  % dedicacion
dedicacion_g <- madrileños %>%
  filter(!is.na(dedicacion)) %>%
  ggplot(aes(dedicacion, fill = dedicacion)) +
  geom_bar() +
  theme(legend.position = "bottom") +
  theme(legend.position = "none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold")) +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  ggtitle("¿Con qué dedicación?") + 
  scale_y_continuous(labels=scales::percent)
dedicacion_g + theme(legend.position = "none",
                     axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("") +
  xlab("")+ ggsave("dedicacion_g.png")

# Grafico de barras con  % salario
salario_g <- madrileños %>%
  filter(!is.na(salario)) %>%
  ggplot(aes(salario, fill = salario)) +
  geom_bar() +
  theme(legend.position = "bottom") +
  theme(legend.position = "none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold")) +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  ggtitle("Salario neto que percibe al mes") + 
  scale_y_continuous(labels=scales::percent)
salario_g + theme(legend.position = "none",
                  axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("") +
  xlab("")+ ggsave("salario_g.png")

# Obtener el directorio temporal de los gráficos
plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE); 
plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)

# Copiar en directorio de destino
file.copy(from=plots.png.paths, to="C:/Users/Petazetas/Desktop/Madrileños por el mundo/Graficos")






