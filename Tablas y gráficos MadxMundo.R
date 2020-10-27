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


madrileños <- read_dta("MadxMundo + libres + recode.dta")
madrileños <- as_factor(madrileños)





# Grafico de barras (en %) de edad_intervalos
edad_intervalos_g <- madrileños %>%
  filter(!is.na(edad_intervalos)) %>%
  ggplot(aes(edad_intervalos, fill = edad_intervalos)) +
  geom_bar() +
  theme(legend.position = "none") +
  ylab("") +
  xlab("") +
  ggtitle("Edad Intervalos") 
edad_intervalos_g + scale_y_continuous(labels=scales::percent)


#Grafico de barras (en %) de años_fuera_intervalo  // Ojo recodificado! mira arriba
años_fuera_intervalo_g <- madrileños %>%
  filter(!is.na(años_fuera_intervalo)) %>%
  ggplot(aes(años_fuera_intervalo, fill = años_fuera_intervalo)) +
  geom_bar() +
  theme(legend.position = "none") +
  ylab("") +
  xlab("") +
  ggtitle("Años fuera de España") 
años_fuera_intervalo_g + scale_y_continuous(labels=scales::percent) 


# Pie chart de nacido_españa (en %)

# Pie chart de nacido_madrid (en %)

# Pie chart de nacionalidad (en %)

# Grafico de barras con el % apilado si/no en antes_turismo antes_laboral antes_familia antes_erasmus antes_beca antes_vivido antes_no
antes_pais_g <- madrileños %>%
  filter(!is.na(antes_pais)) %>%
  ggplot(aes(antes_pais, fill = antes_pais)) +
  geom_bar() +
  theme(legend.position = "bottom") +
  ggtitle("antes_pais")
antes_pais_g + scale_y_continuous(labels=scales::percent) 


# Grafico de barras situacion_españa (en %)

situacion_españa_g <- madrileños %>%
  filter(!is.na(situacion_españa)) %>%
  ggplot(aes(situacion_españa, fill = situacion_españa)) +
  geom_bar() +
  theme(legend.position = "bottom") +
  ggtitle("Situación en España")
situacion_españa_g + scale_y_continuous(labels=scales::percent) 


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
    theme(legend.position = "none") +
    ggtitle("Situación en España")
dific_idioma_g + scale_y_continuous(labels=scales::percent) 

  
dific_recursos_g <- madrileños %>%
    filter(!is.na(dific_recursos)) %>%
    ggplot(aes(dific_recursos, fill = dific_recursos)) +
    geom_bar() +
    theme(legend.position = "none") +
    ggtitle("dific_recursos")
dific_recursos_g + scale_y_continuous(labels=scales::percent) 
  
  
dific_idioma_g <- madrileños %>%
    filter(!is.na(situacion_españa)) %>%
    ggplot(aes(dific_idioma, fill = dific_idioma)) +
    geom_bar() +
    theme(legend.position = "none") +
    ggtitle("Situación en España")
dific_idioma_g + scale_y_continuous(labels=scales::percent) 
  
  
dific_alojamiento_g <- madrileños %>%
    filter(!is.na(dific_alojamiento)) %>%
    ggplot(aes(dific_alojamiento, fill = dific_alojamiento)) +
    geom_bar() +
    theme(legend.position = "none") +
    ggtitle("dific_alojamiento")
dific_alojamiento_g + scale_y_continuous(labels=scales::percent) 
  
  
dific_laborales_g <- madrileños %>%
    filter(!is.na(dific_laborales)) %>%
    ggplot(aes(dific_laborales, fill = dific_laborales)) +
    geom_bar() +
    theme(legend.position = "none") +
    ggtitle("dific_laborales")
dific_laborales_g + scale_y_continuous(labels=scales::percent) 
  
  
  dific_tramites_g <- madrileños %>%
    filter(!is.na(dific_tramites)) %>%
    ggplot(aes(dific_tramites, fill = dific_tramites)) +
    geom_bar() +
    theme(legend.position = "none") +
    ggtitle("dific_tramites")
dific_tramites_g + scale_y_continuous(labels=scales::percent) 
  
  
dific_sanidad_g <- madrileños %>%
    filter(!is.na(dific_sanidad)) %>%
    ggplot(aes(dific_sanidad, fill = dific_sanidad)) +
    geom_bar() +
    theme(legend.position = "none") +
    ggtitle("dific_sanidad")
dific_sanidad_g + scale_y_continuous(labels=scales::percent) 
  
  
dific_integracion_g <- madrileños %>%
    filter(!is.na(dific_integracion)) %>%
    ggplot(aes(dific_integracion, fill = dific_integracion)) +
    geom_bar() +
    theme(legend.position = "none") +
    ggtitle("dific_integracion")
dific_integracion_g + scale_y_continuous(labels=scales::percent) 


  
dific_psico_g <- madrileños %>%
    filter(!is.na(dific_psico)) %>%
    ggplot(aes(dific_psico, fill = dific_psico)) +
    geom_bar() +
    theme(legend.position = "none") +
    ggtitle("dific_psico")
dific_psico_g + scale_y_continuous(labels=scales::percent) 


# grafico de barras (%) viajes_madrid 
  
viajes_madrid_g <- madrileños %>%
    filter(!is.na(dific_psico)) %>%
    ggplot(aes(viajes_madrid, fill = viajes_madrid)) +
    geom_bar() +
    theme(legend.position = "bottom") +
    ggtitle("viajes_madrid")
viajes_madrid_g + scale_y_continuous(labels=scales::percent) 
  
# grafico de barras (%) frecuencia_gente_mad 

frecuencia_gente_mad_g <- madrileños %>%
  filter(!is.na(frecuencia_gente_mad)) %>%
  ggplot(aes(frecuencia_gente_mad, fill = frecuencia_gente_mad)) +
  geom_bar() +
  theme(legend.position = "bottom") +
  ggtitle("frecuencia_gente_mad")
frecuencia_gente_mad_g + scale_y_continuous(labels=scales::percent) 
  
  
# Grafico de barras con el % apilado si/no informa_esp miembro_aso manifestaciones reuniones (quitando ns/nc en los que queda)
    # participacion_df <- madrileños %>%
      filter(!is.na(miembro_aso),
             !is.na(manifestaciones),
             !is.na(reuniones)) %>% 
      select(miembro_aso, manifestaciones, reuniones) 
    

  
# grafico de barras (%) elecciones_generales_esp 
elecciones_generales_esp_g <- madrileños %>%
  filter(!is.na(elecciones_generales_esp)) %>%
  ggplot(aes(elecciones_generales_esp, fill = elecciones_generales_esp)) +
  geom_bar() +
  theme(legend.position = "bottom") +
  ggtitle("elecciones_generales_esp")
elecciones_generales_esp_g  + scale_y_continuous(labels=scales::percent) 

# grafico de barras (%) elecciones_autonomicas_esp 
elecciones_autonomicas_esp_g <- madrileños %>%
  filter(!is.na(elecciones_autonomicas_esp)) %>%
  ggplot(aes(elecciones_autonomicas_esp, fill = elecciones_autonomicas_esp)) +
  geom_bar() +
  theme(legend.position = "bottom") +
  ggtitle("elecciones_autonomicas_esp")
elecciones_autonomicas_esp_g + scale_y_continuous(labels=scales::percent) 


# grafico de barras (%) elecciones_municipales_esp 
elecciones_municipales_esp_g <- madrileños %>%
  filter(!is.na(elecciones_municipales_esp)) %>%
  ggplot(aes(elecciones_municipales_esp, fill = elecciones_municipales_esp)) +
  geom_bar() +
  theme(legend.position = "bottom") +
  ggtitle("elecciones_municipales_esp")
elecciones_municipales_esp_g + scale_y_continuous(labels=scales::percent) 


# grafico de barras (%) elecciones_municipales_reside 
elecciones_municipales_reside_g <- madrileños %>%
  filter(!is.na(elecciones_municipales_reside)) %>%
  ggplot(aes(elecciones_municipales_reside, fill = elecciones_municipales_reside)) +
  geom_bar() +
  theme(legend.position = "bottom") +
  ggtitle("elecciones_municipales_reside")
elecciones_municipales_reside_g + scale_y_continuous(labels=scales::percent)


# grafico de barras (%) elecciones_europeas_españoles 
elecciones_europeas_españoles_g <- madrileños %>%
  filter(!is.na(elecciones_europeas_españoles)) %>%
  ggplot(aes(elecciones_europeas_españoles, fill = elecciones_europeas_españoles)) +
  geom_bar() +
  theme(legend.position = "bottom") +
  ggtitle("elecciones_europeas_españoles")
elecciones_europeas_españoles_g + scale_y_continuous(labels=scales::percent)


# grafico de barras (%) elecciones_europeas_extranjeros
elecciones_europeas_extranjeros_g <- madrileños %>%
  filter(!is.na(elecciones_europeas_extranjeros)) %>%
  ggplot(aes(elecciones_europeas_extranjeros, fill = elecciones_europeas_extranjeros)) +
  geom_bar() +
  theme(legend.position = "bottom") +
  ggtitle("elecciones_europeas_extranjeros")
elecciones_europeas_extranjeros_g + scale_y_continuous(labels=scales::percent)


# Grafico de barras con el % apilado con las 4 categorias para conoce_bolsa conoce_ayudas conoce_asociaciones conoce_consejo conoce_portal conoce_info_salud conoce_info_padron

# Grafico de barras con el % de registrado_consulado
registrado_consulado_g <- madrileños %>%
  filter(!is.na(registrado_consulado)) %>%
  ggplot(aes(registrado_consulado, fill = registrado_consulado)) +
  geom_bar() +
  theme(legend.position = "bottom") +
  ggtitle("registrado_consulado")
registrado_consulado_g + scale_y_continuous(labels=scales::percent)


# Grafico de barras con el % apilado si/no registro_votar registro_documentacion registro_contacto registro_prevencion registro_certificado registro_franquicia registro_transacciones registro_iva

# Grafico de barras con el % apilado si/no no_registro_conocia no_registro_sabia no_registro_tramites no_registro_padron no_registro_covid no_registro_admin no_registro_aporta

# Grafico de barras con % prob_trabajo
prob_trabajo_g <- madrileños %>%
  filter(!is.na(registrado_consulado)) %>%
  ggplot(aes(prob_trabajo, fill = prob_trabajo)) +
  geom_bar() +
  theme(legend.position = "bottom") +
  ggtitle("prob_trabajo")
prob_trabajo_g + scale_y_continuous(labels=scales::percent)



# Grafico de barras con % apilado de categorias emprender_habilidades emprender_experiencia emprender_conocido emprender_oportunidades emprender_fracaso emprender_idea emprender_capital emprender_impuestos emprender_apoyo emprender_innovador

# Pie chart % genero

# Grafico de barras con  % estado_civil

estado_civil_g <- madrileños %>%
  filter(!is.na(registrado_consulado)) %>%
  ggplot(aes(estado_civil, fill = estado_civil)) +
  geom_bar() +
  theme(legend.position = "bottom") +
  ggtitle("estado_civil")
estado_civil_g + scale_y_continuous(labels=scales::percent)


# Grafico de barras con  % vive_solo

vive_solo_g <- madrileños %>%
  filter(!is.na(registrado_consulado)) %>%
  ggplot(aes(vive_solo, fill = vive_solo)) +
  geom_bar() +
  theme(legend.position = "bottom") +
  ggtitle("vive_solo")
vive_solo_g + scale_y_continuous(labels=scales::percent)

# Grafico de barras con  % hijos
hijos_g <- madrileños %>%
  filter(!is.na(registrado_consulado)) %>%
  ggplot(aes(hijos, fill = hijos)) +
  geom_bar() +
  theme(legend.position = "bottom") +
  ggtitle("hijos")
hijos_g + scale_y_continuous(labels=scales::percent)


# Grafico de barras con  % tamaño_municipio

# codigo_postal  queda por recodificar y hacer un grafico   ********
  
# Grafico de barras con  % estudios
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
educacion_g  + scale_y_continuous(labels=scales::percent)


# Grafico de barras con  % actividad_principal  --- hay muchas categorias, igual habria que juntarlas 0; 1; 2; 3-5; 6; 7; 8-10; 11-13, 14-15 o algo asi, como veas oportuno

# Grafico de barras con  % tipo_contrato

# Grafico de barras con  % dedicacion

# Grafico de barras con  % salario























genero_g <- madrileños %>%
  filter(genero %in% c("Masculino", "Femenino")) %>%
  ggplot(aes(genero, fill = genero)) +
  geom_bar() 
genero_g +scale_y_continuous(labels=scales::percent)


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
educacion_g  + scale_y_continuous(labels=scales::percent)
  

años_fuera_intervalo_g <- madrileños %>%
  filter(!is.na(años_fuera_intervalo)) %>%
  ggplot(aes(años_fuera_intervalo, fill = años_fuera_intervalo)) +
  geom_bar() +
  theme(legend.position = "none") +
  ylab("") +
  xlab("") +
  ggtitle("Años fuera de España") 

años_fuera_intervalo_g + scale_y_continuous(labels=scales::percent) 

