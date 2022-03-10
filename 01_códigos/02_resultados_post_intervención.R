#------------------------------------------------------------------------------#
# Proyecto:                   SEMINARIO CRÍTICAS FEMINISTAS AL PUNITIVISMO
# Objetivo:                   Analizar resultados de la encuesta
#
# Encargada:                  Regina Isabel Medina Rosales
# Correo:                     rmedina@intersecta.org
# Fecha de creación:          07 de octubre de 2021
# Última actualización:       07 de octubre de 2021
#------------------------------------------------------------------------------#



# 0. Configuración inicial -----------------------------------------------------

# Silenciar mensajes de .group en dplyr
options(dplyr.summarise.inform = FALSE)


# Librerías 
require(pacman)
p_load(readxl, googledrive, googlesheets4, tidyverse, dplyr, lubridate, 
  ggpubr,zoo, beepr)

# Limpiar espacio de trabajo 
rm(list=ls())

# Establecer directorios
inp     <- "02_datos_crudos/"

# Activar las credenciales de google
googledrive::drive_auth("rmedina@intersecta.org")
googlesheets4::gs4_auth("rmedina@intersecta.org")

# Verificar credenciales 
googledrive::drive_user()
googlesheets4::gs4_user() 

# Función para importar de manera más corta desde drive
imp_dv <- function(x){
  googlesheets4::read_sheet(
    paste0("https://docs.google.com/spreadsheets/d/", x))}


# 1. Cargar datos --------------------------------------------------------------

# ---- Preguntas pre-intervención (curso)
# Respuestas de participantes 
df_part_crudo <- imp_dv(
  "1KhPpSjSZM8i0_4i8SEGSa-WXMQjBHttPaFE0qZpwkTA/edit#gid=994639996")

dim(df_intr_crudo)

# ---- Preguntas post-intervención (curso)
df_post_crudo <- read_excel(
  paste0(inp, "encuesta_salida_seminario_punitivismo.xlsx"))

# 2. Procesar datos --------------------------------------------------------------

# Guardar nombres 
v_names <- names(df_part_crudo  %>% janitor::clean_names())

# Limpiar respuestas de participantes (antes el curso)
df_part_pre <- df_part_crudo    %>% 
  filter(!is.na(respondent_id)) %>% 
  janitor::clean_names()        %>% 
  select(c(1:4, 10:23))         %>% 
  rename(
    t01_pena_muerte = v_names[10],
    t02_carcel_nece = v_names[11], 
    t03_abolir_carc = v_names[12], 
    t04_pena_propor = v_names[13],
    t05_cadena_perp = v_names[14],
    t06_vulnera_der = v_names[15],
    t07_desincentiv = v_names[16],
    t08_registro_ag = v_names[17],
    t09_saber_socie = v_names[18],
    t10_carta_antec = v_names[19],
    t11_amenaza_dig = v_names[20],
    t12_creer_victi = v_names[21],
    t13_politica_pe = v_names[22],
    t14_castigo_vic = v_names[23]) %>% 
  mutate(
    grupo   = "Participantes seminario", 
    momento = "Inicio"
  )

# Limpiar respuestas de participantes post intervención
df_part_post <- df_post_crudo   %>% 
  filter(!is.na(respondent_id)) %>% 
  janitor::clean_names()        %>% 
  select(c(1:4, 10:23))         %>% 
  rename(
    t01_pena_muerte = v_names[10],
    t02_carcel_nece = v_names[11], 
    t03_abolir_carc = v_names[12], 
    t04_pena_propor = v_names[13],
    t05_cadena_perp = v_names[14],
    t06_vulnera_der = v_names[15],
    t07_desincentiv = v_names[16],
    t08_registro_ag = v_names[17],
    t09_saber_socie = v_names[18],
    t10_carta_antec = v_names[19],
    t11_amenaza_dig = v_names[20],
    t12_creer_victi = v_names[21],
    t13_politica_pe = v_names[22],
    t14_castigo_vic = v_names[23]) %>% 
  mutate(
    grupo   = "Participantes seminario", 
    momento = "Final"
  )

# Verificar 
dim(df_part_pre)
dim(df_part_post)

# Cambiar formato de respuestas
df_respuestas_pre <- as.data.frame(df_part_pre)  %>%
  # Eliminar a la persona que respondió de manera incompleta 
  filter(respondent_id != "13025443830") %>% 
  # Hacer que las respuestas de las participantes sean numéricas 
  mutate(
    respondent_id = as.character(respondent_id), 
    t01_pena_muerte = unlist(t01_pena_muerte), 
    t02_carcel_nece = unlist(t02_carcel_nece),
    t03_abolir_carc = unlist(t03_abolir_carc),
    t04_pena_propor = unlist(t04_pena_propor),
    t05_cadena_perp = unlist(t05_cadena_perp),
    t06_vulnera_der = unlist(t06_vulnera_der),
    t07_desincentiv = unlist(t07_desincentiv),
    t08_registro_ag = unlist(t08_registro_ag),
    t09_saber_socie = unlist(t09_saber_socie),
    t10_carta_antec = unlist(t10_carta_antec),
    t11_amenaza_dig = unlist(t11_amenaza_dig),
    t12_creer_victi = unlist(t12_creer_victi),
    t13_politica_pe = unlist(t13_politica_pe),
    t14_castigo_vic = unlist(t14_castigo_vic)
  ) 

df_respuestas_post <- as.data.frame(df_part_post) %>% 
  filter(respondent_id != "13359167614") %>% 
  mutate(
    respondent_id = as.character(respondent_id), 
    t01_pena_muerte = as.numeric(t01_pena_muerte), 
    t02_carcel_nece = as.numeric(t02_carcel_nece),
    t03_abolir_carc = as.numeric(t03_abolir_carc),
    t04_pena_propor = as.numeric(t04_pena_propor),
    t05_cadena_perp = as.numeric(t05_cadena_perp),
    t06_vulnera_der = as.numeric(t06_vulnera_der),
    t07_desincentiv = as.numeric(t07_desincentiv),
    t08_registro_ag = as.numeric(t08_registro_ag),
    t09_saber_socie = as.numeric(t09_saber_socie),
    t10_carta_antec = as.numeric(t10_carta_antec),
    t11_amenaza_dig = as.numeric(t11_amenaza_dig),
    t12_creer_victi = as.numeric(t12_creer_victi),
    t13_politica_pe = as.numeric(t13_politica_pe),
    t14_castigo_vic = as.numeric(t14_castigo_vic)
  ) 

# Unir respuestas de participantes pre y post, recodificar respuestas
df_participantes <- df_respuestas_post      %>% 
  select(-c(date_created, date_modified))   %>% 
  bind_rows(df_respuestas_pre       %>% 
      select(-c(date_created, date_modified))) 

# Convertir a formato largo y agregar
df_resultados <- df_participantes %>% 
  pivot_longer(
    cols = starts_with("t"), 
    names_to = "pregunta", 
    values_to = "respuesta") %>% 
  group_by(momento, pregunta) %>% 
  summarise(respuesta = mean(respuesta, na.rm = T))


# 3. Graficar datos ------------------------------------------------------------

# 3.0. Configuración -----------------------------------------------------------

tema        <-  theme_linedraw() +
  theme(
    plot.title.position   = "plot", 
    plot.caption.position = "plot",
    text                  = element_text(family = "Roboto Slab", color = "black"),
    plot.title            = element_text(family = "Roboto Slab", color = "black",   size = 16,  face  = "bold",  margin = margin(10,5,5,5)),
    plot.subtitle         = element_text(family = "Roboto Slab", color = "black",   size = 14,  margin = margin(5, 5, 5, 5)),
    plot.caption          = element_text(family = "Fira Sans",   color = "#92A39D", size = 11,  hjust = 0),
    panel.grid            = element_line(linetype = 2),
    plot.margin           = margin(0, 2, 0, 1.5, "cm"),
    legend.position       = "top",
    panel.border          = element_blank(),
    legend.title          = element_text(size = 11, family = "Fira Sans", face   = "bold"),
    legend.text           = element_text(size = 11, family = "Fira Sans"),
    axis.title            = element_text(size = 11, family = "Fira Sans", hjust = .5, margin = margin(1,1,1,1)),
    axis.text.y           = element_text(size = 11, family = "Fira Sans", angle=0,  hjust=.5),
    axis.text.x           = element_text(size = 11, family = "Fira Sans", angle=90, hjust=1, vjust = 0.5),
    strip.text.x          = element_text(size = 11, family = "Fira Sans", face = "bold", color = "black"),
    strip.text.y          = element_text(size = 11, family = "Fira Sans", face = "bold", color = "black"), 
    strip.background      = element_rect(fill = "white", color = NA))


## 3.1. Figuras ----------------------------------------------------------------

ggplot(df_resultados, 
  aes(x = respuesta, y = pregunta, color = momento)) +
  geom_point() 
