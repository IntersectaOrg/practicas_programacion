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
inp     <- "datos_crudos/"

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

# Respuestas de participantes 
df_part_crudo <- imp_dv(
  "1KhPpSjSZM8i0_4i8SEGSa-WXMQjBHttPaFE0qZpwkTA/edit#gid=994639996")

# Respuestas de las integrantes de Intersecta 
df_intr_crudo <- imp_dv(
  "1EBOjlw1AhBtMpXLY4VhJbhjsCLT6v5H0zl0ZS2lOuaw/edit#gid=941877612")


dim(df_part_crudo)
dim(df_intr_crudo)

# 2. Procesar datos --------------------------------------------------------------

# Guardar nombres 
v_names <- names(df_part_crudo  %>% janitor::clean_names())

# Limpiar respuestas de participantes 
df_part <- df_part_crudo        %>% 
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

# Limpiar respuestas de las intersecta 
v_names <- names(df_intr_crudo  %>% janitor::clean_names())

df_intr <- df_intr_crudo        %>% 
  # Seleccionar las respuestas que no son piloto 
  arrange(date_modified)        %>% 
  slice(2:8)                    %>% 
  # Renombrar
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
    grupo   = "Intersecta", 
    momento = "Inicio")

# Verificar 
dim(df_part)
dim(df_intr)

# Unir bases
df_respuestas <- as.data.frame(df_part)  %>%
  bind_rows(as.data.frame(df_intr))      %>%
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
    ) %>% 
  # Estandarizar respuestas en escala de -100 a 100
  mutate_if(is.numeric, 
    ~as.numeric((2*(. - min(.))/(max(.)-min(.)))-1))


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

v_preguntas <- names(df_part_crudo)[10:23]
v_title     <- "Respuestas diagnóstico del seminario críticas al punitivismo"
v_xlab      <- "Respuesta"
v_ylab      <- "Densidad"
v_fill      <- "Grupo"

v_niveles <- c("Muy en desacuerdo", "Ni en desacuerdo, ni de acuerdo", "Muy de acuerdo")
v_formato <- ".png"

## 3.1. Figuras ----------------------------------------------------------------

# Cambiar filtros y directorio según el grupo que se quiera estudiar 
out     <- "figs/seminario_punitivismo/participantes/" # Cambiar según filtros 

# Para ambos grupos 
# df_data <- df_respuestas

# Solo para las participantes del seminario
df_data <-  df_respuestas                     %>% 
  filter(grupo == "Participantes seminario")

### 3.1.1. Pena de muerte ------------------------------------------------------


g1 <- ggplot(df_data, 
  aes(x = t01_pena_muerte, fill = grupo, color = grupo)) +
  geom_density(aes(y = ..ndensity..), alpha = 0.3) +
  theme_classic() +
  # scale_y_continuous(label = scales::label_percent()) +
  labs(
    title    = v_title, 
    subtitle = v_preguntas[1], 
    x        = v_xlab, 
    y        = v_ylab, 
    fill     = v_fill
  ) +
  theme(
    legend.position = "top")+ 
  guides(color = "none") +
  scale_x_continuous(breaks = seq(-1, 1, 1),
    labels = v_niveles)
g1

ggsave(paste0(out, "g01_", "pena_muerte", v_formato), 
  width = 6, height = 4)

### 3.1.2. La cárcel es necesaria ----------------------------------------------


g2 <- ggplot(df_data, 
  aes(x = t02_carcel_nece, fill = grupo, color = grupo)) +
  geom_density(aes(y = ..ndensity..), alpha = 0.3) +
  theme_classic() +
  # scale_y_continuous(label = scales::label_percent()) +
  labs(
    title    = v_title, 
    subtitle = v_preguntas[2], 
    x        = v_xlab, 
    y        = v_ylab, 
    fill     = v_fill
  ) +
  theme(legend.position = "top")+ 
  guides(color = "none") +
  scale_x_continuous(breaks = seq(-1, 1, 1),
    labels = v_niveles)

g2

ggsave(paste0(out, "g02_", "carcel_necesaria", v_formato),
  width = 6, height = 4)

### 3.1.3. Abolir cárceles -----------------------------------------------------

g3 <- ggplot(df_data, 
  aes(x = t03_abolir_carc, fill = grupo, color = grupo)) +
  geom_density(aes(y = ..ndensity..), alpha = 0.3) +
  theme_classic() +
  # scale_y_continuous(label = scales::label_percent()) +
  labs(
    title    = v_title, 
    subtitle = v_preguntas[3], 
    x        = v_xlab, 
    y        = v_ylab, 
    fill     = v_fill
  ) +
  theme(legend.position = "top")+ 
  guides(color = "none") +
  scale_x_continuous(breaks = seq(-1, 1, 1),
    labels = v_niveles)

g3

ggsave(paste0(out, "g03_", "abolir_prisiones", v_formato),
  width = 6, height = 4)

### 3.1.4. Penas proporcionales al daño ----------------------------------------

g4 <- ggplot(df_data, 
  aes(x = t04_pena_propor, fill = grupo, color = grupo)) +
  geom_density(aes(y = ..ndensity..), alpha = 0.3) +
  theme_classic() +
  # scale_y_continuous(label = scales::label_percent()) +
  labs(
    title    = v_title, 
    subtitle = v_preguntas[4], 
    x        = v_xlab, 
    y        = v_ylab, 
    fill     = v_fill
  ) +
  theme(legend.position = "top")+ 
  guides(color = "none") +
  scale_x_continuous(breaks = seq(-1, 1, 1),
    labels = v_niveles)

g4

ggsave(paste0(out, "g04_", "pena_proporcional", v_formato),
  width = 6, height = 4)

### 3.1.5. Cadena perpetua -----------------------------------------------------

g5 <- ggplot(df_data, 
  aes(x = t05_cadena_perp, fill = grupo, color = grupo)) +
  geom_density(aes(y = ..ndensity..), alpha = 0.3) +
  theme_classic() +
  # scale_y_continuous(label = scales::label_percent()) +
  labs(
    title    = v_title, 
    subtitle = str_wrap(v_preguntas[5], width = 70), 
    x        = v_xlab, 
    y        = v_ylab, 
    fill     = v_fill
  ) +
  theme(legend.position = "top")+ 
  guides(color = "none") +
  scale_x_continuous(breaks = seq(-1, 1, 1),
    labels = v_niveles)
g5

ggsave(paste0(out, "g05_", "cadena_perpetua", v_formato),
  width = 6, height = 4)

### 3.1.6. Criminalizar protege derechos ---------------------------------------

g6 <- ggplot(df_data, 
  aes(x = t06_vulnera_der, fill = grupo, color = grupo)) +
  geom_density(aes(y = ..ndensity..), alpha = 0.3) +
  theme_classic() +
  # scale_y_continuous(label = scales::label_percent()) +
  labs(
    title    = v_title, 
    subtitle = str_wrap(v_preguntas[6], width = 70), 
    x        = v_xlab, 
    y        = v_ylab, 
    fill     = v_fill
  ) +
  theme(legend.position = "top")+ 
  guides(color = "none") +
  scale_x_continuous(breaks = seq(-1, 1, 1),
    labels = v_niveles)

g6

ggsave(paste0(out, "g06_", "criminalizar_protege", v_formato),
  width = 6, height = 4)

### 3.1.7. Castigar desincentiva -----------------------------------------------

g7 <- ggplot(df_data, 
  aes(x = t07_desincentiv, fill = grupo, color = grupo)) +
  geom_density(aes(y = ..ndensity..), alpha = 0.3) +
  theme_classic() +
  # scale_y_continuous(label = scales::label_percent()) +
  labs(
    title    = v_title, 
    subtitle = str_wrap(v_preguntas[7], width = 70), 
    x        = v_xlab, 
    y        = v_ylab, 
    fill     = v_fill
  ) +
  theme(legend.position = "top")+ 
  guides(color = "none") +
  scale_x_continuous(breaks = seq(-1, 1, 1),
    labels = v_niveles)

g7

ggsave(paste0(out, "g07_", "castigo_desincentivo", v_formato),
  width = 6, height = 4)


### 3.1.8. Registro de agresores -----------------------------------------------

g8 <- ggplot(df_data, 
  aes(x = t08_registro_ag, fill = grupo, color = grupo)) +
  geom_density(aes(y = ..ndensity..), alpha = 0.3) +
  theme_classic() +
  # scale_y_continuous(label = scales::label_percent()) +
  labs(
    title    = v_title, 
    subtitle = str_wrap(v_preguntas[8], width = 70), 
    x        = v_xlab, 
    y        = v_ylab, 
    fill     = v_fill
  ) +
  theme(legend.position = "top")+ 
  guides(color = "none") +
  scale_x_continuous(breaks = seq(-1, 1, 1),
    labels = v_niveles)

g8

ggsave(paste0(out, "g08_", "registro_agresores", v_formato),
  width = 6, height = 4)

### 3.1.9. Derecho a saber quién cometió delitos -------------------------------

g9 <- ggplot(df_data, 
  aes(x = t09_saber_socie, fill = grupo, color = grupo)) +
  geom_density(aes(y = ..ndensity..), alpha = 0.3) +
  theme_classic() +
  # scale_y_continuous(label = scales::label_percent()) +
  labs(
    title    = v_title, 
    subtitle = str_wrap(v_preguntas[9], width = 70), 
    x        = v_xlab, 
    y        = v_ylab, 
    fill     = v_fill
  ) +
  theme(legend.position = "top")+ 
  guides(color = "none") +
  scale_x_continuous(breaks = seq(-1, 1, 1),
    labels = v_niveles)

g9 

ggsave(paste0(out, "g09_", "derecho_conocer_agresores", v_formato),
  width = 6, height = 4)

### 3.1.10. Pena de muerte ------------------------------------------------------

g10 <- ggplot(df_data, 
  aes(x = t10_carta_antec, fill = grupo, color = grupo)) +
  geom_density(aes(y = ..ndensity..), alpha = 0.3) +
  theme_classic() +
  # scale_y_continuous(label = scales::label_percent()) +
  labs(
    title    = v_title, 
    subtitle = str_wrap(v_preguntas[10], width = 70), 
    x        = v_xlab, 
    y        = v_ylab, 
    fill     = v_fill
  ) +
  theme(legend.position = "top")+ 
  guides(color = "none") +
  scale_x_continuous(breaks = seq(-1, 1, 1),
    labels = v_niveles)

g10

ggsave(paste0(out, "g10_", "carta_antecedentes", v_formato),
  width = 6, height = 4)

### 3.1.11. Amenzar a hombres con cárcel ---------------------------------------

g11 <- ggplot(df_data, 
  aes(x = t11_amenaza_dig, fill = grupo, color = grupo)) +
  geom_density(aes(y = ..ndensity..), alpha = 0.3) +
  theme_classic() +
  # scale_y_continuous(label = scales::label_percent()) +
  labs(
    title    = v_title, 
    subtitle = str_wrap(v_preguntas[11], width = 70), 
    x        = v_xlab, 
    y        = v_ylab, 
    fill     = v_fill
  ) +
  theme(legend.position = "top")+ 
  guides(color = "none") +
  scale_x_continuous(breaks = seq(-1, 1, 1),
    labels = v_niveles)

g11

ggsave(paste0(out, "g11_", "amenaza_cárcel", v_formato),
  width = 6, height = 4)


### 3.1.12. Creer víctimas -----------------------------------------------------

g12 <- ggplot(df_data, 
  aes(x = t12_creer_victi, fill = grupo, color = grupo)) +
  geom_density(aes(y = ..ndensity..), alpha = 0.3) +
  theme_classic() +
  # scale_y_continuous(label = scales::label_percent()) +
  labs(
    title    = v_title, 
    subtitle = str_wrap(v_preguntas[12], width = 70), 
    x        = v_xlab, 
    y        = v_ylab, 
    fill     = v_fill
  ) +
  theme(legend.position = "top")+ 
  guides(color = "none") +
  scale_x_continuous(breaks = seq(-1, 1, 1),
    labels = v_niveles) 

g12

ggsave(paste0(out, "g12_", "creer_víctimas", v_formato),
  width = 6, height = 4)


### 3.1.13. Políticas penales y necesidades ------------------------------------

g13 <- ggplot(df_data, 
  aes(x = t13_politica_pe, fill = grupo, color = grupo)) +
  geom_density(aes(y = ..ndensity..), alpha = 0.3) +
  theme_classic() +
  # scale_y_continuous(label = scales::label_percent()) +
  labs(
    title    = v_title, 
    subtitle = str_wrap(v_preguntas[13], width = 70), 
    x        = v_xlab, 
    y        = v_ylab, 
    fill     = v_fill
  ) +
  theme(legend.position = "top")+ 
  guides(color = "none") +
  scale_x_continuous(breaks = seq(-1, 1, 1),
    labels = v_niveles)

g13

ggsave(paste0(out, "g13_", "políticas_penales", v_formato),
  width = 6, height = 4)


### 3.1.14. Cualquier castigo --------------------------------------------------

g14 <- ggplot(df_data, 
  aes(x = t14_castigo_vic, fill = grupo, color = grupo)) +
  geom_density(aes(y = ..ndensity..), alpha = 0.3) +
  theme_classic() +
  # scale_y_continuous(label = scales::label_percent()) +
  labs(
    title    = v_title, 
    subtitle = str_wrap(v_preguntas[14], width = 70), 
    x        = v_xlab, 
    y        = v_ylab, 
    fill     = v_fill
  ) +
  theme(legend.position = "top")+ 
  guides(color = "none") +
  scale_x_continuous(breaks = seq(-1, 1, 1),
    labels = v_niveles)

g14

ggsave(paste0(out, "g14_", "cualquier_castigo", v_formato),
  width = 6, height = 4)



## 3.2. Un solo gráfico --------------------------------------------------------

# ggarrange(g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11, g12, g13, g14, 
  # ncol = 2, nrow = 7)

# Guardar nombres de preguntas en texto y como id 
v_topic_text <- names(df_part_crudo)[10:23]
v_topic_id   <- names(df_respuestas)[5:18]

# Cambiar a formato largo y añadir texto de las preguntas
df_data <- df_respuestas                              %>% 
  filter(grupo == "Participantes seminario")          %>% 
  pivot_longer(
    cols      = c(t01_pena_muerte:t14_castigo_vic), 
    names_to  = "id_pregunta", 
    values_to = "respuesta")                          %>% 
  mutate(
    text_pregunta = case_when(
      id_pregunta == v_topic_id[1]  ~ v_topic_text[1], 
      id_pregunta == v_topic_id[2]  ~ v_topic_text[2], 
      id_pregunta == v_topic_id[3]  ~ v_topic_text[3], 
      id_pregunta == v_topic_id[4]  ~ v_topic_text[4], 
      id_pregunta == v_topic_id[5]  ~ v_topic_text[5], 
      id_pregunta == v_topic_id[6]  ~ v_topic_text[6], 
      id_pregunta == v_topic_id[7]  ~ v_topic_text[7], 
      id_pregunta == v_topic_id[8]  ~ v_topic_text[8], 
      id_pregunta == v_topic_id[9]  ~ v_topic_text[9], 
      id_pregunta == v_topic_id[10] ~ v_topic_text[10], 
      id_pregunta == v_topic_id[11] ~ v_topic_text[11], 
      id_pregunta == v_topic_id[12] ~ v_topic_text[12], 
      id_pregunta == v_topic_id[13] ~ v_topic_text[13], 
      id_pregunta == v_topic_id[14] ~ v_topic_text[14]))


# Graficar 
ggplot(df_data, aes(x = respuesta, fill = grupo, color = grupo)) +
  geom_density(aes(y = ..ndensity..), alpha = 0.3) +
  facet_wrap(~text_pregunta,
    labeller = label_wrap_gen(width = 45)) +
  theme_classic() +
  # scale_y_continuous(label = scales::label_percent()) +
  labs(
    title    = v_title, 
    # subtitle = str_wrap(v_preguntas[14], width = 70), 
    x        = v_xlab, 
    y        = v_ylab, 
    fill     = v_fill
  ) +
  theme(
    legend.position = "top", 
    axis.text.x = element_text(angle = 10))+ 
  guides(color = "none") +
  scale_x_continuous(breaks = seq(-1, 1, 1),
    labels = v_niveles)

ggsave(paste0(out, "g00_", "respuestas_inicio", v_formato),
  width = 12, height = 12)

