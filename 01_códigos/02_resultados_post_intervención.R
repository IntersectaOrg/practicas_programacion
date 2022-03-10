#------------------------------------------------------------------------------#
# Proyecto:                   SEMINARIO CRÍTICAS FEMINISTAS AL PUNITIVISMO
# Objetivo:                   Analizar el cambio antes y después 
#
# Encargada:                  Regina Isabel Medina Rosales
# Correo:                     rmedina@intersecta.org
# Fecha de creación:          09 de marzo de 2022
# Última actualización:       10 de marzo de 2022
#------------------------------------------------------------------------------#

# 0. Configuración inicial -----------------------------------------------------

# Silenciar mensajes de .group en dplyr
options(dplyr.summarise.inform = FALSE)


# Librerías 
require(pacman)
p_load(extrafont, readxl, googledrive, googlesheets4, tidyverse, dplyr, 
       lubridate, ggpubr, ggalt, stringr, MetBrewer, zoo, beepr)

# Limpiar espacio de trabajo 
rm(list=ls())

# Establecer directorios
inp     <- "02_datos_crudos/"
out     <- "figs/seminario_punitivismo/cambio/"

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
    respondent_id   = as.character(respondent_id), 
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
  bind_rows(df_respuestas_pre               %>% 
      select(-c(date_created, date_modified))) %>% 
  # Cambiar a escala de 0-100 las primeras tres preguntas 
  mutate(
    t01_pena_muerte = case_when(
        t01_pena_muerte ==  1 ~ 0,
        t01_pena_muerte ==  2 ~ 25,
        t01_pena_muerte ==  3 ~ 50,
        t01_pena_muerte ==  4 ~ 75,
        t01_pena_muerte ==  5 ~ 100),
    t02_carcel_nece = case_when(
        t02_carcel_nece ==  1 ~ 0,
        t02_carcel_nece ==  2 ~ 25,
        t02_carcel_nece ==  3 ~ 50,
        t02_carcel_nece ==  4 ~ 75,
        t02_carcel_nece ==  5 ~ 100),
    t03_abolir_carc = case_when(
        t03_abolir_carc ==  1 ~ 0,
        t03_abolir_carc ==  2 ~ 25,
        t03_abolir_carc ==  3 ~ 50,
        t03_abolir_carc ==  4 ~ 75,
        t03_abolir_carc ==  5 ~ 100))

# Convertir a formato largo y agregar
df_resultados <- df_participantes           %>% 
  pivot_longer(
    cols = starts_with("t"), 
    names_to = "pregunta", 
    values_to = "respuesta")                %>% 
  group_by(momento, pregunta)               %>% 
  summarise(respuesta = mean(respuesta, na.rm = T))


# 3. Graficar datos ------------------------------------------------------------

# 3.0. Configuración -----------------------------------------------------------

# remotes::install_version("Rttf2pt1", version = "1.3.8")
# 
# # Fuentes tipográficas
# extrafont::font_import()
# extrafont::loadfonts(device = "win")
# extrafont::fonts()

# Tema personalizado
tema <-  theme_linedraw() +
  theme(
    text             = element_text(family = "Fira Sans", color = "black"),
    plot.title       = element_text(size = 14, face = "bold",     hjust = 0.5, margin = margin(10,5,5,5), family="Fira Sans", color = "black"),
    plot.subtitle    = element_text(size = 13, color = "#666666", hjust = 0.5, margin = margin(5, 5, 5, 5), family="Fira Sans"),
    plot.caption     = element_text(hjust = .5, size = 9, family = "Fira Sans", color = "black"),
    panel.grid       = element_line(linetype = 2), 
    legend.position  = "top",
    panel.grid.minor = element_blank(),
    legend.title     = element_text(size = 10, face = "bold", family="Fira Sans"),
    legend.text      = element_text(size = 10, family="Fira Sans"),
    axis.title       = element_text(size = 10, hjust = .5, margin = margin(1,1,1,1), family="Fira Sans"),
    axis.text.y      = element_text(size = 8, family="Fira Sans", angle=0, hjust=.5),
    axis.text.x      = element_text(size = 8, family="Fira Sans", angle=0, hjust=.5),
    strip.background = element_rect(fill="white", colour = NA),
    strip.text.x     = element_text(size=9, family = "Fira Sans", face = "bold", color = "black"),
    strip.text.y     = element_text(size=9, family = "Fira Sans", face = "bold", color = "black"))

# Niveles de las escalas 
v_niveles <- c("Muy en desacuerdo (0)", "Ni en desacuerdo, ni de acuerdo (50)", "Muy de acuerdo (100)")

# Colores
v_Hokusai = c("#6d2f20", "#b75347", "#df7e66", "#e09351", "#edc775", "#94b594", "#224b5e") 

# Formato 
v_formato <- ".png"


## 3.1. Figuras ----------------------------------------------------------------

#### 3.1.1. Cambio promedio en respuestas --------------------------------------

# Procesamiento de los datos 
df_data <- df_resultados          %>% 
  pivot_wider(    
    names_from  = momento,     
    values_from = respuesta)      %>% 
  janitor::clean_names()          %>% 
  mutate(cambio = inicio - final) %>% 
  arrange((cambio))               %>% 
  mutate(p_long = case_when(
    pregunta == "t01_pena_muerte" ~ "La pena de muerte es injustificable",
    pregunta == "t02_carcel_nece" ~ "La cárcel a veces es necesaria",
    pregunta == "t03_abolir_carc" ~ "Hay que abolir las cárceles",
    pregunta == "t04_pena_propor" ~ "Las penas de cárcel deben ser proporcionales al daño",
    pregunta == "t05_cadena_perp" ~ "La cadena perpetua puede ser legítima en ocasiones",
    pregunta == "t06_vulnera_der" ~ "No criminalizar equivale a desproteger un derecho",
    pregunta == "t07_desincentiv" ~ "Castigar desincentiva conductas dañinas",
    pregunta == "t08_registro_ag" ~ "El Registro de Agresores Sexuales es necesario",
    pregunta == "t09_saber_socie" ~ "Debe ser público qué personas han cometido delitos",
    pregunta == "t10_carta_antec" ~ "Un trabajo no debería pedir carta de no antecedentes",
    pregunta == "t11_amenaza_dig" ~ "Los hombres deben ser amenazados con cárcel por difundir contenido íntimo",
    pregunta == "t12_creer_victi" ~ "Siempre le debemos creer a las víctimas",
    pregunta == "t13_politica_pe" ~ "Las políticas penales deben reflejar los deseos de las víctimas",
    pregunta == "t14_castigo_vic" ~ "Cualquier castigo está justificado si lo pide la víctima")
  ) %>% 
  mutate(
    pregunta = forcats::fct_inorder(pregunta),
    p_long   = forcats::fct_inorder(p_long))

# Títulos 
v_title     <- "Cambio en opinones respecto al punitivismo"
v_subtitle  <- "Antes y después del seminario de Intersecta\n\"Críticas feministas al punitivismo\""
v_empty     <- ""
v_caption   <- "Nota: Se compara el promedio de las respuestas de todas las participantes."

# Visualización 
ggplot(df_data, 
  aes(y = p_long, x = inicio, xend = final)) +
  # Geoms
  geom_dumbbell(
    size = 2, color = "grey", colour_x = v_Hokusai[2], colour_xend = v_Hokusai[6],
    dot_guide_size = 50) +
  geom_text(data = df_data %>% filter(pregunta == "t04_pena_propor"), 
            aes(x = inicio, y = 14.4, label = "Antes"), 
            color = "#666666", family = "Fira Sans") +
  geom_text(data = df_data %>% filter(pregunta == "t04_pena_propor"), 
            aes(x = final, y = 14.4, label = "Después"), 
            color = "#666666", family = "Fira Sans") +
  # Etiquetas 
  labs(
    title    = v_title, 
    subtitle = v_subtitle, 
    x        = "\nRespuesta promedio\n", 
    y        = v_empty, 
    caption  = v_caption
  ) +
  # Escalas
  scale_x_continuous(breaks = seq(0, 100, 50), labels = v_niveles) +
  scale_y_discrete(labels = scales::wrap_format(32)) + 
  scale_color_manual(values = met.brewer("Morgenstern", 3)) +
  expand_limits(x = c(0, 110), y = c(0, 15)) +
  # Tema
  tema

ggsave(paste0(out, "g_00_", "cambio_promedio", v_formato), 
       width = 8, height = 8, device = "png", type = "cairo")

# ---- Versión en inglés 
df_data <- df_resultados          %>% 
  pivot_wider(    
    names_from  = momento,     
    values_from = respuesta)      %>% 
  janitor::clean_names()          %>% 
  mutate(cambio = inicio - final) %>% 
  arrange((cambio))               %>% 
  mutate(p_long = case_when(
    pregunta == "t01_pena_muerte" ~ "The death penalty is unjustifiable",
    pregunta == "t02_carcel_nece" ~ "Prison is necessary sometimes",
    pregunta == "t03_abolir_carc" ~ "Prisons should be abolished",
    pregunta == "t04_pena_propor" ~ "Jail sentences should be proportional to the damage",
    pregunta == "t05_cadena_perp" ~ "Life-sentences are legitimate sometimes",
    pregunta == "t06_vulnera_der" ~ "Lack of criminalization equals not protecting a right",
    pregunta == "t07_desincentiv" ~ "Punishment inhibits harmful conducts",
    pregunta == "t08_registro_ag" ~ "Sex Offenders Registries are necessary",
    pregunta == "t09_saber_socie" ~ "Information on who has committed crimes should be public",
    pregunta == "t10_carta_antec" ~ "Jobs should not require criminal records ",
    pregunta == "t11_amenaza_dig" ~ "Men should be threatened with jail time for spreading intimate content without consent",
    pregunta == "t12_creer_victi" ~ "Victims must always be believed",
    pregunta == "t13_politica_pe" ~ "Criminal law should reflect the victims’ desires",
    pregunta == "t14_castigo_vic" ~ "Any punishment is justifiable if the victim wants it")
  ) %>% 
  mutate(
    pregunta = forcats::fct_inorder(pregunta),
    p_long   = forcats::fct_inorder(p_long))

v_levels <- c("Totally against (0)", "Neutral (50)", "Totally in favor (100)")

v_title     <- "Change in perceptions towards punitivism"
v_subtitle  <- "Before and after attending Intersecta's\n\"Feminist Critiques to Punitivism\" Seminar"
v_empty     <- ""
v_caption   <- "
Note: The comparison shows the difference between the average response\nof all the participants, before and after the seminar."

# Visualización 
ggplot(df_data, 
       aes(y = p_long, x = inicio, xend = final)) +
  # Geoms
  geom_dumbbell(
    size = 2, color = "grey", colour_x = v_Hokusai[2], colour_xend = v_Hokusai[6],
    dot_guide_size = 50) +
  geom_text(data = df_data %>% filter(pregunta == "t04_pena_propor"), 
            aes(x = inicio, y = 14.4, label = "Before"), 
            color = "#666666", family = "Fira Sans") +
  geom_text(data = df_data %>% filter(pregunta == "t04_pena_propor"), 
            aes(x = final, y = 14.4, label = "After"), 
            color = "#666666", family = "Fira Sans") +
  # Etiquetas 
  labs(
    title    = v_title, 
    subtitle = v_subtitle, 
    x        = "\nAverage response", 
    y        = v_empty, 
    caption  = v_caption
  ) +
  # Escalas
  scale_x_continuous(breaks = seq(0, 100, 50), labels = v_levels) +
  scale_y_discrete(labels = scales::wrap_format(30)) + 
  scale_color_manual(values = met.brewer("Morgenstern", 3)) +
  expand_limits(x = c(0, 110), y = c(0, 15)) +
  # Tema
  tema

ggsave(paste0(out, "g_00_", "change_mean", v_formato), 
       width = 8, height = 8, device = "png", type = "cairo")

#### 3.1.2. Pena de muerte -----------------------------------------------------

# Preguntas de acuerdo a la redacción original
v_preguntas <- names(df_part_crudo)[10:23]

# Vectores de texto
v_xlab      <- "Respuesta"
v_ylab      <- "Densidad"
v_fill      <- "Momento de respuesta:"

# Renombrar base
df_data <- df_participantes %>% 
  mutate(
    momento = if_else(
      momento == "Inicio", "Antes del seminario", "Después del seminario"))

# Gráfica
g1 <-
  ggplot(df_data, 
             aes(x = t01_pena_muerte, fill = momento, color = momento)) +
  geom_density(aes(y = ..ndensity..), alpha = 0.3) +
  theme_classic() +
  # scale_y_continuous(label = scales::label_percent()) +
  labs(
    title    = v_title, 
    subtitle = v_preguntas[1], 
    x        = v_empty, 
    y        = v_ylab, 
    fill     = v_fill
  ) +
  guides(color = "none") +
  # Escalas 
  scale_x_continuous(breaks = seq(0, 100, 50),
                     labels = v_niveles) +
  scale_fill_manual(values = c(v_Hokusai[2], v_Hokusai[6])) +
  scale_color_manual(values = c(v_Hokusai[2], v_Hokusai[6])) +
  expand_limits(x = c(0, 110)) +
  tema

g1

ggsave(paste0(out, "g01_", "pena_muerte", v_formato), 
       width = 6, height = 4, device = "png", type = "cairo")

ggplot(df_data, 
       aes(x = t01_pena_muerte, fill = momento)) +
  geom_bar() +
  facet_wrap(~momento) +
  theme_classic() +
  # scale_y_continuous(label = scales::label_percent()) +
  labs(
    title    = v_title, 
    subtitle = v_preguntas[1], 
    x        = v_empty, 
    y        = v_ylab, 
    fill     = v_fill
  ) +
  guides(color = "none") +
  # Escalas 
  scale_x_continuous(breaks = seq(0, 100, 50), labels = v_niveles) +
  scale_fill_manual(values = c(v_Hokusai[2], v_Hokusai[6])) +
  scale_color_manual(values = c(v_Hokusai[2], v_Hokusai[6])) +
  expand_limits(x = c(0, 110)) +
  tema

### 3.1.3. La cárcel es necesaria ----------------------------------------------
# Gráfica

g2 <-
  ggplot(df_data, 
         aes(x = t02_carcel_nece, fill = momento, color = momento)) +
  geom_density(aes(y = ..ndensity..), alpha = 0.3) +
  theme_classic() +
  # scale_y_continuous(label = scales::label_percent()) +
  labs(
    title    = v_title, 
    subtitle = v_preguntas[1], 
    x        = v_empty, 
    y        = v_ylab, 
    fill     = v_fill
  ) +
  guides(color = "none") +
  # Escalas 
  scale_x_continuous(breaks = seq(0, 100, 50),
                     labels = v_niveles) +
  scale_fill_manual(values = c(v_Hokusai[2], v_Hokusai[6])) +
  scale_color_manual(values = c(v_Hokusai[2], v_Hokusai[6])) +
  expand_limits(x = c(0, 110)) +
  tema

g2

ggsave(paste0(out, "g02_", "carcel_necesaria", v_formato),
       width = 6, height = 4, device = "png", type = "cairo")


### 3.1.4. Abolir cárceles -----------------------------------------------------

g3 <-
  ggplot(df_data, 
         aes(x = t03_abolir_carc, fill = momento, color = momento)) +
  geom_density(aes(y = ..ndensity..), alpha = 0.3) +
  theme_classic() +
  # scale_y_continuous(label = scales::label_percent()) +
  labs(
    title    = v_title, 
    subtitle = v_preguntas[3], 
    x        = v_empty, 
    y        = v_ylab, 
    fill     = v_fill
  ) +
  guides(color = "none") +
  # Escalas 
  scale_x_continuous(breaks = seq(0, 100, 50),
                     labels = v_niveles) +
  scale_fill_manual(values = c(v_Hokusai[2], v_Hokusai[6])) +
  scale_color_manual(values = c(v_Hokusai[2], v_Hokusai[6])) +
  expand_limits(x = c(0, 110)) +
  tema

g3

ggsave(paste0(out, "g03_", "abolir_prisiones", v_formato),
       width = 6, height = 4, device = "png", type = "cairo")

### 3.1.5. Penas proporcionales al daño ----------------------------------------

g4 <-
  ggplot(df_data, 
         aes(x = t04_pena_propor, fill = momento, color = momento)) +
  geom_density(aes(y = ..ndensity..), alpha = 0.3) +
  theme_classic() +
  # scale_y_continuous(label = scales::label_percent()) +
  labs(
    title    = v_title, 
    subtitle = v_preguntas[4], 
    x        = v_empty, 
    y        = v_ylab, 
    fill     = v_fill
  ) +
  guides(color = "none") +
  # Escalas 
  scale_x_continuous(breaks = seq(0, 100, 50),
                     labels = v_niveles) +
  scale_fill_manual(values = c(v_Hokusai[2], v_Hokusai[6])) +
  scale_color_manual(values = c(v_Hokusai[2], v_Hokusai[6])) +
  expand_limits(x = c(0, 110)) +
  tema

g4

ggsave(paste0(out, "g04_", "pena_proporcional", v_formato),
       width = 6, height = 4, device = "png", type = "cairo")

### 3.1.6. Cadena perpetua -----------------------------------------------------


g5 <-
  ggplot(df_data, 
         aes(x = t05_cadena_perp, fill = momento, color = momento)) +
  geom_density(aes(y = ..ndensity..), alpha = 0.3) +
  theme_classic() +
  # scale_y_continuous(label = scales::label_percent()) +
  labs(
    title    = v_title, 
    subtitle = v_preguntas[5], 
    x        = v_empty, 
    y        = v_ylab, 
    fill     = v_fill
  ) +
  guides(color = "none") +
  # Escalas 
  scale_x_continuous(breaks = seq(0, 100, 50),
                     labels = v_niveles) +
  scale_fill_manual(values = c(v_Hokusai[2], v_Hokusai[6])) +
  scale_color_manual(values = c(v_Hokusai[2], v_Hokusai[6])) +
  expand_limits(x = c(0, 110)) +
  tema

g5

ggsave(paste0(out, "g05_", "cadena_perpetua", v_formato),
       width = 6, height = 4, device = "png", type = "cairo")

### 3.1.6. Criminalizar protege derechos ---------------------------------------

g6 <-
  ggplot(df_data, 
         aes(x = t06_vulnera_der, fill = momento, color = momento)) +
  geom_density(aes(y = ..ndensity..), alpha = 0.3) +
  theme_classic() +
  # scale_y_continuous(label = scales::label_percent()) +
  labs(
    title    = v_title, 
    subtitle = v_preguntas[6], 
    x        = v_empty, 
    y        = v_ylab, 
    fill     = v_fill
  ) +
  guides(color = "none") +
  # Escalas 
  scale_x_continuous(breaks = seq(0, 100, 50),
                     labels = v_niveles) +
  scale_fill_manual(values = c(v_Hokusai[2], v_Hokusai[6])) +
  scale_color_manual(values = c(v_Hokusai[2], v_Hokusai[6])) +
  expand_limits(x = c(0, 110)) +
  tema

g6

ggsave(paste0(out, "g06_", "criminalizar_protege", v_formato),
       width = 6, height = 4, device = "png", type = "cairo")

### 3.1.7. Castigar desincentiva -----------------------------------------------

g7 <-
  ggplot(df_data, 
         aes(x = t07_desincentiv, fill = momento, color = momento)) +
  geom_density(aes(y = ..ndensity..), alpha = 0.3) +
  theme_classic() +
  # scale_y_continuous(label = scales::label_percent()) +
  labs(
    title    = v_title, 
    subtitle = v_preguntas[7], 
    x        = v_empty, 
    y        = v_ylab, 
    fill     = v_fill
  ) +
  guides(color = "none") +
  # Escalas 
  scale_x_continuous(breaks = seq(0, 100, 50),
                     labels = v_niveles) +
  scale_fill_manual(values = c(v_Hokusai[2], v_Hokusai[6])) +
  scale_color_manual(values = c(v_Hokusai[2], v_Hokusai[6])) +
  expand_limits(x = c(0, 110)) +
  tema

g7

ggsave(paste0(out, "g07_", "castigo_desincentivo", v_formato),
       width = 6, height = 4, device = "png", type = "cairo")


### 3.1.8. Registro de agresores -----------------------------------------------

g8 <-
  ggplot(df_data, 
         aes(x = t08_registro_ag, fill = momento, color = momento)) +
  geom_density(aes(y = ..ndensity..), alpha = 0.3) +
  theme_classic() +
  # scale_y_continuous(label = scales::label_percent()) +
  labs(
    title    = v_title, 
    subtitle = v_preguntas[8], 
    x        = v_empty, 
    y        = v_ylab, 
    fill     = v_fill
  ) +
  guides(color = "none") +
  # Escalas 
  scale_x_continuous(breaks = seq(0, 100, 50),
                     labels = v_niveles) +
  scale_fill_manual(values = c(v_Hokusai[2], v_Hokusai[6])) +
  scale_color_manual(values = c(v_Hokusai[2], v_Hokusai[6])) +
  expand_limits(x = c(0, 110)) +
  tema

g8

ggsave(paste0(out, "g08_", "registro_agresores", v_formato),
       width = 6, height = 4, device = "png", type = "cairo")

### 3.1.9. Derecho a saber quién cometió delitos -------------------------------

g9 <-
  ggplot(df_data, 
         aes(x = t09_saber_socie, fill = momento, color = momento)) +
  geom_density(aes(y = ..ndensity..), alpha = 0.3) +
  theme_classic() +
  # scale_y_continuous(label = scales::label_percent()) +
  labs(
    title    = v_title, 
    subtitle = v_preguntas[9], 
    x        = v_empty, 
    y        = v_ylab, 
    fill     = v_fill
  ) +
  guides(color = "none") +
  # Escalas 
  scale_x_continuous(breaks = seq(0, 100, 50),
                     labels = v_niveles) +
  scale_fill_manual(values = c(v_Hokusai[2], v_Hokusai[6])) +
  scale_color_manual(values = c(v_Hokusai[2], v_Hokusai[6])) +
  expand_limits(x = c(0, 110)) +
  tema

g9

ggsave(paste0(out, "g09_", "derecho_conocer_agresores", v_formato),
       width = 6, height = 4, device = "png", type = "cairo")

### 3.1.10. Pena de muerte -----------------------------------------------------

g10 <-
  ggplot(df_data, 
         aes(x = t10_carta_antec, fill = momento, color = momento)) +
  geom_density(aes(y = ..ndensity..), alpha = 0.3) +
  theme_classic() +
  # scale_y_continuous(label = scales::label_percent()) +
  labs(
    title    = v_title, 
    subtitle = v_preguntas[10], 
    x        = v_empty, 
    y        = v_ylab, 
    fill     = v_fill
  ) +
  guides(color = "none") +
  # Escalas 
  scale_x_continuous(breaks = seq(0, 100, 50),
                     labels = v_niveles) +
  scale_fill_manual(values = c(v_Hokusai[2], v_Hokusai[6])) +
  scale_color_manual(values = c(v_Hokusai[2], v_Hokusai[6])) +
  expand_limits(x = c(0, 110)) +
  tema

g10

ggsave(paste0(out, "g10_", "carta_antecedentes", v_formato),
       width = 6, height = 4, device = "png", type = "cairo")

### 3.1.11. Amenzar a hombres con cárcel ---------------------------------------


g11 <-
  ggplot(df_data, 
         aes(x = t11_amenaza_dig, fill = momento, color = momento)) +
  geom_density(aes(y = ..ndensity..), alpha = 0.3) +
  theme_classic() +
  # scale_y_continuous(label = scales::label_percent()) +
  labs(
    title    = v_title, 
    subtitle = v_preguntas[11], 
    x        = v_empty, 
    y        = v_ylab, 
    fill     = v_fill
  ) +
  guides(color = "none") +
  # Escalas 
  scale_x_continuous(breaks = seq(0, 100, 50),
                     labels = v_niveles) +
  scale_fill_manual(values = c(v_Hokusai[2], v_Hokusai[6])) +
  scale_color_manual(values = c(v_Hokusai[2], v_Hokusai[6])) +
  expand_limits(x = c(0, 110)) +
  tema

g11

ggsave(paste0(out, "g11_", "amenaza_cárcel", v_formato),
       width = 6, height = 4, device = "png", type = "cairo")


### 3.1.12. Creer víctimas -----------------------------------------------------

g12 <-
  ggplot(df_data, 
         aes(x = t12_creer_victi, fill = momento, color = momento)) +
  geom_density(aes(y = ..ndensity..), alpha = 0.3) +
  theme_classic() +
  # scale_y_continuous(label = scales::label_percent()) +
  labs(
    title    = v_title, 
    subtitle = v_preguntas[12], 
    x        = v_empty, 
    y        = v_ylab, 
    fill     = v_fill
  ) +
  guides(color = "none") +
  # Escalas 
  scale_x_continuous(breaks = seq(0, 100, 50),
                     labels = v_niveles) +
  scale_fill_manual(values = c(v_Hokusai[2], v_Hokusai[6])) +
  scale_color_manual(values = c(v_Hokusai[2], v_Hokusai[6])) +
  expand_limits(x = c(0, 110)) +
  tema

g12

ggsave(paste0(out, "g12_", "creer_víctimas", v_formato),
       width = 6, height = 4, device = "png", type = "cairo")


### 3.1.13. Políticas penales y necesidades ------------------------------------

g13 <-
  ggplot(df_data, 
         aes(x = t13_politica_pe, fill = momento, color = momento)) +
  geom_density(aes(y = ..ndensity..), alpha = 0.3) +
  theme_classic() +
  # scale_y_continuous(label = scales::label_percent()) +
  labs(
    title    = v_title, 
    subtitle = v_preguntas[13], 
    x        = v_empty, 
    y        = v_ylab, 
    fill     = v_fill
  ) +
  guides(color = "none") +
  # Escalas 
  scale_x_continuous(breaks = seq(0, 100, 50),
                     labels = v_niveles) +
  scale_fill_manual(values = c(v_Hokusai[2], v_Hokusai[6])) +
  scale_color_manual(values = c(v_Hokusai[2], v_Hokusai[6])) +
  expand_limits(x = c(0, 110)) +
  tema

g13

ggsave(paste0(out, "g13_", "políticas_penales", v_formato),
       width = 6, height = 4, device = "png", type = "cairo")


### 3.1.14. Cualquier castigo --------------------------------------------------

g14 <-
  ggplot(df_data, 
         aes(x = t14_castigo_vic, fill = momento, color = momento)) +
  geom_density(aes(y = ..ndensity..), alpha = 0.3) +
  theme_classic() +
  # scale_y_continuous(label = scales::label_percent()) +
  labs(
    title    = v_title, 
    subtitle = v_preguntas[13], 
    x        = v_empty, 
    y        = v_ylab, 
    fill     = v_fill
  ) +
  guides(color = "none") +
  # Escalas 
  scale_x_continuous(breaks = seq(0, 100, 50),
                     labels = v_niveles) +
  scale_fill_manual(values = c(v_Hokusai[2], v_Hokusai[6])) +
  scale_color_manual(values = c(v_Hokusai[2], v_Hokusai[6])) +
  expand_limits(x = c(0, 110)) +
  tema

g14

ggsave(paste0(out, "g14_", "cualquier_castigo", v_formato),
       width = 6, height = 4, device = "png", type = "cairo")

## 3.2. Un solo gráfico --------------------------------------------------------

# Guardar nombres de preguntas en texto y como id 
v_topic_text <- names(df_part_crudo)[10:23]
v_topic_id   <- names(df_participantes)[3:16]

# Cambiar a formato largo y añadir texto de las preguntas
df_data <- df_participantes                              %>% 
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
ggplot(df_data, aes(x = respuesta, fill = momento, color = momento)) +
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
  scale_fill_manual( values = c(v_Hokusai[2], v_Hokusai[6])) +
  scale_color_manual(values = c(v_Hokusai[2], v_Hokusai[6])) +
  scale_x_continuous(breaks = seq(0, 100, 50),
                     labels = v_niveles)

ggsave(paste0(out, "g00_", "cambio_distribuciones", v_formato),
       width = 12, height = 12, device = "png", type = "cairo")
