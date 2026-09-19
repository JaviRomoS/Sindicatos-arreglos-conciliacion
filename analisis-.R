rm(list = ls())
options(scipen = 999)

# =========================================================================
# 1. Cargar librerías -----------------------------------------------------
# =========================================================================

library(pacman)
pacman::p_load(tidyverse, haven, sjmisc, sjPlot, summarytools, texreg, ggplot2, htmltools,ggeffects, mediation )

# =========================================================================
# Abrir bases -------------------------------------------------------------
# =========================================================================

filt_data <- readRDS("output/datosfiltrados.rds") # Base filtrada con empresas que tengan 20 mas trabajadoras

data <- readRDS("output/proc.rds") # Base con toda las empresas de la muestra

# =========================================================================
# 3. Seleccionar variables ------------------------------------------------
# =========================================================================

filt_data <- filt_data %>% dplyr::select(guarderia,
                               densidad_sindical,femin_fuerza_laboral,
                               tamano_empresa, actividad_economica, prop_indef,
                               conflicto_legal,conflicto_disruptivo) %>% na.omit()

data <- data %>% dplyr::select(densidad_sindical, teletrabajo,
                               femin_fuerza_laboral,
                               conflicto_legal, conflicto_disruptivo,
                               tamano_empresa, prop_indef, actividad_economica) %>% na.omit()

# =========================================================================
# 4. Funciones ------------------------------------------------------------
# =========================================================================

modelo_stats <- function(modelo) {
  data.frame(
    AIC = AIC(modelo),
    BIC = BIC(modelo),
    LogLik = as.numeric(logLik(modelo)),
    Deviance = deviance(modelo),
    N = nobs(modelo)
  )
}


# =========================================================================
# 5. Modelos GUARDERIAS ---------------------------------------------------
# =========================================================================

   ## 5.1. HIPOTESIS 1 ----------------------------------------------------

m1a <- glm(guarderia ~ densidad_sindical + 
               femin_fuerza_laboral + tamano_empresa + prop_indef + 
               actividad_economica, data = filt_data, family = binomial())

summary(m1a)

modelo_stats(m1a)

   # Grafico probabilidades predichas modelo 1a

plot_m1a <- ggplot(
  ggpredict(
    m1a,
    terms = "densidad_sindical [0:100 by=5]",
    condition = list(tamano_empresa = 3,
                     actividad_economica = "Enseñanza")),
  aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              fill = "#db4a4a", alpha = 0.2, na.rm = TRUE) +
  geom_line(color = "#db4a4a", linewidth = 1.1, na.rm = TRUE) +
  scale_x_continuous(
    breaks = seq(0, 100, by = 25),
    labels = function(x) paste0(x, "%")) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1, scale = 100),
    breaks = seq(0.4, 1, by = 0.1),
    limits = c(0.4, 1),
    expand = expansion(mult = c(0, 0.05))) +
  labs(
    x = "Densidad sindical",
    y = NULL,
    title = "a) Provisión de guarderías") +
  theme_bw(base_size = 16) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "plain", size = 25, hjust = 0), 
    axis.title.x = element_text(size = 18, margin = margin(t = 12)), 
    axis.text = element_text(size = 18),                             
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    panel.border = element_rect(color = "grey70", fill = NA, linewidth = 0.5))

ggsave("Output/plots/plot_m1a.png", plot = plot_m1a, width = 10, height = 6.5, dpi = 300)

   ## 5.2. HIPOTESIS 2 ----------------------------------------------------

   # Densidad sindical -> conflicto legal

model2a_1 <- lm(conflicto_legal ~ densidad_sindical +
                  femin_fuerza_laboral+ tamano_empresa + prop_indef + actividad_economica, 
                data = filt_data)
summary(model2a_1) # Significativo

   # Modelo 2 GUARDERIAS (se agrega el conflicto legal)
m2a <- glm(guarderia ~ densidad_sindical + 
             conflicto_legal + 
             femin_fuerza_laboral+ tamano_empresa + prop_indef + actividad_economica, 
           data = filt_data, family = binomial())

modelo_stats(m2a)

   # Tabla para comparar m1a y m2a
screenreg(list(m1a, m2a),
          custom.model.names = c("Modelo 1a", "Modelo 2a"),
          digits = 3, 
          single.row = TRUE,
          include.aic = TRUE, include.bic = TRUE, include.loglik = TRUE, include.nobs = TRUE)

    # Mediacion

# med_salacuna_legal<- mediate(
#   model2a_1,
#   m2a,
#   treat = "densidad_sindical",
#   mediator = "conflicto_legal",
#   boot = TRUE,
#   sims = 5000)

  # mediation <- summary(med_salacuna_legal)
  # mediation <- paste(capture.output(mediation), collapse = "\n")

  # save_html(
  #   htmltools::tags$pre(capture.output(mediation)),
  #   file = "output/tables/mediation_salacuna_legal.html")
  
   # Probabilidades predichas (para ver cuánto aumentan las probabilidades de existir guarderia a medida que aumenta la densidad sindical en modelos sin y con la incorporacion del mediador)

ggpredict(
  m1a,
  terms = "densidad_sindical [0:100 by=0.25]",
  condition = list(tamano_empresa = 3,
                   actividad_economica = "Enseñanza")) # Sin incluir conflicto legal

ggpredict(
  m2a,
  terms = "densidad_sindical [0:100 by=0.25]",
  condition = list(tamano_empresa = 3,
                   actividad_economica = "Enseñanza")) # Incluyendo conflicto legal

   # Probabilidades predichas (cuanto aumenta la probabilidad de tener guardería según el aumento de la movilizacion legal)

ggpredict(
  m2a,
  terms = "conflicto_legal",
  condition = list(tamano_empresa = 3,
                   actividad_economica = "Enseñanza"))

   ## 5.3. HIPOTESIS 3 ----------------------------------------------------

   # Densidad sindical -> conflicto disruptivo

model3a_1 <- lm(conflicto_disruptivo ~ densidad_sindical +
                  femin_fuerza_laboral+ tamano_empresa + prop_indef + actividad_economica, 
                data = filt_data)
summary(model3a_1)

   # Modelo 3 GUARDERIAS (se agrega el conflicto disruptivo)
m3a <- glm(guarderia ~ densidad_sindical +
             conflicto_disruptivo +
             femin_fuerza_laboral + tamano_empresa + prop_indef + actividad_economica, 
           data = filt_data, family = binomial())

modelo_stats(m3a)

   # Tabla para comparar m1a y m3a
screenreg(list(m1a, m3a),
          custom.model.names = c("Modelo 1a", "Modelo 3a"),
          digits = 3, 
          single.row = TRUE,
          include.aic = TRUE, include.bic = TRUE, include.loglik = TRUE, include.nobs = TRUE)

# Se rechaza la hipotesis 3 para el modelo de guarderia, ya que no hay efecto de conflicto disruptivo sobre densidad sindical

# =========================================================================
# 6. Modelos TELETRABAJO --------------------------------------------------
# =========================================================================

   ## 6.1. HIPOTESIS 1 ----------------------------------------------------

m1b <- glm(teletrabajo ~ densidad_sindical + 
             femin_fuerza_laboral + tamano_empresa + prop_indef + 
             actividad_economica, data = data, family = binomial())

summary(m1b)

modelo_stats(m1b)

# Probabilidades predichas

ggpredict(
  m1b,
  terms = "densidad_sindical [0:100 by=0.25]",
  condition = list(tamano_empresa = 3,
                   actividad_economica = "Enseñanza")) # Sin incluir conflicto legal
# Grafico probabilidades predichas modelo 1b

plot_m1b <- ggplot(
  ggpredict(
    m1b,
    terms = "densidad_sindical [0:100 by=5]",
    condition = list(tamano_empresa = 3,
                     actividad_economica = "Enseñanza")),
  aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              fill = "#db4a4a", alpha = 0.2, na.rm = TRUE) +
  geom_line(color = "#db4a4a", linewidth = 1.1, na.rm = TRUE) +
  scale_x_continuous(
    breaks = seq(0, 100, by = 25),
    labels = function(x) paste0(x, "%")) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1, scale = 100),
    breaks = seq(0.4, 1, by = 0.1),
    limits = c(0.35, 1),
    expand = expansion(mult = c(0, 0.05))) +
  labs(
    x = "Densidad sindical",
    y = NULL,
    title = "b) Teletrabajo") +
  theme_bw(base_size = 16) +   
  theme(
    legend.position = "none",
    plot.title = element_text(face = "plain", size = 25, hjust = 0), 
    axis.title.x = element_text(size = 18, margin = margin(t = 12)), 
    axis.text = element_text(size = 18),                             
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    panel.border = element_rect(color = "grey70", fill = NA, linewidth = 0.5))

ggsave("Output/plots/plot_m1b.png", plot = plot_m1b, width = 10, height = 6.5, dpi = 300)

   ## 5.2. HIPOTESIS 2 ----------------------------------------------------

# Densidad sindical -> conflicto legal

model2b_1 <- lm(conflicto_legal ~ densidad_sindical +
                  femin_fuerza_laboral+ tamano_empresa + prop_indef + actividad_economica, 
                data = data)
summary(model2b_1) # Significativo

# Modelo 2 TELETRABAJO (se agrega el conflicto legal)
m2b <- glm(teletrabajo ~ densidad_sindical + 
             conflicto_legal + 
             femin_fuerza_laboral+ tamano_empresa + prop_indef + actividad_economica, 
           data = data, family = binomial())

summary(m2b) # Conflicto legal no significativo

modelo_stats(m2b)

# Se rechaza la hipotesis 2 para el modelo de teletrabajo, ya que no hay efecto de conflicto legal sobre densidad sindical

## 5.3. HIPOTESIS 3 ----------------------------------------------------

# Densidad sindical -> conflicto disruptivo

model3b_1 <- lm(conflicto_disruptivo ~ densidad_sindical +
                  femin_fuerza_laboral+ tamano_empresa + prop_indef + actividad_economica, 
                data = data)
summary(model3b_1) # Significativo

# Modelo 3 GUARDERIAS (se agrega el conflicto disruptivo)
m3b <- glm(teletrabajo ~ densidad_sindical +
             conflicto_disruptivo +
             femin_fuerza_laboral + tamano_empresa + prop_indef + actividad_economica, 
           data = data, family = binomial())

summary(m3b) # No significativo

modelo_stats(m3b)

# Se rechaza la hipotesis 3 para el modelo de teletrabajo, ya que no hay efecto de conflicto disruptivo sobre densidad sindical

# =========================================================================
# 7. Tablas ---------------------------------------------------------------
# =========================================================================

## 7.2. Tablas descriptivas

view(dfSummary(filt_data, headings=FALSE, graph.col = FALSE)) # Muestra guarderias

view(dfSummary(data, headings=FALSE, graph.col = FALSE)) # Muestra teletrabajo

## 7.1. Tablas regresion

htmlreg(
  list(m1a, m2a, m3a, m1b, m2b, m3b),
  custom.model.names = c("Modelo 1a", "Modelo 2a", "Modelo 3a", "Modelo 1b", "Modelo 2b", "Modelo 3b"),
  digits = 3, 
  single.row = TRUE,
  include.aic = TRUE, include.bic = TRUE, include.loglik = TRUE, include.nobs = TRUE,
  file = "Output/tables/tabla2_paper.html", 
  doctype = TRUE, 
  html.tag = TRUE)

## 7.2. Tabla anexa

htmlreg(
  list(model2a_1, model3a_1, model2b_1, model3b_1),
  custom.model.names = c("Modelo 2a_1", "Modelo 3a_1", "Modelo 2b_1", "Modelo 3b_1"),
  digits = 3, 
  single.row = TRUE,
  include.aic = TRUE, include.bic = TRUE, include.loglik = TRUE, include.nobs = TRUE,
  file = "Output/tables/tabla-anexa_paper.html", 
  doctype = TRUE, 
  html.tag = TRUE)

