
#1. TABLA DE ESTADÍSTICAS DESCRIPTIVAS POR GRUPO DE PESO
tabla_descriptiva <- Base_datos %>%
  group_by(Peso) %>%
  summarise(
    n = n(),
    Porcentaje = round(100 * n() / nrow(Base_datos), 1),
    Promedio_TG = round(mean(Tiempo_gestación, na.rm = TRUE), 1),
    DE_TG = round(sd(Tiempo_gestación, na.rm = TRUE), 1),
    Min_TG = min(Tiempo_gestación, na.rm = TRUE),
    Max_TG = max(Tiempo_gestación, na.rm = TRUE),
    Promedio_CP = round(mean(Numero_control_prenatal, na.rm = TRUE), 1),
    DE_CP = round(sd(Numero_control_prenatal, na.rm = TRUE), 1),
    Min_CP = min(Numero_control_prenatal, na.rm = TRUE),
    Max_CP = max(Numero_control_prenatal, na.rm = TRUE),
    Promedio_NE = round(mean(Numero_embarazos, na.rm = TRUE), 1),
    DE_NE = round(sd(Numero_embarazos, na.rm = TRUE), 1)
  ) %>%
  mutate(Grupo_Peso = ifelse(Peso == "Si", "Delicado", "Moderado")) %>%
  select(Grupo_Peso, everything(), -Peso)

# 2. DISTRIBUCIÓN DE TIPO DE PARTO
tabla_tipo_parto <- Base_datos %>%
  count(Tipo_parto) %>%
  mutate(Porcentaje = round(100 * n / sum(n), 1),
         Porcentaje_Acumulado = cumsum(Porcentaje))

# 3. DISTRIBUCIÓN DE EDAD DE LA MADRE
tabla_edad_madre <- Base_datos %>%
  count(Edad_madre) %>%
  mutate(Porcentaje = round(100 * n / sum(n), 1),
         Porcentaje_Acumulado = cumsum(Porcentaje))

# 4. TABLA CRUZADA TIPO PARTO vs PESO
tabla_cruzada_parto <- Base_datos %>%
  count(Tipo_parto, Peso) %>%
  group_by(Tipo_parto) %>%
  mutate(Porcentaje_Grupo = round(100 * n / sum(n), 1)) %>%
  ungroup() %>%
  mutate(Grupo_Peso = ifelse(Peso == "Si", "Delicado", "Moderado")) %>%
  select(Tipo_parto, Grupo_Peso, n, Porcentaje_Grupo)

# 5. TABLA CRUZADA EDAD MADRE vs PESO
tabla_cruzada_edad <- Base_datos %>%
  count(Edad_madre, Peso) %>%
  group_by(Edad_madre) %>%
  mutate(Porcentaje_Grupo = round(100 * n / sum(n), 1)) %>%
  ungroup() %>%
  mutate(Grupo_Peso = ifelse(Peso == "Si", "Delicado", "Moderado")) %>%
  select(Edad_madre, Grupo_Peso, n, Porcentaje_Grupo)

# 6. RESUMEN DE VARIABLES NUMÉRICAS GENERAL
tabla_resumen_general <- Base_datos %>%
  summarise(
    Variable = "Tiempo_gestación",
    Media = round(mean(Tiempo_gestación, na.rm = TRUE), 1),
    DE = round(sd(Tiempo_gestación, na.rm = TRUE), 1),
    Mínimo = min(Tiempo_gestación, na.rm = TRUE),
    Máximo = max(Tiempo_gestación, na.rm = TRUE)
  ) %>%
  bind_rows(
    Base_datos %>%
      summarise(
        Variable = "Controles_prenatales",
        Media = round(mean(Numero_control_prenatal, na.rm = TRUE), 1),
        DE = round(sd(Numero_control_prenatal, na.rm = TRUE), 1),
        Mínimo = min(Numero_control_prenatal, na.rm = TRUE),
        Máximo = max(Numero_control_prenatal, na.rm = TRUE)
      )
  ) %>%
  bind_rows(
    Base_datos %>%
      summarise(
        Variable = "Número_embarazos",
        Media = round(mean(Numero_embarazos, na.rm = TRUE), 1),
        DE = round(sd(Numero_embarazos, na.rm = TRUE), 1),
        Mínimo = min(Numero_embarazos, na.rm = TRUE),
        Máximo = max(Numero_embarazos, na.rm = TRUE)
      )
  )

# VER LAS TABLAS
print(tabla_descriptiva)
print(tabla_tipo_parto)
print(tabla_edad_madre)
print(tabla_cruzada_parto)
print(tabla_cruzada_edad)
print(tabla_resumen_general)

#----------visualizaciones---------


library(kableExtra)
library(dplyr)
library(tidyr)

# 1. TABLA COMPARATIVA PROFESIONAL DE VARIABLES NUMÉRICAS
tabla_comparativa <- Base_datos %>%
  group_by(Peso) %>%
  summarise(
    n = n(),
    Porcentaje = round(100 * n() / nrow(Base_datos), 1),
    # Tiempo de gestación
    TG_Media = round(mean(Tiempo_gestación, na.rm = TRUE), 1),
    TG_DE = round(sd(Tiempo_gestación, na.rm = TRUE), 1),
    TG_Min = min(Tiempo_gestación, na.rm = TRUE),
    TG_Max = max(Tiempo_gestación, na.rm = TRUE),
    # Controles prenatales
    CP_Media = round(mean(Numero_control_prenatal, na.rm = TRUE), 1),
    CP_DE = round(sd(Numero_control_prenatal, na.rm = TRUE), 1),
    CP_Min = min(Numero_control_prenatal, na.rm = TRUE),
    CP_Max = max(Numero_control_prenatal, na.rm = TRUE),
    # Número de embarazos
    NE_Media = round(mean(Numero_embarazos, na.rm = TRUE), 1),
    NE_DE = round(sd(Numero_embarazos, na.rm = TRUE), 1),
    NE_Min = min(Numero_embarazos, na.rm = TRUE),
    NE_Max = max(Numero_embarazos, na.rm = TRUE)
  ) %>%
  mutate(Grupo_Peso = ifelse(Peso == "Si", "Delicado", "Moderado")) %>%
  select(Grupo_Peso, n, Porcentaje, everything(), -Peso)

# Formateo profesional de la tabla
kable(tabla_comparativa, 
      col.names = c("Grupo de Peso", "n", "%", 
                    "Media", "DE", "Mín", "Máx",
                    "Media", "DE", "Mín", "Máx", 
                    "Media", "DE", "Mín", "Máx"),
      align = c("l", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c"),
      caption = "Tabla 1. Estadísticas Descriptivas por Grupo de Peso al Nacer") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = TRUE,
                font_size = 12,
                latex_options = "hold_position") %>%
  add_header_above(c(" " = 3, 
                     "Tiempo Gestación (semanas)" = 4, 
                     "Controles Prenatales" = 4, 
                     "Número Embarazos" = 4),
                   background = c("#FFFFFF", "#FFFFFF", "#FFFFFF", 
                                  "#2C3E50", "#2C3E50", "#2C3E50"),
                   color = "white") %>%
  row_spec(0, background = "#34495E", color = "white", bold = TRUE) %>%
  row_spec(1, background = "#F8F9FA") %>%
  row_spec(2, background = "#FFFFFF") %>%
  column_spec(1, bold = TRUE)

# 2. TABLA CRUZADA ELEGANTE - TIPO DE PARTO vs PESO
tabla_parto_elegante <- Base_datos %>%
  count(Tipo_parto, Peso) %>%
  group_by(Tipo_parto) %>%
  mutate(Porcentaje_fila = round(100 * n / sum(n), 1)) %>%
  ungroup() %>%
  group_by(Peso) %>%
  mutate(Porcentaje_columna = round(100 * n / sum(n), 1)) %>%
  ungroup() %>%
  mutate(Grupo_Peso = ifelse(Peso == "Si", "Delicado", "Moderado"),
         Porcentaje_fila = paste0(Porcentaje_fila, "%"),
         Porcentaje_columna = paste0(Porcentaje_columna, "%")) %>%
  select(Tipo_parto, Grupo_Peso, n, Porcentaje_fila, Porcentaje_columna) %>%
  pivot_wider(names_from = Grupo_Peso, 
              values_from = c(n, Porcentaje_fila, Porcentaje_columna),
              names_sep = "_") %>%
  mutate(Total = n_Delicado + n_Moderado,
         Porcentaje_Total = round(100 * Total / sum(Total), 1))

kable(tabla_parto_elegante,
      col.names = c("Tipo de Parto", "n", "n", "% Fila", "% Fila", "% Columna", "% Columna", "Total", "% Total"),
      caption = "Tabla 2. Distribución Cruzada: Tipo de Parto vs Grupo de Peso") %>%
  kable_styling(bootstrap_options = c("striped", "hover"),
                full_width = TRUE,
                font_size = 12) %>%
  add_header_above(c(" " = 1, 
                     "Delicado" = 3, 
                     "Moderado" = 3, 
                     " " = 2),
                   background = c("#FFFFFF", "#E74C3C", "#2ECC71", "#FFFFFF"),
                   color = "white") %>%
  row_spec(0, background = "#34495E", color = "white", bold = TRUE) %>%
  column_spec(1, bold = TRUE)

# 3. TABLA CRUZADA ESTILO ACADÉMICO - EDAD MATERNA vs PESO

tabla_edad_academica <- Base_datos %>%
  count(Edad_madre, Peso) %>%
  group_by(Edad_madre) %>%
  mutate(Porcentaje_fila = round(100 * n / sum(n), 1),
         Porcentaje_fila = paste0(Porcentaje_fila, "%")) %>%
  ungroup() %>%
  mutate(Grupo_Peso = ifelse(Peso == "Si", "Delicado", "Moderado")) %>%
  select(Edad_madre, Grupo_Peso, n, Porcentaje_fila) %>%
  pivot_wider(
    names_from = Grupo_Peso, 
    values_from = c(n, Porcentaje_fila),
    names_sep = " - "
  ) %>%
  # Reemplazar NAs con valores por defecto
  mutate(
    `n - Delicado` = ifelse(is.na(`n - Delicado`), 0, `n - Delicado`),
    `n - Moderado` = ifelse(is.na(`n - Moderado`), 0, `n - Moderado`),
    `Porcentaje_fila - Delicado` = ifelse(is.na(`Porcentaje_fila - Delicado`), "0%", `Porcentaje_fila - Delicado`),
    `Porcentaje_fila - Moderado` = ifelse(is.na(`Porcentaje_fila - Moderado`), "0%", `Porcentaje_fila - Moderado`)
  ) %>%
  mutate(
    Total = `n - Delicado` + `n - Moderado`,
    `% Total` = round(100 * Total / sum(Total), 1)
  )

# Verificar que las columnas existen
print("Columnas disponibles:")
print(colnames(tabla_edad_academica))

# Mostrar tabla corregida
kable(tabla_edad_academica,
      col.names = c("Edad de la Madre", "n", "%", "n", "%", "Total", "% Total"),
      caption = "Tabla 3. Distribución por Edad Materna y Grupo de Peso") %>%
  kable_styling(bootstrap_options = c("striped", "hover"),
                full_width = TRUE,
                font_size = 11) %>%
  add_header_above(c(" " = 1, 
                     "Delicado" = 2, 
                     "Moderado" = 2, 
                     " " = 2),
                   background = c("#FFFFFF", "#E74C3C", "#2ECC71", "#FFFFFF"),
                   color = "white") %>%
  row_spec(0, background = "#2C3E50", color = "white", bold = TRUE) %>%
  column_spec(1, bold = TRUE, width = "3cm")

# 4. TABLA RESUMEN ELEGANTE COMPACTA
tabla_resumen_elegante <- Base_datos %>%
  group_by(Peso) %>%
  summarise(
    n = n(),
    across(c(Tiempo_gestación, Numero_control_prenatal, Numero_embarazos),
           list(
             Media = ~round(mean(., na.rm = TRUE), 1),
             DE = ~round(sd(., na.rm = TRUE), 1)
           )
    )
  ) %>%
  mutate(
    Grupo_Peso = ifelse(Peso == "Si", "Delicado", "Moderado"),
    Porcentaje = round(100 * n / sum(n), 1)
  ) %>%
  select(Grupo_Peso, n, Porcentaje, everything(), -Peso) %>%
  mutate(
    `Tiempo Gestación` = paste0(Tiempo_gestación_Media, " (", Tiempo_gestación_DE, ")"),
    `Controles Prenatales` = paste0(Numero_control_prenatal_Media, " (", Numero_control_prenatal_DE, ")"),
    `Número Embarazos` = paste0(Numero_embarazos_Media, " (", Numero_embarazos_DE, ")")
  ) %>%
  select(Grupo_Peso, n, Porcentaje, `Tiempo Gestación`, `Controles Prenatales`, `Número Embarazos`)

# Mostrar tabla corregida
kable(tabla_resumen_elegante,
      col.names = c("Grupo de Peso", "n", "%", "Tiempo Gestación", "Controles Prenatales", "Número Embarazos"),
      caption = "Tabla 4. Resumen Estadístico Compacto (Media ± DE)") %>%
  kable_styling(bootstrap_options = c("striped", "hover"),
                full_width = TRUE,
                font_size = 12) %>%
  row_spec(0, background = "#34495E", color = "white", bold = TRUE) %>%
  row_spec(1:2, background = c("#F8F9FA", "#FFFFFF")) %>%
  column_spec(1, bold = TRUE) %>%
  footnote(general = "Nota: Los valores representan Media (Desviación Estándar). El tiempo de gestación se mide en semanas.",
           general_title = " ")

#VISUALIZACIONES DE GRAFICAS

#----------visualizaciones CON PLOTLY YGGPLOT CORREGIDAS---------

library(plotly)
library(ggplot2)

#---------- VISUALIZACIONES PROFESIONALES CON PLOTLY ----------

library(plotly)
library(viridis)

# 1. GRÁFICO DE PIE MEJORADO - DISTRIBUCIÓN DEL PESO
frecuencias_peso <- Base_datos %>%
  count(Peso) %>%
  mutate(
    Grupo = ifelse(Peso == "Si", "Delicado", "Moderado"),
    Porcentaje = round(100 * n / sum(n), 1),
    Etiqueta = paste0(Grupo, "\n", n, " (", Porcentaje, "%)")
  )

plot_peso <- plot_ly(frecuencias_peso, 
                     labels = ~Grupo, 
                     values = ~n, 
                     type = 'pie',
                     text = ~Etiqueta,
                     textinfo = 'text',
                     hoverinfo = 'text',
                     marker = list(colors = c('#E74C3C', '#27AE60'),
                                   line = list(color = '#FFFFFF', width = 2)),
                     hole = 0.4) %>%  # Donut chart
  layout(title = list(text = "<b>DISTRIBUCIÓN DEL PESO AL NACER</b><br>Bogotá 2024", 
                      x = 0.5, xanchor = 'center'),
         showlegend = TRUE,
         annotations = list(text = "Peso al Nacer", 
                            x = 0.5, y = 0.5, 
                            font = list(size = 16, color = 'black'), 
                            showarrow = FALSE),
         font = list(family = "Arial", size = 12))

plot_peso

# 2. HISTOGRAMA MEJORADO - TIEMPO DE GESTACIÓN
plot_gestacion <- plot_ly(alpha = 0.7) %>%
  add_histogram(data = Base_datos %>% filter(Peso == "Si"),
                x = ~Tiempo_gestación,
                name = "Delicado", 
                marker = list(color = '#E74C3C',
                              line = list(color = '#C0392B', width = 1)),
                nbinsx = 20) %>%
  add_histogram(data = Base_datos %>% filter(Peso == "No"),
                x = ~Tiempo_gestación,
                name = "Moderado",
                marker = list(color = '#27AE60',
                              line = list(color = '#229954', width = 1)),
                nbinsx = 20) %>%
  layout(title = list(text = "<b>DISTRIBUCIÓN DEL TIEMPO DE GESTACIÓN</b><br>Por Grupo de Peso", 
                      x = 0.5, xanchor = 'center'),
         xaxis = list(title = "<b>Tiempo de Gestación (semanas)</b>",
                      gridcolor = '#ECF0F1',
                      zerolinecolor = '#BDC3C7'),
         yaxis = list(title = "<b>Frecuencia</b>",
                      gridcolor = '#ECF0F1'),
         plot_bgcolor = '#F8F9FA',
         paper_bgcolor = '#FFFFFF',
         barmode = "overlay",
         legend = list(x = 0.02, y = 0.98, 
                       bgcolor = 'rgba(255,255,255,0.8)',
                       bordercolor = '#BDC3C7'))

plot_gestacion

# 3. GRÁFICO DE BARRAS AGRUPADAS MEJORADO - TIPO DE PARTO
datos_parto <- Base_datos %>%
  count(Tipo_parto, Peso) %>%
  mutate(Grupo_Peso = ifelse(Peso == "Si", "Delicado", "Moderado"))

plot_parto <- plot_ly(datos_parto, 
                      x = ~Tipo_parto, 
                      y = ~n, 
                      color = ~Grupo_Peso,
                      colors = c("Delicado" = "#E74C3C", "Moderado" = "#27AE60"),
                      type = 'bar',
                      text = ~paste("<b>", Grupo_Peso, "</b><br>",
                                    "Tipo parto:", Tipo_parto, "<br>",
                                    "Nacimientos:", n),
                      hoverinfo = 'text',
                      marker = list(line = list(color = 'rgba(0,0,0,0.2)', width = 1))) %>%
  layout(title = list(text = "<b>DISTRIBUCIÓN POR TIPO DE PARTO</b><br>Comparación entre Grupos de Peso", 
                      x = 0.5, xanchor = 'center'),
         xaxis = list(title = "<b>Tipo de Parto</b>",
                      gridcolor = '#ECF0F1'),
         yaxis = list(title = "<b>Número de Nacimientos</b>",
                      gridcolor = '#ECF0F1'),
         plot_bgcolor = '#F8F9FA',
         paper_bgcolor = '#FFFFFF',
         barmode = 'group',
         legend = list(x = 0.02, y = 0.98))

plot_parto

# 5 GRÁFICO DE VIOLÍN MEJORADO (CORREGIDO)
plot_violin <- plot_ly(Base_datos, 
                       x = ~Tipo_parto, 
                       y = ~Tiempo_gestación, 
                       color = ~Peso,
                       colors = c("Si" = "#E74C3C", "No" = "#27AE60"),
                       type = 'violin',
                       box = list(visible = TRUE, width = 0.3),
                       meanline = list(visible = TRUE),
                       points = FALSE,
                       spanmode = "hard",
                       opacity = 0.7,  # Transparencia general
                       text = ~paste("<b>", ifelse(Peso == "Si", "Delicado", "Moderado"), "</b><br>",
                                     "Tipo parto:", Tipo_parto, "<br>",
                                     "Gestación:", Tiempo_gestación, "semanas"),
                       hoverinfo = 'text') %>%
  layout(title = list(text = "<b>DISTRIBUCIÓN DE TIEMPO DE GESTACIÓN</b><br>Por Tipo de Parto y Grupo de Peso", 
                      x = 0.5, xanchor = 'center'),
         xaxis = list(title = "<b>Tipo de Parto</b>",
                      gridcolor = '#ECF0F1'),
         yaxis = list(title = "<b>Tiempo de Gestación (semanas)</b>",
                      gridcolor = '#ECF0F1'),
         plot_bgcolor = '#F8F9FA',
         paper_bgcolor = '#FFFFFF')

plot_violin

# 6. GRÁFICO DE DISPERSIÓN MEJORADO
plot_dispersion <- plot_ly(Base_datos, 
                           x = ~Numero_control_prenatal, 
                           y = ~Tiempo_gestación,
                           color = ~Peso,
                           colors = c("Si" = "#E74C3C", "No" = "#27AE60"),
                           type = 'scatter',
                           mode = 'markers',
                           size = ~Numero_embarazos,
                           sizes = c(2, 12),
                           marker = list(opacity = 0.7, 
                                         sizemode = 'diameter',
                                         line = list(color = 'rgba(0,0,0,0.2)', width = 1)),
                           text = ~paste("<b>", ifelse(Peso == "Si", "Delicado", "Moderado"), "</b><br>",
                                         "Controles:", Numero_control_prenatal, "<br>",
                                         "Gestación:", Tiempo_gestación, "semanas<br>",
                                         "Embarazos:", Numero_embarazos),
                           hoverinfo = 'text') %>%
  layout(title = list(text = "<b>RELACIÓN ENTRE CONTROLES PRENATALES Y GESTACIÓN</b>", 
                      x = 0.5, xanchor = 'center'),
         xaxis = list(title = "<b>Número de Controles Prenatales</b>",
                      gridcolor = '#ECF0F1'),
         yaxis = list(title = "<b>Tiempo de Gestación (semanas)</b>",
                      gridcolor = '#ECF0F1'),
         plot_bgcolor = '#F8F9FA',
         paper_bgcolor = '#FFFFFF',
         showlegend = TRUE)

plot_dispersion


# 7. MAPA DE CALOR PROFESIONAL
nums <- Base_datos %>%
  select(Tiempo_gestación, Numero_control_prenatal, Numero_embarazos) %>%
  na.omit()

corr_matrix <- cor(nums)
nombres_bonitos <- c("Tiempo Gestación", "Controles Prenatales", "N° Embarazos")

plot_correlacion <- plot_ly(
  x = nombres_bonitos,
  y = nombres_bonitos,
  z = corr_matrix,
  type = "heatmap",
  colors = colorRamp(c("#E74C3C", "#FFFFFF", "#27AE60")),
  colorbar = list(title = "Correlación", 
                  titleside = "right",
                  tickvals = c(-1, -0.5, 0, 0.5, 1),
                  ticktext = c("-1.0", "-0.5", "0.0", "0.5", "1.0")),
  text = matrix(paste0("Correlación: ", round(corr_matrix, 3)),
                nrow = nrow(corr_matrix), ncol = ncol(corr_matrix)),
  hoverinfo = 'text'
) %>%
  layout(title = list(text = "<b>MATRIZ DE CORRELACIÓN</b><br>Variables Numéricas del Estudio", 
                      x = 0.5, xanchor = 'center'),
         xaxis = list(title = "", side = "bottom"),
         yaxis = list(title = ""),
         margin = list(l = 100, r = 100, b = 100, t = 100))


plot_correlacion

# 8. GRÁFICO DE CAJA MEJORADO
plot_box <- plot_ly(Base_datos, 
                    y = ~Tiempo_gestación, 
                    x = ~Peso, 
                    color = ~Peso,
                    colors = c("Si" = "#E74C3C", "No" = "#27AE60"),
                    type = "box",
                    quartilemethod = "linear",
                    boxpoints = "outliers",
                    marker = list(color = 'rgba(0,0,0,0.6)', size = 3),
                    line = list(width = 2),
                    text = ~paste("<b>", ifelse(Peso == "Si", "Delicado", "Moderado"), "</b><br>",
                                  "Gestación:", Tiempo_gestación, "semanas"),
                    hoverinfo = 'text') %>%
  layout(title = list(text = "<b>COMPARACIÓN DEL TIEMPO DE GESTACIÓN</b><br>Entre Grupos de Peso", 
                      x = 0.5, xanchor = 'center'),
         xaxis = list(title = "<b>Grupo de Peso</b>",
                      ticktext = list("Delicado", "Moderado"),
                      tickvals = list("Si", "No")),
         yaxis = list(title = "<b>Tiempo de Gestación (semanas)</b>",
                      gridcolor = '#ECF0F1'),
         plot_bgcolor = '#F8F9FA',
         paper_bgcolor = '#FFFFFF')

plot_box

# 9. GRÁFICO DE BARRAS HORIZONTAL - COMPARATIVA POR EDAD
tabla_edad_resumen <- Base_datos %>%
  count(Edad_madre, Peso) %>%
  group_by(Edad_madre) %>%
  mutate(Total_edad = sum(n),
         Porcentaje = round(100 * n / Total_edad, 1)) %>%
  ungroup() %>%
  mutate(Grupo_Peso = ifelse(Peso == "Si", "Delicado", "Moderado"),
         Edad_madre = factor(Edad_madre, levels = niveles_edad))

plot_edad_horizontal <- plot_ly(tabla_edad_resumen, 
                                x = ~Porcentaje, 
                                y = ~Edad_madre, 
                                color = ~Grupo_Peso,
                                colors = c("Delicado" = "#E74C3C", "Moderado" = "#27AE60"),
                                type = 'bar',
                                orientation = 'h',
                                text = ~paste("<b>", Grupo_Peso, "</b><br>",
                                              "Edad:", Edad_madre, "<br>",
                                              "Porcentaje:", Porcentaje, "%<br>",
                                              "N:", n),
                                hoverinfo = 'text',
                                marker = list(line = list(color = 'rgba(0,0,0,0.2)', width = 1))) %>%
  layout(title = list(text = "<b>DISTRIBUCIÓN PORCENTUAL POR EDAD MATERNA</b>", 
                      x = 0.5, xanchor = 'center'),
         xaxis = list(title = "<b>Porcentaje (%)</b>",
                      gridcolor = '#ECF0F1',
                      range = c(0, 100)),
         yaxis = list(title = "<b>Edad de la Madre</b>"),
         plot_bgcolor = '#F8F9FA',
         paper_bgcolor = '#FFFFFF',
         barmode = 'stack',
         legend = list(x = 0.02, y = 0.98))

plot_edad_horizontal
