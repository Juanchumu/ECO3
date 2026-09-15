# ============================================================
# LIBRERIAS NECESARIAS
# ============================================================

library(dplyr)
library(tidyr)
library(tidyverse)

# ============================================================
# CARGA DE DATOS
# ============================================================

datos <- read.csv("TP1_eco3_26 - Macro_Meso.csv")

datos[,-(1:2)]
#reemplazar los NA con 0
datos <- replace(datos, is.na(datos),  0  )

tapply(datos[-1:4], datos$LOTE, mean)
# ============================================================
# ANALISIS DE DATOS POR LOTE Y AÑO
# ============================================================

# Columnas que identifican cada observación
columnas_id <- c("ID_sitio", "AÑO", "LOTE", "PUNTO")

# Todas las demás columnas son especies
especies <- setdiff(names(datos), columnas_id)

#Nota Juan: utilizo de manera directa la seleccion para evitar
#problemas en caso de que se reordene la tabla de datos

# ============================================================
# 1. PROMEDIO POR LOTE Y AÑO
# ============================================================


promedios <- datos %>%
  group_by(AÑO, LOTE) %>%
  summarise(
    across(
      all_of(especies),
      mean,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

print(promedios)

# ============================================================
# 2. LOTES COMO COLUMNAS
# ============================================================

promedios_largo <- promedios %>%
  pivot_longer(
    cols = all_of(especies),
    names_to = "ESPECIE",
    values_to = "PROMEDIO"
  )

promedios_ancho <- promedios_largo %>%
  pivot_wider(
    names_from = LOTE,
    values_from = PROMEDIO
  )

print(promedios_ancho)


# ============================================================
# 3. BOXPLOT POR LOTE Y AÑO
# ============================================================

#para mostrar 4 graficos
par(mfrow = c(2, 2))
#despues con la flechita se pueden ver todos
for (especie in especies) {
    boxplot(
    datos[[especie]] ~ interaction(datos$LOTE, datos$AÑO),
    main = paste("Especie:", especie," "),
    #No se me ocurre un mejor titulo
    #Individuo:
    #especie
    xlab = "Lote y Año",
    ylab = "Abundancia",
    las = 2
  )
  
}

# Para volver a tener un solo grafico en Rstudio 
par(mfrow = c(1, 1))


# ============================================================
# 4. AGGREGATE - PROMEDIO
# ============================================================

promedio <- aggregate(
  datos[especies],
  by = list(
    AÑO = datos$AÑO,
    LOTE = datos$LOTE
  ),
  FUN = mean,
  na.rm = TRUE
)

print(promedio)

datos_largo <- datos %>%
  pivot_longer(
    cols = -c(AÑO, LOTE, PUNTO, ID_sitio),
    names_to = "Especie",
    values_to = "Abundancia"
  )

ggplot(datos_largo,
       aes(x = Especie, y = Abundancia, fill = factor(AÑO))) +
  geom_col(position = position_dodge(width = 0.8),
           width = 0.7) +
  facet_wrap(~ LOTE) +
  labs(
    x = "Especie",
    y = "Abundancia",
    fill = "Año"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# ============================================================
# 5. AGGREGATE - DESVIO ESTANDAR
# ============================================================

desvio <- aggregate(
  datos[especies],
  by = list(
    AÑO = datos$AÑO,
    LOTE = datos$LOTE
  ),
  FUN = sd,
  na.rm = TRUE
)

print(desvio)

datos_largo <- desvio %>%
  pivot_longer(
    cols = -c(AÑO, LOTE),
    names_to = "Especie",
    values_to = "Desvio"
  )

ggplot(datos_largo,
       aes(x = Especie, y = Desvio, fill = factor(AÑO))) +
  geom_col(position = position_dodge(width = 0.8),
           width = 0.7) +
  facet_wrap(~ LOTE) +
  labs(
    x = "Especie",
    y = "Desvio",
    fill = "Año"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# ============================================================
# 6. AGGREGATE - VARIANZA
# ============================================================

varianza <- aggregate(
  datos[especies],
  by = list(
    AÑO = datos$AÑO,
    LOTE = datos$LOTE
  ),
  FUN = var,
  na.rm = TRUE
)

print(varianza)

# Saque juvenil de lombriz porque tiene mucha varianza

datos_largo <- varianza %>%
  pivot_longer(
    cols = -c(AÑO, LOTE, juvenil.de.lombriz),
    names_to = "Especie",
    values_to = "Varianza"
  )

ggplot(datos_largo,
       aes(x = Especie, y = Varianza, fill = factor(AÑO))) +
  geom_col(position = position_dodge(width = 0.8),
           width = 0.7) +
  facet_wrap(~ LOTE) +
  labs(
    x = "Especie",
    y = "Varianza",
    fill = "Año"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# ============================================================
# 7. AGGREGATE - MEDIANA
# ============================================================


mediana <- aggregate(
  datos[especies],
  by = list(
    AÑO = datos$AÑO,
    LOTE = datos$LOTE
  ),
  FUN = median,
  na.rm = TRUE
)

print(mediana)


datos_largo <- mediana %>%
  pivot_longer(
    cols = -c(AÑO, LOTE),
    names_to = "Especie",
    values_to = "Mediana"
  )

ggplot(datos_largo,
       aes(x = Especie, y = Mediana, fill = factor(AÑO))) +
  geom_col(position = position_dodge(width = 0.8),
           width = 0.7) +
  facet_wrap(~ LOTE) +
  labs(
    x = "Especie",
    y = "Mediana",
    fill = "Año"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )




# ============================================================
# 8. AGGREGATE - MINIMO
# ============================================================

minimo <- aggregate(
  datos[especies],
  by = list(
    AÑO = datos$AÑO,
    LOTE = datos$LOTE
  ),
  FUN = min,
  na.rm = TRUE
)

print(minimo)


datos_largo <- desvio %>%
  pivot_longer(
    cols = -c(AÑO, LOTE),
    names_to = "Especie",
    values_to = "Minimo"
  )

ggplot(datos_largo,
       aes(x = Especie, y = Minimo, fill = factor(AÑO))) +
  geom_col(position = position_dodge(width = 0.8),
           width = 0.7) +
  facet_wrap(~ LOTE) +
  labs(
    x = "Especie",
    y = "Minimo",
    fill = "Año"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# ============================================================
# 9. AGGREGATE - MAXIMO
# ============================================================

maximo <- aggregate(
  datos[especies],
  by = list(
    AÑO = datos$AÑO,
    LOTE = datos$LOTE
  ),
  FUN = max,
  na.rm = TRUE
)

print(maximo)

datos_largo <- maximo %>%
  pivot_longer(
    cols = -c(AÑO, LOTE),
    names_to = "Especie",
    values_to = "Maximo"
  )

ggplot(datos_largo,
       aes(x = Especie, y = Maximo, fill = factor(AÑO))) +
  geom_col(position = position_dodge(width = 0.8),
           width = 0.7) +
  facet_wrap(~ LOTE) +
  labs(
    x = "Especie",
    y = "Desvio",
    fill = "Año"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# ============================================================
# 10. CORRELACION
# ============================================================

# Seleccionar solamente las especies
datos_variables <- datos[, especies]

# Correlacion de Pearson
matriz_correlacion <- cor(
  datos_variables,
  method = "pearson",
  use = "complete.obs"
)

print(matriz_correlacion)


# ============================================================
# 11. CORRELACION DE SPEARMAN
# ============================================================

matriz_spearman <- cor(
  datos_variables,
  method = "spearman",
  use = "complete.obs"
)

print(matriz_spearman)


# ============================================================
# 12. COVARIANZA
# ============================================================

matriz_covarianza <- cov(
  datos_variables,
  use = "complete.obs"
)

print(matriz_covarianza)


# ============================================================
# 13. CORRELACION ENTRE DOS ESPECIES
# ============================================================

cor(
  datos$Ap_rosea,
  datos$Microscolex.dubius,
  method = "pearson",
  use = "complete.obs"
)


# ============================================================
# 14. TEST DE CORRELACION
# ============================================================

cor.test(
  datos$Ap_rosea,
  datos$Microscolex.dubius,
  method = "pearson"
)
