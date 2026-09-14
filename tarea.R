# ============================================================
# LIBRERIAS NECESARIAS
# ============================================================

library(dplyr)
library(tidyr)

# ============================================================
# CARGA DE DATOS
# ============================================================

datos <- read.csv("TP1_eco3_26 - Macro_Meso.csv")

#reemplazar los NA con 0
datos <- replace(datos, is.na(datos),  0  )

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
