library(haven)    # Para leer archivos STATA (.dta)
install.packages("dplyr")
library(dplyr)    # Para manipulación de datos


# Cargar la base de datos STATA
BD_2022 <- read_dta("C:/Universidad PUCV/2025 universidad/Data science/ed2022-stata.dta")

nombres <- names(BD_2022)

# Rango 1: columnas 13 a 27 (RACEUN a PAYTYPER)
rango1 <- 13:27

# Rango 2: columnas 35 a 908 (PAINSCALE a BOARDED)
rango2 <- 35:908

# Columnas específicas que no deben eliminarse (según nombre)
excluir <- which(nombres %in% c("OBESITY", "REGION", "LWBS"))

# Columnas específicas a eliminar (LOV y ETHUN)
extra <- which(nombres %in% c("LOV", "ETHUN", "CSTRATM" ,"CPSUM" ,"PATWT", "EDWT", "BOARDED", "RESIDNCE", "AGER", "AGEDAYS"))

# Combinar todos los índices a eliminar (exceptuando OBESITY y REGION)
a_eliminar <- setdiff(c(rango1, rango2, extra), excluir)

# Aplicar eliminación
BD2022 <- BD_2022[, -a_eliminar]

BD2022 <- BD2022 %>%
  rename(
    region = REGION,
    esi = IMMEDR,
    age = AGE,
    temp = TEMPF,
    heart_rate = PULSE,
    o2_saturation = POPCT,
    systolic = BPSYS,
    diastolic = BPDIAS,
    respiratory_rate = RESPR,
    wtime = WAITTIME,
    arrival_hour = ARRTIME,
    month = VMONTH,
    day_of_week = VDAYR,
    sex = SEX,
    hispanic_ethnicity = ETHIM,
    obesity_true = OBESITY
  )

# Convertir arrival_hour a hora decimal (numérico)
BD2022$arrival_hour <- as.numeric(substr(BD2022$arrival_hour, 1, 2)) +
  as.numeric(substr(BD2022$arrival_hour, 3, 4)) / 60

# Recodificar sex: 1 (mujer) → 0, 2 (hombre) → 1
BD2022$sex <- ifelse(BD2022$sex == 1, 0, 1)

# Recodificar hispanic_ethnicity: 1 (hispano) → 1, 2 (no hispano) → 0
BD2022$hispanic_ethnicity <- ifelse(BD2022$hispanic_ethnicity == 2, 0, 1)

# Cambiar tipo de obesidad (0/1) a factor binario
BD2022$obesity_true <- as.factor(BD2022$obesity_true)
BD2022$LWBS <- as.factor(BD_2022$LWBS)

# Cambiar tipo de region a factor (categoría geográfica)
BD2022$region <- as.factor(BD2022$region)

# month y day_of_week como factores (puedes usar ordered si lo prefieres)
BD2022$month <- as.factor(BD2022$month)
BD2022$day_of_week <- as.factor(BD2022$day_of_week)

# esi como numeric TEMPORALMENTE (para imputar luego)
BD2022$esi <- as.numeric(BD2022$esi)

# Asegurar que las variables de triage sean numéricas
BD2022$temp <- as.numeric(BD2022$temp)
BD2022$heart_rate <- as.numeric(BD2022$heart_rate)
BD2022$respiratory_rate <- as.numeric(BD2022$respiratory_rate)
BD2022$systolic <- as.numeric(BD2022$systolic)
BD2022$diastolic <- as.numeric(BD2022$diastolic)
BD2022$o2_saturation <- as.numeric(BD2022$o2_saturation)
BD2022$wtime <- as.numeric(BD2022$wtime)
BD2022$age <- as.numeric(BD2022$age)


#LIMPIEZA AÑOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO 2019

# Cargar la base de datos STATA
BD2019 <- read_dta("C:/Universidad PUCV/2025 universidad/Data science/ed2019-stata.dta")

nombres <- names(BD2019)

# Rango 1: columnas 13 a 27 (RACEUN a PAYTYPER)
rango1 <- 13:27

# Rango 2: columnas 35 a 906 (PAINSCALE a BOARDED)
rango2 <- 35:906

# Columnas específicas que no deben eliminarse (según nombre)
excluir <- which(nombres %in% c("OBESITY", "REGION", "LWBS"))

# Columnas específicas a eliminar (LOV y ETHUN)
extra <- which(nombres %in% c("LOV", "ETHUN", "CSTRATM" ,"CPSUM" ,"PATWT", "EDWT", "BOARDED", "RESIDNCE", "AGER", "AGEDAYS"))

# Combinar todos los índices a eliminar (exceptuando OBESITY y REGION)
a_eliminar <- setdiff(c(rango1, rango2, extra), excluir)

# Aplicar eliminación
BD2019 <- BD2019[, -a_eliminar]

BD2019 <- BD2019 %>%
  rename(
    region = REGION,
    esi = IMMEDR,
    age = AGE,
    temp = TEMPF,
    heart_rate = PULSE,
    o2_saturation = POPCT,
    systolic = BPSYS,
    diastolic = BPDIAS,
    respiratory_rate = RESPR,
    wtime = WAITTIME,
    arrival_hour = ARRTIME,
    month = VMONTH,
    day_of_week = VDAYR,
    sex = SEX,
    hispanic_ethnicity = ETHIM,
    obesity_true = OBESITY
  )

# Convertir arrival_hour a hora decimal (numérico)
BD2019$arrival_hour <- as.numeric(substr(BD2019$arrival_hour, 1, 2)) +
  as.numeric(substr(BD2019$arrival_hour, 3, 4)) / 60

# Recodificar sex: 1 (mujer) → 0, 2 (hombre) → 1
BD2019$sex <- ifelse(BD2019$sex == 1, 0, 1)

# Recodificar hispanic_ethnicity: 1 (hispano) → 1, 2 (no hispano) → 0
BD2019$hispanic_ethnicity <- ifelse(BD2019$hispanic_ethnicity == 2, 0, 1)

# Cambiar tipo de obesidad (0/1) a factor binario
BD2019$obesity_true <- as.factor(BD2019$obesity_true)
BD2019$LWBS <- as.factor(BD2019$LWBS)

# Cambiar tipo de region a factor (categoría geográfica)
BD2019$region <- as.factor(BD2019$region)

# month y day_of_week como factores (puedes usar ordered si lo prefieres)
BD2019$month <- as.factor(BD2019$month)
BD2019$day_of_week <- as.factor(BD2019$day_of_week)

# esi como numeric TEMPORALMENTE (para imputar luego)
BD2019$esi <- as.numeric(BD2019$esi)

# Asegurar que las variables de triage sean numéricas
BD2019$temp <- as.numeric(BD2019$temp)
BD2019$heart_rate <- as.numeric(BD2019$heart_rate)
BD2019$respiratory_rate <- as.numeric(BD2019$respiratory_rate)
BD2019$systolic <- as.numeric(BD2019$systolic)
BD2019$diastolic <- as.numeric(BD2019$diastolic)
BD2019$o2_saturation <- as.numeric(BD2019$o2_saturation)
BD2019$wtime <- as.numeric(BD2019$wtime)
BD2019$age <- as.numeric(BD2019$age)



#LIMPIEZA AÑOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO 2018

# Cargar la base de datos STATA
BD2018 <- read_dta("C:/Universidad PUCV/2025 universidad/Data science/ed2018-stata.dta")

nombres <- names(BD2018)

# Rango 1: columnas 13 a 27 (RACEUN a PAYTYPER)
rango1 <- 13:27

# Rango 2: columnas 35 a 945 (PAINSCALE a BOARDED)
rango2 <- 35:945

# Columnas específicas que no deben eliminarse (según nombre)
excluir <- which(nombres %in% c("OBESITY", "REGION", "LWBS"))

# Columnas específicas a eliminar (LOV y ETHUN)
extra <- which(nombres %in% c("LOV", "ETHUN", "CSTRATM" ,"CPSUM" ,"PATWT", "EDWT", "BOARDED", "RESIDNCE", "AGER", "AGEDAYS"))

# Combinar todos los índices a eliminar (exceptuando OBESITY y REGION)
a_eliminar <- setdiff(c(rango1, rango2, extra), excluir)

# Aplicar eliminación
BD2018 <- BD2018[, -a_eliminar]

BD2018 <- BD2018 %>%
  rename(
    region = REGION,
    esi = IMMEDR,
    age = AGE,
    temp = TEMPF,
    heart_rate = PULSE,
    o2_saturation = POPCT,
    systolic = BPSYS,
    diastolic = BPDIAS,
    respiratory_rate = RESPR,
    wtime = WAITTIME,
    arrival_hour = ARRTIME,
    month = VMONTH,
    day_of_week = VDAYR,
    sex = SEX,
    hispanic_ethnicity = ETHIM,
    obesity_true = OBESITY
  )

# Convertir arrival_hour a hora decimal (numérico)
BD2018$arrival_hour <- as.numeric(substr(BD2018$arrival_hour, 1, 2)) +
  as.numeric(substr(BD2018$arrival_hour, 3, 4)) / 60

# Recodificar sex: 1 (mujer) → 0, 2 (hombre) → 1
BD2018$sex <- ifelse(BD2018$sex == 1, 0, 1)

# Recodificar hispanic_ethnicity: 1 (hispano) → 1, 2 (no hispano) → 0
BD2018$hispanic_ethnicity <- ifelse(BD2018$hispanic_ethnicity == 2, 0, 1)

# Cambiar tipo de obesidad (0/1) a factor binario
BD2018$obesity_true <- as.factor(BD2018$obesity_true)
BD2018$LWBS <- as.factor(BD2018$LWBS)

# Cambiar tipo de region a factor (categoría geográfica)
BD2018$region <- as.factor(BD2018$region)

# month y day_of_week como factores (puedes usar ordered si lo prefieres)
BD2018$month <- as.factor(BD2018$month)
BD2018$day_of_week <- as.factor(BD2018$day_of_week)

# esi como numeric TEMPORALMENTE (para imputar luego)
BD2018$esi <- as.numeric(BD2018$esi)

# Asegurar que las variables de triage sean numéricas
BD2018$temp <- as.numeric(BD2018$temp)
BD2018$heart_rate <- as.numeric(BD2018$heart_rate)
BD2018$respiratory_rate <- as.numeric(BD2018$respiratory_rate)
BD2018$systolic <- as.numeric(BD2018$systolic)
BD2018$diastolic <- as.numeric(BD2018$diastolic)
BD2018$o2_saturation <- as.numeric(BD2018$o2_saturation)
BD2018$wtime <- as.numeric(BD2018$wtime)
BD2018$age <- as.numeric(BD2018$age)


#ELIMINACIÓN DE PACIENTES PEDIÁTRICOS
BD2018 <- BD2018 %>% filter(age >= 18)
BD2019 <- BD2019 %>% filter(age >= 18)
BD2022 <- BD2022 %>% filter(age >= 18)


# Unir las bases de datos (apilamiento vertical)
BD_completa <- bind_rows(BD2018, BD2019, BD2022)

# Guardar el resultado en un nuevo CSV
write.csv(BD_completa, "BD_completa.csv", row.names = FALSE)



#GRÁFICO DE DISTRIBUCIÓN DE "ESI"

if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("scales")) install.packages("scales")

# Cargar librerías
library(ggplot2)
library(dplyr)
library(scales)

# Definir las etiquetas para cada categoría ESI
esi_labels <- c(
  "-9" = "-9: Blank",
  "-8" = "-8: Unknown",
  "0" = "0: No triage (ESA con triage)",
  "1" = "1: Immediate",
  "2" = "2: Emergent",
  "3" = "3: Urgent",
  "4" = "4: Semi-urgent",
  "5" = "5: Nonurgent",
  "7" = "7: ESA sin triage"
)

# Calcular frecuencias y porcentajes
datos_esi <- BD_completa %>%
  mutate(esi = as.character(esi)) %>%  # Convertir a carácter para el mapeo
  count(esi) %>%
  mutate(
    porcentaje = n / sum(n),
    etiqueta = paste0(n, " (", percent(porcentaje, accuracy = 0.1), ")"),
    esi_label = factor(esi, levels = names(esi_labels), labels = esi_labels)
  )

# Obtener el total de pacientes
total_pacientes <- sum(datos_esi$n)

# Crear el gráfico
ggplot(datos_esi, aes(x = esi_label, y = n, fill = esi_label)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = etiqueta), vjust = -0.3, size = 3.5) +  # Etiquetas arriba de las barras
  labs(
    title = paste("Distribución de Pacientes por Nivel ESI\nTotal de pacientes:", total_pacientes),
    x = "Categoría ESI",
    y = "Número de Pacientes",
    fill = "Nivel ESI"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),  # Rotar etiquetas del eje X
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),  # Formato del título
    legend.position = "none"  # Ocultar leyenda si no es necesaria
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +  # Ajustar espacio en el eje Y
  scale_fill_viridis_d()  # Usar una paleta de colores amigable


#ELIMINACIÓN DE VALORES 998 (DOPPLER)

BD_completa <- BD_completa %>% 
  filter(heart_rate != 998 & diastolic != 998)

# Verificación rápida
cat("Filas restantes:", nrow(BD_completa))

#TEMP CONVERTIDA A DECIMAL
BD_completa <- BD_completa %>% 
  mutate(temp = ifelse(temp >= 0, temp / 10, temp))  # >0 → divide, negativo → conserva


#CONVERTIR VALORES NEGATIVOS A "NA" PARA USAR KNN
vars <- c("temp", "heart_rate", "respiratory_rate", "systolic", "diastolic", "o2_saturation", "wtime")
BD_completa[vars] <- lapply(BD_completa[vars], function(x) ifelse(x < 0, NA, x))

#IMPUTACIÓN CON KNN

install.packages("VIM")  
library(VIM)
vars_a_imputar <- c("temp", "heart_rate", "respiratory_rate", 
                    "systolic", "diastolic", "o2_saturation", 
                    "arrival_hour", "wtime")
BD_imputada <- kNN(BD_completa, variable = vars_a_imputar, k = 5, imp_var = FALSE)

# Guardar el resultado en un nuevo CSV
write.csv(BD_imputada, "BD_imputada.csv", row.names = FALSE)

#APLICACIÓN DE LOG EN WTIME Y HEART_RATE PARA ESTABILIZAR VAR Y REDUCIR EFECTO DE OUTLIERS
BD_imputada$log_wtime <- log1p(BD_imputada$wtime)
BD_imputada$log_heart_rate <- log1p(BD_imputada$heart_rate)
BD_imputada$log_systolic <- log1p(BD_imputada$systolic)

#ELIMINACIÓN DE CATEGORIAS ESI QUE NO SEAN DEL 1:5
BD_imputada <- subset(BD_imputada, esi %in% 1:5)

sapply(BD_imputada, class)

# CONVERTIR A FACTOR PARA CORREGIR
BD_imputada$month <- factor(BD_imputada$month, levels = 1:12, ordered = TRUE)
BD_imputada$day_of_week <- factor(BD_imputada$day_of_week, levels = 1:7, ordered = TRUE)

BD_imputada$sex <- factor(BD_imputada$sex, levels = c(0, 1))  # 0 = mujer, 1 = hombre
BD_imputada$hispanic_ethnicity <- factor(BD_imputada$hispanic_ethnicity, levels = c(0, 1))  # 0 = no hispano

BD_imputada$esi <- factor(BD_imputada$esi,
                          levels = c(1, 2, 3, 4, 5),
                          ordered = TRUE)
BD_imputada$obesity_true <- factor(BD_imputada$obesity_true, levels = c(0, 1))
BD_imputada$LWBS <- as.factor(BD_imputada$LWBS)
BD_imputada$region <- factor(BD_imputada$region)


write.csv(BD_imputada, "BD_imputada.csv", row.names = FALSE)




#Gráficos histograma por nivel

# ESI 1
library(ggplot2)
library(dplyr)
library(patchwork)

# Filtrar base para ESI 1
datos_esi1 <- BD_imputada %>% filter(esi == 1)

# Histograma de tiempos de espera
histograma_esi1 <- ggplot(datos_esi1, aes(x = wtime)) +
  geom_histogram(fill = "skyblue", color = "black", bins = 60) +
  theme_minimal() +
  labs(title = "Distribución de tiempos de espera - ESI 1",
       x = "Tiempo de espera (minutos)",
       y = "Frecuencia")

# Crear variable de rangos de tiempo
datos_esi1$rango <- cut(datos_esi1$wtime,
                        breaks = c(0, 5, 10, 15, 30, Inf),
                        labels = c("0–5 min", "6–10 min", "11–15 min", "16–30 min", ">30 min"),
                        right = TRUE,
                        include.lowest = TRUE)

# Conteo por rango
conteo_esi1 <- datos_esi1 %>% count(rango)

# Gráfico de barras
barras_esi1 <- ggplot(conteo_esi1, aes(x = rango, y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = n), vjust = -0.5) +
  labs(title = "Pacientes ESI 1 según tiempo de espera",
       x = "Rango de tiempo de espera",
       y = "Número de pacientes") +
  theme_minimal()

# Combinar los gráficos
(histograma_esi1 | barras_esi1)

#ESI 2

library(ggplot2)
library(dplyr)
library(patchwork)

# Filtrar datos para ESI = 2
datos_esi2 <- BD_imputada %>% filter(esi == 2)

# Histograma de tiempos de espera
histograma_esi2 <- ggplot(datos_esi2, aes(x = wtime)) +
  geom_histogram(fill = "skyblue", color = "black", bins = 60) +
  theme_minimal() +
  labs(title = "Distribución de tiempos de espera - ESI 2",
       x = "Tiempo de espera (minutos)",
       y = "Frecuencia") +
  xlim(0, 1000)

# Crear variable de rangos personalizados
datos_esi2$rango <- cut(datos_esi2$wtime,
                        breaks = c(0, 15, 30, 45, 60, Inf),
                        labels = c("< 15 min", "15–30 min", "30–45 min", "45–60 min", ">60 min"),
                        right = TRUE,
                        include.lowest = TRUE)

# Conteo por rangos
conteo_esi2 <- datos_esi2 %>%
  count(rango)

# Gráfico de barras con etiquetas
barras_esi2 <- ggplot(conteo_esi2, aes(x = rango, y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = n), vjust = -0.5) +
  labs(title = "Pacientes ESI 2 según tiempo de espera",
       x = "Rango de tiempo de espera",
       y = "Número de pacientes") +
  theme_minimal()

# Combinar los gráficos
(histograma_esi2 | barras_esi2)

#ESI 3

library(ggplot2)
library(dplyr)
library(patchwork)

# Filtrar datos para ESI = 3
datos_esi3 <- BD_imputada %>% filter(esi == 3)

# Histograma de tiempos de espera
histograma_esi3 <- ggplot(datos_esi3, aes(x = wtime)) +
  geom_histogram(fill = "skyblue", color = "black", bins = 60) +
  theme_minimal() +
  labs(title = "Distribución de tiempos de espera - ESI 3",
       x = "Tiempo de espera (minutos)",
       y = "Frecuencia") +
  xlim(0, 1500)

# Crear variable de rangos personalizados
datos_esi3$rango <- cut(datos_esi3$wtime,
                        breaks = c(0, 60, 80, 100, 120, Inf),
                        labels = c("< 60 min", "60–80 min", "80–100 min", "100–120 min", ">120 min"),
                        right = TRUE,
                        include.lowest = TRUE)

# Conteo por rangos
conteo_esi3 <- datos_esi3 %>%
  count(rango)

# Gráfico de barras con etiquetas
barras_esi3 <- ggplot(conteo_esi3, aes(x = rango, y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = n), vjust = -0.5) +
  labs(title = "Pacientes ESI 3 según tiempo de espera",
       x = "Rango de tiempo de espera",
       y = "Número de pacientes") +
  theme_minimal()

# Combinar los gráficos
(histograma_esi3 | barras_esi3)


#ESI 4

library(ggplot2)
library(dplyr)
library(patchwork)

# Filtrar datos para ESI = 4
datos_esi4 <- BD_imputada %>% filter(esi == 4)

# Histograma de tiempos de espera
histograma_esi4 <- ggplot(datos_esi4, aes(x = wtime)) +
  geom_histogram(fill = "skyblue", color = "black", bins = 60) +
  theme_minimal() +
  labs(title = "Distribución de tiempos de espera - ESI 4",
       x = "Tiempo de espera (minutos)",
       y = "Frecuencia") +
  xlim(0, 800)

# Crear variable de rangos personalizados
datos_esi4$rango <- cut(datos_esi4$wtime,
                        breaks = c(0, 120, 150, 180, 210, 240, Inf),
                        labels = c("< 120 min", "120–150 min", "150–180 min", "180–210 min", "210–240 min", ">240 min"),
                        right = TRUE,
                        include.lowest = TRUE)

# Conteo por rangos
conteo_esi4 <- datos_esi4 %>%
  count(rango)

# Gráfico de barras con etiquetas
barras_esi4 <- ggplot(conteo_esi4, aes(x = rango, y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = n), vjust = -0.5) +
  labs(title = "Pacientes ESI 4 según tiempo de espera",
       x = "Rango de tiempo de espera",
       y = "Número de pacientes") +
  theme_minimal()

# Combinar gráficos lado a lado
(histograma_esi4 | barras_esi4)

#ESI 5

library(ggplot2)
library(dplyr)
library(patchwork)

# Filtrar datos para ESI = 5
datos_esi5 <- BD_imputada %>% filter(esi == 5)

# Histograma de tiempos de espera
histograma_esi5 <- ggplot(datos_esi5, aes(x = wtime)) +
  geom_histogram(fill = "skyblue", color = "black", bins = 60) +
  theme_minimal() +
  labs(title = "Distribución de tiempos de espera - ESI 5",
       x = "Tiempo de espera (minutos)",
       y = "Frecuencia") +
  xlim(0, 1200)

# Crear variable de rangos personalizados
datos_esi5$rango <- cut(datos_esi5$wtime,
                        breaks = c(0, 240, 270, 300, 330, Inf),
                        labels = c("< 240 min", "240–270 min", "270–300 min", "300–330 min", ">330 min"),
                        right = TRUE,
                        include.lowest = TRUE)

# Conteo por rangos
conteo_esi5 <- datos_esi5 %>%
  count(rango)

# Gráfico de barras con etiquetas
barras_esi5 <- ggplot(conteo_esi5, aes(x = rango, y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = n), vjust = -0.5) +
  labs(title = "Pacientes ESI 5 según tiempo de espera",
       x = "Rango de tiempo de espera",
       y = "Número de pacientes") +
  theme_minimal()

# Combinar ambos gráficos
(histograma_esi5 | barras_esi5)






#MODELO RANDOM FOREST PARA LWBS

install.packages("xgboost")
install.packages("SHAPforxgboost")
install.packages("pROC")

# Carga de librerías
library(xgboost)
library(SHAPforxgboost)
library(pROC)

# 1. Variables y preparación de datos
vars <- c("month", "day_of_week", "arrival_hour", "age", "sex",
          "hispanic_ethnicity", "temp", "respiratory_rate",
          "diastolic", "o2_saturation", "obesity_true",
          "region", "log_wtime", "log_heart_rate", "log_systolic")

datos_xgb <- BD_imputada[, c("LWBS", vars)]
datos_xgb <- na.omit(datos_xgb)

# Asegurar que LWBS sea 0/1
datos_xgb$LWBS <- as.numeric(as.character(datos_xgb$LWBS))

# Matriz de predictores
X <- model.matrix(~ . -1, data = datos_xgb[, vars])  # sin intercepto
y <- datos_xgb$LWBS

# 2. Crear DMatrix
dtrain <- xgb.DMatrix(data = X, label = y)

# 3. Entrenar con validación cruzada
params <- list(
  objective = "binary:logistic",
  eval_metric = "auc",
  eta = 0.1,
  max_depth = 6,
  scale_pos_weight = 84.5
)

set.seed(123)

cv <- xgb.cv(
  params = params,
  data = dtrain,
  nrounds = 100,
  nfold = 5,
  stratified = TRUE,
  early_stopping_rounds = 10,
  verbose = 0
)

best_nrounds <- cv$best_iteration

# Modelo final
xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = best_nrounds
)

# 4. Predicciones y umbral ajustado (1.17%)
pred_probs <- predict(xgb_model, dtrain)
threshold <- 0.68
pred_class <- ifelse(pred_probs > threshold, 1, 0)

# 5. Curva ROC
roc_curve <- roc(y, pred_probs)
plot(roc_curve, col = "blue", main = "Curva ROC")
auc(roc_curve)

# 6. Matriz de confusión
conf <- table(factor(pred_class, levels = c(0,1)),
              factor(y, levels = c(0,1)))
colnames(conf) <- c("Real = 0 (se queda)", "Real = 1 (se va)")
rownames(conf) <- c("Predicho = 0", "Predicho = 1")
print(conf)

# Extraer valores
VN <- conf[1, 1]  # Verdaderos negativos
FP <- conf[2, 1]  # Falsos positivos
FN <- conf[1, 2]  # Falsos negativos
VP <- conf[2, 2]  # Verdaderos positivos

# Calcular métricas
accuracy <- (VP + VN) / (VP + VN + FP + FN)
precision <- VP / (VP + FP)
recall <- VP / (VP + FN)
f1_score <- 2 * (precision * recall) / (precision + recall)

# Mostrar resultados
cat("Accuracy: ", round(accuracy, 4), "\n")
cat("Precision: ", round(precision, 4), "\n")
cat("Recall (Sensitivity): ", round(recall, 4), "\n")
cat("F1 Score: ", round(f1_score, 4), "\n")

summary(pred_probs)

# 7. Gráfico SHAP corregido
library(SHAPforxgboost)

# Asegurar que los predictores estén en formato matriz
X_matrix <- as.matrix(X)

# Calcular valores SHAP
shap_values <- tryCatch({
  shap.values(xgb_model = xgb_model, X_train = X_matrix)
}, error = function(e) {
  print("❌ Error al calcular shap.values()")
  print(e)
  return(NULL)
})

# Preparar datos para gráfico
if (!is.null(shap_values)) {
  shap_long <- tryCatch({
    shap.prep(shap_contrib = shap_values$shap_score, X_train = X_matrix)
  }, error = function(e) {
    print("❌ Error al preparar shap_long")
    print(e)
    return(NULL)
  })
  
  # Graficar si todo funcionó
  if (!is.null(shap_long)) {
    shap.plot.summary(shap_long)
  } else {
    print("⚠️ shap_long no está disponible para graficar")
  }
} else {
  print("⚠️ shap_values no fue generado")
}












#Modelos XG Boost para nivel 1, 2, 3, 4 y 5

library(dplyr)
library(pROC)
library(xgboost)
library(caret)

set.seed(123)

# Cargar base
BD <- read.csv("BD_imputada.csv")

# Variables predictoras
vars <- c("month", "day_of_week", "arrival_hour", "age", "sex",
          "hispanic_ethnicity", "temp" , "respiratory_rate",
          "diastolic", "o2_saturation", "obesity_true",
          "region", "log_wtime", "log_heart_rate", "log_systolic")

# Umbrales clínicos por ESI
umbrales <- c("1"=5, "2"=15, "3"=60, "4"=120, "5"=240)
umbrales_modelo <- c("1"=0.5, "2"=0.45, "3"=0.65, "4"=0.68, "5"=0.47)

# Loop por nivel de ESI
for (nivel in 1:5) {
  cat("\n========== ESI", nivel, "==========\n")
  
  data_esi <- BD %>% filter(esi == nivel)
  data_esi$espera_excesiva <- ifelse(data_esi$wtime > umbrales[as.character(nivel)], 1, 0)
  
  # Split: 80% train / 20% test
  train_idx <- createDataPartition(data_esi$espera_excesiva, p = 0.8, list = FALSE)
  train <- data_esi[train_idx, ]
  test  <- data_esi[-train_idx, ]
  
  # Matrices
  dtrain <- xgb.DMatrix(data = as.matrix(train[, vars]), label = train$espera_excesiva)
  dtest  <- xgb.DMatrix(data = as.matrix(test[, vars]),  label = test$espera_excesiva)
  
  # Cross-validation estratificada para nrounds óptimo
  cv <- xgb.cv(
    data = dtrain,
    nrounds = 100,
    nfold = 5,
    stratified = TRUE,
    objective = "binary:logistic",
    eval_metric = "auc",
    early_stopping_rounds = 10,
    verbose = 0
  )
  
  best_nrounds <- cv$best_iteration
  
  # Modelo final
  modelo <- xgboost(
    data = dtrain,
    nrounds = best_nrounds,
    objective = "binary:logistic",
    eval_metric = "logloss",
    verbose = 0
  )
  
  # Predicciones
  probs <- predict(modelo, newdata = as.matrix(test[, vars]))
  threshold <- umbrales_modelo[as.character(nivel)]
  preds <- ifelse(probs > threshold, 1, 0)
  real  <- test$espera_excesiva
  
  # Matriz de confusión
  cm <- table(Real = real, Predicho = preds)
  print(cm)
  
  # Métricas
  acc <- sum(diag(cm)) / sum(cm)
  prec <- ifelse(sum(preds == 1) == 0, 0, sum(preds == 1 & real == 1) / sum(preds == 1))
  rec <- sum(preds == 1 & real == 1) / sum(real == 1)
  f1 <- ifelse((prec + rec) == 0, 0, 2 * prec * rec / (prec + rec))
  auc <- auc(roc(real, probs))
  
  cat(sprintf("Accuracy: %.4f\n", acc))
  cat(sprintf("Precisión: %.4f\n", prec))
  cat(sprintf("Recall: %.4f\n", rec))
  cat(sprintf("F1 Score: %.4f\n", f1))
  cat(sprintf("AUC: %.4f\n", auc))
  
  # Curva ROC
  plot(roc(real, probs), main = paste("Curva ROC - ESI", nivel))
}









library(ggplot2)

# Filtrar solo pacientes ESI 1
datos_esi1 <- BD_imputada[BD_imputada$esi == 1, ]

# Crear el dataframe con predicciones y resultado real
df_plot <- data.frame(
  prob = predict(xgb_model, newdata = datos_esi1),
  real = as.numeric(datos_esi1$espera_excesiva)
)

# Graficar dispersión con suavizado loess
ggplot(df_plot, aes(x = prob, y = real)) +
  geom_jitter(height = 0.05, width = 0, alpha = 0.3, color = "darkblue") +
  geom_smooth(method = "loess", se = TRUE, color = "red") +
  labs(
    x = "Probabilidad predicha por el modelo",
    y = "Resultado real (espera excesiva)",
    title = "Tendencia de predicción vs resultado (ESI 1)"
  ) +
  theme_minimal()
