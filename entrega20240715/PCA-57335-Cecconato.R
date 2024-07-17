library(psych)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(devtools)
library(ggbiplot)
library(ggfortify)
library(FactoMineR)
library(factoextra)

# Cargamos el conjunto de datos e inspeccionamos su estructura y datos
ds_edva <- read.csv("Clase_10/entregable/electr-demand-victoria-australia-57335-Cecconato.csv")
head(ds_edva)
ds_edva_copy <- ds_edva
str(ds_edva_copy)
head(ds_edva_copy)

# Modificamos nombre de las observaciones con su fecha
rownames(ds_edva_copy) <- ds_edva_copy$date
# Modificamos variables a numérico
ds_edva_copy <- ds_edva_copy %>% 
  select(-date) %>% 
  mutate(school_day = case_when(
    school_day == "N" ~ 0,
    school_day == "Y" ~ 1,
  ),holiday = case_when(
    holiday == "N" ~ 0,
    holiday == "Y" ~ 1,
  ))

# Imputamos valores nulos 
ds_edva_copy <- ds_edva_copy %>%
  mutate(solar_exposure = 
           replace_na(solar_exposure, median(solar_exposure, na.rm = TRUE)),
         rainfall = 
           replace_na(rainfall, median(rainfall, na.rm = TRUE)),
  )  

pca_ds_edva_copy <- prcomp(ds_edva_copy, center = TRUE , scale = TRUE)
pca_ds_edva_copy
names(pca_ds_edva_copy)

# Muestro resumen de PCA (STD, Proporción de Varianza y su acumulado)
summary(pca_ds_edva_copy)

# Graficamos los primeros dos componentes
pca_ds_edva_copy$x

plot(pca_ds_edva_copy$x[,1], pca_ds_edva_copy$x[,2], xlab = "PCA 1", ylab = "PCA 2")

# Autovalores y autovectores
autovectores <- pca_ds_edva_copy$rotation 
autovalores <- pca_ds_edva_copy$sdev * pca_ds_edva_copy$sdev

autovectores
autovalores

# Cálculo porcentaje explicado
pca_var_pct <- round(autovalores / sum(autovalores)*100, digits = 2)
barplot(pca_var_pct, main = "Scree Plot", xlab = "Componente Principal", ylab = "Variacion Porcentual")

# Calcuclamos correlacion entre el conjunto de datos original y componentes principales
round(cor(ds_edva_copy, pca_ds_edva_copy$x), digits = 3)

# Scree Plot
screeplot(pca_ds_edva_copy, type = "l", main = "Screeplot mtcars")

