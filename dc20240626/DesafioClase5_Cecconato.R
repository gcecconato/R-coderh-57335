#Consignas:
#Utilizar las funciones mutate, select, filter, group_by y summarize 
#para realizar una transformación en tu dataset seleccionado en clase 2 
#(se puede utilizar transmute en lugar de mutate y select). 
#Estas transformaciones las deberás hacer para tu proyecto final.
#El filter debe tener por lo menos dos condiciones. 
#El group_by debe ser por lo menos por dos variables.


#Cartamgos el dataset e inspeccionamos datos y estrutura
df_1 <- read.csv("Clase_05/electr-demand-victoria-australia-57335-Cecconato.csv")
View(df_1)
head(df_1)
str(df_1)

# Modificamos variables a factor mediante dos métodos, uno de ellos con mutate
df_1 <- mutate(df_1, across(.cols = c(school_day, holiday), .fns = factor)) 
df_1$date<-date(df_1$date)
str(df_1)
#Seleccionamos primeras observaciones (head) 
#de los campos de fecha y las variables que modificamos a tipo de dato factor
df_1 %>% 
  head %>% 
  select(c(date, school_day, holiday))

#Creamos nueva variable categórica teniendo en cuenta
#que no es día de clases y es feriado
day_off_dates <- df_1 %>% 
  filter(school_day=="N" & holiday=="Y") %>% 
  mutate (day_off = as.factor("Y")) %>% 
  select(c(date,school_day,holiday,day_off))
str(day_off_dates)
head(day_off_dates)

#calculamos el promedio de la demanda por ambas varibles categóricas (school_day, holiday)
demand_mean_group <- df_1 %>% 
  group_by(school_day,holiday) %>% 
  summarise(MeanDemand=mean(demand),.groups="drop") 
demand_mean_group

#Nota: Para la realización del desafío me he enfocado en las consignas y aplicado
#lo visto en clases, no obstante para el proyecto final muy posiblemente
#las transformaciones serán ajustadas y reorganizadas considerando el avance del 
#curso y las especificaciones del citado proyecto. 
