#Consigna:
#Escribir un función llamada both_na(),
#que tome dos vectores de la misma longitud
#y que retorne el índice de cada vector donde haya un NA.

#Código completo de la función 
both_na <- function(vector1, vector2)
{
  if(length(vector1) == length(vector2))
  {
    i_na_vector1 <- c()
    for (i in seq_along(vector1))
    {
      if(is.na(vector1[i])) {
        i_na_vector1 <- c(i_na_vector1,i)
      }
    }
    i_na_vector2 <- c()
    for (i in seq_along(vector2))
    {
      if(is.na(vector2[i])) {
        i_na_vector2 <- c(i_na_vector2,i)
      }
    }
    mensaje <- list(resultado = paste("Los vectores sí tienen la misma longitud: ", length(vector1)),
                    indices_NA_vector1 = i_na_vector1, 
                    indices_NA_vector2 = i_na_vector2)
  }
  else
  {
    mensaje <- list(resultado ="Los vectores no tienen la misma longitud",
                    longitud_vector1 = length(vector1), 
                    longitud_vector2 = length(vector2))
  }
  return(mensaje)
}

#Demostración de la función con ejemplos
v1 <- c(1,NA,3,4,5)
v2 <- c("a","b",NA,NA,"e")
v3 <- c("a",NA,"c","d")

#En este caso los vectores sí tiene la misma longitud
#por lo tanto busca los NA en cada uno
both_na(v1,v2) 
#En este caso los vectores no tiene la misma longitud
#por lo tanto sólo devuelve los longitudes diferentes de cada uno
both_na(v1,v3)
