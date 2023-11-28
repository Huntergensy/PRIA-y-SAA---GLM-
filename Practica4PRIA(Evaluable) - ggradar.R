#Antes de nada una aclaración, he preparado lotr.csv para que los nombres de las columnas empiecen 
#por mayúscula y sigan con minúscula. Para ello, con la librería dplyr he usado algo como:

# Cambiar el nombre de la columna "nombre" a "Nombre"
#df <- read.csv("lotr.csv")
#df <- df %>% rename(Nombre = nombre)
#write.csv(df, file = "lotr.csv", row.names = FALSE)

#Dicho esto, instalaríamos los paquetes que vamos a necesitar si no las tenemos ya
#install.packages("devtools")
#devtools::install_github("ricardo-bion/ggradar",dependencies = TRUE)

#Cargamos las librerías
library(ggradar)
library(dplyr)
library(ggplot2)

#Creamos una función para normalizar los datos (de 0 a 1) respecto al valor mínimo y máximo de cada columna
#Se llama "estandarización por rangos" (visto en los apuntes del profesor, SAA 3.1 Distribuciones de probabilidad contínuas)
normalizar <- function(x){
  x = (x - min(x)) / (max(x) - min(x))
  return(x)
}

#Creamos la función para dibujar el radar con la baraja que nos interesa
radartt <- function(baraja){
  
  if(baraja == "1"){
    #Cargamos el archivo de la baraja en cuestión, que en mi caso están en Documentos,
    #si no, habría que poner la ruta completa
    #Ejemplo: baraja1 <- read.csv(paste0("C:/Users/usuario/Documents/","df_harry_potter.csv")
    baraja1 <- read.csv("df_harry_potter.csv")
    #Guardamos las columnas que nos interesan en un dataframe agrupadas por Nombre
    df <- 
      baraja1 %>%
      #Aquí por ejemplo no selecciono la columna Rol
      select(Nombre,Magia,Astucia,Coraje,Sabiduria,Templanza) %>%
      group_by(Nombre)
  }else if(baraja == "2"){
    baraja2 <- read.csv("df_lqsa.csv")
    df <- 
      baraja2 %>%
      select(Nombre,Poder,Convivencia,Liante,Atractivo,Locura) %>%
      group_by(Nombre)
  }else{
    baraja3 <- read.csv("df_lotr.csv")
    df <- 
      baraja3 %>%
      select(Nombre,Tamano,Fuerza,Valentia,Magia,Miedo) %>%
      group_by(Nombre)
  }
  
  #Aplicamos la función de normalizar a las columnas que no sean Nombre,
  #por eso la excluimos con [,-1]
  df <- data.frame(df["Nombre"],lapply(df[,-1],normalizar))
  
  #Seleccionamos 5 personajes aleatorios (Ya que si lo hiciésemos con los 30 personajes
  #de la baraja, se taparían unos con otros y a penas podrían distinguirse)
  filas_seleccionadas <- sample(nrow(df), 5)
  #Creamos un nuevo dataframe con las filas seleccionadas
  df_seleccionado <- df[filas_seleccionadas, ]
  
  #Procedemos a dibujar en un radar los datos seleccionados y normalizados
  #ggradar(as.data.frame(df_seleccionado)) <----------------  NO FUNCIONA
  #Esta misma linea pero fuera de una función sí que dibujaría el radar
  
  #Así que optamos por otra forma que es forzarlo:
  #Esto utiliza el dispositivo gráfico rgl, que es una interfaz gráfica tridimensional en R
  #Solo faltaría hacer zoom en el dibujo que nos muestra en la sección "Plots"
  print(ggradar(as.data.frame(df_seleccionado)), device = "rgl")
  
  #Además podemos guardar el resultado como una imagen
  ggsave("Radar_TOP_TRUMPS.png", pl, width = 10, height = 8, units = "in")
}

#Dialogo para seleccionar la baraja que se quiere mostrar en el radar
while(TRUE){
  cat("Radar TOP TRUMPS\n----------------\n")
  cat("1 <- TOP TRUMPS Harry Potter\n2 <- TOP TRUMPS La que se avecina\n3 <- TOP TRUMPS Señor de los anillos\n4 <- Salir")
  baraja <- readline("Introduce baraja:")
  if(baraja == "1" || baraja == "2" || baraja == "3"){
    radartt(baraja)
  }else if(baraja == "4"){
    print("Fin del programa")
    break
  }else{
    print("Baraja no válida, vuelva a intentarlo")
  }
}