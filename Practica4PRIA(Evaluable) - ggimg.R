#cargamos todas las librerías que vamos a usar
library(ggplot2)
library(ggimage)
library(ggimg)
library(ggpubr)
library(jpeg)
library(png)

#Creamos un vector con la ruta de cada imagen para los personajes
#en mi caso están en Documentos, si no sería algo como:
#"C:/Users/usuario/Documents/GANDALF.jpeg", "C:/Users/usuario/Documents/FARAMIR.jpeg, etc.
Imagen = c("GANDALF.jpeg","FARAMIR.jpeg","SAURON.jpeg","SARUMAN.jpeg","BILBO.jpeg","ELRONO.jpeg","LURCZ.jpeg","GALADRIEL.jpeg","EL_ANILLO_UNICO.jpeg","GOLLUM.jpeg","LEGOLAS.jpeg","LENGUA_DE_SERPIENTE.jpeg","REY_BRUJO.jpeg","BALROG.jpeg","ARWEN.jpeg","SAM.jpeg","HALDIR.jpeg","GIMLI.jpeg","SHELOB.jpeg","EOWYN.jpeg","ARAGORN.jpeg","BARBOL.jpeg","PIPPIN.jpeg","REY_THEODEN.jpeg","EOMER.jpeg","NAZGUL.jpeg","FRODO.jpeg","BOROMIR.jpeg","MERRY.jpeg","DENECHOR.jpeg")

#Cargamos el dataframe en baraja3
baraja3 <- read.csv("df_lotr.csv")

#Creamos la nueva columna Imagen en el dataframe con los valores de la lista Imagen que hemos creado
baraja3$Imagen <- Imagen

#Por si las moscas, añadimos la ruta entera al nombre de la imagen
baraja3$Imagen <- paste0("C:/Users/usuario/Documents/", baraja3$Imagen)

#Verificamos que tenemos el dataframe como corresponde antes de dibujar el gráfico
baraja3

#De esta manera me da error y no he sabido cómo solucionarlo
#ggplot(baraja3) +
#  geom_point_img(aes(x = Tamano, y = Fuerza, img = Imagen), size = 1) +
#  theme_minimal()

#Pero he encontrado otra manera de hacerlo que es esta: geom_image
#Dibujamos el gráfico de dispersión con imágenes
pl <- ggplot(baraja3, aes(x=Tamano, y=Fuerza, image=Imagen)) +
  geom_point() +
  #Modificando el tamaño, si hacemos las imágenes muy grandes, se pueden solapar unas con otras y
  #perder la percepción de que a medida que aumenta x, también lo hace y.
  #Recomiendo que se vea pequeño, guardar la imagen y posteriormente hacerle zoom
  #Además podemos añadir las etiquetas de Nombre para que se entienda mejor el gráfico
  geom_image(aes(label=Nombre),size=0.05) +
  geom_text(aes(label=Nombre),hjust = 1.5, vjust = 0.5, size = 2.5, color = "black") +
  theme_classic()

#Con esto, forzamos que R dibuje el resultado como en la práctica anterior
#si no sería simplemente llamar a "pl"
print(pl, device = "rgl")

#Además podemos guardar el resultado como una imagen
ggsave("Grafico_lotr_tamano_vs_fuerza.png", pl, width = 10, height = 8, units = "in")