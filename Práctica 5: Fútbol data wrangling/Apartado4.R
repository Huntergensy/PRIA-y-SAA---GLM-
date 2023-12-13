library(tidyverse)
library(ggplot2)

# (2 ligas disponibles)
liga <- c('SP1', 'SP2')

# Las temporadas '0001' y '0102' no están disponibles (21 disponibles)
temp <- c('0203', '0304', '0405', '0506', '0607', '0708', '0809', '0910', '1011', '1112', '1213',
          '1314', '1415', '1516', '1617', '1718', '1819', '1920', '2021', '2122', '2223')

total_df <- data.frame(
  Temporada = character(0),
  Div = character(0),
  Promedio_goles = numeric(0),
  stringsAsFactors = FALSE
)

promedio <- function() {
  for (i in liga) {
    for (j in temp) {
      carpeta <- paste0(j, "_", i, ".csv")
      promedio_df <- read.csv(carpeta)
      goles_favor <- sum(sapply(promedio_df$FTHG, as.numeric))
      goles_contra <- sum(sapply(promedio_df$FTAG, as.numeric))
      partidos <- nrow(promedio_df)
      media <- (goles_favor + goles_contra) / partidos
      
      total_df <<- rbind(total_df, data.frame(
        Temporada = j,
        Div = i,
        Promedio_goles = media,
        stringsAsFactors = FALSE
      ))
    }
  }
}

promedio()

sp1_data <- total_df %>% filter(Div == 'SP1') %>% drop_na()
sp2_data <- total_df %>% filter(Div == 'SP2') %>% drop_na()


# Verifica si hay datos para hacer el gráfico
if (nrow(sp1_data) > 1 & nrow(sp2_data) > 1) {
  # Convierte la columna Temporada a factor
  total_df$Temporada <- factor(total_df$Temporada, levels = unique(total_df$Temporada))
  
  # Crea el gráfico solo si hay suficientes datos
  ggplot() +
    geom_line(data = sp1_data, aes(x = Temporada, y = Promedio_goles, color = "SP1", group = 1), size = 1) +
    geom_point(data = sp1_data, aes(x = Temporada, y = Promedio_goles, color = "SP1"), size = 3) +
    geom_line(data = sp2_data, aes(x = Temporada, y = Promedio_goles, color = "SP2", group = 1), size = 1) +
    geom_point(data = sp2_data, aes(x = Temporada, y = Promedio_goles, color = "SP2"), size = 3) +
    labs(title = "Promedio de Goles por Temporada",
         x = "Temporada",
         y = "Promedio de Golespor Partido") +
    scale_color_manual(values = c("SP1" = "blue", "SP2" = "red")) +
    theme_minimal()
} else {
  cat("No hay suficientes datos para graficar.")
}