library(tidyverse)

descarga <- function() {
  # Solicitar la entrada del usuario para la división
  division <- readline("Ingresa la división (SP1,SP2,E0,D1,I1): ")
  
  # Solicitar la entrada del usuario para la temporada
  temporada <- readline("Ingresa la temporada (por ejemplo, 2122): ")
  
  # (5 ligas disponibles)
  liga <- c('SP1', 'SP2', 'E0', 'D1', 'I1')
  # las temporadas '0001' y '0102' no están disponibles (23 disponibles)
  temp <- c('0203', '0304', '0405', '0506', '0607', '0708', '0809', '0910', '1011', '1112', '1213', '1314', '1415', '1516', '1617', '1718', '1819', '1920', '2021', '2122', '2223')
  
  if (division %in% liga && temporada %in% temp) {
    url <- paste0('https://www.football-data.co.uk/mmz4281/', temporada, '/', division, '.csv')
    archivo <- paste0(temporada, '_', division, '.csv')
    download.file(url, destfile = archivo, method = 'auto')
    
    clasificacion <- read.csv(archivo)
    
    clasificacion <- clasificacion %>%
      pivot_longer(cols = c(HomeTeam, AwayTeam), names_to = "HomeAway", values_to = "Equipo") %>%
      mutate(Pts = ifelse(HomeAway == "HomeTeam", ifelse(FTR == "H", 3, ifelse(FTR == "D", 1, 0)),
                          ifelse(HomeAway == "AwayTeam", ifelse(FTR == "A", 3, ifelse(FTR == "D", 1, 0)), 0))) %>%
      group_by(Equipo) %>%
      summarise(
        Pts = sum(Pts),
        PJ = n(),
        PG = sum(ifelse((HomeAway == "HomeTeam" & FTR == "H") | (HomeAway == "AwayTeam" & FTR == "A"), 1, 0)),
        PE = sum(ifelse((HomeAway == "HomeTeam" & FTR == "D") | (HomeAway == "AwayTeam" & FTR == "D"), 1, 0)),
        PP = sum(ifelse((HomeAway == "HomeTeam" & FTR == "A") | (HomeAway == "AwayTeam" & FTR == "H"), 1, 0)),
        GF = sum(ifelse(HomeAway == "HomeTeam", FTHG, ifelse(HomeAway == "AwayTeam", FTAG, 0))),
        GC = sum(ifelse(HomeAway == "HomeTeam", FTAG, ifelse(HomeAway == "AwayTeam", FTHG, 0)))
      ) %>%
      arrange(desc(Pts))
    
    print(clasificacion)
  } else {
    cat("División o Temporada no disponible")
  }
}

# Llamamos a la función sin argumentos porque ahora los valores se piden interactivamente
descarga()
