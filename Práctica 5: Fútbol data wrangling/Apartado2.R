library(tidyverse)
df_csv <- read_csv("2122_SP1.csv")
df_csv <- df_csv[, 1:8]
equipos <- unique(df_csv$HomeTeam)
local <- data.frame(Equipo = character(), Partidos_jug_l = numeric(), Punto_l = numeric(), Victoria_l = numeric(), Empate_l = numeric(), Derrota_l = numeric(), Goles_favor_l = numeric(), Goles_contra_l = numeric(),stringsAsFactors = FALSE)
visitante <- data.frame(Equipo = character(), Punto_v = numeric(), Victoria_v = numeric(), Empate_v = numeric(), Derrota_v = numeric(), Goles_favor_v = numeric(), Goles_contra_v = numeric(),stringsAsFactors = FALSE)
for (equipo in equipos) {
  puntos_local <- sum(ifelse(df_csv$HomeTeam == equipo, ifelse(df_csv$FTR == "H", 3, ifelse(df_csv$FTR == "D", 1, 0)), 0))
  victorias_local <- sum(ifelse(df_csv$HomeTeam == equipo & df_csv$FTR == "H", 1, 0))
  empates_local <- sum(ifelse(df_csv$HomeTeam == equipo & df_csv$FTR == "D", 1, 0))
  derrotas_local <- sum(ifelse(df_csv$HomeTeam == equipo & df_csv$FTR == "A", 1, 0))
  goles_af_local <- sum(ifelse(df_csv$HomeTeam == equipo,df_csv$FTHG,0))
  goles_ec_local <- sum(ifelse(df_csv$HomeTeam == equipo,df_csv$FTAG,0))
  jugados_local <- sum(ifelse(df_csv$HomeTeam == equipo,1,0))
  local <- rbind(local, data.frame(Equipo = equipo, Partidos_jug_l = jugados_local, Punto_l = puntos_local, Victoria_l = victorias_local, Empate_l = empates_local, Derrota_l = derrotas_local, Goles_favor_l = goles_af_local, Goles_contra_l = goles_ec_local, stringsAsFactors = FALSE))
}
for (equipo in equipos) {
  puntos_visitante <- sum(ifelse(df_csv$AwayTeam == equipo, ifelse(df_csv$FTR == "A", 3, ifelse(df_csv$FTR == "D", 1, 0)), 0))
  victorias_visitante <- sum(ifelse(df_csv$AwayTeam == equipo & df_csv$FTR == "A", 1, 0))
  empates_visitante <- sum(ifelse(df_csv$AwayTeam == equipo & df_csv$FTR == "D", 1, 0))
  derrotas_visitante <- sum(ifelse(df_csv$AwayTeam == equipo & df_csv$FTR == "H", 1, 0))
  goles_af_visitante <- sum(ifelse(df_csv$AwayTeam == equipo,df_csv$FTAG,0))
  goles_ec_visitante <- sum(ifelse(df_csv$AwayTeam == equipo,df_csv$FTHG,0))
  jugados_visitante <- sum(ifelse(df_csv$AwayTeam == equipo,1,0))
  visitante <- rbind(visitante, data.frame(Equipo = equipo, Partidos_jug_v = jugados_visitante, Punto_v = puntos_visitante, Victoria_v = victorias_visitante, Empate_v = empates_visitante, Derrota_v = derrotas_visitante, Goles_favor_v = goles_af_visitante, Goles_contra_v = goles_ec_visitante, stringsAsFactors = FALSE))
}

total <- data.frame(
  Equipo = equipos,
  Pts = local$Punto_l + visitante$Punto_v,
  PJ = local$Partidos_jug_l + visitante$Partidos_jug_v,
  PG = local$Victoria_l + visitante$Victoria_v,
  PE = local$Empate_l + visitante$Empate_v,
  PP = local$Derrota_l + visitante$Derrota_v,
  GF = local$Goles_favor_l + visitante$Goles_favor_v,
  GC = local$Goles_contra_l + visitante$Goles_contra_v
)
local <- local[order(local$Punto_l, decreasing = TRUE), ]
visitante <- visitante[order(visitante$Punto_v, decreasing = TRUE), ]
total <- total[order(total$Pts, decreasing = TRUE), ]