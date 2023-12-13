#liga española, inglesa, alemana e italiana
#desde la temporada 2000/01 hasta la 2022/23

#(5 ligas disponibles)
liga <- c('SP1','SP2','E0','D1','I1')
#las temporadas '0001' y '0102' no están disponibles (23 disponibles)
temp <- c('0001','0102','0203','0304','0405','0506','0607','0708','0809','0910','1011','1112','1213','1314','1415','1516','1617','1718','1819','1920','2021','2122','2223')

descarga_historico_temp <- function(){
  
  contliga <- 0
  for(i in liga){
    contemp <- 0
    for(j in temp){
      contemp <- contemp + 1
      url <- paste0("https://www.football-data.co.uk/mmz4281/",j,"/",i,".csv")
      #Para organizar los archivos en carpetas segun la liga
      #if (!file.exists(paste0("C:/Users/usuario/Documents/",i))) {
      #  dir.create(paste0("C:/Users/usuario/Documents/",i))
      #}
      #carpeta <- paste0("C:/Users/usuario/Documents/",i,"/",j,"_",i,".csv")
      carpeta <- paste0(j,"_",i,".csv")
      print(paste("Liga:",i,"Temporada:",j))
      download.file(url,carpeta)
      if(contemp == 23){
        break
      }
    }
    contliga <- contliga + 1
    if(contliga == 5){
      break
    }
  }
}

actualiza_liga_actual <- function(liga){
  # Obtener los dos últimos dígitos del año actual
  dos_ultimos_digitos <- as.numeric(substr(format(Sys.Date(), "%Y"), 3, 4))
  
  # Construir temporada_coincidente
  temporada_coincidente <- paste0(dos_ultimos_digitos, dos_ultimos_digitos + 1)
  
  if (!(temporada_coincidente %in% temp)) {
    cat("La temporada correspondiente al año actual no está en la lista 2000-2023. Se procede a la descarga.\n")
    
    url <- paste0("https://www.football-data.co.uk/mmz4281/",temporada_coincidente,"/",liga,".csv")
    #Organizado por carpetas de liga
    #if (!file.exists(paste0("C:/Users/usuario/Documents/",liga))) {
    #  dir.create(paste0("C:/Users/usuario/Documents/",liga))
    #}
    #carpeta <- paste0("C:/Users/usuario/Documents/",liga,"/",temporada_coincidente,"_",liga,".csv")
    carpeta <- paste0(temporada_coincidente,"_",liga,".csv")
    download.file(url,carpeta)
  } else {
    cat("La temporada de fútbol correspondiente al año actual no está en la lista.\n")
  }
}

descarga_historico_temp()
actualiza_liga_actual("SP1")