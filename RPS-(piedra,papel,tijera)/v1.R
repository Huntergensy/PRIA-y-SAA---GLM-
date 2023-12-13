#Creamos una función que albergará nuestro juego
partida <- function(){
  
  #Inicializamos a 0 las variables del resultado del jugador
  sesion_ganadas <- 0
  sesion_empates <- 0
  sesion_perdidas <- 0
  
  #Creamos un bucle donde elegir entre las distintas opciones del juego
  while (TRUE){
    
    #Seleccionamos la opción
    cat("\nIntroduzca piedra (R), papel (P) o tijera (S): ")
    
    #Recogemos la opción del usuario y la convertimos en minúscula para simplificar operaciones
    opcion_user <- tolower(readline())
    
    #Pulsar q para salir del programa
    if(opcion_user == "q"){
      cat("\n¡Fin de la partida!\n")
      break
    
    #Pulsar h para ver el histórico de partidas  
    }else if(opcion_user == "h"){
      cat("\nSesión actual:\n")
      cat("--------------------------------------\n")
      cat("Ganadas: ",sesion_ganadas,"\n")
      cat("Empates: ",sesion_empates,"\n")
      cat("Perdidas: ",sesion_perdidas,"\n")
      cat("--------------------------------------\n")
      
    #Si pulsamos  r(piedra), p(papel) o s(tijera)  
    }else if(opcion_user %in% c("r","p","s")){
      
      #Se genera aleatoriamente la opción del bot
      opcion_bot <- sample(c("r","p","s"), 1)
      
      #Se comparan ambas opciones para determinar el ganador
      if((opcion_user == "r" & opcion_bot == "s") | (opcion_user == "p" &  opcion_bot == "r") | (opcion_user == "s" & opcion_bot == "p")){
        cat("GANASTE: Has elegido ",opcion_user," VS ",opcion_bot,"\n")
        sesion_ganadas <- sesion_ganadas + 1
      }else if(opcion_user == opcion_bot){
        cat("EMPATE : Has elegido ",opcion_user," VS ",opcion_bot,"\n")
        sesion_empates <- sesion_empates + 1
      }else{
        cat("PERDISTE: Has elegido ",opcion_user," VS ",opcion_bot,"\n")
        sesion_perdidas <- sesion_perdidas + 1
      }
      cat("--------------------------------------\n")
    
    #En caso de introducir una tecla no válida, comunicar del error  
    }else{
      cat("Error, tecla no válida\n")
    }
  }
}

#Ejecutamos el programa
partida()
