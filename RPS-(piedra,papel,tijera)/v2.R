#Función donde albergaremos todo el programa para ejecutarlo de un solo clic
partida <- function(){
  
  #Aquí declararemos el nombre del jugador actual
  cat("\nIntroduzca nombre de jugador: ")
  nombre_jugador <- readline()
  
  #En caso que sea la primera vez que ejecutamos el programa, nos creará un
  #dataframe para albergar el histórico de todos los jugadores
  if(!exists("partidas_df")){
    partidas_df <- data.frame(Jugador = character(),
                              Ganadas = numeric(),
                              Empates = numeric(),
                              Perdidas = numeric(),
                              stringsAsFactors = FALSE)
  }
  
  #Esta parte nos servirá para inicializar las variables del histórico actual
  #################################################
  sesion_ganadas <- 0
  sesion_empates <- 0
  sesion_perdidas <- 0
  
  #Y en concreto esta parte por si la primera elección es mostrar el historial
  actual <- data.frame(Jugador = nombre_jugador,
                       Ganadas = sesion_ganadas,
                       Empates = sesion_empates,
                       Perdidas = sesion_perdidas,
                       stringsAsFactors = FALSE)
  #################################################

  #Con este bucle estaremos navegando entre las distintas opciones del 
  #programa hasta que salgamos del mismo
  while (TRUE){
    
    #Enunciado del programa
    cat("\nIntroduzca piedra (R), papel (P) o tijera (S): ")
    
    #Recogemos la opción del usuario y la convertimos en minúscula para simplificar operaciones
    opcion_user <- tolower(readline())
    
    #Pulsar q para salir del programa
    if(opcion_user == "q"){
      
      #En el histórico general, añadiremos el histórico actual y se guardará
      partidas_df <- rbind(partidas_df,actual)
      
      #Con esto controlamos la primera vez que jugamos, para cargar los datos directamente
      #en el fichero o si tenemos que crearlo para guardar los datos
      if(exists("archivo.csv")){
        write.csv(rbind(read.csv("archivo.csv"),partidas_df),"archivo.csv",row.names = FALSE)
      }else{
        write.csv(partidas_df,"archivo.csv",row.names = FALSE)
      }
      cat("\n¡Fin de la partida!\n")
      break
      
    #Pulsar h para ver el histórico de la sesión actual
    }else if(opcion_user == "h"){
      cat("\nHistórico actual\n")
      cat("--------------------------------------\n")
      print(actual)
      cat("--------------------------------------\n")
      
    #Pulsar c para ver el histórico de jugadores  
    }else if(opcion_user == "c"){
      
      #En caso de que no existan registros previos, indicarlo
      if(!file.exists("archivo.csv")){
        cat("No existen registros de partidas anteriores\n")
      }else{
        cat("\nHistórico del total de partidas\n")
        cat("--------------------------------------\n")
        print(rbind(read.csv("archivo.csv"),actual))
        cat("--------------------------------------\n")
      }
      
    #Si pulsamos  r(piedra), p(papel) o s(tijera)  
    }else if(opcion_user %in% c("r","p","s")){
      
      #Se genera aleatoriamente la opción del bot
      opcion_bot <- sample(c("r","p","s"), 1)
      
      #Se comparan ambas opciones para determinar el ganador
      if((opcion_user == "r" & opcion_bot == "s") | (opcion_user == "p" &  opcion_bot == "r") | (opcion_user == "s" & opcion_bot == "p")){
        cat("GANASTE: Has elegido ",opcion_user," VS ",opcion_bot,"\n")
        sesion_ganadas <- sesion_ganadas + 1
      }else if(opcion_user == opcion_bot){
        cat("EMPATE: Has elegido ",opcion_user," VS ",opcion_bot,"\n")
        sesion_empates <- sesion_empates + 1
      }else{
        cat("PERDISTE: Has elegido ",opcion_user," VS ",opcion_bot,"\n")
        sesion_perdidas <- sesion_perdidas + 1
      }
      cat("--------------------------------------\n")
      
      #Actualizamos el resultado en el dataframe del jugador
      actual <- data.frame(Jugador = nombre_jugador,
                           Ganadas = sesion_ganadas,
                           Empates = sesion_empates,
                           Perdidas = sesion_perdidas,
                           stringsAsFactors = FALSE)
    
    #En caso de introducir cualquier otra cosa, avisar del error  
    }else{
      cat("Error, tecla no válida\n")
    }
  }
}

#Ejecutamos el juego
partida()
