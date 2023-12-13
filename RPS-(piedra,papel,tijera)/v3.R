#Creamos una función para generar la contrajugada del bot
generar_eleccion_amaniada <- function(df) {
  
  #Creamos una tabla de frecuencias con el dataframe del histórico de elecciones de todos los jugadores
  tabla_frecuencias <- table(df$Elecciones, df$Resultados)
  
  # Verificar si la columna "g" existe en tabla_frecuencias, que es la opción 'ganadora' del jugador
  if ("g" %in% colnames(tabla_frecuencias)) {
    
    # Encontrar la elección más frecuente asociada a "g"
    eleccion_mas_frecuente <- rownames(tabla_frecuencias)[which.max(tabla_frecuencias[,"g"])]
    
    # Elige el movimiento que vencería a la opción del usuario más común
    if (eleccion_mas_frecuente == "r") {
      opcion_bot <- sample(c("p", "r", "s"), 1, prob = c(0.4, 0.3, 0.3))
    } else if (eleccion_mas_frecuente == "p") {
      opcion_bot <- sample(c("p", "r", "s"), 1, prob = c(0.3, 0.3, 0.4))
    } else {
      opcion_bot <- sample(c("p", "r", "s"), 1, prob = c(0.3, 0.4, 0.3))
    }
    
  #En el caso de que no haya partidas suficientes para obtener la opción 'g',
  #se generará una opción aleatoria sin amañar  
  }else{
    opcion_bot <- sample(c("p", "r", "s"), 1, prob = c(1/3, 1/3, 1/3))
  }

  # Devolver la elección amañada
  return(opcion_bot)
}

#Esta sería la función para albergar a nuestro programa
partida <- function(){
  
  #Introducimos el nombre
  cat("\nIntroduzca nombre jugador:")
  nombre_jugador <- readline()
  
  #Inicializamos las variables
  sesion_ganadas <- 0
  sesion_empates <- 0
  sesion_perdidas <- 0
  Elecciones <- c()
  Resultados <- c()
  cont <- 0
  
  #Actualizamos los dataframes para la partida actual:
  
  #En el primero guardamos las elecciones 'r', 'p' o 's' y su resultado vs la
  #máquina, si gana, empata o pierde para generar la contrajugada
  lista_actual <- data.frame(Elecciones = c(),
                             Resultados = c(),
                             stringsAsFactors = FALSE)
  
  #En el segundo guardamos el histórico informativo
  historico_actual <- data.frame(Jugador = nombre_jugador,
                                 Ganadas = sesion_ganadas,
                                 Empates = sesion_empates,
                                 Perdidas = sesion_perdidas,
                                 stringsAsFactors = FALSE)
  
  #En caso de ser la primera vez que jugamos, crearemos también los dataframes 
  #para el histórico total tanto el que servirá para la contrajugada como
  #el informativo
  if(!exists("lista_general") || !exists("historico_general")){
    
    lista_general <- data.frame(Elecciones = c(),
                                Resultados = c(),
                                stringsAsFactors = FALSE)
    
    historico_general <- data.frame(Jugador = character(),
                                    Ganadas = numeric(),
                                    Empates = numeric(),
                                    Perdidas = numeric(),
                                    stringsAsFactors = FALSE)
  }
  
  #Con este bucle estaremos navegando entre las distintas opciones del 
  #programa hasta que salgamos del mismo
  while(TRUE){
    
    #Introducimos la opción elegida
    cat("\nIntroduzca piedra (R), papel (P) o tijera (S): ")
    opcion_user <- tolower(readline())
    
    #pulsamos 'q' para salir  
    if(opcion_user == "q"){
      cat("¡Fin de la partida!")
      
      #Actualizamos los dataframes y salimos
      historico_general <- rbind(historico_general,data.frame(Jugador = nombre_jugador,
                                                              Ganadas = sesion_ganadas,
                                                              Empates = sesion_empates,
                                                              Perdidas = sesion_perdidas,
                                                              stringsAsFactors = FALSE))
      
      #Con esto controlamos la primera vez que jugamos, para guardar los datos 
      #en el fichero o crear el fichero y guardar los datos
      if(exists("historico.csv") && exists("lista.csv")){
        write.csv(rbind(read.csv("historico.csv"),historico_general),"historico.csv",row.names = FALSE)
        write.csv(rbind(read.csv("lista.csv"),lista_general),"lista.csv",row.names = FALSE)
      }else{
        write.csv(historico_general,"historico.csv",row.names = FALSE)
        write.csv(lista_general,"lista.csv",row.names = FALSE)
      }
      break
      
    #Pulsamos 'h' para ver el histórico de la partida actual   
    }else if(opcion_user == "h"){
      cat("\nHistórico de partida actual\n")
      cat("--------------------------------------\n")
      print(historico_actual)
      cat("--------------------------------------\n")
      
    #Pulsamos 'c' para histórico total de partidas     
    }else if(opcion_user == "c"){
      
      #Si no existen registros previos, indicarlo
      if(!file.exists("historico.csv")){
        cat("No existen registros de partidas anteriores\n")
      
      #En caso de que existan, se exponen  
      }else{
        cat("\nHistórico del total de partidas\n")
        cat("--------------------------------------\n")
        print(rbind(read.csv("historico.csv"),historico_actual))
        cat("--------------------------------------\n")
      }
      
    #Pulsamos 'r', 'p' o 's' para jugar    
    }else if(opcion_user %in% c("r","p","s")){
      
      #Con el contador controlamos el par de situaciones siguientes:
      cont <- cont + 1
      
      #En caso de ser la primera vez que jugamos, controlamos si existe el archivo o no
      if(exists("lista.csv")){
        
        #Para decidir las 3 primeras jugadas tomamos el histórico total de elecciones de jugadores
        if(cont <= 3){
          opcion_bot <- generar_eleccion_amaniada(read.csv("lista.csv"))
        
        #Para el resto de jugadas, tomamos el histórico total del jugador actual  
        }else{
          opcion_bot <- generar_eleccion_amaniada(lista_actual)
        } 
      
      #Si es la primera vez, generamos una opción aleatoria
      }else{
        opcion_bot <- sample(c("p", "r", "s"), 1, prob = c(1/3, 1/3, 1/3))
      }
      
      #Indicamos el resultado de nuestra elección
      if((opcion_user == "r" & opcion_bot == "s") | (opcion_user == "p" &  opcion_bot == "r") | (opcion_user == "s" & opcion_bot == "p")){
        cat("GANASTE: Has elegido ",toupper(opcion_user)," VS ",toupper(opcion_bot),"\n")
        sesion_ganadas <- sesion_ganadas + 1
        resultado <- "g"
      }else if(opcion_user == opcion_bot){
        cat("EMPATE: Has elegido ",toupper(opcion_user)," VS ",toupper(opcion_bot),"\n")
        sesion_empates <- sesion_empates + 1
        resultado <- "e"
      }else{
        cat("PERDISTE: Has elegido ",toupper(opcion_user)," VS ",toupper(opcion_bot),"\n")
        sesion_perdidas <- sesion_perdidas + 1
        resultado <- "p"
      }
      cat("--------------------------------------\n")
      
      #Actualizamos los dataframes
      ##########################################################################
      historico_actual <- data.frame(Jugador = nombre_jugador,
                                     Ganadas = sesion_ganadas,
                                     Empates = sesion_empates,
                                     Perdidas = sesion_perdidas,
                                     stringsAsFactors = FALSE)
      
      lista_actual <- rbind(lista_actual,data.frame(Elecciones = c(Elecciones,opcion_user),
                                                    Resultados = c(Resultados,resultado),
                                                    stringsAsFactors = FALSE))
      
      lista_general <- rbind(lista_general,data.frame(Elecciones = c(Elecciones,opcion_user),
                                                      Resultados = c(Resultados,resultado),
                                                      stringsAsFactors = FALSE))
      ##########################################################################
    
    #En caso de introducir una tecla no válida, indicamos el mensaje de error    
    }else{
      cat("\nError, tecla no válida\n")
    }
  }
}

#Ejecutamos el programa
partida()
