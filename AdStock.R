
# El df (la base inicial) debe venir en el siguiente formato:
# El primer campo es la variable Fecha
# Los siguientes son las variables que no tienen relación con inversión en medios
# Luego las variables de inverción en medios (O cualquier otra que requiera un proceso de AdStock como TRP)
# Al final va la variable objetivo o predictiva (generalmente VENTA)
# Los nombres de los cambpos son irrelevantes

AdStock <- function(df,field,rate = 0.9) { #Ejemplo: AdStock(Base,35,0.7)
## Effect = Spend + last_week_effect * decay

    df$Effect <- 0 #Crea un campo nuevo, con ceros, llamado Effect
    df[is.na(df)] <- 0 #Tofos los na los transforma en cero
    #Este ciclo recorre desde 1 al numero de filas
    for (i in 1:nrow(df)) {
      
      if (i == 1) #Para la primera fila. length(colnames(df) representa la columna última, en este caso effect
        df[i,length(colnames(df))] <- df[i,field] #aquí reemplazamos el primer registro de effect por el primer dato de el campo field
      else
        df[i,length(colnames(df))] <- df[i,field] + (df[i-1,length(colnames(df))] * rate) #Para las filas siguientes, se reemplaza el cero inicial por la formula
      
    }
    
    return(df$Effect) #Se retorna el vector completo ajustado por el decay, que es el adstock
    drop <- c('Effect') #Se elimina effect para dejar el dataframe como estaba
    df <- df[ , !(names(df) %in% drop)]
}


Opt_Adstock <- function(df,spend_field,media) { #Ejemplo Opt_Adstock(Base,39,c(5:8))
  #df, es el data frame inicial
  #spend_field, es el número de la columna donde está la venta
  #media, es un vector con todos los numoers de columnas donde hay inversión en medios

  rates <- seq(0.001,0.999,0.001) #Creamos un vector con una secuencia de ratios, de 0.1 a 0.9 avanzando de 0.001 Para los ratios
  
  v <- df[,spend_field]
  
  fin <- NULL #Se crea una variable nula, si no se agrega genera error
  for(j in media) { #For para avanzar en las columnas de medios
    c <- NULL #Se crea una variable nula, si no se agrega genera error
    for(i in rates) { #Ciclo para probar con todos los ratios
        x <- AdStock(df,j,i) #Se ocupa la función Adstock para el rato y medio elegido
        c1 <- cbind(cor(x,v),i) #Se saca la correlación entre x y v, y se ocupa cbind (column bind) para hacer un vector 1x2 con la correlación y el ratio 
        c <- rbind(c,c1)#Agrega filas hacia abajo con la correlación y cada ratio
        c <- as.data.frame(c) #guarda todo esto en un dataframe llamad c
    }
  colnames(c) <- c('corr','dacay') #Agregamos nombres a las columnas de df
  c$sign <- sign(c$corr) #Agregamos una nueva columna con el "signo" de la correlación + o -
  c$corr <- abs(c$corr) #Sacamos el valor absoluto de la correlación
  cmax <- c[c[,1] == max(c[,1]),] #Se filtra c por el máximo
  cmax$field <- j #Se agrega la columna con el número del campo
  f1 <- cmax #Se renombra cmax en f1
  fin <- rbind(fin,f1) #Los f1 se van acumulando hacia abajo en un dataframe
  print(paste((j-min(media)+1)/length(media)*100,'%',sep = ' ')) #Se agrega porcentaje de avance
  }
  return(fin)
}



db_AdStock <- function(df,Opt_Ad) { #Ejemplo db_AdStock(Base,Opt_cor), donde Opt_cor <- Opt_Adstock(Base,39,c(5:38))

  fields <- Opt_Ad$field #Se dejan los numeros de los campos en un vector
  
  Ini <- df[,1:min(fields)-1] #Se toman las primeras variables del df. Las que no tienen relación con medios
  for(i in fields) { #Se recorre el vector de los campos de medios
    
    as1 <- AdStock(df,i,Opt_Ad[Opt_Ad$field == i, 2]) #Se calcula el vector de AdStock 
    as1 <- as.data.frame(as1) #Se cambia el tipo de dato a dataframe
    colnames(as1) <- paste(colnames(df)[i],'AS',sep = '_') #Se le agrega _AS al nombre de cada variable de medios
    Ini <- cbind(Ini,as1) #Se anexa a los campos iniciales
  }
  db_Modeling <- cbind(Ini,df[,length(colnames(df))]) #Se agrega la columna con la variable objetivo o predictiva (Por ejemplo VENTAS)
  colnames(db_Modeling)[length(colnames(db_Modeling))] <- 'Y' #Se renombra la variable objetivo como 'Y'
  
  return(db_Modeling) #Se retorna la base original con los campos actualizados por efecto publicitario
  }

