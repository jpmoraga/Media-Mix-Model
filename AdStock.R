
AdStock <- function(df,field,rate = 0.9) { #Ejemplo: AdStock(Base_OMO,35,0.7)
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

#AdStock(Base_OMO,35,0.7)

media <- c(5:34) #Columnas con inversión en medios
v <- Base_OMO$VENTA #Se guarda en v, la columna venta


rates <- seq(0.001,0.999,0.001) #Creamos un vector con una secuencia de ratios, de 0.1 a 0.9 avanzando de 0.001 Para los ratios


f <- NULL #Se crea una variable nula, si no se agrega genera error
for(j in media) { #For para avanzar en las columnas de medios
  c <- NULL #Se crea una variable nula, si no se agrega genera error
  for(i in rates) { #Ciclo para probar con todos los ratios
      x <- AdStock(Base_OMO,j,i) #Se ocupa la función Adstock para el rato y medio elegido
      c1 <- cbind(cor(x,v),i) #Se saca la correlación entre x y v, y se ocupa cbind (column bind) para hacer un vector 1x2 con la correlación y el ratio 
      c <- rbind(c,c1)#Agrega filas hacia abajo con la correlación y cada ratio
      c <- as.data.frame(c) #guarda todo esto en un dataframe llamad c
      #cmax <- c[c[,1] == max(c[,1]),] 
      #cmin <- c[c[,1] == min(c[,1]),]
      #c <- rbind(cmax,cmin) 
      #c <- c[c[,1] == max(c[,1]),]
  }
colnames(c) <- c('corr','dacay') #Agregamos nombres a las columnas de df
c$sign <- sign(c$corr) #Agregamos una nueva columna con el "signo" de la correlación + o -
c$corr <- abs(c$corr) #Sacamos el valor absoluto de la correlación
cmax <- c[c[,1] == max(c[,1]),] #Se filtra c por el máximo
cmax$field <- j #Se agrega la columna con el número del campo
f1 <- cmax #Se renombra cmax en f1
f <- rbind(f,f1) #Los f1 se van acumulando hacia abajo en un dataframe

print(j) 
}


