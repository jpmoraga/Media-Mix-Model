
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

media <- c(5:38)
v <- Base_OMO$VENTA


rates <- seq(0.1,0.9,0.001)


f <- NULL #Se crea una variable nula, si no se agrega genera error
for(j in media) {
c <- NULL #Se crea una variable nula, si no se agrega genera error
for(i in rates) {
    x <- AdStock(Base_OMO,j,i)
    c1 <- cbind(cor(x,v),i)
    c <- rbind(c,c1)
    c <- as.data.frame(c)
    cmax <- c[c[,1] == max(c[,1]),]
    cmin <- c[c[,1] == min(c[,1]),]
    c <- rbind(cmax,cmin)
    c <- c[c[,1] == max(c[,1]),]
    }
colnames(c) <- c('corr','dacay')
c$field <- j
f1 <- c
f <- rbind(f,f1)
print(j)
}
f <- unique(f)


