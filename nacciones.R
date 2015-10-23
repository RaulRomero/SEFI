# Código para descargar precio de "n" acciones
# Por: Rodrigo Hernández Mota

# Este código devuelve los precios de "n" activos contenidos en la base de
# datos de Yahoo Finance. Se realiza una prueba para comprobar que no haya
# datos faltantes (NA) [ver desde línea de código 39-76]. En caso de existir NA's
# se ejecuta un filtro para eliminarlos de la matriz de precios.
# Nomenclatura de matriz de precios original: m_precios
# Nomenclatura de matriz de precios filtrada: m_fprecios
# Nomenclatura de matriz de precios resultante: precios
# - Opcional: descargar la matriz de precios a un archivo .csv
#             - requisitos: cambiar "flag" y agregar workspace.
# Otras consieraciónes, editar:
# - Editar workspace [ver línea 23]
# - Nombre de acciones [ver línea 25]
# - Intervalo de tiempo [ver línea 29]
# - Matriz de precios [ver línea 35-36]
# - Flag [ver línea 86]


# paquete y workspace
require (quantmod)
#setwd("C:/Users/Rodrigo/Desktop")

# Nombrar acciones de interés - EDITAR{n_acciones}
n_acciones <- c("FRANOPRB1.MX","BBVANDQGB.MX","STERNDQB1.MX","NTEUSAI.MX","ACTI500B3.MX","^MXX")
n <- length(n_acciones)

# Intervalo de tiempo para los datos - EDITAR{inter_t}
inter_t <- c(toString(as.Date(as.numeric(Sys.Date())-365*2)),  
             toString(as.Date(Sys.Date())))


getSymbols(n_acciones, src = 'yahoo', from = inter_t[1], to = inter_t[2])

# Matriz de precios  - EDITAR{m_precios}
m_precios <- cbind(FRANOPRB1.MX[, 4],BBVANDQGB.MX[, 4],STERNDQB1.MX[, 4],NTEUSAI.MX[, 4],ACTI500B3.MX[, 4], MXX[, 4])
nr <- nrow(m_precios)
nc <- ncol(m_precios)

# Determinar si hay NA y filtro automático
cont_na <- 0 #numeric(n)
i <- 1;j <- 1;
while(i <= nr){
  if(is.na(m_precios[i,j])==T){
    cont_na <- cont_na + 1
    i <- i + 1
    j <- 1
  } else {
    if(j == nc){
      j <- 1
      i <- i + 1
    }else{
      j = j + 1
    }
  }
}

if(cont_na > 0){
  m_fprecios <- matrix( , ncol=nc, nrow = nr - cont_na) # matriz de precios filtrados
  i <- 1; j <- 1; k <- 1;
  while(i <= nr){
    if(is.na(m_precios[i,j])==T){
      i <- i + 1
      j <- 1
    } else {
      if(j == nc){
        m_fprecios[k, ] <- m_precios[i, ]
        i <- i + 1
        j <- 1
        k <- k + 1
      }else{
        j = j + 1
      }
    }
  }
  colnames(m_fprecios) <- n_acciones
}

if(cont_na > 0){
  precios <- as.matrix(m_fprecios)
} else {
  precios <- as.matrix(m_precios)
}

# Descargar precios a hoja de cálculo .csv {0,1} - EDITAR{flag}
flag <- 0
if (flag == 1){
  write.csv(precios, 'datos1.csv')
}

# Verificar ausencia de NA
contador <- numeric(ncol(precios))
for(i in 1:nrow(precios)){
  for(j in 1:ncol(precios)){
    if(is.na(precios[i,j])==T){
      contador[j] <- contador[j] + 1
    }
  }
}

cat("\014")
cont_na
contador

# Borrar objetos excepto {precios, }
rm(list=setdiff(ls(), c("precios", "n_acciones")))

