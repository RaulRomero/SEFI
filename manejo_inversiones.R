#C�digo para realizar inversiones de SEFI

# nacciones ---------------------------------------------------------------


require(quantmod)
require(timeSeries)

# Nombrar acciones de inter�s - EDITAR{n_acciones}
n_acciones <- c("GBMMODBO.MX","ACTI500B3.MX","FRANOPRB1.MX","^MXX")
n <- length(n_acciones)

# Intervalo de tiempo para los datos - EDITAR{inter_t}
inter_t <- c("2013-10-23",toString(as.Date(Sys.Date())))


getSymbols(n_acciones, src = 'yahoo', from = inter_t[1], to = inter_t[2])

# Matriz de precios  - EDITAR{m_precios}
m_precios <- cbind(GBMMODBO.MX[, 4], 
                   ACTI500B3.MX[, 4],FRANOPRB1.MX[, 4], MXX[, 4])
nr <- nrow(m_precios)
nc <- ncol(m_precios)

# Determinar si hay NA y filtro autom�tico
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

# Descargar precios a hoja de c�lculo .csv {0,1} - EDITAR{flag}
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
rendimientos <- precios[2:nrow(precios), 1:(ncol(precios) - 1) ] / precios[1:(nrow(precios) - 1), 1:(ncol(precios) - 1)] - 1


# Markowitz ---------------------------------------------------------------

filas <- c('Media', 'Varianza')
estadisticos <- matrix( , nrow = length(filas), ncol = (ncol(precios) - 1))
colnames(estadisticos) <- n_acciones[1:(length(n_acciones)-1)]
rownames(estadisticos) <- filas

activos <- matrix( , nrow = n, ncol = 6)
colnames(activos) <- c(n_acciones[1:3],'Valor Esperado','Varianza','Desviaci�n')


estadisticos[1, ] <- colMeans(rendimientos)
estadisticos[2, ] <- colVars(rendimientos)

mcov <- cov(rendimientos)
n  <- 10000
for(i in 1:n){
  p1  <- runif(1)
  p2 <- runif(1)*(1-p1)
  p3 <- 1-(p1+p2)
  activos[i,1] <- p1
  activos[i,2] <- p2
  activos[i,3] <- p3
  activos[i,4] <- 100*((p1* estadisticos[1, 1]) + (p2* estadisticos[1, 2])+(p3* estadisticos[1, 3]))
  activos[i,5] <- ((p1^2)* estadisticos[2, 1]) + ((p2^2)* estadisticos[2, 2])+((p3^2)* estadisticos[2, 3])+
    (2*p1*p2*mcov[2, 1] ) + (2*p1*p3*mcov[3, 1]) + (2*p2*p3*mcov[2, 3])
  activos[i,6] <- 100*(sqrt(activos[i,5]))
}#cierre del for para Markowitz y sus c�lculos
temporal<-as.data.frame(activos)

ggplot(temporal,aes(x=temporal[,6],y=temporal[,4]))+geom_point()+labs(title='Portafolios formados',x='Desviaci�n %',y='Valor esperado %')

#Encontrando proporicones que maxmizan el rendimiento
capital <- 150000 #MXN
renglon <- 0
for(i in 1:nrow(activos)){
  if(activos[i, 4] == max(activos[, 4]))
    renglon <- i
}
proporciones <- c(activos[renglon, 1], activos[renglon, 2], activos[renglon, 3])

#Simulando el precio para ma�ana


# Estimaciones diaras con dSt -----------------------------------------------------
#dst = media*dt + sigma*rnorm*sqrt(t)
require(timeSeries)
dst <- numeric(3)

medias <- colMeans(rendimientos)
sigmas <- colSds(rendimientos)

n <- 10000
estimacion <- matrix( , nrow = n, ncol = length(medias))

for (i in 1:n) {
estimacion[i, ] <- (medias + sigmas*rnorm(1))
}

estimaciones.2 <- numeric(length(medias))
estimaciones.2 <- colMeans(estimacion)
names(estimaciones.2) <- colnames(rendimientos)

#Estimacion para ma�ana
precio_ma�ana <- precios[nrow(precios), 1:(ncol(precios) - 1)] * 
  (1 + estimaciones.2)

#St = So...

estimacion_st <- matrix( , nrow = n, ncol = length(medias))

for (i in 1:n) {
  estimacion_st[i, ] <- precios[nrow(precios), 1:(ncol(precios) - 1)]*
    exp(medias - ((sigmas^2)/2) + sigmas*rnorm(1))
}

estimacion_st.2 <- colMeans(estimacion_st)
estimaciones.2 
(estimacion_st.2/precios[nrow(precios), 1:(ncol(precios) - 1)] - 1)



