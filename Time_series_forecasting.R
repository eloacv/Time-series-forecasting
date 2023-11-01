#Cargando librerias
library(readxl)
library(tidyverse)
library(forecast)
library(tseries)


#Leyendo datos del Excel
datos <- read_excel("afluenciaMP.xls")
#Convirtiendo los meses en número y los años que están en chr en numericos
meses <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Set", "Oct", "Nov", "Dic")
datos <- datos %>%
  mutate(Mes = sprintf("%02d", match(Mes, meses)))
datos$Mes <- as.numeric(datos$Mes)
datos$`2004` <- replace_na(as.numeric(datos$`2004`), 0)
datos$`2010` <- replace_na(as.numeric(datos$`2010`), 0)
datos$`2020` <- replace_na(as.numeric(datos$`2020`), 0)
datos$`2021` <- replace_na(as.numeric(datos$`2021`), 0)
#Quedar con 2 columnas "fecha" y numeo de visitantes. Para fecha colocar anio y mes
datos_modificados <- datos %>%
  pivot_longer(cols = starts_with("20"), names_to = "fecha", values_to = "visitante") %>%
  mutate(fecha = paste(fecha, Mes, sep = "-")) %>%
  select(-Mes) %>%
  mutate(fecha = as.Date(paste0(fecha, "-01"), format = "%Y-%m-%d")) %>%
  mutate(fecha = format(fecha, "%Y-%m")) %>%
  arrange(fecha)
datos_modificados

#Tabla con los 10  primeros datos
knitr::kable(head(datos_modificados, 10))
knitr::kable(tail(datos_modificados, 12))


#Representación grafica 

#Creacion de objeto timeseries
afluencia <- ts(datos_modificados[,-1], start=c(2004,1), frequency=12)
#Reemplazando NAs por 0
afluencia[is.na(afluencia)] <- 0
autoplot(afluencia)+ ggtitle("Numero de visitantes nacionales a Machu Picchu ") + xlab("mes") + ylab("afluencia")

# Prueba de KPSS para saber si es una serie estacionaria
kpss_test <- kpss.test(afluencia)
print(kpss_test)


#Calculo de coeficientes de estacionalidad
afluencia_comp <- decompose(afluencia, type=c("additive"))


#Tabla con los coeficientes 
knitr::kable(afluencia_comp$figure, digits =2,caption = "Coef Estacionalidad")
print(afluencia_comp$seasonal)

#Plot serie, componente estacional, estimacion de tendencia, error
autoplot(afluencia_comp)

#Plot tendencia calculada con la descomposición y la serie ajustada estacionalmenyte

autoplot(afluencia, series="Datos") +
  autolayer(trendcycle(afluencia_comp), series="Tendencia")+
  autolayer(seasadj(afluencia_comp), series="Estacionalmente ajustada")+
  xlab("Year") + ylab("Afluencia") +
  ggtitle("Serie de afluencia") +
  scale_colour_manual(values=c("gray","blue","red"),
                      breaks=c("Datos","Estacionalmente ajustada","Tendencia"))

#Plot series de cada año
ggseasonplot(afluencia, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("Numero") +
  ggtitle("Seasonal plot: afluencia")


#Pregunta 3 reservamos los últimos datos observados
afluencia_train <- window(afluencia, end=c(2020,12))

#Pregunta 4 modelo suavizado exponencial 

#Modelo Holt Winters
afluencia_sh <- hw(afluencia_train,seasonal="additive", h=12, level = c(80, 95))
autoplot(afluencia_sh)

autoplot(afluencia) + autolayer(afluencia_sh, PI = FALSE)

#Mostrar una tabla con las predicciones
knitr::kable(forecast(afluencia_sh,h=12), digits =4,caption = "Predicciones ")

#tabla con los estimadores de los parámetros del modelo elegido
knitr::kable(afluencia_sh$model$par, format = "pipe", digits = 4, caption = "Estimadores de los parámetros")

#Pregunta 5 Representar la serie y los correlogramas

#Autocorelaciones simples hasta el retardo 48

ggAcf(afluencia_train, lag= 48)

#Comentario: Se observa que decrece lentamente y que los retardos múltiplos de 12 presentan una correlación fuerte.

corr<-Acf(afluencia_train, lag=48)
print(corr)

#Autocorrelaciones parciales hasta el retardo 48 

ggPacf(afluencia_train, lag=48)

corrp<-Pacf(afluencia_train, lag=12)
print(corrp)


#Serie diferenciada
autoplot(diff(afluencia_train))+ ggtitle("Afluencia de turistas a Machu Picchu") +
  xlab("mes") + ylab("turistas")

#Autocorrelaciones simples hasta el retardo 48
ggAcf(diff(afluencia_train), lag=48)


#Calculamos las autocorrelaciones parciales hasta el retardo 48
ggPacf(diff(afluencia_train), lag=48)

#Diferenciacion de orden estacional

autoplot(diff(diff(afluencia_train),12))+ ggtitle("Afluencia de turistas a Machu Picchu") +
  xlab("mes") + ylab("turistas")

#Autocorrelaciones simples hasta el retardo 48

ggAcf(diff(diff(afluencia_train),12), lag=48)

#Calculamos las autocorrelaciones parciales hasta el retardo 48

ggPacf(diff(diff(afluencia_train),12), lag=48)

#Ajuste manual modelo ARIMA
fitafluencia1 <- Arima((afluencia_train),c(0,1,1),seasonal=c(0,1,1))
checkresiduals(fitafluencia1)
print(fitafluencia1)

knitr::kable(accuracy(fitafluencia1), digits = 4, caption = "Medidas de ajuste")

#Funcion auto ARIMA

fitafluencia2 <- auto.arima(afluencia_train, seasonal = TRUE)
checkresiduals(fitafluencia2)
print(fitafluencia2)

knitr::kable(accuracy(fitafluencia2), digits = 4, caption = "Medidas de ajuste")

#Pregunta 7
autoplot(forecast(fitafluencia1,h=12))

knitr::kable(forecast(fitafluencia1,h=12))
forecast(fitafluencia1)

#Pregunta 8

autoplot(afluencia) +
  autolayer(forecast(afluencia_sh,h=12), series="suavizado", PI=FALSE) +
  autolayer(forecast(fitafluencia1,h=12), series="manual", PI=FALSE) + 
  ggtitle("Prediciones por diferentes modelos ") + xlab("mes") +
  ylab("numero") +
  guides(colour=guide_legend(title="Forecast"))


#funcion cbind crear dataframe con serie original, el logaritmo de la serie, el log diferenciado y la diff estacional

cbind("Nº turistas"= afluencia,
      "log(afluencia)" = log(afluencia),
      "primera diferencia log(afluencia)" = diff(log(afluencia)),
      "diferencia anual log(afluencia)" = diff(diff(log(afluencia)), 12))%>%
  autoplot(facets=TRUE) + xlab("mes") + ylab("") + ggtitle("Afluencia turistas")

knitr::kable(accuracy(afluencia_sh), digits = 4, caption = "Medidas de ajuste")

fitafluencia1 <- Arima((afluencia_train),c(0,1,1),seasonal=c(0,1,1))
checkresiduals(fitafluencia1)
print(fitafluencia1)


checkresiduals(afluencia_sh)
print(afluencia_sh)

knitr::kable(fitafluencia1$model$par, format = "pipe", digits = 4, caption = "Estimadores de los parámetros")
