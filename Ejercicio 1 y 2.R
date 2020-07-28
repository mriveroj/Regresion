#Ejercicio 1
datos <- read.csv(file="SnakeRiver.csv")
datos <- read.csv(file="SnakeRiver.csv",sep=";")
attach(datos)
datos2 <- read.csv2(file="SnakeRiver.csv")
summary(datos)
plot(datos)
fit <- lm(NivelRio~Nieve, data = datos)
summary(fit)
fit1 <- lm(NivelRio~0+Nieve, data=datos)
summary(fit1)
anova(fit1)
par(mfrow=c(2,2))
plot(fit1)
library(car)
shapiro.test(fit1$residuals)
durbinWatsonTest(fit)
ncvTest(fit1)
newd<-data.frame(Nieve=c(24.65,23.73))
predict(fit,newdata = newd)
predict(fit,newdata = newd, interval = "confidence") 
predict(fit, newdata=newd, interval = "prediction")

#Ejercicio 2
datos<-read.csv(file="problemrlm1.csv",sep=";")
summary(datos)
#Diagramas de dispersión múltiples
pairs(datos)
#Para ver la correlación entre las varibles
cor(datos)
fit<-lm(TiempoSum~envases+Distancia, data=datos)
summary(fit)
anova(fit)
#SUPUESTOS
par(mfrow=c(2,2))
plot(fit)
#Normalidad
shapiro.test(fit$residuals)
#Independencia de los errores
install.packages("car")
require(car)
durbinWatsonTest(fit)
#HOMOCEDASTICIDAD O VARIANZA CONSTANTE
ncvTest(fit)
#INTERVALOS
newd=data.frame(envases=18,Distancia=450)
predict(fit,newdata = newd,interval = "confidence")
predict(fit,newdata=newd,interval="prediction")


