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

#Ejercicio 3
datos<-read.csv(file="Credit.csv")
datos<-datos[,-1]
summary(datos)  
#Para ver la estructura de las variables que estoy usando
str(datos)
pairs(datos)
#Correlación de las variables numéricas (matriz de correlación):
cormat<-cor(cbind(datos$Income,datos$Limit,datos$Rating,
                  datos$Cards,datos$Age,datos$Education,datos$Balance))
cormat

pairs(datos$Balance~datos$Income+datos$Limit+
        datos$Cards+datos$Age+datos$Education)

pairs(Balance~Income+Limit+
        Cards+Age+Education)

#Para asignarle nombres a las columnas
colnames(cormat)<-c("Income","Limit","Rating","Cards","Age","Education","Balance")
#Para colocarle nombre a las filas
row.names(cormat)<-c("Income","Limit","Rating","Cards","Age","Education","Balance")
fit<-lm(Balance~.,data=datos)
summary(fit)
require(car)
#Ver la relación VIF
vif(fit)
#Removiendo rating
datos<-datos[,-3]
summary(datos)
#Para ver la relación entre dos variables independientes 
par(mfrow=c(2,2))
boxplot(Balance~Gender,data=datos,main="Gender")
boxplot(Balance~Student, data=datos, main="Student")
boxplot(Balance~Married, data=datos, main="Married")
boxplot(Balance~Ethnicity, data=datos, main="Ethnicity")
#PRUEBA DE INDEPENDENCIA DE LAS VARIABLES CATEGORICAS
chisq.test(datos$Student,datos$Gender)
#install.packages("leaps")
library(leaps)
par(mfrow=c(1,1))
#Todas las regresiones de la tabla es como el lm del fit
todas<-regsubsets(Balance~.,data=datos,nbest=100,really.big = T)
plot(todas)
#Todas las regresiones con el Cp
plot(todas,scale="Cp")

resultado=summary(todas)
#Out es un objeto que contiene todos los modelos que se crearon arriba. 
#La función with toma un objeto y lo utiliza para hacer operaciones. 
#En este caso nos interesa crear una matriz
out=with(resultado,cbind(which,adjr2,cp,bic))
summary(out)
#En la fila 156 esta el modelo que tiene el menor bic:
out=data.frame(out)
which.min(out$bic)
out[which.min(out$bic),]
