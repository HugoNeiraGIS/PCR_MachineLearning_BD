################################################################
###Trabajo 1: Trabajo reflexivo de inferencia
### Prof.  Luis Carvacho
###Alumno. Hugo Neira
################################################################

# fijar carpeta de trabajo
setwd('C:/R_ARCHIVOS')
df <- read.csv('Trabajo reflexivo de inferencia_HN.csv', header = T, dec = ",", sep = ";")
print(df)
plot(df[c(-1)])
summary(df)
print(df)
#revisar la tabla
View(df)
#Contar el total de NAs en la df de datos (Para tomar accione)
sum(is.na(df))
#Saber el nÃºmero de NAs por columna (Para tomar accione)
colSums(is.na(df))

library(corrplot) #Librería para el gráfico de correlaciones
#Gráfico de las correlaciones
corrplot(cor(df[,-1]), order = "hclust", tl.col='black', tl.cex=1) 

#matiz de correlaciÃ³n
cor(df[c(-1)])

#Primero haremos una regresión multiple (evaluar multicolinealidad), 
#luego PCA para evaluar la las denuncias. Finalmente PCR y PLS usando Machine Learning para evaluar la las denuncias.

################################################################
## Regresión Multiple
################################################################

#redondear correlacion opcional
round(cor(x = df[-c(1)], method = "pearson"), 3)
#Graficamos histogramas de las variables 
library(psych)
multi.hist(x = round(cor(x = df[-c(1)], method = "pearson"), 3), dcol = c("blue", "red"), dlty = c("dotted", "solid"),
           main = "")
#Graficamos correlaciones con graficos, diagramas de dispersión, 
#los valores de correlación para cada par de variables y 
#la distribución de cada una de las variables.
library(GGally)
ggpairs(df[-c(1)], lower = list(continuous = "smooth"),
        diag = list(continuous = "barDiag"), axisLabels = "none")
#Modelo de regresión
modelo <- lm(Denuncias ~ Poblacion + Sobre_Inf + Cesantes + Tasa_Drog +
               Bares.y.CT + Sin_Carabineros + M2A_Verd + Viv_defic, data = df )
summary(modelo)

#El modelo con todas las variables introducidas como predictores tiene un R2 alta (0.9946), 
#es capaz de explicar el 99,46% de la variabilidad observada en la Denuncias. 
#El p-value del modelo es significativo (< 2.2e-16).

#Selección de los mejores predictores
step(object = modelo, direction = "both", trace = 1)

#El mejor modelo resultante del proceso de selección ha sido
modelo_step <- lm(Denuncias ~ Poblacion + Cesantes + Tasa_Drog +
                  Bares.y.CT + Sin_Carabineros + M2A_Verd + Viv_defic, data = df )
summary(modelo_step)

round(cor(x = df[-c(1, 3, 5)], method = "pearson"), 3)
#La ecuación predice en un 99,41% los cambios de las denuncias (multicolinealidad).
#El modelo lineal múltiple es capaz de explicar el % de la variabilidad observada en las denuncias.

#Matriz de correlación entre predictores.
library(corrplot)
corrplot(cor(dplyr::select(df, Poblacion, Cesantes, Tasa_Drog,
                             Bares.y.CT, Sin_Carabineros, M2A_Verd, Viv_defic)),
         method = "number", tl.col = "black")

####Opcional
#intervalo de confianza para cada uno de los coeficientes parciales de regresión
#confint(lm(formula = Denuncias ~ Poblacion + Cesantes + Tasa_Drog + Bares.y.CT + Sin_Carabineros + M2A_Verd + Viv_defic, data = df))

###Validación  corrregir predictores
library(ggplot2)
library(dplyr)
library(gridExtra)
plot1 <- ggplot(data = df, aes(Poblacion, modelo_step$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot2 <- ggplot(data = df, aes(Pob_Act, modelo_step$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot3 <- ggplot(data = df, aes(Tasa_Drog, modelo_step$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot4 <- ggplot(data = df, aes(Bares.y.CT, modelo_step$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot5 <- ggplot(data = df, aes(Carabineros.1000, modelo_step$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
grid.arrange(plot1, plot2, plot3, plot4, plot5)
#cumple la linealidad para todos los predictores

#Distribución normal de los residuos
qqnorm(modelo_step$residuals)
qqline(modelo_step$residuals)
#################################
####IMPORTANTE
#################################
#Factor de Inflación de la Varianza (VIF)
# mide la correlación y la fuerza de la correlación entre 
#las variables predictoras en un modelo de regresión
library(car)
vif(modelo_step)
#Un valor mayor que 5 indica una correlación muy alta entre una variable predictora dada y 
#otras variables predictoras en el modelo.

#Evaluar autocorrelación Durbin-Watson
dwt(modelo_step, alternative = "two.sided")
#existe autocorrelación negativa#

#PRUEBA DE NORMALIDAD DE SHAPIRO-WILK
#aplicable cuando se analizan muestras compuestas por menos de 50 elementos
#shapiro.test(modelo_step$residuals)
#which.max(modelo_step$residuals)
#shapiro.test(modelo$residuals[-50])

##########################################################
#  Se debe hacer PCA debido a los altos valores del VIF  #
##########################################################


################################################################
################################################################
## PCA Directo sobre las variables
################################################################

library(ISLR)
library(dplyr)
library(tidyr)
library(pls)
library(FactoMineR)
library(stats)
library(psych)
library(factoextra)
#Primeros 6 registros de la BD
head(df)

#Hacemos PCA de las variables
#pca <- prcomp(df[c(2, 4, 6, 7, 8)], scale = TRUE)
#Hacemos PCA de las variables
pca <- prcomp(df[c(-1)], scale = TRUE)
names(pca)
pca$center
pca$rotation
head(pca$x)
dim(pca$x)
#representación bidimensional de las dos primeras componentes
biplot(x = pca, scale = 0, cex = 0.6, col = c("blue4", "brown3"))

summary(pca)
print(pca)
#Plot de los PCA
fviz_eig(pca) #visualizar eigenvalores (scree plot)
fviz_screeplot(pca) #visualizar eigenvalores (scree plot)
fviz_pca_ind(pca) #Representacion de observaciones sobre componentes principales.
#Representa la contribuciÃ³n de filas/columnas de los resultados de un pca.
fviz_contrib(pca,choice = "var")
#Grafico PCA en dimensiones
dim_pca <- PCA(X = round(cor(x = df[-c(1,5)], method = "pearson"), 3), scale.unit = FALSE, ncp = 5, graph = TRUE)

library(ggplot2)
#la varianza explicada por cada una de ellas, 
#la proporción respecto al total y la proporción de varianza acumulada
pca$sdev^2
prop_varianza <- pca$sdev^2 / sum(pca$sdev^2)
prop_varianza

ggplot(data = data.frame(prop_varianza, pc = 1:9),
       aes(x = pc, y = prop_varianza)) +
  geom_col(width = 0.3) +
  scale_y_continuous(limits = c(0,1)) +
  theme_bw() +
  labs(x = "Componente principal",
       y = "Prop. de varianza explicada")

prop_varianza_acum <- cumsum(prop_varianza)
prop_varianza_acum

ggplot(data = data.frame(prop_varianza_acum, pc = 1:9),
       aes(x = pc, y = prop_varianza_acum)) +
  geom_col(width = 0.3) +
  scale_y_continuous(limits = c(0,1)) +
  theme_bw() +
  labs(x = "Componente principal",
       y = "Prop. de varianza acumulada")

ggplot(data = data.frame(prop_varianza_acum, pc = 1:9),
       aes(x = pc, y = prop_varianza_acum, group = 1)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  labs(x = "Componente principal",
       y = "Prop. varianza explicada acumulada")

#En este caso, la primera componente explica el % 
#de la varianza observada en los datos, la segunda el % y la tercera + 
#las tres primeras componentes explican el % de la varianza observada.

#############################################
#Otra forma de hacer PCA (Clases)

colnames(df)
#Ahora ejecutaremos el procedimiento principal() y almacenamos el resultado 
#en la variable pca2.
pca2<-principal(df[,-1], nfactors = 9, rotate = 'none')
#plot
pca2
#Dejamos los mejores factores acumulan un 97% de varianza esos tres factores.
pca2<-principal(df[,-1], nfactors = 4, rotate = 'none')
#plot
pca2

#No es necesario rotar, se deja como antecedente
pca2<-principal(df[,-1], nfactors = 3, rotate = 'varimax')
plot
pca2
#scores del data frame de componentes:
head(pca2$scores)
#representación bidimensional de las dos primeras componentes
biplot(x = pca2, scale = 0, cex = 0.6, col = c("blue4", "brown3"))

#Ahora pongamos los nombres correctos de esas variables:
colnames(pca2$scores)<-c("Delincuencia", "Sociedad", "Calidad de Vida")
#plot
head(pca2$scores)
#Y agreguemos los nombres de las Comunas a que corresponde cada puntaje 
#factorial y mostremos los primeros registros a ver
#Ahora creemos un nuevo data frame que contenga tanto los nombres de las 
#localidades como los puntajes. Para eso usaremos las localidades que quedaron 
#en el data frame dc.
del<-data.frame(df$ï..Comuna, pca2$scores)
#Listo. Ahora mostremos los primeros registros del nuevo data frame
head(del)
plot(del)

################################################################
#PCR Regresión de componentes principales - MACHINE LEARNING
################################################################

#Ajustaremos un modelo de regresión de componentes principales (PCR) 
#utilizando Denuncias como variable de respuesta y las siguientes variables 
#como variables predictoras:
#Poblacion + Cesantes + Tasa_Drog + Bares.y.CT + Sin_Carabineros + M2A_Verd + Viv_defic

df2 = na.omit(df) # Omit empty rows
#Semilla
set.seed (5)
print(df2)
head(df2)
#fit PCR model utilizando las variables del step de la regresión multiple
pcr_fit = pcr(Denuncias ~ Poblacion + Cesantes + Tasa_Drog +
                Bares.y.CT + Sin_Carabineros + M2A_Verd + Viv_defic, data = df, scale = TRUE, validation = "CV")

summary(pcr_fit)
print(pcr_fit)

validationplot(pcr_fit, val.type = "MSEP")

#visualize gráficos de validación cruzada
validationplot (pcr_fit)
validationplot (pcr_fit, val.type = "MSEP")
validationplot (pcr_fit, val.type = "R2")

#PCR on the training data and evaluate its test set performance
set.seed(6)

train = df %>%
  sample_frac(0.7)

test = df %>%
  setdiff(train)

pcr_fit2 = pcr(Denuncias ~ Poblacion + Cesantes + Tasa_Drog +
                 Bares.y.CT + Sin_Carabineros + M2A_Verd + Viv_defic, data = train, scale = TRUE, validation = "CV")
validationplot(pcr_fit2, val.type = "MSEP")
summary(pcr_fit2)

pcr_fit2_pred <- predict(pcr_fit2, test, ncomp=7)
summary(pcr_fit2_pred)
plot(pcr_fit2_pred)
list(pcr_fit2_pred)
#visualize gráficos de validación cruzada
validationplot (pcr_fit2)

#test MSE (error cuadrático medio)
x_train = model.matrix(Denuncias ~ Poblacion + Cesantes + Tasa_Drog +
                         Bares.y.CT + Sin_Carabineros + M2A_Verd + Viv_defic, train)[,-1]
x_test = model.matrix(Denuncias ~ Poblacion + Cesantes + Tasa_Drog +
                        Bares.y.CT + Sin_Carabineros + M2A_Verd + Viv_defic, test)[,-1]

y_train = train %>%
  select(Denuncias) %>%
  unlist() %>%
  as.numeric()

y_test = test %>%
  select(Denuncias) %>%
  unlist() %>%
  as.numeric()

pcr_pred = predict(pcr_fit2, x_test, ncomp=5)
plot(pcr_pred)
mean((pcr_pred-y_test)^2)
sqrt (mean((pcr_pred-y_test)^2))

# RMSE de prueba resulta ser 
# Ésta es la desviación promedio entre el valor predicho de denuncias y 
# el valor observado de denuncias para las observaciones.
# El valor de la media entre los predichos y medidos es de 

x = model.matrix(Denuncias~., df)[,-1]

y = df %>%
  select(Denuncias) %>%
  unlist() %>%
  as.numeric()

pcr_fit2 = pcr(y~x, scale = TRUE, ncomp = 7)
summary(pcr_fit2)
pcr_fit2 = pcr(y~x, scale = TRUE, ncomp = 5)
summary(pcr_fit2)
#Con tres componentes se explica el 97.99
pcr_fit2 = pcr(y~x, scale = TRUE, ncomp = 3)
summary(pcr_fit2)
#prediccion con tres componentes
pcr_pred_fit2 = predict(pcr_fit2, x, ncomp=3)
plot(pcr_pred_fit2)
mean((pcr_pred_fit2-y)^2)
sqrt (mean((pcr_pred_fit2-y)^2))

#Ploteo de regresión entre los predichos y los medidos
plot(pcr_fit2, main='Denuncias')
validationplot(pcr_fit2, main ="Denuncias")
pcr_fit2$scores
plot(pcr_fit2$scores, main='Denuncias')
# opcionales
pcr_fit2$coefficients
#pcr_fit2$Xmeans
#pcr_fit2$Ymeans
#predicción de las denuncias
head(pcr_pred_fit2)
print(pcr_pred_fit2)
#asignar nombres de comunas
del2<-data.frame(df$ï..Comuna, pcr_pred_fit2)
colnames(del2)<-c("Comuna", "Predicción_Delincuencia")
print(del2)
View(del2)
#Ahora pongamos los nombres correctos de esas variables:
colnames(pcr_fit2$scores)<-c("Delincuencia", "Ciudadanía", "Seguridad")
#plot
head(pcr_fit2$scores)

################################################################
#Partial Least Squares - MACHINE LEARNING
################################################################

set.seed(4)
pls_fit = plsr(Denuncias ~ Poblacion + Pob_Act + Tasa_Drog +
                 Bares.y.CT + Carabineros.1000, data = df, scale = TRUE, validation = "CV")
summary(pls_fit)
validationplot(pls_fit, val.type = "MSEP")

pls_pred = predict(pls_fit, x_test, ncomp = 5)
mean((pls_pred - y_test)^2)
sqrt (mean((pcr_pred-y_test)^2))

pls_fit2 = plsr(Denuncias ~ Poblacion + Pob_Act + Tasa_Drog +
                  Bares.y.CT + Carabineros.1000, data = df, scale = TRUE, ncomp = 5)
summary(pls_fit2)
#Ploteo de regresión entre los predichos y los medidos
plot(pls_fit2, main='Denuncias')

#predicción de las denuncias PLS
head(pls_pred)

