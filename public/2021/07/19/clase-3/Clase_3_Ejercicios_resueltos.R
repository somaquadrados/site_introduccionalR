#-----------------------------------------------------------------------#
#                             EJERCICIO 1                               #
#-----------------------------------------------------------------------#

#Estudiamos la abundancia de escarabajos en cuatro sitios (a,b,c,d) con diferentes grados de conservación. Medimos la humedad del suelo y el porcentaje de cobertura herbácea a un metro del suelo. 

#  OBJETIVO: Queremos evaluar si la humedad y la cobertura vegetal tienen alguna influencia sobre la abundancia de los escarabajos. 

ls()
rm(list=ls())
ls()
setwd("F:/eli/Curso de capacitacion R_INMeT/Clase_3/Ejercicios")
library(readr)#leer el archivo
library(modeest)
library(dplyr)#cuartiles
library(ggplot2)#graficos
library(psych)#sesgo y curtosis
library(tidyverse)
library(flextable)# para hacer tablas
datos<- read_delim("Ejercicio_1.csv", 
                  ";", escape_double = FALSE, trim_ws = TRUE)

View(datos)
datos$sitio <- as.factor(datos$sitio)

#1.Identifique la variable respuesta: ¿Qué tipo de variable es? ¿Qué valores mínimos y máximos toma?
  
#Variable respuesta: conteo, numerica discreta
min(datos$conteo)#0
max(datos$conteo)#13
datos$conteo <- as.integer(datos$conteo)
#  2.Identifique las variables explicativas: ¿Qué tipo de variable son? ¿Qué valores mínimos y máximos toman?

#variables predictoras: humedad, cobertura, continuas discretizadas
min(datos$humedad)#35.18562
max(datos$humedad)# 79.65838

min(datos$cobertura)#0.4344382
max(datos$cobertura)# 98.83085

datos$humedad <- as.numeric(datos$humedad)
datos$cobertura <- as.numeric(datos$cobertura)
#  3.Calcule la media para la variable respuesta y las variables explicativas para todo el estudio

mean(datos$conteo)#4.075
mean(datos$humedad)# 59.10838
mean(datos$cobertura)# 50.32709

#4.Calcule la moda para la variable respuesta y las explicativas para todo el estudio.
mfv(datos$conteo)
frecuencias <- data.frame(table(datos$conteo))
moda <- frecuencias[which.max(frecuencias$Freq),1]

mfv(datos$humedad)
frecuencias <- data.frame(table(datos$humedad))
moda <- frecuencias[which.max(frecuencias$Freq),1]#35.18562485

mfv(datos$cobertura)
frecuencias <- data.frame(table(datos$cobertura))
moda <- frecuencias[which.max(frecuencias$Freq),1]# 0.434438211


#5.Calcule los cuartiles para la variable respuesta y explicativas para todo el estudio.

quantile(datos$conteo, prob = c(0.25, 0.5, 0.75), na.rm = TRUE)
#25% 50% 75% 
#0   4   6
quantile(datos$humedad, prob= c(0.25,0.5,0.75), na.rm=TRUE)
#25%      50%      75% 
#45.15529 61.18146 71.91867
quantile(datos$cobertura, prob= c(0.25,0.5,0.75), na.rm=TRUE)
#25%      50%      75% 
#26.31862 51.45576 74.54465


#6.¿Hay diferencias entre los sitios? Haga un boxplot donde se visualicen las medidas de posición para la abundancia en cada sitio

# grafico boxplot para comparar los sitios
pl <- ggplot(datos,  aes(x=sitio, y=conteo), fill = sitio)
pl + geom_boxplot()

#no hay diferencias de abundancia entre sitios#


#7.¿Cuál es la variabilidad de la abundancia en cada sitio? ¿Es simétrica mi variable respuesta?

skew(datos$conteo)#0.3707457
kurtosi(datos$conteo)#-0.8170424

skew(datos$conteo)/sqrt(6/160) # 1.914523
kurtosi(datos$conteo)/sqrt(6/160)# -4.219189
#hacemos un solo ejemplo por sitio, pueden hacer ustedes los otros

f1 <- filter(datos, sitio=="a")
f1$conteo
skew(f1$conteo)/sqrt(6/40) #0.909924
kurtosi(f1$conteo)/sqrt(6/40)#-3.693302
#hay que estandarizar# se divide por la raiz cuadrada de 6 (constante en la formula) dividido la cantidad de datos dela variable que queremos evaluar.
skew(datos$conteo)/sqrt(6/160) #1.914523
kurtosi(datos$conteo)/sqrt(6/160)# -4.219189

#8.Piense en el tipo de datos que componen la variable respuesta. ¿Qué distribución de probabilidad cree que presentan?
 
#POISSON# 

# 9.Modele las distribuciones de probabilidad posibles para su variable respuesta y compare el resultado con su identificación previa. 
hist(datos$conteo)

require(fitdistrplus)
mean(datos[datos$conteo>0,]$conteo)#5.927273

windows()
par(mfrow=c(1,2), mai=c(1,1,0.5,0.2), cex.lab=1.5, lwd=2,cex.axis=1.3)
poisson=fitdist(datos$conteo,"pois")
negbinom=fitdist(datos$conteo,"nbinom")
cdfcomp(list(poisson,negbinom),horizontals=F,addlegend=T,
        legendtext=c("Poisson","NB"), main="")
qqcomp(list(poisson,negbinom),addlegend=T,
       legendtext=c("Poisson","NB"), main="")

gofstat(list(poisson,negbinom))$aic# si bien el AIC es mas bajo en la binomial negativa, en los graficos se ve mejor como una poisson 

#1-mle-pois 2-mle-nbinom 
#1016.8601     809.7602  

#10.Grafique la abundancia de los escarabajos en función de la humedad. 

#Abundancia en funcion de la humedad
r1<- ggplot(datos, aes(x= conteo, y = humedad, fill=sitio))+ geom_bar(stat="identity",  position = "dodge")+ xlab("Abundancia") + ylab ("Humedad") +theme (text = element_text(face= "bold" , size=15),panel.background = element_blank())

#11.Grafique la abundancia de los escarabajos en función de la cobertura vegetal.

#Abundancia en funcion de la cobertura vegetal
r2<- ggplot(datos, aes(x= conteo, y = cobertura, fill=sitio))+ geom_bar(stat="identity")+ xlab("Abundancia") + ylab ("cobertura (%)") +theme (text = element_text(face= "bold" , size=15),panel.background = element_blank())

#12.La humedad del suelo depende en cierta manera de la cobertura vegetal presente en un determinado sitio. Entonces, ¿Usted cree que son variables independientes? ¿Cómo corroboraría esto? 
#Correlacion entre humedad y cobertura#
library (corrplot)
cor.test(datos$humedad, datos$cobertura)#-0.1120269; p= 0.1584 no hay correlacion lineal 

# 13.Haga una tabla con los estadísticos descriptivos calculados hasta el momento. Puede hacerlo en Excel o con la función flextable del paquete “flextable”(Gohel, 2021).

#tabla resumen
datos%>%
  summarise(media=mean(datos$conteo),
            sd=sd(datos$conteo),
            asimetria=skew(datos$conteo)/sqrt(6/160),
            apuntamiento=kurtosi(datos$conteo)/sqrt(6/160),
            minimo=min(datos$conteo),
            mediana=median(datos$conteo),
            maximo=max(datos$conteo)) %>% 
  flextable() %>% 
  set_caption("Descripcion de la abundancia de escarabajos")

#-----------------------------------------------------------------------#
#                             EJERCICIO 2                               #
#-----------------------------------------------------------------------#
#Un amigo médico nos escribe un mail pidiéndonos ayuda con los análisis de datos de pacientes jóvenes que sufren fibrosis quística y nos envía la base de datos. Él quiere saber si la capacidad pulmonar en estos jóvenes varía con la edad o el sexo. Entonces, nuestra variable respuesta será “capacidad pulmonar” y las variables explicatorias “edad” y “sexo”. Note que la edad esta expresada en rangos y que el sexo esta expresado en 0,1, 2 siendo 0=hombre, 1= mujer, 2=intersex.

#OBJETIVO: Evaluar si existen cambios en la capacidad  pulmonar del paciente con fibrosis quistica de acuerdo a su edad y sexo.
ls()
rm(list=ls())
ls()

data<- read_delim("Ejercicio_2.csv", 
                   ";", escape_double = FALSE, trim_ws = TRUE)

View(data)
colnames(data)
summary(data)
data$cap_pulmonar <- as.numeric(data$cap_pulmonar)
data$edad <- as.factor(data$edad)
data$sexo <- as.factor(data$sexo)

#1.Identifique qué tipo de variable es “capacidad pulmonar” y evalue la distribución de probabilidad que presenta mediante un histograma.
hist(data$cap_pulmonar)

#variable cuantitativa continua: distribucion normal???
mean(data[data$cap_pulmonar>0,]$cap_pulmonar)#5.927273

windows()
par(mfrow=c(1,2), mai=c(1,1,0.5,0.2), cex.lab=1.5, lwd=2,cex.axis=1.3)
nor <- fitdist(data$cap_pulmonar,"norm")
poisson=fitdist(data$cap_pulmonar,"pois")
cdfcomp(list(nor, poisson),horizontals=F,addlegend=T,
        legendtext=c("Normal","Poisson"), main="")
qqcomp(list(nor,poisson),addlegend=T,
       legendtext=c("Normal","Poisson"), main="")
gofstat(list(nor,poisson))$aic
#1-mle-norm   2-mle-pois  
#213.1595     221.4053     


#2.¿Qué medida estadística puede identificar en el histograma? .
#medidas de posicion

#3.Calcule la media, mediana y moda de la capacidad pulmonar para cada franja etaria. 
data$edad# Levels: 7-9 10-12 13-15 16-18 19-21 
f1<- filter(data, edad == "7-9")
f2 <- filter(data, edad=="10-12")
f3 <- filter(data, edad=="13-15")
f4 <- filter(data, edad== "16-18")
f5 <- filter(data, edad=="19-21")
summary(f1$cap_pulmonar)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  104.0   107.5   121.0   121.8   131.5   147.0 
mfv(f1$cap_pulmonar) #104

summary(f2$cap_pulmonar)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  104     104     104     112     116     128 
mfv(f2$cap_pulmonar)#104

summary(f3$cap_pulmonar)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#90      95     108     110     121     136 
mfv(f3$cap_pulmonar)# 90  95 108 121 136

summary(f4$cap_pulmonar)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#104.0   104.0   104.0   112.4   120.0   130.0
mfv(f4$cap_pulmonar)#104

summary(f5$cap_pulmonar)
# 1st Qu.  Median    Mean 3rd Qu.    Max. 
#81.00   98.75  102.50  105.33  110.75  135.00 
mfv(f5$cap_pulmonar)# 81  98 101 104 113 135

#4.Calcule la media,mediana y moda de la capacidad pulmonar para cada sexo #meda,moda y mediana para cada sexo
f.0 <- filter(data, sexo=="0")
f.1 <- filter(data, sexo== "1")
f.2 <- filter(data, sexo=="2")
summary(f.0$cap_pulmonar)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#95.0   114.2   121.0   120.8   127.0   147.0
mfv(f.0$cap_pulmonar)#   95 113 118 124 128 147

summary(f.1$cap_pulmonar)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#98.0   104.0   106.0   114.1   124.2   136.0 
mfv(f.1$cap_pulmonar)# 104

summary(f.2$cap_pulmonar)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#81.0    97.0   104.0   102.4   104.0   130.0 
mfv(f.2$cap_pulmonar)#  104

#5.¿Qué tan variable es la “capacidad pulmonar” en nuestra población muestra? Mida el sesgo, curtosis y el coeficiente de variación. 
skew(data$cap_pulmonar)/sqrt(6/25) #0.2019598
kurtosi(data$cap_pulmonar)/sqrt(6/25)#-2.156792

#coeficiente de variacion de la capacidad pulmonar 
sd(data$cap_pulmonar)/mean(data$cap_pulmonar)*100#0.1440265;14.40265 tiene una dispersion del 14%, no tiene sesgo pero si una curtosis menor a -2 por lo que hay menor concentracion de data cercanos a la media...


#6.Grafique la capacidad pulmonar en función de la edad y en función del sexo. Puede elegir el gráfico que le resulte más visual o explicativo
r1<- ggplot(data, aes(x= edad, y = cap_pulmonar))+ geom_bar(stat="identity")+ xlab("edad") + ylab ("capacidad pulmonar") +theme (text = element_text(face= "bold" , size=15),panel.background = element_blank())+scale_y_continuous(limits = c(0,150))

r2<- ggplot(data, aes(x= sexo, y = cap_pulmonar))+ geom_bar(stat="identity")+ xlab("sexo") + ylab ("capacidad pulmonar") +theme (text = element_text(face= "bold" , size=15),panel.background = element_blank())+scale_y_continuous(limits = c(0,150))
#7.Concluya brevemente en función de los resultados.

#-----------------------------------------------------------------------#
#                             EJERCICIO 3                               #
#-----------------------------------------------------------------------#
#En el marco de la evaluación del estado de conservación de mamíferos de la provincia, nos designaron la especie Chironectes minimus mejor conocida como cuica de agua, una zarigüeya propia de la Selva Paranaense y que, hasta el momento, en nuestro país solo se encuentra en Misiones. 
#Sabemos que en otras regiones su peso es menor en áreas degradadas respecto de las áreas conservadas. Realizamos la captura de las zarigüeyas (con ayuda de un veterinario,  las medidas de bioseguridad adecuadas y los permisos correspondientes), donde registramos el peso y el sexo de los individuos en dos sitios completamente distintos: un área natural protegida y un cultivo de yerba con arroyos y tajamares.

#OBJETIVO: Evaluar de manera preliminar si las cuicas de agua sufren cambios en sus tamaños en función del ambiente en el que se encuentran.

cuicas<- read_delim("Ejercicio_3.csv", 
                   ";", escape_double = FALSE, trim_ws = TRUE)
View(cuicas)
colnames(cuicas)
cuicas$peso <- as.integer(cuicas$peso)
cuicas$lcorporal <- as.integer(cuicas$lcorporal)
cuicas$uso_suelo <- as.factor(cuicas$uso_suelo)
#1.Si la variable respuesta es el peso de las zarigüeyas ¿Qué tipo de variablees y cual sería la distribución de probabilidades? 
class(cuicas$peso)
cuicas$peso <- as.numeric(cuicas$peso)

#2.Grafica un histograma del peso de las zarigüeyas ¿Qué se observa?
hist(cuicas$peso)  
#3.Identifica la media y la moda del peso. 
mean(cuicas$peso)#607.8205
mfv(cuicas$peso)#586

#4.Grafica el peso de las zarigüeyas en función del uso del suelo.
class(cuicas$uso_suelo)
cuicas$uso_suelo <- as.factor(cuicas$uso_suelo)
cuicas$uso.bin <- as.factor(cuicas$uso.bin)
puso<- ggplot(cuicas, aes(x= uso_suelo, y = peso, fill=uso.bin))+ geom_boxplot() + xlab("uso del suelo") + ylab ("peso") +theme (text = element_text(face= "bold" , size=15),panel.background = element_blank())+scale_y_continuous(limits=c(200,800))
#5.¿Qué podes concluir sobre el grafico anterior?
  
# 6.Dado que solo muestreamos una sola vez por estación y en un solo año, ¿Crees que estos valores son concluyentes? Calcula el coeficiente de variación del peso para uso del suelo y concluye sobre esto.
sd(cuicas$peso)
CV<-function(x){  #D) Funcion que permite calcular el coeficiente de variaci?n
  y<-100*sd(x)/mean(x)
  return(y)
}


CV(cuicas$peso)#10.31989

U1<- filter(cuicas, uso_suelo== "cultivo")
var(U1$peso)#10.31989
sd(U1$peso)# 66.626
CV(U1$peso)  #11.28549
U2 <- filter(cuicas, uso_suelo=="reserva")
sd(U2$peso)#55.49346
CV(U2$peso)#8.893895

#7.Suponiendo que nuestros cuicas son robustos y que pueden servir para hacer inferencias, ¿Cuál crees que es la implicancia de estos resultados para la conservación de esta especie?
  
#  8.¿Qué otro análisis descriptivo podrías realizar? Pueden ser gráficos o el cálculo de otras medidas de posición/dispersión, o ambos.

length(cuicas$peso)
skew(cuicas$peso)/sqrt(6/39) #-0.287116
kurtosi(cuicas$peso)/sqrt(6/39)#-2.249918

#-----------------------------------------------------------------------#
#                             EJERCICIO 4                               #
#-----------------------------------------------------------------------#
#Nos salió una beca para hacer una pasantía en Malasia estudiando reservorios del virus Nipah, un patógeno transmitido por murciélagos frugívoros que viven cercanos a zonas de cria de cerdos para consumo humano. 
#Queremos identificar las especies que intervienen en el ciclo silvestre de este virus y conocer como es la seroprevalencia de dicho patógeno en dos áreas naturales.  
#Fuimos a campo, hicimos la captura de murciélagos, buscamos anticuerpos, y armamos la tabla. Ahora nos toca comenzar a analizar estos datos.

#Objetivo: Evaluar la seroprevalencia del virus Nipah en poblaciones de murciélagos en función del sitio, estación del año y de la riqueza y diversidad del ensamble. 

murcis<- read_delim("Ejercicio_4.csv", 
                   ";", escape_double = FALSE, trim_ws = TRUE)
View(murcis)
colnames(murcis)

#a)	Si mi variable respuesta seria la seroprevalencia del virus en el área natural protegida, ¿qué tipo de variable es? ?Cómo mediría usted la simetría de esta variable? ?A qué distribución corresponde?
class(murcis$prev_nipah)
murcis$prev_nipah <- as.numeric(murcis$prev_nipah)
length(murcis$prev_nipah)#16
skew(murcis$prev_nipah)/sqrt(6/16) #0.3724836
kurtosi(murcis$prev_nipah)/sqrt(6/16)#-2.113057
hist(murcis$prev_nipah)
lines(density(murcis$prev_nipah))

#  b)	Charlando con un colega le pregunta si la seroprevalencia del virus está relacionada con la riqueza y diversidad del ensamble, ?Cómo podría usted, de manera exploratoria, visualizar si existe esta relación? 
r1<- ggplot(murcis, aes(x= prev_nipah, y = riqueza))+ xlab("seroprevalencia") + ylab ("Riqueza") +theme (text = element_text(face= "bold" , size=15))+geom_line()+geom_point()+geom_smooth()
colnames(murcis)

r2<- ggplot(murcis, aes(x=prev_nipah , y =indShannon ))+ xlab("diversidad") + ylab ("seroprevalencia") +theme (text = element_text(face= "bold" , size=15))+geom_line()+geom_point()+geom_smooth()

#  c)	Dado que la diversidad está relacionada a la riqueza, ?qué análisis podría hacer para evaluar estadísticamente la relación entre estas variables?
cor.test(murcis$riqueza, murcis$indShannon, method = "pearson")#0.0002722 cor= 0.789
library(GGally)
windows()
ggpairs(murcis[,c(17:18)])+theme_bw()

#  d)	?Cu?l es la seroprevalencia media del virus en murci?lagos? ?Qu? tan variable es esa seroprevalencia?
mean(murcis$prev_nipah)#0.1436198

sd(murcis$prev_nipah)#0.09341324
CV<-function(x){  #D) Funcion que permite calcular el coeficiente de variaci?n
  y<-100*sd(x)/mean(x)
  return(y)
}


CV(murcis$prev_nipah)#65.04205

#  e)	?Qué sucede con la seroprevalencia media del virus entre estaciones del año? ?Y entre áreas naturales protegidas? 
colnames(murcis)
murcis$uso_suelo <- as.factor(murcis$uso_suelo)
murcis$estacion <- as.factor(murcis$estacion)
f1<- ggplot(murcis, aes(x=estacion , y = prev_nipah, fill=uso_suelo))+ geom_boxplot() + xlab("estacion del año") + ylab ("seroprevalencia") +theme (text = element_text(face= "bold" , size=15))

# f)	En base a todos estos análisis descriptivos, ?qué podría usted concluir? 
##graficos## 
murcis$uso_suelo <- as.factor(murcis$uso_suelo)

ggplot(murcis, aes(x=uso_suelo, y=prev_nipah)) + 
  geom_bar(position="dodge", stat = "identity")


