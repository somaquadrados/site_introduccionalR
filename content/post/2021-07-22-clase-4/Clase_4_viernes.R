#-----------------------------------------------------------------------#
#                             EJERCICIO 4                               #
#-----------------------------------------------------------------------#
#Nos salió una beca para hacer una pasantía en Malasia estudiando reservorios del virus Nipah, un patógeno transmitido por murciélagos frugívoros que viven cercanos a zonas de cria de cerdos para consumo humano. 
#Queremos identificar las especies que intervienen en el ciclo silvestre de este virus y conocer como es la seroprevalencia de dicho patógeno en dos áreas naturales.  
#Fuimos a campo, hicimos la captura de murciélagos, buscamos anticuerpos, y armamos la tabla. Ahora nos toca comenzar a analizar estos datos.

#Objetivo: Evaluar la seroprevalencia del virus Nipah en poblaciones de murciélagos en función del sitio, estación del año y de la riqueza y diversidad del ensamble. 
ls()
rm(list=ls())
ls()

library(readr)#leer el archivo
library(ggplot2)#graficos
murcis<- read_delim("Ejercicio_4.csv", 
                    ";", escape_double = FALSE, trim_ws = TRUE)

murcis$prev_nipah <- as.numeric(murcis$prev_nipah)

#1 Grafique un histograma de frecuencias de la seroprevalencia del virus y su línea de densidad

hist(murcis$prev_nipah, main="Histrograma", ylab="Frecuencia", xlab="Seroprevalencia")
lines(density(murcis$prev_nipah), lwd=2, col="red")

ggplot(murcis, aes(x=prev_nipah, fill="blue", col="black"))+
  geom_histogram(fill="blue", col="black")+
  geom_density(col="red")+
  xlab("seroprevalencia")+
  ylab("frecuencia")+
  ggtitle("Frecuencia de la seroprevalencia de virus Nipah")

ggplot(murcis, aes(x=prev_nipah,fill=uso_suelo))+
  geom_histogram()+
  xlab("seroprevalencia")+
  ylab("frecuencia")+
  ggtitle("Frecuencia de la seroprevalencia de virus Nipah")+
  geom_density(fill = "black", alpha = 0.2)+
  theme (legend.position = "none", plot.title=element_text(hjust=0.5, vjust=0.5,family='', face='bold', colour='red', size=8))

# 2 Realice un grafico de barras de la seroprevalencia en función de las áreas de estudio
#con geom_col
ggplot(murcis, aes(x=uso_suelo, y=prev_nipah))+
  geom_col(stat="identity", position = "dodge", width = 0.5, fill="coral2")+
  xlab("tipo de ambiente")+
  ylab("prevalencia")+
  ggtitle("Prevalencia del virus Nipah en c/ambiente")


#con geom_bar
ggplot(murcis, aes(x=uso_suelo, y=prev_nipah))+
  geom_bar(stat="identity", position="dodge", fill= "coral4", width = 0.2)+
  xlab("tipo de ambiente")+
  ylab("prevalencia")+
  ggtitle("Prevalencia del virus Nipah en c/ambiente")


#3 Realice un gráfico de violin de la seroprevalencia en función de las áreas de estudio y compare con el grafico de barras

ggplot(murcis, aes(x=uso_suelo, y=prev_nipah))+
  geom_violin(aes(fill=uso_suelo))+
  scale_fill_manual(values=c("green2", "green4"))+
  xlab("tipo de ambiente")+
  ylab("prevalencia")+
  ggtitle("Prevalencia del virus Nipah en c/ambiente")+
  theme(legend.position = "none")

#4Al gráfico de violin realizado anteriormente, agréguele la media con el comando stat_summary. Vea el Help para programar las especificaciones de color y linea.
r1 <- ggplot(murcis, aes(x=uso_suelo, y=prev_nipah))+
  geom_violin(aes(fill=uso_suelo))+
  stat_summary(fun=mean, geom= "crossbar", width=0.7, colour="black")+
  scale_fill_manual(values=c("green2", "blue"))+
  xlab("tipo de ambiente")+
  ylab("prevalencia")+
  theme(legend.position = "none")

# 5.Realice un boxplot de la prevalencia por área de estudio. Compare los tres gráficos realizados hasta el momento. ¿Cual elegiria para reportar en un informe y por qué?
r2 <- ggplot(murcis, aes(x=uso_suelo, y=prev_nipah))+
  geom_boxplot(aes(fill=uso_suelo), width =3)+
  scale_fill_manual(values=c("cyan", "darkolivegreen1"))+theme(legend.position = "none")

  r2+geom_jitter(aes(x=uso_suelo, y=prev_nipah), size=2, shape=murcis$uso_suelo)
   
 r3 <-  r2+geom_jitter(aes(x=uso_suelo, y=prev_nipah, color=uso_suelo), size=2, )+
    scale_color_manual(values=c("blue", "green"))
  
r2+geom_jitter(aes(x=uso_suelo, y=prev_nipah))#negro

r2+geom_jitter(aes(x=uso_suelo, y=prev_nipah), shape=murcis$uso_suelo)


library(cowplot)
windows()
par(mfrow = c(2,1), mar = c(5,5,2,2))
plot_grid(r1,r3,nrow=1)

#6.Grafique el número de individuos positivos, en función de los analizados con un grafico de líneas.
murcis$ind_analizados <- as.integer(murcis$ind_analizados)
murcis$ind_positivos <- as.integer(murcis$ind_positivos)

ggplot(murcis)+geom_line(aes(x=ind_analizados, y=ind_positivos), colour="red")
murcis$anio <- as.factor(murcis$anio)
ggplot(murcis, aes(x=uso_suelo, y= ind_positivos))+ 
  geom_bar(stat="identity")+
  geom_line(aes(y=ind_analizados, group=))+scale_y_continuous(sec.axis = sec_axis(~., name = "ind_analizados"))
  

#7.Sabemos que las especies P. vampyrus y P. hypomelanus son las únicas especies donde detectamos anticuerpos contra el virus Nipah. Grafique la seroprevalencia del  virus en función de cada una de las especies. Utilice el gráfico que le resultes más claro y sencillo de interpretar. Si quisiera hacer un grafico de red, recuerde cómo debe ser la estructura de los datos.         

ggplot(murcis) + 
  geom_bar(aes(x=uso_suelo, y=ind_positivos),stat = "identity", position="dodge")+
  ggtitle("Prev de Nipah Pvamp y Phypo")
pos_Pvam <- c(20,3, 15,5,4,0,10,0,3,11,5,9,2,9,7,2)
pos_Phypom <- c(5,4,12,5,15,0,7,2,5,12,10,10,5,5,4,10)
murcis <- cbind(murcis, pos_Pvam,pos_Phypom)

r1 <- ggplot(murcis) + 
  geom_bar(aes(x=estacion, y=pos_Phypom, fill=estacion),stat = "identity", position="dodge")
r1 <- r1+geom_jitter(aes(x=estacion, y=Phypomelanus,colour=estacion), group = 1)+scale_color_manual(values=c("red", "darkgreen", "darkblue", "darkorchid4"))



r2 <- ggplot(murcis) + 
  geom_bar(aes(x=estacion, y=pos_Pvam, fill=estacion),stat = "identity", position="dodge")+geom_jitter(aes(x=estacion, y=Pvampyrus,colour=estacion), group = 1)+scale_color_manual(values=c("red", "darkgreen", "darkblue", "darkorchid4"))

windows()
par(mfrow = c(2,1), mar = c(5,5,2,2))
plot_grid(r1,r2,nrow=1)
a <- ggplot(murcis) +geom_col(aes(x=estacion, y=Phypomelanus), stat="identity", position="dodge", width=0.5, alpha=0.5)


a <- a+geom_bar(aes(x=estacion, y=pos_Phypom, fill=estacion), stat = "identity", position="dodge", width=0.5)+theme(legend.position = "none")
#vean que si le agrego un alfa al primer grafico, me muestra un degrade de colores para cada observacion de la abundancia de P hypomelanus en cada estacion. 
a <- ggplot(murcis) +geom_col(aes(x=estacion, y=Phypomelanus), stat="identity", position="dodge", width=0.5)

a <- a+geom_bar(aes(x=estacion, y=pos_Phypom, fill=estacion), stat = "identity", position="dodge", width=0.5)+theme(legend.position = "none")+ylab("Num de individuos de P. hypomelanus")+xlab("Estación del año")

#hacemos el grafico para la otra especie

b <- ggplot(murcis) +geom_col(aes(x=estacion, y=Pvampyrus), stat="identity", position="dodge", width=0.5)


b <- b+geom_bar(aes(x=estacion, y=pos_Pvam, fill=estacion), stat = "identity", position="dodge", width=0.5)+theme(legend.position = "none")+ylab("Num de individuos de P. vampyrus")+xlab("Estación del año")


windows()
par(mfrow = c(2,1), mar = c(5,5,2,2))
plot_grid(a,b,nrow=1)


#****************************************************************************#
#                                     Dudas                                  #
#****************************************************************************#
#*1- Quisiera saber, cuando seteo que coloree diferente en función de, por ej. sexos, que te pone en rosa y celeste. Cómo puedo cambiar esos colores? 
#*
#*usando el comando scale_fill_manual o scale_color_manual puedo cambiarle los colores al grafico. Tengo que especificar colores para cada nivel de la variable que voy a expresar. 
ggplot(murcis) + 
  geom_bar(aes(x=uso_suelo, y=pos_Phypom, fill=uso_suelo),stat = "identity", position="dodge")+scale_fill_manual(values = c("greenyellow", "green3"))


#2- En algunos casos no quería que se vea la referencia de los colores (lo que digo arriba) porque justo era la misma variable que uno de los ejes. Cómo saco esa etiqueta? 
#con el comando "theme" puedo programar el formato de varios elementos del grafico, como la fuente de los titulos, tamaño, color, y especificar cómo quiero la leyenda de mi grafico o si no la quiero. 

#https://ggplot2.tidyverse.org/reference/ggtheme.html en este link pueden ver algunos temas existentes, pero tambien pueden programar el propio y usarlo para cada grafico que quieran reportar
ggplot(murcis) + 
  geom_bar(aes(x=uso_suelo, y=pos_Phypom, fill=uso_suelo),stat = "identity", position="dodge")+scale_fill_manual(values = c("greenyellow", "green3"))+theme(legend.position = "none")


#  3- En un boxplot, cuando agrego los puntos (valores), logré cambiarlos de color, tamaño y transparencia, pero son todos iguales. Quisiera poder hacer que también se diferencien, sea en color o forma, entre las variables del eje (cada caja de un color y los puntos correspondientes a cada una también diferenciados). Quise usar la misma lógica que con las cajas usando fill o color= "variable" pero no funcionó.

r1 <- ggplot(murcis, aes(x=uso_suelo, y=prev_nipah))+geom_boxplot(aes(fill=uso_suelo))+xlab("Sitios de estudio")+ylab("Seroprevalencia del virus Nipah")+ggtitle("Seroprevalencia del virus Nipah en murcielagos")+theme( plot.title=element_text(hjust=0.5, vjust=0.5,family='', face='bold', colour='black', size=12))

r1+geom_jitter(aes(x=uso_suelo, y=prev_nipah, colour=uso_suelo), size = 2,alpha=1, width = 0.1)+scale_color_manual(values=c("red", "darkblue"))

ggplot(murcis, aes(x=uso_suelo, y=prev_nipah))+geom_boxplot(aes(fill=uso_suelo))+scale_fill_manual(values = c("greenyellow", "green3"))+geom_jitter(aes(x=uso_suelo, y=prev_nipah, shape=uso_suelo), size = 2,alpha=0.5, width = 0.1)+xlab("Sitios de estudio")+ylab("Seroprevalencia del virus Nipah")+ggtitle("Seroprevalencia del virus Nipah en murcielagos")+  theme( plot.title=element_text(hjust=0.5, vjust=0.5,family='', face='bold', colour='black', size=12), legend.position = "none")

