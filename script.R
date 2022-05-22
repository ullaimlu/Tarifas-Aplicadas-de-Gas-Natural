setwd("C:/Tarifas_aplicadas_de_Gas_Natural")



library(readr)
library(dplyr)
library(ggplot2)
library(scales)
library(agricolae)


data <- read_delim("Tarifas_aplicadas_de_Gas_Natural.csv",
                 delim = ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), 
                 trim_ws = TRUE);
View(data)
data2 = data %>% 
  
  select(ANIO, EMPRESA, ESTRATO, RANGO_0, RANGO_21)
View(data2)



#muestras
muestraestrato1 = data2 %>%
  filter (ESTRATO >=1, ESTRATO<=3)


View(muestraestrato1)

muestraestrato2 = data2 %>%
  filter (ESTRATO >=4, ESTRATO<=6)

View(muestraestrato2)



n<-300
muestra1<- muestraestrato1 %>%
  sample_n(size=n,replace=FALSE)

View(muestra1)

muestra2<- muestraestrato2 %>%
  sample_n(size=n,replace=FALSE)

View(muestra2)

# Graficas
# Histograma por anio
AÑO<- muestra1$ANIO
hist(AÑO, col=20)

AÑO <- muestra2$ANIO
hist(AÑO, col=30)


# Diagrama de Torta por Estrato
x1 = summary(as.factor(muestra1$ESTRATO))
x1
labels1 = round(100 * x1/ sum(x1), 2)
leg1 = names(x1)
pie(x1, labels=labels1, radius=1, main='Distribucion Por Estratos', col=rainbow(3))
legend("topright", leg1, cex = 0.8, fill = rainbow(3))

x1 = summary(as.factor(muestra2$ESTRATO))
x1
labels1 = round(100 * x1/ sum(x1), 2)
leg1 = names(x1)
pie(x1, labels=labels1, radius=1, main='Distribucion Por Estratos', col=rainbow(4))
legend("topright", leg1, cex = 0.8, fill = rainbow(6))

#grafico de lineas

rango_0<- muestra1$RANGO_0
empresa<- muestra1$EMPRESA
ggplot(muestra1 = mpg) + 
  geom_point(mapping = aes(x = rango_0,y=empresa), color = "purple")

RANGO0<- muestra2$RANGO_0
ggplot(muestra2 = mpg) + 
  geom_freqpoly(mapping = aes(x =RANGO0 ), color = "blue")
RANGO0<- muestra1$RANGO_0
ggplot(muestra1 = mpg) + 
  geom_freqpoly(mapping = aes(x = RANGO0), color = "purple")

RANGO0<- muestra2$RANGO_0
ggplot(muestra2 = mpg) + 
  geom_freqpoly(mapping = aes(x =RANGO0 ), color = "blue")

#grafico de puntos

RANGO21<- muestra1$RANGO_21
ggplot(muestra1 = mpg) + 
  geom_dotplot(mapping = aes(x = RANGO21), fill = "orange", color="orange")

RANGO21<- muestra2$RANGO_21
ggplot(muestra2 = mpg) + 
  geom_dotplot(mapping = aes(x =RANGO21 ), fill = "green",color="green" )
#Diagrama de Caja de bigotes

d = muestra1 %>% 
  
  select( RANGO_0, RANGO_21)
View(d)
d1= d %>%
  filter (RANGO_21 <=5000, RANGO_0<=5000)

boxplot(d1)

d = muestra2 %>% 
  
  select( RANGO_0, RANGO_21)
View(d)
d1= d %>%
  filter (RANGO_21 <=5000, RANGO_0<=5000)

boxplot(d1)


#TABLA DE FRECUENCIAS 
#VARIABLES CUALITATIVAS
RELEVANCIA_EMPRESA=muestra1 %>%
  group_by(EMPRESA)%>%
  summarise(Cantidad=n()) %>%
  mutate(Porcentaje=round(Cantidad/sum(Cantidad)*100,2))
View(RELEVANCIA_EMPRESA)

RELEVANCIA_EMPRESA=muestra2 %>%
  group_by(EMPRESA)%>%
  summarise(Cantidad=n()) %>%
  mutate(Porcentaje=round(Cantidad/sum(Cantidad)*100,2))
View(RELEVANCIA_EMPRESA)

RELEVANCIA_ESTRATO=muestra1 %>%
  group_by(ESTRATO)%>%
  summarise(Cantidad=n()) %>%
  mutate(Porcentaje=round(Cantidad/sum(Cantidad)*100,2))
View(RELEVANCIA_ESTRATO)

RELEVANCIA_ESTRATO=muestra2 %>%
  group_by(ESTRATO)%>%
  summarise(Cantidad=n()) %>%
  mutate(Porcentaje=round(Cantidad/sum(Cantidad)*100,2))
View(RELEVANCIA_ESTRATO)

#VARIABLES CUANTITATIVAS
RELEVANCIA_ANIO=muestra1 %>%
  group_by(ANIO)%>%
  summarise(Cantidad=n()) %>%
  mutate(Porcentaje=round(Cantidad/sum(Cantidad)*100,2))
View(RELEVANCIA_ANIO)

RELEVANCIA_ANIO=muestra2 %>%
  group_by(ANIO)%>%
  summarise(Cantidad=n()) %>%
  mutate(Porcentaje=round(Cantidad/sum(Cantidad)*100,2))
View(RELEVANCIA_ANIO)


RELEVANCIA_RANGO0<- as.data.frame(table(RANGO0 = factor(cut(muestra1$RANGO_0, breaks = 9))))
View(RELEVANCIA_RANGO0)

RELEVANCIA_RANGO0<- as.data.frame(table(RANGO0 = factor(cut(muestra2$RANGO_0, breaks = 9))))
View(RELEVANCIA_RANGO0)

RELEVANCIA_RANGO21<- as.data.frame(table(RANGO21 = factor(cut(muestra1$RANGO_21, breaks = 9))))
View(RELEVANCIA_RANGO21)

RELEVANCIA_RANGO21<- as.data.frame(table(RANGO21 = factor(cut(muestra2$RANGO_21, breaks = 9))))
View(RELEVANCIA_RANGO21)



# Resumen (media, mediana y quantiles para cada variable cuantitativa)
# Medidas de tendencia central
summary(muestra1 %>% select(ANIO, RANGO_0, RANGO_21))

t_anio = table(muestra1$ANIO); t_anio
moda_anio = which.max(t_anio); moda_anio

t_rango0 = table(muestra1$RANGO_0);t_rango0
moda_rango0 = which.max(t_rango0);moda_rango0

t_rango21 = table(muestra1$RANGO_21);t_rango21
moda_rango21 = which.max(t_rango21);moda_rango21

summary(muestra2 %>% select(ANIO, RANGO_0, RANGO_21))

t_anio = table(muestra2$ANIO); t_anio
moda_anio = which.max(t_anio); moda_anio

t_rango0 = table(muestra2$RANGO_0);t_rango0
moda_rango0 = which.max(t_rango0);moda_rango0

t_rango21 = table(muestra2$RANGO_21);t_rango21
moda_rango21 = which.max(t_rango21);moda_rango21

# Medidas de dispersión
#muestra 1
# ANIO 

R=max(muestra1$ANIO)-min(muestra1$ANIO);R #Rango
S=sd(muestra1$ANIO);S  #Desviación estandar
V=var(muestra1$ANIO);V #Varianza
cv=S/mean(muestra1$ANIO)*100;cv  #coeficiente de variación


#sesgo
skewness(muestra1$ANIO)

#curtosis
kurtosis(muestra1$ANIO)

# RANGO_0

R=max(muestra1$RANGO_0)-min(muestra1$RANGO_0);R #Rango
S=sd(muestra1$RANGO_0);S  #Desviación estandar
V=var(muestra1$RANGO_0);V #Varianza
cv=S/mean(muestra1$RANGO_0)*100;cv  #coeficiente de variación


#sesgo
skewness(muestra1$RANGO_0)

#curtosis
kurtosis(muestra1$RANGO_0)

# RANGO_21

R=max(muestra1$RANGO_21)-min(muestra1$RANGO_21);R #Rango
S=sd(muestra1$RANGO_21);S  #Desviación estandar
V=var(muestra1$RANGO_21);V #Varianza
cv=S/mean(muestra1$RANGO_21)*100;cv  #coeficiente de variación


#sesgo
skewness(muestra1$RANGO_21)

#curtosis
kurtosis(muestra1$RANGO_21)


#muestra 2
# ANIO 

R=max(muestra2$ANIO)-min(muestra2$ANIO);R #Rango
S=sd(muestra2$ANIO);S  #Desviación estandar
V=var(muestra2$ANIO);V #Varianza
cv=S/mean(muestra2$ANIO)*100;cv  #coeficiente de variación


#sesgo
skewness(muestra2$ANIO)

#curtosis
kurtosis(muestra2$ANIO)

# RANGO_0

R=max(muestra2$RANGO_0)-min(muestra2$RANGO_0);R #Rango
S=sd(muestra2$RANGO_0);S  #Desviación estandar
V=var(muestra2$RANGO_0);V #Varianza
cv=S/mean(muestra2$RANGO_0)*100;cv  #coeficiente de variación


#sesgo
skewness(muestra2$RANGO_0)

#curtosis
kurtosis(muestra2$RANGO_0)

# RANGO_21

R=max(muestra2$RANGO_21)-min(muestra2$RANGO_21);R #Rango
S=sd(muestra2$RANGO_21);S  #Desviación estandar
V=var(muestra2$RANGO_21);V #Varianza
cv=S/mean(muestra2$RANGO_21)*100;cv  #coeficiente de variación


#sesgo
skewness(muestra2$RANGO_21)

#curtosis
kurtosis(muestra2$RANGO_21)




#Prueba de hipótesis para la diferencia de medias, muestras grandes
#varianza conocida
#anio
n1=300
n2=300
prom1=2015
prom2=2016
var1=18.28819
var2=15.82139
se=sqrt(var1/n1+var2/n2)
dif=prom1-prom2
zc=(dif-0)/se;zc
z=qnorm(0.025);z
pvalor=pnorm(zc)+pnorm(-zc,lower.tail = FALSE);pvalor




#PH para el cociente de varianzas
s1_2=18.28819
s2_2=15.82139
n1=300
n2=300
Fc=s1_2/s2_2;Fc
F1=qf(0.025,n1-1,n2-1);F1
F2=qf(0.025,n1-1,n2-1,lower.tail = FALSE);F2

#rango_0
n1=300
n2=300
prom1=1638074
prom2=1329214
var1=8.279366e+13
var2=2.977672e+13
se=sqrt(var1/n1+var2/n2)
dif=prom1-prom2
zc=(dif-0)/se;zc
z=qnorm(0.025);z
pvalor=pnorm(zc)+pnorm(-zc,lower.tail = FALSE);pvalor




#PH para el cociente de varianzas
s1_2=8.279366e+13
s2_2=2.977672e+13
n1=300
n2=300
Fc=s1_2/s2_2;Fc
F1=qf(0.025,n1-1,n2-1);F1
F2=qf(0.025,n1-1,n2-1,lower.tail = FALSE);F2

#rango_21
n1=300
n2=300
prom1=650474
prom2=994669
var1=9.917995e+12
var2=1.479715e+13
se=sqrt(var1/n1+var2/n2)
dif=prom1-prom2
zc=(dif-0)/se;zc
z=qnorm(0.025);z
pvalor=pnorm(zc)+pnorm(-zc,lower.tail = FALSE);pvalor




#PH para el cociente de varianzas
s1_2=9.917995e+12
s2_2=1.479715e+13
n1=300
n2=300
Fc=s1_2/s2_2;Fc
F1=qf(0.025,n1-1,n2-1);F1
F2=qf(0.025,n1-1,n2-1,lower.tail = FALSE);F2

