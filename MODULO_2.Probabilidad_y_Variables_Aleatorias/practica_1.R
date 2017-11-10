#---
#title: "Practica_1"
#author: "Tanausú Suárez"
#date: "26 de septiembre de 2017"
#output: word_document
#--

#Ejercicio: Analizar con el comando search() los paquetes presentes en el entorno de trabajo. Con library(help=package), seleccionar el paquete datasets, y, dentro de los distintos conjuntos de datos, visualizar en la consola los contenidos de varios de ellos. Analizar como están estructurados los datos para familiarizarse con ellos.Alguno de estos pueden ser utilizados como parte experimental del proyecto o trabajo de curso**

#Los paquetes instalados en mi entorno de trabajo son los siguientes:
search()

#Como ya tenemos instalado el paquetes "datasets" no hará falta instalarlo. Simplement lo seleccionamos.
#help("datasets")
library(datasets)
#Al ejecutar esta sentencia simplemente nos muestra las caracteristicas del paquete.

#**Ejercicio 2: Analizar el contenido del Data Set "iris" de R con los comandos tail() y head() y listar las filas 10 a 15.**

#Con la instrucción data podemos ver el contenido de las observaciones de la variable iris
data("iris")

#Los comandos tail() y head() listan las 6 últimas observaciones y las 6 primeras, respectivamente.
#Como nos pide obtener los valores de la fila 10 hasta la 15 (son 6 valores), le indico a la función tail que quiero dicho valores desde la fila 15 hacia atrás.
datos_iris = tail(head(iris,15))

#a. Razonar sobre los tipos de datos que lo integran (factores y vectores).**

#Primero podemos observar que la variable iris pertenece a los objetos en R denominados listas
typeof(datos_iris)

#Donde podemos observar el nombre de las variables:
names(datos_iris)

#Ahora podemos acceder a los campos de la variable iris y ver de que tipo son cada uno. En este caso son todas de tipo numéricos.

mode(datos_iris$Sepal.Length)
mode(datos_iris$Sepal.Width)
mode(datos_iris$Petal.Length)
mode(datos_iris$Petal.Width)
mode(datos_iris$Species)
class(datos_iris$Species)

#En este caso el única que se distinge del resto es la variable Species que corresponde con la clase factor.



#b.Cambiar los nombres de los vectores y factores para pasarlos al castellano.**

names(datos_iris) <- c("largo.sepalo","ancho.sepalo","largo.petalo","ancho.petalo","especie")
names(datos_iris)
attach(datos_iris)



#c.Encontrar las medias, medianas y desviaciones estándar de las longitudes y ancho de pétalos por especie.**

#Para el cálculo de la media de la longitud de los pétados:

tapply(iris$Petal.Length,iris$Species,mean)


#Para el cálculo de la media de la anchura de los pétalos

tapply(iris$Petal.Width,iris$Species,mean)


#Para el cálculo de la desviación tipica sabemos que es la raíz cuadrada de la varianza por tanto:

desv = function(x){
  sqrt(var(x))
}


S#olo nos queda proceder como anteriormente:

#Cálculo de la desviación típica de la longitud de los pétalos.

tapply(iris$Petal.Length,iris$Species,desv)


#Cálculo de la desviación típica de la anchura de los pétalo

tapply(iris$Petal.Width,iris$Species,desv)






#d. Visualizar utilizando boxplot() las variaciones de longitud de sépalos y pétalos por especie.**

#Logintud del sépalo por especie

boxplot(iris$Sepal.Length~iris$Species,xlab="Especies",ylab="longitud", col=2)


#Logintud del pétalo por especie

boxplot(iris$Petal.Width~iris$Species,xlab="Especies",ylab="Anchura", col=3)





#e. Obtener un vector de longitudes de sépalo de la especie "virginica" utilizando operadores lógicos**


c(iris$Sepal.Length[0:150])
detach(datos_iris)







#Ejercicio 3: Utilizar la siguiente secuencia de comandos para leer el fichero "daphnia.txt".**

datos<-read.table("daphnia.txt",header = TRUE)
attach(datos)
names(datos)





#a) Analizar el contenido del Data Frame "datos" con los comandos tail() y head()**

tail(Growth.rate)
tail(Water)
tail(Detergent)
tail(Daphnia)
head(Growth.rate)
head(Water)
head(Detergent)
head(Daphnia)

#Podemos observar como nos encontramos frente a tres variables categóricas y una variable numerica




#b) Razonar sobre los tipos de datos que lo integran (factores y vectores).**


class(Growth.rate)
class(Water)
class(Detergent)
class(Daphnia)

#En primer lugar nos encontramos con "Growth.rate" que se trata de un vector numérico. Sin embargo el resto de variables categóricas se trata de objetos en R denominados factores.




#c) Encontrar las medias, medianas y desviaciones estándar de la variable "Growth.rate" por cada variable categórica utilizando tapply() y agregate().**

#Utilizando tapply();

#Cálculo de la media

tapply(Growth.rate,Water,mean)
tapply(Growth.rate,Detergent,mean)
tapply(Growth.rate,Daphnia,mean)


#Cálculo de la mediana

tapply(Growth.rate,Water,median)
tapply(Growth.rate,Detergent,median)
tapply(Growth.rate,Daphnia,median)


#Cálculo de la desviación típica (o desviación estandar)

desv = function(x){
  sqrt(var(x))
}

tapply(Growth.rate,Water,desv)
tapply(Growth.rate,Detergent,desv)
tapply(Growth.rate,Daphnia,desv)




#Utilizando aggregate();

#Cálculo de la media

aggregate(Growth.rate~Water,datos,mean)
aggregate(Growth.rate~Detergent,datos,mean)
aggregate(Growth.rate~Daphnia,datos,mean)

 

#Cálculo de la mediana

aggregate(Growth.rate~Detergent,datos,median)
aggregate(Growth.rate~Detergent,datos,median)
aggregate(Growth.rate~Daphnia,datos,median)


#Cálculo de la desviación típica
```{r}
desv = function(x){
  sqrt(var(x))
}
aggregate(Growth.rate~Water,datos,desv)
aggregate(Growth.rate~Detergent,datos,desv)
aggregate(Growth.rate~Daphnia,datos,desv)






#d) Visualizar utilizando boxplot() las variaciones de "Growth.rate" dependiendo del tipo de agua, detergente y clon.**

#Con agua

boxplot(Growth.rate~Water,xlab="Especies",ylab="Crecimiento", col=2)


#Con detegente

boxplot(Growth.rate~Detergent,xlab="Especies",ylab="Crecimiento", col=3)


#con Clon

boxplot(Growth.rate~Daphnia,xlab="Especies",ylab="Crecimiento", col=4)





#Ejercicio 4: Utilizar el Data Set "HairEyeColor" de R, que proporciona los datos de frecuencia en una muestra de hombres y mujeres de los colores del cabello (Black, Brown, Red y Blond) y del color de los ojos (Brown, Blue, Hazel y Green), utilizados como ejemplo para visualizar tablas de contingencia. Analizar la estructura de los datos correspondientes y convertirla en un data frame. Encontrar:**

data("HairEyeColor") 
datos <- as.data.frame(HairEyeColor)
names(datos)
attach(datos)





#a) La proporción de hombres y mujeres en la muestra total**

conjuntoHombres <- subset(datos,Sex=="Male")
sumaHombres <- sum(conjuntoHombres$Freq)

conjuntoMujeres <- subset(datos,Sex=="Female")
sumaMujeres <- sum(conjuntoMujeres$Freq)

porcHombre <- (sumaHombres/sum(Freq))*100
porcMuje <- (sumaMujeres/sum(Freq))*100

porcHombre
porcMuje
(porcHombre + porcMuje)/100





#b) La proporción de hombres con ojos azules**
#NOTA: HA = Hombre con los ojos Azules.

conjHA <-subset(datos,(Sex=="Male")&(Eye=="Blue"))
sumaHA <-sum(conjHA$Freq)
porHA <- (sumaHA/sum(Freq))*100
porHA





#c) La proporción de mujeres morenas con ojos azules.**
#NOTA: MA = Mujeres morenas ojos azules.

conjMA <- subset(datos,(Sex=="Female")&(Hair=="Black")&(Eye=="Blue"))
class(conjMA)
sumaMA <- sum(conjMA$Freq)
porMA <- (sumaMA/sum(Freq))*100
porMA





#d) Examinar la función de visualización "pie()" y dibujar con ella un diagrama donde se muestre la distribución de los colores de cabello en dos gráficas de mujeres y hombres**

color <- c("Negro","Marrón","Rojo","Rubio")

conjMN <- subset(datos,(Hair=="Black")&(Sex=="Male"))
conjMM <- subset(datos,(Hair=="Brown")&(Sex=="Male"))
conjMR <- subset(datos,(Hair=="Red")&(Sex=="Male"))
conjMB <- subset(datos,(Hair=="Blond")&(Sex=="Male"))

sumMN <- sum(conjMN$Freq)
sumMM <- sum(conjMM$Freq)
sumMR <- sum(conjMR$Freq)
sumMB <- sum(conjMB$Freq)

pror <- c(sumMN,sumMM,sumMR,sumMB)
pie(pror,color,main = "Proporción de color de pelos Hombres")

conjMN <- subset(datos,(Hair=="Black")&(Sex=="Female"))
conjMM <- subset(datos,(Hair=="Brown")&(Sex=="Female"))
conjMR <- subset(datos,(Hair=="Red")&(Sex=="Female"))
conjMB <- subset(datos,(Hair=="Blond")&(Sex=="Female"))

sumMN <- sum(conjMN$Freq)
sumMM <- sum(conjMM$Freq)
sumMR <- sum(conjMR$Freq)
sumMB <- sum(conjMB$Freq)

pror <- c(sumMN,sumMM,sumMR,sumMB)
pie(pror,color,main = "Proporción de color de pelos Mujeres")

