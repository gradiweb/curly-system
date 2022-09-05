##taller preparcial

# Miguel Ángel Soler Nuñez
# Ingrid Umbacia

# Importar base de datos Titanic y declarar e inicializar una variable con su contenido 
# Documentación del método read.csv() aquí https://www.rdocumentation.org/packages/qtl2/versions/0.28/topics/read_csv
# @user es importante cambiar el file path del documento a leer.
library(readxl)
titanicDB <- read_excel("C:/Users/ijngr/Downloads/Titanic.xlsx")
View(titanicDB)

# Visualizar datos titanic BD
# Documentación del método View() aquí -> https://www.rdocumentation.org/packages/utils/versions/3.6.2/topics/View
View(titanicDB)

# Visualizar nombre de las variables
# Documentación del método names() aquí -> https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/names 
names(titanicDB)

# ¿cómo lee R cada una de las variables?
class(titanicDB$pclass) 
class(titanicDB$survived)
class(titanicDB$name)
class(titanicDB$sex)
class(titanicDB$age)
class(titanicDB$sibsp)
class(titanicDB$parch)
class(titanicDB$ticket)
class(titanicDB$fare)
class(titanicDB$cabin)
class(titanicDB$embarked)
class(titanicDB$boat)
class(titanicDB$body)
class(titanicDB$home.dest)
#En principio R entiende las variables body, fare, parch, sibsp, age,survived y pclass como variables númericas. Entanto las otras "name, ticket,cabin, embarked,boat y home dest" son variables denominadas character o que incluyen letras en su casilla. 
#asi las cosas en una primera mirada encontramos que es una base de datos con 7 variables numericas y 4 variables cualitativas.

# Primeras filas de la BD: las cinco primeras filas nos permite ver una organización de sujetos apartir de su apellido.
# Documentación método head() aquí -> https://www.rdocumentation.org/packages/utils/versions/3.6.2/topics/head
head(titanicDB)


# Últimas filas de la BD: acá podemos rectificar que en efecto exite una organización ABCdario apartir de los apellidos.
# Documentación método tail() aquí -> https://www.rdocumentation.org/packages/rotations/versions/1.6.1/topics/tail
tail(titanicDB)

# Dimensión del dataframe / BD.  
# Documentación método dim() aquí -> https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/dim
## hay 14 variables y 1309 observaciones
dim(titanicDB)

# Numero de filas: 1309.
# Documentación método nrow() -> aquí https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/nrow
nrow(titanicDB)

# Numero de columnas: 14.
# Documentación método ncol() -> aquí https://www.rdocumentation.org/packages/hyperSpec/versions/0.98-20140523/topics/ncol
ncol(titanicDB)

# Seleccionar los primeros 100 individuos -> Solo nombre - name variable
# Documentación método data.frime() aquí -> https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/data.frame
persons = data.frame(titanicDB$name)
filter_persons = head(persons,100)
# debbuger View(filter_persons)

# Seleccionar las 100 primeras filas
first_lines =  titanicDB[1:100,]
# debbuger View(first_lines)

#Seleccionar las últimas tres variables: Boat, body, home dest.
last_columns_dframe = data.frame(titanicDB)
last_columns = last_columns_dframe[, c(tail(names(last_columns_dframe), 3))]
# debbuger View(last_columns)

# Últimos 100 individuos con la columna survived y sex
# Instalamos librería dplyr, nos ofrece métodos especificos para hacer querys con base en objetos de tipo data frame. Documentación aquí -> https://cran.r-project.org/web/packages/dplyr/index.html
install.packages("dplyr")
library(dplyr)
last_persons_survived_sex = tail(data.frame(titanicDB),100)
print_last_people = last_persons_survived_sex %>%
  select(c('survived', 'sex'))
# debbuger View(print_last_people)

# Convierta las variables cualitativas en factor y tenga en cuenta si son ordinales.caracter a numero (no se puede)
str(titanicDB)
# descargamos paquete "car"

installed.packages("car")
library(car) #para recodificar
table(titanicDB$sex)
titanicDB$sex1 <- recode(titanicDB$sex, "female=female;male=male")

table(titanicDB$sex1)
titanicDB$sex1 = as.factor(titanicDB$sex1)

# Visualizar las categorias de cada una de las variables cualitativas
# tamaño de los datos 
length(titanicDB)



#Calcule el costo promedio de pago por una entrada (fare), solo para las personas
#que pagaron más de 50 dólares
# Convertimos la columna Fare en numeric, dado que esta como character y así no podemos calcular el promedio
library(dplyr)
mean_cost_dframe = data.frame(titanicDB)
mean_cost_dframe$fare = as.numeric(as.character(mean_cost_dframe$fare))
View(mean_cost_dframe)
# debbuger sapply(mean_cost_dframe, class)   
filter_cost_upper_fifty = filter(mean_cost_dframe,fare > 50) %>%
  select(c('fare'))
colMeans(filter_cost_upper_fifty)
# debbuger View(filter_cost_upper_fifty)

# Calcule el costo promedio de entrada de los pasajeros de Segunda Clase
# Este lo dejo como duda porque hay un error en le coerción de tipos y no avanzamos bien el punto

#Instrucción que permite una comparación de las principales estadísticas descriptivas
#de la variable fare en términos de la clase
# pendiente, no entendimos bien el punto


#Instrucción que compara los promedios de edades de la variable fare en términos del
# genero
mean(titanicDB)
# pendiente, no entendimos bien el punto

#Cuántas personas son mujeres y cuántas hombres
library(dplyr)
sex_dataframe = data.frame(titanicDB)
filter_by_sex_female = filter(sex_dataframe, sex == 'female')
filter_by_sex_male = filter(sex_dataframe, sex == 'male')
#Cantidad de mujeres: 466
View(count(filter_by_sex_female))
#Cantidad de hombres:843
View(count(filter_by_sex_male))

#Cuántos sobrevivientes y cuántos muertos
library(dplyr)
survivors_dataframe = data.frame(titanicDB)
survivors = filter(survivors_dataframe, survived == 1)
dead = filter(survivors_dataframe, survived == 0)
#Cantidad de sobrevivientes:500
View(count(survivors))
#Cantidad de muertos:809
View(count(dead))

#Gráfico de Barras de genero
# Instalamos ggplot2 para graficar. Documentación aquí https://cran.r-project.org/web/packages/ggplot2/index.html
install.packages("ggplot2")
library(ggplot2)
library(dplyr)
bar_chart = data.frame(titanicDB)
ggplot(data = bar_chart ) +
  geom_bar(mapping = aes(x = sex))

#Gráfico de Barras de pclass
ggplot(data = bar_chart ) +
  geom_bar(mapping = aes(x = pclass))


#Hacer un Boxplot de la varaible pclass coloque dos colores y agregue un título.
#en el boxplot encontramos que en las distribuciones en el titanic por clase hay 
#una mayor presencia de personas que habitaron en las clases 2 y 3 al interior del barco.
# Documentación método bloxplot() aquí -> https://www.rdocumentation.org/packages/graphics/versions/3.6.2/topics/boxplot
# Interpretación -> Asimetria positiva o segada a la derecha
boxplot(titanicDB$pclass,
        main = "Comparación distribución PCLASS",
        xlab = "Clase del viajero",
        col = "green",
        border = "black",
        horizontal = TRUE,
        notch = TRUE
)

#Cree un panel gráfico donde compare los histogramas de la variable fare en términos
#de pclass. Encontramos que al interior del titanic existio una mayor población 
#perteneciente a la tercera clase
hist(titanicDB$pclass)

#Cuánto pagaron en promedio las personas muertas  mayores de 70 Años

titanicDB%>%
  select(age:)




survivors_dataframe = data.frame(titanicDB)
survivors = filter(survivors_dataframe, survived == 1)
dead = filter(survivors_dataframe, survived == 0)
#Cantidad de muertos:809
View(count(dead))  
survivors_dataframe=data.frame(titanicDB),age>80


#Cree un data.frame donde se pueda visualizar la media, la desviación estándar, y el
#CV de la variable fare(Costo del ticket) de los sobrevivientes y los muertos.
#Interprete

mean(titanicDB$fare)
sd(titanicDB$fare)
coef_var(titanicDB$fare)





