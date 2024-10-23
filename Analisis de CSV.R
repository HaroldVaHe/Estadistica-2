# Harold Vargas Henao

#install.packages("readr")
install.packages("dplyr")
install.packages("ggplot2")

library(ggplot2)
library(readr)
library(utils)
#library(dplyr)

url = "https://raw.githubusercontent.com/HaroldVaHe/Estadistica-2/refs/heads/main/company_dataset.csv"
data= read.csv(url)
head(data)
# Filtrar hombres
hombres <- subset(data, male_female == "M")
#dfH=data[data$male_female=="M",]
#rm(dfH)

# Filtrar mujeres
mujeres <- subset(data, male_female == "F")

head(hombres)
head(mujeres)
rango_hom=range(hombres$pay_yearly)


#Boxplot Hombres score-pay
boxplot(       ylim=rango_hom,      las = 1,  # Hace que las etiquetas del eje x estén en horizontal
               medcol = "black",  # Color de la línea de la mediana
               outcol = "red",  # Color de los outliers
               outpch = 18,  # Estilo de los puntos de outliers
               whisklty = 1,  # Tipo de línea para los bigotes
               boxwex = 0.6,  # Ancho de las cajas
               varwidth = TRUE,  # Ancho de las cajas proporcional al tamaño de la muestra
               border = "darkblue",  # Color del borde de las cajas
               col = c("lightblue", "lightgreen", "lightcoral", "lightyellow"),  # Colores para cada categoría
               hombres$pay_yearly~hombres$performance_score, ylab="Salario Anual", xlab="Score Desempeño",        main = "Distribución de Salario Anual por Score de Desempeño (Hombres)",
              las = 1)  # Hace que las etiquetas del eje x estén en horizontal

# Añadir una rejilla para mejor visualización
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")

#Boxplot Mujeres score-pay
boxplot(  ylim=rango_hom,           las = 1,  # Hace que las etiquetas del eje x estén en horizontal
          medcol = "black",  # Color de la línea de la mediana
          outcol = "red",  # Color de los outliers
          outpch = 18,  # Estilo de los puntos de outliers
          whisklty = 1,  # Tipo de línea para los bigotes
          boxwex = 0.6,  # Ancho de las cajas
          varwidth = TRUE,  # Ancho de las cajas proporcional al tamaño de la muestra
          border = "darkblue",  # Color del borde de las cajas
          col = c("lightblue", "lightgreen", "lightcoral", "lightyellow"),  # Colores para cada categoría
          mujeres$pay_yearly~mujeres$performance_score, ylab="Salario Anual", xlab="Score Desempeño",        main = "Distribución de Salario Anual por Score de Desempeño (Mujeres)")

#Boxplot Hombres antiguedad-pay
boxplot(       ylim=rango_hom,      las = 1,  # Hace que las etiquetas del eje x estén en horizontal
               medcol = "black",  # Color de la línea de la mediana
               outcol = "red",  # Color de los outliers
               outpch = 18,  # Estilo de los puntos de outliers
               whisklty = 1,  # Tipo de línea para los bigotes
               boxwex = 0.6,  # Ancho de las cajas
               varwidth = TRUE,  # Ancho de las cajas proporcional al tamaño de la muestra
               border = "darkblue",  # Color del borde de las cajas
               col = c("lightblue", "lightgreen", "lightcoral", "lightyellow"),  # Colores para cada categoría
               hombres$pay_yearly~hombres$seniority_years, ylab="Salario Anual", xlab="Antiguedad",        main = "Distribución de Salario Anual por Antiguedad (Hombres)",
las = 1)  # Hace que las etiquetas del eje x estén en horizontal

# Añadir una rejilla para mejor visualización
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")

#Boxplot Mujeres Antiguedad-pay
boxplot(  ylim=rango_hom,           las = 1,  # Hace que las etiquetas del eje x estén en horizontal
          medcol = "black",  # Color de la línea de la mediana
          outcol = "red",  # Color de los outliers
          outpch = 18,  # Estilo de los puntos de outliers
          whisklty = 1,  # Tipo de línea para los bigotes
          boxwex = 0.6,  # Ancho de las cajas
          varwidth = TRUE,  # Ancho de las cajas proporcional al tamaño de la muestra
          border = "darkblue",  # Color del borde de las cajas
          col = c("lightblue", "lightgreen", "lightcoral", "lightyellow"),  # Colores para cada categoría
          mujeres$pay_yearly~mujeres$seniority_years, ylab="Salario Anual", xlab="Antiguedad",        main = "Distribución de Salario Anual por Antiguedad (Mujeres)")

#Construir dos regresiones lineales utilizando lm(), donde van la variable dependiente es salario anual y la independiente es la antiguedad

Regresion_Hombres_Salario_antiguedad <- lm(pay_yearly ~ seniority_years, data = hombres)
summary(Regresion_Hombres_Salario_antiguedad)
#y=ax+b
ah=4216.9
bh=84241.9

Regresion_Mujeres_Salario_antiguedad <- lm(pay_yearly ~ seniority_years, data = mujeres)
summary(Regresion_Mujeres_Salario_antiguedad)
am=5163.3
bm=69346.2
print ("En conclusión, ambos modelos evidencian una relación positiva entre la antigüedad y el salario anual, siendo más pronunciada en el caso de las mujeres.")

print(colnames(data))
#Regresion lineal multiple, la variable a ppredecir ya no va a depender de una sola variables (antiguedad) si no de varias variables y=a1x1+b +a2x2 + a3x3 .....
Regresion_Multiple_Hombres= lm (pay_yearly ~ seniority_years + performance_score + age_years, data = hombres)
summary(Regresion_Multiple_Hombres)

Regresion_Multiple_Mujeres= lm (pay_yearly ~ seniority_years + performance_score + age_years, data = mujeres)
summary(Regresion_Multiple_Mujeres)

boxplot(data$pay_yearly~data$male_female)
