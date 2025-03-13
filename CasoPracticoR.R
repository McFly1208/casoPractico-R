# Cargar librerías
library(tidyverse)  
library(moments)    

# Cargar datos
df <- read.csv("Titanicv2.csv")  # Leer el archivo CSV.

# Resumen de los datos
str(df)  # Ver la estructura del dataframe (tipos de datos y primeras filas).
summary(df)  # Resumen estadístico de las columnas numéricas.
colSums(is.na(df))  # Contar valores faltantes (NA) en cada columna.

# Análisis: 
# - La columna 'Age' tiene valores faltantes que deben ser manejados.
# - Las columnas 'Survived', 'Sex', 'Pclass' y 'Embarked' son categóricas y serán útiles para análisis agrupados.

# Visualización de la distribución de edades
df %>% ggplot(aes(x = Age)) + 
  geom_histogram(binwidth = 5, fill = "blue", color = "black") + 
  labs(title = "Distribución de Edades", x = "Edad", y = "Frecuencia")

# Análisis:
# - La distribución de edades muestra que la mayoría de los pasajeros tenían entre 20 y 40 años.
# - Hay una ligera asimetría hacia la derecha, lo que indica que hay más pasajeros jóvenes que mayores.

# Calcular la asimetría de la columna 'Age'
skewness(df$Age, na.rm = TRUE)

# Análisis:
# - Un valor de asimetría positivo confirma que la distribución de edades está sesgada hacia la derecha.

# Manejo de valores faltantes en 'Age'
df$Age <- df$Age %>% replace_na(median(df$Age, na.rm = TRUE))  # Reemplazar NA con la mediana.
df <- drop_na(df)  # Eliminar filas con valores NA en cualquier columna.

# Análisis:
# - Los valores faltantes en 'Age' se han reemplazado con la mediana para mantener la distribución.
# - Se han eliminado filas con NA en otras columnas para evitar problemas en el análisis.

# Análisis de supervivencia por edad
df %>% ggplot(aes(x = Age, fill = Survived)) + 
  geom_histogram(binwidth = 5, alpha = 0.5, position = "identity") + 
  labs(title = "Supervivencia por Edad", x = "Edad", y = "Frecuencia")

# Análisis:
# - Los niños menores de 10 años tuvieron una tasa de supervivencia más alta.
# - Los pasajeros entre 20 y 40 años tuvieron una tasa de supervivencia más baja, posiblemente debido a la política de "mujeres y niños primero".

# Análisis de supervivencia por género
df %>% count(Survived, Sex)  # Contar supervivientes y no supervivientes por género.

# Análisis:
# - La mayoría de las mujeres sobrevivieron, mientras que la mayoría de los hombres no.
# - Esto confirma la política de "mujeres y niños primero".

df %>% ggplot(aes(x = Sex, y = Survived)) + 
  geom_jitter(width = 0.2, height = 0.2, alpha = 0.5) + 
  labs(title = "Supervivencia por Género", x = "Género", y = "Supervivencia")

# Análisis:
# - El gráfico de puntos muestra claramente que las mujeres tuvieron una mayor tasa de supervivencia.

# Análisis de supervivencia por clase (Pclass)
df %>% count(Survived, Pclass)  # Contar supervivientes y no supervivientes por clase.

# Análisis:
# - Los pasajeros de primera clase tuvieron una tasa de supervivencia mucho más alta que los de tercera clase.
# - Esto sugiere que la clase social fue un factor importante en la supervivencia.

df %>% ggplot(aes(x = Pclass, y = Survived)) + 
  geom_jitter(width = 0.2, height = 0.2, alpha = 0.5) + 
  labs(title = "Supervivencia por Clase", x = "Clase", y = "Supervivencia")

# Análisis:
# - El gráfico de puntos muestra que los pasajeros de primera clase tuvieron más probabilidades de sobrevivir.

# Análisis de supervivencia por puerto de embarque (Embarked)
df %>% count(Survived, Embarked)  # Contar supervivientes y no supervivientes por puerto de embarque.

# Análisis:
# - Los pasajeros que embarcaron en Cherbourg tuvieron una tasa de supervivencia más alta que los de Southampton y Queenstown.
# - Esto podría estar relacionado con diferencias socioeconómicas entre los pasajeros de cada puerto.

df %>% ggplot(aes(x = Embarked, y = Survived)) + 
  geom_jitter(width = 0.2, height = 0.2, alpha = 0.5) + 
  labs(title = "Supervivencia por Puerto de Embarque", x = "Puerto de Embarque", y = "Supervivencia")

# Análisis:
# - El gráfico de puntos sugiere que el puerto de embarque influyó en la supervivencia, con Cherbourg teniendo la tasa más alta.

#Muertos y sobrevivientes por clase social
ggplot(supervivencia_por_clase, aes(x = factor(Pclass), y = Count, fill = factor(Survived))) + 
  geom_bar(stat = "identity", position = "dodge") + 
  labs(
    title = "Supervivencia por Clase Social",
    x = "Clase Social",
    y = "Cantidad de Pasajeros",
    fill = "Supervivencia"
  ) + 
  scale_fill_manual(values = c("No" = "red", "Yes" = "green"), labels = c("No" = "Muertos", "Yes" = "Sobrevivientes"))
