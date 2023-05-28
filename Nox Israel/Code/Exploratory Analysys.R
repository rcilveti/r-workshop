#NOX ISRAEL
#23 05 04

# 0. Initialize and Load R libraries

setwd("/Users/RCP/Library/CloudStorage/OneDrive-AdSalutemInstitute/Recerca/Nox Israel")

library(xlsx)
library(tidyverse)
library(rstatix)
library(ggsci)
library(patchwork)
library(here)


# 1. Read and export Exel table to RStats

NoxIsrael <- read.xlsx(file.choose(),1)
head(NoxIsrael)


# 2. Convert grouping variables as factors

NoxIsrael <- NoxIsrael %>%
  convert_as_factor(AgeG1, AgeG2, Gender, PLMS, SAHS, SAHS.Grade, SD.Combined, Epilepsy, ADD, AHDD, AHDD.Combined, LearningDisability, NeuroCondition, NeuroDiverse.Type)


# 3. Check variable type and descriptive statistics

library(skimr)
str(NoxIsrael)
skim(NoxIsrael)


# 4. Convert table as tibble dataframe

NoxIsrael1 <- as_tibble(NoxIsrael)


# 5. Health Status of the dat aset

# 5.1. Missing, zeros, unique, type

library(Hmisc)
library(funModeling) 

df_status(NoxIsrael)

# q_zeros(candidas)/P-zeros(porcentaje): Las variables con muchos ceros pueden no ser útiles para modelar y, en algunos casos, pueden sesgar dramáticamente el modelo.
# q_na/p_na: Varios modelos excluyen automáticamente las filas que tienen valores NA (random forest por ejemplo). En consecuencia, el modelo final puede resultar sesgado debido a varias filas falt antes por una sola variable. Por ejemplo, si los datos contienen tan sólo una de las 100 variables con el 90% de datos NA, el modelo se ejecutará con sólo el 10% de las filas originales.
# q_inf: Los valores infinitos pueden ocasionar un comportamiento inesperado en algunas funciones en R.
# type: Algunas variables están codificadas como números, pero son códigos o categorías y los modelos no las manejan de la misma manera.
# unique: Las variables factor/categóricas con un alto número de valores diferentes (~30) tienden a sobreajustar si las categorías tienen una baja cardinalidad (árboles de decisión, por ejemplo).


# 5.2. Removing useless variables - MISSING

VarsToRemove <- c('Patient', 'Device', 'AdqID', 'Obs.N1', 'Device')
NoxIsrael2 = select(NoxIsrael, -one_of(VarsToRemove))


# 5.3 Removing cases
NoxIsrael3 <- NoxIsrael2 %>% filter(Unique==1)      # Removing non-unique cases: keep only the first study when multiple  
NoxIsrael4 <- NoxIsrael3 %>% filter(!AgeG2=="<4yo") # Removing <4yo children
NoxIsrael5 <- NoxIsrael4 %>% filter(Epilepsy==0)    # Removing Epileptics

# 6. Chategorical Vars analysis: freq(data=NoxIsrael, input = c('gender','AgeG1', 'AgeG2'))

freq(data=NoxIsrael2, path_out='Graphics')

# 7. Numerical Vars analysis

# 7.1. Visual

plot_num(NoxIsrael2, path_out='Graphics')

# 7.2. Numerical

data_prof=profiling_num(NoxIsrael2)

# Prestar atención a las variables con altp desviación estándar.
# Seleccionar las métricas con las que esté más familiarizado: data_prof %>% select(variable, variation_coef, rango_98): Un valor alto en variation_coef puede indicar valores atípicos. rango_98 indica dónde están la mayoría de los valores

# 8. Describe factors by Factor

# Library(psych)
# describeBy(NoxIsrael2$Weight, NoxIsrael2$NeuroDiverse)

# scale_x_discrete(limits = c("ctrl", "trt1"))


# DATA ESPORT TO JOURNAL

Library(gtsummary)





