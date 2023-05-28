# NOX ISRAEL
# v 23 05 06

# Analysis by factors

# 1. Satisfaction

SatVars3 <- NoxIsrael3 %>%   # SAMPLE: all vars
  select(ParentalSatisfaction, ChildSatisfaction, 
         SleepLabPreference, Epilepsy) 
Sat3.1 <- skim(SatVars3)  
Sat3.1
Sat3.2 <- SatVars3 %>%       # grouped by Epilepsy
  dplyr::group_by(Epilepsy)%>% skim()
Sat3.2

SatVars5 <- NoxIsrael5 %>%   # SAMPLE: NeuroCondition
  select(ParentalSatisfaction, ChildSatisfaction,
         SleepLabPreference, NeuroCondition) 
Sat5 <- SatVars5 %>%         # grouped by NeuoCondition
  dplyr::group_by(NeuroCondition)%>% skim()
Sat5

# 2. Quality of Signal


# 3. NeuroDiverse.Type vs Symptoms





#_________________________________________

nv <- NoxIsrael3$ChildSatisfaccion

SAT <- group_by(NoxIsrael3, Epilepsy) %>%
  summarise(
    count = n(),
    mean = mean(nv, na.rm = TRUE),
    sd = sd(nv, na.rm = TRUE),
    median = median(nv, na.rm = TRUE),
    IQR = IQR(nv, na.rm = TRUE))
  
SAT

# Write output in excel file - library(openxlsx)

write.xlsx(SAT, "aaa.xlsx", overwrite = TRUE, asTable = FALSE)

# Mann Whitney Test for independent groups

res <- wilcox.test(GlobalQuality ~ Epilepsy,
   data = NoxIsrael3,
   exact = FALSE)

res

# Normalitat
library("ggpubr")
ggdensity(NoxIsrael3$Age, fill = "lightgray") #Density plot
ggqqplot(NoxIsrael3$Age) # QQ plot: #If the points in the plot roughly fall along a straight diagonal line, then the data is assumed to be normally distributed
hist(NoxIsrael3$Age, col='steelblue') #If the histogram is roughly “bell-shaped”, then the data is assumed to be normally distributed.

ks.test(NoxIsrael$Age) #if the p-value of the test is > than α = .05, then the data is assumed to be normally distributed
shapiro.test(NoxIsrael3$Age) #if the p-value of the test is > than α = .05, then the data is assumed to be normally distributed
qqnorm(NoxIsrael$Age)
qqline(NoxIsrael$Age)


