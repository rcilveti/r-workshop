# VISUALIZATION

# A. SATISFACTION

# A.1 Selection of vars
Sat3 <- NoxIsrael3 |> select(ChildSatisfaction, ParentalSatisfaction, SleepLabPreference,
                             NeuroCondition, NeuroDiverse.Type)
Sat3 <- as.data.frame(Sat3)                                       # Convert new data as data.frame
Sat5 <- NoxIsrael5 |> select(ChildSatisfaction, ParentalSatisfaction, 
                             SleepLabPreference, NeuroCondition, NeuroDiverse.Type)
Sat5 <- as.data.frame(Sat5)                                       # Convert new data as data.frame

# Boxplot from original table, grouped by NeuroCondition
Satv <- ggplot(Sat5, aes(NeuroContition, ParentalSatisfaction)) +
   stat_boxplot(geom = "errorbar", width = 0.1) + 
   geom_boxplot( outlier.color = NA) +
   scale_y_continuous(limits = c(0, 10)) +
   ggtitle("Parental Satisfaction with H-PSG") +
   theme_classic() +
   theme(plot.title = element_text(color="black",hjus=0.5, size = 18, face="bold")) +  
   xlab("Neuro-Condition") +
   ylab("Parental Satisfaction mesured by VAS (0-10)")
Satv

# A.2. Create pivot-tables where new var, 'Satisfaction' includes the subgroups: Child, Parental and SleepLab Preference 
Sat3.2 <- Sat3 |> pivot_longer(
  cols = c("ChildSatisfaction", "ParentalSatisfaction", "SleepLabPreference"),
  names_to = "Satisfaction",
  values_to = "VAS")
Sat3.2
Sat5.2 <- Sat5 |> pivot_longer(
  cols = c("ChildSatisfaction", "ParentalSatisfaction", "SleepLabPreference"),
  names_to = "Satisfaction",
  values_to = "VAS")
Sat5.2

# A.3. Boxplot with total data 
Satv2.1 <- ggplot(Sat3.2, aes(Satisfaction, VAS)) +
  stat_boxplot(geom = "errorbar", width = 0.1) + 
  geom_boxplot(outlier.color = NA) +
  scale_y_continuous(limits = c(0, 10)) +
  ggtitle("Child, Parental Satisfaction and Sleep Lab Preference \nwith H-PSG") +
  theme_classic() +
  theme(plot.title = element_text(color="black",hjus=0.5, size = 14, face="bold")) +  
  xlab("Satisfaction") +
  ylab("Scores mesured by VAS (0-10)")
Satv2.1

# A.4. Boxplot grouped by Neurocondition 
Satv2.2 <- ggplot(Sat5.2, aes(Satisfaction, VAS, fill=NeuroCondition)) +
  geom_boxplot(outlier.color = NA) +
  scale_y_continuous(limits = c(0, 10)) +
  ggtitle("Child, parental satisfaction and Sleep-lab preference\n with H-PSG by Neuro-Condition of the children") +
  theme_classic() +
  theme(plot.title = element_text(color="black",hjus=0.5, size = 14, face="bold"))+
  theme(legend.position="bottom")+
  theme(legend.title = element_text(size = 10, face="bold"))+
  xlab("Satisfaction") +
  ylab("Scores mesured by VAS (0-10)")
Satv2.2

# B. QUALITY OF SIGNAL

# B.1 Selection of vars
QoS3 <- NoxIsrael3 |> select(GlobalQuality, SpO2Quality, CanulaQuality, RIPQuality, 
                             NeuroCondition, NeuroDiverse.Type)
QoS3 <- as.data.frame(QoS3)                                       # Convert new data as data.frame
QoS5 <- NoxIsrael5 |> select(GlobalQuality, SpO2Quality, CanulaQuality, RIPQuality, 
                             NeuroCondition, NeuroDiverse.Type)
QoS5 <- as.data.frame(QoS5)                                       # Convert new data as data.frame

# B.2. Create pivot-tables where new var, 'SignalQuality' includes the subgroups: Global, SpO2, Flow and RIP Quality 
QoS3.2 <- QoS3 |> pivot_longer(
  cols = c("GlobalQuality", "SpO2Quality", "CanulaQuality", "RIPQuality"),
  names_to = "TypeOfSignal",
  values_to = "Quality")
QoS3.2
QoS5.2 <- QoS5 |> pivot_longer(
  cols = c("GlobalQuality", "SpO2Quality", "CanulaQuality", "RIPQuality"),
  names_to = "TypeOfSignal",
  values_to = "Quality")
QoS5.2

# B.3. Boxplot with total data 
QoSv2.1 <- ggplot(QoS5.2, aes(TypeOfSignal, Quality)) +
  stat_boxplot(geom = "errorbar", width = 0.1) + 
  geom_boxplot(outlier.color = NA) +
  ggtitle("Quality of Signals with the H-PSG") +
  theme_classic() +
  theme(plot.title = element_text(color="black",hjus=0.5, size = 18, face="bold")) +
  scale_x_discrete(limits=c("GlobalQuality", "SpO2Quality", "CanulaQuality", "RIPQuality"))+
  xlab("Type of Signal") +
  ylab("Score of Quality (0-100)")
QoSv2.1

# B.4. Boxplot grouped by Neurocondition 
QoSv2.2 <- ggplot(QoS5.2, aes(TypeOfSignal, Quality, fill=NeuroCondition)) +
  geom_boxplot(outlier.color = NA) +
  ggtitle("Quality of Signals with the H-PSG by\nNeuro-Condition of the children") +
  theme_classic() +
  theme(plot.title = element_text(color="black",hjus=0.5, size = 18, face="bold"))+
  theme(legend.position="bottom")+
  theme(legend.title = element_text(size = 10, face="bold"))+
  scale_x_discrete(limits=c("GlobalQuality", "SpO2Quality", "CanulaQuality", "RIPQuality")) +
  xlab("Type of Signal") +
  ylab("Score of Quality (0-100)")
QoSv2.2

# C. DIAGNOSIS

# C.1 Create new var 'SD'  [DO NOT WORK!!!]
NoxIsrael3.1 <- NoxIsrael3 %>%
  mutate(SD = case_when(SAHS == 'SAHS' & PLMS == 'Normal PLMI ≤5/h' ~ 'SAHS',
                        SAHS == 'Normal' & PLMS == 'PLMI >5/h' ~ 'PLMS',
                        SAHS == 'SAHS' & PLMS == 'PLMI >5/h' ~ 'Combined',
                        SAHS == 'Normal' & PLMS =='Normal PLMI ≤5/h'~ 'Normal'))
NoxIsrael5.1 <- NoxIsrael5 %>%
  mutate(SD = case_when(SAHS == 'SAHS' & PLMS == 'Normal PLMI ≤5/h' ~ 'SAHS',
                        SAHS == 'Normal' & PLMS == 'PLMI >5/h' ~ 'PLMS',
                        SAHS == 'SAHS' & PLMS == 'PLMI >5/h' ~ 'Combined',
                        SAHS == 'Normal' & PLMS =='Normal PLMI ≤5/h'~ 'Normal'))
#_________________________________

# C.2 Convert Data into Table

library(ggmosaic)
library(descr)

TableDx1 <- table(NoxIsrael3$SAHS, NoxIsrael3$PLMS)                   # SAHS vs PLMS
CrossTable(TableDx1, prop.r = FALSE, 
           prop.t = FALSE, prop.chisq= FALSE, chisq=TRUE)

TableDx2 <- table(NoxIsrael5$SAHS.Grade, NoxIsrael5$NeuroCondition)   # SAHS.Grade vs NeuroCondition
CrossTable(TableDx2, prop.r = FALSE, 
           prop.t = FALSE, prop.chisq= FALSE, chisq=TRUE)             
TableDx3 <- table(NoxIsrael5$PLMS, NoxIsrael5$NeuroCondition)            # PLMS vs NeuroCondition
CrossTable(TableDx3, prop.r = FALSE, 
           prop.t = FALSE, prop.chisq= FALSE, chisq=TRUE) 
TableDx4 <- table(NoxIsrael5$SD, NoxIsrael5$NeuroCondition)              # SD vs NeuroCondition
CrossTable(TableDx4, prop.r = FALSE, 
           prop.t = FALSE, prop.chisq= FALSE, chisq=TRUE)

TableDx5.1 <- table(NoxIsrael5$PLMS, NoxIsrael5$NeuroDiverse.Type)       # PLMS vs NeuroDiverse.Type
CrossTable(TableDx5.1, prop.r = FALSE, 
           prop.t = FALSE, prop.chisq= FALSE, chisq=TRUE)
TableDx5.2 <- table(NoxIsrael5$SAHS, NoxIsrael5$NeuroDiverse.Type)       # SAHS vs NeuroDiverse.Type
CrossTable(TableDx5.2, prop.r = FALSE, 
           prop.t = FALSE, prop.chisq= FALSE, chisq=TRUE)
TableDx5.3 <- table(NoxIsrael5$SAHS.Grade, NoxIsrael5$NeuroDiverse.Type) # SAHS.Grade vs NeuroDiverse.Type
CrossTable(TableDx5.3, prop.r = FALSE, 
           prop.t = FALSE, prop.chisq= FALSE, chisq=TRUE)
TableDx5.4 <- table(NoxIsrael5$SD, NoxIsrael5$NeuroDiverse.Type)         # SD   vs NeuroDiverse.Type
CrossTable(TableDx5.4, prop.r = FALSE, 
           prop.t = FALSE, prop.chisq= FALSE, chisq=TRUE)


# create dataframe from contingency table
x <- c()
for (row in rownames(TableDx1)) {
  for (col in colnames(TableDx1)) {
    x <- rbind(x, matrix(rep(c(row, col), TableDx1[row, col]), ncol = 2, byrow = TRUE))
  }
}
Dx1 <- as.data.frame(x)
colnames(df) <- c("SAHS", "PLMS")
Dx1

# Fisher's exact test with raw data
test4 <- fisher.test(TableDx4)
test4

# combine plot and statistical test with ggbarstats
library(ggstatsplot)
ggbarstats(
  NoxIsrael3, PLMS, SAHS,
  title = "Association between diagnosis of SAHS or PLMS",
  legend.title = NULL,
  xlab = "Respiratory Disorder",
  ylab = "% of cases",
  ggtheme = ggstatsplot::theme_ggstatsplot(),
  package = "RColorBrewer",
  palette = "Pastel1",
  results.subtitle = FALSE,
  subtitle = paste0(
    "Fisher's exact test", ", p-value = ",
    ifelse(test$p.value < 0.001, "< 0.001", round(test$p.value, 3))
  )
)

# Graphic: Mosaic Plot
with(NoxIsrael3, mosaicplot(table(SAHS, PLMS), 
                      color=c("lightblue", "darkblue"), 
                      main = "Mosaic plot of the differences in sleep disorders' \ndiagnosis of SAHS or PLMS in the sample",
                      xlab ="Respiratory Disturbance",
                      ylab = "PLMS",
                      las = 1,
                      off = 5))

mosaic(~PLMS+SAHS+SD.Combined, data = NoxIsrael3.1)
                          
with(NoxIsrael5, mosaicplot(table(NeuroCondition, SAHS.Grade), 
                      color=c("#E6EEFF", "#99F9FF", "#51C3CC","#1E8E99"), 
                      main = "Mosaic plot of the association between \nSAHS and Neuro-Condition of the children",                        xlab ="Neuro Condition",
                      ylab = "SAHS Grade",
                      las = 1,
#                     sort = c("Normal AHI < 1/h", "Mild AIH 1 & < 5/h", "Moderate AHI 5 & 10/h", "Severe AHI >10/h"),
                      off = 3))

library("vcd")
library(grid)

struct <- structable(~ PLMS + SAHS, data = NoxIsrael5.2)
dxcolors <- c("lightblue", "lightpink")
mosaic(struct, data = NoxIsrael3.1,
       highlighting = "PLMS",
       highlighting_fill = dxcolors,
       main = "Mosaic plot of the association between\n Diagnosis of SAHS or PLMS",
       xlab ="Respiratory Disturbance",
       ylab = "PLMS",
       labeling = labeling_values,
       direction = "h")


#-------------------

# Renaming, converting as dataframes
NoxIsrael3.1 <- names(NoxIsrael3)[names(NoxIsrael3) == "NeuroDiverse"] <- "NeuroCondition"          #Rename var name
NoxIsrael3.1 <- names(NoxIsrael3)[names(NoxIsrael3) == "ChildSatisfaccion"] <- "ChildSatisfaction"  #Rename var name
NoxIsrael3.1 <- as.data.frame(NoxIsrael3)                       # Convert new data as data.frame
NoxIsrael5.1 <- names(NoxIsrael5)[names(NoxIsrael5) == "NeuroDiverse"] <- "NeuroCondition"          #Rename var name
NoxIsrael5.1 <- names(NoxIsrael5)[names(NoxIsrael5) == "ChildSatisfaccion"] <- "ChildSatisfaction"  #Rename var name
NoxIsrael5.1 <- as.data.frame(NoxIsrael5)                       # Convert new data as data.frame

library("gplots")
dt <- as.table(as.matrix(NoxIsrael5.1))

balloonplot(t(dt), main ="housetasks", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)

