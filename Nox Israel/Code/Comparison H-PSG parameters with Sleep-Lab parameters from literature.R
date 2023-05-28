# Comparison H-PSG parameters with Sleep-Lab parameters from literature

# 1. NewVar AgeG3 dividing sample into <10yo and ≥10yo children

NoxIsrael3.1 <- NoxIsrael3 %>% 
  mutate(AgeG3 = case_when(Age < 10 ~ '<10yo', 
                           Age >= 10 ~ '≥10yo'))

# 2. Var selection and tabulation


SleepPar <- NoxIsrael3.1 %>% select(Age, AgeG3, Efficiency,SLat, REMLat, 
                                    ArousalIndex, N1., N2., N3., REM.)
library(rstatix)
library(openxlsx)

SleepPar <- SleepPar %>% 
  convert_as_factor(AgeG3)

SP <- group_by(SleepPar, AgeG3) %>%
  summarise(
    count = n(),
    mean = mean(Efficiency, na.rm = TRUE),
    sd = sd(Efficiency, na.rm = TRUE),
    median = median(Efficiency, na.rm = TRUE),
    IQR = IQR(Efficiency, na.rm = TRUE))


SP <- SleepPar %>%
  dplyr::group_by(AgeG3) %>%
  skim()

write.xlsx(SP, "aaa.xlsx", overwrite = TRUE, asTable = FALSE)
