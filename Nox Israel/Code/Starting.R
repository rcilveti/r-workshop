# Starting

# 1. Set working directory

# 2. Uploading main libraries

# Generals
library(tidyverse)
library(here)

 # Graphics
# library(ggplot2): it's already included in tidyverse package
library(ggstatsplot)
library(ggsci)
library(grid)
library(gplots)
library(vcd)
library(ggmosaic)
library(patchwork)

# Exploratory Analysis
library(skimr)
library(psych)
library(descr)
library(gtsummary)  # tables
library(Hmisc)
library(funModeling) 

# Statistical Analysis
library(rstatix)
library(compareGroups)

# input/Output
library(readxl)
library(openxlsx)


# Best funtions _________________

# 1. Description of dataset 

str(NoxIsrael3)
skim(NoxIsrael3)


