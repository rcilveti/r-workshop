# Kruskal-Wallis Test in R

# Quality of signal
#   It contains comparison Epileptic - Neurocondition

NoxIsrael6 <- NoxIsrael3 %>% filter(!AgeG2 == "<4yo" )



# To have an idea of what the data look like, we use the the function sample_n()[in dplyr package]. 
#   The sample_n() function randomly picks a few of the observations in the data frame to print out:
#   Show a random sample

library(dplyr)

set.seed(1234)
dplyr::sample_n(NoxIsrael6, 10)

#   Show the group levels
levels(my_data$group)

#   If the levels are not automatically in the correct order, re-order them as follow:
my_data$group <- ordered(my_data$group,
    levels = c("ctrl", "trt1", "trt2"))

# Compute summary statistics by groups:
#    install.packages("dplyr")

group_by(my_data, group) %>%
  summarise(
    count = n(),
    mean = mean(weight, na.rm = TRUE),
    sd = sd(weight, na.rm = TRUE),
    median = median(weight, na.rm = TRUE),
    IQR = IQR(weight, na.rm = TRUE)
  )

# if(!require(devtools)) install.packages("devtools")
# devtools::install_github("kassambara/ggpubr")

# install.packages("ggpubr")

# Visualize your data with ggpubr:
  # Box plots
  # ++++++++++++++++++++
  # Plot weight by group and color by group

library("ggpubr")

ggboxplot(my_data, x = "group", y = "weight", 
          color = "group", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("ctrl", "trt1", "trt2"),
          ylab = "Weight", xlab = "Treatment")

# Mean plots
# ++++++++++++++++++++
# Plot weight by group
# Add error bars: mean_se
# (other values include: mean_sd, mean_ci, median_iqr, ....)

ggline(my_data, x = "group", y = "weight", 
       add = c("mean_se", "jitter"), 
       order = c("ctrl", "trt1", "trt2"),
       ylab = "Weight", xlab = "Treatment")

# Compute Kruskal-Wallis test
kruskal.test(weight ~ group, data = my_data)

# Interpret
# As the p-value is less than the significance level 0.05, we can conclude that there are significant differences between the treatment groups.

# Pairwise-comparison between groups
#   function pairwise.wilcox.test() to calculate pairwise comparisons between group levels with corrections for multiple testing.

pairwise.wilcox.test(PlantGrowth$weight, PlantGrowth$group,
                     p.adjust.method = "BH")

#   The pairwise comparison shows that, only trt1 and trt2 are significantly different (p < 0.05).