# Exporting Library of R packages installed on new device

# Source Devive
RLibrary230528 <- as.data.frame(installed.packages())
RLibrary230528
write.csv(RLibrary230528, 'RLibrary230528.csv')
writexl::write_xlsx(RLibrary230528, 'RLibrary230528.xlsx')

#New Device
RLibrary230528 <- readxl::read_xlsx('RLibrary230528.xlsx')
baseR <- as.data.frame(installed.packages())
toInstall <- setdiff(RLibrary230528, baseR)
install.packages(toInstall)
