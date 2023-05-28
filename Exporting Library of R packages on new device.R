# Exporting Library of R packages installed on new device

# Source Devive
RLibrary230528 <- as.data.frame(installed.packages())
writexl::write_xlsx(RLibrary230528, 'RLibrary230528.xlsx')

# New Device
RLibrary230528 <- readxl::read_xlsx('RLibrary230528.xlsx')
RLibrary230528 <- as.data.frame(RLibrary230528)
baseR <- as.data.frame(installed.packages())
toInstall <- setdiff(RLibrary230528, baseR) # compare dataframes
toInstall <- select(toInstall, "Package" )  # select only var with package's name
toInstall = toInstall[['Package']]          # convert data frame into a vector (index)
install.packages(toInstall)
