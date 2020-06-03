# instalacion paquetes
install.packages ("tidyverse")
library("tidyverse")

# carga database
?read.table()

premier <- read.table("Premier League 2011-12 Match by Match.xls", sep="\t", dec=".", quote = "\"'",
                      header=TRUE, skip = 0, na.strings = "NA")

