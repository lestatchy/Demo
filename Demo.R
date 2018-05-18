

# library("data.table")

library(readxl)
Cash <- read_excel("UST10y_update2.xlsx",sheet = "Cash")
Swap <- read_excel("UST10y_update2.xlsx",sheet = "Swap", skip = 1)
Libor <- read_excel("UST10y_update2.xlsx",sheet = "Libor 1W")
Tsy <- read_excel("UST10y_update2.xlsx",sheet = "Tsy")
library(xts)
library(dplyr)
DailyReturn <- function(x){
  tmp = x$PX_LAST/lag(x$PX_LAST)-1
  return(cbind(x,tmp))
}
Cash = DailyReturn(Cash)
Swap = DailyReturn(Swap)
Libor = DailyReturn(Libor)
Tsy = DailyReturn(Tsy)


