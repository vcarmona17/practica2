# instalacion paquetes
install.packages ("tidyverse")
library("tidyverse")

# inspeccion de la base de datos

str(premier)
summary(premier)
anyNA(premier)
mean(premier$Goals)
mean(premier$Goals, na.rm = TRUE)
which(is.na(premier$PlayerID))


#fecha a numerico

Date <- as.Date(premier$Date)

premier$month <- as.numeric(
  format(premier$Date, '%m'))

premier$year <- as.numeric(
  format(premier$Date, '%Y'))



# maximo goleador

goljugador <- function(Player ID, goals){
  var sum = 0;
  while (player ID)
    sum(goals)
}
return {goals: sum};




goljugador <- function(Goals, Player ID)
{
  vec <- matrix(ncol = ncol(Goals), nrow = nrow(Player ID))  
  for (i in 2:ncol(df)) 
  {
    vec[,i] <- ifelse(Player ID[,i-1]==Player ID, Goals[,i], NA)  
  }
  rowSums(vec, na.rm = TRUE)  
}
premier$goljugador <- goljugador (premier, 10369)


ggplot(premier,
       aes(x=Player ID,y=goals), na.rm=TRUE) + 
  geom_bar()


hist(premier$Time Played)
