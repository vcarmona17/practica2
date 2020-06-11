##  instalacion paquetes
if(!require("tidyverse")) {
  install.packages ("tidyverse")
  library("tidyverse")
}

if(!require("scales")) {
  install.packages("scales")
  library("scales")
}

if(!require("ggthemes")) {
  install.packages("ggthemes")
  library("ggthemes")
}


##  instalar dataframe
library(readxl)
premier <- read_excel("Premier League 2011-12 Match by Match.xls")
View(premier)





## inspeccion de la base de datos

str(premier)
summary(premier)
anyNA(premier)
mean(premier$Goals)
mean(premier$Goals, na.rm = TRUE)
which(is.na(premier$`Player ID`))



#========================================== PARTE VICTOR=========================================================

##  portero más goleado
porteros <- filter(premier, `Position Id` == 1)
porteros1 <- porteros[,c(3,11,12,171)]
dp<-group_by(porteros1, `Player Surname`)
dp1<-summarise(dp, golesenc= sum(`Goals Conceded`, na.rm = TRUE), tiempojug= sum(`Time Played`, na.rm = TRUE),partjug= sum(Appearances, na.rm = TRUE))
arrange(dp1,desc(golesenc))


## Gráfico donde se muestran todos los porteros y los goles  total porteros
ggplot(dp1,
       aes(y= reorder(`Player Surname`,golesenc),x=golesenc)) + 
  geom_bar(stat = "identity")+
  labs( y = "Apellido del Jugador")



## TOP 10 de porteros más goleados
masgoleadotop10 <- top_n( dp1, 10, golesenc)
ggplot( masgoleadotop10, 
        aes( y= reorder(`Player Surname`, golesenc),x=golesenc,
             fill = golesenc )) +
  geom_bar( stat = "identity" ) +
  labs( y = "Apellido del Jugador", x="Goles Encajados", title = "TOP 10 Porteros más goleados" ) 



## Análisis gol por minuto y gol por partido
ratiosportero <- mutate(dp1, golmin=golesenc/tiempojug, golpar=golesenc/partjug) 
view(ratiosportero)


## MEJOR PORTERO
ggplot( ratiosportero, 
        aes( y= reorder(`Player Surname`, golpar),x=golpar,
             fill = golpar )) +
  geom_bar( stat = "identity" ) +
  labs( y = "Apellido del Jugador", x="Goles Encajados por partido", title = "Ratio goles/partido" )


## Gráfico que muestra los goles y las box muestran la media de goles por partido que le han marcado
ggplot(porteros1, aes(y=`Player Surname`,x=`Goals Conceded`)) + 
  geom_jitter() +
  geom_boxplot(color="blue")







## entradas con exito segun posición

ggplot(premier) + 
  geom_count(aes(x = `Player ID`, y = `Tackles Won`, color = `Position Id`))


## Analizamos los resultados del gráfico
tackle <- premier[,c(3,10,150,151)]
tackle<-group_by(tackle, `Player Surname`,`Position Id`)
tackles<-summarise(tackle, tackleswon= sum(`Tackles Won`, na.rm = TRUE))
view(arrange(tackles,desc(tackleswon)))






# EJECUTA PARA SELECCIONAR UN EQUIPO
de<-premier[,c(5,6)]
dee<-group_by(de, `Team`, `Team Id`)
summarise(dee) # En la lista que aparecera en la consola puedes consultar el equipo y su codigo asignado


# Duelos Ganados por equipo
equipo <- filter(premier, `Team Id` == 1) #Cambiar este número según el codigo del equipo deseado
duelos <- equipo[,c(3,144:149)]
dd<-group_by(duelos, `Player Surname`)
dfuelostot <- summarise(dd, duelostot= sum(`Duels won`,`Duels lost`, na.rm = TRUE), "Duels won"= sum(`Duels won`, na.rm = TRUE))

##  Cálculo de los duelos ganados sobre el total 
ratiodu <- mutate(dfuelostot, ratioduelos=(`Duels won`/ duelostot)*100)

## Gráfico de los jugadores más eficientes en duelos   
ggplot(ratiodu) +
  aes(`Player Surname`, ratioduelos, fill = duelostot) +
  geom_col() +
  scale_y_continuous(labels = percent_format())+
  labs( y = "Duelos Ganados(%)", x="Apellido del Jugador", title = "Jugadores más eficientes en los duelos" )+
  scale_fill_gradient(low="red", high="yellow")+ 
  coord_flip()


##  MEJORES JUGADORES AEREOS DEL EQUIPO SELECCIONADO  

ggplot(duelos, aes(x =`Duels won`, y =`Aerial Duels won` )) + 
  geom_jitter() +
  facet_grid(.~`Player Surname`)


##  MEJORES RECUPERADORES DEL EQUIPO SELECCIONADO  

ggplot(duelos, aes(x =`Duels won`, y =`Ground Duels won` )) + 
  geom_jitter() +
  facet_grid(.~`Player Surname`)
