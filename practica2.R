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
















#========================================== PARTE NACHO=========================================================#
##-- INSTALACIÓN DE PAQUETES --##

#install.packages ("tidyverse")
#library("tidyverse")


##-- IMPORTAR DATA FRAME DESDE EXCEL --##

#library(readxl)
#premier <- read_excel("Premier League 2011-12 Match by Match.xls")


##-- GOLES POR JUGADOR --##

Goalsby_Player <- group_by(premier,`Player Surname`)
Goalsby_Player <- as.data.frame(summarise(Goalsby_Player,Goals=sum(Goals,na.rm=TRUE)))
Goalsby_Player <- arrange(Goalsby_Player,desc(Goals))

MaxGoleador <- top_n( Goalsby_Player, 10, Goals)
ggplot( MaxGoleador,
        aes( x=Goals, y= reorder(`Player Surname`, Goals),
             fill = Goals),legend=FALSE) +
  geom_bar( stat = "identity" ) +
  labs(y="Player", title = "Máximos Goleadores")


##-- TIEMPO POR JUGADOR --##

Timeby_Player <- group_by(premier, `Player Surname`)
Timeby_Player <- as.data.frame(summarise(Timeby_Player,TimePlayed=sum(`Time Played`,na.rm=TRUE)))
Timeby_Player <- arrange(Timeby_Player,desc(TimePlayed))

MaxTiempo <- top_n( Timeby_Player, 10, TimePlayed)
ggplot( MaxTiempo,
        aes( x=TimePlayed, y= reorder(`Player Surname`, TimePlayed),
             fill = TimePlayed),legend=FALSE) +
  geom_bar( stat = "identity" ) +
  labs(x="Mins Played",y="Player", title = "Máximo Tiempo Jugado")


##-- GOLES POR MINUTO Y JUGADOR --##

Goals_Minute <- group_by(premier,`Player Surname`)
Goals_Minute <- as.data.frame(summarise(Goals_Minute,Goals=sum(Goals,na.rm=TRUE),Time_Played=sum(`Time Played`,na.rm=TRUE)))
Goals_Minute <- mutate(Goals_Minute, 'Goals/100min'=Goals/(Time_Played/100))
Goals_Minute <- arrange(Goals_Minute,desc(`Goals/100min`))
Goals_Minute <- select(Goals_Minute,-Goals,-Time_Played)

MaxGolesMin <- top_n( Goals_Minute, 10,`Goals/100min`)
ggplot( MaxGolesMin,
        aes( x=`Goals/100min`, y= reorder(`Player Surname`, `Goals/100min`),
             fill = `Goals/100min`),legend=FALSE) +
  geom_bar( stat = "identity" ) +
  labs(x="Goals by 100 min",y="Player", title = "Mejores Ratio Goles por 100 Minutos")


##-- GOLES POR EQUIPO --#

Goalsby_Team <- group_by(premier,Team)
Goalsby_Team <- as.data.frame(summarise(Goalsby_Team,Goals=sum(Goals,na.rm=TRUE)))
Goalsby_Team <- arrange(Goalsby_Team,desc(Goals))

TeamGoleador <- top_n( Goalsby_Team, 10, Goals)
ggplot( TeamGoleador,
        aes( x=Goals, y= reorder(Team, Goals),
             fill = Goals),legend=FALSE) +
  geom_bar( stat = "identity" ) +
  labs(y="Team", title = "Máximos Goleadores")


##-- GOLES EN CASA/FUERA --#

Goalsby_Venue <- group_by(premier,Venue)
Goalsby_Venue <- as.data.frame(summarise(Goalsby_Venue,Goals=sum(Goals,na.rm=TRUE)))
Goalsby_Venue <- arrange(Goalsby_Venue,desc(Goals))

ggplot( Goalsby_Venue,
        aes( x=Goals, y= Venue,
             fill = Goals),legend=FALSE) +
  geom_bar( stat = "identity" ) +
  labs(y="Venue", title = "Goals by Venue")


##-- GOLES POR EQUIPO EN CASA/FUERA --#

VenuebyTeam <- premier[,c("Team","Venue","Goals")]

ggplot(data = VenuebyTeam, aes(x = Team, y = Goals, fill = Venue, group = Venue)) +
  geom_bar(data = subset(VenuebyTeam, Venue == "Home"), stat = "identity") +
  geom_bar(data = subset(VenuebyTeam, Venue == "Away"), stat = "identity") + 
  coord_flip() + 
  scale_fill_manual(values=c('#25AAE2','#8BC540')) +
  labs(x = "Teams",y = "Goals",title = "Goals by Team and Venue")+
  scale_y_continuous(breaks = seq(0, 60, by = 5))