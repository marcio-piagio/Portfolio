library(geobr)
library(ggmap)
library(Rmisc)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(leaflet.extras)
library(corrplot)
library(leaflet)
library(readxl)
library(nortest)
library(RColorBrewer)
library(highcharter)
library(stringr)
library(geosphere)

paciente <- read_excel("data/Atendimentos_MGA.xlsx")
medicos <- read_excel("data/Atendimentos_MGA.xlsx", 
                      sheet = "Atendimento")



Paciente_MGA  <- paciente[(paciente$Longitude > -52.14  &
                             paciente$Longitude < -51.8 &
                             paciente$Latitude > -23.56 &
                             paciente$Latitude < -23.25),]

# contorno do estado PR 
PR_2020 <- geobr::read_state(code_state = 41, year=2020)
PR_M    <- geobr::read_municipality(showProgress = FALSE)
PR_M    <- PR_M[PR_M$code_state == 41,]

# contorno de Maringá
MGA_2020 <- geobr::read_municipality(code_muni = 4115200,
                                     showProgress = FALSE)

# Distancia do paciente medico gearal
distGG <- numeric(length(paciente$Medico))
for(i in seq_along(paciente$Medico)){
  ind <- grep(paciente[i,3], LETTERS[1:7])
  distGG[i] <- distHaversine(paciente[i,c(2,1)], medicos[ind, c(3,2)])
}
paciente$dist <- round(distGG / 1000, 3)

# categoria idade geral
paciente$categoria <- case_when(
  paciente$Idade <= 30 ~ 'Abaixo de 30',
  paciente$Idade <= 40 ~ 'Entre 30 e 40',
  paciente$Idade <= 50 ~ 'Entre 40 e 50',
  paciente$Idade <= 60 ~ 'Entre 50 e 60',
  paciente$Idade <= 70 ~ 'Entre 60 e 70',
  TRUE ~ 'Maior que 70'
)

# categoria distancia geral
paciente$categoriaD <- case_when(
  paciente$dist <= 3 ~ 'Abaixo de 03',
  paciente$dist <= 6 ~ 'Entre 03 e 06',
  paciente$dist <= 9 ~ 'Entre 06 e 09',
  paciente$dist <= 12 ~ 'Entre 09 e 12',
  paciente$dist <= 15 ~ 'Entre 12 e 15',
  TRUE ~ 'Maior que 15'
)

# outros pacientes 
Paciente_outros <- paciente[!(paciente$Longitude > -52.14  &
                                paciente$Longitude < -51.8 &
                                paciente$Latitude > -23.56 &
                                paciente$Latitude < -23.25),]

Paciente_outros$Local <- c("Pinhas", "Cap. Leônidas Marques", "Arapongas", "Curitiba",
                           "Curitiba", "Pinhas", "Curitiba", "Arapongas", "Iguaraçu",
                           "Iguaraçu", "Iguaraçu")

# Distancia do paciente medico maringa
dist <- numeric(length(Paciente_MGA$Medico))
for(i in seq_along(Paciente_MGA$Medico)){
  ind <- grep(Paciente_MGA[i,3], LETTERS[1:7])
  dist[i] <- distHaversine(Paciente_MGA[i,c(2,1)], medicos[ind, c(3,2)])
}
Paciente_MGA$dist <- round(dist / 1000, 3)



# categorizando as idades maringa
Paciente_MGA$categoriaI <- case_when(
  Paciente_MGA$Idade <= 30 ~ 'Abaixo de 30',
  Paciente_MGA$Idade <= 40 ~ 'Entre 30 e 40',
  Paciente_MGA$Idade <= 50 ~ 'Entre 40 e 50',
  Paciente_MGA$Idade <= 60 ~ 'Entre 50 e 60',
  Paciente_MGA$Idade <= 70 ~ 'Entre 60 e 70',
  TRUE ~ 'Maior que 70'
)

# categorizando as distacias maringa
Paciente_MGA$categoriaD <- case_when(
  Paciente_MGA$dist <= 3 ~ 'Abaixo de 03',
  Paciente_MGA$dist <= 6 ~ 'Entre 03 e 06',
  Paciente_MGA$dist <= 9 ~ 'Entre 06 e 09',
  Paciente_MGA$dist <= 12 ~ 'Entre 09 e 12',
  Paciente_MGA$dist <= 15 ~ 'Entre 12 e 15',
  TRUE ~ 'Maior que 15'
)

# distancia para cada medico em linha reta

distT <- array(0, dim = c(2040,7))
colnames(distT) <- paste0('dist',LETTERS[1:7])
for (j in seq_along(medicos$Local)) {
  for(i in seq_along(Paciente_MGA$Medico)){
  ind <- grep(Paciente_MGA[i,3], LETTERS[1:7])
  distT[i,j] <- round(distHaversine(Paciente_MGA[i,c(2,1)], medicos[j, c(3,2)])/ 1000, 3)
  print(c(i,j))
  }
}

Paciente_MGA <- cbind(Paciente_MGA, distT)

# selecionando os minimos de cada paciente 
Min <- apply(distT, 1, min)

# criando a variavel minimo com os medicos especificos 
Paciente_MGA$MinL <- case_when(
    Paciente_MGA$distA == Min ~ 'A',
  Paciente_MGA$distB == Min ~ 'B',
  Paciente_MGA$distC == Min ~ 'C',
  Paciente_MGA$distD == Min ~ 'D',
  Paciente_MGA$distE == Min ~ 'E',
  Paciente_MGA$distF == Min ~ 'F',
  TRUE ~ 'G'
)

# # calculo da distacia por rua e tempo gasto
# 
# coor <- rbind(medicos[,2:3],Paciente_MGA[,1:2]) %>%
#   sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
# 
# m <- list('A'=list(),'B'=list(),'C'=list(),'D'=list(),'E'=list(),'F'=list(),'G'=list())
# 
# for (j in 1:2040) {
#   for (i in 1:7) {
#     m[[c(i,j)]] <- osrm::osrmRoute(loc = coor[c(i,j+7),],
#                                    returnclass = "sf")
#     print(c(i,j+7))
#   }
# 
# }
# 
# distR <- array(NA, dim = c(2040,7))
# colnames(distR) <- paste0('dist',LETTERS[1:7])
# 
# temR  <- array(NA, dim = c(2040,7))
# colnames(temR) <-  paste0('tem',LETTERS[1:7])
# 
# for (i in 1:7) {
#   for (j in 1:2040) {
#     if(i==2 && j==1570){
#       distR[j, i] = NA
#       temR[j, i] =NA
#     } else {
#       distR[j, i] =  m[[i]][[j]]$distance
#       temR[j, i]  =  m[[i]][[j]]$duration
# 
#     }
# 
# 
#     print(c(i,j))
#   }
# 
# }
# 
# Paciente_dis_tem <- as.data.frame(cbind(distR, temR))



# selecionando os minimos de distancia cada paciente
# Min <- apply(distR, 1, min)

# # criando a variavel minimo com os medicos especificos
# Paciente_dis_tem$dist <- case_when(
#   Paciente_dis_tem$distA == Min ~ 'A',
#   Paciente_dis_tem$distB == Min ~ 'B',
#   Paciente_dis_tem$distC == Min ~ 'C',
#   Paciente_dis_tem$distD == Min ~ 'D',
#   Paciente_dis_tem$distE == Min ~ 'E',
#   Paciente_dis_tem$distF == Min ~ 'F',
#   TRUE ~ 'G'
# )

# selecionando os minimos de tempo cada paciente
# tem <- apply(temR, 1, min)

# # criando a variavel minimo com os medicos especificos
# Paciente_dis_tem$tem <- case_when(
#   Paciente_dis_tem$temA == tem ~ 'A',
#   Paciente_dis_tem$temB == tem ~ 'B',
#   Paciente_dis_tem$temC == tem ~ 'C',
#   Paciente_dis_tem$temD == tem ~ 'D',
#   Paciente_dis_tem$temE == tem ~ 'E',
#   Paciente_dis_tem$temF == tem ~ 'F',
#   TRUE ~ 'G'
# )

 # write.table(Paciente_dis_tem,file = 'Paciente_dis_tem.csv',sep = ";")
 # 
 # geoR  <- list('A'=list(),'B'=list(),'C'=list(),'D'=list(),'E'=list(),'F'=list(),'G'=list())
 # 
 # for (i in 1:7) {
 #   for (j in 1:2040) {
 #     if(i==2 && j==1570){
 #       geoR[[c( i,j)]] =  m[[i]][[j]]$geometry
 #     } else {
 #       geoR[[c( i,j)]] =  m[[i]][[j]]$geometry
 #       
 #     }
 #     
 #     
 #     print(c(i,j))
 #   }
 #   
 # }
 


# ☺☺☺☺☺☺☺☺☺☺☺☺☺☺☺☺☺☺☺☺☺☺☺☺☺☺☺☺☺☺☺☺☺☺☺☺☺☺☺☺☺☺☺☺☺☺☺☺☺☺☺☺☺☺☺☺☺☺☺☺☺☺☺☺☺☺☺☺☺☺☺☺☺☺☺☺☺☺

# m$G[[1]]$distance
# m$G[[1]]$duration
# m$G[[1]]$geometry
# 
# leaflet(data = M_MGA) %>%
#   addProviderTiles("CartoDB.Positron") %>%
#   addMarkers(label = ~Local) %>%
#   addPolylines(data = m$G[[1]]$geometry,
#                label = "OSRM engine",
#                color = "red")
