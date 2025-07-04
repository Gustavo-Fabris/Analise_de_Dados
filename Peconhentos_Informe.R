rm(list =ls())

####Indicando Diretório de Trabalho.#####

setwd("/home/gustavo/Área de Trabalho/Análise_de_Dados/")

####Planilha com os dados dos municípios e com os códigos do IBGE. Será utilizada nos for loops para buscar dados######## 
####dos municípios e vinculá-los com os dados da base DBF do SINAN#######################################################

BASE_IBGE<-read.csv(file="Base_de_Dados/Auxiliares/Planilha_Base_IBGE.csv", 
                    header=TRUE, 
                    sep=",")

###Libraries###

library(rio)
library(ggplot2)
library(tidyverse)
library(dbplyr)
library(kableExtra)
library(sf)
library(ggspatial)
library(gt)

############################################################################################
####   Definindo o objeto RS para servir de apoio para    ##################################
####    buscar dados de todas as RS. Usar 1, 2, 3..., 21, 22    ############################
############################################################################################

RS <- 22   #####  Deve-se colocar AQUI a Regional

############################################################################################
########  Identificando os anos que entrarão na análise do Informe  ########################
############################################################################################

Anos_Analise <- c(##  "2015", 
  "2016",
  "2017",
  "2018",
  "2019",
  "2020",
  "2021",
  "2022",
  "2023",
  "2024",
  "2025"
)

######   Criando objeto ID_REG. Será utilizado para selecionar
######   RS no DBF do SINAN ONLINE.

ID_REG <- as.data.frame(BASE_IBGE[which(BASE_IBGE$RS == RS), 6])

ID_REG <- as.numeric(ID_REG[1,1])

#########   Criando objeto Fonte para ser utilizado pelos gráficos   ######

Fonte <- "Fonte: SINAN. Base DBF acessada em 23/11/2023"
Fonte1 <- "Fonte: Controle Interno 22ª Regional de Saúde"
Fonte2 <- "Fonte: SINAP. Acesso em 24/06/2025"

####   Estabelecendo o número de municípios em cada RS

nrow <- NROW(BASE_IBGE[which(BASE_IBGE$RS == RS), 1])

SINAP <- read.csv(file = paste0("Base_de_Dados/PECONHENTOS/SINAP_ENCAMINHADOS.csv"),
                  header = TRUE,
                  sep = ",")

CE_BASE_Notificados <- read.csv(file = paste0("Tabulacoes_R/Peconhentos/RS", RS,"_CE_BASE_Notificados.csv"),
                                header = TRUE,
                                sep = ",")

CE_BASE_ARANHAS_Notificados <- read.csv(file = paste0("Tabulacoes_R/Peconhentos/RS", RS,"_CE_BASE_ARANHAS_Notificados.csv"),
                                        header = TRUE,
                                        sep = ",")

CE_BASE_SERPENTES_Notificados <- read.csv(file = paste0("Tabulacoes_R/Peconhentos/RS", RS,"_CE_BASE_SERPENTES_Notificados.csv"),
                                          header = TRUE,
                                          sep= ",")

CE_BASE_ESCORPIOES_Notificados <- read.csv(file = paste0("Tabulacoes_R/Peconhentos/RS", RS,"_CE_BASE__ESCORPIOES_Notificados.csv"),
                                           header = TRUE,
                                           sep = ",")

CE_BASE_ABELHAS_Notificados <- read.csv(file = paste0("Tabulacoes_R/Peconhentos/RS", RS,"_CE_BASE_ABELHAS_Notificados.csv"),
                                        header = TRUE,
                                        sep = ",")

CE_BASE_LAGARTAS_Notificados <- read.csv(file = paste0("Tabulacoes_R/Peconhentos/RS", RS,"_CE_BASE_LAGARTAS_Notificados.csv"),
                                        header = TRUE,
                                        sep = ",")

CE_BASE_OUTROS_Notificados <- read.csv(file = paste0("Tabulacoes_R/Peconhentos/RS", RS,"_CE_BASE_OUTROS_Notificados.csv"),
                                        header = TRUE,
                                        sep = ",")

RS_Serie_Historica_Geral <- read.csv(file = paste0("Tabulacoes_R/Peconhentos/RS_Serie_Historica_Geral.csv"),
                                     header = TRUE,
                                     sep = ",")

RS_Serie_Histórica_Tempo_de_Atendimento <- read.csv(file = paste0("Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2024_TEMPO_ATEND.csv"),
                                                    header = TRUE,
                                                    sep = ",")

RS_Serie_Histórica_Local_da_Picada <- read.csv(file = paste0("Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2024_LOCAL_PICADA.csv"),
                                               header = TRUE,
                                               sep = ",")

RS_Serie_Historica_Tipo_Acid <- read.csv(file = paste0("Tabulacoes_R/Peconhentos/RS_Serie_Historica_Tipo_Acid.csv"),
                                         header = TRUE,
                                         sep = ",")

RS_Serie_Historica_Tipo_Acid_Serpente <- read.csv(file = paste0("Tabulacoes_R/Peconhentos/RS_Serie_Historica_Tipo_Acid_Serpente.csv"),
                                                  header = TRUE,
                                                  sep = ",")  

RS_Serie_Historica_Tipo_Acid_Aranha <- read.csv(file = paste0("Tabulacoes_R/Peconhentos/RS_Serie_Historica_Tipo_Acid_Aranha.csv"),
                                                header = TRUE,
                                                sep = ",")

RS_Serie_Historica_Tipo_Acid_Lagarta <- read.csv(file = paste0("Tabulacoes_R/Peconhentos/RS_Serie_Historica_Tipo_Acid_Lagarta.csv"),
                                                 header = TRUE,
                                                 sep = ",")

RS_SINAN_Piramide <- read.csv(file = "Tabulacoes_R/Peconhentos/RS_SINAN_Piramide.csv",
                              header = TRUE,
                              sep = ",")

RS_PECONHENTOS_2024_Incidencia <- read.csv(file = paste0("Tabulacoes_R/Peconhentos/RS", RS,"_PECONHENTOS_2024_TIPO_ACID.csv"),
                                      header = TRUE,
                                      sep = ",")

RS_PECONHENTOS_2023_Incidencia  <- read.csv(file = paste0("Tabulacoes_R/Peconhentos/RS", RS,"_PECONHENTOS_2023_TIPO_ACID.csv"),
                                      header = TRUE,
                                      sep = ",")

RS_PECONHENTOS_2022_Incidencia  <- read.csv(file = paste0("Tabulacoes_R/Peconhentos/RS", RS,"_PECONHENTOS_2022_TIPO_ACID.csv"),
                                      header = TRUE,
                                      sep = ",")

SHAPEFILE_REGIONAL <- st_read("/home/gustavo/Área de Trabalho/Análise_de_Dados/Shapefiles/22ª_Regional_de_Saúde/22ª_Regional_de_Saúde.shp")

SHAPEFILE_REGIONAL_Dissolvido <- st_read("/home/gustavo/Área de Trabalho/Análise_de_Dados/Shapefiles/22ª_Regional_de_Saúde/22ª_Regional_de_Saúde_Dissolvido.shp")

##################################################################
###############    2025   ########################################
##################################################################

####Criando um objeto com a base DBF 2025 do SINAN#################

PECONHENTO2025<- import("Base_de_Dados/DBF/ANIMPNET2025.DBF")

SINAN_PECONHENTOS_2025 <- PECONHENTO2025 %>% filter(ID_REGIONA == ID_REG | ID_RG_RESI == ID_REG)

assign(paste0("RS", RS, "_PECONHENTOS_2025_SINAN"), SINAN_PECONHENTOS_2025)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2025_SINAN"), SINAN_PECONHENTOS_2025), 
           paste0("Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2025_SINAN.csv"), 
           row.names = FALSE)

############################################################################################################################
############      Filtrando os dados por SE NOTIFICADOS para elaborar o Canal Endêmico   ###################################

AUX <- matrix(data = NA, 
              nrow = nrow, 
              ncol = 54)

AUX <- as.data.frame(AUX)

colnames(AUX)[1] <- "Município" 

AUX[,1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

colnames (AUX)[2:54] <- c(1:53)

for (i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX == i), 2] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==202501)%>%
                                          count()
  )
  
  AUX[which(AUX == i), 3] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                          filter(ID_MN_RESI ==i,
                                                 SEM_PRI ==202502) %>% 
                                          count()
  )
  
  AUX[which(AUX == i), 4] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==202503) %>% 
                                          count()
  )
  
  AUX[which(AUX == i),5] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                         filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==202504) %>% 
                                         count()
  )
  
  AUX[which(AUX == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==202505) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                          filter(ID_MN_RESI ==i,
                                                 SEM_PRI ==202506) %>%
                                          count()
  )
  
  AUX[which(AUX == i), 8] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                          filter(ID_MN_RESI ==i,
                                                 SEM_PRI ==202507) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 9] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                          filter(ID_MN_RESI ==i,
                                                 SEM_PRI ==202508) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 10] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202509) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 11] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202510) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 12] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202511) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 13] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202512) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 14] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202513) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 15] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202514) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 16] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202515) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 17] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202516) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 18] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202517) %>%  
                                           count() 
  )
  
  AUX[which(AUX == i), 19] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202518) %>%      
                                           count() 
  )
  
  AUX[which(AUX == i), 20] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202519) %>%
                                           count() 
  )
  
  AUX[which(AUX == i),  21] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                            filter(ID_MN_RESI ==i,
                                                   SEM_PRI ==202520) %>%
                                            count() 
  )
  
  AUX[which(AUX == i), 22] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202521) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 23] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202522) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 24] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202523) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 25] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202524) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 26] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202525) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 27] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202526) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 28] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202527) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 29] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202528) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 30] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202529) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 31] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202530) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 32] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202531) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 33] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202532) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 34] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202533) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 35] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202534) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 36] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202535) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 37] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202536) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 38] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202537) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 39] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202538) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 40] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202539) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 41] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202540) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 42] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202541) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 43] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202542) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 44] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202543) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 45] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202544) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 46] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202545) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 47] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202546) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 48] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202547) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 49] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202548) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 50] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202549) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 51] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202550) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 52] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202551) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 53] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202552) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 54] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202553) %>%
                                           count() 
  )
}

AUX[, 1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX[(nrow(AUX)+ 1), 2:54] <- apply(AUX[, 2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

RS_PECONHENTOS_2025_SE_Notificados <- AUX

assign(paste0("RS", RS, "_PECONHENTOS_2025_SE_Notificados"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2025_SE_Notificados"), AUX), 
           paste0("Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2025_SE_Notificados.csv"), 
           row.names = FALSE)

############################################################################################################################
############      Filtrando os dados por SE SERPENTES NOTIFICADOS para elaborar o Canal Endêmico   #########################

AUX <- matrix(data = NA, 
              nrow = nrow, 
              ncol = 54)

AUX <- as.data.frame(AUX)

colnames(AUX)[1] <- "Município" 

AUX[,1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

colnames (AUX)[2:54] <- c(1:53)

for (i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX == i), 2] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==202501,
                                                 TP_ACIDENT == 1)%>%
                                          count()
  )
  
  AUX[which(AUX == i), 3] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                          filter(ID_MN_RESI ==i,
                                                 SEM_PRI ==202502,
                                                 TP_ACIDENT == 1) %>% 
                                          count()
  )
  
  AUX[which(AUX == i), 4] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==202503,
                                                 TP_ACIDENT == 1) %>% 
                                          count()
  )
  
  AUX[which(AUX == i),5] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                         filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==202504,
                                                TP_ACIDENT == 1) %>% 
                                         count()
  )
  
  AUX[which(AUX == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==202505,
                                                 TP_ACIDENT == 1) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                          filter(ID_MN_RESI ==i,
                                                 SEM_PRI ==202506,
                                                 TP_ACIDENT == 1) %>%
                                          count()
  )
  
  AUX[which(AUX == i), 8] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                          filter(ID_MN_RESI ==i,
                                                 SEM_PRI ==202507,
                                                 TP_ACIDENT == 1) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 9] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                          filter(ID_MN_RESI ==i,
                                                 SEM_PRI ==202508,
                                                 TP_ACIDENT == 1) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 10] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202509,
                                                  TP_ACIDENT == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 11] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202510,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 12] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202511,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 13] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202512,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 14] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202513,
                                                  TP_ACIDENT == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 15] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202514,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 16] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202515,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 17] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202516,
                                                  TP_ACIDENT == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 18] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202517,
                                                  TP_ACIDENT == 1) %>%  
                                           count() 
  )
  
  AUX[which(AUX == i), 19] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202518,
                                                  TP_ACIDENT == 1) %>%      
                                           count() 
  )
  
  AUX[which(AUX == i), 20] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202519,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i),  21] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                            filter(ID_MN_RESI ==i,
                                                   SEM_PRI ==202520,
                                                   TP_ACIDENT == 1) %>%
                                            count() 
  )
  
  AUX[which(AUX == i), 22] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202521,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 23] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202522,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 24] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202523,
                                                  TP_ACIDENT == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 25] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202524,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 26] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202525,
                                                  TP_ACIDENT == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 27] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202526,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 28] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202527,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 29] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202528,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 30] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202529,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 31] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202530,
                                                  TP_ACIDENT == 1) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 32] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202531,
                                                  TP_ACIDENT == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 33] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202532,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 34] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202533,
                                                  TP_ACIDENT == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 35] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202534,
                                                  TP_ACIDENT == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 36] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202535,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 37] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202536,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 38] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202537,
                                                  TP_ACIDENT == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 39] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202538,
                                                  TP_ACIDENT == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 40] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202539,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 41] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202540,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 42] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202541,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 43] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202542,
                                                  TP_ACIDENT == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 44] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202543,
                                                  TP_ACIDENT == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 45] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202544,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 46] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202545,
                                                  TP_ACIDENT == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 47] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202546,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 48] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202547,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 49] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202548,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 50] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202549,
                                                  TP_ACIDENT == 1) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 51] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202550,
                                                  TP_ACIDENT == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 52] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202551,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 53] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202552,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 54] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202553,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
}

AUX[, 1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX[(nrow(AUX)+ 1), 2:54] <- apply(AUX[, 2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

RS_PECONHENTOS_2025_SE_SERPENTES_Notificados <- AUX

assign(paste0("RS", RS, "_PECONHENTOS_2025_SE_SERPENTES_Notificados"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2025_SE_SERPENTES_Notificados"), AUX), 
           paste0("Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2025_SE_SERPENTES_Notificados.csv"), 
           row.names = FALSE)

############################################################################################################################
############      Filtrando os dados por SE ARANHAS NOTIFICADOS para elaborar o Canal Endêmico   #########################

AUX <- matrix(data = NA, 
              nrow = nrow, 
              ncol = 54)

AUX <- as.data.frame(AUX)

colnames(AUX)[1] <- "Município" 

AUX[,1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

colnames (AUX)[2:54] <- c(1:53)

for (i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX == i), 2] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==202501,
                                                 TP_ACIDENT == 2)%>%
                                          count()
  )
  
  AUX[which(AUX == i), 3] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                          filter(ID_MN_RESI ==i,
                                                 SEM_PRI ==202502,
                                                 TP_ACIDENT == 2) %>% 
                                          count()
  )
  
  AUX[which(AUX == i), 4] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==202503,
                                                 TP_ACIDENT == 2) %>% 
                                          count()
  )
  
  AUX[which(AUX == i),5] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                         filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==202504,
                                                TP_ACIDENT == 2) %>% 
                                         count()
  )
  
  AUX[which(AUX == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==202505,
                                                 TP_ACIDENT == 2) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                          filter(ID_MN_RESI ==i,
                                                 SEM_PRI ==202506,
                                                 TP_ACIDENT == 2) %>%
                                          count()
  )
  
  AUX[which(AUX == i), 8] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                          filter(ID_MN_RESI ==i,
                                                 SEM_PRI ==202507,
                                                 TP_ACIDENT == 2) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 9] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                          filter(ID_MN_RESI ==i,
                                                 SEM_PRI ==202508,
                                                 TP_ACIDENT == 2) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 10] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202509,
                                                  TP_ACIDENT == 2) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 11] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202510,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 12] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202511,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 13] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202512,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 14] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202513,
                                                  TP_ACIDENT == 2) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 15] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202514,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 16] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202515,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 17] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202516,
                                                  TP_ACIDENT == 2) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 18] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202517,
                                                  TP_ACIDENT == 2) %>%  
                                           count() 
  )
  
  AUX[which(AUX == i), 19] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202518,
                                                  TP_ACIDENT == 2) %>%      
                                           count() 
  )
  
  AUX[which(AUX == i), 20] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202519,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i),  21] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                            filter(ID_MN_RESI ==i,
                                                   SEM_PRI ==202520,
                                                   TP_ACIDENT == 2) %>%
                                            count() 
  )
  
  AUX[which(AUX == i), 22] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202521,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 23] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202522,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 24] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202523,
                                                  TP_ACIDENT == 2) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 25] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202524,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 26] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202525,
                                                  TP_ACIDENT == 2) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 27] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202526,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 28] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202527,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 29] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202528,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 30] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202529,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 31] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202530,
                                                  TP_ACIDENT == 2) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 32] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202531,
                                                  TP_ACIDENT == 2) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 33] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202532,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 34] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202533,
                                                  TP_ACIDENT == 2) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 35] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202534,
                                                  TP_ACIDENT == 2) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 36] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202535,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 37] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202536,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 38] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202537,
                                                  TP_ACIDENT == 2) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 39] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202538,
                                                  TP_ACIDENT == 2) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 40] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202539,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 41] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202540,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 42] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202541,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 43] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202542,
                                                  TP_ACIDENT == 2) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 44] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202543,
                                                  TP_ACIDENT == 2) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 45] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202544,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 46] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202545,
                                                  TP_ACIDENT == 2) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 47] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202546,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 48] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202547,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 49] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202548,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 50] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202549,
                                                  TP_ACIDENT == 2) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 51] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202550,
                                                  TP_ACIDENT == 2) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 52] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202551,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 53] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202552,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 54] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202553,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
}

AUX[, 1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX[(nrow(AUX)+ 1), 2:54] <- apply(AUX[, 2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

RS_PECONHENTOS_2025_SE_ARANHAS_Notificados <- AUX

assign(paste0("RS", RS, "_PECONHENTOS_2025_SE_ARANHAS_Notificados"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2025_SE_ARANHAS_Notificados"), AUX), 
           paste0("Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2025_SE_ARANHAS_Notificados.csv"), 
           row.names = FALSE)

############################################################################################################################
############      Filtrando os dados por SE ESCORPIOES NOTIFICADOS para elaborar o Canal Endêmico   #########################

AUX <- matrix(data = NA, 
              nrow = nrow, 
              ncol = 54)

AUX <- as.data.frame(AUX)

colnames(AUX)[1] <- "Município" 

AUX[,1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

colnames (AUX)[2:54] <- c(1:53)

for (i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX == i), 2] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==202501,
                                                 TP_ACIDENT == 3)%>%
                                          count()
  )
  
  AUX[which(AUX == i), 3] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                          filter(ID_MN_RESI ==i,
                                                 SEM_PRI ==202502,
                                                 TP_ACIDENT == 3) %>% 
                                          count()
  )
  
  AUX[which(AUX == i), 4] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==202503,
                                                 TP_ACIDENT == 3) %>% 
                                          count()
  )
  
  AUX[which(AUX == i),5] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                         filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==202504,
                                                TP_ACIDENT == 3) %>% 
                                         count()
  )
  
  AUX[which(AUX == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==202505,
                                                 TP_ACIDENT == 3) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                          filter(ID_MN_RESI ==i,
                                                 SEM_PRI ==202506,
                                                 TP_ACIDENT == 3) %>%
                                          count()
  )
  
  AUX[which(AUX == i), 8] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                          filter(ID_MN_RESI ==i,
                                                 SEM_PRI ==202507,
                                                 TP_ACIDENT == 3) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 9] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                          filter(ID_MN_RESI ==i,
                                                 SEM_PRI ==202508,
                                                 TP_ACIDENT == 3) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 10] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202509,
                                                  TP_ACIDENT == 3) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 11] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202510,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 12] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202511,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 13] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202512,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 14] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202513,
                                                  TP_ACIDENT == 3) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 15] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202514,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 16] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202515,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 17] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202516,
                                                  TP_ACIDENT == 3) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 18] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202517,
                                                  TP_ACIDENT == 3) %>%  
                                           count() 
  )
  
  AUX[which(AUX == i), 19] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202518,
                                                  TP_ACIDENT == 3) %>%      
                                           count() 
  )
  
  AUX[which(AUX == i), 20] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202519,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i),  21] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                            filter(ID_MN_RESI ==i,
                                                   SEM_PRI ==202520,
                                                   TP_ACIDENT == 3) %>%
                                            count() 
  )
  
  AUX[which(AUX == i), 22] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202521,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 23] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202522,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 24] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202523,
                                                  TP_ACIDENT == 3) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 25] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202524,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 26] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202525,
                                                  TP_ACIDENT == 3) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 27] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202526,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 28] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202527,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 29] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202528,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 30] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202529,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 31] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202530,
                                                  TP_ACIDENT == 3) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 32] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202531,
                                                  TP_ACIDENT == 3) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 33] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202532,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 34] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202533,
                                                  TP_ACIDENT == 3) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 35] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202534,
                                                  TP_ACIDENT == 3) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 36] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202535,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 37] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202536,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 38] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202537,
                                                  TP_ACIDENT == 3) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 39] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202538,
                                                  TP_ACIDENT == 3) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 40] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202539,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 41] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202540,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 42] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202541,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 43] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202542,
                                                  TP_ACIDENT == 3) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 44] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202543,
                                                  TP_ACIDENT == 3) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 45] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202544,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 46] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202545,
                                                  TP_ACIDENT == 3) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 47] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202546,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 48] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202547,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 49] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202548,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 50] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202549,
                                                  TP_ACIDENT == 3) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 51] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202550,
                                                  TP_ACIDENT == 3) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 52] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202551,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 53] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202552,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 54] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202553,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
}

AUX[, 1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX[(nrow(AUX)+ 1), 2:54] <- apply(AUX[, 2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

RS_PECONHENTOS_2025_SE_ESCORPIOES_Notificados <- AUX

assign(paste0("RS", RS, "_PECONHENTOS_2025_SE_ESCORPIOES_Notificados"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2025_SE_ESCORPIOES_Notificados"), AUX), 
           paste0("Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2025_SE_ESCORPIOES_Notificados.csv"), 
           row.names = FALSE)

############################################################################################################################
############      Filtrando os dados por SE Abelhas NOTIFICADOS para elaborar o Canal Endêmico   #########################

AUX <- matrix(data = NA, 
              nrow = nrow, 
              ncol = 54)

AUX <- as.data.frame(AUX)

colnames(AUX)[1] <- "Município" 

AUX[,1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

colnames (AUX)[2:54] <- c(1:53)

for (i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX == i), 2] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==202501,
                                                 TP_ACIDENT == 5)%>%
                                          count()
  )
  
  AUX[which(AUX == i), 3] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                          filter(ID_MN_RESI ==i,
                                                 SEM_PRI ==202502,
                                                 TP_ACIDENT == 5) %>% 
                                          count()
  )
  
  AUX[which(AUX == i), 4] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==202503,
                                                 TP_ACIDENT == 5) %>% 
                                          count()
  )
  
  AUX[which(AUX == i),5] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                         filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==202504,
                                                TP_ACIDENT == 5) %>% 
                                         count()
  )
  
  AUX[which(AUX == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==202505,
                                                 TP_ACIDENT == 5) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                          filter(ID_MN_RESI ==i,
                                                 SEM_PRI ==202506,
                                                 TP_ACIDENT == 5) %>%
                                          count()
  )
  
  AUX[which(AUX == i), 8] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                          filter(ID_MN_RESI ==i,
                                                 SEM_PRI ==202507,
                                                 TP_ACIDENT == 5) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 9] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                          filter(ID_MN_RESI ==i,
                                                 SEM_PRI ==202508,
                                                 TP_ACIDENT == 5) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 10] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202509,
                                                  TP_ACIDENT == 5) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 11] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202510,
                                                  TP_ACIDENT == 5) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 12] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202511,
                                                  TP_ACIDENT == 5) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 13] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202512,
                                                  TP_ACIDENT == 5) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 14] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202513,
                                                  TP_ACIDENT == 5) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 15] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202514,
                                                  TP_ACIDENT == 5) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 16] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202515,
                                                  TP_ACIDENT == 5) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 17] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202516,
                                                  TP_ACIDENT == 5) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 18] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202517,
                                                  TP_ACIDENT == 5) %>%  
                                           count() 
  )
  
  AUX[which(AUX == i), 19] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202518,
                                                  TP_ACIDENT == 5) %>%      
                                           count() 
  )
  
  AUX[which(AUX == i), 20] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202519,
                                                  TP_ACIDENT == 5) %>%
                                           count() 
  )
  
  AUX[which(AUX == i),  21] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                            filter(ID_MN_RESI ==i,
                                                   SEM_PRI ==202520,
                                                   TP_ACIDENT == 5) %>%
                                            count() 
  )
  
  AUX[which(AUX == i), 22] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202521,
                                                  TP_ACIDENT == 5) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 23] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202522,
                                                  TP_ACIDENT == 5) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 24] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202523,
                                                  TP_ACIDENT == 5) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 25] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202524,
                                                  TP_ACIDENT == 5) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 26] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202525,
                                                  TP_ACIDENT == 5) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 27] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202526,
                                                  TP_ACIDENT == 5) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 28] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202527,
                                                  TP_ACIDENT == 5) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 29] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202528,
                                                  TP_ACIDENT == 5) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 30] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202529,
                                                  TP_ACIDENT == 5) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 31] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202530,
                                                  TP_ACIDENT == 5) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 32] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202531,
                                                  TP_ACIDENT == 5) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 33] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202532,
                                                  TP_ACIDENT == 5) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 34] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202533,
                                                  TP_ACIDENT == 5) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 35] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202534,
                                                  TP_ACIDENT == 5) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 36] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202535,
                                                  TP_ACIDENT == 5) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 37] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202536,
                                                  TP_ACIDENT == 5) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 38] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202537,
                                                  TP_ACIDENT == 5) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 39] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202538,
                                                  TP_ACIDENT == 5) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 40] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202539,
                                                  TP_ACIDENT == 5) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 41] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202540,
                                                  TP_ACIDENT == 5) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 42] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202541,
                                                  TP_ACIDENT == 5) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 43] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202542,
                                                  TP_ACIDENT == 5) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 44] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202543,
                                                  TP_ACIDENT == 5) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 45] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202544,
                                                  TP_ACIDENT == 5) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 46] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202545,
                                                  TP_ACIDENT == 5) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 47] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202546,
                                                  TP_ACIDENT == 5) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 48] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202547,
                                                  TP_ACIDENT == 5) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 49] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202548,
                                                  TP_ACIDENT == 5) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 50] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202549,
                                                  TP_ACIDENT == 5) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 51] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202550,
                                                  TP_ACIDENT == 5) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 52] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202551,
                                                  TP_ACIDENT == 5) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 53] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202552,
                                                  TP_ACIDENT == 5) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 54] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202553,
                                                  TP_ACIDENT == 5) %>%
                                           count() 
  )
}

AUX[, 1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX[(nrow(AUX)+ 1), 2:54] <- apply(AUX[, 2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

RS_PECONHENTOS_2025_SE_ABELHAS_Notificados <- AUX

assign(paste0("RS", RS, "_PECONHENTOS_2025_SE_ABELHAS_Notificados"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2025_SE_ABELHAS_Notificados"), AUX), 
           paste0("Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2025_SE_ABELHAS_Notificados.csv"), 
           row.names = FALSE)

############################################################################################################################
############      Filtrando os dados por SE Lagartas NOTIFICADOS para elaborar o Canal Endêmico   #########################

AUX <- matrix(data = NA, 
              nrow = nrow, 
              ncol = 54)

AUX <- as.data.frame(AUX)

colnames(AUX)[1] <- "Município" 

AUX[,1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

colnames (AUX)[2:54] <- c(1:53)

for (i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX == i), 2] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==202501,
                                                 TP_ACIDENT == 4)%>%
                                          count()
  )
  
  AUX[which(AUX == i), 3] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                          filter(ID_MN_RESI ==i,
                                                 SEM_PRI ==202502,
                                                 TP_ACIDENT == 4) %>% 
                                          count()
  )
  
  AUX[which(AUX == i), 4] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==202503,
                                                 TP_ACIDENT == 4) %>% 
                                          count()
  )
  
  AUX[which(AUX == i),5] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                         filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==202504,
                                                TP_ACIDENT == 4) %>% 
                                         count()
  )
  
  AUX[which(AUX == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==202505,
                                                 TP_ACIDENT == 4) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                          filter(ID_MN_RESI ==i,
                                                 SEM_PRI ==202506,
                                                 TP_ACIDENT == 4) %>%
                                          count()
  )
  
  AUX[which(AUX == i), 8] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                          filter(ID_MN_RESI ==i,
                                                 SEM_PRI ==202507,
                                                 TP_ACIDENT == 4) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 9] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                          filter(ID_MN_RESI ==i,
                                                 SEM_PRI ==202508,
                                                 TP_ACIDENT == 4) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 10] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202509,
                                                  TP_ACIDENT == 4) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 11] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202510,
                                                  TP_ACIDENT == 4) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 12] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202511,
                                                  TP_ACIDENT == 4) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 13] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202512,
                                                  TP_ACIDENT == 4) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 14] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202513,
                                                  TP_ACIDENT == 4) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 15] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202514,
                                                  TP_ACIDENT == 4) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 16] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202515,
                                                  TP_ACIDENT == 4) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 17] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202516,
                                                  TP_ACIDENT == 4) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 18] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202517,
                                                  TP_ACIDENT == 4) %>%  
                                           count() 
  )
  
  AUX[which(AUX == i), 19] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202518,
                                                  TP_ACIDENT == 4) %>%      
                                           count() 
  )
  
  AUX[which(AUX == i), 20] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202519,
                                                  TP_ACIDENT == 4) %>%
                                           count() 
  )
  
  AUX[which(AUX == i),  21] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                            filter(ID_MN_RESI ==i,
                                                   SEM_PRI ==202520,
                                                   TP_ACIDENT == 4) %>%
                                            count() 
  )
  
  AUX[which(AUX == i), 22] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202521,
                                                  TP_ACIDENT == 4) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 23] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202522,
                                                  TP_ACIDENT == 4) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 24] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202523,
                                                  TP_ACIDENT == 4) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 25] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202524,
                                                  TP_ACIDENT == 4) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 26] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202525,
                                                  TP_ACIDENT == 4) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 27] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202526,
                                                  TP_ACIDENT == 4) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 28] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202527,
                                                  TP_ACIDENT == 4) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 29] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202528,
                                                  TP_ACIDENT == 4) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 30] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202529,
                                                  TP_ACIDENT == 4) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 31] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202530,
                                                  TP_ACIDENT == 4) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 32] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202531,
                                                  TP_ACIDENT == 4) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 33] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202532,
                                                  TP_ACIDENT == 4) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 34] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202533,
                                                  TP_ACIDENT == 4) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 35] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202534,
                                                  TP_ACIDENT == 4) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 36] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202535,
                                                  TP_ACIDENT == 4) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 37] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202536,
                                                  TP_ACIDENT == 4) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 38] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202537,
                                                  TP_ACIDENT == 4) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 39] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202538,
                                                  TP_ACIDENT == 4) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 40] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202539,
                                                  TP_ACIDENT == 4) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 41] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202540,
                                                  TP_ACIDENT == 4) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 42] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202541,
                                                  TP_ACIDENT == 4) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 43] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202542,
                                                  TP_ACIDENT == 4) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 44] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202543,
                                                  TP_ACIDENT == 4) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 45] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202544,
                                                  TP_ACIDENT == 4) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 46] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202545,
                                                  TP_ACIDENT == 4) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 47] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202546,
                                                  TP_ACIDENT == 4) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 48] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202547,
                                                  TP_ACIDENT == 4) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 49] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202548,
                                                  TP_ACIDENT == 4) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 50] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202549,
                                                  TP_ACIDENT == 4) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 51] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202550,
                                                  TP_ACIDENT == 4) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 52] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202551,
                                                  TP_ACIDENT == 4) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 53] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202552,
                                                  TP_ACIDENT == 4) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 54] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202553,
                                                  TP_ACIDENT == 4) %>%
                                           count() 
  )
}

AUX[, 1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX[(nrow(AUX)+ 1), 2:54] <- apply(AUX[, 2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

RS_PECONHENTOS_2025_SE_LAGARTAS_Notificados <- AUX

assign(paste0("RS", RS, "_PECONHENTOS_2025_SE_LAGARTAS_Notificados"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2025_SE_LAGARTAS_Notificados"), AUX), 
           paste0("Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2025_SE_LAGARTAS_Notificados.csv"), 
           row.names = FALSE)

############################################################################################################################
############      Filtrando os dados por SE OUTROS NOTIFICADOS para elaborar o Canal Endêmico   #########################

AUX <- matrix(data = NA, 
              nrow = nrow, 
              ncol = 54)

AUX <- as.data.frame(AUX)

colnames(AUX)[1] <- "Município" 

AUX[,1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

colnames (AUX)[2:54] <- c(1:53)

for (i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX == i), 2] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==202501,
                                                 TP_ACIDENT == 6)%>%
                                          count()
  )
  
  AUX[which(AUX == i), 3] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                          filter(ID_MN_RESI ==i,
                                                 SEM_PRI ==202502,
                                                 TP_ACIDENT == 6) %>% 
                                          count()
  )
  
  AUX[which(AUX == i), 4] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==202503,
                                                 TP_ACIDENT == 6) %>% 
                                          count()
  )
  
  AUX[which(AUX == i),5] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                         filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==202504,
                                                TP_ACIDENT == 6) %>% 
                                         count()
  )
  
  AUX[which(AUX == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==202505,
                                                 TP_ACIDENT == 6) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                          filter(ID_MN_RESI ==i,
                                                 SEM_PRI ==202506,
                                                 TP_ACIDENT == 6) %>%
                                          count()
  )
  
  AUX[which(AUX == i), 8] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                          filter(ID_MN_RESI ==i,
                                                 SEM_PRI ==202507,
                                                 TP_ACIDENT == 6) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 9] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                          filter(ID_MN_RESI ==i,
                                                 SEM_PRI ==202508,
                                                 TP_ACIDENT == 6) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 10] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202509,
                                                  TP_ACIDENT == 6) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 11] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202510,
                                                  TP_ACIDENT == 6) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 12] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202511,
                                                  TP_ACIDENT == 6) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 13] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202512,
                                                  TP_ACIDENT == 6) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 14] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202513,
                                                  TP_ACIDENT == 6) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 15] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202514,
                                                  TP_ACIDENT == 6) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 16] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202515,
                                                  TP_ACIDENT == 6) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 17] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202516,
                                                  TP_ACIDENT == 6) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 18] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202517,
                                                  TP_ACIDENT == 6) %>%  
                                           count() 
  )
  
  AUX[which(AUX == i), 19] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202518,
                                                  TP_ACIDENT == 6) %>%      
                                           count() 
  )
  
  AUX[which(AUX == i), 20] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202519,
                                                  TP_ACIDENT == 6) %>%
                                           count() 
  )
  
  AUX[which(AUX == i),  21] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                            filter(ID_MN_RESI ==i,
                                                   SEM_PRI ==202520,
                                                   TP_ACIDENT == 6) %>%
                                            count() 
  )
  
  AUX[which(AUX == i), 22] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202521,
                                                  TP_ACIDENT == 6) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 23] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202522,
                                                  TP_ACIDENT == 6) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 24] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202523,
                                                  TP_ACIDENT == 6) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 25] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202524,
                                                  TP_ACIDENT == 6) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 26] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202525,
                                                  TP_ACIDENT == 6) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 27] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202526,
                                                  TP_ACIDENT == 6) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 28] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202527,
                                                  TP_ACIDENT == 6) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 29] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202528,
                                                  TP_ACIDENT == 6) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 30] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202529,
                                                  TP_ACIDENT == 6) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 31] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202530,
                                                  TP_ACIDENT == 6) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 32] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202531,
                                                  TP_ACIDENT == 6) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 33] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202532,
                                                  TP_ACIDENT == 6) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 34] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202533,
                                                  TP_ACIDENT == 6) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 35] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202534,
                                                  TP_ACIDENT == 6) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 36] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202535,
                                                  TP_ACIDENT == 6) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 37] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202536,
                                                  TP_ACIDENT == 6) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 38] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202537,
                                                  TP_ACIDENT == 6) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 39] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202538,
                                                  TP_ACIDENT == 6) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 40] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202539,
                                                  TP_ACIDENT == 6) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 41] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202540,
                                                  TP_ACIDENT == 6) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 42] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202541,
                                                  TP_ACIDENT == 6) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 43] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202542,
                                                  TP_ACIDENT == 6) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 44] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202543,
                                                  TP_ACIDENT == 6) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 45] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202544,
                                                  TP_ACIDENT == 6) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 46] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202545,
                                                  TP_ACIDENT == 6) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 47] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202546,
                                                  TP_ACIDENT == 6) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 48] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202547,
                                                  TP_ACIDENT == 6) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 49] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202548,
                                                  TP_ACIDENT == 6) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 50] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202549,
                                                  TP_ACIDENT == 6) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 51] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202550,
                                                  TP_ACIDENT == 6) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 52] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202551,
                                                  TP_ACIDENT == 6) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 53] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202552,
                                                  TP_ACIDENT == 6) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 54] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202553,
                                                  TP_ACIDENT == 6) %>%
                                           count() 
  )
}

AUX[, 1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX[(nrow(AUX)+ 1), 2:54] <- apply(AUX[, 2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

RS_PECONHENTOS_2025_SE_OUTROS_Notificados <- AUX

assign(paste0("RS", RS, "_PECONHENTOS_2025_SE_OUTROS_Notificados"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2025_SE_OUTROS_Notificados"), AUX), 
           paste0("Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2025_SE_OUTROS_Notificados.csv"), 
           row.names = FALSE)

#####        FIltrando os dados por município e construindo uma tabela geral     s#####

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Notificados <- NA

AUX$Zona_Urbana <- NA

AUX$Zona_Rural <- NA

AUX$Zona_Periurbana <- NA

AUX$Zona_Ignorados <- NA

AUX$Feminino <- NA

AUX$Masculino <- NA

AUX$Menos_um_Ano <- NA

AUX$Um_a_Cinco_Anos <- NA

AUX$Cinco_a_Doze_Anos <- NA

AUX$Doze_a_Dezoito_Anos <- NA

AUX$Dezoito_a_Cinquenta_e_Nove <- NA

AUX$Mais_de_Sessenta <- NA

AUX$Analfabeto <- NA

AUX$Fundamental_Incompleto <- NA

AUX$Fundamental_Completo <- NA

AUX$Ensino_Medio_Incompleto <- NA

AUX$Ensino_Medio_Completo <- NA

AUX$Superior_Incompleto <- NA

AUX$Superior_completo <- NA

AUX$Nao_se_Aplica <- NA

AUX$Esc_Ignorado <- NA

AUX$Gestante_SIM <- NA

AUX$Gestante_NÃO <- NA

AUX$Gestante_NA <- NA

AUX$Gestante_IGNORADO <- NA

AUX$Raça_Branca <- NA

AUX$Raça_Preta <- NA

AUX$Raça_Amarela <- NA

AUX$Raça_Parda <- NA

AUX$Raça_Indígena <- NA

AUX$Raça_Ignorado <- NA


#####For loop para criação de tabela por município dos dados de notificação do SINAN##############

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI == i) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i,
                                                          CS_ZONA == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i,
                                                          CS_ZONA == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i,
                                                          CS_ZONA == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i,
                                                          CS_ZONA == 9 
                                                          |
                                                            is.na(CS_ZONA)) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ID_MN_RESI ==i,
                                                           CS_SEXO == "F") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ID_MN_RESI ==i,
                                                           CS_SEXO == "M") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ID_MN_RESI ==i,  
                                                           NU_IDADE_N <= 3012) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ID_MN_RESI ==i,  
                                                           NU_IDADE_N > 4000 
                                                           & 
                                                             NU_IDADE_N <= 4005) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ID_MN_RESI ==i,
                                                           NU_IDADE_N > 4005 
                                                           & 
                                                             NU_IDADE_N <= 4012) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ID_MN_RESI ==i,
                                                           NU_IDADE_N > 4012 
                                                           & 
                                                             NU_IDADE_N <= 4018) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 16] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ID_MN_RESI ==i,
                                                           NU_IDADE_N > 4018 
                                                           & 
                                                             NU_IDADE_N <= 4059) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 17] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ID_MN_RESI ==i,
                                                           NU_IDADE_N > 4059) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 18] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ID_MN_RESI ==i,
                                                           CS_ESCOL_N == "00") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 19] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ID_MN_RESI ==i,
                                                           CS_ESCOL_N == "01" 
                                                           | 
                                                             CS_ESCOL_N == "02" 
                                                           | 
                                                             CS_ESCOL_N == "03") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 20] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ID_MN_RESI ==i,
                                                           CS_ESCOL_N == "04") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 21] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ID_MN_RESI ==i,
                                                           CS_ESCOL_N == "05") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 22] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ID_MN_RESI ==i,
                                                           CS_ESCOL_N == "06") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 23] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ID_MN_RESI ==i,
                                                           CS_ESCOL_N == "07") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 24] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ID_MN_RESI ==i,
                                                           CS_ESCOL_N == "08") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 25] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ID_MN_RESI ==i,
                                                           CS_ESCOL_N == "10") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 26] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ID_MN_RESI ==i,
                                                           CS_ESCOL_N == "09") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 27] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ID_MN_RESI ==i,
                                                           CS_GESTANT == "1" 
                                                           |
                                                             CS_GESTANT== "2" 
                                                           | 
                                                             CS_GESTANT == "3" 
                                                           | 
                                                             CS_GESTANT == "4") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 28] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ID_MN_RESI ==i,
                                                           CS_GESTANT == "5") %>% 
                                                    count()
  )
  AUX[which(AUX$COD_IBGE == i), 29] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ID_MN_RESI ==i,
                                                           CS_GESTANT == "6") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 30] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ID_MN_RESI ==i,
                                                           CS_GESTANT == "9") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 31] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ID_MN_RESI ==i,
                                                           CS_RACA == "1") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 32] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ID_MN_RESI ==i,
                                                           CS_RACA == "2") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 33] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ID_MN_RESI ==i,
                                                           CS_RACA == "3") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 34] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ID_MN_RESI ==i,
                                                           CS_RACA == "4") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 35] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ID_MN_RESI ==i,
                                                           CS_RACA == "5") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 36] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ID_MN_RESI ==i,
                                                           CS_RACA == "9") %>% 
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2025_GERAL"), AUX)

RS_Serie_Historica_Geral[nrow(RS_Serie_Historica_Geral) +1, ] <- AUX[nrow(AUX),]
RS_Serie_Historica_Geral[nrow(RS_Serie_Historica_Geral), 1] <- "2025"

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2025_GERAL"), AUX), 
           paste0("Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2025_GERAL.csv"), 
           row.names = FALSE)

###############    Tempo entre Picada e Atendimento     #####################

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Até_1H <- NA

AUX$Mais_1H_3H <- NA

AUX$Mais_3H_6H <- NA

AUX$Mais_6H_12H <- NA

AUX$SMais_12H_24 <- NA

AUX$Mais_24H <- NA

AUX$Ignorado <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_TEMPO_ == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_TEMPO_ == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_TEMPO_ == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_TEMPO_ == 4) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_TEMPO_ == 5) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_TEMPO_ == 6) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_TEMPO_ == 9) %>%   
                                                    count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

RS_Serie_Histórica_Tempo_de_Atendimento[nrow(RS_Serie_Histórica_Tempo_de_Atendimento) +1, ] <- AUX[nrow(AUX),]
RS_Serie_Histórica_Tempo_de_Atendimento[nrow(RS_Serie_Histórica_Tempo_de_Atendimento), 1] <- "2025"

assign(paste0("RS", RS, "_PECONHENTOS_2025_TEMPO_ATEND"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2025_TEMPO_ATEND"), AUX), 
           paste0("Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2025_TEMPO_ATEND.csv"), 
           row.names = FALSE)

###############    Local da Picada     #####################

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Cabeça <- NA

AUX$Braço <- NA

AUX$Antebraço <- NA

AUX$Mao <- NA

AUX$Dedo_Mao <- NA

AUX$Tronco <- NA

AUX$Coxa <- NA

AUX$Perna <- NA

AUX$Pe <- NA

AUX$Dedo_pe <- NA

AUX$Ignorado <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_LOCA_1 == "01") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_LOCA_1 == "02") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_LOCA_1 == "03") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_LOCA_1 == "04") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_LOCA_1 == "05") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_LOCA_1 == "06") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_LOCA_1 == "07") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_LOCA_1 == "08") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_LOCA_1 == "09") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_LOCA_1 == 10) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_LOCA_1 == 99) %>%   
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

RS_Serie_Histórica_Local_da_Picada[nrow(RS_Serie_Histórica_Local_da_Picada) +1, ] <- AUX[nrow(AUX),]
RS_Serie_Histórica_Local_da_Picada[nrow(RS_Serie_Histórica_Local_da_Picada), 1] <- "2025"

assign(paste0("RS", RS, "_PECONHENTOS_2025_LOCAL_PICADA"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2025_LOCAL_PICADA"), AUX), 
           paste0("Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2025_LOCAL_PICADA.csv"), 
           row.names = FALSE)

###############    Tipo de acidente     #####################

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Serpente <- NA

AUX$Aranha <- NA

AUX$Escorpiao <- NA

AUX$Lagarta <- NA

AUX$Abelha <- NA

AUX$Outros <- NA

AUX$ignorado <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 4) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 5) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             TP_ACIDENT == 6) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             TP_ACIDENT == 9) %>%   
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

RS_Serie_Historica_Tipo_Acid[nrow(RS_Serie_Historica_Tipo_Acid) +1, ] <- AUX[nrow(AUX),]
RS_Serie_Historica_Tipo_Acid[nrow(RS_Serie_Historica_Tipo_Acid), 1] <- "2025"

RS_PECONHENTOS_2025_Incidencia <- AUX

assign(paste0("RS", RS, "_PECONHENTOS_2025_TIPO_ACID"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2025_TIPO_ACID"), AUX), 
           paste0("Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2025_TIPO_ACID.csv"), 
           row.names = FALSE)

###############    Classificação do Caso (SERPENTE)    #####################

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Leve <- NA

AUX$Moderado <- NA

AUX$Grave <- NA

AUX$Ignorado <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 1,
                                                          TRA_CLASSI == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 1,
                                                          TRA_CLASSI == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 1,
                                                          TRA_CLASSI == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 1,
                                                          TRA_CLASSI == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2025_CLASSI_CASO_SERPENTE"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2025_CLASSI_CASO_SERPENTE"), AUX), 
           paste0("Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2025_CLASSI_CASO_SERPENTE.csv"), 
           row.names = FALSE)

###############    Tipo de Acidente (SERPENTE)    #####################

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Botropico <- NA

AUX$Crotalico <- NA

AUX$Elapidico <- NA

AUX$Laquetico <- NA

AUX$Nao_Peconhenta <- NA

AUX$Ignorado <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 1,
                                                          ANI_SERPEN == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 1,
                                                          ANI_SERPEN == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 1,
                                                          ANI_SERPEN == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 1,
                                                          ANI_SERPEN == 4) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 1,
                                                          ANI_SERPEN == 5) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             TP_ACIDENT == 1,
                                                           ANI_SERPEN == 9) %>%   
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

RS_Serie_Historica_Tipo_Acid_Serpente[nrow(RS_Serie_Historica_Tipo_Acid_Serpente) +1, ] <- AUX[nrow(AUX),]
RS_Serie_Historica_Tipo_Acid_Serpente[nrow(RS_Serie_Historica_Tipo_Acid_Serpente), 1] <- "2025"

assign(paste0("RS", RS, "_PECONHENTOS_2025_TIPO_ACID_SERPENTE"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2025_TIPO_ACID_SERPENTE"), AUX), 
           paste0("Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2025_TIPO_ACID_SERPENTE.csv"), 
           row.names = FALSE)

###############    Uso de Soro (SERPENTE)    #####################

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Sim <- NA

AUX$Nao <- NA

AUX$Ignorado <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 1,
                                                          CON_SOROTE == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 1,
                                                          CON_SOROTE == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 1,
                                                          CON_SOROTE == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2025_SOROTERAPIA_SERPENTE"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2025_SOROTERAPIA_SERPENTE"), AUX), 
           paste0("Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2025_SOROTERAPIA_SERPENTE.csv"), 
           row.names = FALSE)

###############    Complicações locais (SERPENTE)    #####################

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Sim <- NA

AUX$Nao <- NA

AUX$Ignorado <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 1,
                                                          COM_LOC == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 1,
                                                          COM_LOC == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 1,
                                                          COM_LOC == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2025_COMP_LOCAIS_SERPENTE"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2025_COMP_LOCAIS_SERPENTE"), AUX), 
           paste0("Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2025_COMP_LOCAIS_SERPENTE.csv"), 
           row.names = FALSE)

###############    Complicações Sistêmicas (SERPENTE)    #####################

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Sim <- NA

AUX$Nao <- NA

AUX$Ignorado <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 1,
                                                          COM_SISTEM == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 1,
                                                          COM_SISTEM == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 1,
                                                          COM_SISTEM == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2025_COMP_SIST_SERPENTE"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2025_COMP_SIST_SERPENTE"), AUX), 
           paste0("Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2025_COMP_SIST_SERPENTE.csv"), 
           row.names = FALSE)

###############    Manifestações Locais (SERPENTE)    #####################

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Sim <- NA

AUX$Nao <- NA

AUX$Ignorado <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 1,
                                                          MCLI_LOCAL == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 1,
                                                          MCLI_LOCAL == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 1,
                                                          MCLI_LOCAL == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2025_MAN_LOCAIS_SERPENTE"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2025_MAN_LOCAIS_SERPENTE"), AUX), 
           paste0("Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2025_MAN_LOCAIS_SERPENTE.csv"), 
           row.names = FALSE)

###############    Manifestações locais especificadas (SERPENTE)    #####################

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Dor <- NA

AUX$Edema <- NA

AUX$Equimose <- NA

AUX$Necrose <- NA

AUX$Outras <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 1,
                                                          CLI_DOR == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 1,
                                                          CLI_EDEMA == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 1,
                                                          CLI_EQUIMO == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 1,
                                                          CLI_NECROS == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 1,
                                                          CLI_LOCAL_ == 1) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2025_MAN_LOCAIS_ESP_SERPENTE"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2025_MAN_LOCAIS_ESP_SERPENTE"), AUX), 
           paste0("Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2025_MAN_LOCAIS_ESP_SERPENTE.csv"), 
           row.names = FALSE)

###############    Manifestações Sistêmicas (SERPENTE)    #####################

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Sim <- NA

AUX$Nao <- NA

AUX$Ignorado <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 1,
                                                          MCLI_SIST == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 1,
                                                          MCLI_SIST == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 1,
                                                          MCLI_SIST == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2025_MAN_SIST_SERPENTE"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2025_MAN_SIST_SERPENTE"), AUX), 
           paste0("Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2025_MAN_SIST_SERPENTE.csv"), 
           row.names = FALSE)

###############    Manifestações Sistêmicas especificadas (SERPENTE)    #####################

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Neuroparalíticas <- NA

AUX$Hemorragicas <- NA

AUX$Vagais <- NA

AUX$Mio_Hemolíticas <- NA

AUX$Renais <- NA

AUX$Outras <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 1,
                                                          CLI_NEURO == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 1,
                                                          CLI_HEMORR == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 1,
                                                          CLI_VAGAIS == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 1,
                                                          CLI_MIOLIT == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 1,
                                                          CLI_RENAL == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 1,
                                                          CLI_OUTR_2 == 1) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2025_MAN_SIST_ESP_SERPENTE"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2025_MAN_SIST_ESP_SERPENTE"), AUX), 
           paste0("Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2025_MAN_SIST_ESP_SERPENTE.csv"), 
           row.names = FALSE)

###############    Classificação do Caso (ARANHA)    #####################

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Leve <- NA

AUX$Moderado <- NA

AUX$Grave <- NA

AUX$Ignorado <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 2,
                                                          TRA_CLASSI == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 2,
                                                          TRA_CLASSI == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 2,
                                                          TRA_CLASSI == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 2,
                                                          TRA_CLASSI == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2025_CLASSI_CASO_ARANHA"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2025_CLASSI_CASO_ARANHA"), AUX), 
           paste0("Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2025_CLASSI_CASO_ARANHA.csv"), 
           row.names = FALSE)

###############    Tipo de Acidente (ARANHA)    #####################

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Foneutrismo <- NA

AUX$Loxoscelismo <- NA

AUX$Latrodectismo <- NA

AUX$Outra_Aranha <- NA

AUX$Ignorado <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 2,
                                                          ANI_ARANHA == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 2,
                                                          ANI_ARANHA == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 2,
                                                          ANI_ARANHA == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 2,
                                                          ANI_ARANHA == 4) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 2,
                                                          ANI_ARANHA == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

RS_Serie_Historica_Tipo_Acid_Aranha[nrow(RS_Serie_Historica_Tipo_Acid_Aranha) +1, ] <- AUX[nrow(AUX),]
RS_Serie_Historica_Tipo_Acid_Aranha[nrow(RS_Serie_Historica_Tipo_Acid_Aranha), 1] <- "2025"

assign(paste0("RS", RS, "_PECONHENTOS_2025_TIPO_ACID_ARANHA"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2025_TIPO_ACID_ARANHA"), AUX), 
           paste0("Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2025_TIPO_ACID_ARANHA.csv"), 
           row.names = FALSE)

###############    Uso de Soro (ARANHA)    #####################

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Sim <- NA

AUX$Nao <- NA

AUX$Ignorado <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 2,
                                                          CON_SOROTE == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 2,
                                                          CON_SOROTE == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 2,
                                                          CON_SOROTE == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2025_SOROTERAPIA_ARANHA"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2025_SOROTERAPIA_ARANHA"), AUX), 
           paste0("Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2025_SOROTERAPIA_ARANHA.csv"), 
           row.names = FALSE)

###############    Complicações locais (ARANHA)    #####################

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Sim <- NA

AUX$Nao <- NA

AUX$Ignorado <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 2,
                                                          COM_LOC == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 2,
                                                          COM_LOC == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 2,
                                                          COM_LOC == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2025_COMP_LOCAIS_ARANHA"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2025_COMP_LOCAIS_ARANHA"), AUX), 
           paste0("Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2025_COMP_LOCAIS_ARANHA.csv"), 
           row.names = FALSE)

###############    Complicações Sistêmicas (ARANHA)    #####################

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Sim <- NA

AUX$Nao <- NA

AUX$Ignorado <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 2,
                                                          COM_SISTEM == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 2,
                                                          COM_SISTEM == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 2,
                                                          COM_SISTEM == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2025_COMP_SIST_ARANHA"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2025_COMP_SIST_ARANHA"), AUX), 
           paste0("Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2025_COMP_SIST_ARANHA.csv"), 
           row.names = FALSE)

###############    Manifestações Locais (ARANHA)    #####################

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Sim <- NA

AUX$Nao <- NA

AUX$Ignorado <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 2,
                                                          MCLI_LOCAL == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 2,
                                                          MCLI_LOCAL == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 2,
                                                          MCLI_LOCAL == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2025_MAN_LOCAIS_ARANHA"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2025_MAN_LOCAIS_ARANHA"), AUX), 
           paste0("Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2025_MAN_LOCAIS_ARANHA.csv"), 
           row.names = FALSE)

###############    Manifestações locais especificadas (ARANHA)    #####################

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Dor <- NA

AUX$Edema <- NA

AUX$Equimose <- NA

AUX$Necrose <- NA

AUX$Outras <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 2,
                                                          CLI_DOR == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 2,
                                                          CLI_EDEMA == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 2,
                                                          CLI_EQUIMO == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 2,
                                                          CLI_NECROS == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 2,
                                                          CLI_LOCAL_ == 1) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2025_MAN_LOCAIS_ESP_ARANHA"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2025_MAN_LOCAIS_ESP_ARANHA"), AUX), 
           paste0("Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2025_MAN_LOCAIS_ESP_ARANHA.csv"), 
           row.names = FALSE)

###############    Manifestações Sistêmicas (ARANHA)    #####################

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Sim <- NA

AUX$Nao <- NA

AUX$Ignorado <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 2,
                                                          MCLI_SIST == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 2,
                                                          MCLI_SIST == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 2,
                                                          MCLI_SIST == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2025_MAN_SIST_ARANHA"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2025_MAN_SIST_ARANHA"), AUX), 
           paste0("Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2025_MAN_SIST_ARANHA.csv"), 
           row.names = FALSE)

###############    Manifestações Sistêmicas especificadas (ARANHA)    #####################

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Neuroparalíticas <- NA

AUX$Hemorragicas <- NA

AUX$Vagais <- NA

AUX$Mio_Hemolíticas <- NA

AUX$Renais <- NA

AUX$Outras <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 2,
                                                          CLI_NEURO == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 2,
                                                          CLI_HEMORR == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 2,
                                                          CLI_VAGAIS == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 2,
                                                          CLI_MIOLIT == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 2,
                                                          CLI_RENAL == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 2,
                                                          CLI_OUTR_2 == 1) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2025_MAN_SIST_ESP_ARANHA"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2025_MAN_SIST_ESP_ARANHA"), AUX), 
           paste0("Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2025_MAN_SIST_ESP_ARANHA.csv"), 
           row.names = FALSE)

###############    Classificação do Caso (ESCORPIAO)    #####################

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Leve <- NA

AUX$Moderado <- NA

AUX$Grave <- NA

AUX$Ignorado <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 3,
                                                          TRA_CLASSI == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 3,
                                                          TRA_CLASSI == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 3,
                                                          TRA_CLASSI == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 3,
                                                          TRA_CLASSI == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2025_CLASSI_CASO_ESCORPIAO"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2025_CLASSI_CASO_ESCORPIAO"), AUX), 
           paste0("Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2025_CLASSI_CASO_ESCORPIAO.csv"), 
           row.names = FALSE)

###############    Uso de Soro (ESCORPIAO)    #####################

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Sim <- NA

AUX$Nao <- NA

AUX$Ignorado <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 3,
                                                          CON_SOROTE == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 3,
                                                          CON_SOROTE == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 3,
                                                          CON_SOROTE == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2025_SOROTERAPIA_ESCORPIAO"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2025_SOROTERAPIA_ESCORPIAO"), AUX), 
           paste0("Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2025_SOROTERAPIA_ESCORPIAO.csv"), 
           row.names = FALSE)

###############    Complicações locais (ESCORPIAO)    #####################

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Sim <- NA

AUX$Nao <- NA

AUX$Ignorado <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 3,
                                                          COM_LOC == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 3,
                                                          COM_LOC == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 3,
                                                          COM_LOC == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2025_COMP_LOCAIS_ESCORPIAO"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2025_COMP_LOCAIS_ESCORPIAO"), AUX), 
           paste0("Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2025_COMP_LOCAIS_ESCORPIAO.csv"), 
           row.names = FALSE)

###############    Complicações Sistêmicas (ESCORPIAO)    #####################

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Sim <- NA

AUX$Nao <- NA

AUX$Ignorado <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 3,
                                                          COM_SISTEM == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 3,
                                                          COM_SISTEM == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 3,
                                                          COM_SISTEM == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2025_COMP_SIST_ESCORPIAO"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2025_COMP_SIST_ESCORPIAO"), AUX), 
           paste0("Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2025_COMP_SIST_ESCORPIAO.csv"), 
           row.names = FALSE)

###############    Manifestações Locais (ESCORPIAO)    #####################

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Sim <- NA

AUX$Nao <- NA

AUX$Ignorado <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 3,
                                                          MCLI_LOCAL == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 3,
                                                          MCLI_LOCAL == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 3,
                                                          MCLI_LOCAL == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2025_MAN_LOCAIS_ESCORPIAO"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2025_MAN_LOCAIS_ESCORPIAO"), AUX), 
           paste0("Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2025_MAN_LOCAIS_ESCORPIAO.csv"), 
           row.names = FALSE)

###############    Manifestações locais especificadas (ESCORPIAO)    #####################

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Dor <- NA

AUX$Edema <- NA

AUX$Equimose <- NA

AUX$Necrose <- NA

AUX$Outras <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 3,
                                                          CLI_DOR == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 3,
                                                          CLI_EDEMA == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 3,
                                                          CLI_EQUIMO == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 3,
                                                          CLI_NECROS == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 3,
                                                          CLI_LOCAL_ == 1) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2025_MAN_LOCAIS_ESP_ESCORPIAO"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2025_MAN_LOCAIS_ESP_ESCORPIAO"), AUX), 
           paste0("Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2025_MAN_LOCAIS_ESP_ESCORPIAO.csv"), 
           row.names = FALSE)

###############    Manifestações Sistêmicas (ESCORPIAO)    #####################

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Sim <- NA

AUX$Nao <- NA

AUX$Ignorado <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 3,
                                                          MCLI_SIST == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 3,
                                                          MCLI_SIST == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 3,
                                                          MCLI_SIST == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2025_MAN_SIST_ESCORPIAO"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2025_MAN_SIST_ESCORPIAO"), AUX), 
           paste0("Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2025_MAN_SIST_ESCORPIAO.csv"), 
           row.names = FALSE)

###############    Manifestações Sistêmicas especificadas (ESCORPIAO)    #####################

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Neuroparalíticas <- NA

AUX$Hemorragicas <- NA

AUX$Vagais <- NA

AUX$Mio_Hemolíticas <- NA

AUX$Renais <- NA

AUX$Outras <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 3,
                                                          CLI_NEURO == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 3,
                                                          CLI_HEMORR == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 3,
                                                          CLI_VAGAIS == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 3,
                                                          CLI_MIOLIT == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 3,
                                                          CLI_RENAL == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 3,
                                                          CLI_OUTR_2 == 1) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2025_MAN_SIST_ESP_ESCORPIAO"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2025_MAN_SIST_ESP_ESCORPIAO"), AUX), 
           paste0("Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2025_MAN_SIST_ESP_ESCORPIAO.csv"), 
           row.names = FALSE)


###############    Classificação do Caso (LAGARTA)    #####################

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Leve <- NA

AUX$Moderado <- NA

AUX$Grave <- NA

AUX$Ignorado <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 4,
                                                          TRA_CLASSI == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 4,
                                                          TRA_CLASSI == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 4,
                                                          TRA_CLASSI == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 4,
                                                          TRA_CLASSI == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2025_CLASSI_CASO_LAGARTA"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2025_CLASSI_CASO_LAGARTA"), AUX), 
           paste0("Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2025_CLASSI_CASO_LAGARTA.csv"), 
           row.names = FALSE)

###############    Tipo de Acidente (LAGARTA)    #####################

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Lonomia <- NA

AUX$Outra_Lagarta <- NA

AUX$Ignorado <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 4,
                                                          ANI_LAGART == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 4,
                                                          ANI_LAGART == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 4,
                                                          ANI_LAGART == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2025_TIPO_ACID_LAGARTA"), AUX)

RS_Serie_Historica_Tipo_Acid_Lagarta[nrow(RS_Serie_Historica_Tipo_Acid_Lagarta) +1, ] <- AUX[nrow(AUX),]
RS_Serie_Historica_Tipo_Acid_Lagarta[nrow(RS_Serie_Historica_Tipo_Acid_Lagarta), 1] <- "2025"

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2025_TIPO_ACID_LAGARTA"), AUX), 
           paste0("Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2025_TIPO_ACID_LAGARTA.csv"), 
           row.names = FALSE)

###############    Uso de Soro (LAGARTA)    #####################

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Sim <- NA

AUX$Nao <- NA

AUX$Ignorado <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 4,
                                                          CON_SOROTE == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 4,
                                                          CON_SOROTE == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 4,
                                                          CON_SOROTE == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2025_SOROTERAPIA_LAGARTA"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2025_SOROTERAPIA_LAGARTA"), AUX), 
           paste0("Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2025_SOROTERAPIA_LAGARTA.csv"), 
           row.names = FALSE)

###############    Complicações locais (LAGARTA)    #####################

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Sim <- NA

AUX$Nao <- NA

AUX$Ignorado <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 4,
                                                          COM_LOC == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 4,
                                                          COM_LOC == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 4,
                                                          COM_LOC == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2025_COMP_LOCAIS_LAGARTA"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2025_COMP_LOCAIS_LAGARTA"), AUX), 
           paste0("Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2025_COMP_LOCAIS_LAGARTA.csv"), 
           row.names = FALSE)

###############    Complicações Sistêmicas (LAGARTA)    #####################

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Sim <- NA

AUX$Nao <- NA

AUX$Ignorado <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 4,
                                                          COM_SISTEM == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 4,
                                                          COM_SISTEM == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 4,
                                                          COM_SISTEM == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2025_COMP_SIST_LAGARTA"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2025_COMP_SIST_LAGARTA"), AUX), 
           paste0("Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2025_COMP_SIST_LAGARTA.csv"), 
           row.names = FALSE)

###############    Manifestações Locais (LAGARTA)    #####################

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Sim <- NA

AUX$Nao <- NA

AUX$Ignorado <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 4,
                                                          MCLI_LOCAL == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 4,
                                                          MCLI_LOCAL == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 4,
                                                          MCLI_LOCAL == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2025_MAN_LOCAIS_LAGARTA"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2025_MAN_LOCAIS_LAGARTA"), AUX), 
           paste0("Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2025_MAN_LOCAIS_LAGARTA.csv"), 
           row.names = FALSE)

###############    Manifestações locais especificadas (LAGARTA)    #####################

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Dor <- NA

AUX$Edema <- NA

AUX$Equimose <- NA

AUX$Necrose <- NA

AUX$Outras <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 4,
                                                          CLI_DOR == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 4,
                                                          CLI_EDEMA == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 4,
                                                          CLI_EQUIMO == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 4,
                                                          CLI_NECROS == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 4,
                                                          CLI_LOCAL_ == 1) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2025_MAN_LOCAIS_ESP_LAGARTA"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2025_MAN_LOCAIS_ESP_LAGARTA"), AUX), 
           paste0("Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2025_MAN_LOCAIS_ESP_LAGARTA.csv"), 
           row.names = FALSE)

###############    Manifestações Sistêmicas (LAGARTA)    #####################

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Sim <- NA

AUX$Nao <- NA

AUX$Ignorado <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 4,
                                                          MCLI_SIST == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 4,
                                                          MCLI_SIST == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 4,
                                                          MCLI_SIST == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2025_MAN_SIST_LAGARTA"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2025_MAN_SIST_LAGARTA"), AUX), 
           paste0("Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2025_MAN_SIST_LAGARTA.csv"), 
           row.names = FALSE)

###############    Manifestações Sistêmicas especificadas (LAGARTA)    #####################

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Neuroparalíticas <- NA

AUX$Hemorragicas <- NA

AUX$Vagais <- NA

AUX$Mio_Hemolíticas <- NA

AUX$Renais <- NA

AUX$Outras <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 4,
                                                          CLI_NEURO == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 4,
                                                          CLI_HEMORR == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 4,
                                                          CLI_VAGAIS == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 4,
                                                          CLI_MIOLIT == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 4,
                                                          CLI_RENAL == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 4,
                                                          CLI_OUTR_2 == 1) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2025_MAN_SIST_ESP_LAGARTA"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2025_MAN_SIST_ESP_LAGARTA"), AUX), 
           paste0("Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2025_MAN_SIST_ESP_LAGARTA.csv"), 
           row.names = FALSE)

###############    Classificação do Caso (ABELHA)    #####################

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Leve <- NA

AUX$Moderado <- NA

AUX$Grave <- NA

AUX$Ignorado <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 5,
                                                          TRA_CLASSI == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 5,
                                                          TRA_CLASSI == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 5,
                                                          TRA_CLASSI == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 5,
                                                          TRA_CLASSI == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2025_CLASSI_CASO_ABELHA"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2025_CLASSI_CASO_ABELHA"), AUX), 
           paste0("Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2025_CLASSI_CASO_ABELHA.csv"), 
           row.names = FALSE)

###############    Uso de Soro (ABELHA)    #####################

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Sim <- NA

AUX$Nao <- NA

AUX$Ignorado <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 5,
                                                          CON_SOROTE == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 5,
                                                          CON_SOROTE == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 5,
                                                          CON_SOROTE == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2025_SOROTERAPIA_ABELHA"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2025_SOROTERAPIA_ABELHA"), AUX), 
           paste0("Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2025_SOROTERAPIA_ABELHA.csv"), 
           row.names = FALSE)

###############    Complicações locais (ABELHA)    #####################

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Sim <- NA

AUX$Nao <- NA

AUX$Ignorado <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 5,
                                                          COM_LOC == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 5,
                                                          COM_LOC == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 5,
                                                          COM_LOC == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2025_COMP_LOCAIS_ABELHA"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2025_COMP_LOCAIS_ABELHA"), AUX), 
           paste0("Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2025_COMP_LOCAIS_ABELHA.csv"), 
           row.names = FALSE)

###############    Complicações Sistêmicas (ABELHA)    #####################

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Sim <- NA

AUX$Nao <- NA

AUX$Ignorado <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 5,
                                                          COM_SISTEM == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 5,
                                                          COM_SISTEM == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 5,
                                                          COM_SISTEM == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2025_COMP_SIST_ABELHA"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2025_COMP_SIST_ABELHA"), AUX), 
           paste0("Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2025_COMP_SIST_ABELHA.csv"), 
           row.names = FALSE)

###############    Manifestações Locais (ABELHA)    #####################

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Sim <- NA

AUX$Nao <- NA

AUX$Ignorado <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 5,
                                                          MCLI_LOCAL == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 5,
                                                          MCLI_LOCAL == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 5,
                                                          MCLI_LOCAL == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2025_MAN_LOCAIS_ABELHA"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2025_MAN_LOCAIS_ABELHA"), AUX), 
           paste0("Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2025_MAN_LOCAIS_ABELHA.csv"), 
           row.names = FALSE)

###############    Manifestações locais especificadas (ABELHA)    #####################

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Dor <- NA

AUX$Edema <- NA

AUX$Equimose <- NA

AUX$Necrose <- NA

AUX$Outras <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 5,
                                                          CLI_DOR == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 5,
                                                          CLI_EDEMA == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 5,
                                                          CLI_EQUIMO == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 5,
                                                          CLI_NECROS == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 5,
                                                          CLI_LOCAL_ == 1) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2025_MAN_LOCAIS_ESP_ABELHA"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2025_MAN_LOCAIS_ESP_ABELHA"), AUX), 
           paste0("Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2025_MAN_LOCAIS_ESP_ABELHA.csv"), 
           row.names = FALSE)

###############    Manifestações Sistêmicas (ABELHA)    #####################

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Sim <- NA

AUX$Nao <- NA

AUX$Ignorado <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 5,
                                                          MCLI_SIST == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 5,
                                                          MCLI_SIST == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 5,
                                                          MCLI_SIST == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2025_MAN_SIST_ABELHA"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2025_MAN_SIST_ABELHA"), AUX), 
           paste0("Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2025_MAN_SIST_ABELHA.csv"), 
           row.names = FALSE)

###############    Manifestações Sistêmicas especificadas (ABELHA)    #####################

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Neuroparalíticas <- NA

AUX$Hemorragicas <- NA

AUX$Vagais <- NA

AUX$Mio_Hemolíticas <- NA

AUX$Renais <- NA

AUX$Outras <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 5,
                                                          CLI_NEURO == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 5,
                                                          CLI_HEMORR == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 5,
                                                          CLI_VAGAIS == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 5,
                                                          CLI_MIOLIT == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 5,
                                                          CLI_RENAL == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 5,
                                                          CLI_OUTR_2 == 1) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2025_MAN_SIST_ESP_ABELHA"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2025_MAN_SIST_ESP_ABELHA"), AUX), 
           paste0("Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2025_MAN_SIST_ESP_ABELHA.csv"), 
           row.names = FALSE)
########################################################################################################################################
#####################################   FIM FIM FIM FIM FIM FIM FIM #####################################################################
#########################################################################################################################################

############################    Gráficos e Mapas    ##################################################################################

###############################       Canais Endêmicos    ############################################################################

#####################################   NOTIFICADOS      #############################################################################

CE_BASE_Notificados[(nrow(CE_BASE_Notificados) +1), 1] <- "2025"
CE_BASE_Notificados[nrow(CE_BASE_Notificados), 2:54] <- as.integer(data.frame(RS22_PECONHENTOS_2025_SE_Notificados[nrow(RS22_PECONHENTOS_2025_SE_Notificados), 2:54]))

#####Utilizando objetos auxiliares porque se transpor o data frame direto ele transforma as variáveis em#############
#####caracter.            NÃO FOI DESCARTADO AINDA OS PERÍODOS EPIDÊMICOS                               #############
##### VERIFICAR SE PODE-SE UTILIZAR A MÉDIA COMO LIMITE INFERIOR.                                       #############

AUX <- CE_BASE_Notificados[, -1]

AUX <- t(AUX)

AUX2 <- CE_BASE_Notificados[, 1]

colnames(AUX) <- AUX2

CE_BASE_Notificados <- as.data.frame(AUX)

CE_BASE_Notificados <- CE_BASE_Notificados[, Anos_Analise]

######Criando a coluna de média no data.frame#####################

CE_BASE_Notificados <- CE_BASE_Notificados %>%
  mutate(Mediana = apply(CE_BASE_Notificados[,1: (ncol(CE_BASE_Notificados)-1)], 1 , median))

######Criando a coluna de Desvio Padrão no data frame###############

CE_BASE_Notificados <- CE_BASE_Notificados %>%
  mutate(Desvio_Padrao = apply(CE_BASE_Notificados[,1: (ncol(CE_BASE_Notificados) -2)], 1 , sd))

###### Criando a coluna de Média + 2(DP)

CE_BASE_Notificados <- CE_BASE_Notificados %>%
  mutate(Lim_Superior = (Mediana + 1.96 * Desvio_Padrao))

CE_BASE_Notificados[, (ncol(CE_BASE_Notificados)+1)] <- rownames(CE_BASE_Notificados)

CE_BASE_Notificados <- CE_BASE_Notificados[, c(ncol(CE_BASE_Notificados), 1:(ncol(CE_BASE_Notificados) -1))]

CE_BASE_Notificados[,1] <- c(1:53)

colnames(CE_BASE_Notificados)[1] <- "Semana_Epidemiológica"

rownames(CE_BASE_Notificados) <- c(1:nrow(CE_BASE_Notificados))

write.csv (CE_BASE_Notificados, 
           paste0("Tabulacoes_R/Peconhentos/RS", RS, "_CE_Notificados.csv"), 
           row.names = FALSE)

AUX_GRAF <- CE_BASE_Notificados[, c(ncol(CE_BASE_Notificados), ncol(CE_BASE_Notificados) -1, ncol(CE_BASE_Notificados) -2)]

###Criando uma coluna de ordem das se para o R não colocar em ordem numérica.

AUX_GRAF$Ordem <- c(1: nrow(CE_BASE_Notificados))

###Puxando o período sazonal atual para o gráfico de linhas

AUX_GRAF$`2025` <- CE_BASE_Notificados$`2025`

AUX_GRAF$Sem_EPI <-as.character(c("01",  "02", "03",  "04",  "05",  "06",  "07", 
                                  "08",  "09",  "10",  "11",  "12",  "13",  "14",  
                                  "15",  "16",  "17",  "18",  "19",  "20",  "21",  
                                  "22",  "23",  "24",  "25",  "26",  "27",  "28",  
                                  "28",  "30",  "31",  "32",  "33", "34",  "35",  
                                  "36",  "37",  "38",  "39",  "40",  "41",  "42",  
                                  "43",  "44",  "45",  "46",  "47",  "48",  "49",  
                                  "50",  "51",  "52",  "53"))

assign(paste0("RS", RS, "_GRAF_CE_Notificados"), ggplot(AUX_GRAF, aes(Ordem))  +
         theme(axis.text.x = element_text(angle = 90, 
                                          vjust = .5,
                                          face = "bold",
                                          size = 12)) +
         labs(caption = Fonte,
              title = "Canal Endêmico Acidentes com Animais Peçonhentos",
              subtitle = paste0("Casos Notificados - ", RS, "ª Regional de Saúde")) +
         theme(
           panel.grid.major = element_line(color = "#C0C0C0"),
           panel.grid.minor = element_blank(),
           panel.background = element_rect(fill = "#B22222"),
           plot.title = element_text(face = "bold",
                                     size = 24,
                                     colour = "#1C1C1C"),
           plot.caption = element_text(hjust = 0, size = 12)
         ) +
         geom_area(aes(y = Lim_Superior), fill = "#F0E68C",alpha = 0.9) +
         geom_area(aes( y = Mediana), fill = "#556B2F") +
         geom_line(aes( y = `2025`), stat = "identity", color = "black", linewidth = 1.5) +
         xlab("Semana Epidemiológica") +
         ylab("Número de Casos") +
         scale_x_continuous(breaks = c(1:53), label = AUX_GRAF$Sem_EPI) +
         scale_y_continuous(expand = expansion(mult = c(0, 0.05))))

ggsave("/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/PECONHENTOS/Canal_Endemico_PECONHENTOS.png", 
       plot = RS22_GRAF_CE_Notificados,     
       width = 50,             
       height = 15,           
       units = "cm",           
       dpi = 300) 

#####################################   SERPENTES      #############################################################################

CE_BASE_SERPENTES_Notificados[(nrow(CE_BASE_SERPENTES_Notificados) +1), 1] <- "2025"
CE_BASE_SERPENTES_Notificados[nrow(CE_BASE_SERPENTES_Notificados), 2:54] <- as.integer(data.frame(RS22_PECONHENTOS_2025_SE_SERPENTES_Notificados[nrow(RS22_PECONHENTOS_2025_SE_Notificados), 2:54]))

#####Utilizando objetos auxiliares porque se transpor o data frame direto ele transforma as variáveis em#############
#####caracter.            NÃO FOI DESCARTADO AINDA OS PERÍODOS EPIDÊMICOS                               #############
##### VERIFICAR SE PODE-SE UTILIZAR A MÉDIA COMO LIMITE INFERIOR.                                       #############

AUX <- CE_BASE_SERPENTES_Notificados[, -1]

AUX <- t(AUX)

AUX2 <- CE_BASE_SERPENTES_Notificados[, 1]

colnames(AUX) <- AUX2

CE_BASE_SERPENTES_Notificados <- as.data.frame(AUX)

CE_BASE_SERPENTES_Notificados <- CE_BASE_SERPENTES_Notificados[, Anos_Analise]

######Criando a coluna de média no data.frame#####################

CE_BASE_SERPENTES_Notificados <- CE_BASE_SERPENTES_Notificados %>%
  mutate(Mediana = apply(CE_BASE_SERPENTES_Notificados[,1: (ncol(CE_BASE_SERPENTES_Notificados)-1)], 1 , median))

######Criando a coluna de Desvio Padrão no data frame###############

CE_BASE_SERPENTES_Notificados <- CE_BASE_SERPENTES_Notificados %>%
  mutate(Desvio_Padrao = apply(CE_BASE_SERPENTES_Notificados[,1: (ncol(CE_BASE_SERPENTES_Notificados) -2)], 1 , sd))

###### Criando a coluna de Média + 2(DP)

CE_BASE_SERPENTES_Notificados <- CE_BASE_SERPENTES_Notificados %>%
  mutate(Lim_Superior = (Mediana + 1.96 * Desvio_Padrao))

CE_BASE_SERPENTES_Notificados[, (ncol(CE_BASE_SERPENTES_Notificados)+1)] <- rownames(CE_BASE_SERPENTES_Notificados)

CE_BASE_SERPENTES_Notificados <- CE_BASE_SERPENTES_Notificados[, c(ncol(CE_BASE_SERPENTES_Notificados), 1:(ncol(CE_BASE_SERPENTES_Notificados) -1))]

CE_BASE_SERPENTES_Notificados[,1] <- c(1:53)

colnames(CE_BASE_SERPENTES_Notificados)[1] <- "Semana_Epidemiológica"

rownames(CE_BASE_SERPENTES_Notificados) <- c(1:nrow(CE_BASE_SERPENTES_Notificados))

write.csv (CE_BASE_SERPENTES_Notificados, 
           paste0("Tabulacoes_R/Peconhentos/RS", RS, "_CE_Notificados.csv"), 
           row.names = FALSE)

AUX_GRAF <- CE_BASE_SERPENTES_Notificados[, c(ncol(CE_BASE_SERPENTES_Notificados), ncol(CE_BASE_SERPENTES_Notificados) -1, ncol(CE_BASE_SERPENTES_Notificados) -2)]

###Criando uma coluna de ordem das se para o R não colocar em ordem numérica.

AUX_GRAF$Ordem <- c(1: nrow(CE_BASE_SERPENTES_Notificados))

###Puxando o período sazonal atual para o gráfico de linhas

AUX_GRAF$`2025` <- CE_BASE_SERPENTES_Notificados$`2025`

AUX_GRAF$Sem_EPI <-as.character(c("01",  "02", "03",  "04",  "05",  "06",  "07", 
                                  "08",  "09",  "10",  "11",  "12",  "13",  "14",  
                                  "15",  "16",  "17",  "18",  "19",  "20",  "21",  
                                  "22",  "23",  "24",  "25",  "26",  "27",  "28",  
                                  "28",  "30",  "31",  "32",  "33", "34",  "35",  
                                  "36",  "37",  "38",  "39",  "40",  "41",  "42",  
                                  "43",  "44",  "45",  "46",  "47",  "48",  "49",  
                                  "50",  "51",  "52",  "53"))

assign(paste0("RS", RS, "_GRAF_CE_SERPENTES_Notificados"), ggplot(AUX_GRAF, aes(Ordem))  +
         theme(axis.text.x = element_text(angle = 90, 
                                          vjust = .5,
                                          face = "bold",
                                          size = 12)) +
         labs(caption = Fonte,
              title = "Canal Endêmico Acidentes com SERPENTES",
              subtitle = paste0("Casos Notificados - ", RS, "ª Regional de Saúde")) +
         theme(
           panel.grid.major = element_line(color = "#C0C0C0"),
           panel.grid.minor = element_blank(),
           panel.background = element_rect(fill = "#B22222"),
           plot.title = element_text(face = "bold",
                                     size = 24,
                                     colour = "#1C1C1C"),
           plot.caption = element_text(hjust = 0, size = 12)
         ) +
         geom_area(aes(y = Lim_Superior), fill = "#F0E68C",alpha = 0.9) +
         geom_area(aes( y = Mediana), fill = "#556B2F") +
         geom_line(aes( y = `2025`), stat = "identity", color = "black", linewidth = 1.5) +
         xlab("Semana Epidemiológica") +
         ylab("Número de Casos") +
         scale_x_continuous(breaks = c(1:53), label = AUX_GRAF$Sem_EPI) +
         scale_y_continuous(expand = expansion(mult = c(0, 0.05))))

ggsave("/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/PECONHENTOS/Canal_Endemico_SERPENTES_PECONHENTOS.png", 
       plot = RS22_GRAF_CE_SERPENTES_Notificados,     
       width = 50,             
       height = 15,           
       units = "cm",           
       dpi = 300) 

#####################################   ARANHAS      #############################################################################

CE_BASE_ARANHAS_Notificados[(nrow(CE_BASE_ARANHAS_Notificados) +1), 1] <- "2025"
CE_BASE_ARANHAS_Notificados[nrow(CE_BASE_ARANHAS_Notificados), 2:54] <- as.integer(data.frame(RS22_PECONHENTOS_2025_SE_ARANHAS_Notificados[nrow(RS22_PECONHENTOS_2025_SE_Notificados), 2:54]))

#####Utilizando objetos auxiliares porque se transpor o data frame direto ele transforma as variáveis em#############
#####caracter.            NÃO FOI DESCARTADO AINDA OS PERÍODOS EPIDÊMICOS                               #############
##### VERIFICAR SE PODE-SE UTILIZAR A MÉDIA COMO LIMITE INFERIOR.                                       #############

AUX <- CE_BASE_ARANHAS_Notificados[, -1]

AUX <- t(AUX)

AUX2 <- CE_BASE_ARANHAS_Notificados[, 1]

colnames(AUX) <- AUX2

CE_BASE_ARANHAS_Notificados <- as.data.frame(AUX)

CE_BASE_ARANHAS_Notificados <- CE_BASE_ARANHAS_Notificados[, Anos_Analise]

######Criando a coluna de média no data.frame#####################

CE_BASE_ARANHAS_Notificados <- CE_BASE_ARANHAS_Notificados %>%
  mutate(Mediana = apply(CE_BASE_ARANHAS_Notificados[,1: (ncol(CE_BASE_ARANHAS_Notificados)-1)], 1 , median))

######Criando a coluna de Desvio Padrão no data frame###############

CE_BASE_ARANHAS_Notificados <- CE_BASE_ARANHAS_Notificados %>%
  mutate(Desvio_Padrao = apply(CE_BASE_ARANHAS_Notificados[,1: (ncol(CE_BASE_ARANHAS_Notificados) -2)], 1 , sd))

###### Criando a coluna de Média + 2(DP)

CE_BASE_ARANHAS_Notificados <- CE_BASE_ARANHAS_Notificados %>%
  mutate(Lim_Superior = (Mediana + 1.96 * Desvio_Padrao))

CE_BASE_ARANHAS_Notificados[, (ncol(CE_BASE_ARANHAS_Notificados)+1)] <- rownames(CE_BASE_ARANHAS_Notificados)

CE_BASE_ARANHAS_Notificados <- CE_BASE_ARANHAS_Notificados[, c(ncol(CE_BASE_ARANHAS_Notificados), 1:(ncol(CE_BASE_ARANHAS_Notificados) -1))]

CE_BASE_ARANHAS_Notificados[,1] <- c(1:53)

colnames(CE_BASE_ARANHAS_Notificados)[1] <- "Semana_Epidemiológica"

rownames(CE_BASE_ARANHAS_Notificados) <- c(1:nrow(CE_BASE_ARANHAS_Notificados))

write.csv (CE_BASE_ARANHAS_Notificados, 
           paste0("Tabulacoes_R/Peconhentos/RS", RS, "_CE_Notificados.csv"), 
           row.names = FALSE)

AUX_GRAF <- CE_BASE_ARANHAS_Notificados[, c(ncol(CE_BASE_ARANHAS_Notificados), ncol(CE_BASE_ARANHAS_Notificados) -1, ncol(CE_BASE_ARANHAS_Notificados) -2)]

###Criando uma coluna de ordem das se para o R não colocar em ordem numérica.

AUX_GRAF$Ordem <- c(1: nrow(CE_BASE_ARANHAS_Notificados))

###Puxando o período sazonal atual para o gráfico de linhas

AUX_GRAF$`2025` <- CE_BASE_ARANHAS_Notificados$`2025`

AUX_GRAF$Sem_EPI <-as.character(c("01",  "02", "03",  "04",  "05",  "06",  "07", 
                                  "08",  "09",  "10",  "11",  "12",  "13",  "14",  
                                  "15",  "16",  "17",  "18",  "19",  "20",  "21",  
                                  "22",  "23",  "24",  "25",  "26",  "27",  "28",  
                                  "28",  "30",  "31",  "32",  "33", "34",  "35",  
                                  "36",  "37",  "38",  "39",  "40",  "41",  "42",  
                                  "43",  "44",  "45",  "46",  "47",  "48",  "49",  
                                  "50",  "51",  "52",  "53"))

assign(paste0("RS", RS, "_GRAF_CE_ARANHAS_Notificados"), ggplot(AUX_GRAF, aes(Ordem))  +
         theme(axis.text.x = element_text(angle = 90, 
                                          vjust = .5,
                                          face = "bold",
                                          size = 12)) +
         labs(caption = Fonte,
              title = "Canal Endêmico Acidentes com ARANHAS",
              subtitle = paste0("Casos Notificados - ", RS, "ª Regional de Saúde")) +
         theme(
           panel.grid.major = element_line(color = "#C0C0C0"),
           panel.grid.minor = element_blank(),
           panel.background = element_rect(fill = "#B22222"),
           plot.title = element_text(face = "bold",
                                     size = 24,
                                     colour = "#1C1C1C"),
           plot.caption = element_text(hjust = 0, size = 12)
         ) +
         geom_area(aes(y = Lim_Superior), fill = "#F0E68C",alpha = 0.9) +
         geom_area(aes( y = Mediana), fill = "#556B2F") +
         geom_line(aes( y = `2025`), stat = "identity", color = "black", linewidth = 1.5) +
         xlab("Semana Epidemiológica") +
         ylab("Número de Casos") +
         scale_x_continuous(breaks = c(1:53), label = AUX_GRAF$Sem_EPI) +
         scale_y_continuous(expand = expansion(mult = c(0, 0.05))))

ggsave("/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/PECONHENTOS/Canal_Endemico_ARANHAS_PECONHENTOS.png", 
       plot = RS22_GRAF_CE_ARANHAS_Notificados,     
       width = 50,             
       height = 15,           
       units = "cm",           
       dpi = 300) 

#####################################   ESCORPIÕES     #############################################################################

CE_BASE_ESCORPIOES_Notificados[(nrow(CE_BASE_ESCORPIOES_Notificados) +1), 1] <- "2025"
CE_BASE_ESCORPIOES_Notificados[nrow(CE_BASE_ESCORPIOES_Notificados), 2:54] <- as.integer(data.frame(RS22_PECONHENTOS_2025_SE_ESCORPIOES_Notificados[nrow(RS22_PECONHENTOS_2025_SE_Notificados), 2:54]))

#####Utilizando objetos auxiliares porque se transpor o data frame direto ele transforma as variáveis em#############
#####caracter.            NÃO FOI DESCARTADO AINDA OS PERÍODOS EPIDÊMICOS                               #############
##### VERIFICAR SE PODE-SE UTILIZAR A MÉDIA COMO LIMITE INFERIOR.                                       #############

AUX <- CE_BASE_ESCORPIOES_Notificados[, -1]

AUX <- t(AUX)

AUX2 <- CE_BASE_ESCORPIOES_Notificados[, 1]

colnames(AUX) <- AUX2

CE_BASE_ESCORPIOES_Notificados <- as.data.frame(AUX)

CE_BASE_ESCORPIOES_Notificados <- CE_BASE_ESCORPIOES_Notificados[, Anos_Analise]

######Criando a coluna de média no data.frame#####################

CE_BASE_ESCORPIOES_Notificados <- CE_BASE_ESCORPIOES_Notificados %>%
  mutate(Mediana = apply(CE_BASE_ESCORPIOES_Notificados[,1: (ncol(CE_BASE_ESCORPIOES_Notificados)-1)], 1 , median))

######Criando a coluna de Desvio Padrão no data frame###############

CE_BASE_ESCORPIOES_Notificados <- CE_BASE_ESCORPIOES_Notificados %>%
  mutate(Desvio_Padrao = apply(CE_BASE_ESCORPIOES_Notificados[,1: (ncol(CE_BASE_ESCORPIOES_Notificados) -2)], 1 , sd))

###### Criando a coluna de Média + 2(DP)

CE_BASE_ESCORPIOES_Notificados <- CE_BASE_ESCORPIOES_Notificados %>%
  mutate(Lim_Superior = (Mediana + 1.96 * Desvio_Padrao))

CE_BASE_ESCORPIOES_Notificados[, (ncol(CE_BASE_ESCORPIOES_Notificados)+1)] <- rownames(CE_BASE_ESCORPIOES_Notificados)

CE_BASE_ESCORPIOES_Notificados <- CE_BASE_ESCORPIOES_Notificados[, c(ncol(CE_BASE_ESCORPIOES_Notificados), 1:(ncol(CE_BASE_ESCORPIOES_Notificados) -1))]

CE_BASE_ESCORPIOES_Notificados[,1] <- c(1:53)

colnames(CE_BASE_ESCORPIOES_Notificados)[1] <- "Semana_Epidemiológica"

rownames(CE_BASE_ESCORPIOES_Notificados) <- c(1:nrow(CE_BASE_ESCORPIOES_Notificados))

write.csv (CE_BASE_ESCORPIOES_Notificados, 
           paste0("Tabulacoes_R/Peconhentos/RS", RS, "_CE_Notificados.csv"), 
           row.names = FALSE)

AUX_GRAF <- CE_BASE_ESCORPIOES_Notificados[, c(ncol(CE_BASE_ESCORPIOES_Notificados), ncol(CE_BASE_ESCORPIOES_Notificados) -1, ncol(CE_BASE_ESCORPIOES_Notificados) -2)]

###Criando uma coluna de ordem das se para o R não colocar em ordem numérica.

AUX_GRAF$Ordem <- c(1: nrow(CE_BASE_ESCORPIOES_Notificados))

###Puxando o período sazonal atual para o gráfico de linhas

AUX_GRAF$`2025` <- CE_BASE_ESCORPIOES_Notificados$`2025`

AUX_GRAF$Sem_EPI <-as.character(c("01",  "02", "03",  "04",  "05",  "06",  "07", 
                                  "08",  "09",  "10",  "11",  "12",  "13",  "14",  
                                  "15",  "16",  "17",  "18",  "19",  "20",  "21",  
                                  "22",  "23",  "24",  "25",  "26",  "27",  "28",  
                                  "28",  "30",  "31",  "32",  "33", "34",  "35",  
                                  "36",  "37",  "38",  "39",  "40",  "41",  "42",  
                                  "43",  "44",  "45",  "46",  "47",  "48",  "49",  
                                  "50",  "51",  "52",  "53"))

assign(paste0("RS", RS, "_GRAF_CE_ESCORPIOES_Notificados"), ggplot(AUX_GRAF, aes(Ordem))  +
         theme(axis.text.x = element_text(angle = 90, 
                                          vjust = .5,
                                          face = "bold",
                                          size = 12)) +
         labs(caption = Fonte,
              title = "Canal Endêmico Acidentes com ESCORPIÕES",
              subtitle = paste0("Casos Notificados - ", RS, "ª Regional de Saúde")) +
         theme(
           panel.grid.major = element_line(color = "#C0C0C0"),
           panel.grid.minor = element_blank(),
           panel.background = element_rect(fill = "#B22222"),
           plot.title = element_text(face = "bold",
                                     size = 24,
                                     colour = "#1C1C1C"),
           plot.caption = element_text(hjust = 0, size = 12)
         ) +
         geom_area(aes(y = Lim_Superior), fill = "#F0E68C",alpha = 0.9) +
         geom_area(aes( y = Mediana), fill = "#556B2F") +
         geom_line(aes( y = `2025`), stat = "identity", color = "black", linewidth = 1.5) +
         xlab("Semana Epidemiológica") +
         ylab("Número de Casos") +
         scale_x_continuous(breaks = c(1:53), label = AUX_GRAF$Sem_EPI) +
         scale_y_continuous(expand = expansion(mult = c(0, 0.05))))

ggsave("/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/PECONHENTOS/Canal_Endemico_ESCORPIOES_PECONHENTOS.png", 
       plot = RS22_GRAF_CE_ESCORPIOES_Notificados,     
       width = 50,             
       height = 15,           
       units = "cm",           
       dpi = 300) 

#####################################   ABELHAS     #############################################################################

CE_BASE_ABELHAS_Notificados[(nrow(CE_BASE_ABELHAS_Notificados) +1), 1] <- "2025"
CE_BASE_ABELHAS_Notificados[nrow(CE_BASE_ABELHAS_Notificados), 2:54] <- as.integer(data.frame(RS22_PECONHENTOS_2025_SE_ABELHAS_Notificados[nrow(RS22_PECONHENTOS_2025_SE_Notificados), 2:54]))

#####Utilizando objetos auxiliares porque se transpor o data frame direto ele transforma as variáveis em#############
#####caracter.            NÃO FOI DESCARTADO AINDA OS PERÍODOS EPIDÊMICOS                               #############
##### VERIFICAR SE PODE-SE UTILIZAR A MÉDIA COMO LIMITE INFERIOR.                                       #############

AUX <- CE_BASE_ABELHAS_Notificados[, -1]

AUX <- t(AUX)

AUX2 <- CE_BASE_ABELHAS_Notificados[, 1]

colnames(AUX) <- AUX2

CE_BASE_ABELHAS_Notificados <- as.data.frame(AUX)

CE_BASE_ABELHAS_Notificados <- CE_BASE_ABELHAS_Notificados[, Anos_Analise]

######Criando a coluna de média no data.frame#####################

CE_BASE_ABELHAS_Notificados <- CE_BASE_ABELHAS_Notificados %>%
  mutate(Mediana = apply(CE_BASE_ABELHAS_Notificados[,1: (ncol(CE_BASE_ABELHAS_Notificados)-1)], 1 , median))

######Criando a coluna de Desvio Padrão no data frame###############

CE_BASE_ABELHAS_Notificados <- CE_BASE_ABELHAS_Notificados %>%
  mutate(Desvio_Padrao = apply(CE_BASE_ABELHAS_Notificados[,1: (ncol(CE_BASE_ABELHAS_Notificados) -2)], 1 , sd))

###### Criando a coluna de Média + 2(DP)

CE_BASE_ABELHAS_Notificados <- CE_BASE_ABELHAS_Notificados %>%
  mutate(Lim_Superior = (Mediana + 1.96 * Desvio_Padrao))

CE_BASE_ABELHAS_Notificados[, (ncol(CE_BASE_ABELHAS_Notificados)+1)] <- rownames(CE_BASE_ABELHAS_Notificados)

CE_BASE_ABELHAS_Notificados <- CE_BASE_ABELHAS_Notificados[, c(ncol(CE_BASE_ABELHAS_Notificados), 1:(ncol(CE_BASE_ABELHAS_Notificados) -1))]

CE_BASE_ABELHAS_Notificados[,1] <- c(1:53)

colnames(CE_BASE_ABELHAS_Notificados)[1] <- "Semana_Epidemiológica"

rownames(CE_BASE_ABELHAS_Notificados) <- c(1:nrow(CE_BASE_ABELHAS_Notificados))

write.csv (CE_BASE_ABELHAS_Notificados, 
           paste0("Tabulacoes_R/Peconhentos/RS", RS, "_CE_Notificados.csv"), 
           row.names = FALSE)

AUX_GRAF <- CE_BASE_ABELHAS_Notificados[, c(ncol(CE_BASE_ABELHAS_Notificados), ncol(CE_BASE_ABELHAS_Notificados) -1, ncol(CE_BASE_ABELHAS_Notificados) -2)]

###Criando uma coluna de ordem das se para o R não colocar em ordem numérica.

AUX_GRAF$Ordem <- c(1: nrow(CE_BASE_ABELHAS_Notificados))

###Puxando o período sazonal atual para o gráfico de linhas

AUX_GRAF$`2025` <- CE_BASE_ABELHAS_Notificados$`2025`

AUX_GRAF$Sem_EPI <-as.character(c("01",  "02", "03",  "04",  "05",  "06",  "07", 
                                  "08",  "09",  "10",  "11",  "12",  "13",  "14",  
                                  "15",  "16",  "17",  "18",  "19",  "20",  "21",  
                                  "22",  "23",  "24",  "25",  "26",  "27",  "28",  
                                  "28",  "30",  "31",  "32",  "33", "34",  "35",  
                                  "36",  "37",  "38",  "39",  "40",  "41",  "42",  
                                  "43",  "44",  "45",  "46",  "47",  "48",  "49",  
                                  "50",  "51",  "52",  "53"))

assign(paste0("RS", RS, "_GRAF_CE_ABELHAS_Notificados"), ggplot(AUX_GRAF, aes(Ordem))  +
         theme(axis.text.x = element_text(angle = 90, 
                                          vjust = .5,
                                          face = "bold",
                                          size = 12)) +
         labs(caption = Fonte,
              title = "Canal Endêmico Acidentes com ABELHAS",
              subtitle = paste0("Casos Notificados - ", RS, "ª Regional de Saúde")) +
         theme(
           panel.grid.major = element_line(color = "#C0C0C0"),
           panel.grid.minor = element_blank(),
           panel.background = element_rect(fill = "#B22222"),
           plot.title = element_text(face = "bold",
                                     size = 24,
                                     colour = "#1C1C1C"),
           plot.caption = element_text(hjust = 0, size = 12)
         ) +
         geom_area(aes(y = Lim_Superior), fill = "#F0E68C",alpha = 0.9) +
         geom_area(aes( y = Mediana), fill = "#556B2F") +
         geom_line(aes( y = `2025`), stat = "identity", color = "black", linewidth = 1.5) +
         xlab("Semana Epidemiológica") +
         ylab("Número de Casos") +
         scale_x_continuous(breaks = c(1:53), label = AUX_GRAF$Sem_EPI) +
         scale_y_continuous(expand = expansion(mult = c(0, 0.05))))

ggsave("/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/PECONHENTOS/Canal_Endemico_ABELHAS_PECONHENTOS.png", 
       plot = RS22_GRAF_CE_ABELHAS_Notificados,     
       width = 50,             
       height = 15,           
       units = "cm",           
       dpi = 300) 

#####################################   LAGARTAS     #############################################################################

CE_BASE_LAGARTAS_Notificados[(nrow(CE_BASE_LAGARTAS_Notificados) +1), 1] <- "2025"
CE_BASE_LAGARTAS_Notificados[nrow(CE_BASE_LAGARTAS_Notificados), 2:54] <- as.integer(data.frame(RS22_PECONHENTOS_2025_SE_LAGARTAS_Notificados[nrow(RS22_PECONHENTOS_2025_SE_Notificados), 2:54]))

#####Utilizando objetos auxiliares porque se transpor o data frame direto ele transforma as variáveis em#############
#####caracter.            NÃO FOI DESCARTADO AINDA OS PERÍODOS EPIDÊMICOS                               #############
##### VERIFICAR SE PODE-SE UTILIZAR A MÉDIA COMO LIMITE INFERIOR.                                       #############

AUX <- CE_BASE_LAGARTAS_Notificados[, -1]

AUX <- t(AUX)

AUX2 <- CE_BASE_LAGARTAS_Notificados[, 1]

colnames(AUX) <- AUX2

CE_BASE_LAGARTAS_Notificados <- as.data.frame(AUX)

CE_BASE_LAGARTAS_Notificados <- CE_BASE_LAGARTAS_Notificados[, Anos_Analise]

######Criando a coluna de média no data.frame#####################

CE_BASE_LAGARTAS_Notificados <- CE_BASE_LAGARTAS_Notificados %>%
  mutate(Mediana = apply(CE_BASE_LAGARTAS_Notificados[,1: (ncol(CE_BASE_LAGARTAS_Notificados)-1)], 1 , median))

######Criando a coluna de Desvio Padrão no data frame###############

CE_BASE_LAGARTAS_Notificados <- CE_BASE_LAGARTAS_Notificados %>%
  mutate(Desvio_Padrao = apply(CE_BASE_LAGARTAS_Notificados[,1: (ncol(CE_BASE_LAGARTAS_Notificados) -2)], 1 , sd))

###### Criando a coluna de Média + 2(DP)

CE_BASE_LAGARTAS_Notificados <- CE_BASE_LAGARTAS_Notificados %>%
  mutate(Lim_Superior = (Mediana + 1.96 * Desvio_Padrao))

CE_BASE_LAGARTAS_Notificados[, (ncol(CE_BASE_LAGARTAS_Notificados)+1)] <- rownames(CE_BASE_LAGARTAS_Notificados)

CE_BASE_LAGARTAS_Notificados <- CE_BASE_LAGARTAS_Notificados[, c(ncol(CE_BASE_LAGARTAS_Notificados), 1:(ncol(CE_BASE_LAGARTAS_Notificados) -1))]

CE_BASE_LAGARTAS_Notificados[,1] <- c(1:53)

colnames(CE_BASE_LAGARTAS_Notificados)[1] <- "Semana_Epidemiológica"

rownames(CE_BASE_LAGARTAS_Notificados) <- c(1:nrow(CE_BASE_LAGARTAS_Notificados))

write.csv (CE_BASE_LAGARTAS_Notificados, 
           paste0("Tabulacoes_R/Peconhentos/RS", RS, "_CE_Notificados.csv"), 
           row.names = FALSE)

AUX_GRAF <- CE_BASE_LAGARTAS_Notificados[, c(ncol(CE_BASE_LAGARTAS_Notificados), ncol(CE_BASE_LAGARTAS_Notificados) -1, ncol(CE_BASE_LAGARTAS_Notificados) -2)]

###Criando uma coluna de ordem das se para o R não colocar em ordem numérica.

AUX_GRAF$Ordem <- c(1: nrow(CE_BASE_LAGARTAS_Notificados))

###Puxando o período sazonal atual para o gráfico de linhas

AUX_GRAF$`2025` <- CE_BASE_LAGARTAS_Notificados$`2025`

AUX_GRAF$Sem_EPI <-as.character(c("01",  "02", "03",  "04",  "05",  "06",  "07", 
                                  "08",  "09",  "10",  "11",  "12",  "13",  "14",  
                                  "15",  "16",  "17",  "18",  "19",  "20",  "21",  
                                  "22",  "23",  "24",  "25",  "26",  "27",  "28",  
                                  "28",  "30",  "31",  "32",  "33", "34",  "35",  
                                  "36",  "37",  "38",  "39",  "40",  "41",  "42",  
                                  "43",  "44",  "45",  "46",  "47",  "48",  "49",  
                                  "50",  "51",  "52",  "53"))

assign(paste0("RS", RS, "_GRAF_CE_LAGARTAS_Notificados"), ggplot(AUX_GRAF, aes(Ordem))  +
         theme(axis.text.x = element_text(angle = 90, 
                                          vjust = .5,
                                          face = "bold",
                                          size = 12)) +
         labs(caption = Fonte,
              title = "Canal Endêmico Acidentes com LAGARTAS",
              subtitle = paste0("Casos Notificados - ", RS, "ª Regional de Saúde")) +
         theme(
           panel.grid.major = element_line(color = "#C0C0C0"),
           panel.grid.minor = element_blank(),
           panel.background = element_rect(fill = "#B22222"),
           plot.title = element_text(face = "bold",
                                     size = 24,
                                     colour = "#1C1C1C"),
           plot.caption = element_text(hjust = 0, size = 12)
         ) +
         geom_area(aes(y = Lim_Superior), fill = "#F0E68C",alpha = 0.9) +
         geom_area(aes( y = Mediana), fill = "#556B2F") +
         geom_line(aes( y = `2025`), stat = "identity", color = "black", linewidth = 1.5) +
         xlab("Semana Epidemiológica") +
         ylab("Número de Casos") +
         scale_x_continuous(breaks = c(1:53), label = AUX_GRAF$Sem_EPI) +
         scale_y_continuous(expand = expansion(mult = c(0, 0.05))))

ggsave("/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/PECONHENTOS/Canal_Endemico_LAGARTAS_PECONHENTOS.png", 
       plot = RS22_GRAF_CE_LAGARTAS_Notificados,     
       width = 50,             
       height = 15,           
       units = "cm",           
       dpi = 300) 

#####################################   OUTROS     #############################################################################

CE_BASE_OUTROS_Notificados[(nrow(CE_BASE_OUTROS_Notificados) +1), 1] <- "2025"
CE_BASE_OUTROS_Notificados[nrow(CE_BASE_OUTROS_Notificados), 2:54] <- as.integer(data.frame(RS22_PECONHENTOS_2025_SE_OUTROS_Notificados[nrow(RS22_PECONHENTOS_2025_SE_Notificados), 2:54]))

#####Utilizando objetos auxiliares porque se transpor o data frame direto ele transforma as variáveis em#############
#####caracter.            NÃO FOI DESCARTADO AINDA OS PERÍODOS EPIDÊMICOS                               #############
##### VERIFICAR SE PODE-SE UTILIZAR A MÉDIA COMO LIMITE INFERIOR.                                       #############

AUX <- CE_BASE_OUTROS_Notificados[, -1]

AUX <- t(AUX)

AUX2 <- CE_BASE_OUTROS_Notificados[, 1]

colnames(AUX) <- AUX2

CE_BASE_OUTROS_Notificados <- as.data.frame(AUX)

CE_BASE_OUTROS_Notificados <- CE_BASE_OUTROS_Notificados[, Anos_Analise]

######Criando a coluna de média no data.frame#####################

CE_BASE_OUTROS_Notificados <- CE_BASE_OUTROS_Notificados %>%
  mutate(Mediana = apply(CE_BASE_OUTROS_Notificados[,1: (ncol(CE_BASE_OUTROS_Notificados)-1)], 1 , median))

######Criando a coluna de Desvio Padrão no data frame###############

CE_BASE_OUTROS_Notificados <- CE_BASE_OUTROS_Notificados %>%
  mutate(Desvio_Padrao = apply(CE_BASE_OUTROS_Notificados[,1: (ncol(CE_BASE_OUTROS_Notificados) -2)], 1 , sd))

###### Criando a coluna de Média + 2(DP)

CE_BASE_OUTROS_Notificados <- CE_BASE_OUTROS_Notificados %>%
  mutate(Lim_Superior = (Mediana + 1.96 * Desvio_Padrao))

CE_BASE_OUTROS_Notificados[, (ncol(CE_BASE_OUTROS_Notificados)+1)] <- rownames(CE_BASE_OUTROS_Notificados)

CE_BASE_OUTROS_Notificados <- CE_BASE_OUTROS_Notificados[, c(ncol(CE_BASE_OUTROS_Notificados), 1:(ncol(CE_BASE_OUTROS_Notificados) -1))]

CE_BASE_OUTROS_Notificados[,1] <- c(1:53)

colnames(CE_BASE_OUTROS_Notificados)[1] <- "Semana_Epidemiológica"

rownames(CE_BASE_OUTROS_Notificados) <- c(1:nrow(CE_BASE_OUTROS_Notificados))

write.csv (CE_BASE_OUTROS_Notificados, 
           paste0("Tabulacoes_R/Peconhentos/RS", RS, "_CE_Notificados.csv"), 
           row.names = FALSE)

AUX_GRAF <- CE_BASE_OUTROS_Notificados[, c(ncol(CE_BASE_OUTROS_Notificados), ncol(CE_BASE_OUTROS_Notificados) -1, ncol(CE_BASE_OUTROS_Notificados) -2)]

###Criando uma coluna de ordem das se para o R não colocar em ordem numérica.

AUX_GRAF$Ordem <- c(1: nrow(CE_BASE_OUTROS_Notificados))

###Puxando o período sazonal atual para o gráfico de linhas

AUX_GRAF$`2025` <- CE_BASE_OUTROS_Notificados$`2025`

AUX_GRAF$Sem_EPI <-as.character(c("01",  "02", "03",  "04",  "05",  "06",  "07", 
                                  "08",  "09",  "10",  "11",  "12",  "13",  "14",  
                                  "15",  "16",  "17",  "18",  "19",  "20",  "21",  
                                  "22",  "23",  "24",  "25",  "26",  "27",  "28",  
                                  "28",  "30",  "31",  "32",  "33", "34",  "35",  
                                  "36",  "37",  "38",  "39",  "40",  "41",  "42",  
                                  "43",  "44",  "45",  "46",  "47",  "48",  "49",  
                                  "50",  "51",  "52",  "53"))

assign(paste0("RS", RS, "_GRAF_CE_OUTROS_Notificados"), ggplot(AUX_GRAF, aes(Ordem))  +
         theme(axis.text.x = element_text(angle = 90, 
                                          vjust = .5,
                                          face = "bold",
                                          size = 12)) +
         labs(caption = Fonte,
              title = "Canal Endêmico Acidentes (OUTROS)",
              subtitle = paste0("Casos Notificados - ", RS, "ª Regional de Saúde")) +
         theme(
           panel.grid.major = element_line(color = "#C0C0C0"),
           panel.grid.minor = element_blank(),
           panel.background = element_rect(fill = "#B22222"),
           plot.title = element_text(face = "bold",
                                     size = 24,
                                     colour = "#1C1C1C"),
           plot.caption = element_text(hjust = 0, size = 12)
         ) +
         geom_area(aes(y = Lim_Superior), fill = "#F0E68C",alpha = 0.9) +
         geom_area(aes( y = Mediana), fill = "#556B2F") +
         geom_line(aes( y = `2025`), stat = "identity", color = "black", linewidth = 1.5) +
         xlab("Semana Epidemiológica") +
         ylab("Número de Casos") +
         scale_x_continuous(breaks = c(1:53), label = AUX_GRAF$Sem_EPI) +
         scale_y_continuous(expand = expansion(mult = c(0, 0.05))))

ggsave("/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/PECONHENTOS/Canal_Endemico_OUTROS_PECONHENTOS.png", 
       plot = RS22_GRAF_CE_OUTROS_Notificados,     
       width = 50,             
       height = 15,           
       units = "cm",           
       dpi = 300) 

############ Criando uma função Theme para ser utilizado por todos os gráficos      ##################################################

Theme <- function(){
  theme(axis.text.x = element_text(angle = 50, 
                                   vjust = .5,
                                   face = "bold",
                                   size = 14),
        panel.grid.major = element_line(color = "#C0C0C0"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#F5F5F5"),
        plot.title = element_text(face = "bold",
                                  size = 24,
                                  colour = "black"),
        legend.position = "bottom")
}

########################################################################################################################################
######################################################################################################################################
################    Séries Históricas      ######################################

##########  Notificados

RS_Serie_Historica_Geral[, 1] <- as.factor(RS_Serie_Historica_Geral[, 1]) 

RS_Serie_Historica_Geral <- RS_Serie_Historica_Geral[, c(1, 5:36)]

colnames(RS_Serie_Historica_Geral)[1] <- "Ano"

assign(paste0("RS", RS, "_GRAF_Serie_Historica_Notificados"), ggplot(RS_Serie_Historica_Geral, aes(x = Ano,
                                                                                                   y = Notificados, group = 1)
                                                                     ) +
         geom_line(linewidth = 1.8,
                   colour = "black") +
         geom_point(fill = "grey",
                    size = 7,
                    shape = 21) +
         labs(caption = Fonte, 
              y = "Número de Casos",
              x = NULL,
              title = "CASOS NOTIFICADOS - (2015 a 2025)",
              subtitle = "Notificações referentes ao município de residência") +
         geom_label(aes(label = Notificados), 
                    size = 6, 
                    alpha = 0.5,
                    vjust = -0.5)  +
         scale_y_continuous(expand = expansion(mult = c(0.5, 0.5)
                                               )
                            ) +
         Theme() +
         theme(axis.text.x = element_text(angle = 0),
               plot.caption = element_text(hjust = 0, size = 12)
               )
)

ggsave("/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/PECONHENTOS/GRAF_Serie_Historica_Notificados.png", 
       plot = RS22_GRAF_Serie_Historica_Notificados,     
       width = 50,             
       height = 15,           
       units = "cm",           
       dpi = 300) 

###########    Serpentes

RS_Serie_Historica_Tipo_Acid[, 1] <- as.factor(RS_Serie_Historica_Tipo_Acid[, 1]) 

RS_Serie_Historica_Tipo_Acid <- RS_Serie_Historica_Tipo_Acid[, c(1, 5:11)]

colnames(RS_Serie_Historica_Tipo_Acid)[1] <- "Ano"

assign(paste0("RS", RS, "_GRAF_Serie_Historica_Serpentes"), ggplot(RS_Serie_Historica_Tipo_Acid, aes(x = Ano,
                                                                                                     y = Serpente, 
                                                                                                     group = 1)) +
         geom_line(linewidth = 1.8,
                   colour = "black") +
         geom_point(fill = "grey",
                    size = 7,
                    shape = 21) + 
         labs(caption = Fonte, 
              y = "Número de Casos",
              x = NULL,
              title = "CASOS NOTIFICADOS (SERPENTES) - 2015 a 2025",
              subtitle = "Notificações referentes ao município de residência") +
         geom_label(aes(label = Serpente), 
                    size = 6, 
                    alpha = 0.5,
                    vjust = -0.5)  +
         scale_y_continuous(expand = expansion(mult = c(0.5, 0.5))) +
         Theme() +
         theme(axis.text.x = element_text(angle = 0),
               plot.caption = element_text(hjust = 0, size = 12))
)

ggsave("/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/PECONHENTOS/GRAF_SERIE_HISTORICA_SERPENTES.png", 
       plot = RS22_GRAF_Serie_Historica_Serpentes,     
       width = 50,             
       height = 15,           
       units = "cm",           
       dpi = 300) 



###############  Aranhas

assign(paste0("RS", RS, "_GRAF_Serie_Historica_Aranhas"), ggplot(RS_Serie_Historica_Tipo_Acid, aes(x = Ano,
                                                                                                   y = Aranha,
                                                                                                   group = 1)) +
         geom_line(linewidth = 1.8,
                   colour = "black") +
         geom_point(fill = "grey",
                    size = 7,
                    shape = 21) +
         labs(caption = Fonte, 
              y = "Número de Casos",
              x = NULL,
              title = "CASOS NOTIFICADOS (ARANHAS) - 2015 a 2025",
              subtitle = "Notificações referentes ao município de residência") +
         geom_label(aes(label = Aranha), 
                    size = 6, 
                    alpha = 0.5,
                    vjust = -0.5)  +
         scale_y_continuous(expand = expansion(mult = c(0.5, 0.5))) +
         Theme() +
         theme(axis.text.x = element_text(angle = 0),
               plot.caption = element_text(hjust = 0, size = 12))
)

ggsave("/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/PECONHENTOS/GRAF_SERIE_HISTORICA_ARANHAS.png", 
       plot = RS22_GRAF_Serie_Historica_Aranhas,     
       width = 50,             
       height = 15,           
       units = "cm",           
       dpi = 300) 

###########   Escorpiões

assign(paste0("RS", RS, "_GRAF_Serie_Historica_Escorpioes"), ggplot(RS_Serie_Historica_Tipo_Acid, aes(x = Ano,
                                                                                                      y = Escorpiao,
                                                                                                      group = 1)) +
         geom_line(linewidth = 1.8,
                   colour = "black") +
         geom_point(fill = "grey",
                    size = 7,
                    shape = 21) +
         labs(caption = Fonte, 
              y = "Número de Casos",
              x = NULL,
              title = "CASOS NOTIFICADOS (ESCORPIÕES) - 2015 a 2025",
              subtitle = "Notificações referentes ao município de residência") +
         geom_label(aes(label = Escorpiao), 
                    size = 6, 
                    alpha = 0.5,
                    vjust = -0.5)  +
         scale_y_continuous(expand = expansion(mult = c(0.5, 0.5))) +
         Theme() +
         theme(axis.text.x = element_text(angle = 0),
               plot.caption = element_text(hjust = 0, size = 12))
)

ggsave("/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/PECONHENTOS/GRAF_SERIE_HISTORICA_Escorpioes.png", 
       plot = RS22_GRAF_Serie_Historica_Escorpioes,     
       width = 50,             
       height = 15,           
       units = "cm",           
       dpi = 300) 

###########   Abelhas

assign(paste0("RS", RS, "_GRAF_Serie_Historica_Abelha"), ggplot(RS_Serie_Historica_Tipo_Acid, aes(x = Ano,
                                                                                                      y = Abelha,
                                                                                                      group = 1)
                                                                ) +
         geom_line(linewidth = 1.8,
                   colour = "black") +
         geom_point(fill = "grey",
                    size = 7,
                    shape = 21) +
         labs(caption = Fonte, 
              y = "Número de Casos",
              x = NULL,
              title = "CASOS NOTIFICADOS (ABELHAS) - 2015 a 2025",
              subtitle = "Notificações referentes ao município de residência") +
         geom_label(aes(label = Abelha), 
                    size = 6, 
                    alpha = 0.5,
                    vjust = -0.5)  +
         scale_y_continuous(expand = expansion(mult = c(0.5, 0.5)
                                               )
                            ) +
         Theme() +
         theme(axis.text.x = element_text(angle = 0),
               plot.caption = element_text(hjust = 0, 
                                           size = 12)
               )
)

ggsave("/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/PECONHENTOS/GRAF_SERIE_HISTORICA_Abelha.png", 
       plot = RS22_GRAF_Serie_Historica_Abelha,     
       width = 50,             
       height = 15,           
       units = "cm",           
       dpi = 300) 

###########   Lagarta

assign(paste0("RS", RS, "_GRAF_Serie_Historica_Lagarta"), ggplot(RS_Serie_Historica_Tipo_Acid, aes(x = Ano,
                                                                                                  y = Lagarta,
                                                                                                  group = 1)) +
         geom_line(linewidth = 1.8,
                   colour = "black") +
         geom_point(fill = "grey",
                    size = 7,
                    shape = 21) +
         labs(caption = Fonte, 
              y = "Número de Casos",
              x = NULL,
              title = "CASOS NOTIFICADOS (LAGARTAS) - 2015 a 2025",
              subtitle = "Notificações referentes ao município de residência") +
         geom_label(aes(label = Lagarta), 
                    size = 6, 
                    alpha = 0.5,
                    vjust = -0.5)  +
         scale_y_continuous(expand = expansion(mult = c(0.5, 0.5))) +
         Theme() +
         theme(axis.text.x = element_text(angle = 0),
               plot.caption = element_text(hjust = 0, 
                                           size = 12))
)

ggsave("/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/PECONHENTOS/GRAF_SERIE_HISTORICA_Lagarta.png", 
       plot = RS22_GRAF_Serie_Historica_Lagarta,     
       width = 50,             
       height = 15,           
       units = "cm",           
       dpi = 300) 

###########   Outros

assign(paste0("RS", RS, "_GRAF_Serie_Historica_Outros"), ggplot(RS_Serie_Historica_Tipo_Acid, aes(x = Ano,
                                                                                                  y = Outros,
                                                                                                  group = 1)
                                                                ) +
         geom_line(linewidth = 1.8,
                   colour = "black") +
         geom_point(fill = "grey",
                    size = 7,
                    shape = 21) +
         labs(caption = Fonte, 
              y = "Número de Casos",
              x = NULL,
              title = "CASOS NOTIFICADOS (OUTROS) - 2015 a 2025",
              subtitle = "Notificações referentes ao município de residência") +
         geom_label(aes(label = Outros), 
                    size = 4, 
                    alpha = 0.5,
                    vjust = -0.5)  +
         scale_y_continuous(expand = expansion(mult = c(0.5, 0.5)
                                               )
                            ) +
         Theme() +
         theme(axis.text.x = element_text(angle = 0),
               plot.caption = element_text(hjust = 0, 
                                           size = 12)
               )
)

ggsave("/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/PECONHENTOS/GRAF_SERIE_HISTORICA_Outros.png", 
       plot = RS22_GRAF_Serie_Historica_Outros,     
       width = 50,             
       height = 15,           
       units = "cm",           
       dpi = 300) 

###### Gráfico de Incidência

AUX <- RS_PECONHENTOS_2022_Incidencia[-nrow(RS_PECONHENTOS_2022_Incidencia), c(1, 3)]

AUX <- RS_PECONHENTOS_2022_Incidencia %>%
  mutate(Incidencia2022 = (apply(RS_PECONHENTOS_2022_Incidencia[, 5:11], 1, sum)/Populacao) * 100000)

AUX_GRAF <- AUX[, c(2, 4, 12)]

AUX_GRAF$Incidencia2022 <- format(round(AUX_GRAF$Incidencia2022, 2)
                                  )

AUX <- RS_PECONHENTOS_2023_Incidencia[-nrow(RS_PECONHENTOS_2023_Incidencia), c(1, 3)]

AUX <- RS_PECONHENTOS_2023_Incidencia %>%
  mutate(Incidencia2023 = (apply(RS_PECONHENTOS_2023_Incidencia[, 5:11], 1, sum)/Populacao) * 100000)

AUX_GRAF[, 4] <- AUX[, c(12)]

colnames(AUX_GRAF)[4] <- "Incidencia2023"

AUX_GRAF$Incidencia2023 <- format(round(AUX_GRAF$Incidencia2023, 2)
                                  )

AUX <- RS_PECONHENTOS_2024_Incidencia[-nrow(RS_PECONHENTOS_2024_Incidencia), c(1, 3)]

AUX <- RS_PECONHENTOS_2024_Incidencia %>%
  mutate(Incidencia2024 = (apply(RS_PECONHENTOS_2024_Incidencia[, 5:11], 1, sum)/Populacao) * 100000)

AUX_GRAF[, 5] <- AUX[, c(12)]

colnames(AUX_GRAF)[5] <- "Incidencia2024"

AUX_GRAF$Incidencia2024 <- format(round(AUX_GRAF$Incidencia2024, 2)
                                  )

AUX <- RS_PECONHENTOS_2025_Incidencia[-nrow(RS_PECONHENTOS_2025_Incidencia), c(1, 3)]

AUX <- RS_PECONHENTOS_2025_Incidencia %>%
  mutate(Incidencia2025 = (apply(RS_PECONHENTOS_2025_Incidencia[, 5:11], 1, sum)/Populacao) * 100000)

AUX_GRAF[, 6] <- AUX[, c(12)]

colnames(AUX_GRAF)[6] <- "Incidencia2025"

AUX_GRAF$Incidencia2025 <- format(round(AUX_GRAF$Incidencia2025, 2)
                                  )

AUX_GRAF <- AUX_GRAF[-nrow(AUX_GRAF),]


RS_PECONHENTOS_GRAF_Incidencia_1 <- ggplot(AUX_GRAF[c(1:8),], 
                                           aes(x = Município)
                                           ) +
  labs(caption = Fonte, 
       y = "Incidência",
       x = NULL,
       title = "Incidência por Município (Residência) na 22ª RS",
       subtitle = "Casos por 100.000 habitantes") +
  geom_bar(aes(y = Incidencia2022,
               fill = "Incidencia2022"),
           stat = "identity",
           color = "black",
           linewidth = 0.8,
           width = .2,
           position = position_nudge(x = -.30)
           ) +
  geom_label(aes(y = Incidencia2022,
                 label = Incidencia2022), 
             size = 3, 
             alpha = 0.5,
             vjust = 0.1,
             nudge_x = -.30)  +
  geom_bar(aes(y = Incidencia2023,
               fill = "Incidencia2023"),
           stat = "identity",
           color = "black",
           linewidth = 0.8,
           width = .2,
           position = position_nudge(x = -.10)
           ) +
  geom_label(aes(y = Incidencia2023,
                 label = Incidencia2023), 
             size = 3, 
             alpha = 0.5,
             vjust = 0.1,
             nudge_x = -.10)  +
  geom_bar(aes(y = Incidencia2024,
               fill = "Incidencia2024"),
           stat = "identity",
           color = "black",
           linewidth = 0.8,
           width = .2,
           position = position_nudge(x = .10)
           ) +
  geom_label(aes(y = Incidencia2024,
                 label = Incidencia2024), 
             size = 3, 
             alpha = 0.5,
             vjust = 0.1,
             nudge_x = .10)  +
  geom_bar(aes(y = Incidencia2025,
               fill = "Incidencia2025"),
           stat = "identity",
           color = "black",
           linewidth = 0.8,
           width = .2,
           position = position_nudge(x = .30)
           ) +
  geom_label(aes(y = Incidencia2025,
                 label = Incidencia2025), 
             size = 3, 
             alpha = 0.5,
             vjust = 0.1,
             nudge_x = .30) +
  scale_fill_manual(name = "",
                    values = c("Incidencia2022" = "#6495ED", 
                               "Incidencia2023" = "#5F9EA0",
                               "Incidencia2024" = "#008B8B",
                               "Incidencia2025" = "#8FBC8F")
                    ) +
  scale_y_discrete(expand = expansion(mult = c(0, 0.05)
                                      )
                   ) +
  Theme() +
  theme(plot.caption = element_text(hjust = 0, 
                                    size = 12),
        axis.text.x = element_text(angle = 75)
        )

ggsave("/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/PECONHENTOS/GRAF_Incidencia_01.png", 
       plot = RS_PECONHENTOS_GRAF_Incidencia_1,     
       width = 50,             
       height = 20,           
       units = "cm",           
       dpi = 300)

####################################################################################################

RS_PECONHENTOS_GRAF_Incidencia_2 <- ggplot(AUX_GRAF[c(9:16),], aes(x = Município)) +
  labs(caption = Fonte, 
       y = "Incidência",
       x = NULL,
       title = "Incidência por Município (Residência) na 22ª RS",
       subtitle = "Casos por 100.000 habitantes") +
  geom_bar(aes(y = Incidencia2022,
               fill = "Incidencia2022"),
           stat = "identity",
           color = "black",
           linewidth = 0.8,
           width = .2,
           position = position_nudge(x = -.30)) +
  geom_label(aes(y = Incidencia2022,
                 label = Incidencia2022), 
             size = 3, 
             alpha = 0.5,
             vjust = 0.1,
             nudge_x = -.30)  +
  geom_bar(aes(y = Incidencia2023,
               fill = "Incidencia2023"),
           stat = "identity",
           color = "black",
           linewidth = 0.8,
           width = .2,
           position = position_nudge(x = -.10)) +
  geom_label(aes(y = Incidencia2023,
                 label = Incidencia2023), 
             size = 3, 
             alpha = 0.5,
             vjust = 0.1,
             nudge_x = -.10)  +
  geom_bar(aes(y = Incidencia2024,
               fill = "Incidencia2024"),
           stat = "identity",
           color = "black",
           linewidth = 0.8,
           width = .2,
           position = position_nudge(x = .10)) +
  geom_label(aes(y = Incidencia2024,
                 label = Incidencia2024), 
             size = 3, 
             alpha = 0.5,
             vjust = 0.1,
             nudge_x = .10)  +
  geom_bar(aes(y = Incidencia2025,
               fill = "Incidencia2025"),
           stat = "identity",
           color = "black",
           linewidth = 0.8,
           width = .2,
           position = position_nudge(x = .30)) +
  geom_label(aes(y = Incidencia2025,
                 label = Incidencia2025), 
             size = 3, 
             alpha = 0.5,
             vjust = 0.1,
             nudge_x = .30) +
  scale_fill_manual(name = "",
                    values = c("Incidencia2022" = "#6495ED", 
                               "Incidencia2023" = "#5F9EA0",
                               "Incidencia2024" = "#008B8B",
                               "Incidencia2025" = "#8FBC8F")) +
  scale_y_discrete(expand = expansion(mult = c(0, 0.05))) +
  Theme() +
  theme(plot.caption = element_text(hjust = 0, size = 12),
        axis.text.x = element_text(angle = 75))

ggsave("/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/PECONHENTOS/GRAF_Incidencia_02.png", 
       plot = RS_PECONHENTOS_GRAF_Incidencia_2,     
       width = 50,             
       height = 20,           
       units = "cm",           
       dpi = 300)

############# Tipo de acidente SERPENTE

RS22_TAB_Tipo_Serpente_Historico <- RS_Serie_Historica_Tipo_Acid_Serpente[-1, -c(2, 3, 4)] %>%
  gt() %>%
  tab_header(title = md("**Tipo de Acidente Ofídico Notificados (2016 - 2025)**"),
             subtitle = "(Campo 46 SINAN)") %>%
  tab_options(heading.align = "left",
              column_labels.border.top.color = "black",
              column_labels.border.top.width = px(3)) %>%
  tab_spanner(label = "Tipo de Acidente",
              columns = c(2:7)) %>%
  cols_align(align = "center", columns = c(2:7)) %>%
  cols_label(RS = "Ano",
             Botropico = "Botrópico",
             Elapidico = "Elapídico",
             Laquetico = "Laquético",
             Nao_Peconhenta = "Não Peçonhenta") %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels(everything())) %>%
  tab_footnote(footnote = Fonte) %>%
  tab_options(table.font.size = "small")

gtsave(data = RS22_TAB_Tipo_Serpente_Historico,
       filename = "/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS22_TAB_Tipo_Serpente_Historico.png")

############# Tipo de acidente ARANHA

RS22_TAB_Tipo_Aranha_Historico <- RS_Serie_Historica_Tipo_Acid_Aranha[-1, -c(2, 3, 4)] %>%
  gt() %>%
  tab_header(title = md("**Tipo de Acidente com Aranhas Notificados (2016 - 2025)**"),
             subtitle = "(campo 47 SINAN)") %>%
  tab_options(heading.align = "left",
              column_labels.border.top.color = "black",
              column_labels.border.top.width = px(3)) %>%
  tab_spanner(label = "Tipo de Acidente",
              columns = c(2:6)) %>%
  cols_align(align = "center", columns = c(2:6)) %>%
  cols_label(RS = "Ano",
             Outra_Aranha = "Outras") %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels(everything())) %>%
  tab_footnote(footnote = Fonte) %>%
  tab_options(table.font.size = "small")

gtsave(data = RS22_TAB_Tipo_Aranha_Historico,
       filename = "/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS22_TAB_Tipo_Aranha_Historico.png")

############# Tipo de acidente LAGARTA

RS22_TAB_Tipo_Lagarta_Historico <- RS_Serie_Historica_Tipo_Acid_Lagarta[-1, -c(2, 3, 4)] %>%
  gt() %>%
  tab_header(title = md("**Tipo de Acidente com Lagartas Notificados (2016 - 2025)**"),
             subtitle = "(campo 48 SINAN)") %>%
  tab_options(heading.align = "left",
              column_labels.border.top.color = "black",
              column_labels.border.top.width = px(3)) %>%
  tab_spanner(label = "Tipo de Acidente",
              columns = c(2:4)) %>%
  cols_align(align = "center", columns = c(2:4)) %>%
  cols_label(RS = "Ano",
             Outra_Lagarta = "Outras") %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels(everything())) %>%
  tab_footnote(footnote = Fonte) %>%
  tab_options(table.font.size = "small")

gtsave(data = RS22_TAB_Tipo_Lagarta_Historico,
       filename = "/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS22_TAB_Tipo_Lagarta_Historico.png")

#################################################################################################################
###############  Caracterização Lugar   #################################
#### Zona de ocorrência

AUX <- RS_Serie_Historica_Geral[- nrow(RS_Serie_Historica_Geral), c(1, 3:6)]

RS22_TAB_Zona_Ocorrencia <- AUX %>%
  gt() %>%
  tab_header(title = md("**Notificações por Zona de Ocorrência (2015 - 2024)**")) %>%
  tab_options(heading.align = "left",
              column_labels.border.top.color = "black",
              column_labels.border.top.width = px(3)) %>%
  tab_spanner(label = "Zona de Ocorrência",
              columns = c(2:5)) %>%
  cols_align(align = "center", columns = c(2:5)) %>%
  cols_label(Zona_Urbana = "Urbana",
             Zona_Rural = "Rural",
             Zona_Periurbana = "Periurbana",
             Zona_Ignorados = "Ignorado") %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels(everything())) %>%
  tab_footnote(footnote = Fonte) %>%
  tab_options(table.font.size = "small")

gtsave(data = RS22_TAB_Zona_Ocorrencia,
       filename = "/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/PECONHENTOS/Tabela_ZONA.png")

#### Gráfico 

AUX <- RS_Serie_Historica_Geral[nrow(RS_Serie_Historica_Geral), 3:6]

AUX <- as.data.frame(t(AUX))

AUX[,2] <- c("Urbana", "Rural", "Periurbana", "Ignorado")

colnames(AUX) <- c("Casos", "Zona")

RS22_GRAF_ZONA_Ocorrencia <- ggplot(AUX, aes(x = Zona, y = Casos)) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#E6E6FA",
           linewidth = 0.8) +
  labs(caption = Fonte, 
       y = "Número de Casos",
       x = "Zona de Ocorrência",
       title = "Zona de Ocorrência dos Acidentes Notificados - 2025",
       subtitle = "Casos notificados no território da 22ª Regional de Saúde") +
  geom_label(aes(label = Casos), 
             size = 3, 
             alpha = 0.5,
             vjust = 0.1)  +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  Theme() +
  theme(axis.text.x = element_text(angle = 0),
        plot.caption = element_text(hjust = 0))

ggsave("/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS22_GRAF_ZONA_Ocorrencia.png", 
       plot = RS22_GRAF_ZONA_Ocorrencia,     
       width = 30,             
       height = 20,           
       units = "cm",           
       dpi = 300) 

##### Tabela Municípios Zona de Ocorrência

Tabela_SEXO_MUNI <- RS22_PECONHENTOS_2025_GERAL[-nrow(RS22_PECONHENTOS_2025_GERAL), c(2, 6:9)] %>%
  gt() %>%
  tab_header(title = md("**Tabela 01 - Notificações por Zona de Ocorrência**"),
             subtitle = "Notificações entre 2015 - 2024") %>%
  tab_options(heading.align = "left",
              column_labels.border.top.color = "black",
              column_labels.border.top.width = px(3)) %>%
  tab_spanner(label = "Zona de Ocorrência",
              columns = c(2:5)) %>%
  cols_align(align = "center", columns = c(2:5)) %>%
  cols_label(Zona_Urbana = "Urbana",
             Zona_Rural = "Rural",
             Zona_Periurbana = "Periurbana",
             Zona_Ignorados = "Ignorado") %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels(everything())) %>%
  tab_footnote(footnote = Fonte) %>%
  tab_options(table.font.size = "small")

gtsave(data = Tabela_SEXO_MUNI,
       filename = "/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/PECONHENTOS/Tabela_SEXO_MUNI.png")

######Caracterização Pessoa
#### Sexo

AUX <- RS_Serie_Historica_Geral[- nrow(RS_Serie_Historica_Geral), c(1, 7:8)]

RS22_TAB_Sexo <- AUX %>%
  gt() %>%
  tab_header(title = md("**Tabela 02 - Notificações por Sexo**"),
             subtitle = "Notificações entre 2015 - 2024") %>%
  tab_options(heading.align = "left",
              column_labels.border.top.color = "black",
              column_labels.border.top.width = px(3)) %>%
  tab_spanner(label = "Sexo",
              columns = c(2:3)) %>%
  cols_align(align = "center", columns = c(2:3)) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels(everything())) %>%
  tab_footnote(footnote = Fonte) %>%
  tab_options(table.font.size = "small")

gtsave(data = RS22_TAB_Sexo,
       filename = "/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS22_TAB_Sexo.png")


AUX <- RS_Serie_Historica_Geral[nrow(RS_Serie_Historica_Geral), 7:8]

AUX <- as.data.frame(t(AUX))

AUX[,2] <- c("Feminino", "Masculino")

colnames(AUX) <- c("Casos", "Sexo")

RS22_GRAF_Sexo <- ggplot(AUX, aes(x = Sexo, 
                                  y = Casos)) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#DDA0DD",
           linewidth = 0.8) +
  labs(caption = Fonte, 
       y = "Número de Casos",
       x = "Sexo",
       title = "Sexo dos Acidentes Notificados - 2025",
       subtitle = "Casos notificados no território da 22ª Regional de Saúde") +
  geom_label(aes(label = Casos), 
             size = 3, 
             alpha = 0.5,
             vjust = 0.1)  +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  Theme() +
  theme(axis.text.x = element_text(angle = 0),
        plot.caption = element_text(hjust = 0))

ggsave("/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS22_GRAF_Sexo.png", 
       plot = RS22_GRAF_Sexo,     
       width = 30,             
       height = 20,           
       units = "cm",           
       dpi = 300)

################ Pirâmide Etária  

AUX <- c("< 01", "< 01", "01 - 04", "01 - 04", "05 - 09", "05 - 09", "10 - 14", "10 - 14", "15 - 19", "15 - 19", "20 - 24", "20 - 24", 
"25 - 29", "25 - 29", "30 - 34", "30 - 34", "35 - 39", "35 - 39", "40 - 44", "40 - 44", "45 - 49", "45 - 49", "50 - 54", "50 - 54",
"55 - 59", "55 - 59", "60 - 64", "60 - 64", "65 - 69", "65 - 69", "70 - 74",  "70 - 74", "75 - 79", "75 - 79", "80 - 84", 
"80 - 84", "> 84", "> 84")

AUX <- as.data.frame(AUX)

AUX[, 2] <- c("M", "F", "M", "F", "M", "F", "M", "F", "M", "F", "M", "F", "M", "F", "M", "F", "M", "F", 
              "M", "F", "M", "F", "M", "F", "M", "F", "M", "F", "M", "F", "M", "F", "M", "F", "M", "F", "M", "F")

AUX[1, 3] <- SINAN_PECONHENTOS_2025[, 17:18] %>%
  filter(NU_IDADE_N <= 4000 & CS_SEXO == "M") %>%
  count()

AUX[2, 3] <- SINAN_PECONHENTOS_2025[, 17:18] %>%
  filter(NU_IDADE_N <= 4000 & CS_SEXO == "F") %>%
  count()

AUX[3, 3] <- SINAN_PECONHENTOS_2025[, 17:18] %>%
  filter(NU_IDADE_N > 4000 & NU_IDADE_N <= 4004 & CS_SEXO == "M") %>%
  count()

AUX[4, 3] <- SINAN_PECONHENTOS_2025[, 17:18] %>%
  filter(NU_IDADE_N > 4000 & NU_IDADE_N <= 4004 & CS_SEXO == "F") %>%
  count()

AUX[5, 3] <- SINAN_PECONHENTOS_2025[, 17:18] %>%
  filter(NU_IDADE_N > 4004 & NU_IDADE_N <= 4009 & CS_SEXO == "M") %>%
  count()

AUX[6, 3] <- SINAN_PECONHENTOS_2025[, 17:18] %>%
  filter(NU_IDADE_N > 4004 & NU_IDADE_N <= 4009 & CS_SEXO == "F") %>%
  count()

AUX[7, 3] <- SINAN_PECONHENTOS_2025[, 17:18] %>%
  filter(NU_IDADE_N > 4009 & NU_IDADE_N <= 4014 & CS_SEXO == "M") %>%
  count()

AUX[8, 3] <- SINAN_PECONHENTOS_2025[, 17:18] %>%
  filter(NU_IDADE_N > 4009 & NU_IDADE_N <= 4014 & CS_SEXO == "F") %>%
  count()

AUX[9, 3] <- SINAN_PECONHENTOS_2025[, 17:18] %>%
  filter(NU_IDADE_N > 4014 & NU_IDADE_N <= 4019 & CS_SEXO == "M") %>%
  count()

AUX[10, 3] <- SINAN_PECONHENTOS_2025[, 17:18] %>%
  filter(NU_IDADE_N > 4014 & NU_IDADE_N <= 4019 & CS_SEXO == "F") %>%
  count()

AUX[11, 3] <- SINAN_PECONHENTOS_2025[, 17:18] %>%
  filter(NU_IDADE_N > 4019 & NU_IDADE_N <= 4024 & CS_SEXO == "M") %>%
  count()

AUX[12, 3] <- SINAN_PECONHENTOS_2025[, 17:18] %>%
  filter(NU_IDADE_N > 4019 & NU_IDADE_N <= 4024 & CS_SEXO == "F") %>%
  count()

AUX[13, 3] <- SINAN_PECONHENTOS_2025[, 17:18] %>%
  filter(NU_IDADE_N > 4024 & NU_IDADE_N <= 4029 & CS_SEXO == "M") %>%
  count()

AUX[14, 3] <- SINAN_PECONHENTOS_2025[, 17:18] %>%
  filter(NU_IDADE_N > 4024 & NU_IDADE_N <= 4029 & CS_SEXO == "F") %>%
  count()

AUX[15, 3] <- SINAN_PECONHENTOS_2025[, 17:18] %>%
  filter(NU_IDADE_N > 4029 & NU_IDADE_N <= 4034 & CS_SEXO == "M") %>%
  count()

AUX[16, 3] <- SINAN_PECONHENTOS_2025[, 17:18] %>%
  filter(NU_IDADE_N > 4029 & NU_IDADE_N <= 4034 & CS_SEXO == "F") %>%
  count()

AUX[17, 3] <- SINAN_PECONHENTOS_2025[, 17:18] %>%
  filter(NU_IDADE_N > 4034 & NU_IDADE_N <= 4039 & CS_SEXO == "M") %>%
  count()

AUX[18, 3] <- SINAN_PECONHENTOS_2025[, 17:18] %>%
  filter(NU_IDADE_N > 4034 & NU_IDADE_N <= 4039 & CS_SEXO == "F") %>%
  count()

AUX[19, 3] <- SINAN_PECONHENTOS_2025[, 17:18] %>%
  filter(NU_IDADE_N > 4039 & NU_IDADE_N <= 4044 & CS_SEXO == "M") %>%
  count()

AUX[20, 3] <- SINAN_PECONHENTOS_2025[, 17:18] %>%
  filter(NU_IDADE_N > 4039 & NU_IDADE_N <= 4044 & CS_SEXO == "F") %>%
  count()

AUX[21, 3] <- SINAN_PECONHENTOS_2025[, 17:18] %>%
  filter(NU_IDADE_N > 4044 & NU_IDADE_N <= 4049 & CS_SEXO == "M") %>%
  count()

AUX[22, 3] <- SINAN_PECONHENTOS_2025[, 17:18] %>%
  filter(NU_IDADE_N > 4044 & NU_IDADE_N <= 4049 & CS_SEXO == "F") %>%
  count()

AUX[23, 3] <- SINAN_PECONHENTOS_2025[, 17:18] %>%
  filter(NU_IDADE_N > 4049 & NU_IDADE_N <= 4054 & CS_SEXO == "M") %>%
  count()

AUX[24, 3] <- SINAN_PECONHENTOS_2025[, 17:18] %>%
  filter(NU_IDADE_N > 4049 & NU_IDADE_N <= 4054 & CS_SEXO == "F") %>%
  count()

AUX[25, 3] <- SINAN_PECONHENTOS_2025[, 17:18] %>%
  filter(NU_IDADE_N > 4054 & NU_IDADE_N <= 4059 & CS_SEXO == "M") %>%
  count()

AUX[26, 3] <- SINAN_PECONHENTOS_2025[, 17:18] %>%
  filter(NU_IDADE_N > 4054 & NU_IDADE_N <= 4059 & CS_SEXO == "F") %>%
  count()

AUX[27, 3] <- SINAN_PECONHENTOS_2025[, 17:18] %>%
  filter(NU_IDADE_N > 4059 & NU_IDADE_N <= 4064 & CS_SEXO == "M") %>%
  count()

AUX[28, 3] <- SINAN_PECONHENTOS_2025[, 17:18] %>%
  filter(NU_IDADE_N > 4059 & NU_IDADE_N <= 4064 & CS_SEXO == "F") %>%
  count()

AUX[29, 3] <- SINAN_PECONHENTOS_2025[, 17:18] %>%
  filter(NU_IDADE_N > 4064 & NU_IDADE_N <= 4069 & CS_SEXO == "M") %>%
  count()

AUX[30, 3] <- SINAN_PECONHENTOS_2025[, 17:18] %>%
  filter(NU_IDADE_N > 4064 & NU_IDADE_N <= 4069 & CS_SEXO == "F") %>%
  count()

AUX[31, 3] <- SINAN_PECONHENTOS_2025[, 17:18] %>%
  filter(NU_IDADE_N > 4069 & NU_IDADE_N <= 4074 & CS_SEXO == "M") %>%
  count()

AUX[32, 3] <- SINAN_PECONHENTOS_2025[, 17:18] %>%
  filter(NU_IDADE_N > 4069 & NU_IDADE_N <= 4074 & CS_SEXO == "F") %>%
  count()

AUX[33, 3] <- SINAN_PECONHENTOS_2025[, 17:18] %>%
  filter(NU_IDADE_N > 4074 & NU_IDADE_N <= 4079 & CS_SEXO == "M") %>%
  count()

AUX[34, 3] <- SINAN_PECONHENTOS_2025[, 17:18] %>%
  filter(NU_IDADE_N > 4074 & NU_IDADE_N <= 4079 & CS_SEXO == "F") %>%
  count()

AUX[35, 3] <- SINAN_PECONHENTOS_2025[, 17:18] %>%
  filter(NU_IDADE_N > 4079 & NU_IDADE_N <= 4084 & CS_SEXO == "M") %>%
  count()

AUX[36, 3] <- SINAN_PECONHENTOS_2025[, 17:18] %>%
  filter(NU_IDADE_N > 4079 & NU_IDADE_N <= 4084 & CS_SEXO == "F") %>%
  count()

AUX[37, 3] <- SINAN_PECONHENTOS_2025[, 17:18] %>%
  filter(NU_IDADE_N > 4084 & CS_SEXO == "M") %>%
  count()

AUX[38, 3] <- SINAN_PECONHENTOS_2025[, 17:18] %>%
  filter(NU_IDADE_N > 4084 & CS_SEXO == "F") %>%
  count()

colnames(AUX) <- c("Grupo_Idade", "Sexo", "Populacao")

AUX <- AUX %>%
  mutate(Pop = case_when(Sexo == "M" ~ Populacao * -1,
                         Sexo == "F" ~ Populacao ))

AUX <- AUX %>% 
  mutate(Sexo_Legenda = case_when(Sexo == "M" ~ "Masculino",
                                  Sexo == "F" ~ "Feminino"))

AUX <- AUX %>%
  mutate(grupo_Idade_FACT = factor(Grupo_Idade, 
                                   levels = c(
    "< 01",
    "01 - 04",
    "05 - 09",
    "10 - 14",
    "15 - 19",
    "20 - 24",
    "25 - 29",
    "30 - 34",
    "35 - 39",
    "40 - 44",
    "45 - 49",
    "50 - 54",
    "55 - 59",
    "60 - 64",
    "65 - 69",
    "70 - 74",
    "75 - 79",
    "80 - 84",
    "> 84")
    )
    )

RS_PECONHENTOS_GRAF_Piramide <- ggplot(AUX, 
                                       aes(x = Pop,
                                           y = grupo_Idade_FACT, 
                                           fill = Sexo_Legenda)) +
  geom_col(color = "black",
           linewidth = 0.8) +
  labs(title = "Pirâmide Etária dos Casos Notificados na 22ª Regional de Saúde",
       y = "Faixa Etária",
       x = "População",
       fill = "Sexo",
       caption = Fonte1) +
  scale_x_continuous(labels = abs) +
  Theme() +
  theme(axis.text.x = element_text(angle = 0),
        plot.caption = element_text(hjust = 0)) 


ggsave("/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/PECONHENTOS/Mapa_Piramide.png", 
       plot = RS_PECONHENTOS_GRAF_Piramide,     
       width = 24,             
       height = 20,           
       units = "cm",           
       dpi = 300) 

####################  Pirâmide Etária Série Histórica  ########################
AUX <- c("< 01", "< 01", "01 - 04", "01 - 04", "05 - 09", "05 - 09", "10 - 14", "10 - 14", "15 - 19", "15 - 19", "20 - 24", "20 - 24", 
         "25 - 29", "25 - 29", "30 - 34", "30 - 34", "35 - 39", "35 - 39", "40 - 44", "40 - 44", "45 - 49", "45 - 49", "50 - 54", "50 - 54",
         "55 - 59", "55 - 59", "60 - 64", "60 - 64", "65 - 69", "65 - 69", "70 - 74",  "70 - 74", "75 - 79", "75 - 79", "80 - 84", 
         "80 - 84", "> 84", "> 84")

AUX <- as.data.frame(AUX)

AUX[, 2] <- c("M", "F", "M", "F", "M", "F", "M", "F", "M", "F", "M", "F", "M", "F", "M", "F", "M", "F", 
              "M", "F", "M", "F", "M", "F", "M", "F", "M", "F", "M", "F", "M", "F", "M", "F", "M", "F", "M", "F")

AUX[1, 3] <- RS_SINAN_Piramide %>%
  filter(NU_IDADE_N <= 4000 & CS_SEXO == "M") %>%
  count()

AUX[2, 3] <- RS_SINAN_Piramide %>%
  filter(NU_IDADE_N <= 4000 & CS_SEXO == "F") %>%
  count()

AUX[3, 3] <- RS_SINAN_Piramide %>%
  filter(NU_IDADE_N > 4000 & NU_IDADE_N <= 4004 & CS_SEXO == "M") %>%
  count()

AUX[4, 3] <- RS_SINAN_Piramide %>%
  filter(NU_IDADE_N > 4000 & NU_IDADE_N <= 4004 & CS_SEXO == "F") %>%
  count()

AUX[5, 3] <- RS_SINAN_Piramide %>%
  filter(NU_IDADE_N > 4004 & NU_IDADE_N <= 4009 & CS_SEXO == "M") %>%
  count()

AUX[6, 3] <- RS_SINAN_Piramide %>%
  filter(NU_IDADE_N > 4004 & NU_IDADE_N <= 4009 & CS_SEXO == "F") %>%
  count()

AUX[7, 3] <- RS_SINAN_Piramide %>%
  filter(NU_IDADE_N > 4009 & NU_IDADE_N <= 4014 & CS_SEXO == "M") %>%
  count()

AUX[8, 3] <- RS_SINAN_Piramide %>%
  filter(NU_IDADE_N > 4009 & NU_IDADE_N <= 4014 & CS_SEXO == "F") %>%
  count()

AUX[9, 3] <- RS_SINAN_Piramide %>%
  filter(NU_IDADE_N > 4014 & NU_IDADE_N <= 4019 & CS_SEXO == "M") %>%
  count()

AUX[10, 3] <- RS_SINAN_Piramide %>%
  filter(NU_IDADE_N > 4014 & NU_IDADE_N <= 4019 & CS_SEXO == "F") %>%
  count()

AUX[11, 3] <- RS_SINAN_Piramide %>%
  filter(NU_IDADE_N > 4019 & NU_IDADE_N <= 4024 & CS_SEXO == "M") %>%
  count()

AUX[12, 3] <- RS_SINAN_Piramide %>%
  filter(NU_IDADE_N > 4019 & NU_IDADE_N <= 4024 & CS_SEXO == "F") %>%
  count()

AUX[13, 3] <- RS_SINAN_Piramide %>%
  filter(NU_IDADE_N > 4024 & NU_IDADE_N <= 4029 & CS_SEXO == "M") %>%
  count()

AUX[14, 3] <- RS_SINAN_Piramide %>%
  filter(NU_IDADE_N > 4024 & NU_IDADE_N <= 4029 & CS_SEXO == "F") %>%
  count()

AUX[15, 3] <- RS_SINAN_Piramide %>%
  filter(NU_IDADE_N > 4029 & NU_IDADE_N <= 4034 & CS_SEXO == "M") %>%
  count()

AUX[16, 3] <- RS_SINAN_Piramide %>%
  filter(NU_IDADE_N > 4029 & NU_IDADE_N <= 4034 & CS_SEXO == "F") %>%
  count()

AUX[17, 3] <- RS_SINAN_Piramide %>%
  filter(NU_IDADE_N > 4034 & NU_IDADE_N <= 4039 & CS_SEXO == "M") %>%
  count()

AUX[18, 3] <- RS_SINAN_Piramide %>%
  filter(NU_IDADE_N > 4034 & NU_IDADE_N <= 4039 & CS_SEXO == "F") %>%
  count()

AUX[19, 3] <- RS_SINAN_Piramide %>%
  filter(NU_IDADE_N > 4039 & NU_IDADE_N <= 4044 & CS_SEXO == "M") %>%
  count()

AUX[20, 3] <- RS_SINAN_Piramide %>%
  filter(NU_IDADE_N > 4039 & NU_IDADE_N <= 4044 & CS_SEXO == "F") %>%
  count()

AUX[21, 3] <- RS_SINAN_Piramide %>%
  filter(NU_IDADE_N > 4044 & NU_IDADE_N <= 4049 & CS_SEXO == "M") %>%
  count()

AUX[22, 3] <- RS_SINAN_Piramide %>%
  filter(NU_IDADE_N > 4044 & NU_IDADE_N <= 4049 & CS_SEXO == "F") %>%
  count()

AUX[23, 3] <- RS_SINAN_Piramide %>%
  filter(NU_IDADE_N > 4049 & NU_IDADE_N <= 4054 & CS_SEXO == "M") %>%
  count()

AUX[24, 3] <- RS_SINAN_Piramide %>%
  filter(NU_IDADE_N > 4049 & NU_IDADE_N <= 4054 & CS_SEXO == "F") %>%
  count()

AUX[25, 3] <- RS_SINAN_Piramide %>%
  filter(NU_IDADE_N > 4054 & NU_IDADE_N <= 4059 & CS_SEXO == "M") %>%
  count()

AUX[26, 3] <- RS_SINAN_Piramide %>%
  filter(NU_IDADE_N > 4054 & NU_IDADE_N <= 4059 & CS_SEXO == "F") %>%
  count()

AUX[27, 3] <- RS_SINAN_Piramide %>%
  filter(NU_IDADE_N > 4059 & NU_IDADE_N <= 4064 & CS_SEXO == "M") %>%
  count()

AUX[28, 3] <- RS_SINAN_Piramide %>%
  filter(NU_IDADE_N > 4059 & NU_IDADE_N <= 4064 & CS_SEXO == "F") %>%
  count()

AUX[29, 3] <- RS_SINAN_Piramide %>%
  filter(NU_IDADE_N > 4064 & NU_IDADE_N <= 4069 & CS_SEXO == "M") %>%
  count()

AUX[30, 3] <- RS_SINAN_Piramide %>%
  filter(NU_IDADE_N > 4064 & NU_IDADE_N <= 4069 & CS_SEXO == "F") %>%
  count()

AUX[31, 3] <- RS_SINAN_Piramide %>%
  filter(NU_IDADE_N > 4069 & NU_IDADE_N <= 4074 & CS_SEXO == "M") %>%
  count()

AUX[32, 3] <- RS_SINAN_Piramide %>%
  filter(NU_IDADE_N > 4069 & NU_IDADE_N <= 4074 & CS_SEXO == "F") %>%
  count()

AUX[33, 3] <- RS_SINAN_Piramide %>%
  filter(NU_IDADE_N > 4074 & NU_IDADE_N <= 4079 & CS_SEXO == "M") %>%
  count()

AUX[34, 3] <- RS_SINAN_Piramide %>%
  filter(NU_IDADE_N > 4074 & NU_IDADE_N <= 4079 & CS_SEXO == "F") %>%
  count()

AUX[35, 3] <- RS_SINAN_Piramide %>%
  filter(NU_IDADE_N > 4079 & NU_IDADE_N <= 4084 & CS_SEXO == "M") %>%
  count()

AUX[36, 3] <- RS_SINAN_Piramide %>%
  filter(NU_IDADE_N > 4079 & NU_IDADE_N <= 4084 & CS_SEXO == "F") %>%
  count()

AUX[37, 3] <- RS_SINAN_Piramide %>%
  filter(NU_IDADE_N > 4084 & CS_SEXO == "M") %>%
  count()

AUX[38, 3] <- RS_SINAN_Piramide %>%
  filter(NU_IDADE_N > 4084 & CS_SEXO == "F") %>%
  count()

colnames(AUX) <- c("Grupo_Idade", "Sexo", "Populacao")

AUX <- AUX %>%
  mutate(Pop = case_when(Sexo == "M" ~ Populacao * -1,
                         Sexo == "F" ~ Populacao ))

AUX <- AUX %>% 
  mutate(Sexo_Legenda = case_when(Sexo == "M" ~ "Masculino",
                                  Sexo == "F" ~ "Feminino"))

AUX <- AUX %>%
  mutate(grupo_Idade_FACT = factor(Grupo_Idade, 
                                   levels = c(
                                     "< 01",
                                     "01 - 04",
                                     "05 - 09",
                                     "10 - 14",
                                     "15 - 19",
                                     "20 - 24",
                                     "25 - 29",
                                     "30 - 34",
                                     "35 - 39",
                                     "40 - 44",
                                     "45 - 49",
                                     "50 - 54",
                                     "55 - 59",
                                     "60 - 64",
                                     "65 - 69",
                                     "70 - 74",
                                     "75 - 79",
                                     "80 - 84",
                                     "> 84")
  )
  )

RS_PECONHENTOS_GRAF_Piramide_Historico <- ggplot(AUX, 
                                                 aes(x = Pop,
                                                     y = grupo_Idade_FACT, 
                                                     fill = Sexo_Legenda)) +
  geom_col(color = "black",
           linewidth = 0.8) +
  labs(title = "Pirâmide Etária Notificações - (2016 - 2024)",
       y = "Faixa Etária",
       x = "População",
       fill = "Sexo",
       caption = Fonte1) +
  scale_x_continuous(labels = abs) +
  Theme() +
  theme(axis.text.x = element_text(angle = 0),
        plot.caption = element_text(hjust = 0)) 


ggsave("/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/PECONHENTOS/Mapa_Piramide_Historico.png", 
       plot = RS_PECONHENTOS_GRAF_Piramide_Historico,     
       width = 24,             
       height = 20,           
       units = "cm",           
       dpi = 300) 

###########  Escolaridade

###Série

AUX <- RS_Serie_Historica_Geral[-c(1, nrow(RS_Serie_Historica_Geral)), c(1, 15:23)]

AUX[nrow(AUX) +1, -1] <- apply(AUX[, -1], 2, sum)

AUX <- t(AUX[nrow(AUX),])

AUX <- as.data.frame(AUX[-1,])

AUX[, 2] <- as.factor(c("Analfabeto", "Fundamental Incompleto", 
              "Fundamental Completo", "Ensino Médio Incompleto", 
              "Ensino Médio Completo", "Superior Incompleto", "Superior Completo",
              "Não se Aplica", "Ignorado"))

AUX[, 3] <- as.factor(c(1:9))
                      
colnames(AUX) <- c("Casos", "Escolaridade", "Ordem")

AUX[, 1] <- as.numeric(AUX[, 1])

RS_PECONHENTOS_GRAF_Serie_Escolaridade <- ggplot(AUX) +
  geom_bar(aes(x =  Ordem, 
               y = Casos),
           stat = "identity",
           color = "black",
           fill = "#FFE4C4",
           linewidth = 0.8) +
  labs(title = "Escolaridade dos Casos Notificados",
       subtitle = "Total de Notificações (2016 - 2024)",
       y = "Casos",
       x = NULL,
       caption = Fonte1) + 
  geom_label(aes(label = Casos, 
                 x =  Ordem, 
                 y = Casos),
             alpha = 0.5,
             vjust = 0.1)  +
  scale_x_discrete(labels = AUX$Escolaridade) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)
                                        )
                     ) +
  Theme() +
  theme(plot.caption = element_text(hjust = 0)) 

ggsave("/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/PECONHENTOS/Mapa_Escolaridade_Historico.png", 
       plot = RS_PECONHENTOS_GRAF_Serie_Escolaridade,     
       width = 24,             
       height = 20,           
       units = "cm",           
       dpi = 300) 
    
### Ano corrente

AUX <- RS_Serie_Historica_Geral[nrow(RS_Serie_Historica_Geral), c(1, 15:23)]
  
AUX <- t(AUX)

AUX <- as.data.frame(AUX[-1,])

AUX[, 2] <- as.factor(c("Analfabeto", "Fundamental Incompleto", 
                        "Fundamental Completo", "Ensino Médio Incompleto", 
                        "Ensino Médio Completo", "Superior Incompleto", "Superior Completo",
                        "Não se Aplica", "Ignorado"))

AUX[, 3] <- as.factor(c(1:9)
                      )

colnames(AUX) <- c("Casos", "Escolaridade", "Ordem")

AUX[, 1] <- as.numeric(AUX[, 1])

RS_PECONHENTOS_GRAF_Escolaridade <- ggplot(AUX) +
  geom_bar(aes(x =  Ordem, 
               y = Casos),
           stat = "identity",
           color = "black",
           fill = "#FFE4C4",
           linewidth = 0.8) +
  labs(title = "Escolaridade dos Casos Notificados",
       subtitle = "2025",
       y = "Casos",
       x = NULL,
       caption = Fonte1) + 
  geom_label(aes(label = Casos, 
                 x =  Ordem, 
                 y = Casos),
             alpha = 0.5,
             vjust = 0.1)  +
  scale_x_discrete(labels = AUX$Escolaridade) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)
                                        )
                     ) +
  Theme() +
  theme(plot.caption = element_text(hjust = 0)) 

ggsave("/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/PECONHENTOS/Mapa_Escolaridade.png", 
       plot = RS_PECONHENTOS_GRAF_Escolaridade,     
       width = 24,             
       height = 20,           
       units = "cm",           
       dpi = 300) 

################    Mapas   ################################

#####################  Municipios que aplicam soro antiescorpiônico  ###################

SHAPEFILE_SINAP <- SHAPEFILE_REGIONAL %>% mutate(NUCLEO = case_when(NM_MUNICIP == "IVAIPORÃ" 
                                                                    | NM_MUNICIP == "NOVA TEBAS" 
                                                                    | NM_MUNICIP == "LUNARDELLI"
                                                                    | NM_MUNICIP == "SÃO JOÃO DO IVAÍ"
                                                                    | NM_MUNICIP == "MANOEL RIBAS" 
                                                                    ~ "Rede Descentralizada")
)

RS22_PECONHENTOS_MAPA_SORO_ESCORPIOES <- ggplot(SHAPEFILE_SINAP) +
  geom_sf(size = 1,
          aes(fill = NUCLEO),
          color = "black") +
  scale_fill_manual(name = "", 
                    values = c("Rede Descentralizada" = "#2F4F4F"),
                    na.value = "#ADD8E6") + 
  coord_sf(expand = FALSE) +
  geom_sf_label(data = SHAPEFILE_SINAP %>% filter(NUCLEO == "Rede Descentralizada"), 
                aes(label = NM_MUNICIP),
                label.padding = unit(0.5, "mm"),
                size = 3,
                position = "identity") +
  labs(title = "Rede Descentralizada para Atendimento de Acidentes Escorpiônicos
22ª Regional de Saúde",
       subtitle = "Municípios que Possuem Soro Antiescorpiônico em sua Rede de Frio",
       y = NULL,
       x = NULL,
       caption = Fonte1) +
  theme(
    legend.position = "bottom",  
    legend.title = element_text(face = "bold"),  
    legend.text = element_text(size = 10),  
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 0)
  )

ggsave("/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/PECONHENTOS/Mapa_Rede_escorpioes.png", 
       plot = RS22_PECONHENTOS_MAPA_SORO_ESCORPIOES,     
       width = 24,             
       height = 20,           
       units = "cm",           
       dpi = 300) 

################  DISCRIMINANDO A REDE   ######################################

##########  Ivaiporã

SHAPEFILE_IVAIPORA <- SHAPEFILE_SINAP %>% filter(NM_MUNICIP == "IVAIPORÃ" 
                                                 | NM_MUNICIP == "ARIRANHA DO IVAÍ" 
                                                 | NM_MUNICIP == "ARAPUÃ"
                                                 | NM_MUNICIP == "CRUZMALTINA"
                                                 | NM_MUNICIP == "LIDIANÓPOLIS"
                                                 | NM_MUNICIP == "JARDIM ALEGRE"
                                                 | NM_MUNICIP == "ROSÁRIO DO IVAÍ"
                                                 | NM_MUNICIP == "RIO BRANCO DO IVAÍ" 
)

RS22_PECONHENTOS_MAPA_REDE_IVAIPORA <- ggplot(SHAPEFILE_IVAIPORA) +
  geom_sf(aes(fill = NUCLEO), 
          color = "black", 
          size = 0.5) +
  scale_fill_manual(name = "", 
                    values = c("Rede Descentralizada" = "#2F4F4F"),
                    na.value = "#ADD8E6") + 
  coord_sf(expand = FALSE) +
  geom_sf_label(data = SHAPEFILE_IVAIPORA, 
                aes(label = NM_MUNICIP),
                size = 3,
                alpha = 0.5,
                position = "identity") +
  labs(title = "Rede Descentralizada para Atendimento de Acidentes Escorpiônicos
22ª Regional de Saúde",
       subtitle = "Municípios atendidos pelo Núcleo de Administração de Soro Antiescorpiônico de Ivaiporã",
       y = NULL,
       x = NULL,
       caption = Fonte1) +
  theme(
    legend.position = "bottom",  
    legend.title = element_text(face = "bold"),  
    legend.text = element_text(size = 10),  
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.caption = element_text(hjust = 0)
  ) 

ggsave("/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/PECONHENTOS/Mapa_Rede_escorpioes_IVAIPORA.png", 
       plot = RS22_PECONHENTOS_MAPA_REDE_IVAIPORA,     
       width = 24,             
       height = 20,           
       units = "cm",           
       dpi = 300) 

########################  São João do Ivaí   ##########################

SHAPEFILE_SJI <- SHAPEFILE_SINAP %>% filter(NM_MUNICIP == "SÃO JOÃO DO IVAÍ" |
                                              NM_MUNICIP == "GODOY MOREIRA")

RS22_PECONHENTOS_MAPA_SJI <- ggplot(SHAPEFILE_SJI) +
  geom_sf(aes(fill = NUCLEO), 
          color = "black", 
          size = 0.5) +
  scale_fill_manual(name = "", 
                    values = c("Rede Descentralizada" = "#2F4F4F"),
                    na.value = "#ADD8E6") + 
  coord_sf(expand = FALSE) +
  geom_sf_label(data = SHAPEFILE_SJI, 
                aes(label = NM_MUNICIP),
                size = 3,
                alpha = 0.5,
                position = "identity") +
  labs(title = "Rede Descentralizada para Atendimento de Acidentes Escorpiônicos
22ª Regional de Saúde",
       subtitle = "Municípios atendidos pelo Núcleo de Administração de Soro Antiescorpiônico de São João do Ivaí",
       y = NULL,
       x = NULL,
       caption = Fonte1) +
  theme(
    legend.position = "bottom",  
    legend.title = element_text(face = "bold"),  
    legend.text = element_text(size = 10),  
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)  
  ) 

ggsave("/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/PECONHENTOS/Mapa_Rede_escorpioes_SJI.png", 
       plot = RS22_PECONHENTOS_MAPA_SJI,     
       width = 26,             
       height = 26,           
       units = "cm",           
       dpi = 300) 

##############################   MANOEL RIBAS   ##############################

SHAPEFILE_MRI <- SHAPEFILE_SINAP %>% filter(NM_MUNICIP == "MANOEL RIBAS" |
                                              NM_MUNICIP == "CÂNDIDO DE ABREU" |
                                              NM_MUNICIP == "SANTA MARIA DO OESTE" |
                                              NM_MUNICIP == "MATO RICO")

RS22_PECONHENTOS_MAPA_MRI <- ggplot(SHAPEFILE_MRI) +
  geom_sf(aes(fill = NUCLEO), 
          color = "black", 
          size = 0.5) +
  scale_fill_manual(name = "", 
                    values = c("Rede Descentralizada" = "#2F4F4F"),
                    na.value = "#ADD8E6") + 
  coord_sf(expand = FALSE) +
  geom_sf_label(data = SHAPEFILE_MRI, 
                aes(label = NM_MUNICIP),
                size = 3,
                alpha = 0.5,
                position = "identity") +
  labs(title = "Rede Descentralizada para Atendimento de Acidentes Escorpiônicos
22ª Regional de Saúde",
       subtitle = "Municípios atendidos pelo Núcleo de Administração de Soro Antiescorpiônico de Manoel Ribas",
       y = NULL,
       x = NULL,
       caption = Fonte1) +
  theme(
    legend.position = "bottom",  
    legend.title = element_text(face = "bold"),  
    legend.text = element_text(size = 10),  
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)  
  ) 

ggsave("/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/PECONHENTOS/Mapa_Rede_escorpioes_MRI.png", 
       plot = RS22_PECONHENTOS_MAPA_MRI,     
       width = 24,             
       height = 20,           
       units = "cm",           
       dpi = 300) 

##########################  NOVA TEBAS   ###############################

##############################   NOVA TEBAS   ##############################

SHAPEFILE_NOVA_TEBAS <- SHAPEFILE_SINAP %>% filter(NM_MUNICIP == "NOVA TEBAS")

RS22_PECONHENTOS_MAPA_NOVA_TEBAS <- ggplot(SHAPEFILE_NOVA_TEBAS) +
  geom_sf(aes(fill = NUCLEO), 
          color = "black", 
          size = 0.5) +
  scale_fill_manual(name = "", 
                    values = c("Rede Descentralizada" = "#2F4F4F"),
                    na.value = "#ADD8E6") + 
  coord_sf(expand = FALSE) +
  geom_sf_label(data = SHAPEFILE_NOVA_TEBAS, 
                aes(label = NM_MUNICIP),
                size = 3,
                alpha = 0.5,
                position = "identity") +
  labs(title = "Rede Descentralizada para Atendimento de Acidentes Escorpiônicos
22ª Regional de Saúde",
       subtitle = "Núcleo de Administração de Soro Antiescorpiônico de Nova Tebas",
       y = NULL,
       x = NULL,
       caption = Fonte1) +
  theme(
    legend.position = "bottom",  
    legend.title = element_text(face = "bold"),  
    legend.text = element_text(size = 10),  
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)  
  ) 

ggsave("/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/PECONHENTOS/Mapa_Rede_escorpioes_NOVA_TEBAS.png", 
       plot = RS22_PECONHENTOS_MAPA_NOVA_TEBAS,     
       width = 24,             
       height = 25,           
       units = "cm",           
       dpi = 300) 

###########################  LUNARDELLI   ################################

##########################################################

SHAPEFILE_LUNARDELLI <- SHAPEFILE_SINAP %>% filter(NM_MUNICIP == "LUNARDELLI")

RS22_PECONHENTOS_MAPA_LUNARDELLI <- ggplot(SHAPEFILE_LUNARDELLI) +
  geom_sf(aes(fill = NUCLEO), 
          color = "black", 
          size = 0.5) +
  scale_fill_manual(name = "", 
                    values = c("Rede Descentralizada" = "#2F4F4F"),
                    na.value = "#ADD8E6") + 
  coord_sf(expand = FALSE) +
  geom_sf_label(data = SHAPEFILE_LUNARDELLI, 
                aes(label = NM_MUNICIP),
                size = 3,
                alpha = 0.5,
                position = "identity") +
  labs(title = "Rede Descentralizada para Atendimento de Acidentes Escorpiônicos
22ª Regional de Saúde",
       subtitle = "Núcleo de Administração de Soro Antiescorpiônico de Lunardelli",
       y = NULL,
       x = NULL,
       caption = Fonte1) +
  theme(
    legend.position = "bottom",  
    legend.title = element_text(face = "bold"),  
    legend.text = element_text(size = 10),  
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)  
  ) +
  annotation_north_arrow(style = north_arrow_fancy_orienteering())

ggsave("/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/PECONHENTOS/Mapa_Rede_escorpioes_LUNARDELLI.png", 
       plot = RS22_PECONHENTOS_MAPA_LUNARDELLI,     
       width = 24,             
       height = 20,           
       units = "cm",           
       dpi = 300) 

#####################    SINAP   ################################

SINAP <- read.csv(file = paste0("Base_de_Dados/PECONHENTOS/SINAP_ENCAMINHADOS.csv"),
                  header = TRUE,
                  sep = ",")

SINAP[,7] <- str_to_upper(SINAP[,7])

###############################################################

AUX <- data.frame(RS = BASE_IBGE[which(BASE_IBGE$RS == RS), 1])

AUX$Municipio <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Amostras <- NA

AUX$Causador_acidente <- NA

AUX$Aranha <- NA

AUX$Escorpiao <- NA

AUX$Lagarta <- NA

AUX$Serpente <- NA

AUX$Outros <- NA


###      For Loop para geração da tabela RS22_Extra       ###

for(i in SINAP[, 6]){
  
  AUX[which(AUX$COD_IBGE == i), 4] <- as.integer(SINAP %>% 
                                                   filter(Codigo_IBGE == i  
                                                   ) %>% 
                                                   count() 
  )
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAP %>% 
                                                   filter(Codigo_IBGE == i,
                                                          Causador_acidente == "S"
                                                   ) %>% 
                                                   count() 
  )
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAP %>% 
                                                   filter(Codigo_IBGE == i,
                                                          Animal == "Aranha"
                                                   ) %>% 
                                                   count() 
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAP %>% 
                                                   filter(Codigo_IBGE == i,
                                                          Animal == "Escorpião"
                                                   ) %>% 
                                                   count() 
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAP %>% 
                                                   filter(Codigo_IBGE == i,
                                                          Animal == "Lagartas/Taturana/Orugas"
                                                   ) %>% 
                                                   count() 
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAP %>% 
                                                   filter(Codigo_IBGE == i,
                                                          Animal == "Serpentes"
                                                   ) %>% 
                                                   count() 
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAP %>% 
                                                    filter(Codigo_IBGE == i,
                                                           Animal == "Outros"
                                                    ) %>% 
                                                    count() 
  )}

SINAP_GERAL <- AUX[-c(1, 3)]

SINAP_GERAL <-replace(SINAP_GERAL, is.na(SINAP_GERAL), "S/E")


#######  Municípios que possuem encontro de escorpião amarelo

SHAPEFILE_Escorpiao_Amarelo <- SHAPEFILE_REGIONAL %>% mutate(ESC_AMAR = case_when(NM_MUNICIP == "IVAIPORÃ" 
                                                                    | NM_MUNICIP == "JARDIM ALEGRE" 
                                                                    | NM_MUNICIP == "LUNARDELLI"
                                                                    | NM_MUNICIP == "SÃO JOÃO DO IVAÍ"
                                                                    | NM_MUNICIP == "MANOEL RIBAS" 
                                                                    ~ "Registro")
)

RS22_PECONHENTOS_MAPA_ESCORPIOES <- ggplot(SHAPEFILE_Escorpiao_Amarelo) +
  geom_sf(size = 1,
          aes(fill = ESC_AMAR),
          color = "black") +
  scale_fill_manual(name = "", 
                    values = c("Registro" = "#9ACD32"),
                    na.value = "#ADD8E6") + 
  coord_sf(expand = FALSE) +
  geom_sf_label(data = SHAPEFILE_Escorpiao_Amarelo %>% filter(ESC_AMAR == "Registro"), 
                aes(label = NM_MUNICIP),
                label.padding = unit(0.5, "mm"),
                size = 3,
                position = "identity") +
  labs(title = "Municípios da 22ª Regional de Saúde com Registro de Escorpiões Amarelos",
       y = NULL,
       x = NULL,
       caption = Fonte1) +
  theme(
    legend.position = "bottom",  
    legend.title = element_text(face = "bold"),  
    legend.text = element_text(size = 10),  
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 0)
  )

ggsave("/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/PECONHENTOS/Mapa_escorpioes_amarelos.png", 
       plot = RS22_PECONHENTOS_MAPA_ESCORPIOES,     
       width = 24,             
       height = 20,           
       units = "cm",           
       dpi = 300) 

################################################################################

SINAP_GERAL <- SINAP_GERAL %>%
  gt() %>%
  tab_header(title = md("**Tabela 01 - Total de Amostras Encaminhadas por Município**"),
             subtitle = "Amostras encaminhadas entre os anos de 2020 e 2025") %>%
  tab_options(heading.align = "left",
              column_labels.border.top.color = "black",
              column_labels.border.top.width = px(3)) %>%
  tab_spanner(label = "Animal",
              columns = c(4:8),
              id = "Animal") %>%
  tab_spanner(label = "Amostras Encaminhadas",
              columns = c(2:8),
              id = "AMostras Geral") %>%
  cols_align(align = "center", columns = c(2:8)) %>%
  cols_label(Municipio = "Município",
             Causador_acidente = "Causador de Acidente",
             Escorpiao = "Escorpião") %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels(everything())) %>%
  tab_footnote(footnote = "Nota: S/E = Sem Encaminhamentos") %>%
  tab_footnote(footnote = Fonte2) %>%
  tab_options(table.font.size = "small")

gtsave(data = SINAP_GERAL,
       filename = "/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/PECONHENTOS/SINAP_GERAL.png")
##############################################################################

AUX <- AUX %>% mutate(Cortes = cut(x = Amostras,
                                   breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, +Inf),
                                   labels = c("1 - 10", "11 - 20", "21 - 30", "31 - 40", "41 - 50", "51 - 60", "61 - 70", "71 - 80", "> 80")))

SHAPEFILE_SINAP <- left_join(SHAPEFILE_SINAP, AUX, by = c("NM_MUNICIP" = "Municipio"))

SHAPEFILE_SINAP$Cortes <- as.character(SHAPEFILE_SINAP$Cortes)

SHAPEFILE_SINAP$Cortes <-replace(SHAPEFILE_SINAP$Cortes, is.na(SHAPEFILE_SINAP$Cortes), "Sem Encaminhamentos")

RS_MAP_SINAP_AMOSTRAS <- ggplot(SHAPEFILE_SINAP) +
  geom_sf(aes(fill = Cortes),
          size = 0.5,
          color = "black") +
  scale_fill_manual(name = "Nº de Amostras Encaminhadas", values = c("1 - 10" = "#40E0D0",
                                                                     "11 - 20" = "#7FFFD4",
                                                                     "21 - 30" = "#66CDAA",
                                                                     "31 - 40" = "#8FBC8F",
                                                                     "41 - 50" = "#20B2AA",
                                                                     "51 - 60" = "#9ACD32",
                                                                     "61 - 70" = "#228B22",
                                                                     "71 - 80" = "#6B8E23",
                                                                     "> 80" = "#556B2F",
                                                                     "Sem Encaminhamentos" = "red")) +
  labs(title = "Amostras de Animais Peçonhentos Encaminhadas para o LABTAX",
       subtitle = " Número Total de Amostras Encaminhadas (2020 - 2025)",
       caption = Fonte2) +
  theme(
    legend.position = "bottom",  
    legend.title = element_text(face = "bold"),  
    legend.text = element_text(size = 10),  
    plot.title = element_text(hjust = 0, 
                              face = "bold", 
                              size = 14),
    plot.subtitle = element_text(hjust = 0),
    plot.caption = element_text(hjust =0)
  ) +
  annotation_north_arrow(style = north_arrow_fancy_orienteering())

ggsave("/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/PECONHENTOS/Mapa_SINAP_ENCAMINHADOS.png", 
       plot = RS_MAP_SINAP_AMOSTRAS,     
       width = 24,             
       height = 20,           
       units = "cm",           
       dpi = 300) 
#########################################################################

Tabela_SINAP <- SINAP[, c(1, 5, 7, 8, 12, 16, 17, 18, 26, 27) ]

Tabela_SINAP <- Tabela_SINAP %>%
  filter(str_detect(Tabela_SINAP$Coleta, "2025")) 

Tabela_SINAP <- Tabela_SINAP %>%
  mutate(Acidente = case_when(Causador_acidente == "S" ~ "Sim",
                              Causador_acidente == "N" ~ "Não"))

Tabela_SINAP <- Tabela_SINAP[, c(1, 3, 4, 11, 7, 8, 9, 10)]

Tabela_SINAP <- Tabela_SINAP %>%
  gt() %>%
  tab_header( title = md("**Tabela 01. Amostras de Animais Peçonhentos Encaminhadas ao DVVZI/LABTAX (2025)**")) %>%
  tab_options(heading.align = "left",
              column_labels.border.top.color = "black",
              column_labels.border.top.width = px(3)) %>%
  cols_align(align = "center") %>%
  cols_label(Municipio = "Município",
             Genero = "Gênero",
             Especie = "Espécie") %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels(everything())) %>%
  tab_footnote(footnote = Fonte2) %>%
  tab_options(table.font.size = "small")

gtsave(data = Tabela_SINAP,
       filename = "/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/PECONHENTOS/Tabela_SINAP.png")


################    Dados do Período atual    ##############

##############Serpentes por município

AUX <- RS22_PECONHENTOS_2025_TIPO_ACID[-nrow(RS22_PECONHENTOS_2025_TIPO_ACID),]

RS_PECONHENTOS_GRAF_TIPO_ACID_Municipios_Serpente <- ggplot(AUX, 
                                                   aes(x = Município)
                                                   ) +
  geom_bar(aes(y = Serpente),
           stat = "identity",
           color = "black",
           fill = "green",
           linewidth = 0.8) +
  labs(title = "Acidentes com Serpentes/Município - 2025",
       subtitle = "Município de residência",
       y = "Casos",
       x = NULL,
       caption = Fonte1) + 
  geom_label(aes(label = Serpente, 
                 x =  Município, 
                 y = Serpente),
             alpha = 0.5,
             vjust = 0.1)  +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)
  )
  ) +
  Theme() +
  theme(plot.caption = element_text(hjust = 0),
        axis.text.x = element_text(angle = 75)
        ) 

ggsave("/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_GRAF_TIPO_ACID_Municipios_Serpente.png", 
       plot = RS_PECONHENTOS_GRAF_TIPO_ACID_Municipios_Serpente,     
       width = 24,             
       height = 20,           
       units = "cm",           
       dpi = 300) 

##  Tipo de Serpente / Município

AUX <- RS22_PECONHENTOS_2025_TIPO_ACID_SERPENTE[-nrow(RS22_PECONHENTOS_2025_TIPO_ACID_SERPENTE),-c(1, 3, 4)]

RS_PECONHENTOS_TAB_TIPO_ACID_Municipio_Serpente <- AUX %>%
  gt() %>%
  tab_header(title = md("**Tipo de Acidente Ofídico por Município (2025)**")
             ) %>%
  tab_options(heading.align = "left",
              column_labels.border.top.color = "black",
              column_labels.border.top.width = px(3)) %>%
  tab_spanner(label = "Tipo de Acidente",
              columns = c(2:7),
              id = "Animal") %>%
    cols_align(align = "center", columns = c(2:7)) %>%
  cols_label(Município = "Município",
             Botropico = "Botrópico",
             Crotalico = "Crotálico",
             Elapidico = "Elapídico",
             Laquetico = "Laquético",
             Nao_Peconhenta = "Não Peçonhenta") %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels(everything()
                                            )) %>%
  tab_footnote(footnote = Fonte2) %>%
  tab_options(table.font.size = "small")

gtsave(data = RS_PECONHENTOS_TAB_TIPO_ACID_Municipio_Serpente,
       filename = "/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/RS_PECONHENTOS_TAB_TIPO_ACID_Municipio_Serpente.png")

##############Aranhas por município

AUX <- RS22_PECONHENTOS_2025_TIPO_ACID[-nrow(RS22_PECONHENTOS_2025_TIPO_ACID),]

RS_PECONHENTOS_GRAF_TIPO_ACID_Municipios_Aranha <- ggplot(AUX, 
                                                   aes(x = Município)
) +
  geom_bar(aes(y = Aranha),
           stat = "identity",
           color = "black",
           fill = "green",
           linewidth = 0.8) +
  labs(title = "Acidentes com Aranhas/Município - 2025",
       subtitle = "Município de residência",
       y = "Casos",
       x = NULL,
       caption = Fonte1) + 
  geom_label(aes(label = Aranha, 
                 x =  Município, 
                 y = Aranha),
             alpha = 0.5,
             vjust = 0.1)  +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)
  )
  ) +
  Theme() +
  theme(plot.caption = element_text(hjust = 0),
        axis.text.x = element_text(angle = 75)
  ) 

ggsave("/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_GRAF_TIPO_ACID_Municipios_Aranha.png", 
       plot = RS_PECONHENTOS_GRAF_TIPO_ACID_Municipios_Aranha,     
       width = 24,             
       height = 20,           
       units = "cm",           
       dpi = 300) 

##  Tipo de Aranha / Município

AUX <- RS22_PECONHENTOS_2025_TIPO_ACID_ARANHA[-nrow(RS22_PECONHENTOS_2025_TIPO_ACID_ARANHA),-c(1, 3, 4)]

RS_PECONHENTOS_TAB_TIPO_ACID_Municipio_Aranha <- AUX %>%
  gt() %>%
  tab_header(title = md("**Tipo de Acidente com Aranhas por Município (2025)**")
  ) %>%
  tab_options(heading.align = "left",
              column_labels.border.top.color = "black",
              column_labels.border.top.width = px(3)) %>%
  tab_spanner(label = "Tipo de Acidente",
              columns = c(2:6),
              id = "Animal") %>%
  cols_align(align = "center", columns = c(2:6)) %>%
  cols_label(Município = "Município",
             Outra_Aranha = "Outras") %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels(everything()
            )) %>%
  tab_footnote(footnote = Fonte2) %>%
  tab_options(table.font.size = "small")

gtsave(data = RS_PECONHENTOS_TAB_TIPO_ACID_Municipio_Aranha,
       filename = "/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/RS_PECONHENTOS_TAB_TIPO_ACID_Municipio_Aranha.png")

##############Abelhas por município

AUX <- RS22_PECONHENTOS_2025_TIPO_ACID[-nrow(RS22_PECONHENTOS_2025_TIPO_ACID),]

RS_PECONHENTOS_GRAF_TIPO_ACID_Municipios_Abelha <- ggplot(AUX, 
                                                          aes(x = Município)
) +
  geom_bar(aes(y = Abelha),
           stat = "identity",
           color = "black",
           fill = "green",
           linewidth = 0.8) +
  labs(title = "Acidentes com Abelhas/Município - 2025",
       subtitle = "Município de residência",
       y = "Casos",
       x = NULL,
       caption = Fonte1) + 
  geom_label(aes(label = Abelha, 
                 x =  Município, 
                 y = Abelha),
             alpha = 0.5,
             vjust = 0.1)  +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)
  )
  ) +
  Theme() +
  theme(plot.caption = element_text(hjust = 0),
        axis.text.x = element_text(angle = 75)
  ) 

ggsave("/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_GRAF_TIPO_ACID_Municipios_Abelha.png", 
       plot = RS_PECONHENTOS_GRAF_TIPO_ACID_Municipios_Abelha,     
       width = 24,             
       height = 20,           
       units = "cm",           
       dpi = 300)


##############  Escorpião por município

AUX <- RS22_PECONHENTOS_2025_TIPO_ACID[-nrow(RS22_PECONHENTOS_2025_TIPO_ACID),]

RS_PECONHENTOS_GRAF_TIPO_ACID_Municipios_Escorpiao <- ggplot(AUX, 
                                                          aes(x = Município)
) +
  geom_bar(aes(y = Escorpiao),
           stat = "identity",
           color = "black",
           fill = "green",
           linewidth = 0.8) +
  labs(title = "Acidentes com Escorpiões/Município - 2025",
       subtitle = "Município de residência",
       y = "Casos",
       x = NULL,
       caption = Fonte1) + 
  geom_label(aes(label = Escorpiao, 
                 x =  Município, 
                 y = Escorpiao),
             alpha = 0.5,
             vjust = 0.1)  +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)
  )
  ) +
  Theme() +
  theme(plot.caption = element_text(hjust = 0),
        axis.text.x = element_text(angle = 75)
  ) 

ggsave("/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_GRAF_TIPO_ACID_Municipios_Escorpiao.png", 
       plot = RS_PECONHENTOS_GRAF_TIPO_ACID_Municipios_Escorpiao,     
       width = 24,             
       height = 20,           
       units = "cm",           
       dpi = 300)


##############  Lagarta por município

AUX <- RS22_PECONHENTOS_2025_TIPO_ACID[-nrow(RS22_PECONHENTOS_2025_TIPO_ACID),]

RS_PECONHENTOS_GRAF_TIPO_ACID_Municipios_Lagarta <- ggplot(AUX, 
                                                             aes(x = Município)
) +
  geom_bar(aes(y = Lagarta),
           stat = "identity",
           color = "black",
           fill = "green",
           linewidth = 0.8) +
  labs(title = "Acidentes com Lagartas/Município - 2025",
       subtitle = "Município de residência",
       y = "Casos",
       x = NULL,
       caption = Fonte1) + 
  geom_label(aes(label = Lagarta, 
                 x =  Município, 
                 y = Lagarta),
             alpha = 0.5,
             vjust = 0.1)  +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)
  )
  ) +
  Theme() +
  theme(plot.caption = element_text(hjust = 0),
        axis.text.x = element_text(angle = 75)
  ) 

ggsave("/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_GRAF_TIPO_ACID_Municipios_Lagarta.png", 
       plot = RS_PECONHENTOS_GRAF_TIPO_ACID_Municipios_Lagarta,     
       width = 24,             
       height = 20,           
       units = "cm",           
       dpi = 300)

##  Tipo de lagarta / Município

AUX <- RS22_PECONHENTOS_2025_TIPO_ACID_LAGARTA[-nrow(RS22_PECONHENTOS_2025_TIPO_ACID_LAGARTA),-c(1, 3, 4)]

RS_PECONHENTOS_TAB_TIPO_ACID_Municipio_lagarta <- AUX %>%
  gt() %>%
  tab_header(title = md("**Tipo de Acidente com Lagartas por Município (2025)**")
  ) %>%
  tab_options(heading.align = "left",
              column_labels.border.top.color = "black",
              column_labels.border.top.width = px(3)) %>%
  tab_spanner(label = "Tipo de Acidente",
              columns = c(2:4),
              id = "Animal") %>%
  cols_align(align = "center", columns = c(2:4)) %>%
  cols_label(Município = "Município",
             Outra_Lagarta = "Outras") %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels(everything()
            )) %>%
  tab_footnote(footnote = Fonte2) %>%
  tab_options(table.font.size = "small")

gtsave(data = RS_PECONHENTOS_TAB_TIPO_ACID_Municipio_lagarta,
       filename = "/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/RS_PECONHENTOS_TAB_TIPO_ACID_Municipio_Lagarta.png")

##############  Outros por município

AUX <- RS22_PECONHENTOS_2025_TIPO_ACID[-nrow(RS22_PECONHENTOS_2025_TIPO_ACID),]

RS_PECONHENTOS_GRAF_TIPO_ACID_Municipios_Outros <- ggplot(AUX, 
                                                           aes(x = Município)
) +
  geom_bar(aes(y = Outros),
           stat = "identity",
           color = "black",
           fill = "green",
           linewidth = 0.8) +
  labs(title = "Acidentes com Outros Animais/Município - 2025",
       subtitle = "Município de residência",
       y = "Casos",
       x = NULL,
       caption = Fonte1) + 
  geom_label(aes(label = Outros, 
                 x =  Município, 
                 y = Outros),
             alpha = 0.5,
             vjust = 0.1)  +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)
  )
  ) +
  Theme() +
  theme(plot.caption = element_text(hjust = 0),
        axis.text.x = element_text(angle = 75)
  ) 

ggsave("/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_GRAF_TIPO_ACID_Municipios_Outros.png", 
       plot = RS_PECONHENTOS_GRAF_TIPO_ACID_Municipios_Outros,     
       width = 24,             
       height = 20,           
       units = "cm",           
       dpi = 300)

################  Local da picada

AUX <- RS22_PECONHENTOS_2025_LOCAL_PICADA[nrow(RS22_PECONHENTOS_2025_LOCAL_PICADA), - c(1:4)]

AUX <- t(AUX)

AUX <- as.data.frame(AUX)

AUX[, 2] <- c("Cabeça", "Braço", "Antebraço", "Mão", "Dedo (Mão)", "Tronco", "Coxa", "Perna", "Pé", "Dedo (Pé)", "Ignorado")

colnames(AUX) <- c("Casos", "Locais")

RS_PECONHENTOS_GRAF_LOCAL_PICADA_regional <- ggplot(AUX) +
  geom_bar(aes(x = Locais,
               y = Casos),
           stat = "identity",
           color = "black",
           fill = "green",
           linewidth = 0.8) +
  labs(title = "Local da Picada -22ª Regional de Saúde (2025)",
       y = "Casos",
       x = NULL,
       caption = Fonte1) + 
  geom_label(aes(label = Casos,
                 x = Locais,
                 y = Casos),
             alpha = 0.5,
             vjust = 0.1)  +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)
  )
  ) +
  Theme() +
  theme(plot.caption = element_text(hjust = 0),
        axis.text.x = element_text(angle = 75)
  ) 

ggsave("/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_GRAF_LOCAL_PICADA_regional.png", 
       plot = RS_PECONHENTOS_GRAF_LOCAL_PICADA_regional,     
       width = 24,             
       height = 20,           
       units = "cm",           
       dpi = 300)