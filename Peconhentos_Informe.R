rm(list =ls())

####Indicando Diretório de Trabalho.#####

setwd("/home/gustavo/Área de trabalho/Análise_de_Dados/")

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

Fonte <- "Fonte: SINAN. Base DBF acessada em 29/08/2025. Dados Sujeitos a alteração."
Fonte1 <- "Fonte: Controle Interno 22ª Regional de Saúde."
Fonte2 <- "Fonte: SINAP. Acesso em 29/08/2025. Dados sujeitos a alteração."
Fonte3 <- "Fonte: SIES. Acesso em 29/08/2025. Dados correspondentes a data da baixa do imunobiológico no sistema"

####   Estabelecendo o número de municípios em cada RS

nrow <- NROW(BASE_IBGE[which(BASE_IBGE$RS == RS), 1])

SINAP <- read.csv(file = paste0("Base_de_Dados/SINAP/SINAP_ENCAMINHADOS.csv"),
                  header = TRUE,
                  sep = ",")

CE_BASE_Notificados <- read.csv(file = paste0("Tabulacoes_R/Peconhentos/RS", RS,"_CE_BASE_Notificados.csv"),
                                header = TRUE,
                                sep = ",")

SIES_SORO_ANTIBOTROPICO <- read.csv(file = "Base_de_Dados/SIES/SIES_SORO_ANTIBOTROPICO.csv",
                                    header = TRUE,
                                    sep = ",")

SIES_SORO_ANTICROTALICO <- read.csv(file = "Base_de_Dados/SIES/SIES_SORO_ANTICROTALICO.csv",
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

RS_Serie_Histórica_Tempo_de_Atendimento <- read.csv(file = paste0("Tabulacoes_R/Peconhentos/RS_Serie_Histórica_Tempo_de_Atendimento.csv"),
                                                    header = TRUE,
                                                    sep = ",")

RS_Serie_Histórica_Local_da_Picada <- read.csv(file = paste0("Tabulacoes_R/Peconhentos/RS_Serie_Histórica_Local_da_Picada.csv"),
                                               header = TRUE,
                                               sep = ",")

RS_Serie_Histórica_Local_da_Picada_Aranha <- read.csv(file = paste0("Tabulacoes_R/Peconhentos/RS_Serie_Histórica_Local_da_Picada_Aranha.csv"),
                                                      header = TRUE,
                                                      sep = ",")

RS_Serie_Histórica_Local_da_Picada_Serpente <- read.csv(file = paste0("Tabulacoes_R/Peconhentos/RS_Serie_Histórica_Local_da_Picada_Serpente.csv"),
                                                        header = TRUE,
                                                        sep = ",")

RS_Serie_Histórica_Local_da_Picada_Escorpiao <- read.csv(file = paste0("Tabulacoes_R/Peconhentos/RS_Serie_Histórica_Local_da_Picada_Escorpiao.csv"),
                                                         header = TRUE,
                                                         sep = ",")

RS_Serie_Histórica_Local_da_Picada_Lagarta <- read.csv(file = paste0("Tabulacoes_R/Peconhentos/RS_Serie_Histórica_Local_da_Picada_Lagarta.csv"),
                                                       header = TRUE,
                                                       sep = ",")

RS_Serie_Historica_Obitos <- read.csv(file = paste0("Tabulacoes_R/Peconhentos/RS_Serie_Historica_Obitos.csv"),
                                         header = TRUE,
                                         sep = ",")

RS_Serie_Historica_Tipo_Acid <- read.csv(file = paste0("Tabulacoes_R/Peconhentos/RS_Serie_Historica_Tipo_Acid.csv"),
                                         header = TRUE,
                                         sep = ",")

RS_Serie_Historica_Tipo_Acid_Municipios <- read.csv(file = paste0("Tabulacoes_R/Peconhentos/RS_Serie_Historica_Tipo_Acid_Municipios.csv"),
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

SHAPEFILE_REGIONAL <- st_read("/home/gustavo/Área de trabalho/Análise_de_Dados/Shapefiles/22ª_Regional_de_Saúde/22ª_Regional_de_Saúde.shp")

SHAPEFILE_REGIONAL_Dissolvido <- st_read("/home/gustavo/Área de trabalho/Análise_de_Dados/Shapefiles/22ª_Regional_de_Saúde/22ª_Regional_de_Saúde_Dissolvido.shp")

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
                                          filter(ANT_MUNIC_ ==i &
                                                   SEM_PRI ==202501)%>%
                                          count()
  )
  
  AUX[which(AUX == i), 3] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                          filter(ANT_MUNIC_ ==i,
                                                 SEM_PRI ==202502) %>% 
                                          count()
  )
  
  AUX[which(AUX == i), 4] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                          filter(ANT_MUNIC_ ==i &
                                                   SEM_PRI ==202503) %>% 
                                          count()
  )
  
  AUX[which(AUX == i),5] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                         filter(ANT_MUNIC_ ==i &
                                                  SEM_PRI ==202504) %>% 
                                         count()
  )
  
  AUX[which(AUX == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                          filter(ANT_MUNIC_ ==i &
                                                   SEM_PRI ==202505) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                          filter(ANT_MUNIC_ ==i,
                                                 SEM_PRI ==202506) %>%
                                          count()
  )
  
  AUX[which(AUX == i), 8] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                          filter(ANT_MUNIC_ ==i,
                                                 SEM_PRI ==202507) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 9] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                          filter(ANT_MUNIC_ ==i,
                                                 SEM_PRI ==202508) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 10] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202509) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 11] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202510) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 12] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202511) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 13] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202512) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 14] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202513) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 15] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202514) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 16] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202515) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 17] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202516) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 18] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202517) %>%  
                                           count() 
  )
  
  AUX[which(AUX == i), 19] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202518) %>%      
                                           count() 
  )
  
  AUX[which(AUX == i), 20] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202519) %>%
                                           count() 
  )
  
  AUX[which(AUX == i),  21] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                            filter(ANT_MUNIC_ ==i,
                                                   SEM_PRI ==202520) %>%
                                            count() 
  )
  
  AUX[which(AUX == i), 22] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202521) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 23] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202522) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 24] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202523) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 25] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202524) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 26] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202525) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 27] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202526) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 28] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202527) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 29] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202528) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 30] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202529) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 31] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202530) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 32] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202531) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 33] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202532) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 34] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202533) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 35] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202534) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 36] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202535) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 37] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202536) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 38] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202537) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 39] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202538) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 40] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202539) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 41] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202540) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 42] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202541) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 43] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202542) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 44] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202543) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 45] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202544) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 46] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202545) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 47] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202546) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 48] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202547) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 49] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202548) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 50] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202549) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 51] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202550) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 52] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202551) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 53] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202552) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 54] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
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
                                          filter(ANT_MUNIC_ ==i &
                                                   SEM_PRI ==202501,
                                                 TP_ACIDENT == 1)%>%
                                          count()
  )
  
  AUX[which(AUX == i), 3] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                          filter(ANT_MUNIC_ ==i,
                                                 SEM_PRI ==202502,
                                                 TP_ACIDENT == 1) %>% 
                                          count()
  )
  
  AUX[which(AUX == i), 4] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                          filter(ANT_MUNIC_ ==i &
                                                   SEM_PRI ==202503,
                                                 TP_ACIDENT == 1) %>% 
                                          count()
  )
  
  AUX[which(AUX == i),5] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                         filter(ANT_MUNIC_ ==i &
                                                  SEM_PRI ==202504,
                                                TP_ACIDENT == 1) %>% 
                                         count()
  )
  
  AUX[which(AUX == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                          filter(ANT_MUNIC_ ==i &
                                                   SEM_PRI ==202505,
                                                 TP_ACIDENT == 1) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                          filter(ANT_MUNIC_ ==i,
                                                 SEM_PRI ==202506,
                                                 TP_ACIDENT == 1) %>%
                                          count()
  )
  
  AUX[which(AUX == i), 8] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                          filter(ANT_MUNIC_ ==i,
                                                 SEM_PRI ==202507,
                                                 TP_ACIDENT == 1) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 9] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                          filter(ANT_MUNIC_ ==i,
                                                 SEM_PRI ==202508,
                                                 TP_ACIDENT == 1) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 10] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202509,
                                                  TP_ACIDENT == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 11] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202510,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 12] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202511,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 13] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202512,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 14] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202513,
                                                  TP_ACIDENT == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 15] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202514,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 16] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202515,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 17] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202516,
                                                  TP_ACIDENT == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 18] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202517,
                                                  TP_ACIDENT == 1) %>%  
                                           count() 
  )
  
  AUX[which(AUX == i), 19] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202518,
                                                  TP_ACIDENT == 1) %>%      
                                           count() 
  )
  
  AUX[which(AUX == i), 20] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202519,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i),  21] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                            filter(ANT_MUNIC_ ==i,
                                                   SEM_PRI ==202520,
                                                   TP_ACIDENT == 1) %>%
                                            count() 
  )
  
  AUX[which(AUX == i), 22] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202521,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 23] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202522,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 24] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202523,
                                                  TP_ACIDENT == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 25] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202524,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 26] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202525,
                                                  TP_ACIDENT == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 27] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202526,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 28] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202527,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 29] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202528,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 30] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202529,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 31] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202530,
                                                  TP_ACIDENT == 1) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 32] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202531,
                                                  TP_ACIDENT == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 33] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202532,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 34] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202533,
                                                  TP_ACIDENT == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 35] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202534,
                                                  TP_ACIDENT == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 36] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202535,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 37] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202536,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 38] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202537,
                                                  TP_ACIDENT == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 39] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202538,
                                                  TP_ACIDENT == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 40] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202539,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 41] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202540,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 42] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202541,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 43] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202542,
                                                  TP_ACIDENT == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 44] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202543,
                                                  TP_ACIDENT == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 45] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202544,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 46] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202545,
                                                  TP_ACIDENT == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 47] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202546,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 48] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202547,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 49] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202548,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 50] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202549,
                                                  TP_ACIDENT == 1) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 51] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202550,
                                                  TP_ACIDENT == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 52] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202551,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 53] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202552,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 54] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
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
                                          filter(ANT_MUNIC_ ==i &
                                                   SEM_PRI ==202501,
                                                 TP_ACIDENT == 2)%>%
                                          count()
  )
  
  AUX[which(AUX == i), 3] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                          filter(ANT_MUNIC_ ==i,
                                                 SEM_PRI ==202502,
                                                 TP_ACIDENT == 2) %>% 
                                          count()
  )
  
  AUX[which(AUX == i), 4] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                          filter(ANT_MUNIC_ ==i &
                                                   SEM_PRI ==202503,
                                                 TP_ACIDENT == 2) %>% 
                                          count()
  )
  
  AUX[which(AUX == i),5] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                         filter(ANT_MUNIC_ ==i &
                                                  SEM_PRI ==202504,
                                                TP_ACIDENT == 2) %>% 
                                         count()
  )
  
  AUX[which(AUX == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                          filter(ANT_MUNIC_ ==i &
                                                   SEM_PRI ==202505,
                                                 TP_ACIDENT == 2) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                          filter(ANT_MUNIC_ ==i,
                                                 SEM_PRI ==202506,
                                                 TP_ACIDENT == 2) %>%
                                          count()
  )
  
  AUX[which(AUX == i), 8] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                          filter(ANT_MUNIC_ ==i,
                                                 SEM_PRI ==202507,
                                                 TP_ACIDENT == 2) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 9] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                          filter(ANT_MUNIC_ ==i,
                                                 SEM_PRI ==202508,
                                                 TP_ACIDENT == 2) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 10] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202509,
                                                  TP_ACIDENT == 2) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 11] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202510,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 12] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202511,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 13] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202512,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 14] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202513,
                                                  TP_ACIDENT == 2) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 15] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202514,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 16] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202515,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 17] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202516,
                                                  TP_ACIDENT == 2) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 18] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202517,
                                                  TP_ACIDENT == 2) %>%  
                                           count() 
  )
  
  AUX[which(AUX == i), 19] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202518,
                                                  TP_ACIDENT == 2) %>%      
                                           count() 
  )
  
  AUX[which(AUX == i), 20] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202519,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i),  21] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                            filter(ANT_MUNIC_ ==i,
                                                   SEM_PRI ==202520,
                                                   TP_ACIDENT == 2) %>%
                                            count() 
  )
  
  AUX[which(AUX == i), 22] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202521,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 23] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202522,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 24] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202523,
                                                  TP_ACIDENT == 2) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 25] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202524,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 26] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202525,
                                                  TP_ACIDENT == 2) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 27] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202526,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 28] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202527,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 29] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202528,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 30] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202529,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 31] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202530,
                                                  TP_ACIDENT == 2) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 32] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202531,
                                                  TP_ACIDENT == 2) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 33] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202532,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 34] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202533,
                                                  TP_ACIDENT == 2) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 35] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202534,
                                                  TP_ACIDENT == 2) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 36] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202535,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 37] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202536,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 38] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202537,
                                                  TP_ACIDENT == 2) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 39] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202538,
                                                  TP_ACIDENT == 2) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 40] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202539,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 41] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202540,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 42] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202541,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 43] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202542,
                                                  TP_ACIDENT == 2) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 44] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202543,
                                                  TP_ACIDENT == 2) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 45] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202544,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 46] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202545,
                                                  TP_ACIDENT == 2) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 47] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202546,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 48] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202547,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 49] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202548,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 50] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202549,
                                                  TP_ACIDENT == 2) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 51] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202550,
                                                  TP_ACIDENT == 2) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 52] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202551,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 53] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202552,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 54] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
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
                                          filter(ANT_MUNIC_ ==i &
                                                   SEM_PRI ==202501,
                                                 TP_ACIDENT == 3)%>%
                                          count()
  )
  
  AUX[which(AUX == i), 3] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                          filter(ANT_MUNIC_ ==i,
                                                 SEM_PRI ==202502,
                                                 TP_ACIDENT == 3) %>% 
                                          count()
  )
  
  AUX[which(AUX == i), 4] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                          filter(ANT_MUNIC_ ==i &
                                                   SEM_PRI ==202503,
                                                 TP_ACIDENT == 3) %>% 
                                          count()
  )
  
  AUX[which(AUX == i),5] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                         filter(ANT_MUNIC_ ==i &
                                                  SEM_PRI ==202504,
                                                TP_ACIDENT == 3) %>% 
                                         count()
  )
  
  AUX[which(AUX == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                          filter(ANT_MUNIC_ ==i &
                                                   SEM_PRI ==202505,
                                                 TP_ACIDENT == 3) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                          filter(ANT_MUNIC_ ==i,
                                                 SEM_PRI ==202506,
                                                 TP_ACIDENT == 3) %>%
                                          count()
  )
  
  AUX[which(AUX == i), 8] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                          filter(ANT_MUNIC_ ==i,
                                                 SEM_PRI ==202507,
                                                 TP_ACIDENT == 3) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 9] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                          filter(ANT_MUNIC_ ==i,
                                                 SEM_PRI ==202508,
                                                 TP_ACIDENT == 3) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 10] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202509,
                                                  TP_ACIDENT == 3) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 11] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202510,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 12] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202511,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 13] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202512,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 14] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202513,
                                                  TP_ACIDENT == 3) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 15] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202514,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 16] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202515,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 17] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202516,
                                                  TP_ACIDENT == 3) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 18] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202517,
                                                  TP_ACIDENT == 3) %>%  
                                           count() 
  )
  
  AUX[which(AUX == i), 19] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202518,
                                                  TP_ACIDENT == 3) %>%      
                                           count() 
  )
  
  AUX[which(AUX == i), 20] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202519,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i),  21] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                            filter(ANT_MUNIC_ ==i,
                                                   SEM_PRI ==202520,
                                                   TP_ACIDENT == 3) %>%
                                            count() 
  )
  
  AUX[which(AUX == i), 22] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202521,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 23] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202522,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 24] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202523,
                                                  TP_ACIDENT == 3) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 25] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202524,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 26] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202525,
                                                  TP_ACIDENT == 3) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 27] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202526,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 28] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202527,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 29] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202528,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 30] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202529,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 31] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202530,
                                                  TP_ACIDENT == 3) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 32] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202531,
                                                  TP_ACIDENT == 3) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 33] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202532,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 34] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202533,
                                                  TP_ACIDENT == 3) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 35] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202534,
                                                  TP_ACIDENT == 3) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 36] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202535,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 37] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202536,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 38] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202537,
                                                  TP_ACIDENT == 3) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 39] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202538,
                                                  TP_ACIDENT == 3) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 40] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202539,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 41] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202540,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 42] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202541,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 43] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202542,
                                                  TP_ACIDENT == 3) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 44] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202543,
                                                  TP_ACIDENT == 3) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 45] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202544,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 46] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202545,
                                                  TP_ACIDENT == 3) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 47] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202546,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 48] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202547,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 49] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202548,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 50] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202549,
                                                  TP_ACIDENT == 3) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 51] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202550,
                                                  TP_ACIDENT == 3) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 52] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202551,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 53] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202552,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 54] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
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
                                          filter(ANT_MUNIC_ ==i &
                                                   SEM_PRI ==202501,
                                                 TP_ACIDENT == 5)%>%
                                          count()
  )
  
  AUX[which(AUX == i), 3] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                          filter(ANT_MUNIC_ ==i,
                                                 SEM_PRI ==202502,
                                                 TP_ACIDENT == 5) %>% 
                                          count()
  )
  
  AUX[which(AUX == i), 4] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                          filter(ANT_MUNIC_ ==i &
                                                   SEM_PRI ==202503,
                                                 TP_ACIDENT == 5) %>% 
                                          count()
  )
  
  AUX[which(AUX == i),5] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                         filter(ANT_MUNIC_ ==i &
                                                  SEM_PRI ==202504,
                                                TP_ACIDENT == 5) %>% 
                                         count()
  )
  
  AUX[which(AUX == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                          filter(ANT_MUNIC_ ==i &
                                                   SEM_PRI ==202505,
                                                 TP_ACIDENT == 5) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                          filter(ANT_MUNIC_ ==i,
                                                 SEM_PRI ==202506,
                                                 TP_ACIDENT == 5) %>%
                                          count()
  )
  
  AUX[which(AUX == i), 8] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                          filter(ANT_MUNIC_ ==i,
                                                 SEM_PRI ==202507,
                                                 TP_ACIDENT == 5) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 9] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                          filter(ANT_MUNIC_ ==i,
                                                 SEM_PRI ==202508,
                                                 TP_ACIDENT == 5) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 10] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202509,
                                                  TP_ACIDENT == 5) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 11] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202510,
                                                  TP_ACIDENT == 5) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 12] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202511,
                                                  TP_ACIDENT == 5) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 13] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202512,
                                                  TP_ACIDENT == 5) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 14] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202513,
                                                  TP_ACIDENT == 5) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 15] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202514,
                                                  TP_ACIDENT == 5) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 16] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202515,
                                                  TP_ACIDENT == 5) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 17] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202516,
                                                  TP_ACIDENT == 5) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 18] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202517,
                                                  TP_ACIDENT == 5) %>%  
                                           count() 
  )
  
  AUX[which(AUX == i), 19] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202518,
                                                  TP_ACIDENT == 5) %>%      
                                           count() 
  )
  
  AUX[which(AUX == i), 20] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202519,
                                                  TP_ACIDENT == 5) %>%
                                           count() 
  )
  
  AUX[which(AUX == i),  21] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                            filter(ANT_MUNIC_ ==i,
                                                   SEM_PRI ==202520,
                                                   TP_ACIDENT == 5) %>%
                                            count() 
  )
  
  AUX[which(AUX == i), 22] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202521,
                                                  TP_ACIDENT == 5) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 23] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202522,
                                                  TP_ACIDENT == 5) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 24] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202523,
                                                  TP_ACIDENT == 5) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 25] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202524,
                                                  TP_ACIDENT == 5) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 26] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202525,
                                                  TP_ACIDENT == 5) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 27] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202526,
                                                  TP_ACIDENT == 5) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 28] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202527,
                                                  TP_ACIDENT == 5) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 29] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202528,
                                                  TP_ACIDENT == 5) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 30] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202529,
                                                  TP_ACIDENT == 5) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 31] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202530,
                                                  TP_ACIDENT == 5) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 32] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202531,
                                                  TP_ACIDENT == 5) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 33] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202532,
                                                  TP_ACIDENT == 5) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 34] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202533,
                                                  TP_ACIDENT == 5) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 35] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202534,
                                                  TP_ACIDENT == 5) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 36] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202535,
                                                  TP_ACIDENT == 5) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 37] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202536,
                                                  TP_ACIDENT == 5) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 38] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202537,
                                                  TP_ACIDENT == 5) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 39] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202538,
                                                  TP_ACIDENT == 5) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 40] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202539,
                                                  TP_ACIDENT == 5) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 41] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202540,
                                                  TP_ACIDENT == 5) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 42] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202541,
                                                  TP_ACIDENT == 5) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 43] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202542,
                                                  TP_ACIDENT == 5) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 44] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202543,
                                                  TP_ACIDENT == 5) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 45] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202544,
                                                  TP_ACIDENT == 5) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 46] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202545,
                                                  TP_ACIDENT == 5) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 47] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202546,
                                                  TP_ACIDENT == 5) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 48] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202547,
                                                  TP_ACIDENT == 5) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 49] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202548,
                                                  TP_ACIDENT == 5) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 50] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202549,
                                                  TP_ACIDENT == 5) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 51] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202550,
                                                  TP_ACIDENT == 5) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 52] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202551,
                                                  TP_ACIDENT == 5) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 53] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202552,
                                                  TP_ACIDENT == 5) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 54] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
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
                                          filter(ANT_MUNIC_ ==i &
                                                   SEM_PRI ==202501,
                                                 TP_ACIDENT == 4)%>%
                                          count()
  )
  
  AUX[which(AUX == i), 3] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                          filter(ANT_MUNIC_ ==i,
                                                 SEM_PRI ==202502,
                                                 TP_ACIDENT == 4) %>% 
                                          count()
  )
  
  AUX[which(AUX == i), 4] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                          filter(ANT_MUNIC_ ==i &
                                                   SEM_PRI ==202503,
                                                 TP_ACIDENT == 4) %>% 
                                          count()
  )
  
  AUX[which(AUX == i),5] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                         filter(ANT_MUNIC_ ==i &
                                                  SEM_PRI ==202504,
                                                TP_ACIDENT == 4) %>% 
                                         count()
  )
  
  AUX[which(AUX == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                          filter(ANT_MUNIC_ ==i &
                                                   SEM_PRI ==202505,
                                                 TP_ACIDENT == 4) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                          filter(ANT_MUNIC_ ==i,
                                                 SEM_PRI ==202506,
                                                 TP_ACIDENT == 4) %>%
                                          count()
  )
  
  AUX[which(AUX == i), 8] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                          filter(ANT_MUNIC_ ==i,
                                                 SEM_PRI ==202507,
                                                 TP_ACIDENT == 4) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 9] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                          filter(ANT_MUNIC_ ==i,
                                                 SEM_PRI ==202508,
                                                 TP_ACIDENT == 4) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 10] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202509,
                                                  TP_ACIDENT == 4) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 11] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202510,
                                                  TP_ACIDENT == 4) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 12] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202511,
                                                  TP_ACIDENT == 4) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 13] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202512,
                                                  TP_ACIDENT == 4) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 14] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202513,
                                                  TP_ACIDENT == 4) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 15] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202514,
                                                  TP_ACIDENT == 4) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 16] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202515,
                                                  TP_ACIDENT == 4) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 17] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202516,
                                                  TP_ACIDENT == 4) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 18] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202517,
                                                  TP_ACIDENT == 4) %>%  
                                           count() 
  )
  
  AUX[which(AUX == i), 19] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202518,
                                                  TP_ACIDENT == 4) %>%      
                                           count() 
  )
  
  AUX[which(AUX == i), 20] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202519,
                                                  TP_ACIDENT == 4) %>%
                                           count() 
  )
  
  AUX[which(AUX == i),  21] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                            filter(ANT_MUNIC_ ==i,
                                                   SEM_PRI ==202520,
                                                   TP_ACIDENT == 4) %>%
                                            count() 
  )
  
  AUX[which(AUX == i), 22] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202521,
                                                  TP_ACIDENT == 4) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 23] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202522,
                                                  TP_ACIDENT == 4) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 24] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202523,
                                                  TP_ACIDENT == 4) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 25] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202524,
                                                  TP_ACIDENT == 4) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 26] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202525,
                                                  TP_ACIDENT == 4) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 27] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202526,
                                                  TP_ACIDENT == 4) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 28] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202527,
                                                  TP_ACIDENT == 4) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 29] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202528,
                                                  TP_ACIDENT == 4) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 30] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202529,
                                                  TP_ACIDENT == 4) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 31] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202530,
                                                  TP_ACIDENT == 4) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 32] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202531,
                                                  TP_ACIDENT == 4) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 33] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202532,
                                                  TP_ACIDENT == 4) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 34] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202533,
                                                  TP_ACIDENT == 4) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 35] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202534,
                                                  TP_ACIDENT == 4) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 36] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202535,
                                                  TP_ACIDENT == 4) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 37] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202536,
                                                  TP_ACIDENT == 4) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 38] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202537,
                                                  TP_ACIDENT == 4) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 39] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202538,
                                                  TP_ACIDENT == 4) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 40] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202539,
                                                  TP_ACIDENT == 4) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 41] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202540,
                                                  TP_ACIDENT == 4) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 42] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202541,
                                                  TP_ACIDENT == 4) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 43] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202542,
                                                  TP_ACIDENT == 4) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 44] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202543,
                                                  TP_ACIDENT == 4) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 45] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202544,
                                                  TP_ACIDENT == 4) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 46] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202545,
                                                  TP_ACIDENT == 4) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 47] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202546,
                                                  TP_ACIDENT == 4) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 48] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202547,
                                                  TP_ACIDENT == 4) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 49] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202548,
                                                  TP_ACIDENT == 4) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 50] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202549,
                                                  TP_ACIDENT == 4) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 51] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202550,
                                                  TP_ACIDENT == 4) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 52] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202551,
                                                  TP_ACIDENT == 4) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 53] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202552,
                                                  TP_ACIDENT == 4) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 54] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
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
                                          filter(ANT_MUNIC_ ==i &
                                                   SEM_PRI ==202501,
                                                 TP_ACIDENT == 6)%>%
                                          count()
  )
  
  AUX[which(AUX == i), 3] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                          filter(ANT_MUNIC_ ==i,
                                                 SEM_PRI ==202502,
                                                 TP_ACIDENT == 6) %>% 
                                          count()
  )
  
  AUX[which(AUX == i), 4] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                          filter(ANT_MUNIC_ ==i &
                                                   SEM_PRI ==202503,
                                                 TP_ACIDENT == 6) %>% 
                                          count()
  )
  
  AUX[which(AUX == i),5] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                         filter(ANT_MUNIC_ ==i &
                                                  SEM_PRI ==202504,
                                                TP_ACIDENT == 6) %>% 
                                         count()
  )
  
  AUX[which(AUX == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                          filter(ANT_MUNIC_ ==i &
                                                   SEM_PRI ==202505,
                                                 TP_ACIDENT == 6) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                          filter(ANT_MUNIC_ ==i,
                                                 SEM_PRI ==202506,
                                                 TP_ACIDENT == 6) %>%
                                          count()
  )
  
  AUX[which(AUX == i), 8] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                          filter(ANT_MUNIC_ ==i,
                                                 SEM_PRI ==202507,
                                                 TP_ACIDENT == 6) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 9] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                          filter(ANT_MUNIC_ ==i,
                                                 SEM_PRI ==202508,
                                                 TP_ACIDENT == 6) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 10] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202509,
                                                  TP_ACIDENT == 6) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 11] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202510,
                                                  TP_ACIDENT == 6) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 12] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202511,
                                                  TP_ACIDENT == 6) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 13] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202512,
                                                  TP_ACIDENT == 6) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 14] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202513,
                                                  TP_ACIDENT == 6) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 15] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202514,
                                                  TP_ACIDENT == 6) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 16] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202515,
                                                  TP_ACIDENT == 6) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 17] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202516,
                                                  TP_ACIDENT == 6) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 18] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202517,
                                                  TP_ACIDENT == 6) %>%  
                                           count() 
  )
  
  AUX[which(AUX == i), 19] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202518,
                                                  TP_ACIDENT == 6) %>%      
                                           count() 
  )
  
  AUX[which(AUX == i), 20] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202519,
                                                  TP_ACIDENT == 6) %>%
                                           count() 
  )
  
  AUX[which(AUX == i),  21] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                            filter(ANT_MUNIC_ ==i,
                                                   SEM_PRI ==202520,
                                                   TP_ACIDENT == 6) %>%
                                            count() 
  )
  
  AUX[which(AUX == i), 22] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202521,
                                                  TP_ACIDENT == 6) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 23] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202522,
                                                  TP_ACIDENT == 6) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 24] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202523,
                                                  TP_ACIDENT == 6) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 25] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202524,
                                                  TP_ACIDENT == 6) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 26] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202525,
                                                  TP_ACIDENT == 6) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 27] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202526,
                                                  TP_ACIDENT == 6) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 28] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202527,
                                                  TP_ACIDENT == 6) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 29] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202528,
                                                  TP_ACIDENT == 6) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 30] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202529,
                                                  TP_ACIDENT == 6) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 31] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202530,
                                                  TP_ACIDENT == 6) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 32] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202531,
                                                  TP_ACIDENT == 6) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 33] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202532,
                                                  TP_ACIDENT == 6) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 34] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202533,
                                                  TP_ACIDENT == 6) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 35] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202534,
                                                  TP_ACIDENT == 6) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 36] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202535,
                                                  TP_ACIDENT == 6) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 37] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202536,
                                                  TP_ACIDENT == 6) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 38] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202537,
                                                  TP_ACIDENT == 6) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 39] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202538,
                                                  TP_ACIDENT == 6) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 40] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202539,
                                                  TP_ACIDENT == 6) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 41] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202540,
                                                  TP_ACIDENT == 6) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 42] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202541,
                                                  TP_ACIDENT == 6) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 43] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202542,
                                                  TP_ACIDENT == 6) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 44] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202543,
                                                  TP_ACIDENT == 6) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 45] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202544,
                                                  TP_ACIDENT == 6) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 46] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202545,
                                                  TP_ACIDENT == 6) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 47] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202546,
                                                  TP_ACIDENT == 6) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 48] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202547,
                                                  TP_ACIDENT == 6) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 49] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202548,
                                                  TP_ACIDENT == 6) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 50] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i,
                                                  SEM_PRI ==202549,
                                                  TP_ACIDENT == 6) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 51] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202550,
                                                  TP_ACIDENT == 6) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 52] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202551,
                                                  TP_ACIDENT == 6) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 53] <- as.integer(SINAN_PECONHENTOS_2025 %>%
                                           filter(ANT_MUNIC_ ==i &
                                                    SEM_PRI ==202552,
                                                  TP_ACIDENT == 6) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 54] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                           filter(ANT_MUNIC_ ==i,
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
                                                   filter(ANT_MUNIC_ == i) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i,
                                                          ANT_ZONA == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i,
                                                          ANT_ZONA == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i,
                                                          ANT_ZONA == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i,
                                                          ANT_ZONA == 9 
                                                          |
                                                            is.na(CS_ZONA)) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ANT_MUNIC_ ==i,
                                                           CS_SEXO == "F") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ANT_MUNIC_ ==i,
                                                           CS_SEXO == "M") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ANT_MUNIC_ ==i,  
                                                           NU_IDADE_N <= 3012) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ANT_MUNIC_ ==i,  
                                                           NU_IDADE_N > 4000 
                                                           & 
                                                             NU_IDADE_N <= 4005) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ANT_MUNIC_ ==i,
                                                           NU_IDADE_N > 4005 
                                                           & 
                                                             NU_IDADE_N <= 4012) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ANT_MUNIC_ ==i,
                                                           NU_IDADE_N > 4012 
                                                           & 
                                                             NU_IDADE_N <= 4018) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 16] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ANT_MUNIC_ ==i,
                                                           NU_IDADE_N > 4018 
                                                           & 
                                                             NU_IDADE_N <= 4059) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 17] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ANT_MUNIC_ ==i,
                                                           NU_IDADE_N > 4059) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 18] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ANT_MUNIC_ ==i,
                                                           CS_ESCOL_N == "00") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 19] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ANT_MUNIC_ ==i,
                                                           CS_ESCOL_N == "01" 
                                                           | 
                                                             CS_ESCOL_N == "02" 
                                                           | 
                                                             CS_ESCOL_N == "03") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 20] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ANT_MUNIC_ ==i,
                                                           CS_ESCOL_N == "04") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 21] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ANT_MUNIC_ ==i,
                                                           CS_ESCOL_N == "05") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 22] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ANT_MUNIC_ ==i,
                                                           CS_ESCOL_N == "06") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 23] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ANT_MUNIC_ ==i,
                                                           CS_ESCOL_N == "07") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 24] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ANT_MUNIC_ ==i,
                                                           CS_ESCOL_N == "08") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 25] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ANT_MUNIC_ ==i,
                                                           CS_ESCOL_N == "10") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 26] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ANT_MUNIC_ ==i,
                                                           CS_ESCOL_N == "09") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 27] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ANT_MUNIC_ ==i,
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
                                                    filter(ANT_MUNIC_ ==i,
                                                           CS_GESTANT == "5") %>% 
                                                    count()
  )
  AUX[which(AUX$COD_IBGE == i), 29] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ANT_MUNIC_ ==i,
                                                           CS_GESTANT == "6") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 30] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ANT_MUNIC_ ==i,
                                                           CS_GESTANT == "9") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 31] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ANT_MUNIC_ ==i,
                                                           CS_RACA == "1") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 32] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ANT_MUNIC_ ==i,
                                                           CS_RACA == "2") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 33] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ANT_MUNIC_ ==i,
                                                           CS_RACA == "3") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 34] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ANT_MUNIC_ ==i,
                                                           CS_RACA == "4") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 35] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ANT_MUNIC_ ==i,
                                                           CS_RACA == "5") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 36] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ANT_MUNIC_ ==i,
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
                                                   filter(ANT_MUNIC_ == i,
                                                          ANT_TEMPO_ == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            ANT_TEMPO_ == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            ANT_TEMPO_ == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            ANT_TEMPO_ == 4) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            ANT_TEMPO_ == 5) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ANT_MUNIC_ ==i &
                                                             ANT_TEMPO_ == 6) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ANT_MUNIC_ ==i &
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
                                                   filter(ANT_MUNIC_ == i,
                                                          ANT_LOCA_1 == "01") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            ANT_LOCA_1 == "02") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            ANT_LOCA_1 == "03") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            ANT_LOCA_1 == "04") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            ANT_LOCA_1 == "05") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ANT_MUNIC_ ==i &
                                                             ANT_LOCA_1 == "06") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ANT_MUNIC_ ==i &
                                                             ANT_LOCA_1 == "07") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ANT_MUNIC_ ==i &
                                                             ANT_LOCA_1 == "08") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ANT_MUNIC_ ==i &
                                                             ANT_LOCA_1 == "09") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ANT_MUNIC_ ==i &
                                                             ANT_LOCA_1 == "10") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ANT_MUNIC_ ==i &
                                                             ANT_LOCA_1 == "99") %>%   
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

###############    Local da Picada  Aranha   #####################

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
                                                   filter(ANT_MUNIC_ == i,
                                                          ANT_LOCA_1 == "01",
                                                          TP_ACIDENT == "2") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            ANT_LOCA_1 == "02",
                                                          TP_ACIDENT == "2") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            ANT_LOCA_1 == "03",
                                                          TP_ACIDENT == "2") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            ANT_LOCA_1 == "04",
                                                          TP_ACIDENT == "2") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            ANT_LOCA_1 == "05",
                                                          TP_ACIDENT == "2") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ANT_MUNIC_ ==i &
                                                             ANT_LOCA_1 == "06",
                                                           TP_ACIDENT == "2") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ANT_MUNIC_ ==i &
                                                             ANT_LOCA_1 == "07",
                                                           TP_ACIDENT == "2") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ANT_MUNIC_ ==i &
                                                             ANT_LOCA_1 == "08",
                                                           TP_ACIDENT == "2") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ANT_MUNIC_ ==i &
                                                             ANT_LOCA_1 == "09",
                                                           TP_ACIDENT == "2") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ANT_MUNIC_ ==i &
                                                             ANT_LOCA_1 == "10",
                                                           TP_ACIDENT == "2") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ANT_MUNIC_ ==i &
                                                             ANT_LOCA_1 == "99",
                                                           TP_ACIDENT == "2") %>%   
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

RS_Serie_Histórica_Local_da_Picada_Aranha[nrow(RS_Serie_Histórica_Local_da_Picada_Aranha) +1, ] <- AUX[nrow(AUX),]

RS_Serie_Histórica_Local_da_Picada_Aranha[nrow(RS_Serie_Histórica_Local_da_Picada_Aranha), 1] <- "2025"

assign(paste0("RS", RS, "_PECONHENTOS_2025_LOCAL_PICADA_ARANHA"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2025_LOCAL_PICADA_ARANHA"), AUX), 
           paste0("Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2025_LOCAL_PICADA_ARANHA.csv"), 
           row.names = FALSE)

###############    Local da Picada  ASerpente   #####################

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
                                                   filter(ANT_MUNIC_ == i,
                                                          ANT_LOCA_1 == "01",
                                                          TP_ACIDENT == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            ANT_LOCA_1 == "02",
                                                          TP_ACIDENT == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            ANT_LOCA_1 == "03",
                                                          TP_ACIDENT == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            ANT_LOCA_1 == "04",
                                                          TP_ACIDENT == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            ANT_LOCA_1 == "05",
                                                          TP_ACIDENT == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ANT_MUNIC_ ==i &
                                                             ANT_LOCA_1 == "06",
                                                           TP_ACIDENT == "1") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ANT_MUNIC_ ==i &
                                                             ANT_LOCA_1 == "07",
                                                           TP_ACIDENT == "1") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ANT_MUNIC_ ==i &
                                                             ANT_LOCA_1 == "08",
                                                           TP_ACIDENT == "1") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ANT_MUNIC_ ==i &
                                                             ANT_LOCA_1 == "09",
                                                           TP_ACIDENT == "1") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ANT_MUNIC_ ==i &
                                                             ANT_LOCA_1 == "10",
                                                           TP_ACIDENT == "1") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ANT_MUNIC_ ==i &
                                                             ANT_LOCA_1 == "99",
                                                           TP_ACIDENT == "1") %>%   
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

RS_Serie_Histórica_Local_da_Picada_Serpente[nrow(RS_Serie_Histórica_Local_da_Picada_Serpente) +1, ] <- AUX[nrow(AUX),]

RS_Serie_Histórica_Local_da_Picada_Serpente[nrow(RS_Serie_Histórica_Local_da_Picada_Serpente), 1] <- "2025"

assign(paste0("RS", RS, "_PECONHENTOS_2025_LOCAL_PICADA_SERPENTE"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2025_LOCAL_PICADA_SERPENTE"), AUX), 
           paste0("Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2025_LOCAL_PICADA_SERPENTE.csv"), 
           row.names = FALSE)

###############    Local da Picada  Escorpião   #####################

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
                                                   filter(ANT_MUNIC_ == i,
                                                          ANT_LOCA_1 == "01",
                                                          TP_ACIDENT == "3") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            ANT_LOCA_1 == "02",
                                                          TP_ACIDENT == "3") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            ANT_LOCA_1 == "03",
                                                          TP_ACIDENT == "3") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            ANT_LOCA_1 == "04",
                                                          TP_ACIDENT == "3") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            ANT_LOCA_1 == "05",
                                                          TP_ACIDENT == "3") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ANT_MUNIC_ ==i &
                                                             ANT_LOCA_1 == "06",
                                                           TP_ACIDENT == "3") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ANT_MUNIC_ ==i &
                                                             ANT_LOCA_1 == "07",
                                                           TP_ACIDENT == "3") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ANT_MUNIC_ ==i &
                                                             ANT_LOCA_1 == "08",
                                                           TP_ACIDENT == "3") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ANT_MUNIC_ ==i &
                                                             ANT_LOCA_1 == "09",
                                                           TP_ACIDENT == "3") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ANT_MUNIC_ ==i &
                                                             ANT_LOCA_1 == "10",
                                                           TP_ACIDENT == "3") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ANT_MUNIC_ ==i &
                                                             ANT_LOCA_1 == "99",
                                                           TP_ACIDENT == "3") %>%   
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

RS_Serie_Histórica_Local_da_Picada_Escorpiao[nrow(RS_Serie_Histórica_Local_da_Picada_Escorpiao) +1, ] <- AUX[nrow(AUX),]

RS_Serie_Histórica_Local_da_Picada_Escorpiao[nrow(RS_Serie_Histórica_Local_da_Picada_Escorpiao), 1] <- "2025"

assign(paste0("RS", RS, "_PECONHENTOS_2025_LOCAL_PICADA_ESCORPIAO"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2025_LOCAL_PICADA_ESCORPIAO"), AUX), 
           paste0("Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2025_LOCAL_PICADA_ESCORPIAO.csv"), 
           row.names = FALSE)

###############    Local da Picada  Lagarta   #####################

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
                                                   filter(ANT_MUNIC_ == i,
                                                          ANT_LOCA_1 == "01",
                                                          TP_ACIDENT == "4") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            ANT_LOCA_1 == "02",
                                                          TP_ACIDENT == "4") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            ANT_LOCA_1 == "03",
                                                          TP_ACIDENT == "4") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            ANT_LOCA_1 == "04",
                                                          TP_ACIDENT == "4") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            ANT_LOCA_1 == "05",
                                                          TP_ACIDENT == "4") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ANT_MUNIC_ ==i &
                                                             ANT_LOCA_1 == "06",
                                                           TP_ACIDENT == "4") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ANT_MUNIC_ ==i &
                                                             ANT_LOCA_1 == "07",
                                                           TP_ACIDENT == "4") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ANT_MUNIC_ ==i &
                                                             ANT_LOCA_1 == "08",
                                                           TP_ACIDENT == "4") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ANT_MUNIC_ ==i &
                                                             ANT_LOCA_1 == "09",
                                                           TP_ACIDENT == "4") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ANT_MUNIC_ ==i &
                                                             ANT_LOCA_1 == "10",
                                                           TP_ACIDENT == "4") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ANT_MUNIC_ ==i &
                                                             ANT_LOCA_1 == "99",
                                                           TP_ACIDENT == "4") %>%   
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

RS_Serie_Histórica_Local_da_Picada_Lagarta[nrow(RS_Serie_Histórica_Local_da_Picada_Lagarta) +1, ] <- AUX[nrow(AUX),]

RS_Serie_Histórica_Local_da_Picada_Lagarta[nrow(RS_Serie_Histórica_Local_da_Picada_Lagarta), 1] <- "2025"

assign(paste0("RS", RS, "_PECONHENTOS_2025_LOCAL_PICADA_LAGARTA"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2025_LOCAL_PICADA_LAGARTA"), AUX), 
           paste0("Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2025_LOCAL_PICADA_LAGARTA.csv"), 
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
                                                   filter(ANT_MUNIC_ == i,
                                                          TP_ACIDENT == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 4) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 5) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ANT_MUNIC_ ==i &
                                                             TP_ACIDENT == 6) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ANT_MUNIC_ ==i &
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
                                                   filter(ANT_MUNIC_ == i,
                                                          TP_ACIDENT == 1,
                                                          TRA_CLASSI == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 1,
                                                          TRA_CLASSI == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 1,
                                                          TRA_CLASSI == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
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
                                                   filter(ANT_MUNIC_ == i,
                                                          TP_ACIDENT == 1,
                                                          ANI_SERPEN == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 1,
                                                          ANI_SERPEN == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 1,
                                                          ANI_SERPEN == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 1,
                                                          ANI_SERPEN == 4) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 1,
                                                          ANI_SERPEN == 5) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ANT_MUNIC_ ==i &
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
                                                   filter(ANT_MUNIC_ == i,
                                                          TP_ACIDENT == 1,
                                                          CON_SOROTE == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 1,
                                                          CON_SOROTE == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
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
                                                   filter(ANT_MUNIC_ == i,
                                                          TP_ACIDENT == 1,
                                                          COM_LOC == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 1,
                                                          COM_LOC == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
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
                                                   filter(ANT_MUNIC_ == i,
                                                          TP_ACIDENT == 1,
                                                          COM_SISTEM == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 1,
                                                          COM_SISTEM == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
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
                                                   filter(ANT_MUNIC_ == i,
                                                          TP_ACIDENT == 1,
                                                          MCLI_LOCAL == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 1,
                                                          MCLI_LOCAL == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
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
                                                   filter(ANT_MUNIC_ == i,
                                                          TP_ACIDENT == 1,
                                                          CLI_DOR == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 1,
                                                          CLI_EDEMA == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 1,
                                                          CLI_EQUIMO == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 1,
                                                          CLI_NECROS == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
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
                                                   filter(ANT_MUNIC_ == i,
                                                          TP_ACIDENT == 1,
                                                          MCLI_SIST == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 1,
                                                          MCLI_SIST == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
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
                                                   filter(ANT_MUNIC_ == i,
                                                          TP_ACIDENT == 1,
                                                          CLI_NEURO == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 1,
                                                          CLI_HEMORR == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 1,
                                                          CLI_VAGAIS == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 1,
                                                          CLI_MIOLIT == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 1,
                                                          CLI_RENAL == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
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
                                                   filter(ANT_MUNIC_ == i,
                                                          TP_ACIDENT == 2,
                                                          TRA_CLASSI == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 2,
                                                          TRA_CLASSI == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 2,
                                                          TRA_CLASSI == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
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
                                                   filter(ANT_MUNIC_ == i,
                                                          TP_ACIDENT == 2,
                                                          ANI_ARANHA == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 2,
                                                          ANI_ARANHA == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 2,
                                                          ANI_ARANHA == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 2,
                                                          ANI_ARANHA == 4) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
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
                                                   filter(ANT_MUNIC_ == i,
                                                          TP_ACIDENT == 2,
                                                          CON_SOROTE == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 2,
                                                          CON_SOROTE == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
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
                                                   filter(ANT_MUNIC_ == i,
                                                          TP_ACIDENT == 2,
                                                          COM_LOC == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 2,
                                                          COM_LOC == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
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
                                                   filter(ANT_MUNIC_ == i,
                                                          TP_ACIDENT == 2,
                                                          COM_SISTEM == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 2,
                                                          COM_SISTEM == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
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
                                                   filter(ANT_MUNIC_ == i,
                                                          TP_ACIDENT == 2,
                                                          MCLI_LOCAL == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 2,
                                                          MCLI_LOCAL == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
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
                                                   filter(ANT_MUNIC_ == i,
                                                          TP_ACIDENT == 2,
                                                          CLI_DOR == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 2,
                                                          CLI_EDEMA == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 2,
                                                          CLI_EQUIMO == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 2,
                                                          CLI_NECROS == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
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
                                                   filter(ANT_MUNIC_ == i,
                                                          TP_ACIDENT == 2,
                                                          MCLI_SIST == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 2,
                                                          MCLI_SIST == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
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
                                                   filter(ANT_MUNIC_ == i,
                                                          TP_ACIDENT == 2,
                                                          CLI_NEURO == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 2,
                                                          CLI_HEMORR == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 2,
                                                          CLI_VAGAIS == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 2,
                                                          CLI_MIOLIT == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 2,
                                                          CLI_RENAL == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
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
                                                   filter(ANT_MUNIC_ == i,
                                                          TP_ACIDENT == 3,
                                                          TRA_CLASSI == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 3,
                                                          TRA_CLASSI == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 3,
                                                          TRA_CLASSI == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
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
                                                   filter(ANT_MUNIC_ == i,
                                                          TP_ACIDENT == 3,
                                                          CON_SOROTE == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 3,
                                                          CON_SOROTE == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
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
                                                   filter(ANT_MUNIC_ == i,
                                                          TP_ACIDENT == 3,
                                                          COM_LOC == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 3,
                                                          COM_LOC == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
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
                                                   filter(ANT_MUNIC_ == i,
                                                          TP_ACIDENT == 3,
                                                          COM_SISTEM == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 3,
                                                          COM_SISTEM == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
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
                                                   filter(ANT_MUNIC_ == i,
                                                          TP_ACIDENT == 3,
                                                          MCLI_LOCAL == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 3,
                                                          MCLI_LOCAL == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
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
                                                   filter(ANT_MUNIC_ == i,
                                                          TP_ACIDENT == 3,
                                                          CLI_DOR == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 3,
                                                          CLI_EDEMA == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 3,
                                                          CLI_EQUIMO == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 3,
                                                          CLI_NECROS == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
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
                                                   filter(ANT_MUNIC_ == i,
                                                          TP_ACIDENT == 3,
                                                          MCLI_SIST == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 3,
                                                          MCLI_SIST == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
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
                                                   filter(ANT_MUNIC_ == i,
                                                          TP_ACIDENT == 3,
                                                          CLI_NEURO == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 3,
                                                          CLI_HEMORR == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 3,
                                                          CLI_VAGAIS == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 3,
                                                          CLI_MIOLIT == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 3,
                                                          CLI_RENAL == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
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
                                                   filter(ANT_MUNIC_ == i,
                                                          TP_ACIDENT == 4,
                                                          TRA_CLASSI == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 4,
                                                          TRA_CLASSI == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 4,
                                                          TRA_CLASSI == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
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
                                                   filter(ANT_MUNIC_ == i,
                                                          TP_ACIDENT == 4,
                                                          ANI_LAGART == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 4,
                                                          ANI_LAGART == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
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
                                                   filter(ANT_MUNIC_ == i,
                                                          TP_ACIDENT == 4,
                                                          CON_SOROTE == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 4,
                                                          CON_SOROTE == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
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
                                                   filter(ANT_MUNIC_ == i,
                                                          TP_ACIDENT == 4,
                                                          COM_LOC == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 4,
                                                          COM_LOC == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
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
                                                   filter(ANT_MUNIC_ == i,
                                                          TP_ACIDENT == 4,
                                                          COM_SISTEM == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 4,
                                                          COM_SISTEM == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
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
                                                   filter(ANT_MUNIC_ == i,
                                                          TP_ACIDENT == 4,
                                                          MCLI_LOCAL == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 4,
                                                          MCLI_LOCAL == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
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
                                                   filter(ANT_MUNIC_ == i,
                                                          TP_ACIDENT == 4,
                                                          CLI_DOR == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 4,
                                                          CLI_EDEMA == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 4,
                                                          CLI_EQUIMO == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 4,
                                                          CLI_NECROS == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
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
                                                   filter(ANT_MUNIC_ == i,
                                                          TP_ACIDENT == 4,
                                                          MCLI_SIST == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 4,
                                                          MCLI_SIST == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
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
                                                   filter(ANT_MUNIC_ == i,
                                                          TP_ACIDENT == 4,
                                                          CLI_NEURO == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 4,
                                                          CLI_HEMORR == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 4,
                                                          CLI_VAGAIS == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 4,
                                                          CLI_MIOLIT == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 4,
                                                          CLI_RENAL == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
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
                                                   filter(ANT_MUNIC_ == i,
                                                          TP_ACIDENT == 5,
                                                          TRA_CLASSI == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 5,
                                                          TRA_CLASSI == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 5,
                                                          TRA_CLASSI == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
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
                                                   filter(ANT_MUNIC_ == i,
                                                          TP_ACIDENT == 5,
                                                          CON_SOROTE == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 5,
                                                          CON_SOROTE == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
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
                                                   filter(ANT_MUNIC_ == i,
                                                          TP_ACIDENT == 5,
                                                          COM_LOC == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 5,
                                                          COM_LOC == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
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
                                                   filter(ANT_MUNIC_ == i,
                                                          TP_ACIDENT == 5,
                                                          COM_SISTEM == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 5,
                                                          COM_SISTEM == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
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
                                                   filter(ANT_MUNIC_ == i,
                                                          TP_ACIDENT == 5,
                                                          MCLI_LOCAL == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 5,
                                                          MCLI_LOCAL == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
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
                                                   filter(ANT_MUNIC_ == i,
                                                          TP_ACIDENT == 5,
                                                          CLI_DOR == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 5,
                                                          CLI_EDEMA == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 5,
                                                          CLI_EQUIMO == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 5,
                                                          CLI_NECROS == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
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
                                                   filter(ANT_MUNIC_ == i,
                                                          TP_ACIDENT == 5,
                                                          MCLI_SIST == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 5,
                                                          MCLI_SIST == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
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
                                                   filter(ANT_MUNIC_ == i,
                                                          TP_ACIDENT == 5,
                                                          CLI_NEURO == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 5,
                                                          CLI_HEMORR == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 5,
                                                          CLI_VAGAIS == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 5,
                                                          CLI_MIOLIT == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 5,
                                                          CLI_RENAL == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
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

###############    Óbitos    #####################

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Serpentes <- NA

AUX$Aranha <- NA

AUX$Escorpiao <- NA

AUX$Lagarta <- NA

AUX$Abelha <- NA

AUX$Outros <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ == i,
                                                          TP_ACIDENT == 1,
                                                          EVOLUCAO == 2) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 2,
                                                          EVOLUCAO == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 3,
                                                          EVOLUCAO == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 4,
                                                          EVOLUCAO == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            TP_ACIDENT == 5,
                                                          EVOLUCAO == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                    filter(ANT_MUNIC_ ==i &
                                                             TP_ACIDENT == 6,
                                                           EVOLUCAO == 2) %>%   
                                                    count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

RS_Serie_Historica_Obitos[nrow(RS_Serie_Historica_Obitos)+1,] <- AUX[nrow(AUX),]

RS_Serie_Historica_Obitos[nrow(RS_Serie_Historica_Obitos), 1] <- "2025"

assign(paste0("RS", RS, "_PECONHENTOS_2025_OBITOS"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2025_OBITOS"), AUX), 
           paste0("Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2025_OBITOS.csv"), 
           row.names = FALSE)
########################################################################################################################################
#####################################   FIM FIM FIM FIM FIM FIM FIM #####################################################################
#########################################################################################################################################

###################################   Análise SINAN    ############################################3

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Abertas <- NA

AUX$Sem_Evo <- NA

AUX$Trabalho <- NA

Abertas <- PECONHENTO2025 %>%
  filter(is.na(DT_ENCERRA))

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(Abertas %>% 
                                                   filter(ANT_MUNIC_ == i) %>%   
                                                   count() 
  )
  
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            EVOLUCAO != 1,
                                                          EVOLUCAO != 2,
                                                          EVOLUCAO != 3
                                                   ) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ ==i &
                                                            DOENCA_TRA != 1,
                                                          DOENCA_TRA != 2) %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2025_SINAN_PROB_ENCERRAMENTO"), AUX)

###################################   Análise Inconsistência SINAN    ############################################3

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Botropico_Leve <- NA

AUX$Botropico_Moderado <- NA

AUX$Botropico_Grave <- NA

AUX$Crotalico_Leve <- NA

AUX$Crotalico_Moderado <- NA

AUX$Crotalico_Grave <- NA

AUX$Escorpiônico_Moderado <- NA

AUX$Escorpiônico_Grave <- NA

AUX$Loxoscelico_Moderado <- NA

AUX$Loxoscelico_Grave <- NA

AUX$Foneutrismo_Moderado <- NA

AUX$Foneutrismo_Grave <- NA

AUX$Lonomia_Moderado <- NA

AUX$Lonomia_Grave <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(PECONHENTO2025 %>% 
                                                   filter(ANT_MUNIC_ == i,
                                                          ANI_SERPEN == 1,
                                                          TRA_CLASSI == 1,
                                                          NU_AMPOLAS <= "2" | 
                                                            NU_AMPOLAS >= "4"
                                                   ) %>%   
                                                   count() 
  )
  
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ == i &
                                                          ANI_SERPEN == 1 &
                                                          TRA_CLASSI == 2 &
                                                          NU_AMPOLAS <= "4" |
                                                            NU_AMPOLAS >= "8" 
                                                   ) %>%   
                                                   count() 
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2025 %>% 
                                                   filter(ANT_MUNIC_ == i &
                                                            ANI_SERPEN == 1 &
                                                            TRA_CLASSI == 3 &
                                                            NU_AMPOLAS != "12"
                                                   ) %>%   
                                                   count()
  ) 
  
    AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(PECONHENTO2025 %>% 
                                                   filter(ANT_MUNIC_ == i &
                                                          ANI_SERPEN == 2 &
                                                          TRA_CLASSI == 1 &
                                                          NU_AMPOL_1 != "5" 
                                                   ) %>%   
                                                   count() 
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(PECONHENTO2025 %>% 
                                                   filter(ANT_MUNIC_ == i &
                                                          ANI_SERPEN == 2 &
                                                          TRA_CLASSI == 2 &
                                                          NU_AMPOL_1 != "10" 
                                                   ) %>%   
                                                   count() 
  )
  
  
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(PECONHENTO2025 %>% 
                                                    filter(ANT_MUNIC_ == i &
                                                           ANI_SERPEN == 2 &
                                                           TRA_CLASSI == 3 &
                                                           NU_AMPOL_1 != "20" 
                                                    ) %>%   
                                                    count() 
  ) 

  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(PECONHENTO2025 %>% 
                                                    filter(ANT_MUNIC_ == i,
                                                           TP_ACIDENT == 3,
                                                           TRA_CLASSI == 2,
                                                           NU_AMPOL_9 < "2" |
                                                             NU_AMPOL_9 > "3"
                                                    ) %>%   
                                                    count() 
  )
  
    AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(PECONHENTO2025 %>% 
                                                    filter(ANT_MUNIC_ == i,
                                                           TP_ACIDENT == 3,
                                                           TRA_CLASSI == 3,
                                                           NU_AMPOL_9 <= "4" |
                                                             NU_AMPOL_9 >= "6"
                                                    ) %>%   
                                                    count() 
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(PECONHENTO2025 %>% 
                                                    filter(ANT_MUNIC_ == i,
                                                           ANI_ARANHA == 2,
                                                           TRA_CLASSI == 2,
                                                           CON_SOROTE == 2,
                                                           
                                                    ) %>%   
                                                    count() 
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(PECONHENTO2025 %>% 
                                                    filter(ANT_MUNIC_ == i,
                                                           ANI_ARANHA == 2,
                                                           TRA_CLASSI == 3,
                                                           CON_SOROTE == 2
                                                    ) %>%   
                                                    count() 
  )
  
    AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(PECONHENTO2025 %>% 
                                                    filter(ANT_MUNIC_ == i,
                                                           ANI_ARANHA == 1,
                                                           TRA_CLASSI == 2,
                                                           CON_SOROTE == 2
                                                    ) %>%   
                                                    count() 
  )
  
  AUX[which(AUX$COD_IBGE == i), 16] <- as.integer(PECONHENTO2025 %>% 
                                                    filter(ANT_MUNIC_ == i,
                                                           ANI_ARANHA == 1,
                                                           TRA_CLASSI == 2,
                                                           CON_SOROTE == 2
                                                    ) %>%   
                                                    count() 
  )
  
  AUX[which(AUX$COD_IBGE == i), 17] <- as.integer(PECONHENTO2025 %>% 
                                                    filter(ANT_MUNIC_ == i,
                                                           ANI_LAGART == 1,
                                                           TRA_CLASSI == 2,
                                                           CON_SOROTE == 2
                                                    ) %>%   
                                                    count() 
  )
  
  AUX[which(AUX$COD_IBGE == i), 18] <- as.integer(PECONHENTO2025 %>% 
                                                    filter(ANT_MUNIC_ == i,
                                                           ANI_LAGART == 1,
                                                           TRA_CLASSI == 3,
                                                           CON_SOROTE == 2
                                                    ) %>%   
                                                    count() 
  )
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2025_SINAN_SORO_INCONSISTENCIA"), AUX)

#########################################   SIES   ####################################################

#######################################   Antibotrópico   ############################################

SIES_SORO_ANTIBOTROPICO[, 8] <- c(1: nrow(SIES_SORO_ANTIBOTROPICO))

SIES_SORO_ANTIBOTROPICO <- SIES_SORO_ANTIBOTROPICO[, c(8, 1:7)]

AUX <- format(as.Date(SIES_SORO_ANTIBOTROPICO$Data, "%d/%m/%Y"), "%Y-%m-%d")

AUX <- as.Date(AUX)

SIES_SORO_ANTIBOTROPICO[, 9] <- epiweek(AUX)

SIES_SORO_ANTIBOTROPICO[, 10] <- str_sub(SIES_SORO_ANTIBOTROPICO$Data, start = 7, end = 10)

colnames(SIES_SORO_ANTIBOTROPICO)[c(1, 9, 10)] <- c("Ordem", "SE", "Ano")

AUX <- matrix(data = NA, 
              nrow = 6, 
              ncol = 54)

AUX <- as.data.frame(AUX)

colnames(AUX)[1] <- "Ano" 

AUX[,1] <- c("2020", "2021", "2022", "2023", "2024", "2025")

colnames (AUX)[2:54] <- c(1:53)

for (i in SIES_SORO_ANTIBOTROPICO[, 10]){
  
  AUX[which(AUX == i), 2] <- as.integer(SIES_SORO_ANTIBOTROPICO %>%
                                          filter(Ano == i &
                                                   SE == 1)%>%
                                          summarise('2' = sum(Quantidade))
                                          
  )
  
  AUX[which(AUX == i), 3] <- as.integer(SIES_SORO_ANTIBOTROPICO %>%
                                          filter(Ano == i &
                                                   SE == 2)%>%
                                          summarise('3' = sum(Quantidade))
                                        
  )
  
  AUX[which(AUX == i), 4] <- as.integer(SIES_SORO_ANTIBOTROPICO %>%
                                          filter(Ano == i &
                                                   SE == 3)%>%
                                          summarise('4' = sum(Quantidade))
                                        
  )
  
  AUX[which(AUX == i), 5] <- as.integer(SIES_SORO_ANTIBOTROPICO %>%
                                          filter(Ano == i &
                                                   SE == 4)%>%
                                          summarise('5' = sum(Quantidade))
                                        
  )
  
  AUX[which(AUX == i), 6] <- as.integer(SIES_SORO_ANTIBOTROPICO %>%
                                          filter(Ano == i &
                                                   SE == 5)%>%
                                          summarise('6' = sum(Quantidade))
                                        
  )
  
  AUX[which(AUX == i), 7] <- as.integer(SIES_SORO_ANTIBOTROPICO %>%
                                          filter(Ano == i &
                                                   SE == 6)%>%
                                          summarise('7' = sum(Quantidade))
                                        
  )
  
  AUX[which(AUX == i), 8] <- as.integer(SIES_SORO_ANTIBOTROPICO %>%
                                          filter(Ano == i &
                                                   SE == 7)%>%
                                          summarise('8' = sum(Quantidade))
                                        
  )
  
  AUX[which(AUX == i), 9] <- as.integer(SIES_SORO_ANTIBOTROPICO %>%
                                          filter(Ano == i &
                                                   SE == 8)%>%
                                          summarise('9' = sum(Quantidade))
                                        
  )
  
  AUX[which(AUX == i), 10] <- as.integer(SIES_SORO_ANTIBOTROPICO %>%
                                          filter(Ano == i &
                                                   SE == 9)%>%
                                          summarise('10' = sum(Quantidade))
                                        
  )
  
  AUX[which(AUX == i), 11] <- as.integer(SIES_SORO_ANTIBOTROPICO %>%
                                          filter(Ano == i &
                                                   SE == 10)%>%
                                          summarise('11' = sum(Quantidade))
                                        
  )
  
  AUX[which(AUX == i), 12] <- as.integer(SIES_SORO_ANTIBOTROPICO %>%
                                          filter(Ano == i &
                                                   SE == 11)%>%
                                          summarise('12' = sum(Quantidade))
                                        
  )
  
  AUX[which(AUX == i), 13] <- as.integer(SIES_SORO_ANTIBOTROPICO %>%
                                          filter(Ano == i &
                                                   SE == 12)%>%
                                          summarise('13' = sum(Quantidade))
                                        
  )
  
  AUX[which(AUX == i), 14] <- as.integer(SIES_SORO_ANTIBOTROPICO %>%
                                          filter(Ano == i &
                                                   SE == 13)%>%
                                          summarise('14' = sum(Quantidade))
                                        
  )
  
  AUX[which(AUX == i), 15] <- as.integer(SIES_SORO_ANTIBOTROPICO %>%
                                          filter(Ano == i &
                                                   SE == 14)%>%
                                          summarise('15' = sum(Quantidade))
                                        
  )
  
  AUX[which(AUX == i), 16] <- as.integer(SIES_SORO_ANTIBOTROPICO %>%
                                          filter(Ano == i &
                                                   SE == 15)%>%
                                          summarise('16' = sum(Quantidade))
                                        
  )
  
  AUX[which(AUX == i), 17] <- as.integer(SIES_SORO_ANTIBOTROPICO %>%
                                          filter(Ano == i &
                                                   SE == 16)%>%
                                          summarise('17' = sum(Quantidade))
                                        
  )
  
  AUX[which(AUX == i), 18] <- as.integer(SIES_SORO_ANTIBOTROPICO %>%
                                          filter(Ano == i &
                                                   SE == 17)%>%
                                          summarise('18' = sum(Quantidade))
                                        
  )
  
  AUX[which(AUX == i), 19] <- as.integer(SIES_SORO_ANTIBOTROPICO %>%
                                           filter(Ano == i &
                                                    SE == 18)%>%
                                           summarise('19' = sum(Quantidade))
                                         
  )
  
  AUX[which(AUX == i), 20] <- as.integer(SIES_SORO_ANTIBOTROPICO %>%
                                          filter(Ano == i &
                                                   SE == 19)%>%
                                          summarise('20' = sum(Quantidade))
                                        
  )
  
  AUX[which(AUX == i), 21] <- as.integer(SIES_SORO_ANTIBOTROPICO %>%
                                          filter(Ano == i &
                                                   SE == 20)%>%
                                          summarise('21' = sum(Quantidade))
                                        
  )
  
  AUX[which(AUX == i), 22] <- as.integer(SIES_SORO_ANTIBOTROPICO %>%
                                          filter(Ano == i &
                                                   SE == 21)%>%
                                          summarise('22' = sum(Quantidade))
                                        
  )
  
  AUX[which(AUX == i), 23] <- as.integer(SIES_SORO_ANTIBOTROPICO %>%
                                          filter(Ano == i &
                                                   SE == 22)%>%
                                          summarise('23' = sum(Quantidade))
                                        
  )
  
  AUX[which(AUX == i), 24] <- as.integer(SIES_SORO_ANTIBOTROPICO %>%
                                          filter(Ano == i &
                                                   SE == 23)%>%
                                          summarise('24' = sum(Quantidade))
                                        
  )
  
  AUX[which(AUX == i), 25] <- as.integer(SIES_SORO_ANTIBOTROPICO %>%
                                          filter(Ano == i &
                                                   SE == 24)%>%
                                          summarise('25' = sum(Quantidade))
                                        
  )
  
  AUX[which(AUX == i), 26] <- as.integer(SIES_SORO_ANTIBOTROPICO %>%
                                          filter(Ano == i &
                                                   SE == 25)%>%
                                          summarise('26' = sum(Quantidade))
                                        
  )
  
  AUX[which(AUX == i), 27] <- as.integer(SIES_SORO_ANTIBOTROPICO %>%
                                          filter(Ano == i &
                                                   SE == 26)%>%
                                          summarise('27' = sum(Quantidade))
                                        
  )
  
  AUX[which(AUX == i), 28] <- as.integer(SIES_SORO_ANTIBOTROPICO %>%
                                           filter(Ano == i &
                                                    SE == 27)%>%
                                           summarise('28' = sum(Quantidade))
                                         
  )
  
  AUX[which(AUX == i), 29] <- as.integer(SIES_SORO_ANTIBOTROPICO %>%
                                           filter(Ano == i &
                                                    SE == 28)%>%
                                           summarise('29' = sum(Quantidade))
                                         
  )
  
  AUX[which(AUX == i), 30] <- as.integer(SIES_SORO_ANTIBOTROPICO %>%
                                           filter(Ano == i &
                                                    SE == 29)%>%
                                           summarise('30' = sum(Quantidade))
                                         
  )
  
  AUX[which(AUX == i), 31] <- as.integer(SIES_SORO_ANTIBOTROPICO %>%
                                           filter(Ano == i &
                                                    SE == 30)%>%
                                           summarise('31' = sum(Quantidade))
                                         
  )
  
  AUX[which(AUX == i), 32] <- as.integer(SIES_SORO_ANTIBOTROPICO %>%
                                           filter(Ano == i &
                                                    SE == 31)%>%
                                           summarise('32' = sum(Quantidade))
                                         
  )
  
  AUX[which(AUX == i), 33] <- as.integer(SIES_SORO_ANTIBOTROPICO %>%
                                           filter(Ano == i &
                                                    SE == 32)%>%
                                           summarise('33' = sum(Quantidade))
                                         
  )
  
  AUX[which(AUX == i), 34] <- as.integer(SIES_SORO_ANTIBOTROPICO %>%
                                           filter(Ano == i &
                                                    SE == 33)%>%
                                           summarise('34' = sum(Quantidade))
                                         
  )
  
  AUX[which(AUX == i), 35] <- as.integer(SIES_SORO_ANTIBOTROPICO %>%
                                           filter(Ano == i &
                                                    SE == 34)%>%
                                           summarise('35' = sum(Quantidade))
                                         
  )
  
  AUX[which(AUX == i), 36] <- as.integer(SIES_SORO_ANTIBOTROPICO %>%
                                           filter(Ano == i &
                                                    SE == 35)%>%
                                           summarise('36' = sum(Quantidade))
                                         
  )
  
  AUX[which(AUX == i), 37] <- as.integer(SIES_SORO_ANTIBOTROPICO %>%
                                           filter(Ano == i &
                                                    SE == 36)%>%
                                           summarise('37' = sum(Quantidade))
                                         
  )
  
  AUX[which(AUX == i), 38] <- as.integer(SIES_SORO_ANTIBOTROPICO %>%
                                           filter(Ano == i &
                                                    SE == 37)%>%
                                           summarise('38' = sum(Quantidade))
                                         
  )
  
  AUX[which(AUX == i), 39] <- as.integer(SIES_SORO_ANTIBOTROPICO %>%
                                           filter(Ano == i &
                                                    SE == 38)%>%
                                           summarise('39' = sum(Quantidade))
                                         
  )
  
  AUX[which(AUX == i), 40] <- as.integer(SIES_SORO_ANTIBOTROPICO %>%
                                           filter(Ano == i &
                                                    SE == 39)%>%
                                           summarise('40' = sum(Quantidade))
                                         
  )
  
  AUX[which(AUX == i), 41] <- as.integer(SIES_SORO_ANTIBOTROPICO %>%
                                           filter(Ano == i &
                                                    SE == 40)%>%
                                           summarise('41' = sum(Quantidade))
                                         
  )
  
  AUX[which(AUX == i), 42] <- as.integer(SIES_SORO_ANTIBOTROPICO %>%
                                           filter(Ano == i &
                                                    SE == 41)%>%
                                           summarise('42' = sum(Quantidade))
                                         
  )
  
  AUX[which(AUX == i), 43] <- as.integer(SIES_SORO_ANTIBOTROPICO %>%
                                           filter(Ano == i &
                                                    SE == 42)%>%
                                           summarise('43' = sum(Quantidade))
                                         
  )
  
  AUX[which(AUX == i), 44] <- as.integer(SIES_SORO_ANTIBOTROPICO %>%
                                           filter(Ano == i &
                                                    SE == 43)%>%
                                           summarise('44' = sum(Quantidade))
                                         
  )
  
  AUX[which(AUX == i), 45] <- as.integer(SIES_SORO_ANTIBOTROPICO %>%
                                           filter(Ano == i &
                                                    SE == 44)%>%
                                           summarise('45' = sum(Quantidade))
                                         
  )
  
  AUX[which(AUX == i), 46] <- as.integer(SIES_SORO_ANTIBOTROPICO %>%
                                           filter(Ano == i &
                                                    SE == 45)%>%
                                           summarise('46' = sum(Quantidade))
                                         
  )
  
  AUX[which(AUX == i), 47] <- as.integer(SIES_SORO_ANTIBOTROPICO %>%
                                           filter(Ano == i &
                                                    SE == 46)%>%
                                           summarise('47' = sum(Quantidade))
                                         
  )
  
  AUX[which(AUX == i), 48] <- as.integer(SIES_SORO_ANTIBOTROPICO %>%
                                           filter(Ano == i &
                                                    SE == 47)%>%
                                           summarise('48' = sum(Quantidade))
                                         
  )
  
  AUX[which(AUX == i), 49] <- as.integer(SIES_SORO_ANTIBOTROPICO %>%
                                           filter(Ano == i &
                                                    SE == 48)%>%
                                           summarise('49' = sum(Quantidade))
                                         
  )
  
  AUX[which(AUX == i), 50] <- as.integer(SIES_SORO_ANTIBOTROPICO %>%
                                           filter(Ano == i &
                                                    SE == 49)%>%
                                           summarise('50' = sum(Quantidade))
                                         
  )
  
  AUX[which(AUX == i), 51] <- as.integer(SIES_SORO_ANTIBOTROPICO %>%
                                           filter(Ano == i &
                                                    SE == 50)%>%
                                           summarise('51' = sum(Quantidade))
                                         
  )
  
  AUX[which(AUX == i), 52] <- as.integer(SIES_SORO_ANTIBOTROPICO %>%
                                           filter(Ano == i &
                                                    SE == 51)%>%
                                           summarise('52' = sum(Quantidade))
                                         
  )
  
  AUX[which(AUX == i), 53] <- as.integer(SIES_SORO_ANTIBOTROPICO %>%
                                           filter(Ano == i &
                                                    SE == 52)%>%
                                           summarise('53' = sum(Quantidade))
                                         
  )
  
  AUX[which(AUX == i), 54] <- as.integer(SIES_SORO_ANTIBOTROPICO %>%
                                           filter(Ano == i &
                                                    SE == 53)%>%
                                           summarise('54' = sum(Quantidade))
                                         
  )
}

assign(paste0("RS", RS, "_PECONHENTOS_2025_SE_SIES_ANTIBOTROPICO"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2025_SE_SIES_ANTIBOTROPICO"), AUX), 
           paste0("Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2025_SE_SIES_ANTIBOTROPICO.csv"), 
           row.names = FALSE)


#######################################   Anticrotálico   ############################################

SIES_SORO_ANTICROTALICO[, 8] <- c(1: nrow(SIES_SORO_ANTICROTALICO))

SIES_SORO_ANTICROTALICO <- SIES_SORO_ANTICROTALICO[, c(8, 1:7)]

AUX <- format(as.Date(SIES_SORO_ANTICROTALICO$Data, "%d/%m/%Y"), "%Y-%m-%d")

AUX <- as.Date(AUX)

SIES_SORO_ANTICROTALICO[, 9] <- epiweek(AUX)

SIES_SORO_ANTICROTALICO[, 10] <- str_sub(SIES_SORO_ANTICROTALICO$Data, start = 7, end = 10)

colnames(SIES_SORO_ANTICROTALICO)[c(1, 9, 10)] <- c("Ordem", "SE", "Ano")

AUX <- matrix(data = NA, 
              nrow = 6, 
              ncol = 54)

AUX <- as.data.frame(AUX)

colnames(AUX)[1] <- "Ano" 

AUX[,1] <- c("2020", "2021", "2022", "2023", "2024", "2025")

colnames (AUX)[2:54] <- c(1:53)

for (i in SIES_SORO_ANTICROTALICO[, 10]){
  
  AUX[which(AUX == i), 2] <- as.integer(SIES_SORO_ANTICROTALICO %>%
                                          filter(Ano == i &
                                                   SE == 1)%>%
                                          summarise('2' = sum(Quantidade))
                                        
  )
  
  AUX[which(AUX == i), 3] <- as.integer(SIES_SORO_ANTICROTALICO %>%
                                          filter(Ano == i &
                                                   SE == 2)%>%
                                          summarise('3' = sum(Quantidade))
                                        
  )
  
  AUX[which(AUX == i), 4] <- as.integer(SIES_SORO_ANTICROTALICO %>%
                                          filter(Ano == i &
                                                   SE == 3)%>%
                                          summarise('4' = sum(Quantidade))
                                        
  )
  
  AUX[which(AUX == i), 5] <- as.integer(SIES_SORO_ANTICROTALICO %>%
                                          filter(Ano == i &
                                                   SE == 4)%>%
                                          summarise('5' = sum(Quantidade))
                                        
  )
  
  AUX[which(AUX == i), 6] <- as.integer(SIES_SORO_ANTICROTALICO %>%
                                          filter(Ano == i &
                                                   SE == 5)%>%
                                          summarise('6' = sum(Quantidade))
                                        
  )
  
  AUX[which(AUX == i), 7] <- as.integer(SIES_SORO_ANTICROTALICO %>%
                                          filter(Ano == i &
                                                   SE == 6)%>%
                                          summarise('7' = sum(Quantidade))
                                        
  )
  
  AUX[which(AUX == i), 8] <- as.integer(SIES_SORO_ANTICROTALICO %>%
                                          filter(Ano == i &
                                                   SE == 7)%>%
                                          summarise('8' = sum(Quantidade))
                                        
  )
  
  AUX[which(AUX == i), 9] <- as.integer(SIES_SORO_ANTICROTALICO %>%
                                          filter(Ano == i &
                                                   SE == 8)%>%
                                          summarise('9' = sum(Quantidade))
                                        
  )
  
  AUX[which(AUX == i), 10] <- as.integer(SIES_SORO_ANTICROTALICO %>%
                                           filter(Ano == i &
                                                    SE == 9)%>%
                                           summarise('10' = sum(Quantidade))
                                         
  )
  
  AUX[which(AUX == i), 11] <- as.integer(SIES_SORO_ANTICROTALICO %>%
                                           filter(Ano == i &
                                                    SE == 10)%>%
                                           summarise('11' = sum(Quantidade))
                                         
  )
  
  AUX[which(AUX == i), 12] <- as.integer(SIES_SORO_ANTICROTALICO %>%
                                           filter(Ano == i &
                                                    SE == 11)%>%
                                           summarise('12' = sum(Quantidade))
                                         
  )
  
  AUX[which(AUX == i), 13] <- as.integer(SIES_SORO_ANTICROTALICO %>%
                                           filter(Ano == i &
                                                    SE == 12)%>%
                                           summarise('13' = sum(Quantidade))
                                         
  )
  
  AUX[which(AUX == i), 14] <- as.integer(SIES_SORO_ANTICROTALICO %>%
                                           filter(Ano == i &
                                                    SE == 13)%>%
                                           summarise('14' = sum(Quantidade))
                                         
  )
  
  AUX[which(AUX == i), 15] <- as.integer(SIES_SORO_ANTICROTALICO %>%
                                           filter(Ano == i &
                                                    SE == 14)%>%
                                           summarise('15' = sum(Quantidade))
                                         
  )
  
  AUX[which(AUX == i), 16] <- as.integer(SIES_SORO_ANTICROTALICO %>%
                                           filter(Ano == i &
                                                    SE == 15)%>%
                                           summarise('16' = sum(Quantidade))
                                         
  )
  
  AUX[which(AUX == i), 17] <- as.integer(SIES_SORO_ANTICROTALICO %>%
                                           filter(Ano == i &
                                                    SE == 16)%>%
                                           summarise('17' = sum(Quantidade))
                                         
  )
  
  AUX[which(AUX == i), 18] <- as.integer(SIES_SORO_ANTICROTALICO %>%
                                           filter(Ano == i &
                                                    SE == 17)%>%
                                           summarise('18' = sum(Quantidade))
                                         
  )
  
  AUX[which(AUX == i), 19] <- as.integer(SIES_SORO_ANTICROTALICO %>%
                                           filter(Ano == i &
                                                    SE == 18)%>%
                                           summarise('19' = sum(Quantidade))
                                         
  )
  
  AUX[which(AUX == i), 20] <- as.integer(SIES_SORO_ANTICROTALICO %>%
                                           filter(Ano == i &
                                                    SE == 19)%>%
                                           summarise('20' = sum(Quantidade))
                                         
  )
  
  AUX[which(AUX == i), 21] <- as.integer(SIES_SORO_ANTICROTALICO %>%
                                           filter(Ano == i &
                                                    SE == 20)%>%
                                           summarise('21' = sum(Quantidade))
                                         
  )
  
  AUX[which(AUX == i), 22] <- as.integer(SIES_SORO_ANTICROTALICO %>%
                                           filter(Ano == i &
                                                    SE == 21)%>%
                                           summarise('22' = sum(Quantidade))
                                         
  )
  
  AUX[which(AUX == i), 23] <- as.integer(SIES_SORO_ANTICROTALICO %>%
                                           filter(Ano == i &
                                                    SE == 22)%>%
                                           summarise('23' = sum(Quantidade))
                                         
  )
  
  AUX[which(AUX == i), 24] <- as.integer(SIES_SORO_ANTICROTALICO %>%
                                           filter(Ano == i &
                                                    SE == 23)%>%
                                           summarise('24' = sum(Quantidade))
                                         
  )
  
  AUX[which(AUX == i), 25] <- as.integer(SIES_SORO_ANTICROTALICO %>%
                                           filter(Ano == i &
                                                    SE == 24)%>%
                                           summarise('25' = sum(Quantidade))
                                         
  )
  
  AUX[which(AUX == i), 26] <- as.integer(SIES_SORO_ANTICROTALICO %>%
                                           filter(Ano == i &
                                                    SE == 25)%>%
                                           summarise('26' = sum(Quantidade))
                                         
  )
  
  AUX[which(AUX == i), 27] <- as.integer(SIES_SORO_ANTICROTALICO %>%
                                           filter(Ano == i &
                                                    SE == 26)%>%
                                           summarise('27' = sum(Quantidade))
                                         
  )
  
  AUX[which(AUX == i), 28] <- as.integer(SIES_SORO_ANTICROTALICO %>%
                                           filter(Ano == i &
                                                    SE == 27)%>%
                                           summarise('28' = sum(Quantidade))
                                         
  )
  
  AUX[which(AUX == i), 29] <- as.integer(SIES_SORO_ANTICROTALICO %>%
                                           filter(Ano == i &
                                                    SE == 28)%>%
                                           summarise('29' = sum(Quantidade))
                                         
  )
  
  AUX[which(AUX == i), 30] <- as.integer(SIES_SORO_ANTICROTALICO %>%
                                           filter(Ano == i &
                                                    SE == 29)%>%
                                           summarise('30' = sum(Quantidade))
                                         
  )
  
  AUX[which(AUX == i), 31] <- as.integer(SIES_SORO_ANTICROTALICO %>%
                                           filter(Ano == i &
                                                    SE == 30)%>%
                                           summarise('31' = sum(Quantidade))
                                         
  )
  
  AUX[which(AUX == i), 32] <- as.integer(SIES_SORO_ANTICROTALICO %>%
                                           filter(Ano == i &
                                                    SE == 31)%>%
                                           summarise('32' = sum(Quantidade))
                                         
  )
  
  AUX[which(AUX == i), 33] <- as.integer(SIES_SORO_ANTICROTALICO %>%
                                           filter(Ano == i &
                                                    SE == 32)%>%
                                           summarise('33' = sum(Quantidade))
                                         
  )
  
  AUX[which(AUX == i), 34] <- as.integer(SIES_SORO_ANTICROTALICO %>%
                                           filter(Ano == i &
                                                    SE == 33)%>%
                                           summarise('34' = sum(Quantidade))
                                         
  )
  
  AUX[which(AUX == i), 35] <- as.integer(SIES_SORO_ANTICROTALICO %>%
                                           filter(Ano == i &
                                                    SE == 34)%>%
                                           summarise('35' = sum(Quantidade))
                                         
  )
  
  AUX[which(AUX == i), 36] <- as.integer(SIES_SORO_ANTICROTALICO %>%
                                           filter(Ano == i &
                                                    SE == 35)%>%
                                           summarise('36' = sum(Quantidade))
                                         
  )
  
  AUX[which(AUX == i), 37] <- as.integer(SIES_SORO_ANTICROTALICO %>%
                                           filter(Ano == i &
                                                    SE == 36)%>%
                                           summarise('37' = sum(Quantidade))
                                         
  )
  
  AUX[which(AUX == i), 38] <- as.integer(SIES_SORO_ANTICROTALICO %>%
                                           filter(Ano == i &
                                                    SE == 37)%>%
                                           summarise('38' = sum(Quantidade))
                                         
  )
  
  AUX[which(AUX == i), 39] <- as.integer(SIES_SORO_ANTICROTALICO %>%
                                           filter(Ano == i &
                                                    SE == 38)%>%
                                           summarise('39' = sum(Quantidade))
                                         
  )
  
  AUX[which(AUX == i), 40] <- as.integer(SIES_SORO_ANTICROTALICO %>%
                                           filter(Ano == i &
                                                    SE == 39)%>%
                                           summarise('40' = sum(Quantidade))
                                         
  )
  
  AUX[which(AUX == i), 41] <- as.integer(SIES_SORO_ANTICROTALICO %>%
                                           filter(Ano == i &
                                                    SE == 40)%>%
                                           summarise('41' = sum(Quantidade))
                                         
  )
  
  AUX[which(AUX == i), 42] <- as.integer(SIES_SORO_ANTICROTALICO %>%
                                           filter(Ano == i &
                                                    SE == 41)%>%
                                           summarise('42' = sum(Quantidade))
                                         
  )
  
  AUX[which(AUX == i), 43] <- as.integer(SIES_SORO_ANTICROTALICO %>%
                                           filter(Ano == i &
                                                    SE == 42)%>%
                                           summarise('43' = sum(Quantidade))
                                         
  )
  
  AUX[which(AUX == i), 44] <- as.integer(SIES_SORO_ANTICROTALICO %>%
                                           filter(Ano == i &
                                                    SE == 43)%>%
                                           summarise('44' = sum(Quantidade))
                                         
  )
  
  AUX[which(AUX == i), 45] <- as.integer(SIES_SORO_ANTICROTALICO %>%
                                           filter(Ano == i &
                                                    SE == 44)%>%
                                           summarise('45' = sum(Quantidade))
                                         
  )
  
  AUX[which(AUX == i), 46] <- as.integer(SIES_SORO_ANTICROTALICO %>%
                                           filter(Ano == i &
                                                    SE == 45)%>%
                                           summarise('46' = sum(Quantidade))
                                         
  )
  
  AUX[which(AUX == i), 47] <- as.integer(SIES_SORO_ANTICROTALICO %>%
                                           filter(Ano == i &
                                                    SE == 46)%>%
                                           summarise('47' = sum(Quantidade))
                                         
  )
  
  AUX[which(AUX == i), 48] <- as.integer(SIES_SORO_ANTICROTALICO %>%
                                           filter(Ano == i &
                                                    SE == 47)%>%
                                           summarise('48' = sum(Quantidade))
                                         
  )
  
  AUX[which(AUX == i), 49] <- as.integer(SIES_SORO_ANTICROTALICO %>%
                                           filter(Ano == i &
                                                    SE == 48)%>%
                                           summarise('49' = sum(Quantidade))
                                         
  )
  
  AUX[which(AUX == i), 50] <- as.integer(SIES_SORO_ANTICROTALICO %>%
                                           filter(Ano == i &
                                                    SE == 49)%>%
                                           summarise('50' = sum(Quantidade))
                                         
  )
  
  AUX[which(AUX == i), 51] <- as.integer(SIES_SORO_ANTICROTALICO %>%
                                           filter(Ano == i &
                                                    SE == 50)%>%
                                           summarise('51' = sum(Quantidade))
                                         
  )
  
  AUX[which(AUX == i), 52] <- as.integer(SIES_SORO_ANTICROTALICO %>%
                                           filter(Ano == i &
                                                    SE == 51)%>%
                                           summarise('52' = sum(Quantidade))
                                         
  )
  
  AUX[which(AUX == i), 53] <- as.integer(SIES_SORO_ANTICROTALICO %>%
                                           filter(Ano == i &
                                                    SE == 52)%>%
                                           summarise('53' = sum(Quantidade))
                                         
  )
  
  AUX[which(AUX == i), 54] <- as.integer(SIES_SORO_ANTICROTALICO %>%
                                           filter(Ano == i &
                                                    SE == 53)%>%
                                           summarise('54' = sum(Quantidade))
                                         
  )
}

assign(paste0("RS", RS, "_PECONHENTOS_2025_SE_SIES_ANTICROTALICO"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2025_SE_SIES_ANTICROTALICO"), AUX), 
           paste0("Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2025_SE_SIES_ANTICROTALICO.csv"), 
           row.names = FALSE)

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

RS_PECONHENTOS_GRAF_CE_Geral <- ggplot(AUX_GRAF, aes(Ordem))  +
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = .5,
                                   face = "bold",
                                   size = 12)) +
  labs(caption = Fonte,
       title = "Canal Endêmico - Acidentes com Animais Peçonhentos",
       subtitle = paste0("Casos Notificados - ", RS, "ª Regional de Saúde")) +
  theme(
    panel.grid.major = element_line(color = "#C0C0C0"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#B22222"),
    plot.title = element_text(face = "bold",
                              size = 24,
                              colour = "#1C1C1C"),
    plot.caption = element_text(hjust = 0, 
                                size = 12),
    plot.subtitle = element_text(size = 12)
  ) +
  geom_area(aes(y = Lim_Superior), 
            fill = "#F0E68C",
            alpha = 0.9) +
  geom_area(aes( y = Mediana), 
            fill = "#556B2F") +
  geom_line(aes( y = `2025`), 
            stat = "identity",
            color = "black", 
            linewidth = 1.5) +
  xlab("Semana Epidemiológica") +
  ylab("Número de Casos") +
  scale_x_continuous(breaks = c(1:53), 
                     label = AUX_GRAF$Sem_EPI) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

# #####################################   SERPENTES      #############################################################################
# 
# CE_BASE_SERPENTES_Notificados[(nrow(CE_BASE_SERPENTES_Notificados) +1), 1] <- "2025"
# CE_BASE_SERPENTES_Notificados[nrow(CE_BASE_SERPENTES_Notificados), 2:54] <- as.integer(data.frame(RS22_PECONHENTOS_2025_SE_SERPENTES_Notificados[nrow(RS22_PECONHENTOS_2025_SE_Notificados), 2:54]))
# 
# #####Utilizando objetos auxiliares porque se transpor o data frame direto ele transforma as variáveis em#############
# #####caracter.            NÃO FOI DESCARTADO AINDA OS PERÍODOS EPIDÊMICOS                               #############
# ##### VERIFICAR SE PODE-SE UTILIZAR A MÉDIA COMO LIMITE INFERIOR.                                       #############
# 
# AUX <- CE_BASE_SERPENTES_Notificados[, -1]
# 
# AUX <- t(AUX)
# 
# AUX2 <- CE_BASE_SERPENTES_Notificados[, 1]
# 
# colnames(AUX) <- AUX2
# 
# CE_BASE_SERPENTES_Notificados <- as.data.frame(AUX)
# 
# CE_BASE_SERPENTES_Notificados <- CE_BASE_SERPENTES_Notificados[, Anos_Analise]
# 
# ######Criando a coluna de média no data.frame#####################
# 
# CE_BASE_SERPENTES_Notificados <- CE_BASE_SERPENTES_Notificados %>%
#   mutate(Mediana = apply(CE_BASE_SERPENTES_Notificados[,1: (ncol(CE_BASE_SERPENTES_Notificados)-1)], 1 , median))
# 
# ######Criando a coluna de Desvio Padrão no data frame###############
# 
# CE_BASE_SERPENTES_Notificados <- CE_BASE_SERPENTES_Notificados %>%
#   mutate(Desvio_Padrao = apply(CE_BASE_SERPENTES_Notificados[,1: (ncol(CE_BASE_SERPENTES_Notificados) -2)], 1 , sd))
# 
# ###### Criando a coluna de Média + 2(DP)
# 
# CE_BASE_SERPENTES_Notificados <- CE_BASE_SERPENTES_Notificados %>%
#   mutate(Lim_Superior = (Mediana + 1.96 * Desvio_Padrao))
# 
# CE_BASE_SERPENTES_Notificados[, (ncol(CE_BASE_SERPENTES_Notificados)+1)] <- rownames(CE_BASE_SERPENTES_Notificados)
# 
# CE_BASE_SERPENTES_Notificados <- CE_BASE_SERPENTES_Notificados[, c(ncol(CE_BASE_SERPENTES_Notificados), 1:(ncol(CE_BASE_SERPENTES_Notificados) -1))]
# 
# CE_BASE_SERPENTES_Notificados[,1] <- c(1:53)
# 
# colnames(CE_BASE_SERPENTES_Notificados)[1] <- "Semana_Epidemiológica"
# 
# rownames(CE_BASE_SERPENTES_Notificados) <- c(1:nrow(CE_BASE_SERPENTES_Notificados))
# 
# write.csv (CE_BASE_SERPENTES_Notificados, 
#            paste0("Tabulacoes_R/Peconhentos/RS", RS, "_CE_Notificados.csv"), 
#            row.names = FALSE)
# 
# AUX_GRAF <- CE_BASE_SERPENTES_Notificados[, c(ncol(CE_BASE_SERPENTES_Notificados), ncol(CE_BASE_SERPENTES_Notificados) -1, ncol(CE_BASE_SERPENTES_Notificados) -2)]
# 
# ###Criando uma coluna de ordem das se para o R não colocar em ordem numérica.
# 
# AUX_GRAF$Ordem <- c(1: nrow(CE_BASE_SERPENTES_Notificados))
# 
# ###Puxando o período sazonal atual para o gráfico de linhas
# 
# AUX_GRAF$`2025` <- CE_BASE_SERPENTES_Notificados$`2025`
# 
# AUX_GRAF$Sem_EPI <-as.character(c("01",  "02", "03",  "04",  "05",  "06",  "07", 
#                                   "08",  "09",  "10",  "11",  "12",  "13",  "14",  
#                                   "15",  "16",  "17",  "18",  "19",  "20",  "21",  
#                                   "22",  "23",  "24",  "25",  "26",  "27",  "28",  
#                                   "28",  "30",  "31",  "32",  "33", "34",  "35",  
#                                   "36",  "37",  "38",  "39",  "40",  "41",  "42",  
#                                   "43",  "44",  "45",  "46",  "47",  "48",  "49",  
#                                   "50",  "51",  "52",  "53"))
# 
# RS_PECONHENTOS_GRAF_CE_Serpentes <- ggplot(AUX_GRAF, aes(Ordem))  +
#   theme(axis.text.x = element_text(angle = 90, 
#                                    vjust = .5,
#                                    face = "bold",
#                                    size = 12)) +
#   labs(caption = Fonte,
#        title = "CANAL ENDÊMICO - ACIDENTES COM SERPENTES",
#        subtitle = paste0("Casos Notificados - ", RS, "ª Regional de Saúde")) +
#   theme(
#     panel.grid.major = element_line(color = "#C0C0C0"),
#     panel.grid.minor = element_blank(),
#     panel.background = element_rect(fill = "#B22222"),
#     plot.title = element_text(face = "bold",
#                               size = 24,
#                               colour = "#1C1C1C"),
#     plot.caption = element_text(hjust = 0, size = 12)
#   ) +
#   geom_area(aes(y = Lim_Superior), 
#             fill = "#F0E68C",
#             alpha = 0.9) +
#   geom_area(aes( y = Mediana), 
#             fill = "#556B2F") +
#   geom_line(aes( y = `2025`), 
#             stat = "identity", 
#             color = "black", 
#             linewidth = 1.5) +
#   xlab("Semana Epidemiológica") +
#   ylab("Número de Casos") +
#   scale_x_continuous(breaks = c(1:53), 
#                      label = AUX_GRAF$Sem_EPI) +
#   scale_y_continuous(expand = expansion(mult = c(0, 0.05)))
# 
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

RS_PECONHENTOS_GRAF_CE_Aranhas <- ggplot(AUX_GRAF, aes(Ordem))  +
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = .5,
                                   face = "bold",
                                   size = 12)) +
  labs(caption = Fonte,
       title = "Canal Endêmico - Acidentes com Aranhas",
       subtitle = paste0("Casos Notificados - ", RS, "ª Regional de Saúde")) +
  theme(
    panel.grid.major = element_line(color = "#C0C0C0"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#B22222"),
    plot.title = element_text(face = "bold",
                              size = 24,
                              colour = "#1C1C1C"),
    plot.caption = element_text(hjust = 0, 
                                size = 12),
    plot.subtitle = element_text(size = 12)
  ) +
  geom_area(aes(y = Lim_Superior), 
            fill = "#F0E68C",
            alpha = 0.9) +
  geom_area(aes( y = Mediana), 
            fill = "#556B2F") +
  geom_line(aes( y = `2025`), 
            stat = "identity", 
            color = "black", 
            linewidth = 1.5) +
  xlab("Semana Epidemiológica") +
  ylab("Número de Casos") +
  scale_x_continuous(breaks = c(1:53), 
                     label = AUX_GRAF$Sem_EPI) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)
  )
  )

# #####################################   ESCORPIÕES     #############################################################################
# 
# CE_BASE_ESCORPIOES_Notificados[(nrow(CE_BASE_ESCORPIOES_Notificados) +1), 1] <- "2025"
# CE_BASE_ESCORPIOES_Notificados[nrow(CE_BASE_ESCORPIOES_Notificados), 2:54] <- as.integer(data.frame(RS22_PECONHENTOS_2025_SE_ESCORPIOES_Notificados[nrow(RS22_PECONHENTOS_2025_SE_Notificados), 2:54]))
# 
# #####Utilizando objetos auxiliares porque se transpor o data frame direto ele transforma as variáveis em#############
# #####caracter.            NÃO FOI DESCARTADO AINDA OS PERÍODOS EPIDÊMICOS                               #############
# ##### VERIFICAR SE PODE-SE UTILIZAR A MÉDIA COMO LIMITE INFERIOR.                                       #############
# 
# AUX <- CE_BASE_ESCORPIOES_Notificados[, -1]
# 
# AUX <- t(AUX)
# 
# AUX2 <- CE_BASE_ESCORPIOES_Notificados[, 1]
# 
# colnames(AUX) <- AUX2
# 
# CE_BASE_ESCORPIOES_Notificados <- as.data.frame(AUX)
# 
# CE_BASE_ESCORPIOES_Notificados <- CE_BASE_ESCORPIOES_Notificados[, Anos_Analise]
# 
# ######Criando a coluna de média no data.frame#####################
# 
# CE_BASE_ESCORPIOES_Notificados <- CE_BASE_ESCORPIOES_Notificados %>%
#   mutate(Mediana = apply(CE_BASE_ESCORPIOES_Notificados[,1: (ncol(CE_BASE_ESCORPIOES_Notificados)-1)], 1 , median))
# 
# ######Criando a coluna de Desvio Padrão no data frame###############
# 
# CE_BASE_ESCORPIOES_Notificados <- CE_BASE_ESCORPIOES_Notificados %>%
#   mutate(Desvio_Padrao = apply(CE_BASE_ESCORPIOES_Notificados[,1: (ncol(CE_BASE_ESCORPIOES_Notificados) -2)], 1 , sd))
# 
# ###### Criando a coluna de Média + 2(DP)
# 
# CE_BASE_ESCORPIOES_Notificados <- CE_BASE_ESCORPIOES_Notificados %>%
#   mutate(Lim_Superior = (Mediana + 1.96 * Desvio_Padrao))
# 
# CE_BASE_ESCORPIOES_Notificados[, (ncol(CE_BASE_ESCORPIOES_Notificados)+1)] <- rownames(CE_BASE_ESCORPIOES_Notificados)
# 
# CE_BASE_ESCORPIOES_Notificados <- CE_BASE_ESCORPIOES_Notificados[, c(ncol(CE_BASE_ESCORPIOES_Notificados), 1:(ncol(CE_BASE_ESCORPIOES_Notificados) -1))]
# 
# CE_BASE_ESCORPIOES_Notificados[,1] <- c(1:53)
# 
# colnames(CE_BASE_ESCORPIOES_Notificados)[1] <- "Semana_Epidemiológica"
# 
# rownames(CE_BASE_ESCORPIOES_Notificados) <- c(1:nrow(CE_BASE_ESCORPIOES_Notificados))
# 
# write.csv (CE_BASE_ESCORPIOES_Notificados, 
#            paste0("Tabulacoes_R/Peconhentos/RS", RS, "_CE_Notificados.csv"), 
#            row.names = FALSE)
# 
# AUX_GRAF <- CE_BASE_ESCORPIOES_Notificados[, c(ncol(CE_BASE_ESCORPIOES_Notificados), ncol(CE_BASE_ESCORPIOES_Notificados) -1, ncol(CE_BASE_ESCORPIOES_Notificados) -2)]
# 
# ###Criando uma coluna de ordem das se para o R não colocar em ordem numérica.
# 
# AUX_GRAF$Ordem <- c(1: nrow(CE_BASE_ESCORPIOES_Notificados))
# 
# ###Puxando o período sazonal atual para o gráfico de linhas
# 
# AUX_GRAF$`2025` <- CE_BASE_ESCORPIOES_Notificados$`2025`
# 
# AUX_GRAF$Sem_EPI <-as.character(c("01",  "02", "03",  "04",  "05",  "06",  "07", 
#                                   "08",  "09",  "10",  "11",  "12",  "13",  "14",  
#                                   "15",  "16",  "17",  "18",  "19",  "20",  "21",  
#                                   "22",  "23",  "24",  "25",  "26",  "27",  "28",  
#                                   "28",  "30",  "31",  "32",  "33", "34",  "35",  
#                                   "36",  "37",  "38",  "39",  "40",  "41",  "42",  
#                                   "43",  "44",  "45",  "46",  "47",  "48",  "49",  
#                                   "50",  "51",  "52",  "53"))
# 
# RS_PECONHENTOS_GRAF_CE_Escorpiao <- ggplot(AUX_GRAF, aes(Ordem))  +
#   theme(axis.text.x = element_text(angle = 90, 
#                                    vjust = .5,
#                                    face = "bold",
#                                    size = 12)) +
#   labs(caption = Fonte,
#        title = "CANAL ENDÊMICO - ACIDENTES COM ESCORPIÕES",
#        subtitle = paste0("Casos Notificados - ", RS, "ª Regional de Saúde")) +
#   theme(
#     panel.grid.major = element_line(color = "#C0C0C0"),
#     panel.grid.minor = element_blank(),
#     panel.background = element_rect(fill = "#B22222"),
#     plot.title = element_text(face = "bold",
#                               size = 24,
#                               colour = "#1C1C1C"),
#     plot.caption = element_text(hjust = 0, size = 12)
#   ) +
#   geom_area(aes(y = Lim_Superior), 
#             fill = "#F0E68C",
#             alpha = 0.9) +
#   geom_area(aes( y = Mediana), 
#             fill = "#556B2F") +
#   geom_line(aes( y = `2025`), 
#             stat = "identity", 
#             color = "black", 
#             linewidth = 1.5) +
#   xlab("Semana Epidemiológica") +
#   ylab("Número de Casos") +
#   scale_x_continuous(breaks = c(1:53), 
#                      label = AUX_GRAF$Sem_EPI) +
#   scale_y_continuous(expand = expansion(mult = c(0, 0.05)
#   )
#   )
# 
# #####################################   ABELHAS     #############################################################################
# 
# CE_BASE_ABELHAS_Notificados[(nrow(CE_BASE_ABELHAS_Notificados) +1), 1] <- "2025"
# CE_BASE_ABELHAS_Notificados[nrow(CE_BASE_ABELHAS_Notificados), 2:54] <- as.integer(data.frame(RS22_PECONHENTOS_2025_SE_ABELHAS_Notificados[nrow(RS22_PECONHENTOS_2025_SE_Notificados), 2:54]))
# 
# #####Utilizando objetos auxiliares porque se transpor o data frame direto ele transforma as variáveis em#############
# #####caracter.            NÃO FOI DESCARTADO AINDA OS PERÍODOS EPIDÊMICOS                               #############
# ##### VERIFICAR SE PODE-SE UTILIZAR A MÉDIA COMO LIMITE INFERIOR.                                       #############
# 
# AUX <- CE_BASE_ABELHAS_Notificados[, -1]
# 
# AUX <- t(AUX)
# 
# AUX2 <- CE_BASE_ABELHAS_Notificados[, 1]
# 
# colnames(AUX) <- AUX2
# 
# CE_BASE_ABELHAS_Notificados <- as.data.frame(AUX)
# 
# CE_BASE_ABELHAS_Notificados <- CE_BASE_ABELHAS_Notificados[, Anos_Analise]
# 
# ######Criando a coluna de média no data.frame#####################
# 
# CE_BASE_ABELHAS_Notificados <- CE_BASE_ABELHAS_Notificados %>%
#   mutate(Mediana = apply(CE_BASE_ABELHAS_Notificados[,1: (ncol(CE_BASE_ABELHAS_Notificados)-1)], 1 , median))
# 
# ######Criando a coluna de Desvio Padrão no data frame###############
# 
# CE_BASE_ABELHAS_Notificados <- CE_BASE_ABELHAS_Notificados %>%
#   mutate(Desvio_Padrao = apply(CE_BASE_ABELHAS_Notificados[,1: (ncol(CE_BASE_ABELHAS_Notificados) -2)], 1 , sd))
# 
# ###### Criando a coluna de Média + 2(DP)
# 
# CE_BASE_ABELHAS_Notificados <- CE_BASE_ABELHAS_Notificados %>%
#   mutate(Lim_Superior = (Mediana + 1.96 * Desvio_Padrao))
# 
# CE_BASE_ABELHAS_Notificados[, (ncol(CE_BASE_ABELHAS_Notificados)+1)] <- rownames(CE_BASE_ABELHAS_Notificados)
# 
# CE_BASE_ABELHAS_Notificados <- CE_BASE_ABELHAS_Notificados[, c(ncol(CE_BASE_ABELHAS_Notificados), 1:(ncol(CE_BASE_ABELHAS_Notificados) -1))]
# 
# CE_BASE_ABELHAS_Notificados[,1] <- c(1:53)
# 
# colnames(CE_BASE_ABELHAS_Notificados)[1] <- "Semana_Epidemiológica"
# 
# rownames(CE_BASE_ABELHAS_Notificados) <- c(1:nrow(CE_BASE_ABELHAS_Notificados))
# 
# write.csv (CE_BASE_ABELHAS_Notificados, 
#            paste0("Tabulacoes_R/Peconhentos/RS", RS, "_CE_Notificados.csv"), 
#            row.names = FALSE)
# 
# AUX_GRAF <- CE_BASE_ABELHAS_Notificados[, c(ncol(CE_BASE_ABELHAS_Notificados), ncol(CE_BASE_ABELHAS_Notificados) -1, ncol(CE_BASE_ABELHAS_Notificados) -2)]
# 
# ###Criando uma coluna de ordem das se para o R não colocar em ordem numérica.
# 
# AUX_GRAF$Ordem <- c(1: nrow(CE_BASE_ABELHAS_Notificados))
# 
# ###Puxando o período sazonal atual para o gráfico de linhas
# 
# AUX_GRAF$`2025` <- CE_BASE_ABELHAS_Notificados$`2025`
# 
# AUX_GRAF$Sem_EPI <-as.character(c("01",  "02", "03",  "04",  "05",  "06",  "07", 
#                                   "08",  "09",  "10",  "11",  "12",  "13",  "14",  
#                                   "15",  "16",  "17",  "18",  "19",  "20",  "21",  
#                                   "22",  "23",  "24",  "25",  "26",  "27",  "28",  
#                                   "28",  "30",  "31",  "32",  "33", "34",  "35",  
#                                   "36",  "37",  "38",  "39",  "40",  "41",  "42",  
#                                   "43",  "44",  "45",  "46",  "47",  "48",  "49",  
#                                   "50",  "51",  "52",  "53"))
# 
# RS_PECONHENTOS_GRAF_CE_Abelha <- ggplot(AUX_GRAF, aes(Ordem))  +
#   theme(axis.text.x = element_text(angle = 90, 
#                                    vjust = .5,
#                                    face = "bold",
#                                    size = 12)) +
#   labs(caption = Fonte,
#        title = "CANAL ENDÊMICO - ACIDENTES COM ABELHAS",
#        subtitle = paste0("Casos Notificados - ", RS, "ª Regional de Saúde")) +
#   theme(
#     panel.grid.major = element_line(color = "#C0C0C0"),
#     panel.grid.minor = element_blank(),
#     panel.background = element_rect(fill = "#B22222"),
#     plot.title = element_text(face = "bold",
#                               size = 24,
#                               colour = "#1C1C1C"),
#     plot.caption = element_text(hjust = 0, 
#                                 size = 12)
#   ) +
#   geom_area(aes(y = Lim_Superior), 
#             fill = "#F0E68C", 
#             alpha = 0.9) +
#   geom_area(aes( y = Mediana), 
#             fill = "#556B2F") +
#   geom_line(aes( y = `2025`), 
#             stat = "identity", 
#             color = "black", 
#             linewidth = 1.5) +
#   xlab("Semana Epidemiológica") +
#   ylab("Número de Casos") +
#   scale_x_continuous(breaks = c(1:53), 
#                      label = AUX_GRAF$Sem_EPI) +
#   scale_y_continuous(expand = expansion(mult = c(0, 0.05)
#   )
#   )
# 
# #####################################   LAGARTAS     #############################################################################
# 
# CE_BASE_LAGARTAS_Notificados[(nrow(CE_BASE_LAGARTAS_Notificados) +1), 1] <- "2025"
# CE_BASE_LAGARTAS_Notificados[nrow(CE_BASE_LAGARTAS_Notificados), 2:54] <- as.integer(data.frame(RS22_PECONHENTOS_2025_SE_LAGARTAS_Notificados[nrow(RS22_PECONHENTOS_2025_SE_Notificados), 2:54]))
# 
# #####Utilizando objetos auxiliares porque se transpor o data frame direto ele transforma as variáveis em#############
# #####caracter.            NÃO FOI DESCARTADO AINDA OS PERÍODOS EPIDÊMICOS                               #############
# ##### VERIFICAR SE PODE-SE UTILIZAR A MÉDIA COMO LIMITE INFERIOR.                                       #############
# 
# AUX <- CE_BASE_LAGARTAS_Notificados[, -1]
# 
# AUX <- t(AUX)
# 
# AUX2 <- CE_BASE_LAGARTAS_Notificados[, 1]
# 
# colnames(AUX) <- AUX2
# 
# CE_BASE_LAGARTAS_Notificados <- as.data.frame(AUX)
# 
# CE_BASE_LAGARTAS_Notificados <- CE_BASE_LAGARTAS_Notificados[, Anos_Analise]
# 
# ######Criando a coluna de média no data.frame#####################
# 
# CE_BASE_LAGARTAS_Notificados <- CE_BASE_LAGARTAS_Notificados %>%
#   mutate(Mediana = apply(CE_BASE_LAGARTAS_Notificados[,1: (ncol(CE_BASE_LAGARTAS_Notificados)-1)], 1 , median))
# 
# ######Criando a coluna de Desvio Padrão no data frame###############
# 
# CE_BASE_LAGARTAS_Notificados <- CE_BASE_LAGARTAS_Notificados %>%
#   mutate(Desvio_Padrao = apply(CE_BASE_LAGARTAS_Notificados[,1: (ncol(CE_BASE_LAGARTAS_Notificados) -2)], 1 , sd))
# 
# ###### Criando a coluna de Média + 2(DP)
# 
# CE_BASE_LAGARTAS_Notificados <- CE_BASE_LAGARTAS_Notificados %>%
#   mutate(Lim_Superior = (Mediana + 1.96 * Desvio_Padrao))
# 
# CE_BASE_LAGARTAS_Notificados[, (ncol(CE_BASE_LAGARTAS_Notificados)+1)] <- rownames(CE_BASE_LAGARTAS_Notificados)
# 
# CE_BASE_LAGARTAS_Notificados <- CE_BASE_LAGARTAS_Notificados[, c(ncol(CE_BASE_LAGARTAS_Notificados), 1:(ncol(CE_BASE_LAGARTAS_Notificados) -1))]
# 
# CE_BASE_LAGARTAS_Notificados[,1] <- c(1:53)
# 
# colnames(CE_BASE_LAGARTAS_Notificados)[1] <- "Semana_Epidemiológica"
# 
# rownames(CE_BASE_LAGARTAS_Notificados) <- c(1:nrow(CE_BASE_LAGARTAS_Notificados))
# 
# write.csv (CE_BASE_LAGARTAS_Notificados, 
#            paste0("Tabulacoes_R/Peconhentos/RS", RS, "_CE_Notificados.csv"), 
#            row.names = FALSE)
# 
# AUX_GRAF <- CE_BASE_LAGARTAS_Notificados[, c(ncol(CE_BASE_LAGARTAS_Notificados), ncol(CE_BASE_LAGARTAS_Notificados) -1, ncol(CE_BASE_LAGARTAS_Notificados) -2)]
# 
# ###Criando uma coluna de ordem das se para o R não colocar em ordem numérica.
# 
# AUX_GRAF$Ordem <- c(1: nrow(CE_BASE_LAGARTAS_Notificados))
# 
# ###Puxando o período sazonal atual para o gráfico de linhas
# 
# AUX_GRAF$`2025` <- CE_BASE_LAGARTAS_Notificados$`2025`
# 
# AUX_GRAF$Sem_EPI <-as.character(c("01",  "02", "03",  "04",  "05",  "06",  "07", 
#                                   "08",  "09",  "10",  "11",  "12",  "13",  "14",  
#                                   "15",  "16",  "17",  "18",  "19",  "20",  "21",  
#                                   "22",  "23",  "24",  "25",  "26",  "27",  "28",  
#                                   "28",  "30",  "31",  "32",  "33", "34",  "35",  
#                                   "36",  "37",  "38",  "39",  "40",  "41",  "42",  
#                                   "43",  "44",  "45",  "46",  "47",  "48",  "49",  
#                                   "50",  "51",  "52",  "53"))
# 
# RS_PECONHENTOS_GRAF_CE_Lagarta <- ggplot(AUX_GRAF, aes(Ordem))  +
#   theme(axis.text.x = element_text(angle = 90, 
#                                    vjust = .5,
#                                    face = "bold",
#                                    size = 12)) +
#   labs(caption = Fonte,
#        title = "CANAL ENDÊMICO - ACIDENTES COM LAGARTAS",
#        subtitle = paste0("Casos Notificados - ", RS, "ª Regional de Saúde")) +
#   theme(
#     panel.grid.major = element_line(color = "#C0C0C0"),
#     panel.grid.minor = element_blank(),
#     panel.background = element_rect(fill = "#B22222"),
#     plot.title = element_text(face = "bold",
#                               size = 24,
#                               colour = "#1C1C1C"),
#     plot.caption = element_text(hjust = 0, size = 12)
#   ) +
#   geom_area(aes(y = Lim_Superior), 
#             fill = "#F0E68C", 
#             alpha = 0.9) +
#   geom_area(aes( y = Mediana), 
#             fill = "#556B2F") +
#   geom_line(aes( y = `2025`), 
#             stat = "identity", 
#             color = "black", 
#             linewidth = 1.5) +
#   xlab("Semana Epidemiológica") +
#   ylab("Número de Casos") +
#   scale_x_continuous(breaks = c(1:53), 
#                      label = AUX_GRAF$Sem_EPI) +
#   scale_y_continuous(expand = expansion(mult = c(0, 0.05)
#   )
#   )
# 
# #####################################   OUTROS     #############################################################################
# 
# CE_BASE_OUTROS_Notificados[(nrow(CE_BASE_OUTROS_Notificados) +1), 1] <- "2025"
# CE_BASE_OUTROS_Notificados[nrow(CE_BASE_OUTROS_Notificados), 2:54] <- as.integer(data.frame(RS22_PECONHENTOS_2025_SE_OUTROS_Notificados[nrow(RS22_PECONHENTOS_2025_SE_Notificados), 2:54]))
# 
# #####Utilizando objetos auxiliares porque se transpor o data frame direto ele transforma as variáveis em#############
# #####caracter.            NÃO FOI DESCARTADO AINDA OS PERÍODOS EPIDÊMICOS                               #############
# ##### VERIFICAR SE PODE-SE UTILIZAR A MÉDIA COMO LIMITE INFERIOR.                                       #############
# 
# AUX <- CE_BASE_OUTROS_Notificados[, -1]
# 
# AUX <- t(AUX)
# 
# AUX2 <- CE_BASE_OUTROS_Notificados[, 1]
# 
# colnames(AUX) <- AUX2
# 
# CE_BASE_OUTROS_Notificados <- as.data.frame(AUX)
# 
# CE_BASE_OUTROS_Notificados <- CE_BASE_OUTROS_Notificados[, Anos_Analise]
# 
# ######Criando a coluna de média no data.frame#####################
# 
# CE_BASE_OUTROS_Notificados <- CE_BASE_OUTROS_Notificados %>%
#   mutate(Mediana = apply(CE_BASE_OUTROS_Notificados[,1: (ncol(CE_BASE_OUTROS_Notificados)-1)], 1 , median))
# 
# ######Criando a coluna de Desvio Padrão no data frame###############
# 
# CE_BASE_OUTROS_Notificados <- CE_BASE_OUTROS_Notificados %>%
#   mutate(Desvio_Padrao = apply(CE_BASE_OUTROS_Notificados[,1: (ncol(CE_BASE_OUTROS_Notificados) -2)], 1 , sd))
# 
# ###### Criando a coluna de Média + 2(DP)
# 
# CE_BASE_OUTROS_Notificados <- CE_BASE_OUTROS_Notificados %>%
#   mutate(Lim_Superior = (Mediana + 1.96 * Desvio_Padrao))
# 
# CE_BASE_OUTROS_Notificados[, (ncol(CE_BASE_OUTROS_Notificados)+1)] <- rownames(CE_BASE_OUTROS_Notificados)
# 
# CE_BASE_OUTROS_Notificados <- CE_BASE_OUTROS_Notificados[, c(ncol(CE_BASE_OUTROS_Notificados), 1:(ncol(CE_BASE_OUTROS_Notificados) -1))]
# 
# CE_BASE_OUTROS_Notificados[,1] <- c(1:53)
# 
# colnames(CE_BASE_OUTROS_Notificados)[1] <- "Semana_Epidemiológica"
# 
# rownames(CE_BASE_OUTROS_Notificados) <- c(1:nrow(CE_BASE_OUTROS_Notificados))
# 
# write.csv (CE_BASE_OUTROS_Notificados, 
#            paste0("Tabulacoes_R/Peconhentos/RS", RS, "_CE_Notificados.csv"), 
#            row.names = FALSE)
# 
# AUX_GRAF <- CE_BASE_OUTROS_Notificados[, c(ncol(CE_BASE_OUTROS_Notificados), ncol(CE_BASE_OUTROS_Notificados) -1, ncol(CE_BASE_OUTROS_Notificados) -2)]
# 
# ###Criando uma coluna de ordem das se para o R não colocar em ordem numérica.
# 
# AUX_GRAF$Ordem <- c(1: nrow(CE_BASE_OUTROS_Notificados))
# 
# ###Puxando o período sazonal atual para o gráfico de linhas
# 
# AUX_GRAF$`2025` <- CE_BASE_OUTROS_Notificados$`2025`
# 
# AUX_GRAF$Sem_EPI <-as.character(c("01",  "02", "03",  "04",  "05",  "06",  "07", 
#                                   "08",  "09",  "10",  "11",  "12",  "13",  "14",  
#                                   "15",  "16",  "17",  "18",  "19",  "20",  "21",  
#                                   "22",  "23",  "24",  "25",  "26",  "27",  "28",  
#                                   "28",  "30",  "31",  "32",  "33", "34",  "35",  
#                                   "36",  "37",  "38",  "39",  "40",  "41",  "42",  
#                                   "43",  "44",  "45",  "46",  "47",  "48",  "49",  
#                                   "50",  "51",  "52",  "53"))
# 
# RS_PECONHENTOS_GRAF_CE_Outros <- ggplot(AUX_GRAF, aes(Ordem))  +
#   theme(axis.text.x = element_text(angle = 90, 
#                                    vjust = .5,
#                                    face = "bold",
#                                    size = 12)) +
#   labs(caption = Fonte,
#        title = "CANAL ENDÊMICO - ACIDENTES COM OUTROS ANIMAIS PEÇONHENTOS",
#        subtitle = paste0("Casos Notificados - ", RS, "ª Regional de Saúde")) +
#   theme(
#     panel.grid.major = element_line(color = "#C0C0C0"),
#     panel.grid.minor = element_blank(),
#     panel.background = element_rect(fill = "#B22222"),
#     plot.title = element_text(face = "bold",
#                               size = 24,
#                               colour = "#1C1C1C"),
#     plot.caption = element_text(hjust = 0, 
#                                 size = 12)
#   ) +
#   geom_area(aes(y = Lim_Superior),
#             fill = "#F0E68C",
#             alpha = 0.9) +
#   geom_area(aes( y = Mediana), 
#             fill = "#556B2F") +
#   geom_line(aes( y = `2025`), 
#             stat = "identity", 
#             color = "black", 
#             linewidth = 1.5) +
#   xlab("Semana Epidemiológica") +
#   ylab("Número de Casos") +
#   scale_x_continuous(breaks = c(1:53), 
#                      label = AUX_GRAF$Sem_EPI) +
#   scale_y_continuous(expand = expansion(mult = c(0, 0.05)
#   )
#   )

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

######################################################################################################################################
#################################################    Séries Históricas     ###########################################################

###################################  Notificados  ##########################################################################

AUX <- RS_Serie_Historica_Geral[-1, c(1, 5:36)]

AUX[, 1] <- as.factor(AUX[, 1])

colnames(AUX)[1] <- "Ano"

RS_PECONHENTOS_GRAF_SERIE_HIST_Geral <- ggplot(AUX, aes(x = Ano,
                                                        y = Notificados, 
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
       title = "Série Histórica - Casos Notificados (2016 a 2025)",
       subtitle = "Notificações referentes ao município de ocorrência") +
  geom_label(aes(label = Notificados), 
             size = 6, 
             alpha = 0.5,
             vjust = -0.5)  +
  scale_y_continuous(expand = expansion(mult = c(0.5, 0.5)
  )
  ) +
  Theme() +
  theme(legend.position = "bottom",  
        legend.title = element_text(face = "bold",
                                    size = 14), 
        axis.text.x = element_text(angle = 0),
        legend.text = element_text(size = 14), 
        plot.subtitle = element_text(hjust = 0,
                                     size = 12),
        plot.caption = element_text(size = 12,
                                    hjust = 0),
        plot.title = element_text(hjust = 0, 
                                  face = "bold",
                                  size = 20)    
  )

########################################    Serpentes, Escorpiões e lagartas    #######################################################

AUX <- RS_Serie_Historica_Tipo_Acid[-1, c(1, 5, 7:10)]

AUX[, 1] <- as.factor(AUX[, 1])

colnames(AUX)[1] <- "Ano"

AUX <- pivot_longer(AUX, 
                    2:6, 
                    names_to = "Animal", 
                    values_to = "Notificacoes")

RS_PECONHENTOS_GRAF_SERIE_HIST_DEMAIS <- ggplot(AUX, aes(x = Ano, 
                                                           y = Notificacoes)) +
  geom_line(aes(x = Ano,
                y = Notificacoes,
                colour = Animal,
                group = Animal),
            linewidth = 1.5) +
  geom_point(fill = "grey",
             size = 4,
             shape = 21) + 
  labs(caption = Fonte, 
       y = "Número de Casos",
       x = NULL,
       title = "Série Histórica - Demais Animais Peçonhentos (2016 a 2025)",
       subtitle = "Notificações (Abelha, Escorpião, Lagarta, Serpente, Outros) referentes ao município de ocorrência") +
  # geom_label(aes(label = Notificacoes), 
  #            size = 4, 
  #            alpha = 0.5,
  #            vjust = -0.5)  +
  scale_colour_manual(name = "Animal",
                    values = c("Lagarta" = "#6495ED", 
                               "Serpente" = "#4F4F4F",
                               "Abelha" = "#8B008B",
                               "Outros" = "#FF4500",
                               "Escorpiao" = "#008B8B")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.2, 0.2))) +
  Theme() +
  theme(legend.position = "bottom",  
        legend.title = element_text(face = "bold",
                                    size = 18), 
        axis.text.x = element_text(angle = 0),
        legend.text = element_text(size = 14), 
        plot.subtitle = element_text(hjust = 0,
                                     size = 12),
        plot.caption = element_text(size = 12,
                                    hjust = 0),
        plot.title = element_text(hjust = 0, 
                                  face = "bold",
                                  size = 20)    
  )

########################################  Aranhas   ####################################################################

AUX <- RS_Serie_Historica_Tipo_Acid[-1, c(1, 5:11)]  
  
AUX[, 1] <- as.factor(AUX[, 1])
  
colnames(AUX)[1] <- "Ano"
  
RS_PECONHENTOS_GRAF_SERIE_HIST_Aranha <- ggplot(AUX, aes(x = Ano,
                                                         y = Aranha,
                                                         group = 1)) +
  geom_line(linewidth = 1.8,
            colour = "black") +
  geom_point(fill = "grey",
             size = 6,
             shape = 21) +
  labs(caption = Fonte, 
       y = "Número de Casos",
       x = NULL,
       title = "Série Histórica - Acidentes com Aranhas (2016 a 2025)",
       subtitle = "Notificações referentes ao município de ocorrência") +
  geom_label(aes(label = Aranha), 
             size = 6, 
             alpha = 0.5,
             vjust = -0.5)  +
  scale_y_continuous(expand = expansion(mult = c(0.5, 0.5))) +
  Theme() +
  theme(legend.position = "bottom",  
        legend.title = element_text(face = "bold",
                                    size = 14), 
        axis.text.x = element_text(angle = 0),
        legend.text = element_text(size = 14), 
        plot.subtitle = element_text(hjust = 0,
                                     size = 12),
        plot.caption = element_text(size = 12,
                                    hjust = 0),
        plot.title = element_text(hjust = 0, 
                                  face = "bold",
                                  size = 20)    
  )

# ################################################   Serpentes    ######################################################
# 
# RS_PECONHENTOS_GRAF_SERIE_HIST_Serpentes <-  ggplot(AUX, aes(x = Ano,
#                                                              y = Serpente,
#                                                              group = 1)) +
#   geom_line(linewidth = 1.8,
#             colour = "black") +
#   geom_point(fill = "grey",
#              size = 7,
#              shape = 21) +
#   labs(caption = Fonte, 
#        y = "Número de Casos",
#        x = NULL,
#        title = "SÉRIE HISTÓRICA - NOTIFICAÇÕES DE ACIDENTES COM SERPENTES (2016 a 2025)",
#        subtitle = "Notificações referentes ao município de residência") +
#   geom_label(aes(label = Serpente), 
#              size = 6, 
#              alpha = 0.5,
#              vjust = -0.5)  +
#   scale_y_continuous(expand = expansion(mult = c(0.5, 0.5)
#   )
#   ) +
#   Theme() +
#   theme(axis.text.x = element_text(angle = 0),
#         plot.caption = element_text(hjust = 0, 
#                                     size = 13),
#         plot.subtitle = element_text(size = 20)
#   )
# 

# ################################################   Escorpiões    ######################################################
# 
# RS_PECONHENTOS_GRAF_SERIE_HIST_Escorpiao <-  ggplot(AUX, aes(x = Ano,
#                                                              y = Escorpiao,
#                                                              group = 1)) +
#   geom_line(linewidth = 1.8,
#             colour = "black") +
#   geom_point(fill = "grey",
#              size = 7,
#              shape = 21) +
#   labs(caption = Fonte, 
#        y = "Número de Casos",
#        x = NULL,
#        title = "SÉRIE HISTÓRICA - NOTIFICAÇÕES DE ACIDENTES COM ESCORPIÕES (2016 a 2025)",
#        subtitle = "Notificações referentes ao município de residência") +
#   geom_label(aes(label = Escorpiao), 
#              size = 6, 
#              alpha = 0.5,
#              vjust = -0.5)  +
#   scale_y_continuous(expand = expansion(mult = c(0.5, 0.5)
#   )
#   ) +
#   Theme() +
#   theme(axis.text.x = element_text(angle = 0),
#         plot.caption = element_text(hjust = 0, 
#                                     size = 13),
#         plot.subtitle = element_text(size = 20)
#   )
# 
# #######################################   Abelhas   ############################################################################
# 
# RS_PECONHENTOS_GRAF_SERIE_HIST_Abelha <-  ggplot(AUX, aes(x = Ano,
#                                                           y = Abelha,
#                                                           group = 1)
# ) +
#   geom_line(linewidth = 1.8,
#             colour = "black") +
#   geom_point(fill = "grey",
#              size = 7,
#              shape = 21) +
#   labs(caption = Fonte, 
#        y = "Número de Casos",
#        x = NULL,
#        title = "SÉRIE HISTÓRICA - NOTIFICAÇÕES DE ACIDENTES COM ABELHAS (2016 a 2025)",
#        subtitle = "Notificações referentes ao município de residência") +
#   geom_label(aes(label = Abelha), 
#              size = 6, 
#              alpha = 0.5,
#              vjust = -0.5)  +
#   scale_y_continuous(expand = expansion(mult = c(0.5, 0.5)
#   )
#   ) +
#   Theme() +
#   theme(axis.text.x = element_text(angle = 0),
#         plot.caption = element_text(hjust = 0, 
#                                     size = 13),
#         plot.subtitle = element_text(size = 20)
#   )
# 
# ##################################   Lagarta  ####################################################################
# 
# RS_PECONHENTOS_GRAF_SERIE_HIST_Lagarta <-  ggplot(AUX, aes(x = Ano,
#                                                            y = Lagarta,
#                                                            group = 1)) +
#   geom_line(linewidth = 1.8,
#             colour = "black") +
#   geom_point(fill = "grey",
#              size = 7,
#              shape = 21) +
#   labs(caption = Fonte, 
#        y = "Número de Casos",
#        x = NULL,
#        title = "SÉRIE HISTÓRICA - NOTIFICAÇÕES DE ACIDENTES COM LAGARTAS (2016 a 2025)",
#        subtitle = "Notificações referentes ao município de residência") +
#   geom_label(aes(label = Lagarta), 
#              size = 6, 
#              alpha = 0.5,
#              vjust = -0.5)  +
#   scale_y_continuous(expand = expansion(mult = c(0.5, 0.5)
#   )
#   ) +
#   Theme() +
#   theme(axis.text.x = element_text(angle = 0),
#         plot.caption = element_text(hjust = 0, 
#                                     size = 13),
#         plot.subtitle = element_text(size = 20)
#   )
# 
# ####################################   Outros    #################################################################
# 
# RS_PECONHENTOS_GRAF_SERIE_HIST_Outros <- ggplot(AUX, aes(x = Ano,
#                                                          y = Outros,
#                                                          group = 1)
# ) +
#   geom_line(linewidth = 1.8,
#             colour = "black") +
#   geom_point(fill = "grey",
#              size = 7,
#              shape = 21) +
#   labs(caption = Fonte, 
#        y = "Número de Casos",
#        x = NULL,
#        title = "SÉRIE HISTÓRICA - NOTIFICAÇÕES DE ACIDENTES COM OUTROS ANIMAIS PEÇONHENTOS (2016 a 2025)",
#        subtitle = "Notificações referentes ao município de residência") +
#   geom_label(aes(label = Outros), 
#              size = 4, 
#              alpha = 0.5,
#              vjust = -0.5)  +
#   scale_y_continuous(expand = expansion(mult = c(0.5, 0.5)
#   )
#   ) +
#   Theme() +
#   theme(axis.text.x = element_text(angle = 0),
#         plot.caption = element_text(hjust = 0, 
#                                     size = 13),
#         plot.subtitle = element_text(size = 20)
#   )

###################################################################################################################################
########################################### Gráfico de Incidência  ################################################################

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

RS_PECONHENTOS_GRAF_Incidencia_01 <- ggplot(AUX_GRAF[c(1:8),], 
                                            aes(x = Município)
) +
  labs(caption = Fonte, 
       y = "Incidência",
       x = NULL,
       title = "Incidência por Município (Ocorrência) na 22ª RS",
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
             size = 2.7, 
             alpha = 0.5,
             vjust = 0.1,
             nudge_x = -.30,
  )   +
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
             size = 2.7, 
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
  theme(legend.position = "bottom",  
        legend.title = element_text(face = "bold",
                                    size = 14), 
        axis.text.x = element_text(angle = 25),
        legend.text = element_text(size = 14), 
        plot.subtitle = element_text(hjust = 0,
                                     size = 12),
        plot.caption = element_text(size = 12,
                                    hjust = 0),
        plot.title = element_text(hjust = 0, 
                                  face = "bold",
                                  size = 20)    
  )

####################################################################################################

RS_PECONHENTOS_GRAF_Incidencia_02 <- ggplot(AUX_GRAF[c(9:16),], aes(x = Município)) +
  labs(caption = Fonte, 
       y = "Incidência",
       x = NULL,
       title = "Incidência por Município (Ocorrência) na 22ª RS",
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
  theme(legend.position = "bottom",  
        legend.title = element_text(face = "bold",
                                    size = 14), 
        axis.text.x = element_text(angle = 25),
        legend.text = element_text(size = 14), 
        plot.subtitle = element_text(hjust = 0,
                                     size = 12),
        plot.caption = element_text(size = 12,
                                    hjust = 0),
        plot.title = element_text(hjust = 0, 
                                  face = "bold",
                                  size = 20)    
  )

#######################################   Zona Ocorrência  ##################################################################
##################################### Gráfico total  ########################################################################

AUX <- RS_Serie_Historica_Geral[-c(1, nrow(RS_Serie_Historica_Geral)), c(1, 6:9)]

AUX <- apply(AUX[, -1], 2, sum)

AUX <- as.data.frame(AUX)

AUX[, 2] <- c("Urbana", "Rural", "Periurbana", "Ignorados")

colnames(AUX) <- c("Casos", "Zona")

RS_PECONHENTOS_GRAF_ZONA_REGIONAL_Historico <- ggplot(AUX, aes(x = Zona,
                                                               y = Casos)) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#E6E6FA",
           linewidth = 0.8) +
  labs(caption = Fonte, 
       y = "Número de Casos",
       x = "Zona de Ocorrência",
       title = "Zona de Ocorrência dos Acidentes Notificados (2016 - 2024)",
       subtitle = "Casos notificados no território da 22ª Regional de Saúde") +
  geom_label(aes(label = Casos), 
             size = 5, 
             alpha = 0.5,
             vjust = 0.1)  +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  Theme() +
  theme(legend.position = "bottom",  
        legend.title = element_text(face = "bold",
                                    size = 14), 
        axis.text.x = element_text(angle = 0),
        legend.text = element_text(size = 14), 
        plot.subtitle = element_text(hjust = 0,
                                     size = 12),
        plot.caption = element_text(size = 12,
                                    hjust = 0),
        plot.title = element_text(hjust = 0, 
                                  face = "bold",
                                  size = 20)    
  )

######################## Zona de Ocorrência Gráfico 2025  #############################################################

AUX <- RS_Serie_Historica_Geral[nrow(RS_Serie_Historica_Geral), 6:9]

AUX <- as.data.frame(t(AUX))

AUX[,2] <- c("Urbana", "Rural", "Periurbana", "Ignorado")

colnames(AUX) <- c("Casos", "Zona")

RS_PECONHENTOS_GRAF_ZONA_REGIONAL <- ggplot(AUX, aes(x = Zona, 
                                                     y = Casos)
) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#008B8B",
           linewidth = 0.8) +
  labs(caption = Fonte, 
       y = "Número de Casos",
       x = "Zona de Ocorrência",
       title = "Zona de Ocorrência dos Acidentes Notificados - 2025",
       subtitle = "Casos notificados no território da 22ª Regional de Saúde") +
  geom_label(aes(label = Casos), 
             size = 5, 
             alpha = 0.5,
             vjust = 0.1)  +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  Theme() +
  theme(legend.position = "bottom",  
        legend.title = element_text(face = "bold",
                                    size = 14), 
        axis.text.x = element_text(angle = 0),
        legend.text = element_text(size = 14), 
        plot.subtitle = element_text(hjust = 0,
                                     size = 12),
        plot.caption = element_text(size = 12,
                                    hjust = 0),
        plot.title = element_text(hjust = 0, 
                                  face = "bold",
                                  size = 20)    
  )

# #######################     Gráfico Sexo histórico    ########################################################
# 
# AUX <- RS_Serie_Historica_Geral[-c(1, nrow(RS_Serie_Historica_Geral)), c(1, 10:11)]
# 
# AUX <- apply(AUX[, -1], 2, sum)
# 
# AUX <- as.data.frame(AUX)
# 
# AUX[,2] <- c("Feminino", "Masculino")
# 
# colnames(AUX) <- c("Casos", "Sexo")
# 
# RS_PECONHENTOS_GRAF_SEXO_HIST <- ggplot(AUX, 
#                                         aes(x = Sexo, 
#                                             y = Casos)
# ) +
#   geom_bar(stat = "identity",
#            color = "black",
#            fill = "#DDA0DD",
#            linewidth = 0.8,
#            width = 0.4) +
#   labs(caption = Fonte, 
#        y = "Número de Casos",
#        x = "Sexo",
#        title = "Sexo dos Acidentes Notificados (2016 - 2024)",
#        subtitle = "Casos notificados no território da 22ª Regional de Saúde") +
#   geom_label(aes(label = Casos), 
#              size = 5, 
#              alpha = 0.5,
#              vjust = 0.1)  +
#   scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
#   Theme() +
#   theme(axis.text.x = element_text(angle = 0),
#         plot.caption = element_text(hjust = 0))

################################### SEXO 2025 ###########################################################################

AUX <- RS_Serie_Historica_Geral[nrow(RS_Serie_Historica_Geral), 10:11]

AUX <- as.data.frame(t(AUX))

AUX[,2] <- c("Feminino", "Masculino")

colnames(AUX) <- c("Casos", "Sexo")

RS_PECONHENTOS_GRAF_SEXO <- ggplot(AUX, aes(x = Sexo, 
                                            y = Casos)
) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#556B2F",
           linewidth = 0.8,
           width = 0.4) +
  labs(caption = Fonte, 
       y = "Número de Casos",
       x = "Sexo",
       title = "Sexo dos Acidentes Notificados - 2025",
       subtitle = "Casos notificados no território da 22ª Regional de Saúde") +
  geom_label(aes(label = Casos), 
             size = 5, 
             alpha = 0.5,
             vjust = 0.1)  +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  Theme() +
  theme(legend.position = "bottom",  
        legend.title = element_text(face = "bold",
                                    size = 14), 
        axis.text.x = element_text(angle = 0),
        legend.text = element_text(size = 14), 
        plot.subtitle = element_text(hjust = 0,
                                     size = 12),
        plot.caption = element_text(size = 12,
                                    hjust = 0),
        plot.title = element_text(hjust = 0, 
                                  face = "bold",
                                  size = 20)    
  )

####################################    Pirâmide Etária   ###########################################################

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

RS_PECONHENTOS_GRAF_PIRAMIDE <- ggplot(AUX, 
                                       aes(x = Pop,
                                           y = grupo_Idade_FACT, 
                                           fill = Sexo_Legenda)) +
  geom_col(color = "black",
           linewidth = 0.8) +
  labs(title = "Pirâmide Etária 22ª RS (2025)",
       y = "Faixa Etária",
       x = "Nº de Notificações",
       fill = "Sexo",
       caption = Fonte) +
  scale_x_continuous(labels = abs) +
  Theme() +
  theme(legend.position = "bottom",  
        legend.title = element_text(face = "bold",
                                    size = 14), 
        axis.text.x = element_text(angle = 0),
        legend.text = element_text(size = 14), 
        plot.subtitle = element_text(hjust = 0,
                                     size = 12),
        plot.caption = element_text(size = 12,
                                    hjust = 0),
        plot.title = element_text(hjust = 0, 
                                  face = "bold",
                                  size = 20)    
  )

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

RS_PECONHENTOS_GRAF_PIRAMIDE_HIST <- ggplot(AUX, 
                                            aes(x = Pop,
                                                y = grupo_Idade_FACT, 
                                                fill = Sexo_Legenda)) +
  geom_col(color = "black",
           linewidth = 0.8) +
  labs(title = "Pirâmide Etária 22ª RS (2016 - 2024)",
       y = "Faixa Etária",
       x = "Nº de Notificações",
       fill = "Sexo",
       caption = Fonte) +
  scale_x_continuous(labels = abs) +
  Theme() +
  theme(legend.position = "bottom",  
        legend.title = element_text(face = "bold",
                                    size = 14), 
        axis.text.x = element_text(angle = 0),
        legend.text = element_text(size = 14), 
        plot.subtitle = element_text(hjust = 0),
        plot.caption = element_text(size = 12,
                                    hjust = 0),
        plot.title = element_text(hjust = 0, 
                                  face = "bold",
                                  size = 20)    
  )
##########################################  Escolaridade  Histórico   ###############################################

AUX <- RS_Serie_Historica_Geral[-c(1, nrow(RS_Serie_Historica_Geral)), c(1, 18:26)]

AUX[nrow(AUX) +1, -1] <- apply(AUX[, -1], 2, sum)

AUX <- t(AUX[nrow(AUX),])

AUX <- as.data.frame(AUX[-1,])

AUX[, 2] <- as.factor(c("Analf.", "Fund. Incomp.", 
                        "Fundamental", "Médio Incomp.", 
                        "Médio", "Sup. Incomp.", "Superior",
                        "Não se Aplica", "Ignorado"))

AUX[, 3] <- as.factor(c(1:9))

colnames(AUX) <- c("Casos", "Escolaridade", "Ordem")

AUX[, 1] <- as.numeric(AUX[, 1])

RS_PECONHENTOS_GRAF_SERIE_HIST_Escolaridade <- ggplot(AUX) +
  geom_bar(aes(x =  Ordem, 
               y = Casos),
           stat = "identity",
           color = "black",
           fill = "#3CB371",
           linewidth = 0.8) +
  labs(title = "Notificações por Escolaridade (2016 - 2024)",
       y = "Casos",
       x = NULL,
       caption = Fonte) + 
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
  theme(legend.position = "bottom",  
        legend.title = element_text(face = "bold",
                                    size = 14), 
        axis.text.x = element_text(angle = 0),
        legend.text = element_text(size = 14), 
        plot.subtitle = element_text(hjust = 0),
        plot.caption = element_text(size = 12,
                                    hjust = 0),
        plot.title = element_text(hjust = 0, 
                                  face = "bold",
                                  size = 20)    
  )

#############################    Escolaridade Ano corrente  ####################################################

AUX <- RS_Serie_Historica_Geral[nrow(RS_Serie_Historica_Geral), c(1, 18:26)]

AUX <- t(AUX)

AUX <- as.data.frame(AUX[-1,])

AUX[, 2] <- as.factor(c("Analf.", "Fund. Incomp.", 
                        "Fundamental", "Médio Incomp.", 
                        "Médio", "Sup. Incomp.", "Superior",
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
           fill = "#98FB98",
           linewidth = 0.8) +
  labs(title = "Notificações por Escolaridade (2025)",
       y = "Casos",
       x = NULL,
       caption = Fonte) + 
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
  theme(legend.position = "bottom",  
        legend.title = element_text(face = "bold",
                                    size = 14), 
        axis.text.x = element_text(angle = 0),
        legend.text = element_text(size = 14), 
        plot.subtitle = element_text(hjust = 0),
        plot.caption = element_text(size = 12,
                                    hjust = 0),
        plot.title = element_text(hjust = 0, 
                                  face = "bold",
                                  size = 20)    
  )

################      Tipo de Acidente   ################################################################################

AUX <- RS22_PECONHENTOS_2025_TIPO_ACID[nrow(RS22_PECONHENTOS_2025_TIPO_ACID), -c(1, 2, 3, 4)]

AUX <- as.data.frame(t(AUX))

AUX[, 2] <- c("Serpente", "Aranha", "Escorpião", "Lagarta", "Abelha", "Outros", "Ignorado")

colnames(AUX) <- c("Casos", "Animal")

RS_PECONHENTOS_GRAF_TIPO_ACID <- ggplot(AUX, 
                                        aes(x = Animal)
) +
  geom_bar(aes(y = Casos),
           stat = "identity",
           color = "black",
           fill = "#66CDAA",
           linewidth = 0.8) +
  labs(title = "Acidentes com Animais Peçonhentos na 22ª RS (2025)",
       y = "Casos",
       x = NULL,
       caption = Fonte) + 
  geom_label(aes(label = Casos, 
                 x =  Animal, 
                 y = Casos),
             alpha = 0.5,
             vjust = 0.1)  +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)
  )
  ) +
  Theme() +
  theme(legend.position = "bottom",  
        legend.title = element_text(face = "bold",
                                    size = 14), 
        axis.text.x = element_text(angle = 0),
        legend.text = element_text(size = 14), 
        plot.subtitle = element_text(hjust = 0),
        plot.caption = element_text(size = 12,
                                    hjust = 0),
        plot.title = element_text(hjust = 0, 
                                  face = "bold",
                                  size = 20)    
  )

################      Tipo de Acidente   ################################################################################

AUX <- RS_Serie_Historica_Tipo_Acid[-c(1, nrow(RS_Serie_Historica_Tipo_Acid)), -c(2, 3, 4)]

AUX <- as.data.frame(apply(AUX[, -1], 2, sum))

AUX[, 2] <- c("Serpente", "Aranha", "Escorpião", "Lagarta", "Abelha", "Outros", "Ignorado")

colnames(AUX) <- c("Casos", "Animal")

RS_PECONHENTOS_GRAF_TIPO_ACID_HIST <- ggplot(AUX, 
                                        aes(x = Animal)
) +
  geom_bar(aes(y = Casos),
           stat = "identity",
           color = "black",
           fill = "#9ACD32",
           linewidth = 0.8) +
  labs(title = "Acidentes com Animais Peçonhentos na 22ª RS (2016 -2024)",
       y = "Casos",
       x = NULL,
       caption = Fonte) + 
  geom_label(aes(label = Casos, 
                 x =  Animal, 
                 y = Casos),
             alpha = 0.5,
             vjust = 0.1)  +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)
  )
  ) +
  Theme() +
  theme(legend.position = "bottom",  
        legend.title = element_text(face = "bold",
                                    size = 14), 
        axis.text.x = element_text(angle = 0),
        legend.text = element_text(size = 14), 
        plot.subtitle = element_text(hjust = 0),
        plot.caption = element_text(size = 12,
                                    hjust = 0),
        plot.title = element_text(hjust = 0, 
                                  face = "bold",
                                  size = 20)    
  )

# ################      Serpentes por município   ################################################################################
# 
# AUX <- RS22_PECONHENTOS_2025_TIPO_ACID[-nrow(RS22_PECONHENTOS_2025_TIPO_ACID),]
# 
# RS_PECONHENTOS_GRAF_TIPO_ACID_Municipios_Serpente <- ggplot(AUX, 
#                                                             aes(x = Município)
# ) +
#   geom_bar(aes(y = Serpente),
#            stat = "identity",
#            color = "black",
#            fill = "green",
#            linewidth = 0.8) +
#   labs(title = "Acidentes com Serpentes/Município - 2025",
#        subtitle = "Município de residência",
#        y = "Casos",
#        x = NULL,
#        caption = Fonte) + 
#   geom_label(aes(label = Serpente, 
#                  x =  Município, 
#                  y = Serpente),
#              alpha = 0.5,
#              vjust = 0.1)  +
#   scale_y_continuous(expand = expansion(mult = c(0, 0.1)
#   )
#   ) +
#   Theme() +
#   theme(plot.caption = element_text(hjust = 0),
#         axis.text.x = element_text(angle = 75)
#   ) 
# 
# ###########################    Aranha por município    ###########################################3
# 
# AUX <- RS22_PECONHENTOS_2025_TIPO_ACID[-nrow(RS22_PECONHENTOS_2025_TIPO_ACID),]
# 
# RS_PECONHENTOS_GRAF_TIPO_ACID_Municipios_Aranha <- ggplot(AUX, 
#                                                           aes(x = Município)
# ) +
#   geom_bar(aes(y = Aranha),
#            stat = "identity",
#            color = "black",
#            fill = "green",
#            linewidth = 0.8) +
#   labs(title = "Acidentes com Aranhas/Município - 2025",
#        subtitle = "Município de residência",
#        y = "Casos",
#        x = NULL,
#        caption = Fonte) + 
#   geom_label(aes(label = Aranha, 
#                  x =  Município, 
#                  y = Aranha),
#              alpha = 0.5,
#              vjust = 0.1)  +
#   scale_y_continuous(expand = expansion(mult = c(0, 0.1)
#   )
#   ) +
#   Theme() +
#   theme(plot.caption = element_text(hjust = 0),
#         axis.text.x = element_text(angle = 75)
#   ) 
# 
# ##############Abelhas por município
# 
# AUX <- RS22_PECONHENTOS_2025_TIPO_ACID[-nrow(RS22_PECONHENTOS_2025_TIPO_ACID),]
# 
# RS_PECONHENTOS_GRAF_TIPO_ACID_Municipios_Abelha <- ggplot(AUX, 
#                                                           aes(x = Município)
# ) +
#   geom_bar(aes(y = Abelha),
#            stat = "identity",
#            color = "black",
#            fill = "green",
#            linewidth = 0.8) +
#   labs(title = "Acidentes com Abelhas/Município - 2025",
#        subtitle = "Município de residência",
#        y = "Casos",
#        x = NULL,
#        caption = Fonte) + 
#   geom_label(aes(label = Abelha, 
#                  x =  Município, 
#                  y = Abelha),
#              alpha = 0.5,
#              vjust = 0.1)  +
#   scale_y_continuous(expand = expansion(mult = c(0, 0.1)
#   )
#   ) +
#   Theme() +
#   theme(plot.caption = element_text(hjust = 0),
#         axis.text.x = element_text(angle = 75)
#   ) 
# 
# #########################################################  Escorpião por município
# 
# AUX <- RS22_PECONHENTOS_2025_TIPO_ACID[-nrow(RS22_PECONHENTOS_2025_TIPO_ACID),]
# 
# RS_PECONHENTOS_GRAF_TIPO_ACID_Municipios_Escorpiao <- ggplot(AUX, 
#                                                              aes(x = Município)
# ) +
#   geom_bar(aes(y = Escorpiao),
#            stat = "identity",
#            color = "black",
#            fill = "green",
#            linewidth = 0.8) +
#   labs(title = "Acidentes com Escorpiões/Município - 2025",
#        subtitle = "Município de residência",
#        y = "Casos",
#        x = NULL,
#        caption = Fonte) + 
#   geom_label(aes(label = Escorpiao, 
#                  x =  Município, 
#                  y = Escorpiao),
#              alpha = 0.5,
#              vjust = 0.1)  +
#   scale_y_continuous(expand = expansion(mult = c(0, 0.1)
#   )
#   ) +
#   Theme() +
#   theme(plot.caption = element_text(hjust = 0),
#         axis.text.x = element_text(angle = 75)
#   ) 
# 
# ###############################################  Lagarta por município
# 
# AUX <- RS22_PECONHENTOS_2025_TIPO_ACID[-nrow(RS22_PECONHENTOS_2025_TIPO_ACID),]
# 
# RS_PECONHENTOS_GRAF_TIPO_ACID_Municipios_Lagarta <- ggplot(AUX, 
#                                                            aes(x = Município)
# ) +
#   geom_bar(aes(y = Lagarta),
#            stat = "identity",
#            color = "black",
#            fill = "green",
#            linewidth = 0.8) +
#   labs(title = "Acidentes com Lagartas/Município - 2025",
#        subtitle = "Município de residência",
#        y = "Casos",
#        x = NULL,
#        caption = Fonte) + 
#   geom_label(aes(label = Lagarta, 
#                  x =  Município, 
#                  y = Lagarta),
#              alpha = 0.5,
#              vjust = 0.1)  +
#   scale_y_continuous(expand = expansion(mult = c(0, 0.1)
#   )
#   ) +
#   Theme() +
#   theme(plot.caption = element_text(hjust = 0),
#         axis.text.x = element_text(angle = 75)
#   ) 
# 
# ############################################################  Outros por município
# 
# AUX <- RS22_PECONHENTOS_2025_TIPO_ACID[-nrow(RS22_PECONHENTOS_2025_TIPO_ACID),]
# 
# RS_PECONHENTOS_GRAF_TIPO_ACID_Municipios_Outros <- ggplot(AUX, 
#                                                           aes(x = Município)
# ) +
#   geom_bar(aes(y = Outros),
#            stat = "identity",
#            color = "black",
#            fill = "green",
#            linewidth = 0.8) +
#   labs(title = "Acidentes com Outros Animais/Município - 2025",
#        subtitle = "Município de residência",
#        y = "Casos",
#        x = NULL,
#        caption = Fonte) + 
#   geom_label(aes(label = Outros, 
#                  x =  Município, 
#                  y = Outros),
#              alpha = 0.5,
#              vjust = 0.1)  +
#   scale_y_continuous(expand = expansion(mult = c(0, 0.1)
#   )
#   ) +
#   Theme() +
#   theme(plot.caption = element_text(hjust = 0),
#         axis.text.x = element_text(angle = 75)
#   ) 
# 
##################################################  Local da picada  2025

AUX <- RS22_PECONHENTOS_2025_LOCAL_PICADA[nrow(RS22_PECONHENTOS_2025_LOCAL_PICADA), - c(1:4)]

AUX <- t(AUX)

AUX <- as.data.frame(AUX)

AUX[, 2] <- c("Cabeça", "Braço", "Antebraço", "Mão", "Dedo (Mão)", 
              "Tronco", "Coxa", "Perna", "Pé", "Dedo (Pé)", "Ignorado")

colnames(AUX) <- c("Casos", "Locais")

RS_PECONHENTOS_GRAF_LOCAL_PICADA_regional <- ggplot(AUX) +
  geom_bar(aes(x = Locais,
               y = Casos),
           stat = "identity",
           color = "black",
           fill = "#DEB887",
           linewidth = 0.8) +
  labs(title = "Local da Picada (2025)",
       y = "Casos",
       x = NULL,
       caption = Fonte) + 
  geom_label(aes(label = Casos,
                 x = Locais,
                 y = Casos),
             alpha = 0.5,
             vjust = 0.1)  +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)
  )
  ) +
  Theme() +
  theme(legend.position = "bottom",  
        legend.title = element_text(face = "bold",
                                    size = 14), 
        axis.text.x = element_text(angle = 0),
        legend.text = element_text(size = 14), 
        plot.subtitle = element_text(hjust = 0),
        plot.caption = element_text(size = 12,
                                    hjust = 0),
        plot.title = element_text(hjust = 0, 
                                  face = "bold",
                                  size = 20)    
  )

# ######################################################  Local da picada Serpente
# 
# AUX <- RS_Serie_Histórica_Local_da_Picada_Serpente[nrow(RS_Serie_Histórica_Local_da_Picada_Serpente), -c(1, 2, 3, 4)]
# 
# AUX <- as.data.frame(t(AUX))
# 
# AUX[, 2] <- c("Cabeça", "Braço", "Antebraço", "Mão", "Dedo (Mão)", 
#               "Tronco", "Coxa", "Perna", "Pé", "Dedo (Pé)", "Ignorado")
# 
# colnames(AUX) <- c("Casos", "Locais")
# 
# RS_PECONHENTOS_GRAF_LOCAL_PICADA_Serpente_regional <- ggplot(AUX) +
#   geom_bar(aes(x = Locais,
#                y = Casos),
#            stat = "identity",
#            color = "black",
#            fill = "green",
#            linewidth = 0.8) +
#   labs(title = "Local da Picada - Acidentes com Serpentes (2025)",
#        y = "Casos",
#        x = NULL,
#        caption = Fonte) + 
#   geom_label(aes(label = Casos,
#                  x = Locais,
#                  y = Casos),
#              alpha = 0.5,
#              vjust = 0.1)  +
#   scale_y_continuous(expand = expansion(mult = c(0, 0.1)
#   )
#   ) +
#   Theme() +
#   theme(plot.caption = element_text(hjust = 0),
#         axis.text.x = element_text(angle = 75)
#   ) 
# 
# ################  Local da picada Aranha
# 
# AUX <- RS_Serie_Histórica_Local_da_Picada_Aranha[nrow(RS_Serie_Histórica_Local_da_Picada_Aranha), -c(1, 2, 3, 4)]
# 
# AUX <- as.data.frame(t(AUX))
# 
# AUX[, 2] <- c("Cabeça", "Braço", "Antebraço", "Mão", "Dedo (Mão)", 
#               "Tronco", "Coxa", "Perna", "Pé", "Dedo (Pé)", "Ignorado")
# 
# colnames(AUX) <- c("Casos", "Locais")
# 
# RS_PECONHENTOS_GRAF_LOCAL_PICADA_Aranha_regional <- ggplot(AUX) +
#   geom_bar(aes(x = Locais,
#                y = Casos),
#            stat = "identity",
#            color = "black",
#            fill = "green",
#            linewidth = 0.8) +
#   labs(title = "Local da Picada - Acidentes com Aranhas (2025)",
#        y = "Casos",
#        x = NULL,
#        caption = Fonte) + 
#   geom_label(aes(label = Casos,
#                  x = Locais,
#                  y = Casos),
#              alpha = 0.5,
#              vjust = 0.1)  +
#   scale_y_continuous(expand = expansion(mult = c(0, 0.1)
#   )
#   ) +
#   Theme() +
#   theme(plot.caption = element_text(hjust = 0),
#         axis.text.x = element_text(angle = 75)
#   ) 
# 
# ################  Local da picada Escorpiões
# 
# AUX <- RS_Serie_Histórica_Local_da_Picada_Escorpiao[nrow(RS_Serie_Histórica_Local_da_Picada_Escorpiao), -c(1, 2, 3, 4)]
# 
# AUX <- as.data.frame(t(AUX))
# 
# AUX[, 2] <- c("Cabeça", "Braço", "Antebraço", "Mão", "Dedo (Mão)", 
#               "Tronco", "Coxa", "Perna", "Pé", "Dedo (Pé)", "Ignorado")
# 
# colnames(AUX) <- c("Casos", "Locais")
# 
# RS_PECONHENTOS_GRAF_LOCAL_PICADA_Escorpiao_regional <- ggplot(AUX) +
#   geom_bar(aes(x = Locais,
#                y = Casos),
#            stat = "identity",
#            color = "black",
#            fill = "green",
#            linewidth = 0.8) +
#   labs(title = "Local da Picada - Acidentes com Escorpiões (2025)",
#        y = "Casos",
#        x = NULL,
#        caption = Fonte) + 
#   geom_label(aes(label = Casos,
#                  x = Locais,
#                  y = Casos),
#              alpha = 0.5,
#              vjust = 0.1)  +
#   scale_y_continuous(expand = expansion(mult = c(0, 0.1)
#   )
#   ) +
#   Theme() +
#   theme(plot.caption = element_text(hjust = 0),
#         axis.text.x = element_text(angle = 75)
#   ) 
# 
# ################  Local da picada Lagartas
# 
# AUX <- RS_Serie_Histórica_Local_da_Picada_Lagarta[nrow(RS_Serie_Histórica_Local_da_Picada_Lagarta), -c(1, 2, 3, 4)]
# 
# AUX <- as.data.frame(t(AUX))
# 
# AUX[, 2] <- c("Cabeça", "Braço", "Antebraço", "Mão", "Dedo (Mão)", 
#               "Tronco", "Coxa", "Perna", "Pé", "Dedo (Pé)", "Ignorado")
# 
# colnames(AUX) <- c("Casos", "Locais")
# 
# RS_PECONHENTOS_GRAF_LOCAL_PICADA_Lagarta_regional <- ggplot(AUX) +
#   geom_bar(aes(x = Locais,
#                y = Casos),
#            stat = "identity",
#            color = "black",
#            fill = "green",
#            linewidth = 0.8) +
#   labs(title = "Local da Picada - Acidentes com Lagartas (2025)",
#        y = "Casos",
#        x = NULL,
#        caption = Fonte) + 
#   geom_label(aes(label = Casos,
#                  x = Locais,
#                  y = Casos),
#              alpha = 0.5,
#              vjust = 0.1)  +
#   scale_y_continuous(expand = expansion(mult = c(0, 0.1)
#   )
#   ) +
#   Theme() +
#   theme(plot.caption = element_text(hjust = 0),
#         axis.text.x = element_text(angle = 75)
#   ) 

################  Local da picada histórico

AUX <- RS_Serie_Histórica_Local_da_Picada[-c(1, nrow(RS_Serie_Histórica_Local_da_Picada)), -c(2, 3, 4)]

AUX <- apply(AUX[, 2:12], 2, sum)

AUX <- as.data.frame(AUX)

AUX[, 2] <- c("Cabeça", "Braço", "Antebraço", "Mão", "Dedo (Mão)", 
              "Tronco", "Coxa", "Perna", "Pé", "Dedo (Pé)", "Ignorado")

colnames(AUX) <- c("Casos", "Locais")

RS_PECONHENTOS_GRAF_LOCAL_PICADA_Historico_regional <- ggplot(AUX) +
  geom_bar(aes(x = Locais,
               y = Casos),
           stat = "identity",
           color = "black",
           fill = "#BC8F8F",
           linewidth = 0.8) +
  labs(title = "Local da Picada (2016 - 2024)",
       y = "Casos",
       x = NULL,
       caption = Fonte) + 
  geom_label(aes(label = Casos,
                 x = Locais,
                 y = Casos),
             alpha = 0.5,
             vjust = 0.1)  +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)
  )
  ) +
  Theme() +
  theme(legend.position = "bottom",  
        legend.title = element_text(face = "bold",
                                    size = 14), 
        axis.text.x = element_text(angle = 0),
        legend.text = element_text(size = 14), 
        plot.subtitle = element_text(hjust = 0),
        plot.caption = element_text(size = 12,
                                    hjust = 0),
        plot.title = element_text(hjust = 0, 
                                  face = "bold",
                                  size = 20)    
  )

############################   Raça (2016 - 2024)   ########

AUX <- RS_Serie_Historica_Geral[-c(1, nrow(RS_Serie_Historica_Geral)), c(31:36)]

AUX <- as.data.frame(apply(AUX, 2, sum))

AUX[, 2] <- c("Branca", "Preta", "Amarela", "Parda", "Indígena", "Ignorado")

colnames(AUX) <- c("Notificacoes", "Raca")

RS_PECONHENTOS_GRAF_RACA_HIST <- ggplot(AUX) +
  geom_bar(aes(x = Raca,
               y = Notificacoes),
           stat = "identity",
           color = "black",
           fill = "#2E8B57",
           linewidth = 0.8) +
  labs(title = "Notificações por Raça (2016 - 2024)",
       y = "Casos",
       x = NULL,
       caption = Fonte) + 
  geom_label(aes(label = Notificacoes,
                 x = Raca,
                 y = Notificacoes),
             alpha = 0.5,
             vjust = 0.1)  +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)
  )
  ) +
  Theme() +
  theme(legend.position = "bottom",  
        legend.title = element_text(face = "bold",
                                    size = 14), 
        axis.text.x = element_text(angle = 0),
        legend.text = element_text(size = 14), 
        plot.subtitle = element_text(hjust = 0),
        plot.caption = element_text(size = 12,
                                    hjust = 0),
        plot.title = element_text(hjust = 0, 
                                  face = "bold",
                                  size = 20)    
  )

############################   Raça (2025)   ########

AUX <- RS_Serie_Historica_Geral[ nrow(RS_Serie_Historica_Geral), c(31:36)]

AUX <- as.data.frame(t(AUX))

AUX[, 2] <- c("Branca", "Preta", "Amarela", "Parda", "Indígena", "Ignorado")

colnames(AUX) <- c("Notificacoes", "Raca")

RS_PECONHENTOS_GRAF_RACA <- ggplot(AUX) +
  geom_bar(aes(x = Raca,
               y = Notificacoes),
           stat = "identity",
           color = "black",
           fill = "#00FFFF",
           linewidth = 0.8) +
  labs(title = "Notificações por Raça (2025)",
       y = "Casos",
       x = NULL,
       caption = Fonte) + 
  geom_label(aes(label = Notificacoes,
                 x = Raca,
                 y = Notificacoes),
             alpha = 0.5,
             vjust = 0.1)  +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)
  )
  ) +
  Theme() +
  theme(legend.position = "bottom",  
        legend.title = element_text(face = "bold",
                                    size = 14), 
        axis.text.x = element_text(angle = 0),
        legend.text = element_text(size = 14), 
        plot.subtitle = element_text(hjust = 0),
        plot.caption = element_text(size = 12,
                                    hjust = 0),
        plot.title = element_text(hjust = 0, 
                                  face = "bold",
                                  size = 20)    
  )

# ################  Local da picada Serpente histórico
# 
# AUX <- RS_Serie_Histórica_Local_da_Picada_Serpente[-c(1, nrow(RS_Serie_Histórica_Local_da_Picada_Serpente)), -c(2, 3, 4)]
# 
# AUX <- apply(AUX[, 2:12], 2, sum)
# 
# AUX <- as.data.frame(AUX)
# 
# AUX[, 2] <- c("Cabeça", "Braço", "Antebraço", "Mão", "Dedo (Mão)", 
#               "Tronco", "Coxa", "Perna", "Pé", "Dedo (Pé)", "Ignorado")
# 
# colnames(AUX) <- c("Casos", "Locais")
# 
# RS_PECONHENTOS_GRAF_LOCAL_PICADA_Historico_Serpente_regional <- ggplot(AUX) +
#   geom_bar(aes(x = Locais,
#                y = Casos),
#            stat = "identity",
#            color = "black",
#            fill = "green",
#            linewidth = 0.8) +
#   labs(title = "Local da Picada - Acidentes com Serpentes (2016 - 2024)",
#        y = "Casos",
#        x = NULL,
#        caption = Fonte) + 
#   geom_label(aes(label = Casos,
#                  x = Locais,
#                  y = Casos),
#              alpha = 0.5,
#              vjust = 0.1)  +
#   scale_y_continuous(expand = expansion(mult = c(0, 0.1)
#   )
#   ) +
#   Theme() +
#   theme(plot.caption = element_text(hjust = 0),
#         axis.text.x = element_text(angle = 75)
#   ) 
# 
# ################  Local da picada Aranha histórico
# 
# AUX <- RS_Serie_Histórica_Local_da_Picada_Aranha[-c(1, nrow(RS_Serie_Histórica_Local_da_Picada_Aranha)), -c(2, 3, 4)]
# 
# AUX <- apply(AUX[, 2:12], 2, sum)
# 
# AUX <- as.data.frame(AUX)
# 
# AUX[, 2] <- c("Cabeça", "Braço", "Antebraço", "Mão", "Dedo (Mão)", 
#               "Tronco", "Coxa", "Perna", "Pé", "Dedo (Pé)", "Ignorado")
# 
# colnames(AUX) <- c("Casos", "Locais")
# 
# RS_PECONHENTOS_GRAF_LOCAL_PICADA_Historico_Aranha_regional <- ggplot(AUX) +
#   geom_bar(aes(x = Locais,
#                y = Casos),
#            stat = "identity",
#            color = "black",
#            fill = "green",
#            linewidth = 0.8) +
#   labs(title = "Local da Picada - Acidentes com Aranhas (2016 - 2024)",
#        y = "Casos",
#        x = NULL,
#        caption = Fonte) + 
#   geom_label(aes(label = Casos,
#                  x = Locais,
#                  y = Casos),
#              alpha = 0.5,
#              vjust = 0.1)  +
#   scale_y_continuous(expand = expansion(mult = c(0, 0.1)
#   )
#   ) +
#   Theme() +
#   theme(plot.caption = element_text(hjust = 0),
#         axis.text.x = element_text(angle = 75)
#   ) 
# 
# ################  Local da picada Escorpião histórico
# 
# AUX <- RS_Serie_Histórica_Local_da_Picada_Escorpiao[-c(1, nrow(RS_Serie_Histórica_Local_da_Picada_Escorpiao)), -c(2, 3, 4)]
# 
# AUX <- apply(AUX[, 2:12], 2, sum)
# 
# AUX <- as.data.frame(AUX)
# 
# AUX[, 2] <- c("Cabeça", "Braço", "Antebraço", "Mão", "Dedo (Mão)", 
#               "Tronco", "Coxa", "Perna", "Pé", "Dedo (Pé)", "Ignorado")
# 
# colnames(AUX) <- c("Casos", "Locais")
# 
# RS_PECONHENTOS_GRAF_LOCAL_PICADA_Historico_Escorpiao_regional <- ggplot(AUX) +
#   geom_bar(aes(x = Locais,
#                y = Casos),
#            stat = "identity",
#            color = "black",
#            fill = "green",
#            linewidth = 0.8) +
#   labs(title = "Local da Picada - Acidentes com Escorpiões (2016 - 2024)",
#        y = "Casos",
#        x = NULL,
#        caption = Fonte) + 
#   geom_label(aes(label = Casos,
#                  x = Locais,
#                  y = Casos),
#              alpha = 0.5,
#              vjust = 0.1)  +
#   scale_y_continuous(expand = expansion(mult = c(0, 0.1)
#   )
#   ) +
#   Theme() +
#   theme(plot.caption = element_text(hjust = 0),
#         axis.text.x = element_text(angle = 75)
#   ) 
# 
# ################  Local da picada lagartas histórico
# 
# AUX <- RS_Serie_Histórica_Local_da_Picada_Lagarta[-c(1, nrow(RS_Serie_Histórica_Local_da_Picada_Lagarta)), -c(2, 3, 4)]
# 
# AUX <- apply(AUX[, 2:12], 2, sum)
# 
# AUX <- as.data.frame(AUX)
# 
# AUX[, 2] <- c("Cabeça", "Braço", "Antebraço", "Mão", "Dedo (Mão)", 
#               "Tronco", "Coxa", "Perna", "Pé", "Dedo (Pé)", "Ignorado")
# 
# colnames(AUX) <- c("Casos", "Locais")
# 
# RS_PECONHENTOS_GRAF_LOCAL_PICADA_Historico_Lagarta_regional <- ggplot(AUX) +
#   geom_bar(aes(x = Locais,
#                y = Casos),
#            stat = "identity",
#            color = "black",
#            fill = "green",
#            linewidth = 0.8) +
#   labs(title = "Local da Picada - Acidentes com Lagartas (2016 - 2024)",
#        y = "Casos",
#        x = NULL,
#        caption = Fonte) + 
#   geom_label(aes(label = Casos,
#                  x = Locais,
#                  y = Casos),
#              alpha = 0.5,
#              vjust = 0.1)  +
#   scale_y_continuous(expand = expansion(mult = c(0, 0.1)
#   )
#   ) +
#   Theme() +
#   theme(plot.caption = element_text(hjust = 0),
#         axis.text.x = element_text(angle = 75)
#   ) 

###################   Tempo de Atendimento    ##########################

AUX <- RS_Serie_Histórica_Tempo_de_Atendimento[-c(1, nrow(RS_Serie_Histórica_Tempo_de_Atendimento)), -c(2:4)]

AUX <- as.data.frame(apply(AUX[,-1], 2, sum))

AUX[, 2] <- c("0⊢1h", "1⊢3h", "3⊢6h", "6⊢2h", "12⊢4h", "24 ou +h", "Ignorado")

colnames(AUX) <- c("N", "Tempo")

AUX$Tempo <- factor(AUX$Tempo, levels = AUX$Tempo)

RS_PECONHENTOS_GRAF_TEMPO_ATEND_HIST <- ggplot(AUX, aes(x = Tempo, 
                                                        y = N)) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#E6E6FA",
           linewidth = 0.8) +
  labs(title = "Tempo entre Acidente e Atendimento (2016 - 2024)",
       subtitle = "Total de Notificações - 22ª Regional de Saúde",
       y = "Notificações",
       x = NULL,
       caption = Fonte) + 
  geom_label(aes(label = N,
                 x = Tempo,
                 y = N),
             alpha = 0.5,
             vjust = 0.1)  +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)
  )
  ) +
  Theme() +
  theme(legend.position = "bottom",  
        legend.title = element_text(face = "bold",
                                    size = 14), 
        axis.text.x = element_text(angle = 0),
        legend.text = element_text(size = 14), 
        plot.subtitle = element_text(hjust = 0,
                                     size = 12),
        plot.caption = element_text(size = 12,
                                    hjust = 0),
        plot.title = element_text(hjust = 0, 
                                  face = "bold",
                                  size = 20)    
  )

###################   Tempo de Atendimento    ##########################

AUX <- RS_Serie_Histórica_Tempo_de_Atendimento[nrow(RS_Serie_Histórica_Tempo_de_Atendimento), -c(2:4)]

AUX <- as.data.frame(apply(AUX[,-1], 2, sum))

AUX[, 2] <- c("0⊢1h", "1⊢3h", "3⊢6h", "6⊢2h", "12⊢4h", "24 ou +h", "Ignorado")

colnames(AUX) <- c("N", "Tempo")

AUX$Tempo <- factor(AUX$Tempo, levels = AUX$Tempo)

RS_PECONHENTOS_GRAF_TEMPO_ATEND <- ggplot(AUX, aes(x = Tempo, y = N)) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#EEEBAA",
           linewidth = 0.8) +
  labs(title = "Tempo entre Acidente e Atendimento (2025)",
       subtitle = "Total de Notificações - 22ª Regional de Saúde",
       y = "Notificações",
       x = NULL,
       caption = Fonte) + 
  geom_label(aes(label = N,
                 x = Tempo,
                 y = N),
             alpha = 0.5,
             vjust = 0.1)  +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)
  )
  ) +
  Theme() +
  theme(legend.position = "bottom",  
        legend.title = element_text(face = "bold",
                                    size = 14), 
        axis.text.x = element_text(angle = 0),
        legend.text = element_text(size = 14), 
        plot.subtitle = element_text(hjust = 0,
                                     size = 12),
        plot.caption = element_text(size = 12,
                                    hjust = 0),
        plot.title = element_text(hjust = 0, 
                                  face = "bold",
                                  size = 20)    
  )

##################   Gráfico SIES  Sem Epi   ########################

#######  Soro antibotrópico    #####################

AUX <- RS22_PECONHENTOS_2025_SE_SIES_ANTIBOTROPICO[str_detect(RS22_PECONHENTOS_2025_SE_SIES_ANTIBOTROPICO$Ano, "2025"),]

AUX <- as.data.frame(t(AUX))

AUX[, 2] <- rownames(AUX)

colnames(AUX) <- c("Quantidade", "SE")

AUX <- AUX[-1,]

AUX[, 1] <- as.numeric(AUX[, 1])

levels(AUX$SE)

AUX$SE <- factor(AUX$SE, levels = AUX$SE)

RS_PECONHENTOS_SIES_ANTIBOTROPICO <- ggplot(AUX, 
                                            aes(x = SE)) +
  geom_bar(aes(y = Quantidade),
           stat = "identity",
           color = "black",
           fill = "#F0E68C",
           linewidth = 0.8) +
  labs(title = "Ampolas de Soro Antibotrópico Dispensadas por Semana Epidemiológica (2025)",
       y = "Ampolas Dispensadas",
       x = "Semana Epidemiológica",
       caption = Fonte3) + 
  geom_label(aes(label = Quantidade,
                 x = as.factor(SE),
                 y = Quantidade),
             alpha = 0.5,
             vjust = 0.1)  +
  scale_x_discrete(breaks = as.factor(AUX$SE), 
                   label = AUX$SE ) +
  scale_y_discrete(expand = expansion(mult = c(0, 0.1)
  )
  ) +
  Theme() +
  theme(legend.position = "bottom",  
        legend.title = element_text(face = "bold",
                                    size = 14), 
        axis.text.x = element_text(angle = 0),
        legend.text = element_text(size = 14), 
        plot.subtitle = element_text(hjust = 0),
        plot.caption = element_text(size = 12,
                                    hjust = 0),
        plot.title = element_text(hjust = 0, 
                                  face = "bold",
                                  size = 20)    
  )

#######  Soro crotálico    #####################

AUX <- RS22_PECONHENTOS_2025_SE_SIES_ANTICROTALICO[str_detect(RS22_PECONHENTOS_2025_SE_SIES_ANTICROTALICO$Ano, "2025"), ]

AUX <- as.data.frame(t(AUX))

AUX[, 2] <- rownames(AUX)

colnames(AUX) <- c("Quantidade", "SE")

AUX <- AUX[-1,]

AUX[, 1] <- as.numeric(AUX[, 1])

levels(AUX$SE)

AUX$SE <- factor(AUX$SE, levels = AUX$SE)

RS_PECONHENTOS_SIES_ANTICROTALICO <- ggplot(AUX, aes(x = SE)) +
  geom_bar(aes(y = Quantidade),
           stat = "identity",
           color = "black",
           fill = "#D2B48C",
           linewidth = 0.8) +
  labs(title = "Ampolas de Soro Anticrotálico Dispensadas por Semana Epidemiológica (2025)",
       y = "Ampolas Dispensadas",
       x = "Semana Epidemiológica",
       caption = Fonte3) + 
  geom_label(aes(label = Quantidade,
                 x = as.factor(SE),
                 y = Quantidade),
             alpha = 0.5,
             vjust = 0.1)  +
  scale_x_discrete(breaks = as.factor(AUX$SE), 
                   label = AUX$SE ) +
  scale_y_discrete(expand = expansion(mult = c(0, 0.1)
  )
  ) +
  Theme() +
  theme(legend.position = "bottom",  
        legend.title = element_text(face = "bold",
                                    size = 14), 
        axis.text.x = element_text(angle = 0),
        legend.text = element_text(size = 14), 
        plot.subtitle = element_text(hjust = 0),
        plot.caption = element_text(size = 12,
                                    hjust = 0),
        plot.title = element_text(hjust = 0, 
                                  face = "bold",
                                  size = 20)    
  ) 
  
#######################################################################################################################################
############################################### TABELAS   #############################################################################

##################################  Local da Picada (Ano Corrente)

AUX <- RS22_PECONHENTOS_2025_LOCAL_PICADA_ARANHA[nrow(RS22_PECONHENTOS_2025_LOCAL_PICADA_ARANHA), -c(2, 3, 4)]

AUX[2, ] <- RS22_PECONHENTOS_2025_LOCAL_PICADA_ESCORPIAO[nrow(RS22_PECONHENTOS_2025_LOCAL_PICADA_ESCORPIAO), -c(2, 3, 4)]

AUX[3, ] <- RS22_PECONHENTOS_2025_LOCAL_PICADA_SERPENTE[nrow(RS22_PECONHENTOS_2025_LOCAL_PICADA_SERPENTE), -c(2, 3, 4)]

AUX[4, ] <- RS22_PECONHENTOS_2025_LOCAL_PICADA_LAGARTA[nrow(RS22_PECONHENTOS_2025_LOCAL_PICADA_LAGARTA), -c(2, 3, 4)]

AUX <- as.data.frame(t(AUX[,-1]))

colnames(AUX) <- c("Aranha", "Escorpião", "Serpente", "Lagarta")

AUX[, 5] <- c("Cabeça", "Braço", "Antebraço", "Mão", "Dedo (Mão)", 
              "Tronco", "Coxa", "Perna", "Pé", "Dedo (Pé)", "Ignorado")

AUX <- AUX[, c(5, 1:4)]

RS_PECONHENTOS_TAB_LOCAL_PICADA <- AUX %>% 
  gt() %>%
  tab_header(title = md("**Local da Picada (2025)**")) %>%
  tab_options(heading.align = "left",
              column_labels.border.top.color = "black",
              column_labels.border.top.width = px(3)) %>%
  tab_spanner(label = "Localização",
              columns = c(2:5)) %>%
  cols_align(align = "center", columns = c(2:5)
  ) %>%
  cols_label(V5 = "") %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels(everything()
            )
  ) %>%
  tab_footnote(footnote = Fonte) %>%
  tab_options(table.font.size = "small")

##################################  Local da Picada (2016 - 2024)

AUX <- RS_Serie_Histórica_Local_da_Picada_Aranha[- c(1, nrow(RS_Serie_Histórica_Local_da_Picada_Aranha)), -c(2, 3, 4)]

AUX <- apply(AUX[, -1], 2, sum)

AUX <- as.data.frame(AUX)

AUX2 <- RS_Serie_Histórica_Local_da_Picada_Escorpiao[- c(1, nrow(RS_Serie_Histórica_Local_da_Picada_Escorpiao)), -c(2, 3, 4)]

AUX[, 2] <- apply(AUX2[, -1], 2, sum)

AUX2 <- RS_Serie_Histórica_Local_da_Picada_Serpente[- c(1, nrow(RS_Serie_Histórica_Local_da_Picada_Serpente)), -c(2, 3, 4)]

AUX[, 3] <- apply(AUX2[, -1], 2, sum)

AUX2 <- RS_Serie_Histórica_Local_da_Picada_Lagarta[- c(1, nrow(RS_Serie_Histórica_Local_da_Picada_Lagarta)), -c(2, 3, 4)]

AUX[, 4] <- apply(AUX2[, -1], 2, sum)

colnames(AUX) <- c("Aranha", "Escorpião", "Serpente", "Lagarta")

AUX[, 5] <- c("Cabeça", "Braço", "Antebraço", "Mão", "Dedo (Mão)", 
              "Tronco", "Coxa", "Perna", "Pé", "Dedo (Pé)", "Ignorado")

AUX <- AUX[, c(5, 1:4)]

RS_PECONHENTOS_TAB_LOCAL_PICADA_HIST <- AUX %>% 
  gt() %>%
  tab_header(title = md("**Local da Picada (2016 - 2024)**")) %>%
  tab_options(heading.align = "left",
              column_labels.border.top.color = "black",
              column_labels.border.top.width = px(3)) %>%
  tab_spanner(label = "Localização",
              columns = c(2:5)) %>%
  cols_align(align = "center", columns = c(2:5)
  ) %>%
  cols_label(V5 = "") %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels(everything()
            )
  ) %>%
  tab_footnote(footnote = Fonte) %>%
  tab_options(table.font.size = "small")

########################   Tipo de Acidente Histórico   ########################

AUX <- RS_Serie_Historica_Tipo_Acid[-c(1, nrow(RS_Serie_Historica_Tipo_Acid)), - c(2, 3, 4)]

RS_PECONHENTOS_TAB_TIPO_ACID_HIST <- AUX %>%
  gt() %>%
  tab_header(title = md("**Tipo de Acidente Notificado (2016 - 2024)**"),
             subtitle = "(Campo 45 SINAN)") %>%
  tab_options(heading.align = "left",
              column_labels.border.top.color = "black",
              column_labels.border.top.width = px(3)) %>%
  tab_spanner(label = "Tipo de Acidente",
              columns = c(2:8)) %>%
  cols_align(align = "center", columns = c(2:8)
  ) %>%
  cols_label(RS = "Ano",
             Escorpiao = "Escorpião",
             ignorado = "Ignorado") %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels(everything()
            )
  ) %>%
  tab_footnote(footnote = Fonte) %>%
  tab_options(table.font.size = "small")

############################### Tipo de acidente SERPENTE (2016 - 2025)    ############################################################

RS_PECONHENTOS_TAB_TIPO_ACID_HIST_Serpente <- RS_Serie_Historica_Tipo_Acid_Serpente[-1, -c(2, 3, 4)] %>%
  gt() %>%
  tab_header(title = md("**Tipo de Acidente Ofídico Notificados (2016 - 2025)**"),
             subtitle = "(Campo 46 SINAN)") %>%
  tab_options(heading.align = "left",
              column_labels.border.top.color = "black",
              column_labels.border.top.width = px(3)) %>%
  tab_spanner(label = "Tipo de Acidente",
              columns = c(2:7)) %>%
  cols_align(align = "center", columns = c(2:7)
  ) %>%
  cols_label(RS = "Ano",
             Botropico = "Botrópico",
             Elapidico = "Elapídico",
             Laquetico = "Laquético",
             Nao_Peconhenta = "Não Peçonhenta") %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels(everything()
            )
  ) %>%
  tab_footnote(footnote = Fonte) %>%
  tab_options(table.font.size = "small")

################################ Tipo de acidente ARANHA (2016 - 2025)   ################################################

RS_PECONHENTOS_TAB_TIPO_ACID_HIST_Aranha <- RS_Serie_Historica_Tipo_Acid_Aranha[-1, -c(2, 3, 4)] %>%
  gt() %>%
  tab_header(title = md("**Tipo de Acidente com Aranhas Notificados (2016 - 2025)**"),
             subtitle = "(campo 47 SINAN)") %>%
  tab_options(heading.align = "left",
              column_labels.border.top.color = "black",
              column_labels.border.top.width = px(3)
  ) %>%
  tab_spanner(label = "Tipo de Acidente",
              columns = c(2:6)) %>%
  cols_align(align = "center", columns = c(2:6)
  ) %>%
  cols_label(RS = "Ano",
             Outra_Aranha = "Outras") %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels(everything()
            )
  ) %>%
  tab_footnote(footnote = Fonte) %>%
  tab_options(table.font.size = "small")

###############################   Tipo de acidente LAGARTA (2016 - 2025)   ##########################################

RS_PECONHENTOS_TAB_TIPO_ACID_HIST_Lagarta <- RS_Serie_Historica_Tipo_Acid_Lagarta[-1, -c(2, 3, 4)] %>%
  gt() %>%
  tab_header(title = md("**Tipo de Acidente com Lagartas Notificados (2016 - 2025)**"),
             subtitle = "(campo 48 SINAN)") %>%
  tab_options(heading.align = "left",
              column_labels.border.top.color = "black",
              column_labels.border.top.width = px(3)
  ) %>%
  tab_spanner(label = "Tipo de Acidente",
              columns = c(2:4)
  ) %>%
  cols_align(align = "center", 
             columns = c(2:4)
  ) %>%
  cols_label(RS = "Ano",
             Outra_Lagarta = "Outras") %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels(everything()
            )
  ) %>%
  tab_footnote(footnote = Fonte) %>%
  tab_options(table.font.size = "small")

########################################  Caracterização Lugar   ############################3##############################
########################################### Zona de ocorrência   ###########################################################

AUX <- RS_Serie_Historica_Geral[- c(1, nrow(RS_Serie_Historica_Geral)), c(1, 6:9)]

colnames(AUX)[1] <- "Ano"

RS_PECONHENTOS_TAB_ZONA_OCORRENCIA_HIST <- AUX %>%
  gt() %>%
  tab_header(title = md("**Notificações por Zona de Ocorrência (2016 - 2024)**")
  ) %>%
  tab_options(heading.align = "left",
              column_labels.border.top.color = "black",
              column_labels.border.top.width = px(3)
  ) %>%
  tab_spanner(label = "Zona de Ocorrência",
              columns = c(2:5)) %>%
  cols_align(align = "center", 
             columns = c(2:5)
  ) %>%
  cols_label(Zona_Urbana = "Urbana",
             Zona_Rural = "Rural",
             Zona_Periurbana = "Periurbana",
             Zona_Ignorados = "Ignorado") %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels(everything()
            )
  ) %>%
  tab_footnote(footnote = Fonte) %>%
  tab_options(table.font.size = "small")

######################################### Tabela Municípios Zona de Ocorrência  #######################################

RS_PECONHENTOS_TAB_ZONA_OCORRENCIA_Municipios <- RS22_PECONHENTOS_2025_GERAL[-nrow(RS22_PECONHENTOS_2025_GERAL), c(2, 6:9)] %>%
  gt() %>%
  tab_header(title = md("**Notificações por Zona de Ocorrência (2025)**")) %>%
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

##############################    Caracterização Pessoa   ###############################################################
####################################### Sexo   ##########################################################################

AUX <- RS_Serie_Historica_Geral[-c(1, nrow(RS_Serie_Historica_Geral)), c(1, 10:11)]

RS_PECONHENTOS_TAB_SEXO_HIST <- AUX %>%
  gt() %>%
  tab_header(title = md("**Notificações por Sexo**"),
             subtitle = "Notificações entre 2016 - 2024") %>%
  tab_options(heading.align = "left",
              column_labels.border.top.color = "black",
              column_labels.border.top.width = px(3)) %>%
  tab_spanner(label = "Sexo",
              columns = c(2:3)) %>%
  cols_align(align = "center", columns = c(2:3)) %>%
  cols_label("RS" = "Ano") %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels(everything())) %>%
  tab_footnote(footnote = Fonte) %>%
  tab_options(table.font.size = "small")

###################################   SINAP    ################################################################################

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

##################################   SINAP GERAL   #############################################################################

RS_PECONHENTOS_TAB_SINAP_GERAL <- SINAP_GERAL %>%
  gt() %>%
  tab_header(title = md("**Total de Amostras Encaminhadas por Município**"),
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

#######################################################

AUX <- SINAP[, c(1, 5, 7, 8, 12, 16, 17, 18, 26, 27) ]

AUX <- AUX %>%
  filter(str_detect(AUX$Coleta, "2025")) 

AUX <- AUX %>%
  mutate(Acidente = case_when(Causador_acidente == "S" ~ "Sim",
                              Causador_acidente == "N" ~ "Não"))

AUX <- AUX[, c(1, 3, 4, 11, 7, 8, 9, 10)]

RS_PECONHENTOS_TAB_SINAP_Laudos_1 <- AUX[c(1:25),] %>%
  gt() %>%
  tab_header( title = md("**Amostras de Animais Peçonhentos Encaminhadas ao DVVZI/LABTAX (2025)**")) %>%
  tab_options(heading.align = "left",
              column_labels.border.top.color = "black",
              column_labels.border.top.width = px(3)) %>%
  cols_align(align = "center") %>%
  cols_label(Municipio = "Município",
             Genero = "Gênero",
             Especie = "Espécie") %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels(everything())) %>%
  tab_footnote(footnote = "Continua") %>%
  tab_footnote(footnote = Fonte2) %>%
  tab_options(table.font.size = "small")

RS_PECONHENTOS_TAB_SINAP_Laudos_2 <- AUX[c(26:nrow(AUX)),] %>%
  gt() %>%
  tab_header( title = md("**Amostras de Animais Peçonhentos Encaminhadas ao DVVZI/LABTAX (2025)**"),
              subtitle = "Continuação") %>%
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

#########################################  Tipo de acidente (2025)   #####################

AUX <- RS22_PECONHENTOS_2025_TIPO_ACID[-nrow(RS22_PECONHENTOS_2025_TIPO_ACID_SERPENTE),-c(1, 3, 4)]

RS_PECONHENTOS_TAB_TIPO_ACID_Municipios <- AUX %>%
  gt() %>%
  tab_header(title = md("**Tipo de Acidente por Município (2025)**")
  ) %>%
  tab_options(heading.align = "left",
              column_labels.border.top.color = "black",
              column_labels.border.top.width = px(3)) %>%
  tab_spanner(label = "Tipo de Acidente",
              columns = c(2:8),
              id = "Animal") %>%
  cols_align(align = "center", columns = c(2:8)) %>%
  cols_label(Município = "Município",
             Escorpiao = "Escorpião",
             ignorado = "Ignorado") %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels(everything()
            )) %>%
  tab_footnote(footnote = Fonte2) %>%
  tab_options(table.font.size = "small")

#########################################  Tipo de acidente com serpente  Municípios   #####################

AUX <- RS22_PECONHENTOS_2025_TIPO_ACID_SERPENTE[-nrow(RS22_PECONHENTOS_2025_TIPO_ACID_SERPENTE),-c(1, 3, 4)]

RS_PECONHENTOS_TAB_TIPO_ACID_Municipios_Serpente <- AUX %>%
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

#################################################  Tipo de Aranha / Município

AUX <- RS22_PECONHENTOS_2025_TIPO_ACID_ARANHA[-nrow(RS22_PECONHENTOS_2025_TIPO_ACID_ARANHA),-c(1, 3, 4)]

RS_PECONHENTOS_TAB_TIPO_ACID_Municipios_Aranha <- AUX %>%
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

########################################################  Tipo de lagarta / Município

AUX <- RS22_PECONHENTOS_2025_TIPO_ACID_LAGARTA[-nrow(RS22_PECONHENTOS_2025_TIPO_ACID_LAGARTA),-c(1, 3, 4)]

RS_PECONHENTOS_TAB_TIPO_ACID_Municipios_lagarta <- AUX %>%
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

####################   SINAN ENCERRAMENTO

AUX <- RS22_PECONHENTOS_2025_SINAN_PROB_ENCERRAMENTO[- nrow(RS22_PECONHENTOS_2025_SINAN_PROB_ENCERRAMENTO), -c(1, 3, 4)]

RS_PECONHENTOS_TAB_TIPO_SINAN_ENCERRAMENTO <- AUX %>%
  gt() %>%
  tab_header(title = md("**SINAN: Notificações Abertas ou com Encerramento Inadequado (2025)**")
  ) %>%
  tab_options(heading.align = "left",
              column_labels.border.top.color = "black",
              column_labels.border.top.width = px(3)) %>%
  tab_spanner(label = "Campos Inadequados",
              columns = c(2:4),
              id = "Animal") %>%
  cols_align(align = "center", columns = c(2:4)) %>%
  cols_label(Município = "Município",
             Sem_Evo = "Evolução 
             do Caso",
             Trabalho = "Acidente Relacionado 
             ao Trabalho") %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels(everything()
            )) %>%
  tab_footnote(footnote = Fonte2) %>%
  tab_options(table.font.size = "small")

####################   SINAN Inconsistencia (Serpentes)

AUX <- RS22_PECONHENTOS_2025_SINAN_SORO_INCONSISTENCIA[- nrow(RS22_PECONHENTOS_2025_SINAN_SORO_INCONSISTENCIA), c(2, 5:10)]

RS_PECONHENTOS_TAB_TIPO_SINAN_INCONSISTENCIAS_Serpentes <- AUX %>%
  gt() %>%
  tab_header(title = md("**Fichas com Utilização de Soro Inconsistente com Classificação do Caso (2025)**"),
             subtitle = "Acidentes com serpentes"
  ) %>%
  tab_options(heading.align = "left",
              column_labels.border.top.color = "black",
              column_labels.border.top.width = px(3)) %>%
  tab_spanner(label = "Acidente Botrópico",
              columns = c(2:4),
              id = "Animal1") %>%
  tab_spanner(label = "Acidente Crotálico",
              columns = c(5:7),
              id = "Animal2") %>%
  cols_align(align = "center", columns = c(2:7)) %>%
  cols_label(Município = "Município",
             Botropico_Leve = "Leve",
             Botropico_Moderado = "Moderado",
             Botropico_Grave = "Grave",
             Crotalico_Leve = "Leve",
             Crotalico_Moderado = "Moderado",
             Crotalico_Grave = "Grave") %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels(everything()
            )) %>%
  tab_footnote(footnote = Fonte2) %>%
  tab_options(table.font.size = "small")

####################   SINAN Inconsistencia (Serpentes)

AUX <- RS22_PECONHENTOS_2025_SINAN_SORO_INCONSISTENCIA[- nrow(RS22_PECONHENTOS_2025_SINAN_SORO_INCONSISTENCIA), c(2, 11:18)]

RS_PECONHENTOS_TAB_TIPO_SINAN_INCONSISTENCIAS_Outros <- AUX %>%
  gt() %>%
  tab_header(title = md("**Fichas com Utilização de Soro Inconsistente com Classificação do Caso (2025)**"),
             subtitle = "Acidentes com escorpiões, aranhas e lagartas."
  ) %>%
  tab_options(heading.align = "left",
              column_labels.border.top.color = "black",
              column_labels.border.top.width = px(3)) %>%
  tab_spanner(label = "Acidente Escorpiônico",
              columns = c(2:3),
              id = "Animal1") %>%
  tab_spanner(label = "Acidente Loxoscélico",
              columns = c(4:5),
              id = "Animal2") %>%
  tab_spanner(label = "Foneutrismo",
              columns = c(6:7),
              id = "Animal3") %>%
  tab_spanner(label = "Lonômia",
              columns = c(8:9),
              id = "Animal4") %>%
  cols_align(align = "center", columns = c(2:9)) %>%
  cols_label(Município = "Município",
             Escorpiônico_Moderado = "Moderado",
             Escorpiônico_Grave = "Grave",
             Loxoscelico_Moderado = "Moderado",
             Loxoscelico_Grave = "Grave",
             Foneutrismo_Moderado = "Moderado",
             Foneutrismo_Grave = "Grave",
             Lonomia_Moderado = "Moderado",
             Lonomia_Grave = "Grave") %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels(everything()
            )) %>%
  tab_footnote(footnote = Fonte2) %>%
  tab_options(table.font.size = "small")

#####################################   Óbitos   ##################################

AUX <- RS_Serie_Historica_Obitos[-c(1, nrow(RS_Serie_Historica_Obitos)), -c(2, 3, 4)]

RS_PECONHENTOS_TAB_OBITOS_HIST <- AUX %>%
  gt() %>%
  tab_header(title = md("**Óbitos por Acidentes com Animais Peçonhentos (2016 - 2024)**"),
             subtitle = "Casos notificados (município de ocorrência do acidente)"
  ) %>%
  tab_options(heading.align = "left",
              column_labels.border.top.color = "black",
              column_labels.border.top.width = px(3)) %>%
  tab_spanner(label = "Número de Óbitos",
              columns = c(2:7),
              id = "Animal1") %>%
  cols_align(align = "center", columns = c(2:7)) %>%
  cols_label(RS = "Município",
             Escorpiao = "Escorpião") %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels(everything()
            )) %>%
  tab_footnote(footnote = Fonte) %>%
  tab_options(table.font.size = "small")

######################   Óbitos   Ano corrente   ###################

AUX <- RS22_PECONHENTOS_2025_OBITOS[-nrow(RS22_PECONHENTOS_2025_OBITOS), -c(1, 3, 4)]

RS_PECONHENTOS_TAB_OBITOS <- AUX %>%
  gt() %>%
  tab_header(title = md("**Óbitos por Acidentes com Animais Peçonhentos por Município (2025)**"),
             subtitle = "Casos notificados (município de ocorrência do acidente)"
  ) %>%
  tab_options(heading.align = "left",
              column_labels.border.top.color = "black",
              column_labels.border.top.width = px(3)) %>%
  tab_spanner(label = "Número de Óbitos",
              columns = c(2:7),
              id = "Animal1") %>%
  cols_align(align = "center", columns = c(2:7)) %>%
  cols_label(Município = "Município",
             Escorpiao = "Escorpião") %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels(everything()
            )) %>%
  tab_footnote(footnote = Fonte) %>%
  tab_options(table.font.size = "small")

###################   Tempo de Atendimento ano corrente   ##########################

AUX <- RS22_PECONHENTOS_2025_TEMPO_ATEND[-nrow(RS22_PECONHENTOS_2025_TEMPO_ATEND), -c(1, 3, 4)]

colnames(AUX)[-1] <- c("0⊢1h", "1⊢3h", "3⊢6h", "6⊢2h", "12⊢4h", "24 ou +h", "Ignorado")

RS_PECONHENTOS_TAB_TEMPO_ATEND <- AUX %>%
  gt() %>%
  tab_header(title = md("**Tempo entre Acidente e Atendimento por Município (2025)**"),
             subtitle = "Casos notificados (município de ocorrência do acidente)"
  ) %>%
  tab_options(heading.align = "left",
              column_labels.border.top.color = "black",
              column_labels.border.top.width = px(3)) %>%
  tab_spanner(label = "Tempo de Atendimento",
              columns = c(2:7),
              id = "Animal1") %>%
  cols_align(align = "center", columns = c(2:7)) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels(everything()
            )) %>%
  tab_footnote(footnote = Fonte) %>%
  tab_options(table.font.size = "small")

###################   Tempo de Atendimento Histórico   ##########################

AUX <- RS_Serie_Histórica_Tempo_de_Atendimento[-c(1, nrow(RS_Serie_Histórica_Tempo_de_Atendimento)), -c(2, 3, 4)]

colnames(AUX)[-1] <- c("0⊢1h", "1⊢3h", "3⊢6h", "6⊢2h", "12⊢4h", "24 ou +h", "Ignorado")

RS_PECONHENTOS_TAB_TEMPO_ATEND_HIST <- AUX %>%
  gt() %>%
  tab_header(title = md("**Tempo entre Acidente e Atendimento por Município (2016 - 2024)**")
  ) %>%
  tab_options(heading.align = "left",
              column_labels.border.top.color = "black",
              column_labels.border.top.width = px(3)) %>%
  tab_spanner(label = "Tempo de Atendimento",
              columns = c(2:8),
              id = "Animal1") %>%
  cols_align(align = "center", columns = c(2:8)) %>%
  cols_label(RS = "Ano") %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels(everything()
            )) %>%
  tab_footnote(footnote = Fonte) %>%
  tab_options(table.font.size = "small")

#####################################################################################################################################
##############################################    Mapas   ###########################################################################

###################################  Municipios que aplicam soro antiescorpiônico  ##################################################

SHAPEFILE_SINAP <- SHAPEFILE_REGIONAL %>% mutate(NUCLEO = case_when(NM_MUNICIP == "IVAIPORÃ" 
                                                                    | NM_MUNICIP == "NOVA TEBAS" 
                                                                    | NM_MUNICIP == "LUNARDELLI"
                                                                    | NM_MUNICIP == "SÃO JOÃO DO IVAÍ"
                                                                    | NM_MUNICIP == "MANOEL RIBAS"
                                                                    | NM_MUNICIP == "MATO RICO"
                                                                    ~ "Rede Descentralizada")
)

RS_PECONHENTOS_MAPA_REDE_SORO_ESC <- ggplot(SHAPEFILE_SINAP) +
  geom_sf(size = 2,
          aes(fill = NUCLEO),
          color = "black") +
  scale_fill_manual(name = "", 
                    values = c("Rede Descentralizada" = "#2F4F4F"),
                    na.value = "#ADD8E6") + 
  coord_sf(expand = FALSE) +
  geom_sf_label(data = SHAPEFILE_SINAP %>% filter(NUCLEO == "Rede Descentralizada"), 
                aes(label = NM_MUNICIP),
                label.padding = unit(0.5, "mm"),
                size = 4,
                position = "identity") +
  labs(title = "Rede Descentralizada para Atendimento de Acidentes Escorpiônicos
22ª Regional de Saúde",
       subtitle = "Municípios que Possuem Soro Antiescorpiônico em sua Rede de Frio",
       y = NULL,
       x = NULL,
       caption = Fonte1) +
  annotation_scale(location = "br") +
  annotation_north_arrow(which_north = "true",
                         location = "tr")+
  theme(
    legend.position = "bottom",  
    legend.title = element_text(face = "bold",
                                size = 14),  
    legend.text = element_text(size = 14), 
    plot.subtitle = element_text(hjust = 0),
    plot.caption = element_text(size = 12,
                                hjust = 0),
    plot.title = element_text(hjust = 0, 
                              face = "bold", 
                              size = 20)  
  )

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

RS_PECONHENTOS_MAPA_REDE_Ivaipora <- ggplot(SHAPEFILE_IVAIPORA) +
  geom_sf(aes(fill = NUCLEO), 
          color = "black", 
          size = 2) +
  scale_fill_manual(name = "", 
                    values = c("Rede Descentralizada" = "#2F4F4F"),
                    na.value = "#ADD8E6") + 
  coord_sf(expand = FALSE) +
  geom_sf_label(data = SHAPEFILE_IVAIPORA, 
                aes(label = NM_MUNICIP),
                size = 5,
                alpha = 0.5,
                position = "identity") +
  labs(title = "Rede Descentralizada para Atendimento de Acidentes Escorpiônicos
22ª Regional de Saúde",
       subtitle = "Municípios atendidos pelo Núcleo de Administração de Soro Antiescorpiônico de Ivaiporã",
       y = NULL,
       x = NULL,
       caption = Fonte1) +
  annotation_scale(location = "br") +
  annotation_north_arrow(which_north = "true",
                         location = "tr") +
  theme(
    legend.position = "bottom",  
    legend.title = element_text(face = "bold",
                                size = 14),  
    legend.text = element_text(size = 14), 
    plot.subtitle = element_text(hjust = 0),
    plot.caption = element_text(size = 12,
                                hjust = 0),
    plot.title = element_text(hjust = 0, 
                              face = "bold", 
                              size = 20)  
  ) 

########################  São João do Ivaí   ##########################

SHAPEFILE_SJI <- SHAPEFILE_SINAP %>% filter(NM_MUNICIP == "SÃO JOÃO DO IVAÍ" |
                                              NM_MUNICIP == "GODOY MOREIRA")

RS_PECONHENTOS_MAPA_REDE_SJI <- ggplot(SHAPEFILE_SJI) +
  geom_sf(aes(fill = NUCLEO), 
          color = "black", 
          size = 2) +
  scale_fill_manual(name = "", 
                    values = c("Rede Descentralizada" = "#2F4F4F"),
                    na.value = "#ADD8E6") + 
  coord_sf(expand = FALSE) +
  geom_sf_label(data = SHAPEFILE_SJI, 
                aes(label = NM_MUNICIP),
                size = 5,
                alpha = 0.5,
                position = "identity") +
  labs(title = "Rede Descentralizada para Atendimento de Acidentes Escorpiônicos
22ª Regional de Saúde",
       subtitle = "Municípios atendidos pelo Núcleo de Administração de Soro Antiescorpiônico de São João do Ivaí",
       y = NULL,
       x = NULL,
       caption = Fonte1) +
  annotation_scale(location = "br") +
  annotation_north_arrow(which_north = "true",
                         location = "tr") +
  theme(
    legend.position = "bottom",  
    legend.title = element_text(face = "bold",
                                size = 14),  
    legend.text = element_text(size = 14), 
    plot.subtitle = element_text(hjust = 0),
    plot.caption = element_text(size = 12,
                                hjust = 0),
    plot.title = element_text(hjust = 0, 
                              face = "bold", 
                              size = 20)   
  ) 

##############################   MANOEL RIBAS   ##############################

SHAPEFILE_MRI <- SHAPEFILE_SINAP %>% filter(NM_MUNICIP == "MANOEL RIBAS" |
                                              NM_MUNICIP == "CÂNDIDO DE ABREU" |
                                              NM_MUNICIP == "SANTA MARIA DO OESTE")

RS_PECONHENTOS_MAPA_REDE_MRI <- ggplot(SHAPEFILE_MRI) +
  geom_sf(aes(fill = NUCLEO), 
          color = "black", 
          size = 0.5) +
  scale_fill_manual(name = "", 
                    values = c("Rede Descentralizada" = "#2F4F4F"),
                    na.value = "#ADD8E6") + 
  coord_sf(expand = FALSE) +
  geom_sf_label(data = SHAPEFILE_MRI, 
                aes(label = NM_MUNICIP),
                size = 5,
                alpha = 0.5,
                position = "identity") +
  labs(title = "Rede Descentralizada para Atendimento de Acidentes Escorpiônicos
22ª Regional de Saúde",
       subtitle = "Municípios atendidos pelo Núcleo de Administração de Soro Antiescorpiônico de Manoel Ribas",
       y = NULL,
       x = NULL,
       caption = Fonte1) +
  annotation_scale(location = "br") +
  annotation_north_arrow(which_north = "true",
                         location = "tr")+
  theme(
    legend.position = "bottom",  
    legend.title = element_text(face = "bold",
                                size = 14),  
    legend.text = element_text(size = 14), 
    plot.subtitle = element_text(hjust = 0),
    plot.caption = element_text(size = 12,
                                hjust = 0),
    plot.title = element_text(hjust = 0, 
                              face = "bold", 
                              size = 20)    
  ) 

##########################  NOVA TEBAS   ###############################

##############################   NOVA TEBAS   ##############################

SHAPEFILE_NOVA_TEBAS <- SHAPEFILE_SINAP %>% filter(NM_MUNICIP == "NOVA TEBAS")

RS_PECONHENTOS_MAPA_REDE_Nova_Tebas <- ggplot(SHAPEFILE_NOVA_TEBAS) +
  geom_sf(aes(fill = NUCLEO), 
          color = "black", 
          size = 0.5) +
  scale_fill_manual(name = "", 
                    values = c("Rede Descentralizada" = "#2F4F4F"),
                    na.value = "#ADD8E6") + 
  coord_sf(expand = FALSE) +
  geom_sf_label(data = SHAPEFILE_NOVA_TEBAS, 
                aes(label = NM_MUNICIP),
                size = 5,
                alpha = 0.5,
                position = "identity") +
  labs(title = "Rede Descentralizada para Atendimento de Acidentes Escorpiônicos
22ª Regional de Saúde",
       subtitle = "Núcleo de Administração de Soro Antiescorpiônico de Nova Tebas",
       y = NULL,
       x = NULL,
       caption = Fonte1) +
  annotation_scale(location = "br") +
  annotation_north_arrow(which_north = "true",
                         location = "tr") +
  theme(
    legend.position = "bottom",  
    legend.title = element_text(face = "bold",
                                size = 14),  
    legend.text = element_text(size = 14), 
    plot.subtitle = element_text(hjust = 0),
    plot.caption = element_text(size = 12,
                                hjust = 0),
    plot.title = element_text(hjust = 0, 
                              face = "bold", 
                              size = 20)   
  ) 

##############################   Mato rico   ##############################

SHAPEFILE_MATO_RICO <- SHAPEFILE_SINAP %>% filter(NM_MUNICIP == "MATO RICO")

RS_PECONHENTOS_MAPA_REDE_Mato_Rico <- ggplot(SHAPEFILE_MATO_RICO) +
  geom_sf(aes(fill = NUCLEO), 
          color = "black", 
          size = 0.5) +
  scale_fill_manual(name = "", 
                    values = c("Rede Descentralizada" = "#2F4F4F"),
                    na.value = "#ADD8E6") + 
  coord_sf(expand = FALSE) +
  geom_sf_label(data = SHAPEFILE_MATO_RICO, 
                aes(label = NM_MUNICIP),
                size = 5,
                alpha = 0.5,
                position = "identity") +
  labs(title = "Rede Descentralizada para Atendimento de Acidentes Escorpiônicos
22ª Regional de Saúde",
       subtitle = "Núcleo de Administração de Soro Antiescorpiônico de Mato Rico",
       y = NULL,
       x = NULL,
       caption = Fonte1) +
  annotation_scale(location = "br") +
  annotation_north_arrow(which_north = "true",
                         location = "tr") +
  theme(
    legend.position = "bottom",  
    legend.title = element_text(face = "bold",
                                size = 14),  
    legend.text = element_text(size = 14), 
    plot.subtitle = element_text(hjust = 0),
    plot.caption = element_text(size = 12,
                                hjust = 0),
    plot.title = element_text(hjust = 0, 
                              face = "bold", 
                              size = 20)   
  ) 
###########################  LUNARDELLI   ################################

##########################################################

SHAPEFILE_LUNARDELLI <- SHAPEFILE_SINAP %>% filter(NM_MUNICIP == "LUNARDELLI")

RS_PECONHENTOS_MAPA_REDE_Lunardelli <- ggplot(SHAPEFILE_LUNARDELLI) +
  geom_sf(aes(fill = NUCLEO), 
          color = "black", 
          size = 0.5) +
  scale_fill_manual(name = "", 
                    values = c("Rede Descentralizada" = "#2F4F4F"),
                    na.value = "#ADD8E6") + 
  coord_sf(expand = FALSE) +
  geom_sf_label(data = SHAPEFILE_LUNARDELLI, 
                aes(label = NM_MUNICIP),
                size = 5,
                alpha = 0.5,
                position = "identity") +
  labs(title = "Rede Descentralizada para Atendimento de Acidentes Escorpiônicos
22ª Regional de Saúde",
       subtitle = "Núcleo de Administração de Soro Antiescorpiônico de Lunardelli",
       y = NULL,
       x = NULL,
       caption = Fonte1) +
  annotation_scale(location = "br") +
  annotation_north_arrow(which_north = "true",
                         location = "tr") +
  theme(
    legend.position = "bottom",  
    legend.title = element_text(face = "bold",
                                size = 14),  
    legend.text = element_text(size = 14), 
    plot.subtitle = element_text(hjust = 0),
    plot.caption = element_text(size = 12,
                                hjust = 0),
    plot.title = element_text(hjust = 0, 
                              face = "bold", 
                              size = 20)   
  ) 

#######  Municípios que possuem encontro de escorpião amarelo

SHAPEFILE_Escorpiao_Amarelo <- SHAPEFILE_REGIONAL %>% mutate(ESC_AMAR = case_when(NM_MUNICIP == "IVAIPORÃ" 
                                                                                  | NM_MUNICIP == "JARDIM ALEGRE" 
                                                                                  | NM_MUNICIP == "ARIRANHA DO IVAÍ" 
                                                                                  | NM_MUNICIP == "LUNARDELLI"
                                                                                  | NM_MUNICIP == "SÃO JOÃO DO IVAÍ"
                                                                                  | NM_MUNICIP == "MANOEL RIBAS" 
                                                                                  ~ "Registro")
)

RS_PECONHENTOS_MAPA_ESCORPIOES <- ggplot(SHAPEFILE_Escorpiao_Amarelo) +
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
                size = 4,
                position = "identity") +
  labs(title = "Municípios da 22ª Regional de Saúde com Registro de Escorpiões Amarelos",
       y = NULL,
       x = NULL,
       caption = Fonte1) +
  annotation_scale(location = "br") +
  annotation_north_arrow(which_north = "true",
                         location = "tr")+
  theme(
    legend.position = "bottom",  
    legend.title = element_text(face = "bold",
                                size = 14),  
    legend.text = element_text(size = 14), 
    plot.subtitle = element_text(hjust = 0),
    plot.caption = element_text(size = 12,
                                hjust = 0),
    plot.title = element_text(hjust = 0, 
                              face = "bold", 
                              size = 20)    
  ) 

#################################################    Amostras SINAP   2020 - 2025   #######################################

SINAP_GERAL[, 2] <- as.numeric(SINAP_GERAL[, 2])

AUX <- SINAP_GERAL %>% mutate(Cortes = cut(x = Amostras,
                                           breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, +Inf),
                                           labels = c("1 - 10", "11 - 20", "21 - 30", "31 - 40", "41 - 50", "51 - 60", "61 - 70", "71 - 80", "> 80")))

SHAPEFILE_SINAP <- left_join(SHAPEFILE_SINAP, AUX, by = c("NM_MUNICIP" = "Municipio"))

SHAPEFILE_SINAP$Cortes <- as.character(SHAPEFILE_SINAP$Cortes)

SHAPEFILE_SINAP$Cortes <-replace(SHAPEFILE_SINAP$Cortes, is.na(SHAPEFILE_SINAP$Cortes), "Sem Encaminhamentos")

RS_PECONHENTOS_MAPA_SINAP_Amostras <- ggplot(SHAPEFILE_SINAP) +
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
  annotation_scale(location = "br") +
  annotation_north_arrow(which_north = "true",
                         location = "tr")+
  theme(
    legend.position = "bottom",  
    legend.title = element_text(face = "bold",
                                size = 14),  
    legend.text = element_text(size = 14), 
    plot.subtitle = element_text(hjust = 0),
    plot.caption = element_text(size = 12,
                                hjust = 0),
    plot.title = element_text(hjust = 0, 
                              face = "bold", 
                              size = 20)   
  ) 

########################  Total de Tipo de Acidentes por municípios (2020 - 2024)   ##########################

######   Serpente

AUX <- as.data.frame(tapply(RS_Serie_Historica_Tipo_Acid_Municipios$Serpente, RS_Serie_Historica_Tipo_Acid_Municipios$Município, sum))

AUX[, 2] <- rownames(AUX)

colnames(AUX) <- c("Serpente", "Municipio")

AUX <- left_join(SHAPEFILE_REGIONAL, AUX, by = c("NM_MUNICIP" = "Municipio"))

AUX <- AUX %>% mutate(Cortes = cut(x = Serpente,
                                           breaks = c(-Inf, 0, 10, 20, 30, 40, 50, 60, 80, +Inf),
                                           labels = c("0",  "01 - 10", "11 - 20", "21 - 30", "31 - 40", "41 - 50", 
                                                      "51 - 60", "61 - 80", "> 80")))


RS_PECONHENTOS_MAP_ACID_SERPENTES_HIST <- ggplot(AUX) +
  geom_sf(aes(fill = Cortes),
          size = 0.5,
          color = "black") +
  scale_fill_manual(name = "Nº de Acidentes", 
                    values = c("0" = "#FEF9F7",
                                                         "01 - 10" = "#FCECE7",
                                                         "11 - 20" = "#F9D9CF",
                                                         "21 - 30" = "#F1AC97",
                                                         "31 - 40" = "#EE987F",
                                                         "41 - 50" = "#C78C7A",
                                                         "51 - 60" = "#B3887C",
                                                         "61 - 80" = "#A6867D",
                                                         "> 80"     = "#54342A")
                    ) +
  labs(title = "Acidentes com Serpentes Notificados (2016 - 2024)",
       subtitle = "Número Total por Município",
       caption = Fonte) +
  annotation_scale(location = "br") +
  annotation_north_arrow(which_north = "true",
                         location = "tr") +
  theme(
    legend.position = "bottom",  
    legend.title = element_text(face = "bold",
                                size = 14),  
    legend.text = element_text(size = 14), 
    plot.subtitle = element_text(hjust = 0),
    plot.caption = element_text(size = 12,
                                hjust = 0),
    plot.title = element_text(hjust = 0, 
                              face = "bold", 
                              size = 20)    
  ) 

######   Aranha

AUX <- as.data.frame(tapply(RS_Serie_Historica_Tipo_Acid_Municipios$Aranha, RS_Serie_Historica_Tipo_Acid_Municipios$Município, sum))

AUX[, 2] <- rownames(AUX)

colnames(AUX) <- c("Aranha", "Municipio")

AUX <- left_join(SHAPEFILE_REGIONAL, AUX, by = c("NM_MUNICIP" = "Municipio"))

AUX <- AUX %>% mutate(Cortes = cut(x = Aranha,
                                   breaks = c(-Inf, 0, 25, 50, 70, 100, 130, 180, 230, 300 +Inf),
                                   labels = c("0",  "01 - 25", "26 - 50", "51 - 70", "71 - 100", 
                                              "101 - 130", "131 - 180", "181 - 230", "> 230")))


RS_PECONHENTOS_MAP_ACID_ARANHAS_HIST <- ggplot(AUX) +
  geom_sf(aes(fill = Cortes),
          size = 0.5,
          color = "black") +
  scale_fill_manual(name = "Nº de Acidentes", 
                    values = c("0" = "#FEF9F7",
                                                                     "01 - 25" = "#FCECE7",
                                                                     "26 - 50" = "#F9D9CF",
                                                                     "51 - 70" = "#F1AC97",
                                                                     "71 - 100" = "#EE987F",
                                                                     "101 - 130" = "#C78C7A",
                                                                     "131 - 180" = "#B3887C",
                                                                     "181 - 230" = "#A6867D",
                                                                     "> 230"     = "#54342A")
  ) +
  labs(title = "Acidentes com Aranhas Notificados (2016 - 2024)",
       subtitle = " Número Total por Município",
       caption = Fonte) +
  annotation_scale(location = "br") +
  annotation_north_arrow(which_north = "true",
                         location = "tr") +
  theme(
    legend.position = "bottom",  
    legend.title = element_text(face = "bold",
                                size = 14),  
    legend.text = element_text(size = 14), 
    plot.subtitle = element_text(hjust = 0),
    plot.caption = element_text(size = 12,
                                hjust = 0),
    plot.title = element_text(hjust = 0, 
                              face = "bold", 
                              size = 20)    
  ) 

######   Escorpião

AUX <- as.data.frame(tapply(RS_Serie_Historica_Tipo_Acid_Municipios$Escorpiao, RS_Serie_Historica_Tipo_Acid_Municipios$Município, sum))

AUX[, 2] <- rownames(AUX)

colnames(AUX) <- c("Escorpiao", "Municipio")

AUX <- left_join(SHAPEFILE_REGIONAL, AUX, by = c("NM_MUNICIP" = "Municipio"))

AUX <- AUX %>% mutate(Cortes = cut(x = Escorpiao,
                                   breaks = c(-Inf, 0, 5, 10, 15, 20, 30, 40, 60, 80 +Inf),
                                   labels = c("0",  "01 - 05", "06 - 10", "11 - 15", "16 - 20", 
                                              "21 - 30", "31 - 40", "41 - 60", "> 60")))


RS_PECONHENTOS_MAP_ACID_ESCORPIAO_HIST <- ggplot(AUX) +
  geom_sf(aes(fill = Cortes),
          size = 0.5,
          color = "black") +
  scale_fill_manual(name = "Nº de Acidentes", 
                    values = c("0" = "#FEF9F7",
                                                                     "01 - 05" = "#FCECE7",
                                                                     "06 - 10" = "#F9D9CF",
                                                                     "11 - 15" = "#F1AC97",
                                                                     "16 - 20" = "#EE987F",
                                                                     "21 - 30" = "#C78C7A",
                                                                     "31 - 40" = "#B3887C",
                                                                     "41 - 60" = "#54342A",
                                                                     "> 60"     = "#38221C")
  ) +
  labs(title = "Acidentes com Escorpiões (2016 - 2024)",
       subtitle = " Número Total por Município",
       caption = Fonte) +
  annotation_scale(location = "br") +
  annotation_north_arrow(which_north = "true",
                         location = "tr") +
  theme(
    legend.position = "bottom",  
    legend.title = element_text(face = "bold",
                                size = 14),  
    legend.text = element_text(size = 14), 
    plot.subtitle = element_text(hjust = 0),
    plot.caption = element_text(size = 12,
                                hjust = 0),
    plot.title = element_text(hjust = 0, 
                              face = "bold", 
                              size = 20)    
  ) 

######   aBELHAS

AUX <- as.data.frame(tapply(RS_Serie_Historica_Tipo_Acid_Municipios$Abelha, RS_Serie_Historica_Tipo_Acid_Municipios$Município, sum))

AUX[, 2] <- rownames(AUX)

colnames(AUX) <- c("Abelha", "Municipio")

AUX <- left_join(SHAPEFILE_REGIONAL, AUX, by = c("NM_MUNICIP" = "Municipio"))

AUX <- AUX %>% mutate(Cortes = cut(x = Abelha,
                                   breaks = c(-Inf, 0, 5, 10, 20, 30, 40, 50, 55, 60 +Inf),
                                   labels = c("0",  "01 - 05", "06 - 10", "11 - 20", "21 - 30", 
                                              "31 - 60", "61 - 90", "91 - 130", "> 130")))


RS_PECONHENTOS_MAP_ACID_ABELHA_HIST <- ggplot(AUX) +
  geom_sf(aes(fill = Cortes),
          size = 0.5,
          color = "black") +
  scale_fill_manual(name = "Nº de Acidentes", 
                    values = c("0" = "#FEF9F7",
                               "01 - 05" = "#FCECE7",
                               "06 - 10" = "#F9D9CF",
                               "11 - 20" = "#F1AC97",
                               "21 - 30" = "#EE987F",
                               "31 - 60" = "#C78C7A",
                               "61 - 90" = "#B3887C",
                               "91 - 130" = "#54342A",
                               "> 130"     = "#38221C")
  ) +
  labs(title = "Acidentes com Abelhas (2016 - 2024)",
       subtitle = " Frequência absoluta/município",
       caption = Fonte) +
  annotation_scale(location = "br") +
  annotation_north_arrow(which_north = "true",
                         location = "tr") +
  theme(
    legend.position = "bottom",  
    legend.title = element_text(face = "bold",
                                size = 14),  
    legend.text = element_text(size = 14), 
    plot.subtitle = element_text(hjust = 0),
    plot.caption = element_text(size = 12,
                                hjust = 0),
    plot.title = element_text(hjust = 0, 
                              face = "bold", 
                              size = 20)  
  ) 

######   Lagarta

AUX <- as.data.frame(tapply(RS_Serie_Historica_Tipo_Acid_Municipios$Lagarta, 
                            RS_Serie_Historica_Tipo_Acid_Municipios$Município, 
                            sum))

AUX[, 2] <- rownames(AUX)

colnames(AUX) <- c("Lagarta", "Municipio")

AUX <- left_join(SHAPEFILE_REGIONAL, AUX, by = c("NM_MUNICIP" = "Municipio"))

AUX <- AUX %>% mutate(Cortes = cut(x = Lagarta,
                                   breaks = c(-Inf, 0, 3, 6, 10, 14, 18, 21, 25, 30 +Inf),
                                   labels = c("0",  "01 - 03", "04 - 06", "07 - 10", "11 - 14", 
                                              "15 - 18", "19 - 21", "22 - 25", "> 25")))


RS_PECONHENTOS_MAP_ACID_LAGARTA_HIST <- ggplot(AUX) +
  geom_sf(aes(fill = Cortes),
          size = 0.5,
          color = "black") +
  scale_fill_manual(name = "Nº de Acidentes", 
                    values = c("0" = "#FEF9F7",
                               "01 - 03" = "#FCECE7",
                               "04 - 06" = "#F9D9CF",
                               "07 - 10" = "#F1AC97",
                               "11 - 14" = "#EE987F",
                               "15 - 18" = "#C78C7A",
                               "19 - 21" = "#B3887C",
                               "22 - 25" = "#54342A",
                               "> 25"     = "#38221C")
  ) +
  labs(title = "Acidentes com Lagartas (2016 - 2024)",
       subtitle = " Número Total por Município",
       caption = Fonte) +
  annotation_scale(location = "br") +
  annotation_north_arrow(which_north = "true",
                         location = "tr") +
  theme(
    legend.position = "bottom",  
    legend.title = element_text(face = "bold",
                                size = 14),  
    legend.text = element_text(size = 14), 
    plot.subtitle = element_text(hjust = 0),
    plot.caption = element_text(size = 12,
                                hjust = 0),
    plot.title = element_text(hjust = 0, 
                              face = "bold", 
                              size = 20)    
  ) 

######################################################################################################################################
###############################################  Salvando Gráficos, Tabelas e Mapas  #################################################

########################  Gráficos   ###############################################

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_GRAF_CE_Geral_Pag_08_A.png", 
       plot = RS_PECONHENTOS_GRAF_CE_Geral,     
       width = 50,             
       height = 20,           
       units = "cm",           
       dpi = 300) 

# ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_GRAF_CE_Serpente_Pag_09_C.png", 
#        plot = RS_PECONHENTOS_GRAF_CE_Serpentes,     
#        width = 50,             
#        height = 20,           
#        units = "cm",           
#        dpi = 300) 
# 

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_GRAF_CE_Aranha_Pag_09_A.png", 
       plot = RS_PECONHENTOS_GRAF_CE_Aranhas,     
       width = 50,             
       height = 20,           
       units = "cm",           
       dpi = 300) 


# ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_GRAF_CE_Escorpiao_Pag_00.png", 
#        plot = RS_PECONHENTOS_GRAF_CE_Escorpiao,     
#        width = 50,             
#        height = 20,           
#        units = "cm",           
#        dpi = 300) 
# 
# ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_GRAF_CE_Abelha_Pag_00.png", 
#        plot = RS_PECONHENTOS_GRAF_CE_Abelha ,     
#        width = 50,             
#        height = 20,           
#        units = "cm",           
#        dpi = 300) 
# 
# ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_GRAF_CE_Lagarta_Pag_00.png", 
#        plot = RS_PECONHENTOS_GRAF_CE_Lagarta,     
#        width = 50,             
#        height = 20,           
#        units = "cm",           
#        dpi = 300) 
# 
# ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_GRAF_CE_Outros_Pag_.png", 
#        plot = RS_PECONHENTOS_GRAF_CE_Outros,     
#        width = 50,             
#        height = 20,           
#        units = "cm",           
#        dpi = 300) 

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_GRAF_SERIE_HIST_Geral_Pag_08_B.png", 
       plot = RS_PECONHENTOS_GRAF_SERIE_HIST_Geral,     
       width = 50,             
       height = 15,           
       units = "cm",           
       dpi = 300) 

# ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_GRAF_SERIE_HIST_Serpente_Pag.png", 
#        plot = RS_PECONHENTOS_GRAF_SERIE_HIST_Serpente,     
#        width = 50,             
#        height = 15,           
#        units = "cm",           
#        dpi = 300) 

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_GRAF_SERIE_HIST_Aranhas_Pag_09_B.png", 
       plot = RS_PECONHENTOS_GRAF_SERIE_HIST_Aranha,     
       width = 50,             
       height = 15,           
       units = "cm",           
       dpi = 300) 

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_GRAF_SERIE_HIST_DEMAIS_09_C.png", 
       plot = RS_PECONHENTOS_GRAF_SERIE_HIST_DEMAIS,     
       width = 50,             
       height = 15,           
       units = "cm",           
       dpi = 300)

# ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_GRAF_SERIE_HIST_Escorpiao_Pag.png", 
#        plot = RS_PECONHENTOS_GRAF_SERIE_HIST_Escorpiao,     
#        width = 50,             
#        height = 15,           
#        units = "cm",           
#        dpi = 300) 
# 
# ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_GRAF_SERIE_HIST_Abelha_Pag.png", 
#        plot = RS_PECONHENTOS_GRAF_SERIE_HIST_Abelha,     
#        width = 50,             
#        height = 15,           
#        units = "cm",           
#        dpi = 300) 
# 
# ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_GRAF_SERIE_HIST_Lagarta_Pag.png", 
#        plot = RS_PECONHENTOS_GRAF_SERIE_HIST_Lagarta,     
#        width = 50,             
#        height = 15,           
#        units = "cm",           
#        dpi = 300) 
# 
# ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_GRAF_SERIE_HIST_Outros_Pag.png", 
#        plot = RS_PECONHENTOS_GRAF_SERIE_HIST_Outros,     
#        width = 50,             
#        height = 15,           
#        units = "cm",           
#        dpi = 300) 

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_GRAF_Incidencia_1_Pag_10_A.png", 
       plot = RS_PECONHENTOS_GRAF_Incidencia_01,     
       width = 50,             
       height = 20,           
       units = "cm",           
       dpi = 300)

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_GRAF_Incidencia_02_Pag_10_B.png", 
       plot = RS_PECONHENTOS_GRAF_Incidencia_02,     
       width = 50,             
       height = 20,           
       units = "cm",           
       dpi = 300)

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_GRAF_RACA_HIST_11_E.png", 
       plot = RS_PECONHENTOS_GRAF_RACA_HIST,     
       width = 32,             
       height = 20,           
       units = "cm",           
       dpi = 300)

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_GRAF_RACA_11_F.png", 
       plot = RS_PECONHENTOS_GRAF_RACA,     
       width = 32,             
       height = 20,           
       units = "cm",           
       dpi = 300)

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_GRAF_ZONA_REGIONAL_Historico_Pag_12_A.png", 
       plot = RS_PECONHENTOS_GRAF_ZONA_REGIONAL_Historico,     
       width = 32,             
       height = 20,           
       units = "cm",           
       dpi = 300) 

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_GRAF_ZONA_REGIONAL_Pag_12_C.png", 
       plot = RS_PECONHENTOS_GRAF_ZONA_REGIONAL,     
       width = 32,             
       height = 20,           
       units = "cm",           
       dpi = 300) 

# ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/RS_PECONHENTOS_GRAF_SEXO_HIST_Pag.png", 
#        plot = RS_PECONHENTOS_GRAF_SEXO_HIST,     
#        width = 30,             
#        height = 20,           
#        units = "cm",           
#        dpi = 300)

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_GRAF_SEXO_Pag_11_B.png", 
       plot = RS_PECONHENTOS_GRAF_SEXO,     
       width = 30,             
       height = 20,           
       units = "cm",           
       dpi = 300)

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_GRAF_PIRAMIDE_Pag_11_A.png", 
       plot = RS_PECONHENTOS_GRAF_PIRAMIDE,     
       width = 26,             
       height = 20,           
       units = "cm",           
       dpi = 300) 

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_GRAF_PIRAMIDE_HIST_Pag_10_D.png", 
       plot = RS_PECONHENTOS_GRAF_PIRAMIDE_HIST,     
       width = 26,             
       height = 20,           
       units = "cm",           
       dpi = 300)

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_GRAF_SERIE_HIST_Escolaridade_Pag_11C.png", 
       plot = RS_PECONHENTOS_GRAF_SERIE_HIST_Escolaridade,     
       width = 40,             
       height = 20,           
       units = "cm",           
       dpi = 300) 

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_GRAF_Escolaridade_Pag_11_D.png", 
       plot = RS_PECONHENTOS_GRAF_Escolaridade,     
       width = 40,             
       height = 20,           
       units = "cm",           
       dpi = 300) 

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_GRAF_TIPO_ACID_14_E.png", 
       plot = RS_PECONHENTOS_GRAF_TIPO_ACID,     
       width = 28,             
       height = 20,           
       units = "cm",           
       dpi = 300) 

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_GRAF_TIPO_ACID_HIST_12_E.png", 
       plot = RS_PECONHENTOS_GRAF_TIPO_ACID_HIST,     
       width = 32,             
       height = 16,           
       units = "cm",           
       dpi = 300)

# ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_GRAF_TIPO_ACID_Municipios_Serpente.png", 
#        plot = RS_PECONHENTOS_GRAF_TIPO_ACID_Municipios_Serpente,     
#        width = 24,             
#        height = 20,           
#        units = "cm",           
#        dpi = 300) 

# ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_GRAF_TIPO_ACID_Municipios_Aranha.png", 
#        plot = RS_PECONHENTOS_GRAF_TIPO_ACID_Municipios_Aranha,     
#        width = 24,             
#        height = 20,           
#        units = "cm",           
#        dpi = 300)
# 
# ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_GRAF_TIPO_ACID_Municipios_Abelha.png", 
#        plot = RS_PECONHENTOS_GRAF_TIPO_ACID_Municipios_Abelha,     
#        width = 24,             
#        height = 20,           
#        units = "cm",           
#        dpi = 300)
# 
# ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_GRAF_TIPO_ACID_Municipios_Escorpiao.png", 
#        plot = RS_PECONHENTOS_GRAF_TIPO_ACID_Municipios_Escorpiao,     
#        width = 24,             
#        height = 20,           
#        units = "cm",           
#        dpi = 300)
# 
# ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_GRAF_TIPO_ACID_Municipios_Lagarta.png", 
#        plot = RS_PECONHENTOS_GRAF_TIPO_ACID_Municipios_Lagarta,     
#        width = 24,             
#        height = 20,           
#        units = "cm",           
#        dpi = 300)
# 
# ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_GRAF_TIPO_ACID_Municipios_Outros.png", 
#        plot = RS_PECONHENTOS_GRAF_TIPO_ACID_Municipios_Outros,     
#        width = 24,             
#        height = 20,           
#        units = "cm",           
#        dpi = 300)

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_GRAF_LOCAL_PICADA_regional_Pag_16_A.png", 
       plot = RS_PECONHENTOS_GRAF_LOCAL_PICADA_regional,     
       width = 33,             
       height = 20,           
       units = "cm",           
       dpi = 300)

# ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_GRAF_LOCAL_PICADA_Serpente_regional.png", 
#        plot = RS_PECONHENTOS_GRAF_LOCAL_PICADA_Serpente_regional,     
#        width = 29,             
#        height = 20,           
#        units = "cm",           
#        dpi = 300)

# ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_GRAF_LOCAL_PICADA_Aranha_regional.png", 
#        plot = RS_PECONHENTOS_GRAF_LOCAL_PICADA_Aranha_regional,     
#        width = 29,             
#        height = 20,           
#        units = "cm",           
#        dpi = 300)
# 
# ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_GRAF_LOCAL_PICADA_Escorpiao_regional.png", 
#        plot = RS_PECONHENTOS_GRAF_LOCAL_PICADA_Escorpiao_regional,     
#        width = 29,             
#        height = 20,           
#        units = "cm",           
#        dpi = 300)
# 
# ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_GRAF_LOCAL_PICADA_Lagarta_regional.png", 
#        plot = RS_PECONHENTOS_GRAF_LOCAL_PICADA_Lagarta_regional,     
#        width = 29,             
#        height = 20,           
#        units = "cm",           
#        dpi = 300)

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_GRAF_LOCAL_PICADA_Historico_regional_Pag_15_E.png", 
       plot = RS_PECONHENTOS_GRAF_LOCAL_PICADA_Historico_regional,     
       width = 33,             
       height = 20,           
       units = "cm",           
       dpi = 300)

# ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_GRAF_LOCAL_PICADA_Historico_Serpente_regional.png", 
#        plot = RS_PECONHENTOS_GRAF_LOCAL_PICADA_Historico_Serpente_regional,     
#        width = 29,             
#        height = 20,           
#        units = "cm",           
#        dpi = 300)
# 
# ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_GRAF_LOCAL_PICADA_Historico_Aranha_regional.png", 
#        plot = RS_PECONHENTOS_GRAF_LOCAL_PICADA_Historico_Aranha_regional,     
#        width = 29,             
#        height = 20,           
#        units = "cm",           
#        dpi = 300)
# 
# ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_GRAF_LOCAL_PICADA_Historico_Escorpiao_regional.png", 
#        plot = RS_PECONHENTOS_GRAF_LOCAL_PICADA_Historico_Escorpiao_regional,     
#        width = 29,             
#        height = 20,           
#        units = "cm",           
#        dpi = 300)
# 
# ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_GRAF_LOCAL_PICADA_Historico_Lagarta_regional.png", 
#        plot = RS_PECONHENTOS_GRAF_LOCAL_PICADA_Historico_Lagarta_regional,     
#        width = 32,             
#        height = 16,           
#        units = "cm",           
#        dpi = 300)

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_SIES_ANTIBOTROPICO_Pag_17_A.png", 
       plot = RS_PECONHENTOS_SIES_ANTIBOTROPICO,     
       width = 50,             
       height = 15,           
       units = "cm",           
       dpi = 300)

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_SIES_ANTICROTALICO_Pag_17B.png", 
       plot = RS_PECONHENTOS_SIES_ANTICROTALICO,     
       width = 50,             
       height = 15,           
       units = "cm",           
       dpi = 300)

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_GRAF_TEMPO_ATEND_HIST_Pag_16_C.png", 
       plot = RS_PECONHENTOS_GRAF_TEMPO_ATEND_HIST,     
       width = 32,             
       height = 16,           
       units = "cm",           
       dpi = 300)

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_GRAF_TEMPO_ATEND_Pag_16_E.png", 
       plot = RS_PECONHENTOS_GRAF_TEMPO_ATEND,     
       width = 32,             
       height = 16,           
       units = "cm",           
       dpi = 300)

#######################################    Tabelas  ##################################################################

gtsave(data = RS_PECONHENTOS_TAB_LOCAL_PICADA,
       filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_TAB_LOCAL_PICADA_Pag_16_B.png")

gtsave(data = RS_PECONHENTOS_TAB_LOCAL_PICADA_HIST,
       filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_TAB_LOCAL_PICADA_HIST_Pag_15_F.png")

gtsave(data = RS_PECONHENTOS_TAB_TIPO_ACID_HIST,
       filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_TAB_TIPO_ACID_HIST_Pag_13_A.png")

gtsave(data = RS_PECONHENTOS_TAB_TIPO_ACID_HIST_Serpente,
       filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_TAB_TIPO_ACID_HIST_Serpente_Pag_13_B.png")

gtsave(data = RS_PECONHENTOS_TAB_TIPO_ACID_HIST_Aranha,
       filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_TAB_TIPO_ACID_HIST_Aranha_Pag_13_D.png")

gtsave(data = RS_PECONHENTOS_TAB_TIPO_ACID_HIST_Lagarta,
       filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_TAB_TAB_TIPO_ACID_HIST_Lagarta_Pag_14_A.png")

gtsave(data = RS_PECONHENTOS_TAB_ZONA_OCORRENCIA_HIST,
       filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_TAB_ZONA_HIST_Pag12_B.png")

gtsave(data = RS_PECONHENTOS_TAB_ZONA_OCORRENCIA_Municipios,
       filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_TAB_ZONA_Municipios_Pag_12_D.png")

gtsave(data = RS_PECONHENTOS_TAB_SEXO_HIST,
       filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_TAB_SEXO_HIST_Pag_10_D.png")

gtsave(data = RS_PECONHENTOS_TAB_SINAP_GERAL,
       filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_TAB_SINAP_GERAL_Pag_19_B.png")

gtsave(data = RS_PECONHENTOS_TAB_SINAP_Laudos_1,
       filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_TAB_SINAP_Laudos_1_Pag_20.png")

gtsave(data = RS_PECONHENTOS_TAB_SINAP_Laudos_2,
       filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_TAB_SINAP_Laudos_2_Pag_21.png")

gtsave(data = RS_PECONHENTOS_TAB_TIPO_ACID_Municipios,
       filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_TAB_TIPO_ACID_Municipios_Pag_15_A.png")

gtsave(data = RS_PECONHENTOS_TAB_TIPO_ACID_Municipios_Serpente,
       filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_TAB_TIPO_ACID_Municipios_Serpente_Pag_15_B.png")

gtsave(data = RS_PECONHENTOS_TAB_TIPO_ACID_Municipios_Aranha,
       filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_TAB_TIPO_ACID_Municipios_Aranha_15_C.png")

gtsave(data = RS_PECONHENTOS_TAB_TIPO_ACID_Municipios_lagarta,
       filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_TAB_TIPO_ACID_Municipios_Lagarta_15_D.png")

gtsave(data = RS_PECONHENTOS_TAB_TIPO_SINAN_ENCERRAMENTO,
       filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_TAB_TIPO_SINAN_ENCERRAMENTO_Pag_18_A.png")

gtsave(data = RS_PECONHENTOS_TAB_TIPO_SINAN_INCONSISTENCIAS_Serpentes,
       filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_TAB_TIPO_SINAN_INCONSISTENCIAS_Serpentes_Pag_18_B.png")

gtsave(data = RS_PECONHENTOS_TAB_TIPO_SINAN_INCONSISTENCIAS_Outros,
       filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_TAB_TIPO_SINAN_INCONSISTENCIAS_Outros_Pag_18_C.png")

gtsave(data = RS_PECONHENTOS_TAB_OBITOS,
       filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_TAB_OBITOS_Pag_17_D.png")

gtsave(data = RS_PECONHENTOS_TAB_OBITOS_HIST,
       filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_TAB_OBITOS_HIST_Pag_17_C.png")

gtsave(data = RS_PECONHENTOS_TAB_TEMPO_ATEND,
       filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_TAB_TEMPO_ATEND_Pag_16_D.png")

gtsave(data = RS_PECONHENTOS_TAB_TEMPO_ATEND_HIST,
       filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_TAB_TEMPO_ATEND_HIST_Pag_16_F.png")

#######################################    Mapas     #################################################################################

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_MAPA_REDE_SORO_ESC_Pag_04_B.png", 
       plot = RS_PECONHENTOS_MAPA_REDE_SORO_ESC,     
       width = 32,             
       height = 30,           
       units = "cm",           
       dpi = 300) 

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_MAPA_REDE_Ivaipora_Pag_05_A.png", 
       plot = RS_PECONHENTOS_MAPA_REDE_Ivaipora,     
       width = 32,             
       height = 30,           
       units = "cm",           
       dpi = 300) 

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_MAPA_REDE_SJI.Pag_06_A.png", 
       plot = RS_PECONHENTOS_MAPA_REDE_SJI,     
       width = 34,             
       height = 30,           
       units = "cm",           
       dpi = 300) 

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_MAPA_REDE_Nova_Tebas_Pag_08_A.png", 
       plot = RS_PECONHENTOS_MAPA_REDE_Nova_Tebas,     
       width = 40,             
       height = 30,           
       units = "cm",           
       dpi = 300) 

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_MAPA_REDE_MRI_Pag_07_A.png", 
       plot = RS_PECONHENTOS_MAPA_REDE_MRI,     
       width = 32,             
       height = 30,           
       units = "cm",           
       dpi = 300) 

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_MAPA_REDE_Lunardelli_Pag_08_C.png", 
       plot = RS_PECONHENTOS_MAPA_REDE_Lunardelli,     
       width = 32,             
       height = 30,           
       units = "cm",           
       dpi = 300) 

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_MAPA_ESCORPIOES_Pag_04_A.png", 
       plot = RS_PECONHENTOS_MAPA_ESCORPIOES,     
       width = 32,             
       height = 30,           
       units = "cm",           
       dpi = 300)

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_MAPA_SINAP_Amostras_Pag_19_A.png", 
       plot = RS_PECONHENTOS_MAPA_SINAP_Amostras,     
       width = 32,             
       height = 30,           
       units = "cm",           
       dpi = 300) 

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_MAP_ACID_ABELHA_HIST_Pag_14_C.png", 
       plot = RS_PECONHENTOS_MAP_ACID_ABELHA_HIST,     
       width = 32,             
       height = 30,           
       units = "cm",           
       dpi = 300) 

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_MAP_ACID_ESCORPIAO_HIST_Pag_14_D.png", 
       plot = RS_PECONHENTOS_MAP_ACID_ESCORPIAO_HIST,     
       width = 32,             
       height = 30,           
       units = "cm",           
       dpi = 300) 

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_MAP_ACID_ARANHAS_HIST_Pag_13_E.png", 
       plot = RS_PECONHENTOS_MAP_ACID_ARANHAS_HIST,     
       width = 32,             
       height = 30,           
       units = "cm",           
       dpi = 300) 

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_MAP_ACID_SERPENTES_HIST_Pag_13_C.png", 
       plot = RS_PECONHENTOS_MAP_ACID_SERPENTES_HIST,     
       width = 32,             
       height = 30,           
       units = "cm",           
       dpi = 300) 
 
ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/PECONHENTOS/RS_PECONHENTOS_MAP_ACID_LAGARTA_HIST_Pag_14_B.png", 
       plot = RS_PECONHENTOS_MAP_ACID_LAGARTA_HIST,     
       width = 32,             
       height = 30,           
       units = "cm",           
       dpi = 300) 


