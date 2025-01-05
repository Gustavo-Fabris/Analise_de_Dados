rm(list =ls())

#####      Definindo diretório de trabalho, caso tenha que trabalhar em Windows, acertar o diretório       ####

setwd("/home/gustavo/Área de Trabalho/Análise_de_Dados/")

############################################################################################
####   Definindo o objeto RS para servir de apoio para    ##################################
####    buscar dados de todas as RS. Usar 1, 2, 3..., 21, 22    ############################
############################################################################################

RS <- 22   #####  Deve-se colocar AQUI a Regional

####  libraries a serem utilizadas  ###

library(foreign)
library (dplyr)
library(stringr)
library(tidyr)
library(stringi)
library(tidygeocoder)
library(usethis)

####  Importando as bases de dados para formulação do Informe Epidemiológico      ####
####       As Bases IBGE são planilhas contendo os nomes do municípios e o        ####
####       código do IBGE (entre outros dados).                                   ####
####       São as bases para que os for loops funcionem.                          ####

BASE_IBGE<-read.table(file="Base_de_Dados/Auxiliares/Planilha_Base_IBGE.csv", 
                      header=TRUE, 
                      sep=",")

BASE_IBGE_BRASIL <- read.csv (file = "Base_de_Dados/Auxiliares/Planilha_Base_IBGE_BRASIL.csv",
                              header = TRUE,
                              sep = ",")

######   Criando objeto ID_REG. Será utilizado para selecionar
######   RS no DBF do SINAN ONLINE.

ID_REG <- as.data.frame(BASE_IBGE[which(BASE_IBGE$RS == RS), 6])

ID_REG <- as.numeric(ID_REG[1,1])

####   Estabelecendo o número de municípios em cada RS

nrow <- NROW(BASE_IBGE[which(BASE_IBGE$RS == RS), 1])

#######   Lista de Notificações com Coordenadas   ######

SINAN_Coordenadas_001 <- read.csv(file = "Tabulacoes_R/Arboviroses/RS22_23_24_SINAN_DECODIFICADO.csv",
                              header = TRUE,
                              sep = ",")

SINAN_Coordenadas_002 <- read.csv(file = "Base_de_Dados/Auxiliares/SINAN_Coordenadas_DENGUE.csv",
                                  header = TRUE,
                                  sep = ",")

####  Base DBF do SINAN. Deve-se baixá-las, renomeá-las e salvá-las no diretório correto  ######

DENGON2023 <- read.dbf(file = "Base_de_Dados/DBF/DENGON2023.dbf", 
                       as.is = FALSE) %>% select(ID_REGIONA, ID_RG_RESI, SEM_PRI, NU_NOTIFIC, NM_PACIENT, 
                                                 NM_LOGRADO, NU_NUMERO, NM_BAIRRO, NU_CEP, ID_MN_RESI, SG_UF)

DENGON2024 <- read.dbf(file = "Base_de_Dados/DBF/DENGON2024.dbf", 
                       as.is = FALSE) %>% select(ID_REGIONA, ID_RG_RESI, SEM_PRI, NU_NOTIFIC, NM_PACIENT, 
                                                 NM_LOGRADO, NU_NUMERO, NM_BAIRRO, NU_CEP, ID_MN_RESI, SG_UF)

###################################################################################################
###                                                                                             ###
###      Transformando coluna de semana epidemiológica de fator para numérica                   ###
###      (passando por texto, se for direto, ela transforma ###200905 em 06.                    ###
###      Seria possível realizar busca de SE passando direto de fator para numérica             ###
###      utilizando as.integer    (DENGON2009 ###%>% filter(ID_MUNICIP == 410165,               ###
###      SEM_PRI == 6) -1, para buscar SE 05?                                                   ###
###                                                                                             ###
###                  Será usado para buscar semanas epidemiológicas                             ###                       
###################################################################################################
###
DENGON2023$SEM_PRI <-as.numeric(as.character(DENGON2023$SEM_PRI))   ###############################
###
DENGON2024$SEM_PRI <-as.numeric(as.character(DENGON2024$SEM_PRI))   ###############################

######    Usando rbind para juntar as duas metades do período sazonal   ######

AUX01 <- DENGON2023 %>% 
  filter(ID_REGIONA == ID_REG | ID_RG_RESI == ID_REG, 
         SEM_PRI >= 202331)

AUX02 <- DENGON2024 %>% 
  filter(ID_REGIONA == ID_REG | ID_RG_RESI == ID_REG, 
         SEM_PRI <=202430)

SINAN_DENGUE_RS <- rbind(AUX01, 
                         AUX02)

SINAN_DENGUE_RS <- SINAN_DENGUE_RS[, c(4:11)]

####  For loop visando buscar o nome do município   #####

AUX02 <- data.frame(COD = SINAN_DENGUE_RS[,7], 
                    Municipio = NA)

for (i in SINAN_DENGUE_RS[,7]){
  AUX02[which(AUX02$COD == i), 2] <- BASE_IBGE_BRASIL[which(BASE_IBGE_BRASIL$Código.Município.Completo == i),13]
  
}

SINAN_DENGUE_RS[, 7] <- AUX02[, 2]

AUX01 <- data.frame(UF = SINAN_DENGUE_RS[, 8],
                    Estado = NA)

AUX01 <- AUX01 %>% mutate(UF = case_when(UF == 41 ~ "Parana",
                                         UF != 41 ~ "Não Procurar no API"))

SINAN_DENGUE_RS[, 8] <- as.data.frame(AUX01[,1])

AUX02 <- data.frame(UF = SINAN_DENGUE_RS[, 8],
                    Estado = NA)

AUX02 <- AUX02 %>% mutate(UF = case_when(UF == "Parana" ~ "Brasil",
                                         UF != "Paraná" ~ "Não Procurar no API"))

SINAN_DENGUE_RS$Pais <- AUX02[,1]

SINAN_DENGUE_RS <-SINAN_DENGUE_RS %>% filter(NM_LOGRADO != is.na(SINAN_DENGUE_RS$NM_LOGRADO),
                           NU_NUMERO != is.na(SINAN_DENGUE_RS$NU_NUMERO),
                           NU_NUMERO != "SN",
                           NU_CEP != is.na(SINAN_DENGUE_RS$NU_CEP))

SINAN_Coord <- SINAN_Coordenadas_001 %>% filter(Latitude != is.na(SINAN_Coordenadas_001$Latitude))

SINAN_Coord <- anti_join(SINAN_Coordenadas_001, SINAN_Coord,  by = c("SINAN" = "SINAN"))

SINAN_Coord$SINAN <- as.factor(SINAN_Coord$SINAN)

SINAN_Coord <- inner_join(SINAN_DENGUE_RS, SINAN_Coord, by = c("NU_NOTIFIC" = "SINAN"))

SINAN_Coord <- SINAN_Coord[, c(1:9)]

assign(paste0("RS", RS, "_23_24_SINAN_LOGRADOUROS"), 
       SINAN_Coord) 

write.csv(SINAN_Coord, 
          "Base_de_Dados/Auxiliares/RS_23_24_SINAN_LOGRADOUROS.csv",
          row.names = FALSE)

RS22_23_24_SINAN_LOGRADOUROS <- read.csv("Base_de_Dados/Auxiliares/RS_23_24_SINAN_LOGRADOUROS.csv", 
                                         header = TRUE,
                                         sep = ",",
                                         fileEncoding = "windows-1256")   

RS22_23_24_SINAN_LOGRADOUROS[, 7] <- SINAN_Coord[, 7]

AUX01 <- as.data.frame(apply(RS22_23_24_SINAN_LOGRADOUROS[, 3:9], 1, paste, collapse =", "))

AUX01[,2] <- RS22_23_24_SINAN_LOGRADOUROS[,1]

colnames(AUX01)[1] <- "Endereço"
colnames(AUX01)[2] <- "SINAN"

AUX01 <- AUX01[, c(2, 1)]

teste <- geocode(AUX01,
  address = Endereço,
        method = "google")

Notificacoes <- read.csv(file = "Tabulacoes_R/Arboviroses/RS22_23_24_SINAN_PROVAVEIS_DECODIFICADO.csv",
                         header = TRUE,
                         sep = ",")

Notificacoes <- left_join(Notificacoes, teste, by = c("SINAN" = "SINAN"))

write.csv(teste,
          "/home/gustavo/Área de Trabalho/teste.csv",
          row.names=FALSE)

edit_r_environ()

