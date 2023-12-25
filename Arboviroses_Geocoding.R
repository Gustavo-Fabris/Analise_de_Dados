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
                       as.is = FALSE) %>% select(ID_REGIONA, NU_NOTIFIC, ID_GEO1,ID_GEO2, ID_AGRAVO, ID_REGIONA, DT_NOTIFIC, 
                                                 NU_ANO, SEM_NOT, DT_SIN_PRI, SEM_PRI,  SG_UF_NOT, ID_MUNICIP, NM_PACIENT, DT_NASC,
                                                 NU_IDADE_N, CS_SEXO, CS_GESTANT, CS_ESCOL_N, NM_MAE_PAC, ID_MN_RESI, SG_UF, ID_RG_RESI, 
                                                 NM_LOGRADO, NU_NUMERO, NM_BAIRRO, NU_CEP, CS_ZONA, DT_DIGITA, DT_INVEST, FEBRE, MIALGIA, 
                                                 CEFALEIA, EXANTEMA, VOMITO, NAUSEA, DOR_COSTAS, CONJUNTVIT, ARTRITE, ARTRALGIA, PETEQUIA_N,
                                                 LEUCOPENIA, LACO, DOR_RETRO, DIABETES, HEMATOLOG, HEPATOPAT, RENAL, HIPERTENSA, ACIDO_PEPT, 
                                                 AUTO_IMUNE, DT_SORO, RESUL_SORO, DT_PCR, RESUL_PCR_, SOROTIPO, CLASSI_FIN, CRITERIO, 
                                                 TPAUTOCTO, COUFINF, COMUNINF, CO_BAINF, EVOLUCAO, HOSPITALIZ, DT_INTERNA, DT_OBITO, 
                                                 DT_ENCERRA, DT_ALRM, ALRM_LETAR, ALRM_HEPAT, ALRM_LIQ, ALRM_HIPOT, ALRM_PLAQ, ALRM_VOM, 
                                                 ALRM_SANG, ALRM_HEMAT, ALRM_ABDOM, DT_GRAV, GRAV_PULSO, GRAV_CONV, GRAV_ENCH, GRAV_INSUF, 
                                                 GRAV_TAQUI, GRAV_EXTRE, GRAV_HIPOT, GRAV_HEMAT, GRAV_MELEN, GRAV_METRO, GRAV_SANG, GRAV_AST,
                                                 GRAV_MIOC, GRAV_CONSC, GRAV_ORGAO, MANI_HEMOR, EPISTAXE, GENGIVO, METRO, DS_OBS)

DENGON2024 <- read.dbf(file = "Base_de_Dados/DBF/DENGON2024.dbf", 
                       as.is = FALSE) %>% select(ID_REGIONA, NU_NOTIFIC, ID_GEO1,ID_GEO2, ID_AGRAVO, ID_REGIONA, DT_NOTIFIC, NU_ANO, 
                                                 SEM_NOT, DT_SIN_PRI, SEM_PRI,  SG_UF_NOT, ID_MUNICIP, NM_PACIENT, DT_NASC, NU_IDADE_N, 
                                                 CS_SEXO, CS_GESTANT, CS_ESCOL_N, NM_MAE_PAC, ID_MN_RESI, SG_UF, ID_RG_RESI, NM_LOGRADO,
                                                 NU_NUMERO, NM_BAIRRO, NU_CEP, CS_ZONA, DT_DIGITA, DT_INVEST, FEBRE, MIALGIA, CEFALEIA, 
                                                 EXANTEMA, VOMITO, NAUSEA, DOR_COSTAS, CONJUNTVIT, ARTRITE, ARTRALGIA, PETEQUIA_N, 
                                                 LEUCOPENIA, LACO, DOR_RETRO, DIABETES, HEMATOLOG, HEPATOPAT, RENAL, HIPERTENSA, ACIDO_PEPT,
                                                 AUTO_IMUNE, DT_SORO, RESUL_SORO, DT_PCR, RESUL_PCR_, SOROTIPO, CLASSI_FIN, CRITERIO, 
                                                 TPAUTOCTO, COUFINF, COMUNINF, CO_BAINF, EVOLUCAO, HOSPITALIZ, DT_INTERNA, DT_OBITO,
                                                 DT_ENCERRA, DT_ALRM, ALRM_LETAR, ALRM_HEPAT, ALRM_LIQ, ALRM_HIPOT, ALRM_PLAQ, ALRM_VOM, 
                                                 ALRM_SANG, ALRM_HEMAT, ALRM_ABDOM, DT_GRAV, GRAV_PULSO, GRAV_CONV, GRAV_ENCH, GRAV_INSUF, 
                                                 GRAV_TAQUI, GRAV_EXTRE, GRAV_HIPOT, GRAV_HEMAT, GRAV_MELEN, GRAV_METRO, GRAV_SANG, GRAV_AST,
                                                 GRAV_MIOC, GRAV_CONSC, GRAV_ORGAO, MANI_HEMOR, EPISTAXE, GENGIVO, METRO, DS_OBS)

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

SINAN_DENGUE_RS_Reduzido <- SINAN_DENGUE_RS[, c(2, 23, 24, 25, 26, 21)]

AUX01 <- data.frame(UF = SINAN_DENGUE_RS_Reduzido[, 6],
                    Estado = NA)

AUX01 <- AUX01 %>% mutate(UF = case_when(UF == 41 ~ "Paraná",
                                         UF != 41 ~ "Não Procurar no API"))

SINAN_DENGUE_RS_Reduzido[, 6] <- as.data.frame(AUX01[,1])


AUX02 <- data.frame(UF = SINAN_DENGUE_RS_Reduzido[, 6],
                    Estado = NA)

AUX02 <- AUX02 %>% mutate(UF = case_when(UF == "Paraná" ~ "Brasil",
                                         UF != "Paraná" ~ "Não Procurar no API"))

SINAN_DENGUE_RS_Reduzido$Pais <- AUX02[,1]

SINAN_DENGUE_RS_Reduzido <-SINAN_DENGUE_RS_Reduzido %>% filter(NM_LOGRADO != is.na(SINAN_DENGUE_RS_Reduzido$NM_LOGRADO),
                           NU_NUMERO != is.na(SINAN_DENGUE_RS_Reduzido$NU_NUMERO))

SINAN_DENGUE_RS_Reduzido <- SINAN_DENGUE_RS_Reduzido %>% filter(NU_NUMERO != "SN")

SINAN_Coord <- SINAN_Coordenadas_001 %>% filter(Latitude != is.na(SINAN_Coordenadas_001$Latitude))

SINAN_Coord <- anti_join(SINAN_Coordenadas_001, SINAN_Coord,  by = c("SINAN" = "SINAN"))

SINAN_Coord$SINAN <- as.factor(SINAN_Coord$SINAN)

RS22_23_24_SINAN_LOGRADOUROS <- left_join(SINAN_DENGUE_RS_Reduzido, SINAN_Coord, by = c("NU_NOTIFIC" = "SINAN"))

AUX01 <- RS22_23_24_SINAN_LOGRADOUROS %>% filter(Caso != is.na(RS22_23_24_SINAN_LOGRADOUROS$Caso))

RS22_23_24_SINAN_LOGRADOUROS <- anti_join(RS22_23_24_SINAN_LOGRADOUROS, AUX01, by = c("NU_NOTIFIC" = "NU_NOTIFIC"))

assign(paste0("RS", RS, "_23_24_SINAN_LOGRADOUROS"), 
       SINAN_DENGUE_RS_Reduzido) 

write.csv(RS22_23_24_SINAN_LOGRADOUROS, 
          "Base_de_Dados/Auxiliares/RS22_23_24_SINAN_LOGRADOUROS.csv",
          row.names = FALSE)

RS22_23_24_SINAN_LOGRADOUROS <- read.csv("/home/gustavo/Área de Trabalho/RS22_23_24_SINAN_LOGRADOUROS.csv", 
                                         header = TRUE,
                                         sep = ",",
                                         fileEncoding = "windows-1252")
rm(SINAN_DENGUE_RS, 
   DENGON2023, 
   DENGON2024)



AUX01 <- as.data.frame(apply(RS22_23_24_SINAN_LOGRADOUROS[, 2:3], 1, paste, collapse =", "))
AUX01[,2:3] <- RS22_23_24_SINAN_LOGRADOUROS[,c(1,4)]

colnames(AUX01)[1] <- "Endereço"
colnames(AUX01)[3] <- "CEP"

AUX01 <- AUX01[, c(2, 1, 3)]

teste <- geocode(AUX01,
  address = Endereço,
        method = "osm")

?paste
rm(endereço, teste)
