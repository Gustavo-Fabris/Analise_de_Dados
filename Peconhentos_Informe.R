rm(list =ls())

####Indicando Diretório de Trabalho.#####

setwd("/home/gustavo/Área de Trabalho/Análise_de_Dados/")

###Libraries###

library(foreign)
library (dplyr)
library(ggplot2)

####Planilha com os dados dos municípios e com os códigos do IBGE. Será utilizada nos for loops para buscar dados######## 
####dos municípios e vinculá-los com os dados da base DBF do SINAN#######################################################

BASE_IBGE<-read.csv(file="Base_de_Dados/Planilha_Base_IBGE.csv", 
                    header=TRUE, 
                    sep=",")

CE_BASE_Notificados <- read.csv(file = "Base_de_Dados/Tabulacoes_R/Peconhentos/RS22_CE_BASE_Notificados.csv",
                                header = TRUE,
                                sep = ",")

CE_BASE_Aranhas_Notificados <- read.csv(file = "Base_de_Dados/Tabulacoes_R/Peconhentos/RS22_CE_BASE_Aranhas_Notificados.csv",
                                        header = TRUE,
                                        sep = ",")

CE_BASE_Serpentes_Notificados <- read.csv(file = "Base_de_Dados/Tabulacoes_R/Peconhentos/RS22_CE_BASE_Serpentes_Notificados.csv",
                                          header = TRUE,
                                          sep= ",")

CE_BASE_Escorpioes_Notificados <- read.csv(file = "Base_de_Dados/Tabulacoes_R/Peconhentos/RS22_CE_BASE_Escorpioes_Notificados.csv",
                                           header = TRUE,
                                           sep = ",")
RS_Serie_Historica <- read.csv(file ="Base_de_Dados/Tabulacoes_R/Peconhentos/RS22_Serie_Historica.csv",
                               header = TRUE,
                               sep = ",")

############################################################################################
####   Definindo o objeto RS para servir de apoio para    ##################################
####    buscar dados de todas as RS. Usar 1, 2, 3..., 21, 22    ############################
############################################################################################

RS <- 22   #####  Deve-se colocar AQUI a Regional

######   Criando objeto ID_REG. Será utilizado para selecionar
######   RS no DBF do SINAN ONLINE.

ID_REG <- as.data.frame(BASE_IBGE[which(BASE_IBGE$RS == RS), 6])

ID_REG <- as.numeric(ID_REG[1,1])

#########   Criando objeto Fonte para ser utilizado pelos gráficos   ######

Fonte <- "Fonte: SINAN. Base DBF acessada em 23/11/2023"

####   Estabelecendo o número de municípios em cada RS

nrow <- NROW(BASE_IBGE[which(BASE_IBGE$RS == RS), 1])

##################################################################
###############    2023   ########################################
##################################################################

####Criando um objeto com a base DBF 2023 do SINAN#################

PECONHENTO2023<- read.dbf(file = "Base_de_Dados/DBF/ANIMPNET2023.DBF",
                          as.is = FALSE)

SINAN_PECONHENTOS_2023 <- PECONHENTO2023 %>% filter(ID_REGIONA == ID_REG | ID_RG_RESI == ID_REG,
                                                    NU_ANO == 2023)

assign(paste0("RS", RS, "_PECONHENTOS_2023_SINAN"), SINAN_PECONHENTOS_2023)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2023_SINAN"), SINAN_PECONHENTOS_2023), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2023_SINAN.csv"), 
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
  
  AUX[which(AUX == i), 2] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==202301)%>%
                                          count()
  )
  
  AUX[which(AUX == i), 3] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                          filter(ID_MN_RESI ==i,
                                                 SEM_PRI ==202302) %>% 
                                          count()
  )
  
  AUX[which(AUX == i), 4] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==202303) %>% 
                                          count()
  )
  
  AUX[which(AUX == i),5] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                         filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==202304) %>% 
                                         count()
  )
  
  AUX[which(AUX == i), 6] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==202305) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 7] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                          filter(ID_MN_RESI ==i,
                                                 SEM_PRI ==202306) %>%
                                          count()
  )
  
  AUX[which(AUX == i), 8] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                          filter(ID_MN_RESI ==i,
                                                 SEM_PRI ==202307) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 9] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                          filter(ID_MN_RESI ==i,
                                                 SEM_PRI ==202308) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 10] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202309) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 11] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202310) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 12] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202311) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 13] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202312) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 14] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202313) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 15] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202314) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 16] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202315) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 17] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202316) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 18] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202317) %>%  
                                           count() 
  )
  
  AUX[which(AUX == i), 19] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202318) %>%      
                                           count() 
  )
  
  AUX[which(AUX == i), 20] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202319) %>%
                                           count() 
  )
  
  AUX[which(AUX == i),  21] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                            filter(ID_MN_RESI ==i,
                                                   SEM_PRI ==202320) %>%
                                            count() 
  )
  
  AUX[which(AUX == i), 22] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202321) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 23] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202322) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 24] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202323) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 25] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202324) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 26] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202325) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 27] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202326) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 28] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202327) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 29] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202328) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 30] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202329) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 31] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202330) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 32] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202331) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 33] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202332) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 34] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202333) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 35] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202334) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 36] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202335) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 37] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202336) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 38] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202337) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 39] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202338) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 40] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202339) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 41] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202340) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 42] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202341) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 43] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202342) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 44] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202343) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 45] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202344) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 46] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202345) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 47] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202346) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 48] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202347) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 49] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202348) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 50] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202349) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 51] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202350) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 52] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202351) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 53] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202352) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 54] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202353) %>%
                                           count() 
  )
}

AUX[, 1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX[(nrow(AUX)+ 1), 2:54] <- apply(AUX[, 2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2023_SE_Notificados"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2023_SE_Notificados"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2023_SE_Notificados.csv"), 
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
  
  AUX[which(AUX == i), 2] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==202301,
                                                 TP_ACIDENT == 1)%>%
                                          count()
  )
  
  AUX[which(AUX == i), 3] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                          filter(ID_MN_RESI ==i,
                                                 SEM_PRI ==202302,
                                                 TP_ACIDENT == 1) %>% 
                                          count()
  )
  
  AUX[which(AUX == i), 4] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==202303,
                                                 TP_ACIDENT == 1) %>% 
                                          count()
  )
  
  AUX[which(AUX == i),5] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                         filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==202304,
                                                TP_ACIDENT == 1) %>% 
                                         count()
  )
  
  AUX[which(AUX == i), 6] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==202305,
                                                 TP_ACIDENT == 1) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 7] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                          filter(ID_MN_RESI ==i,
                                                 SEM_PRI ==202306,
                                                 TP_ACIDENT == 1) %>%
                                          count()
  )
  
  AUX[which(AUX == i), 8] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                          filter(ID_MN_RESI ==i,
                                                 SEM_PRI ==202307,
                                                 TP_ACIDENT == 1) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 9] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                          filter(ID_MN_RESI ==i,
                                                 SEM_PRI ==202308,
                                                 TP_ACIDENT == 1) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 10] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202309,
                                                  TP_ACIDENT == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 11] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202310,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 12] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202311,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 13] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202312,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 14] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202313,
                                                  TP_ACIDENT == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 15] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202314,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 16] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202315,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 17] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202316,
                                                  TP_ACIDENT == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 18] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202317,
                                                  TP_ACIDENT == 1) %>%  
                                           count() 
  )
  
  AUX[which(AUX == i), 19] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202318,
                                                  TP_ACIDENT == 1) %>%      
                                           count() 
  )
  
  AUX[which(AUX == i), 20] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202319,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i),  21] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                            filter(ID_MN_RESI ==i,
                                                   SEM_PRI ==202320,
                                                   TP_ACIDENT == 1) %>%
                                            count() 
  )
  
  AUX[which(AUX == i), 22] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202321,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 23] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202322,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 24] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202323,
                                                  TP_ACIDENT == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 25] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202324,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 26] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202325,
                                                  TP_ACIDENT == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 27] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202326,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 28] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202327,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 29] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202328,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 30] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202329,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 31] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202330,
                                                  TP_ACIDENT == 1) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 32] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202331,
                                                  TP_ACIDENT == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 33] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202332,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 34] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202333,
                                                  TP_ACIDENT == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 35] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202334,
                                                  TP_ACIDENT == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 36] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202335,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 37] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202336,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 38] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202337,
                                                  TP_ACIDENT == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 39] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202338,
                                                  TP_ACIDENT == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 40] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202339,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 41] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202340,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 42] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202341,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 43] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202342,
                                                  TP_ACIDENT == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 44] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202343,
                                                  TP_ACIDENT == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 45] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202344,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 46] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202345,
                                                  TP_ACIDENT == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 47] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202346,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 48] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202347,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 49] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202348,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 50] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202349,
                                                  TP_ACIDENT == 1) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 51] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202350,
                                                  TP_ACIDENT == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 52] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202351,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 53] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202352,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 54] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202353,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
}

AUX[, 1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX[(nrow(AUX)+ 1), 2:54] <- apply(AUX[, 2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2023_SE_SERPENTES_Notificados"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2023_SE_SERPENTES_Notificados"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2023_SE_SERPENTES_Notificados.csv"), 
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
  
  AUX[which(AUX == i), 2] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==202301,
                                                 TP_ACIDENT == 2)%>%
                                          count()
  )
  
  AUX[which(AUX == i), 3] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                          filter(ID_MN_RESI ==i,
                                                 SEM_PRI ==202302,
                                                 TP_ACIDENT == 2) %>% 
                                          count()
  )
  
  AUX[which(AUX == i), 4] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==202303,
                                                 TP_ACIDENT == 2) %>% 
                                          count()
  )
  
  AUX[which(AUX == i),5] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                         filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==202304,
                                                TP_ACIDENT == 2) %>% 
                                         count()
  )
  
  AUX[which(AUX == i), 6] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==202305,
                                                 TP_ACIDENT == 2) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 7] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                          filter(ID_MN_RESI ==i,
                                                 SEM_PRI ==202306,
                                                 TP_ACIDENT == 2) %>%
                                          count()
  )
  
  AUX[which(AUX == i), 8] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                          filter(ID_MN_RESI ==i,
                                                 SEM_PRI ==202307,
                                                 TP_ACIDENT == 2) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 9] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                          filter(ID_MN_RESI ==i,
                                                 SEM_PRI ==202308,
                                                 TP_ACIDENT == 2) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 10] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202309,
                                                  TP_ACIDENT == 2) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 11] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202310,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 12] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202311,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 13] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202312,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 14] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202313,
                                                  TP_ACIDENT == 2) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 15] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202314,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 16] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202315,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 17] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202316,
                                                  TP_ACIDENT == 2) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 18] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202317,
                                                  TP_ACIDENT == 2) %>%  
                                           count() 
  )
  
  AUX[which(AUX == i), 19] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202318,
                                                  TP_ACIDENT == 2) %>%      
                                           count() 
  )
  
  AUX[which(AUX == i), 20] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202319,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i),  21] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                            filter(ID_MN_RESI ==i,
                                                   SEM_PRI ==202320,
                                                   TP_ACIDENT == 2) %>%
                                            count() 
  )
  
  AUX[which(AUX == i), 22] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202321,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 23] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202322,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 24] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202323,
                                                  TP_ACIDENT == 2) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 25] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202324,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 26] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202325,
                                                  TP_ACIDENT == 2) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 27] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202326,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 28] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202327,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 29] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202328,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 30] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202329,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 31] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202330,
                                                  TP_ACIDENT == 2) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 32] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202331,
                                                  TP_ACIDENT == 2) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 33] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202332,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 34] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202333,
                                                  TP_ACIDENT == 2) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 35] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202334,
                                                  TP_ACIDENT == 2) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 36] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202335,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 37] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202336,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 38] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202337,
                                                  TP_ACIDENT == 2) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 39] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202338,
                                                  TP_ACIDENT == 2) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 40] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202339,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 41] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202340,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 42] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202341,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 43] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202342,
                                                  TP_ACIDENT == 2) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 44] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202343,
                                                  TP_ACIDENT == 2) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 45] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202344,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 46] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202345,
                                                  TP_ACIDENT == 2) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 47] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202346,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 48] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202347,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 49] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202348,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 50] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202349,
                                                  TP_ACIDENT == 2) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 51] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202350,
                                                  TP_ACIDENT == 2) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 52] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202351,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 53] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202352,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 54] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202353,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
}

AUX[, 1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX[(nrow(AUX)+ 1), 2:54] <- apply(AUX[, 2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2023_SE_ARANHAS_Notificados"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2023_SE_ARANHAS_Notificados"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2023_SE_ARANHAS_Notificados.csv"), 
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
  
  AUX[which(AUX == i), 2] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==202301,
                                                 TP_ACIDENT == 3)%>%
                                          count()
  )
  
  AUX[which(AUX == i), 3] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                          filter(ID_MN_RESI ==i,
                                                 SEM_PRI ==202302,
                                                 TP_ACIDENT == 3) %>% 
                                          count()
  )
  
  AUX[which(AUX == i), 4] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==202303,
                                                 TP_ACIDENT == 3) %>% 
                                          count()
  )
  
  AUX[which(AUX == i),5] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                         filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==202304,
                                                TP_ACIDENT == 3) %>% 
                                         count()
  )
  
  AUX[which(AUX == i), 6] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==202305,
                                                 TP_ACIDENT == 3) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 7] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                          filter(ID_MN_RESI ==i,
                                                 SEM_PRI ==202306,
                                                 TP_ACIDENT == 3) %>%
                                          count()
  )
  
  AUX[which(AUX == i), 8] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                          filter(ID_MN_RESI ==i,
                                                 SEM_PRI ==202307,
                                                 TP_ACIDENT == 3) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 9] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                          filter(ID_MN_RESI ==i,
                                                 SEM_PRI ==202308,
                                                 TP_ACIDENT == 3) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 10] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202309,
                                                  TP_ACIDENT == 3) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 11] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202310,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 12] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202311,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 13] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202312,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 14] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202313,
                                                  TP_ACIDENT == 3) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 15] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202314,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 16] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202315,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 17] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202316,
                                                  TP_ACIDENT == 3) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 18] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202317,
                                                  TP_ACIDENT == 3) %>%  
                                           count() 
  )
  
  AUX[which(AUX == i), 19] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202318,
                                                  TP_ACIDENT == 3) %>%      
                                           count() 
  )
  
  AUX[which(AUX == i), 20] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202319,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i),  21] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                            filter(ID_MN_RESI ==i,
                                                   SEM_PRI ==202320,
                                                   TP_ACIDENT == 3) %>%
                                            count() 
  )
  
  AUX[which(AUX == i), 22] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202321,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 23] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202322,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 24] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202323,
                                                  TP_ACIDENT == 3) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 25] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202324,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 26] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202325,
                                                  TP_ACIDENT == 3) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 27] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202326,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 28] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202327,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 29] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202328,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 30] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202329,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 31] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202330,
                                                  TP_ACIDENT == 3) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 32] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202331,
                                                  TP_ACIDENT == 3) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 33] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202332,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 34] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202333,
                                                  TP_ACIDENT == 3) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 35] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202334,
                                                  TP_ACIDENT == 3) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 36] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202335,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 37] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202336,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 38] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202337,
                                                  TP_ACIDENT == 3) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 39] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202338,
                                                  TP_ACIDENT == 3) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 40] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202339,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 41] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202340,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 42] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202341,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 43] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202342,
                                                  TP_ACIDENT == 3) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 44] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202343,
                                                  TP_ACIDENT == 3) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 45] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202344,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 46] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202345,
                                                  TP_ACIDENT == 3) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 47] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202346,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 48] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202347,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 49] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202348,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 50] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202349,
                                                  TP_ACIDENT == 3) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 51] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202350,
                                                  TP_ACIDENT == 3) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 52] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202351,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 53] <- as.integer(SINAN_PECONHENTOS_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202352,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 54] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                           filter(ID_MN_RESI ==i,
                                                  SEM_PRI ==202353,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
}

AUX[, 1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX[(nrow(AUX)+ 1), 2:54] <- apply(AUX[, 2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2023_SE_ESCORPIOES_Notificados"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2023_SE_ESCORPIOES_Notificados"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2023_SE_ESCORPIOES_Notificados.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI == i) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i,
                                                          CS_ZONA == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i,
                                                          CS_ZONA == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i,
                                                          CS_ZONA == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i,
                                                          CS_ZONA == 9 
                                                          |
                                                            is.na(CS_ZONA)) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                    filter(ID_MN_RESI ==i,
                                                           CS_SEXO == "F") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                    filter(ID_MN_RESI ==i,
                                                           CS_SEXO == "M") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                    filter(ID_MN_RESI ==i,  
                                                           NU_IDADE_N <= 3012) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                    filter(ID_MN_RESI ==i,  
                                                           NU_IDADE_N > 4000 
                                                           & 
                                                             NU_IDADE_N <= 4005) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                    filter(ID_MN_RESI ==i,
                                                           NU_IDADE_N > 4005 
                                                           & 
                                                             NU_IDADE_N <= 4012) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                    filter(ID_MN_RESI ==i,
                                                           NU_IDADE_N > 4012 
                                                           & 
                                                             NU_IDADE_N <= 4018) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 16] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                    filter(ID_MN_RESI ==i,
                                                           NU_IDADE_N > 4018 
                                                           & 
                                                             NU_IDADE_N <= 4059) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 17] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                    filter(ID_MN_RESI ==i,
                                                           NU_IDADE_N > 4059) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 18] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                    filter(ID_MN_RESI ==i,
                                                           CS_ESCOL_N == "00") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 19] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                    filter(ID_MN_RESI ==i,
                                                           CS_ESCOL_N == "01" 
                                                           | 
                                                             CS_ESCOL_N == "02" 
                                                           | 
                                                             CS_ESCOL_N == "03") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 20] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                    filter(ID_MN_RESI ==i,
                                                           CS_ESCOL_N == "04") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 21] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                    filter(ID_MN_RESI ==i,
                                                           CS_ESCOL_N == "05") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 22] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                    filter(ID_MN_RESI ==i,
                                                           CS_ESCOL_N == "06") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 23] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                    filter(ID_MN_RESI ==i,
                                                           CS_ESCOL_N == "07") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 24] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                    filter(ID_MN_RESI ==i,
                                                           CS_ESCOL_N == "08") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 25] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                    filter(ID_MN_RESI ==i,
                                                           CS_ESCOL_N == "10") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 26] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                    filter(ID_MN_RESI ==i,
                                                           CS_ESCOL_N == "09") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 27] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
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
  
  AUX[which(AUX$COD_IBGE == i), 28] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                    filter(ID_MN_RESI ==i,
                                                           CS_GESTANT == "5") %>% 
                                                    count()
  )
  AUX[which(AUX$COD_IBGE == i), 29] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                    filter(ID_MN_RESI ==i,
                                                           CS_GESTANT == "6") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 30] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                    filter(ID_MN_RESI ==i,
                                                           CS_GESTANT == "9") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 31] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                    filter(ID_MN_RESI ==i,
                                                           CS_RACA == "01") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 32] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                    filter(ID_MN_RESI ==i,
                                                           CS_RACA == "2") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 33] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                    filter(ID_MN_RESI ==i,
                                                           CS_RACA == "3") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 34] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                    filter(ID_MN_RESI ==i,
                                                           CS_RACA == "4") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 35] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                    filter(ID_MN_RESI ==i,
                                                           CS_RACA == "5") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 36] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                    filter(ID_MN_RESI ==i,
                                                           CS_RACA == "9") %>% 
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2023_GERAL"), AUX)

assign(paste0("RS_PECONHENTOS_2023_GERAL"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2023_GERAL"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2023_GERAL.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_TEMPO_ == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_TEMPO_ == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_TEMPO_ == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_TEMPO_ == 4) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_TEMPO_ == 5) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_TEMPO_ == 6) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_TEMPO_ == 9) %>%   
                                                    count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2023_TEMPO_ATEND"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2023_TEMPO_ATEND"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2023_TEMPO_ATEND.csv"), 
           row.names = FALSE)

###############    Zona de Ocorrência     #####################

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Urbana <- NA

AUX$Rural <- NA

AUX$Periurbana <- NA

AUX$Ignorado <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_ZONA == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_ZONA == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_ZONA == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_ZONA == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2023_ZONA_ACIDENTE"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2023_ZONA_ACIDENTE"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2023_ZONA_ACIDENTE.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_LOCA_1 == "01") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_LOCA_1 == "02") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_LOCA_1 == "03") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_LOCA_1 == "04") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_LOCA_1 == "05") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_LOCA_1 == "06") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_LOCA_1 == "07") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_LOCA_1 == "08") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_LOCA_1 == "09") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_LOCA_1 == 10) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_LOCA_1 == 99) %>%   
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2023_LOCAL_PICADA"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2023_LOCAL_PICADA"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2023_LOCAL_PICADA.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 4) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 5) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             TP_ACIDENT == 6) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             TP_ACIDENT == 9) %>%   
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2023_TIPO_ACID"), AUX)

assign(paste0("RS_PECONHENTOS_2023_TIPO_ACID"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2023_TIPO_ACID"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2023_TIPO_ACID.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 1,
                                                          TRA_CLASSI == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 1,
                                                          TRA_CLASSI == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 1,
                                                          TRA_CLASSI == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 1,
                                                          TRA_CLASSI == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2023_CLASSI_CASO_SERPENTE"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2023_CLASSI_CASO_SERPENTE"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2023_CLASSI_CASO_SERPENTE.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 1,
                                                          ANI_SERPEN == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 1,
                                                          ANI_SERPEN == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 1,
                                                          ANI_SERPEN == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 1,
                                                          ANI_SERPEN == 4) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 1,
                                                          ANI_SERPEN == 5) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             TP_ACIDENT == 1,
                                                           ANI_SERPEN == 9) %>%   
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2023_TIPO_ACID_SERPENTE"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2023_TIPO_ACID_SERPENTE"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2023_TIPO_ACID_SERPENTE.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 1,
                                                          CON_SOROTE == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 1,
                                                          CON_SOROTE == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 1,
                                                          CON_SOROTE == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2023_SOROTERAPIA_SERPENTE"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2023_SOROTERAPIA_SERPENTE"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2023_SOROTERAPIA_SERPENTE.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 1,
                                                          COM_LOC == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 1,
                                                          COM_LOC == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 1,
                                                          COM_LOC == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2023_COMP_LOCAIS_SERPENTE"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2023_COMP_LOCAIS_SERPENTE"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2023_COMP_LOCAIS_SERPENTE.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 1,
                                                          COM_SISTEM == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 1,
                                                          COM_SISTEM == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 1,
                                                          COM_SISTEM == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2023_COMP_SIST_SERPENTE"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2023_COMP_SIST_SERPENTE"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2023_COMP_SIST_SERPENTE.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 1,
                                                          MCLI_LOCAL == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 1,
                                                          MCLI_LOCAL == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 1,
                                                          MCLI_LOCAL == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2023_MAN_LOCAIS_SERPENTE"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2023_MAN_LOCAIS_SERPENTE"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2023_MAN_LOCAIS_SERPENTE.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 1,
                                                          CLI_DOR == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 1,
                                                          CLI_EDEMA == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 1,
                                                          CLI_EQUIMO == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 1,
                                                          CLI_NECROS == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 1,
                                                          CLI_LOCAL_ == 1) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2023_MAN_LOCAIS_ESP_SERPENTE"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2023_MAN_LOCAIS_ESP_SERPENTE"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2023_MAN_LOCAIS_ESP_SERPENTE.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 1,
                                                          MCLI_SIST == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 1,
                                                          MCLI_SIST == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 1,
                                                          MCLI_SIST == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2023_MAN_SIST_SERPENTE"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2023_MAN_SIST_SERPENTE"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2023_MAN_SIST_SERPENTE.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 1,
                                                          CLI_NEURO == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 1,
                                                          CLI_HEMORR == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 1,
                                                          CLI_VAGAIS == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 1,
                                                          CLI_MIOLIT == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 1,
                                                          CLI_RENAL == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 1,
                                                          CLI_OUTR_2 == 1) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2023_MAN_SIST_ESP_SERPENTE"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2023_MAN_SIST_ESP_SERPENTE"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2023_MAN_SIST_ESP_SERPENTE.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 2,
                                                          TRA_CLASSI == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 2,
                                                          TRA_CLASSI == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 2,
                                                          TRA_CLASSI == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 2,
                                                          TRA_CLASSI == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2023_CLASSI_CASO_ARANHA"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2023_CLASSI_CASO_ARANHA"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2023_CLASSI_CASO_ARANHA.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 2,
                                                          ANI_ARANHA == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 2,
                                                          ANI_ARANHA == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 2,
                                                          ANI_ARANHA == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 2,
                                                          ANI_ARANHA == 4) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 2,
                                                          ANI_ARANHA == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2023_TIPO_ACID_ARANHA"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2023_TIPO_ACID_ARANHA"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2023_TIPO_ACID_ARANHA.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 2,
                                                          CON_SOROTE == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 2,
                                                          CON_SOROTE == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 2,
                                                          CON_SOROTE == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2023_SOROTERAPIA_ARANHA"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2023_SOROTERAPIA_ARANHA"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2023_SOROTERAPIA_ARANHA.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 2,
                                                          COM_LOC == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 2,
                                                          COM_LOC == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 2,
                                                          COM_LOC == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2023_COMP_LOCAIS_ARANHA"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2023_COMP_LOCAIS_ARANHA"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2023_COMP_LOCAIS_ARANHA.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 2,
                                                          COM_SISTEM == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 2,
                                                          COM_SISTEM == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 2,
                                                          COM_SISTEM == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2023_COMP_SIST_ARANHA"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2023_COMP_SIST_ARANHA"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2023_COMP_SIST_ARANHA.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 2,
                                                          MCLI_LOCAL == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 2,
                                                          MCLI_LOCAL == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 2,
                                                          MCLI_LOCAL == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2023_MAN_LOCAIS_ARANHA"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2023_MAN_LOCAIS_ARANHA"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2023_MAN_LOCAIS_ARANHA.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 2,
                                                          CLI_DOR == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 2,
                                                          CLI_EDEMA == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 2,
                                                          CLI_EQUIMO == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 2,
                                                          CLI_NECROS == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 2,
                                                          CLI_LOCAL_ == 1) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2023_MAN_LOCAIS_ESP_ARANHA"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2023_MAN_LOCAIS_ESP_ARANHA"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2023_MAN_LOCAIS_ESP_ARANHA.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 2,
                                                          MCLI_SIST == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 2,
                                                          MCLI_SIST == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 2,
                                                          MCLI_SIST == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2023_MAN_SIST_ARANHA"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2023_MAN_SIST_ARANHA"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2023_MAN_SIST_ARANHA.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 2,
                                                          CLI_NEURO == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 2,
                                                          CLI_HEMORR == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 2,
                                                          CLI_VAGAIS == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 2,
                                                          CLI_MIOLIT == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 2,
                                                          CLI_RENAL == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 2,
                                                          CLI_OUTR_2 == 1) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2023_MAN_SIST_ESP_ARANHA"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2023_MAN_SIST_ESP_ARANHA"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2023_MAN_SIST_ESP_ARANHA.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 3,
                                                          TRA_CLASSI == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 3,
                                                          TRA_CLASSI == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 3,
                                                          TRA_CLASSI == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 3,
                                                          TRA_CLASSI == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2023_CLASSI_CASO_ESCORPIAO"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2023_CLASSI_CASO_ESCORPIAO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2023_CLASSI_CASO_ESCORPIAO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 3,
                                                          CON_SOROTE == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 3,
                                                          CON_SOROTE == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 3,
                                                          CON_SOROTE == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2023_SOROTERAPIA_ESCORPIAO"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2023_SOROTERAPIA_ESCORPIAO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2023_SOROTERAPIA_ESCORPIAO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 3,
                                                          COM_LOC == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 3,
                                                          COM_LOC == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 3,
                                                          COM_LOC == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2023_COMP_LOCAIS_ESCORPIAO"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2023_COMP_LOCAIS_ESCORPIAO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2023_COMP_LOCAIS_ESCORPIAO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 3,
                                                          COM_SISTEM == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 3,
                                                          COM_SISTEM == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 3,
                                                          COM_SISTEM == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2023_COMP_SIST_ESCORPIAO"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2023_COMP_SIST_ESCORPIAO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2023_COMP_SIST_ESCORPIAO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 3,
                                                          MCLI_LOCAL == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 3,
                                                          MCLI_LOCAL == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 3,
                                                          MCLI_LOCAL == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2023_MAN_LOCAIS_ESCORPIAO"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2023_MAN_LOCAIS_ESCORPIAO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2023_MAN_LOCAIS_ESCORPIAO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 3,
                                                          CLI_DOR == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 3,
                                                          CLI_EDEMA == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 3,
                                                          CLI_EQUIMO == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 3,
                                                          CLI_NECROS == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 3,
                                                          CLI_LOCAL_ == 1) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2023_MAN_LOCAIS_ESP_ESCORPIAO"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2023_MAN_LOCAIS_ESP_ESCORPIAO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2023_MAN_LOCAIS_ESP_ESCORPIAO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 3,
                                                          MCLI_SIST == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 3,
                                                          MCLI_SIST == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 3,
                                                          MCLI_SIST == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2023_MAN_SIST_ESCORPIAO"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2023_MAN_SIST_ESCORPIAO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2023_MAN_SIST_ESCORPIAO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 3,
                                                          CLI_NEURO == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 3,
                                                          CLI_HEMORR == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 3,
                                                          CLI_VAGAIS == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 3,
                                                          CLI_MIOLIT == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 3,
                                                          CLI_RENAL == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 3,
                                                          CLI_OUTR_2 == 1) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2023_MAN_SIST_ESP_ESCORPIAO"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2023_MAN_SIST_ESP_ESCORPIAO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2023_MAN_SIST_ESP_ESCORPIAO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 4,
                                                          TRA_CLASSI == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 4,
                                                          TRA_CLASSI == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 4,
                                                          TRA_CLASSI == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 4,
                                                          TRA_CLASSI == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2023_CLASSI_CASO_LAGARTA"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2023_CLASSI_CASO_LAGARTA"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2023_CLASSI_CASO_LAGARTA.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 4,
                                                          ANI_LAGART == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 4,
                                                          ANI_LAGART == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 4,
                                                          ANI_LAGART == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2023_TIPO_ACID_LAGARTA"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2023_TIPO_ACID_LAGARTA"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2023_TIPO_ACID_LAGARTA.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 4,
                                                          CON_SOROTE == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 4,
                                                          CON_SOROTE == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 4,
                                                          CON_SOROTE == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2023_SOROTERAPIA_LAGARTA"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2023_SOROTERAPIA_LAGARTA"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2023_SOROTERAPIA_LAGARTA.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 4,
                                                          COM_LOC == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 4,
                                                          COM_LOC == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 4,
                                                          COM_LOC == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2023_COMP_LOCAIS_LAGARTA"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2023_COMP_LOCAIS_LAGARTA"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2023_COMP_LOCAIS_LAGARTA.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 4,
                                                          COM_SISTEM == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 4,
                                                          COM_SISTEM == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 4,
                                                          COM_SISTEM == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2023_COMP_SIST_LAGARTA"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2023_COMP_SIST_LAGARTA"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2023_COMP_SIST_LAGARTA.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 4,
                                                          MCLI_LOCAL == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 4,
                                                          MCLI_LOCAL == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 4,
                                                          MCLI_LOCAL == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2023_MAN_LOCAIS_LAGARTA"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2023_MAN_LOCAIS_LAGARTA"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2023_MAN_LOCAIS_LAGARTA.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 4,
                                                          CLI_DOR == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 4,
                                                          CLI_EDEMA == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 4,
                                                          CLI_EQUIMO == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 4,
                                                          CLI_NECROS == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 4,
                                                          CLI_LOCAL_ == 1) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2023_MAN_LOCAIS_ESP_LAGARTA"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2023_MAN_LOCAIS_ESP_LAGARTA"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2023_MAN_LOCAIS_ESP_LAGARTA.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 4,
                                                          MCLI_SIST == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 4,
                                                          MCLI_SIST == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 4,
                                                          MCLI_SIST == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2023_MAN_SIST_LAGARTA"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2023_MAN_SIST_LAGARTA"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2023_MAN_SIST_LAGARTA.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 4,
                                                          CLI_NEURO == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 4,
                                                          CLI_HEMORR == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 4,
                                                          CLI_VAGAIS == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 4,
                                                          CLI_MIOLIT == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 4,
                                                          CLI_RENAL == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 4,
                                                          CLI_OUTR_2 == 1) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2023_MAN_SIST_ESP_LAGARTA"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2023_MAN_SIST_ESP_LAGARTA"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2023_MAN_SIST_ESP_LAGARTA.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 5,
                                                          TRA_CLASSI == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 5,
                                                          TRA_CLASSI == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 5,
                                                          TRA_CLASSI == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 5,
                                                          TRA_CLASSI == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2023_CLASSI_CASO_ABELHA"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2023_CLASSI_CASO_ABELHA"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2023_CLASSI_CASO_ABELHA.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 5,
                                                          CON_SOROTE == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 5,
                                                          CON_SOROTE == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 5,
                                                          CON_SOROTE == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2023_SOROTERAPIA_ABELHA"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2023_SOROTERAPIA_ABELHA"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2023_SOROTERAPIA_ABELHA.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 5,
                                                          COM_LOC == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 5,
                                                          COM_LOC == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 5,
                                                          COM_LOC == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2023_COMP_LOCAIS_ABELHA"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2023_COMP_LOCAIS_ABELHA"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2023_COMP_LOCAIS_ABELHA.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 5,
                                                          COM_SISTEM == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 5,
                                                          COM_SISTEM == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 5,
                                                          COM_SISTEM == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2023_COMP_SIST_ABELHA"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2023_COMP_SIST_ABELHA"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2023_COMP_SIST_ABELHA.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 5,
                                                          MCLI_LOCAL == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 5,
                                                          MCLI_LOCAL == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 5,
                                                          MCLI_LOCAL == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2023_MAN_LOCAIS_ABELHA"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2023_MAN_LOCAIS_ABELHA"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2023_MAN_LOCAIS_ABELHA.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 5,
                                                          CLI_DOR == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 5,
                                                          CLI_EDEMA == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 5,
                                                          CLI_EQUIMO == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 5,
                                                          CLI_NECROS == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 5,
                                                          CLI_LOCAL_ == 1) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2023_MAN_LOCAIS_ESP_ABELHA"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2023_MAN_LOCAIS_ESP_ABELHA"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2023_MAN_LOCAIS_ESP_ABELHA.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 5,
                                                          MCLI_SIST == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 5,
                                                          MCLI_SIST == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 5,
                                                          MCLI_SIST == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2023_MAN_SIST_ABELHA"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2023_MAN_SIST_ABELHA"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2023_MAN_SIST_ABELHA.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 5,
                                                          CLI_NEURO == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 5,
                                                          CLI_HEMORR == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 5,
                                                          CLI_VAGAIS == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 5,
                                                          CLI_MIOLIT == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 5,
                                                          CLI_RENAL == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 5,
                                                          CLI_OUTR_2 == 1) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2023_MAN_SIST_ESP_ABELHA"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2023_MAN_SIST_ESP_ABELHA"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2023_MAN_SIST_ESP_ABELHA.csv"), 
           row.names = FALSE)

rm(SINAN_PECONHENTOS_2023)

######################################################################################################################################
######################################################################################################################################
######################          FIM FIM FIM FIM FIM FIM     ##########################################################################


############################    Gráficos e Mapas    ##################################################################################

###############################       Canais Endêmicos    ############################################################################

#####################################   NOTIFICADOS      #############################################################################

CE_BASE_Notificados[(nrow(CE_BASE_Notificados) +1), 1] <- "2023"
CE_BASE_Notificados[nrow(CE_BASE_Notificados), 2:54] <- as.integer(data.frame(RS22_PECONHENTOS_2023_SE_Notificados[nrow(RS22_PECONHENTOS_2023_SE_Notificados), 2:54]))

#####Utilizando objetos auxiliares porque se transpor o data frame direto ele transforma as variáveis em#############
#####caracter.            NÃO FOI DESCARTADO AINDA OS PERÍODOS EPIDÊMICOS                               #############
##### VERIFICAR SE PODE-SE UTILIZAR A MÉDIA COMO LIMITE INFERIOR.                                       #############

AUX <- CE_BASE_Notificados[, -1]

AUX <- t(AUX)

AUX2 <- CE_BASE_Notificados[, 1]

colnames(AUX) <- AUX2

CE_BASE_Notificados <- AUX

######Criando a coluna de média no data.frame#####################

AUX <- apply(CE_BASE_Notificados[,1: (ncol(CE_BASE_Notificados)-1)], 1 , mean)

CE_BASE_Notificados <- as.data.frame(CE_BASE_Notificados)

CE_BASE_Notificados$Media <- AUX

######Criando a coluna de Desvio Padrão no data frame###############

AUX <- apply(CE_BASE_Notificados[,1: (ncol(CE_BASE_Notificados) -2)], 1 , sd)

CE_BASE_Notificados$Desvio_Padrao <- AUX

###### Criando a coluna de Média + 2(DP)

AUX <- CE_BASE_Notificados[, (ncol(CE_BASE_Notificados)-1):ncol(CE_BASE_Notificados)]

AUX <- AUX %>% mutate(Lim_Superior = (Media + 1.96 * Desvio_Padrao))

CE_BASE_Notificados$Lim_Superior <- AUX$Lim_Superior

CE_BASE_Notificados[, (ncol(CE_BASE_Notificados)+1)] <- rownames(CE_BASE_Notificados)

CE_BASE_Notificados <- CE_BASE_Notificados[, c(ncol(CE_BASE_Notificados), 1:(ncol(CE_BASE_Notificados) -1))]

CE_BASE_Notificados[,1] <- c(1:53)

colnames(CE_BASE_Notificados)[1] <- "Semana_Epidemiológica"

rownames(CE_BASE_Notificados) <- c(1:nrow(CE_BASE_Notificados))

rm(AUX, AUX2)

write.csv (CE_BASE_Notificados, 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_CE_Notificados.csv"), 
           row.names = FALSE)

AUX_GRAF <- CE_BASE_Notificados[, c(ncol(CE_BASE_Notificados), ncol(CE_BASE_Notificados) -1, ncol(CE_BASE_Notificados) -2)]

###Criando uma coluna de ordem das se para o R não colocar em ordem numérica.

AUX_GRAF$Ordem <- c(1: nrow(CE_BASE_Notificados))

###Puxando o período sazonal atual para o gráfico de linhas

AUX_GRAF$`2023` <- CE_BASE_Notificados$`2023`

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
       title = "Acidentes com Animais Peçonhentos",
       subtitle = paste0("Canal Endêmico ", RS, "ª Regional de Saúde")) +
  theme(
    panel.grid.major = element_line(color = "#C0C0C0"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#DC143C"),
    plot.title = element_text(face = "bold",
                              size = 24,
                              colour = "black")
  ) +
  geom_area(aes(y = Lim_Superior), fill = "#F0E68C",alpha = 0.9) +
  geom_area(aes( y = Media), fill = "#556B2F") +
  geom_line(aes( y = `2023`), stat = "identity", color = "black", linewidth = 1.5) +
  xlab("Semana Epidemiológica") +
  ylab("Número de Casos") +
  scale_x_continuous(breaks = c(1:53), label = AUX_GRAF$Sem_EPI) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))))

################################################       ARANHAS     #####################################################################

CE_BASE_Aranhas_Notificados[(nrow(CE_BASE_Aranhas_Notificados) +1), 1] <- "2023"
CE_BASE_Aranhas_Notificados[nrow(CE_BASE_Aranhas_Notificados), 2:54] <- as.integer(data.frame(RS22_PECONHENTOS_2023_SE_ARANHAS_Notificados[nrow(RS22_PECONHENTOS_2023_SE_ARANHAS_Notificados), 2:54]))

#####Utilizando objetos auxiliares porque se transpor o data frame direto ele transforma as variáveis em#############
#####caracter.            NÃO FOI DESCARTADO AINDA OS PERÍODOS EPIDÊMICOS                               #############
##### VERIFICAR SE PODE-SE UTILIZAR A MÉDIA COMO LIMITE INFERIOR.                                       #############

AUX <- CE_BASE_Aranhas_Notificados[, -1]

AUX <- t(AUX)

AUX2 <- CE_BASE_Aranhas_Notificados[, 1]

colnames(AUX) <- AUX2

CE_BASE_Aranhas_Notificados <- AUX

######Criando a coluna de média no data.frame#####################

AUX <- apply(CE_BASE_Aranhas_Notificados[,1: (ncol(CE_BASE_Aranhas_Notificados)-1)], 1 , mean)

CE_BASE_Aranhas_Notificados <- as.data.frame(CE_BASE_Aranhas_Notificados)

CE_BASE_Aranhas_Notificados$Media <- AUX

######Criando a coluna de Desvio Padrão no data frame###############

AUX <- apply(CE_BASE_Aranhas_Notificados[,1: (ncol(CE_BASE_Aranhas_Notificados) -2)], 1 , sd)

CE_BASE_Aranhas_Notificados$Desvio_Padrao <- AUX

###### Criando a coluna de Média + 2(DP)

AUX <- CE_BASE_Aranhas_Notificados[, (ncol(CE_BASE_Aranhas_Notificados)-1):ncol(CE_BASE_Aranhas_Notificados)]

AUX <- AUX %>% mutate(Lim_Superior = (Media + 1.96 * Desvio_Padrao))

CE_BASE_Aranhas_Notificados$Lim_Superior <- AUX$Lim_Superior

CE_BASE_Aranhas_Notificados[, (ncol(CE_BASE_Aranhas_Notificados)+1)] <- rownames(CE_BASE_Aranhas_Notificados)

CE_BASE_Aranhas_Notificados <- CE_BASE_Aranhas_Notificados[, c(ncol(CE_BASE_Aranhas_Notificados), 1:(ncol(CE_BASE_Aranhas_Notificados) -1))]

CE_BASE_Aranhas_Notificados[,1] <- c(1:53)

colnames(CE_BASE_Notificados)[1] <- "Semana_Epidemiológica"

rownames(CE_BASE_Aranhas_Notificados) <- c(1:nrow(CE_BASE_Aranhas_Notificados))

rm(AUX, AUX2)

write.csv (CE_BASE_Aranhas_Notificados, 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_CE_Aranhas_Notificados.csv"), 
           row.names = FALSE)

AUX_GRAF <- CE_BASE_Aranhas_Notificados[, c(ncol(CE_BASE_Aranhas_Notificados), ncol(CE_BASE_Aranhas_Notificados) -1, ncol(CE_BASE_Aranhas_Notificados) -2)]

###Criando uma coluna de ordem das se para o R não colocar em ordem numérica.

AUX_GRAF$Ordem <- c(1: nrow(CE_BASE_Aranhas_Notificados))

###Puxando o período sazonal atual para o gráfico de linhas

AUX_GRAF$`2023` <- CE_BASE_Aranhas_Notificados$`2023`

AUX_GRAF$Sem_EPI <-as.character(c("01",  "02", "03",  "04",  "05",  "06",  "07", 
                                  "08",  "09",  "10",  "11",  "12",  "13",  "14",  
                                  "15",  "16",  "17",  "18",  "19",  "20",  "21",  
                                  "22",  "23",  "24",  "25",  "26",  "27",  "28",  
                                  "28",  "30",  "31",  "32",  "33", "34",  "35",  
                                  "36",  "37",  "38",  "39",  "40",  "41",  "42",  
                                  "43",  "44",  "45",  "46",  "47",  "48",  "49",  
                                  "50",  "51",  "52",  "53"))

assign(paste0("RS", RS, "_GRAF_CE_Notificados_Aranhas"), ggplot(AUX_GRAF, aes(Ordem))  +
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = .5,
                                   face = "bold",
                                   size = 12)) +
  labs(caption = Fonte,
       title = "Acidentes com Aranhas",
       subtitle = paste0("Canal Endêmico ", RS, "ª Regional de Saúde"))+
  theme(
    panel.grid.major = element_line(color = "#C0C0C0"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#DC143C"),
    plot.title = element_text(face = "bold",
                              size = 24,
                              colour = "black")
  ) +
  geom_area(aes(y = Lim_Superior), fill = "#F0E68C",alpha = 0.9) +
  geom_area(aes( y = Media), fill = "#556B2F") +
  geom_line(aes( y = `2023`), stat = "identity", color = "black", linewidth = 1.5) +
  xlab("Semana Epidemiológica") +
  ylab("Número de Casos") +
  scale_x_continuous(breaks = c(1:53), label = AUX_GRAF$Sem_EPI) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))))

################################################       Serpentes     #####################################################################

CE_BASE_Serpentes_Notificados[(nrow(CE_BASE_Serpentes_Notificados) +1), 1] <- "2023"
CE_BASE_Serpentes_Notificados[nrow(CE_BASE_Serpentes_Notificados), 2:54] <- as.integer(data.frame(RS22_PECONHENTOS_2023_SE_SERPENTES_Notificados[nrow(RS22_PECONHENTOS_2023_SE_SERPENTES_Notificados), 2:54]))

#####Utilizando objetos auxiliares porque se transpor o data frame direto ele transforma as variáveis em#############
#####caracter.            NÃO FOI DESCARTADO AINDA OS PERÍODOS EPIDÊMICOS                               #############
##### VERIFICAR SE PODE-SE UTILIZAR A MÉDIA COMO LIMITE INFERIOR.                                       #############

AUX <- CE_BASE_Serpentes_Notificados[, -1]

AUX <- t(AUX)

AUX2 <- CE_BASE_Serpentes_Notificados[, 1]

colnames(AUX) <- AUX2

CE_BASE_Serpentes_Notificados <- AUX

######Criando a coluna de média no data.frame#####################

AUX <- apply(CE_BASE_Serpentes_Notificados[,1: (ncol(CE_BASE_Serpentes_Notificados)-1)], 1 , mean)

CE_BASE_Serpentes_Notificados <- as.data.frame(CE_BASE_Serpentes_Notificados)

CE_BASE_Serpentes_Notificados$Media <- AUX

######Criando a coluna de Desvio Padrão no data frame###############

AUX <- apply(CE_BASE_Serpentes_Notificados[,1: (ncol(CE_BASE_Serpentes_Notificados) -2)], 1 , sd)

CE_BASE_Serpentes_Notificados$Desvio_Padrao <- AUX

###### Criando a coluna de Média + 2(DP)

AUX <- CE_BASE_Serpentes_Notificados[, (ncol(CE_BASE_Serpentes_Notificados)-1):ncol(CE_BASE_Serpentes_Notificados)]

AUX <- AUX %>% mutate(Lim_Superior = (Media + 1.96 * Desvio_Padrao))

CE_BASE_Serpentes_Notificados$Lim_Superior <- AUX$Lim_Superior

CE_BASE_Serpentes_Notificados[, (ncol(CE_BASE_Serpentes_Notificados)+1)] <- rownames(CE_BASE_Serpentes_Notificados)

CE_BASE_Serpentes_Notificados <- CE_BASE_Serpentes_Notificados[, c(ncol(CE_BASE_Serpentes_Notificados), 1:(ncol(CE_BASE_Serpentes_Notificados) -1))]

CE_BASE_Serpentes_Notificados[,1] <- c(1:53)

colnames(CE_BASE_Notificados)[1] <- "Semana_Epidemiológica"

rownames(CE_BASE_Serpentes_Notificados) <- c(1:nrow(CE_BASE_Serpentes_Notificados))

rm(AUX, AUX2)

write.csv (CE_BASE_Serpentes_Notificados, 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_CE_Serpentes_Notificados.csv"), 
           row.names = FALSE)

AUX_GRAF <- CE_BASE_Serpentes_Notificados[, c(ncol(CE_BASE_Serpentes_Notificados), ncol(CE_BASE_Serpentes_Notificados) -1, ncol(CE_BASE_Serpentes_Notificados) -2)]

###Criando uma coluna de ordem das se para o R não colocar em ordem numérica.

AUX_GRAF$Ordem <- c(1: nrow(CE_BASE_Serpentes_Notificados))

###Puxando o período sazonal atual para o gráfico de linhas

AUX_GRAF$`2023` <- CE_BASE_Serpentes_Notificados$`2023`

AUX_GRAF$Sem_EPI <-as.character(c("01",  "02", "03",  "04",  "05",  "06",  "07", 
                                  "08",  "09",  "10",  "11",  "12",  "13",  "14",  
                                  "15",  "16",  "17",  "18",  "19",  "20",  "21",  
                                  "22",  "23",  "24",  "25",  "26",  "27",  "28",  
                                  "28",  "30",  "31",  "32",  "33", "34",  "35",  
                                  "36",  "37",  "38",  "39",  "40",  "41",  "42",  
                                  "43",  "44",  "45",  "46",  "47",  "48",  "49",  
                                  "50",  "51",  "52",  "53"))

assign(paste0("RS", RS, "_GRAF_CE_Notificados_Serpentes"), ggplot(AUX_GRAF, aes(Ordem))  +
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = .5,
                                   face = "bold",
                                   size = 12)) +
  labs(caption = Fonte,
       title = "Acidentes com Serpentes",
       paste0("Canal Endêmico ", RS, "ª Regional de Saúde")) +
  theme(
    panel.grid.major = element_line(color = "#C0C0C0"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#DC143C"),
    plot.title = element_text(face = "bold",
                              size = 24,
                              colour = "black")
  ) +
  geom_area(aes(y = Lim_Superior), fill = "#F0E68C",alpha = 0.9) +
  geom_area(aes( y = Media), fill = "#556B2F") +
  geom_line(aes( y = `2023`), stat = "identity", color = "black", linewidth = 1.5) +
  xlab("Semana Epidemiológica") +
  ylab("Número de Casos") +
  scale_x_continuous(breaks = c(1:53), label = AUX_GRAF$Sem_EPI) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))))


################################################       Escorpioes     #####################################################################

CE_BASE_Escorpioes_Notificados[(nrow(CE_BASE_Escorpioes_Notificados) +1), 1] <- "2023"
CE_BASE_Escorpioes_Notificados[nrow(CE_BASE_Escorpioes_Notificados), 2:54] <- as.integer(data.frame(RS22_PECONHENTOS_2023_SE_ESCORPIOES_Notificados[nrow(RS22_PECONHENTOS_2023_SE_ESCORPIOES_Notificados), 2:54]))

#####Utilizando objetos auxiliares porque se transpor o data frame direto ele transforma as variáveis em#############
#####caracter.            NÃO FOI DESCARTADO AINDA OS PERÍODOS EPIDÊMICOS                               #############
##### VERIFICAR SE PODE-SE UTILIZAR A MÉDIA COMO LIMITE INFERIOR.                                       #############

AUX <- CE_BASE_Escorpioes_Notificados[, -1]

AUX <- t(AUX)

AUX2 <- CE_BASE_Escorpioes_Notificados[, 1]

colnames(AUX) <- AUX2

CE_BASE_Escorpioes_Notificados <- AUX

######Criando a coluna de média no data.frame#####################

AUX <- apply(CE_BASE_Escorpioes_Notificados[,1: (ncol(CE_BASE_Escorpioes_Notificados)-1)], 1 , mean)

CE_BASE_Escorpioes_Notificados <- as.data.frame(CE_BASE_Escorpioes_Notificados)

CE_BASE_Escorpioes_Notificados$Media <- AUX

######Criando a coluna de Desvio Padrão no data frame###############

AUX <- apply(CE_BASE_Escorpioes_Notificados[,1: (ncol(CE_BASE_Escorpioes_Notificados) -2)], 1 , sd)

CE_BASE_Escorpioes_Notificados$Desvio_Padrao <- AUX

###### Criando a coluna de Média + 2(DP)

AUX <- CE_BASE_Escorpioes_Notificados[, (ncol(CE_BASE_Escorpioes_Notificados)-1):ncol(CE_BASE_Escorpioes_Notificados)]

AUX <- AUX %>% mutate(Lim_Superior = (Media + 1.96 * Desvio_Padrao))

CE_BASE_Escorpioes_Notificados$Lim_Superior <- AUX$Lim_Superior

CE_BASE_Escorpioes_Notificados[, (ncol(CE_BASE_Escorpioes_Notificados)+1)] <- rownames(CE_BASE_Escorpioes_Notificados)

CE_BASE_Escorpioes_Notificados <- CE_BASE_Escorpioes_Notificados[, c(ncol(CE_BASE_Escorpioes_Notificados), 1:(ncol(CE_BASE_Escorpioes_Notificados) -1))]

CE_BASE_Escorpioes_Notificados[,1] <- c(1:53)

colnames(CE_BASE_Notificados)[1] <- "Semana_Epidemiológica"

rownames(CE_BASE_Escorpioes_Notificados) <- c(1:nrow(CE_BASE_Escorpioes_Notificados))

rm(AUX, AUX2)

write.csv (CE_BASE_Escorpioes_Notificados, 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_CE_Escorpioes_Notificados.csv"), 
           row.names = FALSE)

AUX_GRAF <- CE_BASE_Escorpioes_Notificados[, c(ncol(CE_BASE_Escorpioes_Notificados), ncol(CE_BASE_Escorpioes_Notificados) -1, ncol(CE_BASE_Escorpioes_Notificados) -2)]

###Criando uma coluna de ordem das se para o R não colocar em ordem numérica.

AUX_GRAF$Ordem <- c(1: nrow(CE_BASE_Escorpioes_Notificados))

###Puxando o período sazonal atual para o gráfico de linhas

AUX_GRAF$`2023` <- CE_BASE_Escorpioes_Notificados$`2023`

AUX_GRAF$Sem_EPI <-as.character(c("01",  "02", "03",  "04",  "05",  "06",  "07", 
                                  "08",  "09",  "10",  "11",  "12",  "13",  "14",  
                                  "15",  "16",  "17",  "18",  "19",  "20",  "21",  
                                  "22",  "23",  "24",  "25",  "26",  "27",  "28",  
                                  "28",  "30",  "31",  "32",  "33", "34",  "35",  
                                  "36",  "37",  "38",  "39",  "40",  "41",  "42",  
                                  "43",  "44",  "45",  "46",  "47",  "48",  "49",  
                                  "50",  "51",  "52",  "53"))

assign(paste0("RS", RS, "_GRAF_CE_Notificados_Escorpioes"), ggplot(AUX_GRAF, aes(Ordem))  +
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = .5,
                                   face = "bold",
                                   size = 12)) +
  labs(caption = Fonte,
       title = "Acidentes com Escorpiões",
       paste0("Canal Endêmico ", RS, "ª Regional de Saúde")) +
  theme(
    panel.grid.major = element_line(color = "#C0C0C0"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#DC143C"),
    plot.title = element_text(face = "bold",
                              size = 24,
                              colour = "black")
  ) +
  geom_area(aes(y = Lim_Superior), fill = "#F0E68C",alpha = 0.9) +
  geom_area(aes( y = Media), fill = "#556B2F") +
  geom_line(aes( y = `2023`), stat = "identity", color = "black", linewidth = 1.5) +
  xlab("Semana Epidemiológica") +
  ylab("Número de Casos") +
  scale_x_continuous(breaks = c(1:53), label = AUX_GRAF$Sem_EPI) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))))

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
                                  colour = "#556B2F"),
        legend.position = "bottom")
}

######################################################################################################################################
################    Séries Históricas      ######################################

RS_Serie_Historica[nrow(RS_Serie_Historica) +1, 2] <- RS_PECONHENTOS_2023_GERAL[nrow(RS_PECONHENTOS_2023_GERAL), 5]

RS_Serie_Historica[nrow(RS_Serie_Historica), 1] <- "2023"

RS_Serie_Historica[nrow(RS_Serie_Historica), c(3:9)] <- RS_PECONHENTOS_2023_TIPO_ACID[nrow(RS_PECONHENTOS_2023_TIPO_ACID), c(5:11)]

RS_Serie_Historica[, 1] <- as.factor(RS_Serie_Historica[, 1])

assign(paste0("RS", RS, "_GRAF_Serie_Historica_Notificados"), ggplot(RS_Serie_Historica, aes(x = Ano,
                                        y = Notificados)) +
                  geom_bar(stat = "identity",
                           color = "black",
                           fill = "#856363") +
         labs(caption = Fonte, 
              y = "Número de Casos",
              x = NULL,
              title = "SÉRIE HISTÓRICA DE CASOS NOTIFICADOS - 2009 a 2023",
              subtitle = "Casos notificados no Território da 22ª Regional de Saúde") +
         geom_label(aes(label = Notificados), 
                    size = 3, 
                    alpha = 0.5,
                    vjust = 0.1)  +
         scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
         Theme() +
         theme(axis.text.x = element_text(angle = 0))
)

assign(paste0("RS", RS, "_GRAF_Serie_Historica_Serpentes"), ggplot(RS_Serie_Historica, aes(x = Ano,
                                                                                             y = Serpente)) +
         geom_bar(stat = "identity",
                  color = "black",
                  fill = "#856363") +
         labs(caption = Fonte, 
              y = "Número de Casos",
              x = NULL,
              title = "SÉRIE HISTÓRICA DE CASOS NOTIFICADOS (SERPENTES) - 2009 a 2023",
              subtitle = "Casos notificados no Território da 22ª Regional de Saúde") +
         geom_label(aes(label = Serpente), 
                    size = 3, 
                    alpha = 0.5,
                    vjust = 0.1)  +
         scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
         Theme() +
         theme(axis.text.x = element_text(angle = 0))
)

assign(paste0("RS", RS, "_GRAF_Serie_Historica_Notificados_Aranhas"), ggplot(RS_Serie_Historica, aes(x = Ano,
                                                                                             y = Aranha)) +
         geom_bar(stat = "identity",
                  color = "black",
                  fill = "#856363") +
         labs(caption = Fonte, 
              y = "Número de Casos",
              x = NULL,
              title = "SÉRIE HISTÓRICA DE CASOS NOTIFICADOS (ARANHAS) - 2009 a 2023",
              subtitle = "Casos notificados no Território da 22ª Regional de Saúde") +
         geom_label(aes(label = Aranha), 
                    size = 3, 
                    alpha = 0.5,
                    vjust = 0.1)  +
         scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
         Theme() +
         theme(axis.text.x = element_text(angle = 0))
)

assign(paste0("RS", RS, "_GRAF_Serie_Historica_Notificados_Escorpioes"), ggplot(RS_Serie_Historica, aes(x = Ano,
                                                                                             y = Escorpiao)) +
         geom_bar(stat = "identity",
                  color = "black",
                  fill = "#856363") +
         labs(caption = Fonte, 
              y = "Número de Casos",
              x = NULL,
              title = "SÉRIE HISTÓRICA DE CASOS NOTIFICADOS (ESCORPIÕES) - 2009 a 2023",
              subtitle = "Casos notificados no Território da 22ª Regional de Saúde") +
         geom_label(aes(label = Escorpiao), 
                    size = 3, 
                    alpha = 0.5,
                    vjust = 0.1)  +
         scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
         Theme() +
         theme(axis.text.x = element_text(angle = 0))
)

################    Dados do Período atual    ##############

AUX_GRAF <- RS_PECONHENTOS_2023_GERAL[nrow(RS_PECONHENTOS_2023_GERAL), 6:9]

AUX_GRAF[2, ] <- colnames(AUX_GRAF)

AUX_GRAF <- t(AUX_GRAF)

colnames(AUX_GRAF)[1] <- "Dados"

colnames(AUX_GRAF)[2] <- "Rotulos"

AUX_GRAF <- as.data.frame(AUX_GRAF)

AUX_GRAF$Ordem <- as.factor(1: nrow(AUX_GRAF))

AUX_GRAF[, 1] <- as.numeric(AUX_GRAF[, 1])

assign(paste0("RS", RS, "_GRAF_ZONA_OCORRENCIA"), ggplot(AUX_GRAF, aes(x = Rotulos)) +
                                                           geom_bar(aes(y = Dados),
                                                         stat = "identity"))

RS22_GRAF_ZONA_OCORRENCIA
rm(AUX_GRAF,
   BASE_IBGE,
   CE_BASE_Aranhas_Notificados,
   CE_BASE_Escorpioes_Notificados,
   CE_BASE_Notificados,
   CE_BASE_Serpentes_Notificados,
   PECONHENTO2023,
   RS_Serie_Historica,
   RS_PECONHENTOS_2023_GERAL,
   RS_PECONHENTOS_2023_TIPO_ACID,
   Fonte,
   i,
   ID_REG,
   nrow,
   RS,
   Theme)
