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

######  Definindo a fonte dos gráficos   #####

Fonte <- "SINAN: Base DBF. Acessada em"

############################################################################################
####   Definindo o objeto RS para servir de apoio para    ##################################
####    buscar dados de todas as RS. Usar 1, 2, 3..., 21, 22    ############################
############################################################################################

RS <- 22   #####  Deve-se colocar AQUI a Regional

######   Criando objeto ID_REG. Será utilizado para selecionar
######   RS no DBF do SINAN ONLINE.

ID_REG <- as.data.frame(BASE_IBGE[which(BASE_IBGE$RS == RS), 6])

ID_REG <- as.numeric(ID_REG[1,1])

####   Estabelecendo o número de municípios em cada RS

nrow <- NROW(BASE_IBGE[which(BASE_IBGE$RS == RS), 1])

#############   BASE DE DADOS SINAN   #####

LEPTONET2023 <- read.dbf(file = "Base_de_Dados/DBF/LEPTONET.DBF",
                         as.is = FALSE)
##################################################################################
#################   2023    ######################################################
##################################################################################

SINAN_LEPTO_2023 <- LEPTONET2023 %>% filter(ID_REGIONA == ID_REG | ID_RG_RESI == ID_REG,
                                        NU_ANO == 2023)

assign(paste0("RS", RS, "_LEPTO_2023_SINAN"), SINAN_LEPTO_2023)

write.csv (assign(paste0("RS", RS, "_LEPTO_2023_SINAN"), SINAN_LEPTO_2023), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2023_SINAN.csv"), 
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
  
  AUX[which(AUX == i), 2] <- as.integer(SINAN_LEPTO_2023 %>%
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==202301)%>%
                                          count()
  )
  
  AUX[which(AUX == i), 3] <- as.integer(SINAN_LEPTO_2023 %>% 
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==202302) %>% 
                                          count()
  )
  
  AUX[which(AUX == i), 4] <- as.integer(SINAN_LEPTO_2023 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==202303) %>% 
                                          count()
  )
  
  AUX[which(AUX == i),5] <- as.integer(SINAN_LEPTO_2023 %>% 
                                         filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==202304) %>% 
                                         count()
  )
  
  AUX[which(AUX == i), 6] <- as.integer(SINAN_LEPTO_2023 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==202305) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 7] <- as.integer(SINAN_LEPTO_2023 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==202306) %>%
                                          count()
  )
  
  AUX[which(AUX == i), 8] <- as.integer(SINAN_LEPTO_2023 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==202307) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 9] <- as.integer(SINAN_LEPTO_2023 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==202308) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 10] <- as.integer(SINAN_LEPTO_2023 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202309) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 11] <- as.integer(SINAN_LEPTO_2023 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202310) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 12] <- as.integer(SINAN_LEPTO_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202311) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 13] <- as.integer(SINAN_LEPTO_2023 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202312) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 14] <- as.integer(SINAN_LEPTO_2023 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202313) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 15] <- as.integer(SINAN_LEPTO_2023 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202314) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 16] <- as.integer(SINAN_LEPTO_2023 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202315) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 17] <- as.integer(SINAN_LEPTO_2023 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202316) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 18] <- as.integer(SINAN_LEPTO_2023 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202317) %>%  
                                           count() 
  )
  
  AUX[which(AUX == i), 19] <- as.integer(SINAN_LEPTO_2023 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202318) %>%      
                                           count() 
  )
  
  AUX[which(AUX == i), 20] <- as.integer(SINAN_LEPTO_2023 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202319) %>%
                                           count() 
  )
  
  AUX[which(AUX == i),  21] <- as.integer(SINAN_LEPTO_2023 %>% 
                                            filter(ID_MN_RESI ==i & 
                                                     SEM_PRI ==202320) %>%
                                            count() 
  )
  
  AUX[which(AUX == i), 22] <- as.integer(SINAN_LEPTO_2023 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202321) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 23] <- as.integer(SINAN_LEPTO_2023 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202322) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 24] <- as.integer(SINAN_LEPTO_2023 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202323) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 25] <- as.integer(SINAN_LEPTO_2023 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202324) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 26] <- as.integer(SINAN_LEPTO_2023 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202325) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 27] <- as.integer(SINAN_LEPTO_2023 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202326) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 28] <- as.integer(SINAN_LEPTO_2023 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202327) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 29] <- as.integer(SINAN_LEPTO_2023 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202328) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 30] <- as.integer(SINAN_LEPTO_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202329) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 31] <- as.integer(SINAN_LEPTO_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202330) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 32] <- as.integer(SINAN_LEPTO_2023 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202331) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 33] <- as.integer(SINAN_LEPTO_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202332) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 34] <- as.integer(SINAN_LEPTO_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202333) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 35] <- as.integer(SINAN_LEPTO_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202334) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 36] <- as.integer(SINAN_LEPTO_2023 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202335) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 37] <- as.integer(SINAN_LEPTO_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202336) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 38] <- as.integer(SINAN_LEPTO_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202337) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 39] <- as.integer(SINAN_LEPTO_2023 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202338) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 40] <- as.integer(SINAN_LEPTO_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202339) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 41] <- as.integer(SINAN_LEPTO_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202340) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 42] <- as.integer(SINAN_LEPTO_2023 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202341) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 43] <- as.integer(SINAN_LEPTO_2023 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202342) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 44] <- as.integer(SINAN_LEPTO_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202343) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 45] <- as.integer(SINAN_LEPTO_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202344) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 46] <- as.integer(SINAN_LEPTO_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202345) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 47] <- as.integer(SINAN_LEPTO_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202346) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 48] <- as.integer(SINAN_LEPTO_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202347) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 49] <- as.integer(SINAN_LEPTO_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202348) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 50] <- as.integer(SINAN_LEPTO_2023 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202349) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 51] <- as.integer(SINAN_LEPTO_2023 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202350) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 52] <- as.integer(SINAN_LEPTO_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202351) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 53] <- as.integer(SINAN_LEPTO_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202352) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 54] <- as.integer(SINAN_LEPTO_2023 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202353) %>%
                                           count() 
  )
}

AUX[, 1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX[(nrow(AUX)+ 1), 2:54] <- apply(AUX[, 2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2023_SE_Notificados"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2023_SE_Notificados"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2023_SE_Notificados.csv"), 
           row.names = FALSE)

############################################################################################################################
############      Filtrando os dados por SE Confirmados para elaborar o Canal Endêmico   ###################################

AUX <- matrix(data = NA, 
              nrow = nrow, 
              ncol = 54)

AUX <- as.data.frame(AUX)

colnames(AUX)[1] <- "Município" 

AUX[,1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

colnames (AUX)[2:54] <- c(1:53)

for (i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX == i), 2] <- as.integer(SINAN_LEPTO_2023 %>%
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==202301,
                                                 CLASSI_FIN == 1)%>%
                                          count()
  )
  
  AUX[which(AUX == i), 3] <- as.integer(SINAN_LEPTO_2023 %>% 
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==202302,
                                                 CLASSI_FIN == 1) %>% 
                                          count()
  )
  
  AUX[which(AUX == i), 4] <- as.integer(SINAN_LEPTO_2023 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==202303,
                                                 CLASSI_FIN == 1) %>% 
                                          count()
  )
  
  AUX[which(AUX == i),5] <- as.integer(SINAN_LEPTO_2023 %>% 
                                         filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==202304,
                                                CLASSI_FIN == 1) %>% 
                                         count()
  )
  
  AUX[which(AUX == i), 6] <- as.integer(SINAN_LEPTO_2023 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==202305,
                                                 CLASSI_FIN == 1) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 7] <- as.integer(SINAN_LEPTO_2023 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==202306,
                                                 CLASSI_FIN == 1) %>%
                                          count()
  )
  
  AUX[which(AUX == i), 8] <- as.integer(SINAN_LEPTO_2023 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==202307,
                                                 CLASSI_FIN == 1) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 9] <- as.integer(SINAN_LEPTO_2023 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==202308,
                                                 CLASSI_FIN == 1) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 10] <- as.integer(SINAN_LEPTO_2023 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202309,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 11] <- as.integer(SINAN_LEPTO_2023 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202310,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 12] <- as.integer(SINAN_LEPTO_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202311,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 13] <- as.integer(SINAN_LEPTO_2023 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202312,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 14] <- as.integer(SINAN_LEPTO_2023 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202313,
                                                  CLASSI_FIN == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 15] <- as.integer(SINAN_LEPTO_2023 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202314,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 16] <- as.integer(SINAN_LEPTO_2023 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202315,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 17] <- as.integer(SINAN_LEPTO_2023 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202316,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 18] <- as.integer(SINAN_LEPTO_2023 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202317,
                                                  CLASSI_FIN == 1) %>%  
                                           count() 
  )
  
  AUX[which(AUX == i), 19] <- as.integer(SINAN_LEPTO_2023 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202318,
                                                  CLASSI_FIN == 1) %>%      
                                           count() 
  )
  
  AUX[which(AUX == i), 20] <- as.integer(SINAN_LEPTO_2023 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202319,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i),  21] <- as.integer(SINAN_LEPTO_2023 %>% 
                                            filter(ID_MN_RESI ==i & 
                                                     SEM_PRI ==202320,
                                                   CLASSI_FIN == 1) %>%
                                            count() 
  )
  
  AUX[which(AUX == i), 22] <- as.integer(SINAN_LEPTO_2023 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202321,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 23] <- as.integer(SINAN_LEPTO_2023 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202322,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 24] <- as.integer(SINAN_LEPTO_2023 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202323,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 25] <- as.integer(SINAN_LEPTO_2023 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202324,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 26] <- as.integer(SINAN_LEPTO_2023 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202325,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 27] <- as.integer(SINAN_LEPTO_2023 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202326,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 28] <- as.integer(SINAN_LEPTO_2023 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202327,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 29] <- as.integer(SINAN_LEPTO_2023 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202328,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 30] <- as.integer(SINAN_LEPTO_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202329,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 31] <- as.integer(SINAN_LEPTO_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202330,
                                                  CLASSI_FIN == 1) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 32] <- as.integer(SINAN_LEPTO_2023 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202331,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 33] <- as.integer(SINAN_LEPTO_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202332,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 34] <- as.integer(SINAN_LEPTO_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202333,
                                                  CLASSI_FIN == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 35] <- as.integer(SINAN_LEPTO_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202334,
                                                  CLASSI_FIN == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 36] <- as.integer(SINAN_LEPTO_2023 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202335,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 37] <- as.integer(SINAN_LEPTO_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202336,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 38] <- as.integer(SINAN_LEPTO_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202337,
                                                  CLASSI_FIN == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 39] <- as.integer(SINAN_LEPTO_2023 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202338,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 40] <- as.integer(SINAN_LEPTO_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202339,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 41] <- as.integer(SINAN_LEPTO_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202340,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 42] <- as.integer(SINAN_LEPTO_2023 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202341,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 43] <- as.integer(SINAN_LEPTO_2023 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202342,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 44] <- as.integer(SINAN_LEPTO_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202343,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 45] <- as.integer(SINAN_LEPTO_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202344,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 46] <- as.integer(SINAN_LEPTO_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202345,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 47] <- as.integer(SINAN_LEPTO_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202346,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 48] <- as.integer(SINAN_LEPTO_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202347,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 49] <- as.integer(SINAN_LEPTO_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202348,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 50] <- as.integer(SINAN_LEPTO_2023 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202349,
                                                  CLASSI_FIN == 1) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 51] <- as.integer(SINAN_LEPTO_2023 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202350,
                                                  CLASSI_FIN == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 52] <- as.integer(SINAN_LEPTO_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202351,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 53] <- as.integer(SINAN_LEPTO_2023 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202352,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 54] <- as.integer(SINAN_LEPTO_2023 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202353,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
}

AUX[, 1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX[(nrow(AUX)+ 1), 2:54] <- apply(AUX[, 2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2023_SE_Confirmados"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2023_SE_Confirmados"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2023_SE_Confirmados.csv"), 
           row.names = FALSE)

#####        FIltrando os dados por município e construindo uma tabela geral     s#####

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Notificados <- NA

AUX$Confirmados <- NA

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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                   filter(ID_MN_RESI == i) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CLASSI_FIN == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CS_ZONA == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CS_ZONA == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CS_ZONA == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ZONA == 9 
                                                           |
                                                             is.na(CS_ZONA)) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_SEXO == "F") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_SEXO == "M") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             NU_IDADE_N <= 3012) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             NU_IDADE_N > 4000 
                                                           & 
                                                             NU_IDADE_N <= 4005) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             NU_IDADE_N > 4005 
                                                           & 
                                                             NU_IDADE_N <= 4012) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 16] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             NU_IDADE_N > 4012 
                                                           & 
                                                             NU_IDADE_N <= 4018) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 17] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             NU_IDADE_N > 4018 
                                                           & 
                                                             NU_IDADE_N <= 4059) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 18] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             NU_IDADE_N > 4059) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 19] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "00") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 20] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "01" 
                                                           | 
                                                             CS_ESCOL_N == "02" 
                                                           | 
                                                             CS_ESCOL_N == "03") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 21] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "04") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 22] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "05") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 23] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "06") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 24] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "07") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 25] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "08") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 26] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "10") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 27] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "09") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 28] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_GESTANT == "1" 
                                                           |
                                                             CS_GESTANT== "2" 
                                                           | 
                                                             CS_GESTANT == "3" 
                                                           | 
                                                             CS_GESTANT == "4") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 29] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_GESTANT == "5") %>% 
                                                    count()
  )
  AUX[which(AUX$COD_IBGE == i), 30] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_GESTANT == "6") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 31] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_GESTANT == "9") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 32] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "01") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 33] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "2") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 34] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "3") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 35] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "4") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 36] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "5") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 37] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "9") %>% 
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2023_GERAL"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2023_GERAL"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2023_GERAL.csv"), 
           row.names = FALSE)

#####     Situação de Risco      ##############

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Agua_Lama <- NA

AUX$Criacao_Animais <- NA

AUX$Caixa_dagua <- NA

AUX$Fossa <- NA

AUX$Sinais_Roedores <- NA

AUX$Plantio_Colheita <- NA

AUX$Rio_Corrego <- NA

AUX$Roedores_Diretamente <- NA

AUX$Armazenamento_Graos <- NA

AUX$Terreno_Baldio <- NA

AUX$Lixo_Entulho <- NA

AUX$Outras <- NA


for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_CB_LAM == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_CB_CRI == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_CB_CAI == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_CB_FOS == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_CB_SIN == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_CB_PLA == 1) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_CB_COR == 1) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             ANT_CB_ROE == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             ANT_CB_GRA == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_CB_TER == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_CB_LIX == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 16] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_CB_OUT == 4018) %>% 
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2023_SIT_RISCO"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2023_SIT_RISCO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2023_SIT_RISCO.csv"), 
           row.names = FALSE)

#####     Sinais e Sintomas      ##############

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Febre <- NA

AUX$Mialgia <- NA

AUX$Cefaleia <- NA

AUX$Prostracao <- NA

AUX$Congestao_Conjuntiva <- NA

AUX$Dor_Panturrilha <- NA

AUX$Vomito <- NA

AUX$Diarreia <- NA

AUX$Ictericia <- NA

AUX$IRA <- NA

AUX$Alt_Respiratoria <- NA

AUX$Alt_Cardiaca <- NA

AUX$Hemorragia_Pulmonar <- NA

AUX$Outras_Hemorragias <- NA

AUX$Meningismo <- NA

AUX$Outros <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CLI_FEBRE == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CLI_MIALGI == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CLI_CEFALE == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CLI_PROST == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CLI_CONGES == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_PANTUR == 1) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_VOMITO == 1) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             CLI_DIARRE == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             CLI_ICTERI == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_RENAL == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_RESPIR == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 16] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_CARDIA == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 17] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_HEMOPU == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 18] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_HEMORR == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 19] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_MENING == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 20] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_OUTROS == 1) %>% 
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2023_SINTOMAS"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2023_SINTOMAS"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2023_SINTOMAS.csv"), 
           row.names = FALSE)

#####     Hospitalização      ##############

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$SIM <- NA

AUX$NAO <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ATE_HOSP == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ATE_HOSP == 2) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2023_HOSPITALIZACAO"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2023_HOSPITALIZACAO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2023_HOSPITALIZACAO.csv"), 
           row.names = FALSE)

#####     Classificação Final      ##############

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Confirmado <- NA

AUX$Descartado <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CLASSI_FIN == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CLASSI_FIN == 2) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2023_CLASSI_FIN"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2023_CLASSI_FIN"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2023_CLASSI_FIN.csv"), 
           row.names = FALSE)

#####     Critério Encerramento      ##############

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Clinico_Lab <- NA

AUX$Clinico_Epi <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CRITERIO == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CRITERIO == 2) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2023_CRIT_ENCER"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2023_CRIT_ENCER"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2023_CRIT_ENCER.csv"), 
           row.names = FALSE)

#####     LPI      ##############

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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CON_AREA == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AREA == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AREA == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AREA == 9) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4: ncol(AUX)] <- apply(AUX[, 4: ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2023_LPI"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2023_LPI"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2023_LPI.csv"), 
           row.names = FALSE)

#####     Ambiente Infecção      ##############

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Domiciliar <- NA

AUX$Trabalho <- NA

AUX$lazer <- NA

AUX$utro <- NA

AUX$Ignorado <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CON_AMBIEN == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 4) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 9) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4: ncol(AUX)] <- apply(AUX[, 4: ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2023_AMBIENTE"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2023_AMBIENTE"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2023_AMBIENTE.csv"), 
           row.names = FALSE)

#####     Ambiente Infecção      ##############

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Sim <- NA

AUX$Nao <- NA

AUX$Ignorado <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CON_AMBIEN == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 9) %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4: ncol(AUX)] <- apply(AUX[, 4: ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2023_TRABALHO"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2023_TRABALHO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2023_TRABALHO.csv"), 
           row.names = FALSE)

#####     Evolução      ##############

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Cura <- NA

AUX$Obito <- NA

AUX$Obito_Outras_Causas <- NA

AUX$Ignorado <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          EVOLUCAO == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            EVOLUCAO == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            EVOLUCAO == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2023 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            EVOLUCAO == 9) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4: ncol(AUX)] <- apply(AUX[, 4: ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2023_EVOLUCAO"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2023_EVOLUCAO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2023_EVOLUCAO.csv"), 
           row.names = FALSE)

rm(SINAN_LEPTO_2023)

##############################################################################################################################
##############################################################################################################################
#######################    Gráficos Informe    ###############################################################################

##### Criando uma função para o tema dos gráficos   #######

Tema_Graf <- function() {theme(axis.text.x = element_text(angle = 75, 
                                                          vjust = 0.5,
                                                          face = "bold"),
                               axis.text.y = element_text(angle = 90,
                                                          vjust = 0.5,
                                                          hjust = 0.5,
                                                          face = "bold"),
                               panel.grid.major = element_line(color = "#C0C0C0"),
                               panel.grid.minor = element_blank(),
                               panel.background = element_rect(fill = "#F5F5F5"),
                               plot.title = element_text(face = "bold",
                                                         size = 19,
                                                         colour = "#556B2F")) 
}

######   Zona 2023   ####

AUX <- RS22_LEPTO_2023_GERAL[- 17, c(2, 7:9)]

RS_GRAF_ZONA <- ggplot(AUX, aes(x = Município)) +
  geom_bar(aes(y = Zona_Urbana,
               fill = "Urbana"),
           stat = "identity", 
           color = "black",
           width = .3,
           position = position_nudge(x = -.30)
           ) +
  geom_label(aes(y = Zona_Urbana,
             label = Zona_Urbana),
             size = 3, 
             alpha = 0.5,
             nudge_x = -.30,
             vjust = 0.1) +
  geom_bar(aes(y = Zona_Rural,
               fill = "Rural"),
           stat = "identity",
           color = "black",
           width = .3,
           position = position_nudge(x = -.0)
           ) +
  geom_label(aes(y= Zona_Rural,
                 label = Zona_Rural),
             size =3,
             alpha = 0.5,
             nudge_x = -.00,
             vjust = 0.1) +
  geom_bar(aes(y = Zona_Periurbana,
               fill = "Periurbana"),
           stat = "identity",
           color = "black",
           width = .3,
           position = position_nudge(x = .30)
  ) +
  geom_label(aes(y= Zona_Periurbana,
                 label = Zona_Periurbana),
             size =3,
             alpha = 0.5,
             nudge_x = .30,
             vjust = 0.1) +
  scale_fill_manual(name = "", values = c("Urbana" = "#C4BF7A", 
                                          "Rural" = "#C4A37A", 
                                          "Periurbana" = "grey")) +
  labs(caption = Fonte, 
       x = NULL,
       y = "Número de Casos",
       title = paste0("ZONA DE OCORRÊNCIA/MUNICÍPIO ", RS, "ªRS - 2023")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  Tema_Graf()
  
######   Sexo/Município 2023   ####

AUX <- RS22_LEPTO_2023_GERAL[-17, c(2, 11:12)]

RS_GRAF_SEXO <- ggplot(AUX, aes(x = Município)) +
  geom_bar(aes(y = Feminino,
               fill = "Feminino"),
           color = "black",
           stat = "identity",
           width = .4,
           position = position_nudge(x = -.20)) +
  geom_label(aes(y = Feminino,
                 label = Feminino),
             size =3,
             alpha = 0.5,
             nudge_x = -.20,
             vjust = 0.1) +
  geom_bar(aes(y = Masculino,
               fill = "Masculino"),
           color = "black",
           stat = "identity",
           width = .4,
           position = position_nudge(x = .20)) +
  geom_label(aes(y = Masculino,
                 label = Masculino),
             size =3,
             alpha = 0.5,
             nudge_x = .20,
             vjust = 0.1) +
  scale_fill_manual(name = "", values = c("Feminino" = "#C4BF7A", 
                                          "Masculino" = "#C4A37A")) +
  labs(caption = Fonte, 
       x = NULL,
       y = "Número de Casos",
       title = paste0("SEXO/MUNICÍPIO ", RS, "ªRS - 2023")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  Tema_Graf()
             

#############    criando gráfico de série histórica PARANÁ    #######

AUX <- LEPTONET2023 %>% filter(NU_ANO != "2002" & 
                             NU_ANO != "2006" & 
                             NU_ANO != "2007" &
                             NU_ANO != "2008") %>% 
  count(NU_ANO)

AUX01 <- LEPTONET2023 %>% filter(NU_ANO != "2002" & 
                               NU_ANO != "2006" & 
                               NU_ANO != "2007" &
                               NU_ANO != "2008",
                             CLASSI_FIN == 1) %>% 
  count(NU_ANO)

AUX$c <- AUX01$n

PR_GRAF_Serie_Historica <- ggplot(AUX, aes(x= NU_ANO))+
  theme(axis.text.x = element_text(angle = 0, 
                                   vjust = 1,
                                   face = "bold"),
        axis.text.y = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 0.5,
                                   face = "bold"),
        panel.grid.major = element_line(color = "#C0C0C0"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#F5F5F5"),
        plot.title = element_text(face = "bold",
                                  size = 19,
                                  colour = "#556B2F")) +
  geom_bar(aes(y = n, fill = "Notificados"),
           stat = "identity",
           color = "black",
           width = .4,
           position = position_nudge(x = -.20)) +
  geom_bar(aes(y = c, fill = "Confirmados"),
           stat = "identity",
           color = "black",
           width = .4,
           position = position_nudge(x = .20)) +
  labs(caption = "Fonte", 
       x = NULL,
       y = "Número de Casos",
       title = "Série Histórica - Paraná",
       subtitle = "Casos Notificados/2009 - 2023") +
  geom_label(aes(y = AUX$n,
                 label = AUX$n), 
             size = 3, 
             alpha = 0.5,
             nudge_x = -.20,
             vjust = 0.2) +
  geom_label(aes(y = AUX$c, 
                 label = AUX$c), 
             size = 3, 
             alpha = 0.5,
             nudge_x = .20,
             vjust = 0.2) +
  scale_fill_manual(name = "", values = c("Notificados" = "#556B2F", "Confirmados" = "#FF6347")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

##################   Série histórica 22ªRS   #########

AUX <- LEPTONET2023 %>% filter(NU_ANO != "2002" & 
                             NU_ANO != "2006" & 
                             NU_ANO != "2007" &
                             NU_ANO != "2008",
                           ID_REGIONA == ID_REG) %>% 
  count(NU_ANO)

AUX01 <- LEPTONET2023 %>% filter(NU_ANO != "2002" & 
                               NU_ANO != "2006" & 
                               NU_ANO != "2007" &
                               NU_ANO != "2008",
                             CLASSI_FIN == 1,
                             ID_REGIONA == ID_REG) %>% 
  count(NU_ANO)

AUX$c <- AUX01$n

RS_GRAF_Serie_Historica <- ggplot(AUX, aes(x= NU_ANO))+
  theme(axis.text.x = element_text(angle = 0, 
                                   vjust = 1,
                                   face = "bold"),
        axis.text.y = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 0.5,
                                   face = "bold"),
        panel.grid.major = element_line(color = "#C0C0C0"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#F5F5F5"),
        plot.title = element_text(face = "bold",
                                  size = 19,
                                  colour = "#556B2F")) +
  geom_bar(aes(y = n, fill = "Notificados"),
           stat = "identity",
           color = "black",
           width = .4,
           position = position_nudge(x = -.20)) +
  geom_bar(aes(y = c, fill = "Confirmados"),
           stat = "identity",
           color = "black",
           width = .4,
           position = position_nudge(x = .20)) +
  labs(caption = "Fonte", 
       x = NULL,
       y = "Número de Casos",
       title = paste0("Série Histórica - ", RS, "ªRS"),
       subtitle = "Casos Notificados 2009 a 2023") +
  geom_label(aes(y = AUX$n,
                 label = AUX$n), 
             size = 3, 
             alpha = 0.5,
             nudge_x = -.20,
             vjust = 0.2) +
  geom_label(aes(y = AUX$c, 
                 label = AUX$c), 
             size = 3, 
             alpha = 0.5,
             nudge_x = .20,
             vjust = 0.2) +
  scale_fill_manual(name = "", values = c("Notificados" = "#556B2F", "Confirmados" = "#FF6347")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

#################   Série Histórica   zona   PARANÁ   #####

AUX <- LEPTONET2023 %>% filter(NU_ANO != "2002" & 
                             NU_ANO != "2006" & 
                             NU_ANO != "2007" &
                             NU_ANO != "2008",
                           CLASSI_FIN == 1,
                           CS_ZONA == 1) %>% 
  count(NU_ANO) 

colnames(AUX)[2] <- "Urbano"

AUX01 <- LEPTONET2023 %>% filter(NU_ANO != "2002" & 
                               NU_ANO != "2006" & 
                               NU_ANO != "2007" &
                               NU_ANO != "2008",
                             CLASSI_FIN == 1,
                             CS_ZONA == 2) %>% 
  count(NU_ANO) 

colnames(AUX01)[2] <- "Rural"

AUX_GRAF <- full_join(AUX, AUX01, by = c("NU_ANO" = "NU_ANO"))

AUX_GRAF[is.na(AUX_GRAF)] <- 0

PR_GRAF_Serie_Historica_ZONA <- ggplot(AUX_GRAF, aes(x= NU_ANO))+
  theme(axis.text.x = element_text(angle = 0, 
                                   vjust = 1,
                                   face = "bold"),
        axis.text.y = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 0.5,
                                   face = "bold"),
        panel.grid.major = element_line(color = "#C0C0C0"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#F5F5F5"),
        plot.title = element_text(face = "bold",
                                  size = 19,
                                  colour = "#556B2F")) +
  geom_bar(aes(y = Urbano, 
               fill = "Urbano"),
           stat = "identity",
           color = "black",
           width = .4,
           position = position_nudge(x = -.20)) +
  geom_bar(aes(y = Rural, 
               fill = "Rural"),
           stat = "identity",
           color = "black",
           width = .4,
           position = position_nudge(x = .20)) +
  labs(caption = "Fonte", 
       x = NULL,
       y = "Número de Casos",
       title = "Série Histórica Zona de Ocorrência - Paraná",
       subtitle = "Casos Confirmados 2009 a 2023") +
  geom_label(aes(y = AUX_GRAF$Urbano,
                 label = AUX_GRAF$Urbano), 
             size = 3, 
             alpha = 0.5,
             nudge_x = -.20,
             vjust = 0.2) +
  geom_label(aes(y = AUX_GRAF$Rural, 
                 label = AUX_GRAF$Rural), 
             size = 3, 
             alpha = 0.5,
             nudge_x = .20,
             vjust = 0.2) +
  scale_fill_manual(name = "", values = c("Urbano" = "#556B2F", 
                                          "Rural" = "#FF6347")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

#################   Série Histórica   zona  22RS #####

AUX <- LEPTONET2023 %>% filter(NU_ANO != "2002" & 
                             NU_ANO != "2006" & 
                             NU_ANO != "2007" &
                             NU_ANO != "2008",
                           ID_REGIONA == ID_REG,
                           CLASSI_FIN == 1,
                           CS_ZONA == 1) %>% 
  count(NU_ANO) 

colnames(AUX)[2] <- "Urbano"

AUX01 <- LEPTONET2023 %>% filter(NU_ANO != "2002" & 
                               NU_ANO != "2006" & 
                               NU_ANO != "2007" &
                               NU_ANO != "2008",
                             CLASSI_FIN == 1,
                             ID_REGIONA == ID_REG,
                             CS_ZONA == 2) %>% 
  count(NU_ANO) 

colnames(AUX01)[2] <- "Rural"

AUX_GRAF <- full_join(AUX, AUX01, by = c("NU_ANO" = "NU_ANO"))

AUX_GRAF[is.na(AUX_GRAF)] <- 0

RS_GRAF_Serie_Historica_ZONA <- ggplot(AUX_GRAF, aes(x= NU_ANO))+
  theme(axis.text.x = element_text(angle = 0, 
                                   vjust = 1,
                                   face = "bold"),
        axis.text.y = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 0.5,
                                   face = "bold"),
        panel.grid.major = element_line(color = "#C0C0C0"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#F5F5F5"),
        plot.title = element_text(face = "bold",
                                  size = 19,
                                  colour = "#556B2F")) +
  geom_bar(aes(y = Urbano, 
               fill = "Urbano"),
           stat = "identity",
           color = "black",
           width = .4,
           position = position_nudge(x = -.20)) +
  geom_bar(aes(y = Rural, 
               fill = "Rural"),
           stat = "identity",
           color = "black",
           width = .4,
           position = position_nudge(x = .20)) +
  labs(caption = "Fonte", 
       x = NULL,
       y = "Número de Casos",
       title = paste0("Série Histórica - ", RS, "ªRS"),
       subtitle = "Casos Confirmados 2009 a 2023") +
  geom_label(aes(y = AUX_GRAF$Urbano,
                 label = AUX_GRAF$Urbano), 
             size = 3, 
             alpha = 0.5,
             nudge_x = -.20,
             vjust = 0.2) +
  geom_label(aes(y = AUX_GRAF$Rural, 
                 label = AUX_GRAF$Rural), 
             size = 3, 
             alpha = 0.5,
             nudge_x = .20,
             vjust = 0.2) +
  scale_fill_manual(name = "", values = c("Urbano" = "#556B2F", "Rural" = "#FF6347")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

#################   Série Histórica Sexo 22RS   #####

AUX <- LEPTONET2023 %>% filter(NU_ANO != "2002" & 
                             NU_ANO != "2006" & 
                             NU_ANO != "2007" &
                             NU_ANO != "2008",
                           ID_REGIONA == ID_REG,
                           CLASSI_FIN == 1,
                           CS_SEXO == "F") %>% 
  count(NU_ANO) 

colnames(AUX)[2] <- "Feminino"

AUX01 <- LEPTONET2023 %>% filter(NU_ANO != "2002" & 
                               NU_ANO != "2006" & 
                               NU_ANO != "2007" &
                               NU_ANO != "2008",
                             CLASSI_FIN == 1,
                             ID_REGIONA == ID_REG,
                             CS_SEXO == "M") %>% 
  count(NU_ANO) 

colnames(AUX01)[2] <- "Masculino"

AUX_GRAF <- full_join(AUX, AUX01, by = c("NU_ANO" = "NU_ANO"))

AUX_GRAF[is.na(AUX_GRAF)] <- 0

RS_GRAF_Serie_Historica_SEXO <-ggplot(AUX_GRAF, aes(x= NU_ANO))+
  theme(axis.text.x = element_text(angle = 0, 
                                   vjust = 1,
                                   face = "bold"),
        axis.text.y = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 0.5,
                                   face = "bold"),
        panel.grid.major = element_line(color = "#C0C0C0"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#F5F5F5"),
        plot.title = element_text(face = "bold",
                                  size = 19,
                                  colour = "#556B2F")) +
  geom_bar(aes(y = Feminino, 
               fill = "Feminino"),
           stat = "identity",
           color = "black",
           width = .4,
           position = position_nudge(x = -.20)) +
  geom_bar(aes(y = Masculino, 
               fill = "Masculino"),
           stat = "identity",
           color = "black",
           width = .4,
           position = position_nudge(x = .20)) +
  labs(caption = "Fonte", 
       x = NULL,
       y = "Número de Casos",
       title = paste0("Série Histórica -", RS, "ªRS"),
       subtitle = "Casos Confirmados 2009 a 2023") +
  geom_label(aes(y = AUX_GRAF$Feminino,
                 label = AUX_GRAF$Feminino), 
             size = 3, 
             alpha = 0.5,
             nudge_x = -.20,
             vjust = 0.2) +
  geom_label(aes(y = AUX_GRAF$Masculino, 
                 label = AUX_GRAF$Masculino), 
             size = 3, 
             alpha = 0.5,
             nudge_x = .20,
             vjust = 0.2) +
  scale_fill_manual(name = "", values = c("Feminino" = "#556B2F", "Masculino" = "#FF6347")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

#################   Série Histórica Sexo PARANÁ   #####

AUX <- LEPTONET2023 %>% filter(NU_ANO != "2002" & 
                             NU_ANO != "2006" & 
                             NU_ANO != "2007" &
                             NU_ANO != "2008",
                           CLASSI_FIN == 1,
                           CS_SEXO == "F") %>% 
  count(NU_ANO) 

colnames(AUX)[2] <- "Feminino"

AUX01 <- LEPTONET2023 %>% filter(NU_ANO != "2002" & 
                               NU_ANO != "2006" & 
                               NU_ANO != "2007" &
                               NU_ANO != "2008",
                             CLASSI_FIN == 1,
                             CS_SEXO == "M") %>% 
  count(NU_ANO) 

colnames(AUX01)[2] <- "Masculino"

AUX_GRAF <- full_join(AUX, AUX01, by = c("NU_ANO" = "NU_ANO"))

AUX_GRAF[is.na(AUX_GRAF)] <- 0

PR_GRAF_Serie_Historica_SEXO <- ggplot(AUX_GRAF, aes(x= NU_ANO))+
  theme(axis.text.x = element_text(angle = 0, 
                                   vjust = 1,
                                   face = "bold"),
        axis.text.y = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 0.5,
                                   face = "bold"),
        panel.grid.major = element_line(color = "#C0C0C0"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#F5F5F5"),
        plot.title = element_text(face = "bold",
                                  size = 19,
                                  colour = "#556B2F")) +
  geom_bar(aes(y = Feminino, 
               fill = "Feminino"),
           stat = "identity",
           color = "black",
           width = .4,
           position = position_nudge(x = -.20)) +
  geom_bar(aes(y = Masculino, 
               fill = "Masculino"),
           stat = "identity",
           color = "black",
           width = .4,
           position = position_nudge(x = .20)) +
  labs(caption = "Fonte", 
       x = NULL,
       y = "Número de Casos",
       title = "Série Histórica - Paraná",
       subtitle = "Casos Confirmados 2009 a 2023") +
  geom_label(aes(y = AUX_GRAF$Feminino,
                 label = AUX_GRAF$Feminino), 
             size = 3, 
             alpha = 0.5,
             nudge_x = -.20,
             vjust = 0.2) +
  geom_label(aes(y = AUX_GRAF$Masculino, 
                 label = AUX_GRAF$Masculino), 
             size = 3, 
             alpha = 0.5,
             nudge_x = .20,
             vjust = 0.2) +
  scale_fill_manual(name = "", values = c("Feminino" = "#556B2F", "Masculino" = "#FF6347")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

###      Tabela faixa etária Paraná       ###

AUX <- LEPTONET2023 %>% filter(NU_ANO != "2002" & 
                             NU_ANO != "2006" & 
                             NU_ANO != "2007" &
                             NU_ANO != "2008",
                           CLASSI_FIN == 1,
                           NU_IDADE_N <=3012) %>% 
  count(NU_ANO) 

colnames(AUX)[2] <- "Menor_1_ano"

AUX01 <- LEPTONET2023 %>% filter(NU_ANO != "2002" & 
                               NU_ANO != "2006" & 
                               NU_ANO != "2007" &
                               NU_ANO != "2008",
                             CLASSI_FIN == 1,  
                             NU_IDADE_N >= 4000 
                             & 
                               NU_IDADE_N <4005) %>% 
  count(NU_ANO) 

colnames(AUX01)[2] <- "1_a_5_anos"

AUX02 <- LEPTONET2023 %>% filter(NU_ANO != "2002" & 
                               NU_ANO != "2006" & 
                               NU_ANO != "2007" &
                               NU_ANO != "2008",
                             CLASSI_FIN == 1,  
                             NU_IDADE_N >= 4005 
                             & 
                               NU_IDADE_N < 4012) %>% 
  count(NU_ANO) 

colnames(AUX02)[2] <- "5_a_12_anos"

AUX03 <- LEPTONET2023 %>% filter(NU_ANO != "2002" & 
                               NU_ANO != "2006" & 
                               NU_ANO != "2007" &
                               NU_ANO != "2008",
                             CLASSI_FIN == 1,  
                             NU_IDADE_N >= 4012 
                             & 
                               NU_IDADE_N < 4018) %>% 
  count(NU_ANO) 

colnames(AUX03)[2] <- "12_a_18_anos"

AUX04 <- LEPTONET2023 %>% filter(NU_ANO != "2002" & 
                               NU_ANO != "2006" & 
                               NU_ANO != "2007" &
                               NU_ANO != "2008",
                             CLASSI_FIN == 1,  
                             NU_IDADE_N >= 4018 
                             & 
                               NU_IDADE_N < 4059) %>% 
  count(NU_ANO) 

colnames(AUX04)[2] <- "18_a_59_anos"

AUX05 <- LEPTONET2023 %>% filter(NU_ANO != "2002" & 
                               NU_ANO != "2006" & 
                               NU_ANO != "2007" &
                               NU_ANO != "2008",
                             CLASSI_FIN == 1,  
                             NU_IDADE_N >= 4059) %>% 
  count(NU_ANO) 

colnames(AUX05)[2] <- "mais_59_anos"

AUX_GRAF <- full_join(AUX, AUX01, by = c("NU_ANO" = "NU_ANO"))

AUX_GRAF <- full_join(AUX_GRAF, AUX02, by = c("NU_ANO" = "NU_ANO")) 

AUX_GRAF <- full_join(AUX_GRAF, AUX03, by = c("NU_ANO" = "NU_ANO")) 

AUX_GRAF <- full_join(AUX_GRAF, AUX04, by = c("NU_ANO" = "NU_ANO")) 

AUX_GRAF <- full_join(AUX_GRAF, AUX05, by = c("NU_ANO" = "NU_ANO")) 

AUX_GRAF <- t(AUX_GRAF)

