rm(list =ls())

####Indicando Diretório de Trabalho.#####

setwd("/home/gustavo/Área de Trabalho/Análise_de_Dados/")

###Libraries###

library(foreign)
library (dplyr)
library(ggplot2)

####Planilha com os dados dos municípios e com os códigos do IBGE. Será utilizada nos for loops para buscar dados######## 
####dos municípios e vinculá-los com os dados da base DBF do SINAN#######################################################

BASE_IBGE<-read.csv(file="Base_de_Dados/Auxiliares/Planilha_Base_IBGE.csv", 
                    header=TRUE, 
                    sep=",")

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

####Criando um objeto com a base DBF do SINAN#################

LEPTONET<- read.dbf(file = "Base_de_Dados/DBF/LEPTONET.DBF",
                       as.is = FALSE)

##################################################################################
#################   2009    ######################################################
##################################################################################

SINAN_LEPTO_2009 <- LEPTONET %>% filter(ID_REGIONA == ID_REG | ID_RG_RESI == ID_REG,
                                        NU_ANO == 2009)

assign(paste0("RS", RS, "_LEPTO_2009_SINAN"), SINAN_LEPTO_2009)

write.csv (assign(paste0("RS", RS, "_LEPTO_2009_SINAN"), SINAN_LEPTO_2009), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2009_SINAN.csv"), 
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
  
  AUX[which(AUX == i), 2] <- as.integer(SINAN_LEPTO_2009 %>%
                                          filter(ID_MN_RESI ==i &
                                                 SEM_PRI ==200901)%>%
                                          count()
  )
  
  AUX[which(AUX == i), 3] <- as.integer(SINAN_LEPTO_2009 %>% 
                                          filter(ID_MN_RESI ==i & 
                                                 SEM_PRI ==200902) %>% 
                                          count()
  )
  
  AUX[which(AUX == i), 4] <- as.integer(SINAN_LEPTO_2009 %>% 
                                          filter(ID_MN_RESI ==i &
                                                 SEM_PRI ==200903) %>% 
                                          count()
  )
  
  AUX[which(AUX == i),5] <- as.integer(SINAN_LEPTO_2009 %>% 
                                         filter(ID_MN_RESI ==i &
                                                SEM_PRI ==200904) %>% 
                                         count()
  )
  
  AUX[which(AUX == i), 6] <- as.integer(SINAN_LEPTO_2009 %>% 
                                          filter(ID_MN_RESI ==i &
                                                 SEM_PRI ==200905) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 7] <- as.integer(SINAN_LEPTO_2009 %>%
                                          filter(ID_MN_RESI ==i & 
                                                 SEM_PRI ==200906) %>%
                                          count()
  )
  
  AUX[which(AUX == i), 8] <- as.integer(SINAN_LEPTO_2009 %>%
                                          filter(ID_MN_RESI ==i & 
                                                 SEM_PRI ==200907) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 9] <- as.integer(SINAN_LEPTO_2009 %>%
                                          filter(ID_MN_RESI ==i & 
                                                 SEM_PRI ==200908) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 10] <- as.integer(SINAN_LEPTO_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                  SEM_PRI ==200909) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 11] <- as.integer(SINAN_LEPTO_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                  SEM_PRI ==200910) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 12] <- as.integer(SINAN_LEPTO_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==200911) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 13] <- as.integer(SINAN_LEPTO_2009 %>% 
                                           filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==200912) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 14] <- as.integer(SINAN_LEPTO_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                  SEM_PRI ==200913) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 15] <- as.integer(SINAN_LEPTO_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                  SEM_PRI ==200914) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 16] <- as.integer(SINAN_LEPTO_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                  SEM_PRI ==200915) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 17] <- as.integer(SINAN_LEPTO_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                  SEM_PRI ==200916) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 18] <- as.integer(SINAN_LEPTO_2009 %>%
                                           filter(ID_MN_RESI ==i & 
                                                  SEM_PRI ==200917) %>%  
                                           count() 
  )
  
  AUX[which(AUX == i), 19] <- as.integer(SINAN_LEPTO_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                  SEM_PRI ==200918) %>%      
                                           count() 
  )
  
  AUX[which(AUX == i), 20] <- as.integer(SINAN_LEPTO_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                  SEM_PRI ==200919) %>%
                                           count() 
  )
  
  AUX[which(AUX == i),  21] <- as.integer(SINAN_LEPTO_2009 %>% 
                                            filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==200920) %>%
                                            count() 
  )
  
  AUX[which(AUX == i), 22] <- as.integer(SINAN_LEPTO_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                  SEM_PRI ==200921) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 23] <- as.integer(SINAN_LEPTO_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                  SEM_PRI ==200922) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 24] <- as.integer(SINAN_LEPTO_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                  SEM_PRI ==200923) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 25] <- as.integer(SINAN_LEPTO_2009 %>%
                                           filter(ID_MN_RESI ==i & 
                                                  SEM_PRI ==200924) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 26] <- as.integer(SINAN_LEPTO_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                  SEM_PRI ==200925) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 27] <- as.integer(SINAN_LEPTO_2009 %>%
                                           filter(ID_MN_RESI ==i & 
                                                  SEM_PRI ==200926) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 28] <- as.integer(SINAN_LEPTO_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                  SEM_PRI ==200927) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 29] <- as.integer(SINAN_LEPTO_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                  SEM_PRI ==200928) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 30] <- as.integer(SINAN_LEPTO_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==200929) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 31] <- as.integer(SINAN_LEPTO_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==200930) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 32] <- as.integer(SINAN_LEPTO_2009 %>% 
                                           filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==200931) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 33] <- as.integer(SINAN_LEPTO_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==200932) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 34] <- as.integer(SINAN_LEPTO_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==200933) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 35] <- as.integer(SINAN_LEPTO_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==200934) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 36] <- as.integer(SINAN_LEPTO_2009 %>% 
                                           filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==200935) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 37] <- as.integer(SINAN_LEPTO_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==200936) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 38] <- as.integer(SINAN_LEPTO_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==200937) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 39] <- as.integer(SINAN_LEPTO_2009 %>% 
                                           filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==200938) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 40] <- as.integer(SINAN_LEPTO_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==200939) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 41] <- as.integer(SINAN_LEPTO_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==200940) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 42] <- as.integer(SINAN_LEPTO_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                  SEM_PRI ==200941) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 43] <- as.integer(SINAN_LEPTO_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                  SEM_PRI ==200942) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 44] <- as.integer(SINAN_LEPTO_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==200943) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 45] <- as.integer(SINAN_LEPTO_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==200944) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 46] <- as.integer(SINAN_LEPTO_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==200945) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 47] <- as.integer(SINAN_LEPTO_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==200946) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 48] <- as.integer(SINAN_LEPTO_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==200947) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 49] <- as.integer(SINAN_LEPTO_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==200948) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 50] <- as.integer(SINAN_LEPTO_2009 %>%
                                           filter(ID_MN_RESI ==i & 
                                                  SEM_PRI ==200949) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 51] <- as.integer(SINAN_LEPTO_2009 %>% 
                                           filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==200950) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 52] <- as.integer(SINAN_LEPTO_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==200951) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 53] <- as.integer(SINAN_LEPTO_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==200952) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 54] <- as.integer(SINAN_LEPTO_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                  SEM_PRI ==200953) %>%
                                           count() 
  )
}

AUX[, 1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX[(nrow(AUX)+ 1), 2:54] <- apply(AUX[, 2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2009_SE_Notificados"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2009_SE_Notificados"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2009_SE_Notificados.csv"), 
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
  
  AUX[which(AUX == i), 2] <- as.integer(SINAN_LEPTO_2009 %>%
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==200901,
                                                 CLASSI_FIN == 1)%>%
                                          count()
  )
  
  AUX[which(AUX == i), 3] <- as.integer(SINAN_LEPTO_2009 %>% 
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==200902,
                                                 CLASSI_FIN == 1) %>% 
                                          count()
  )
  
  AUX[which(AUX == i), 4] <- as.integer(SINAN_LEPTO_2009 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==200903,
                                                 CLASSI_FIN == 1) %>% 
                                          count()
  )
  
  AUX[which(AUX == i),5] <- as.integer(SINAN_LEPTO_2009 %>% 
                                         filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==200904,
                                                CLASSI_FIN == 1) %>% 
                                         count()
  )
  
  AUX[which(AUX == i), 6] <- as.integer(SINAN_LEPTO_2009 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==200905,
                                                 CLASSI_FIN == 1) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 7] <- as.integer(SINAN_LEPTO_2009 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==200906,
                                                 CLASSI_FIN == 1) %>%
                                          count()
  )
  
  AUX[which(AUX == i), 8] <- as.integer(SINAN_LEPTO_2009 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==200907,
                                                 CLASSI_FIN == 1) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 9] <- as.integer(SINAN_LEPTO_2009 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==200908,
                                                 CLASSI_FIN == 1) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 10] <- as.integer(SINAN_LEPTO_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200909,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 11] <- as.integer(SINAN_LEPTO_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200910,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 12] <- as.integer(SINAN_LEPTO_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200911,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 13] <- as.integer(SINAN_LEPTO_2009 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200912,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 14] <- as.integer(SINAN_LEPTO_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200913,
                                                  CLASSI_FIN == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 15] <- as.integer(SINAN_LEPTO_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200914,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 16] <- as.integer(SINAN_LEPTO_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200915,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 17] <- as.integer(SINAN_LEPTO_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200916,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 18] <- as.integer(SINAN_LEPTO_2009 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200917,
                                                  CLASSI_FIN == 1) %>%  
                                           count() 
  )
  
  AUX[which(AUX == i), 19] <- as.integer(SINAN_LEPTO_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200918,
                                                  CLASSI_FIN == 1) %>%      
                                           count() 
  )
  
  AUX[which(AUX == i), 20] <- as.integer(SINAN_LEPTO_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200919,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i),  21] <- as.integer(SINAN_LEPTO_2009 %>% 
                                            filter(ID_MN_RESI ==i & 
                                                     SEM_PRI ==200920,
                                                   CLASSI_FIN == 1) %>%
                                            count() 
  )
  
  AUX[which(AUX == i), 22] <- as.integer(SINAN_LEPTO_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200921,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 23] <- as.integer(SINAN_LEPTO_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200922,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 24] <- as.integer(SINAN_LEPTO_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200923,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 25] <- as.integer(SINAN_LEPTO_2009 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200924,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 26] <- as.integer(SINAN_LEPTO_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200925,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 27] <- as.integer(SINAN_LEPTO_2009 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200926,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 28] <- as.integer(SINAN_LEPTO_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200927,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 29] <- as.integer(SINAN_LEPTO_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200928,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 30] <- as.integer(SINAN_LEPTO_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200929,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 31] <- as.integer(SINAN_LEPTO_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200930,
                                                  CLASSI_FIN == 1) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 32] <- as.integer(SINAN_LEPTO_2009 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200931,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 33] <- as.integer(SINAN_LEPTO_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200932,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 34] <- as.integer(SINAN_LEPTO_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200933,
                                                  CLASSI_FIN == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 35] <- as.integer(SINAN_LEPTO_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200934,
                                                  CLASSI_FIN == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 36] <- as.integer(SINAN_LEPTO_2009 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200935,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 37] <- as.integer(SINAN_LEPTO_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200936,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 38] <- as.integer(SINAN_LEPTO_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200937,
                                                  CLASSI_FIN == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 39] <- as.integer(SINAN_LEPTO_2009 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200938,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 40] <- as.integer(SINAN_LEPTO_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200939,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 41] <- as.integer(SINAN_LEPTO_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200940,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 42] <- as.integer(SINAN_LEPTO_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200941,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 43] <- as.integer(SINAN_LEPTO_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200942,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 44] <- as.integer(SINAN_LEPTO_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200943,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 45] <- as.integer(SINAN_LEPTO_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200944,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 46] <- as.integer(SINAN_LEPTO_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200945,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 47] <- as.integer(SINAN_LEPTO_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200946,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 48] <- as.integer(SINAN_LEPTO_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200947,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 49] <- as.integer(SINAN_LEPTO_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200948,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 50] <- as.integer(SINAN_LEPTO_2009 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200949,
                                                  CLASSI_FIN == 1) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 51] <- as.integer(SINAN_LEPTO_2009 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200950,
                                                  CLASSI_FIN == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 52] <- as.integer(SINAN_LEPTO_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200951,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 53] <- as.integer(SINAN_LEPTO_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200952,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 54] <- as.integer(SINAN_LEPTO_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200953,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
}

AUX[, 1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX[(nrow(AUX)+ 1), 2:54] <- apply(AUX[, 2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2009_SE_Confirmados"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2009_SE_Confirmados"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2009_SE_Confirmados.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                   filter(ID_MN_RESI == i) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CLASSI_FIN == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                          CS_ZONA == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                          CS_ZONA == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                          CS_ZONA == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                          CS_ZONA == 9 
                                                          |
                                                            is.na(CS_ZONA)) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                           CS_SEXO == "F") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                           CS_SEXO == "M") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                           NU_IDADE_N <= 3012) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                           NU_IDADE_N > 4000 
                                                           & 
                                                             NU_IDADE_N <= 4005) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                           NU_IDADE_N > 4005 
                                                           & 
                                                             NU_IDADE_N <= 4012) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 16] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                           NU_IDADE_N > 4012 
                                                           & 
                                                             NU_IDADE_N <= 4018) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 17] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                           NU_IDADE_N > 4018 
                                                           & 
                                                             NU_IDADE_N <= 4059) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 18] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                           NU_IDADE_N > 4059) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 19] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                           CS_ESCOL_N == "00") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 20] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                           CS_ESCOL_N == "01" 
                                                           | 
                                                             CS_ESCOL_N == "02" 
                                                           | 
                                                             CS_ESCOL_N == "03") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 21] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                           CS_ESCOL_N == "04") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 22] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                           CS_ESCOL_N == "05") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 23] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                           CS_ESCOL_N == "06") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 24] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                           CS_ESCOL_N == "07") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 25] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                           CS_ESCOL_N == "08") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 26] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                           CS_ESCOL_N == "10") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 27] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                           CS_ESCOL_N == "09") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 28] <- as.integer(SINAN_LEPTO_2009 %>% 
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
  
  AUX[which(AUX$COD_IBGE == i), 29] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                           CS_GESTANT == "5") %>% 
                                                    count()
  )
  AUX[which(AUX$COD_IBGE == i), 30] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                           CS_GESTANT == "6") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 31] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                           CS_GESTANT == "9") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 32] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                           CS_RACA == "01") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 33] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                           CS_RACA == "2") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 34] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                           CS_RACA == "3") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 35] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                           CS_RACA == "4") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 36] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                           CS_RACA == "5") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 37] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                           CS_RACA == "9") %>% 
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2009_GERAL"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2009_GERAL"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2009_GERAL.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_CB_LAM == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_CB_CRI == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_CB_CAI == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_CB_FOS == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_CB_SIN == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_CB_PLA == 1) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_CB_COR == 1) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             ANT_CB_ROE == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             ANT_CB_GRA == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_CB_TER == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_CB_LIX == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 16] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_CB_OUT == 4018) %>% 
                                                    count()
  )
  }

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2009_SIT_RISCO"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2009_SIT_RISCO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2009_SIT_RISCO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CLI_FEBRE == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CLI_MIALGI == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CLI_CEFALE == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CLI_PROST == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CLI_CONGES == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_PANTUR == 1) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_VOMITO == 1) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             CLI_DIARRE == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             CLI_ICTERI == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_RENAL == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_RESPIR == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 16] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_CARDIA == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 17] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_HEMOPU == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 18] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_HEMORR == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 19] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_MENING == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 20] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_OUTROS == 1) %>% 
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2009_SINTOMAS"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2009_SINTOMAS"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2009_SINTOMAS.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ATE_HOSP == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ATE_HOSP == 2) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2009_HOSPITALIZACAO"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2009_HOSPITALIZACAO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2009_HOSPITALIZACAO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CLASSI_FIN == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CLASSI_FIN == 2) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2009_CLASSI_FIN"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2009_CLASSI_FIN"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2009_CLASSI_FIN.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CRITERIO == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CRITERIO == 2) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2009_CRIT_ENCER"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2009_CRIT_ENCER"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2009_CRIT_ENCER.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CON_AREA == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AREA == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AREA == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AREA == 9) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4: ncol(AUX)] <- apply(AUX[, 4: ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2009_LPI"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2009_LPI"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2009_LPI.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CON_AMBIEN == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 4) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 9) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4: ncol(AUX)] <- apply(AUX[, 4: ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2009_AMBIENTE"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2009_AMBIENTE"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2009_AMBIENTE.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CON_AMBIEN == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 9) %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4: ncol(AUX)] <- apply(AUX[, 4: ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2009_TRABALHO"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2009_TRABALHO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2009_TRABALHO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          EVOLUCAO == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            EVOLUCAO == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            EVOLUCAO == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            EVOLUCAO == 9) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4: ncol(AUX)] <- apply(AUX[, 4: ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2009_EVOLUCAO"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2009_EVOLUCAO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2009_EVOLUCAO.csv"), 
           row.names = FALSE)

rm(SINAN_LEPTO_2009)


##################################################################################
#################   2010    ######################################################
##################################################################################

SINAN_LEPTO_2010 <- LEPTONET %>% filter(ID_REGIONA == ID_REG | ID_RG_RESI == ID_REG,
                                        NU_ANO == 2010)

assign(paste0("RS", RS, "_LEPTO_2010_SINAN"), SINAN_LEPTO_2010)

write.csv (assign(paste0("RS", RS, "_LEPTO_2010_SINAN"), SINAN_LEPTO_2010), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2010_SINAN.csv"), 
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
  
  AUX[which(AUX == i), 2] <- as.integer(SINAN_LEPTO_2010 %>%
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==201001)%>%
                                          count()
  )
  
  AUX[which(AUX == i), 3] <- as.integer(SINAN_LEPTO_2010 %>% 
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==201002) %>% 
                                          count()
  )
  
  AUX[which(AUX == i), 4] <- as.integer(SINAN_LEPTO_2010 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==201003) %>% 
                                          count()
  )
  
  AUX[which(AUX == i),5] <- as.integer(SINAN_LEPTO_2010 %>% 
                                         filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==201004) %>% 
                                         count()
  )
  
  AUX[which(AUX == i), 6] <- as.integer(SINAN_LEPTO_2010 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==201005) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 7] <- as.integer(SINAN_LEPTO_2010 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==201006) %>%
                                          count()
  )
  
  AUX[which(AUX == i), 8] <- as.integer(SINAN_LEPTO_2010 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==201007) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 9] <- as.integer(SINAN_LEPTO_2010 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==201008) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 10] <- as.integer(SINAN_LEPTO_2010 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201009) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 11] <- as.integer(SINAN_LEPTO_2010 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201010) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 12] <- as.integer(SINAN_LEPTO_2010 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201011) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 13] <- as.integer(SINAN_LEPTO_2010 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201012) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 14] <- as.integer(SINAN_LEPTO_2010 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201013) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 15] <- as.integer(SINAN_LEPTO_2010 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201014) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 16] <- as.integer(SINAN_LEPTO_2010 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201015) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 17] <- as.integer(SINAN_LEPTO_2010 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201016) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 18] <- as.integer(SINAN_LEPTO_2010 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201017) %>%  
                                           count() 
  )
  
  AUX[which(AUX == i), 19] <- as.integer(SINAN_LEPTO_2010 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201018) %>%      
                                           count() 
  )
  
  AUX[which(AUX == i), 20] <- as.integer(SINAN_LEPTO_2010 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201019) %>%
                                           count() 
  )
  
  AUX[which(AUX == i),  21] <- as.integer(SINAN_LEPTO_2010 %>% 
                                            filter(ID_MN_RESI ==i & 
                                                     SEM_PRI ==201020) %>%
                                            count() 
  )
  
  AUX[which(AUX == i), 22] <- as.integer(SINAN_LEPTO_2010 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201021) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 23] <- as.integer(SINAN_LEPTO_2010 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201022) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 24] <- as.integer(SINAN_LEPTO_2010 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201023) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 25] <- as.integer(SINAN_LEPTO_2010 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201024) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 26] <- as.integer(SINAN_LEPTO_2010 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201025) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 27] <- as.integer(SINAN_LEPTO_2010 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201026) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 28] <- as.integer(SINAN_LEPTO_2010 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201027) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 29] <- as.integer(SINAN_LEPTO_2010 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201028) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 30] <- as.integer(SINAN_LEPTO_2010 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201029) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 31] <- as.integer(SINAN_LEPTO_2010 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201030) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 32] <- as.integer(SINAN_LEPTO_2010 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201031) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 33] <- as.integer(SINAN_LEPTO_2010 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201032) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 34] <- as.integer(SINAN_LEPTO_2010 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201033) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 35] <- as.integer(SINAN_LEPTO_2010 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201034) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 36] <- as.integer(SINAN_LEPTO_2010 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201035) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 37] <- as.integer(SINAN_LEPTO_2010 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201036) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 38] <- as.integer(SINAN_LEPTO_2010 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201037) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 39] <- as.integer(SINAN_LEPTO_2010 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201038) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 40] <- as.integer(SINAN_LEPTO_2010 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201039) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 41] <- as.integer(SINAN_LEPTO_2010 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201040) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 42] <- as.integer(SINAN_LEPTO_2010 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201041) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 43] <- as.integer(SINAN_LEPTO_2010 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201042) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 44] <- as.integer(SINAN_LEPTO_2010 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201043) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 45] <- as.integer(SINAN_LEPTO_2010 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201044) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 46] <- as.integer(SINAN_LEPTO_2010 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201045) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 47] <- as.integer(SINAN_LEPTO_2010 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201046) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 48] <- as.integer(SINAN_LEPTO_2010 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201047) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 49] <- as.integer(SINAN_LEPTO_2010 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201048) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 50] <- as.integer(SINAN_LEPTO_2010 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201049) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 51] <- as.integer(SINAN_LEPTO_2010 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201050) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 52] <- as.integer(SINAN_LEPTO_2010 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201051) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 53] <- as.integer(SINAN_LEPTO_2010 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201052) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 54] <- as.integer(SINAN_LEPTO_2010 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201053) %>%
                                           count() 
  )
}

AUX[, 1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX[(nrow(AUX)+ 1), 2:54] <- apply(AUX[, 2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2010_SE_Notificados"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2010_SE_Notificados"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2010_SE_Notificados.csv"), 
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
  
  AUX[which(AUX == i), 2] <- as.integer(SINAN_LEPTO_2010 %>%
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==201001,
                                                 CLASSI_FIN == 1)%>%
                                          count()
  )
  
  AUX[which(AUX == i), 3] <- as.integer(SINAN_LEPTO_2010 %>% 
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==201002,
                                                 CLASSI_FIN == 1) %>% 
                                          count()
  )
  
  AUX[which(AUX == i), 4] <- as.integer(SINAN_LEPTO_2010 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==201003,
                                                 CLASSI_FIN == 1) %>% 
                                          count()
  )
  
  AUX[which(AUX == i),5] <- as.integer(SINAN_LEPTO_2010 %>% 
                                         filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==201004,
                                                CLASSI_FIN == 1) %>% 
                                         count()
  )
  
  AUX[which(AUX == i), 6] <- as.integer(SINAN_LEPTO_2010 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==201005,
                                                 CLASSI_FIN == 1) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 7] <- as.integer(SINAN_LEPTO_2010 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==201006,
                                                 CLASSI_FIN == 1) %>%
                                          count()
  )
  
  AUX[which(AUX == i), 8] <- as.integer(SINAN_LEPTO_2010 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==201007,
                                                 CLASSI_FIN == 1) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 9] <- as.integer(SINAN_LEPTO_2010 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==201008,
                                                 CLASSI_FIN == 1) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 10] <- as.integer(SINAN_LEPTO_2010 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201009,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 11] <- as.integer(SINAN_LEPTO_2010 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201010,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 12] <- as.integer(SINAN_LEPTO_2010 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201011,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 13] <- as.integer(SINAN_LEPTO_2010 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201012,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 14] <- as.integer(SINAN_LEPTO_2010 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201013,
                                                  CLASSI_FIN == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 15] <- as.integer(SINAN_LEPTO_2010 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201014,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 16] <- as.integer(SINAN_LEPTO_2010 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201015,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 17] <- as.integer(SINAN_LEPTO_2010 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201016,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 18] <- as.integer(SINAN_LEPTO_2010 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201017,
                                                  CLASSI_FIN == 1) %>%  
                                           count() 
  )
  
  AUX[which(AUX == i), 19] <- as.integer(SINAN_LEPTO_2010 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201018,
                                                  CLASSI_FIN == 1) %>%      
                                           count() 
  )
  
  AUX[which(AUX == i), 20] <- as.integer(SINAN_LEPTO_2010 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201019,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i),  21] <- as.integer(SINAN_LEPTO_2010 %>% 
                                            filter(ID_MN_RESI ==i & 
                                                     SEM_PRI ==201020,
                                                   CLASSI_FIN == 1) %>%
                                            count() 
  )
  
  AUX[which(AUX == i), 22] <- as.integer(SINAN_LEPTO_2010 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201021,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 23] <- as.integer(SINAN_LEPTO_2010 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201022,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 24] <- as.integer(SINAN_LEPTO_2010 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201023,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 25] <- as.integer(SINAN_LEPTO_2010 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201024,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 26] <- as.integer(SINAN_LEPTO_2010 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201025,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 27] <- as.integer(SINAN_LEPTO_2010 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201026,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 28] <- as.integer(SINAN_LEPTO_2010 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201027,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 29] <- as.integer(SINAN_LEPTO_2010 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201028,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 30] <- as.integer(SINAN_LEPTO_2010 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201029,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 31] <- as.integer(SINAN_LEPTO_2010 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201030,
                                                  CLASSI_FIN == 1) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 32] <- as.integer(SINAN_LEPTO_2010 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201031,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 33] <- as.integer(SINAN_LEPTO_2010 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201032,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 34] <- as.integer(SINAN_LEPTO_2010 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201033,
                                                  CLASSI_FIN == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 35] <- as.integer(SINAN_LEPTO_2010 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201034,
                                                  CLASSI_FIN == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 36] <- as.integer(SINAN_LEPTO_2010 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201035,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 37] <- as.integer(SINAN_LEPTO_2010 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201036,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 38] <- as.integer(SINAN_LEPTO_2010 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201037,
                                                  CLASSI_FIN == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 39] <- as.integer(SINAN_LEPTO_2010 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201038,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 40] <- as.integer(SINAN_LEPTO_2010 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201039,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 41] <- as.integer(SINAN_LEPTO_2010 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201040,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 42] <- as.integer(SINAN_LEPTO_2010 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201041,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 43] <- as.integer(SINAN_LEPTO_2010 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201042,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 44] <- as.integer(SINAN_LEPTO_2010 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201043,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 45] <- as.integer(SINAN_LEPTO_2010 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201044,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 46] <- as.integer(SINAN_LEPTO_2010 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201045,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 47] <- as.integer(SINAN_LEPTO_2010 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201046,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 48] <- as.integer(SINAN_LEPTO_2010 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201047,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 49] <- as.integer(SINAN_LEPTO_2010 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201048,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 50] <- as.integer(SINAN_LEPTO_2010 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201049,
                                                  CLASSI_FIN == 1) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 51] <- as.integer(SINAN_LEPTO_2010 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201050,
                                                  CLASSI_FIN == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 52] <- as.integer(SINAN_LEPTO_2010 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201051,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 53] <- as.integer(SINAN_LEPTO_2010 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201052,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 54] <- as.integer(SINAN_LEPTO_2010 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201053,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
}

AUX[, 1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX[(nrow(AUX)+ 1), 2:54] <- apply(AUX[, 2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2010_SE_Confirmados"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2010_SE_Confirmados"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2010_SE_Confirmados.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2010 %>% 
                                                   filter(ID_MN_RESI == i) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2010 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CLASSI_FIN == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2010 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CS_ZONA == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2010 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CS_ZONA == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_LEPTO_2010 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CS_ZONA == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_LEPTO_2010 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ZONA == 9 
                                                           |
                                                             is.na(CS_ZONA)) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_LEPTO_2010 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_SEXO == "F") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(SINAN_LEPTO_2010 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_SEXO == "M") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_LEPTO_2010 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             NU_IDADE_N <= 3012) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(SINAN_LEPTO_2010 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             NU_IDADE_N > 4000 
                                                           & 
                                                             NU_IDADE_N <= 4005) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_LEPTO_2010 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             NU_IDADE_N > 4005 
                                                           & 
                                                             NU_IDADE_N <= 4012) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 16] <- as.integer(SINAN_LEPTO_2010 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             NU_IDADE_N > 4012 
                                                           & 
                                                             NU_IDADE_N <= 4018) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 17] <- as.integer(SINAN_LEPTO_2010 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             NU_IDADE_N > 4018 
                                                           & 
                                                             NU_IDADE_N <= 4059) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 18] <- as.integer(SINAN_LEPTO_2010 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             NU_IDADE_N > 4059) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 19] <- as.integer(SINAN_LEPTO_2010 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "00") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 20] <- as.integer(SINAN_LEPTO_2010 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "01" 
                                                           | 
                                                             CS_ESCOL_N == "02" 
                                                           | 
                                                             CS_ESCOL_N == "03") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 21] <- as.integer(SINAN_LEPTO_2010 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "04") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 22] <- as.integer(SINAN_LEPTO_2010 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "05") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 23] <- as.integer(SINAN_LEPTO_2010 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "06") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 24] <- as.integer(SINAN_LEPTO_2010 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "07") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 25] <- as.integer(SINAN_LEPTO_2010 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "08") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 26] <- as.integer(SINAN_LEPTO_2010 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "10") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 27] <- as.integer(SINAN_LEPTO_2010 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "09") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 28] <- as.integer(SINAN_LEPTO_2010 %>% 
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
  
  AUX[which(AUX$COD_IBGE == i), 29] <- as.integer(SINAN_LEPTO_2010 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_GESTANT == "5") %>% 
                                                    count()
  )
  AUX[which(AUX$COD_IBGE == i), 30] <- as.integer(SINAN_LEPTO_2010 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_GESTANT == "6") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 31] <- as.integer(SINAN_LEPTO_2010 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_GESTANT == "9") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 32] <- as.integer(SINAN_LEPTO_2010 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "01") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 33] <- as.integer(SINAN_LEPTO_2010 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "2") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 34] <- as.integer(SINAN_LEPTO_2010 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "3") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 35] <- as.integer(SINAN_LEPTO_2010 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "4") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 36] <- as.integer(SINAN_LEPTO_2010 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "5") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 37] <- as.integer(SINAN_LEPTO_2010 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "9") %>% 
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2010_GERAL"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2010_GERAL"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2010_GERAL.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2010 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_CB_LAM == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2010 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_CB_CRI == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2010 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_CB_CAI == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2010 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_CB_FOS == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_LEPTO_2010 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_CB_SIN == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_LEPTO_2010 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_CB_PLA == 1) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_LEPTO_2010 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_CB_COR == 1) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(SINAN_LEPTO_2010 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             ANT_CB_ROE == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_LEPTO_2010 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             ANT_CB_GRA == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(SINAN_LEPTO_2010 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_CB_TER == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_LEPTO_2010 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_CB_LIX == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 16] <- as.integer(SINAN_LEPTO_2010 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_CB_OUT == 4018) %>% 
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2010_SIT_RISCO"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2010_SIT_RISCO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2010_SIT_RISCO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2010 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CLI_FEBRE == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2010 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CLI_MIALGI == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2010 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CLI_CEFALE == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2010 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CLI_PROST == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_LEPTO_2010 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CLI_CONGES == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_LEPTO_2010 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_PANTUR == 1) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_LEPTO_2010 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_VOMITO == 1) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(SINAN_LEPTO_2010 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             CLI_DIARRE == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_LEPTO_2010 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             CLI_ICTERI == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(SINAN_LEPTO_2010 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_RENAL == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_LEPTO_2010 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_RESPIR == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 16] <- as.integer(SINAN_LEPTO_2010 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_CARDIA == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 17] <- as.integer(SINAN_LEPTO_2010 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_HEMOPU == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 18] <- as.integer(SINAN_LEPTO_2010 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_HEMORR == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 19] <- as.integer(SINAN_LEPTO_2010 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_MENING == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 20] <- as.integer(SINAN_LEPTO_2010 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_OUTROS == 1) %>% 
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2010_SINTOMAS"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2010_SINTOMAS"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2010_SINTOMAS.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2010 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ATE_HOSP == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2010 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ATE_HOSP == 2) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2010_HOSPITALIZACAO"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2010_HOSPITALIZACAO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2010_HOSPITALIZACAO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2010 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CLASSI_FIN == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2010 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CLASSI_FIN == 2) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2010_CLASSI_FIN"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2010_CLASSI_FIN"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2010_CLASSI_FIN.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2010 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CRITERIO == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2010 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CRITERIO == 2) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2010_CRIT_ENCER"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2010_CRIT_ENCER"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2010_CRIT_ENCER.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2010 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CON_AREA == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2010 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AREA == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2010 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AREA == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2010 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AREA == 9) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4: ncol(AUX)] <- apply(AUX[, 4: ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2010_LPI"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2010_LPI"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2010_LPI.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2010 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CON_AMBIEN == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2010 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2010 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2010 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 4) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_LEPTO_2010 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 9) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4: ncol(AUX)] <- apply(AUX[, 4: ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2010_AMBIENTE"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2010_AMBIENTE"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2010_AMBIENTE.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2010 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CON_AMBIEN == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2010 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2010 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 9) %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4: ncol(AUX)] <- apply(AUX[, 4: ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2010_TRABALHO"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2010_TRABALHO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2010_TRABALHO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2010 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          EVOLUCAO == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2010 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            EVOLUCAO == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2010 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            EVOLUCAO == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2010 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            EVOLUCAO == 9) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4: ncol(AUX)] <- apply(AUX[, 4: ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2010_EVOLUCAO"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2010_EVOLUCAO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2010_EVOLUCAO.csv"), 
           row.names = FALSE)

rm(SINAN_LEPTO_2010)


##################################################################################
#################   2011    ######################################################
##################################################################################

SINAN_LEPTO_2011 <- LEPTONET %>% filter(ID_REGIONA == ID_REG | ID_RG_RESI == ID_REG,
                                        NU_ANO == 2011)

assign(paste0("RS", RS, "_LEPTO_2011_SINAN"), SINAN_LEPTO_2011)

write.csv (assign(paste0("RS", RS, "_LEPTO_2011_SINAN"), SINAN_LEPTO_2011), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2011_SINAN.csv"), 
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
  
  AUX[which(AUX == i), 2] <- as.integer(SINAN_LEPTO_2011 %>%
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==201101)%>%
                                          count()
  )
  
  AUX[which(AUX == i), 3] <- as.integer(SINAN_LEPTO_2011 %>% 
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==201102) %>% 
                                          count()
  )
  
  AUX[which(AUX == i), 4] <- as.integer(SINAN_LEPTO_2011 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==201103) %>% 
                                          count()
  )
  
  AUX[which(AUX == i),5] <- as.integer(SINAN_LEPTO_2011 %>% 
                                         filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==201104) %>% 
                                         count()
  )
  
  AUX[which(AUX == i), 6] <- as.integer(SINAN_LEPTO_2011 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==201105) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 7] <- as.integer(SINAN_LEPTO_2011 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==201106) %>%
                                          count()
  )
  
  AUX[which(AUX == i), 8] <- as.integer(SINAN_LEPTO_2011 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==201107) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 9] <- as.integer(SINAN_LEPTO_2011 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==201108) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 10] <- as.integer(SINAN_LEPTO_2011 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201109) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 11] <- as.integer(SINAN_LEPTO_2011 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201110) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 12] <- as.integer(SINAN_LEPTO_2011 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201111) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 13] <- as.integer(SINAN_LEPTO_2011 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201112) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 14] <- as.integer(SINAN_LEPTO_2011 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201113) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 15] <- as.integer(SINAN_LEPTO_2011 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201114) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 16] <- as.integer(SINAN_LEPTO_2011 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201115) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 17] <- as.integer(SINAN_LEPTO_2011 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201116) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 18] <- as.integer(SINAN_LEPTO_2011 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201117) %>%  
                                           count() 
  )
  
  AUX[which(AUX == i), 19] <- as.integer(SINAN_LEPTO_2011 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201118) %>%      
                                           count() 
  )
  
  AUX[which(AUX == i), 20] <- as.integer(SINAN_LEPTO_2011 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201119) %>%
                                           count() 
  )
  
  AUX[which(AUX == i),  21] <- as.integer(SINAN_LEPTO_2011 %>% 
                                            filter(ID_MN_RESI ==i & 
                                                     SEM_PRI ==201120) %>%
                                            count() 
  )
  
  AUX[which(AUX == i), 22] <- as.integer(SINAN_LEPTO_2011 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201121) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 23] <- as.integer(SINAN_LEPTO_2011 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201122) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 24] <- as.integer(SINAN_LEPTO_2011 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201123) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 25] <- as.integer(SINAN_LEPTO_2011 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201124) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 26] <- as.integer(SINAN_LEPTO_2011 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201125) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 27] <- as.integer(SINAN_LEPTO_2011 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201126) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 28] <- as.integer(SINAN_LEPTO_2011 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201127) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 29] <- as.integer(SINAN_LEPTO_2011 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201128) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 30] <- as.integer(SINAN_LEPTO_2011 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201129) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 31] <- as.integer(SINAN_LEPTO_2011 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201130) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 32] <- as.integer(SINAN_LEPTO_2011 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201131) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 33] <- as.integer(SINAN_LEPTO_2011 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201132) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 34] <- as.integer(SINAN_LEPTO_2011 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201133) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 35] <- as.integer(SINAN_LEPTO_2011 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201134) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 36] <- as.integer(SINAN_LEPTO_2011 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201135) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 37] <- as.integer(SINAN_LEPTO_2011 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201136) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 38] <- as.integer(SINAN_LEPTO_2011 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201137) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 39] <- as.integer(SINAN_LEPTO_2011 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201138) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 40] <- as.integer(SINAN_LEPTO_2011 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201139) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 41] <- as.integer(SINAN_LEPTO_2011 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201140) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 42] <- as.integer(SINAN_LEPTO_2011 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201141) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 43] <- as.integer(SINAN_LEPTO_2011 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201142) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 44] <- as.integer(SINAN_LEPTO_2011 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201143) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 45] <- as.integer(SINAN_LEPTO_2011 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201144) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 46] <- as.integer(SINAN_LEPTO_2011 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201145) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 47] <- as.integer(SINAN_LEPTO_2011 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201146) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 48] <- as.integer(SINAN_LEPTO_2011 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201147) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 49] <- as.integer(SINAN_LEPTO_2011 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201148) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 50] <- as.integer(SINAN_LEPTO_2011 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201149) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 51] <- as.integer(SINAN_LEPTO_2011 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201150) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 52] <- as.integer(SINAN_LEPTO_2011 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201151) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 53] <- as.integer(SINAN_LEPTO_2011 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201152) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 54] <- as.integer(SINAN_LEPTO_2011 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201153) %>%
                                           count() 
  )
}

AUX[, 1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX[(nrow(AUX)+ 1), 2:54] <- apply(AUX[, 2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2011_SE_Notificados"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2011_SE_Notificados"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2011_SE_Notificados.csv"), 
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
  
  AUX[which(AUX == i), 2] <- as.integer(SINAN_LEPTO_2011 %>%
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==201101,
                                                 CLASSI_FIN == 1)%>%
                                          count()
  )
  
  AUX[which(AUX == i), 3] <- as.integer(SINAN_LEPTO_2011 %>% 
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==201102,
                                                 CLASSI_FIN == 1) %>% 
                                          count()
  )
  
  AUX[which(AUX == i), 4] <- as.integer(SINAN_LEPTO_2011 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==201103,
                                                 CLASSI_FIN == 1) %>% 
                                          count()
  )
  
  AUX[which(AUX == i),5] <- as.integer(SINAN_LEPTO_2011 %>% 
                                         filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==201104,
                                                CLASSI_FIN == 1) %>% 
                                         count()
  )
  
  AUX[which(AUX == i), 6] <- as.integer(SINAN_LEPTO_2011 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==201105,
                                                 CLASSI_FIN == 1) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 7] <- as.integer(SINAN_LEPTO_2011 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==201106,
                                                 CLASSI_FIN == 1) %>%
                                          count()
  )
  
  AUX[which(AUX == i), 8] <- as.integer(SINAN_LEPTO_2011 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==201107,
                                                 CLASSI_FIN == 1) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 9] <- as.integer(SINAN_LEPTO_2011 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==201108,
                                                 CLASSI_FIN == 1) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 10] <- as.integer(SINAN_LEPTO_2011 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201109,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 11] <- as.integer(SINAN_LEPTO_2011 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201110,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 12] <- as.integer(SINAN_LEPTO_2011 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201111,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 13] <- as.integer(SINAN_LEPTO_2011 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201112,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 14] <- as.integer(SINAN_LEPTO_2011 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201113,
                                                  CLASSI_FIN == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 15] <- as.integer(SINAN_LEPTO_2011 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201114,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 16] <- as.integer(SINAN_LEPTO_2011 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201115,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 17] <- as.integer(SINAN_LEPTO_2011 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201116,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 18] <- as.integer(SINAN_LEPTO_2011 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201117,
                                                  CLASSI_FIN == 1) %>%  
                                           count() 
  )
  
  AUX[which(AUX == i), 19] <- as.integer(SINAN_LEPTO_2011 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201118,
                                                  CLASSI_FIN == 1) %>%      
                                           count() 
  )
  
  AUX[which(AUX == i), 20] <- as.integer(SINAN_LEPTO_2011 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201119,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i),  21] <- as.integer(SINAN_LEPTO_2011 %>% 
                                            filter(ID_MN_RESI ==i & 
                                                     SEM_PRI ==201120,
                                                   CLASSI_FIN == 1) %>%
                                            count() 
  )
  
  AUX[which(AUX == i), 22] <- as.integer(SINAN_LEPTO_2011 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201121,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 23] <- as.integer(SINAN_LEPTO_2011 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201122,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 24] <- as.integer(SINAN_LEPTO_2011 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201123,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 25] <- as.integer(SINAN_LEPTO_2011 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201124,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 26] <- as.integer(SINAN_LEPTO_2011 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201125,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 27] <- as.integer(SINAN_LEPTO_2011 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201126,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 28] <- as.integer(SINAN_LEPTO_2011 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201127,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 29] <- as.integer(SINAN_LEPTO_2011 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201128,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 30] <- as.integer(SINAN_LEPTO_2011 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201129,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 31] <- as.integer(SINAN_LEPTO_2011 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201130,
                                                  CLASSI_FIN == 1) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 32] <- as.integer(SINAN_LEPTO_2011 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201131,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 33] <- as.integer(SINAN_LEPTO_2011 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201132,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 34] <- as.integer(SINAN_LEPTO_2011 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201133,
                                                  CLASSI_FIN == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 35] <- as.integer(SINAN_LEPTO_2011 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201134,
                                                  CLASSI_FIN == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 36] <- as.integer(SINAN_LEPTO_2011 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201135,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 37] <- as.integer(SINAN_LEPTO_2011 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201136,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 38] <- as.integer(SINAN_LEPTO_2011 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201137,
                                                  CLASSI_FIN == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 39] <- as.integer(SINAN_LEPTO_2011 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201138,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 40] <- as.integer(SINAN_LEPTO_2011 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201139,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 41] <- as.integer(SINAN_LEPTO_2011 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201140,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 42] <- as.integer(SINAN_LEPTO_2011 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201141,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 43] <- as.integer(SINAN_LEPTO_2011 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201142,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 44] <- as.integer(SINAN_LEPTO_2011 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201143,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 45] <- as.integer(SINAN_LEPTO_2011 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201144,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 46] <- as.integer(SINAN_LEPTO_2011 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201145,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 47] <- as.integer(SINAN_LEPTO_2011 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201146,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 48] <- as.integer(SINAN_LEPTO_2011 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201147,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 49] <- as.integer(SINAN_LEPTO_2011 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201148,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 50] <- as.integer(SINAN_LEPTO_2011 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201149,
                                                  CLASSI_FIN == 1) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 51] <- as.integer(SINAN_LEPTO_2011 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201150,
                                                  CLASSI_FIN == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 52] <- as.integer(SINAN_LEPTO_2011 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201151,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 53] <- as.integer(SINAN_LEPTO_2011 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201152,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 54] <- as.integer(SINAN_LEPTO_2011 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201153,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
}

AUX[, 1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX[(nrow(AUX)+ 1), 2:54] <- apply(AUX[, 2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2011_SE_Confirmados"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2011_SE_Confirmados"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2011_SE_Confirmados.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2011 %>% 
                                                   filter(ID_MN_RESI == i) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2011 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CLASSI_FIN == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2011 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CS_ZONA == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2011 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CS_ZONA == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_LEPTO_2011 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CS_ZONA == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_LEPTO_2011 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ZONA == 9 
                                                           |
                                                             is.na(CS_ZONA)) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_LEPTO_2011 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_SEXO == "F") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(SINAN_LEPTO_2011 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_SEXO == "M") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_LEPTO_2011 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             NU_IDADE_N <= 3012) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(SINAN_LEPTO_2011 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             NU_IDADE_N > 4000 
                                                           & 
                                                             NU_IDADE_N <= 4005) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_LEPTO_2011 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             NU_IDADE_N > 4005 
                                                           & 
                                                             NU_IDADE_N <= 4012) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 16] <- as.integer(SINAN_LEPTO_2011 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             NU_IDADE_N > 4012 
                                                           & 
                                                             NU_IDADE_N <= 4018) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 17] <- as.integer(SINAN_LEPTO_2011 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             NU_IDADE_N > 4018 
                                                           & 
                                                             NU_IDADE_N <= 4059) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 18] <- as.integer(SINAN_LEPTO_2011 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             NU_IDADE_N > 4059) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 19] <- as.integer(SINAN_LEPTO_2011 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "00") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 20] <- as.integer(SINAN_LEPTO_2011 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "01" 
                                                           | 
                                                             CS_ESCOL_N == "02" 
                                                           | 
                                                             CS_ESCOL_N == "03") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 21] <- as.integer(SINAN_LEPTO_2011 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "04") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 22] <- as.integer(SINAN_LEPTO_2011 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "05") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 23] <- as.integer(SINAN_LEPTO_2011 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "06") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 24] <- as.integer(SINAN_LEPTO_2011 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "07") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 25] <- as.integer(SINAN_LEPTO_2011 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "08") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 26] <- as.integer(SINAN_LEPTO_2011 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "10") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 27] <- as.integer(SINAN_LEPTO_2011 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "09") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 28] <- as.integer(SINAN_LEPTO_2011 %>% 
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
  
  AUX[which(AUX$COD_IBGE == i), 29] <- as.integer(SINAN_LEPTO_2011 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_GESTANT == "5") %>% 
                                                    count()
  )
  AUX[which(AUX$COD_IBGE == i), 30] <- as.integer(SINAN_LEPTO_2011 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_GESTANT == "6") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 31] <- as.integer(SINAN_LEPTO_2011 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_GESTANT == "9") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 32] <- as.integer(SINAN_LEPTO_2011 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "01") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 33] <- as.integer(SINAN_LEPTO_2011 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "2") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 34] <- as.integer(SINAN_LEPTO_2011 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "3") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 35] <- as.integer(SINAN_LEPTO_2011 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "4") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 36] <- as.integer(SINAN_LEPTO_2011 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "5") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 37] <- as.integer(SINAN_LEPTO_2011 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "9") %>% 
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2011_GERAL"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2011_GERAL"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2011_GERAL.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2011 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_CB_LAM == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2011 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_CB_CRI == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2011 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_CB_CAI == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2011 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_CB_FOS == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_LEPTO_2011 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_CB_SIN == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_LEPTO_2011 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_CB_PLA == 1) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_LEPTO_2011 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_CB_COR == 1) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(SINAN_LEPTO_2011 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             ANT_CB_ROE == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_LEPTO_2011 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             ANT_CB_GRA == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(SINAN_LEPTO_2011 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_CB_TER == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_LEPTO_2011 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_CB_LIX == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 16] <- as.integer(SINAN_LEPTO_2011 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_CB_OUT == 4018) %>% 
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2011_SIT_RISCO"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2011_SIT_RISCO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2011_SIT_RISCO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2011 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CLI_FEBRE == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2011 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CLI_MIALGI == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2011 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CLI_CEFALE == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2011 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CLI_PROST == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_LEPTO_2011 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CLI_CONGES == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_LEPTO_2011 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_PANTUR == 1) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_LEPTO_2011 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_VOMITO == 1) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(SINAN_LEPTO_2011 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             CLI_DIARRE == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_LEPTO_2011 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             CLI_ICTERI == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(SINAN_LEPTO_2011 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_RENAL == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_LEPTO_2011 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_RESPIR == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 16] <- as.integer(SINAN_LEPTO_2011 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_CARDIA == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 17] <- as.integer(SINAN_LEPTO_2011 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_HEMOPU == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 18] <- as.integer(SINAN_LEPTO_2011 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_HEMORR == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 19] <- as.integer(SINAN_LEPTO_2011 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_MENING == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 20] <- as.integer(SINAN_LEPTO_2011 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_OUTROS == 1) %>% 
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2011_SINTOMAS"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2011_SINTOMAS"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2011_SINTOMAS.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2011 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ATE_HOSP == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2011 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ATE_HOSP == 2) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2011_HOSPITALIZACAO"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2011_HOSPITALIZACAO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2011_HOSPITALIZACAO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2011 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CLASSI_FIN == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2011 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CLASSI_FIN == 2) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2011_CLASSI_FIN"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2011_CLASSI_FIN"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2011_CLASSI_FIN.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2011 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CRITERIO == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2011 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CRITERIO == 2) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2011_CRIT_ENCER"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2011_CRIT_ENCER"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2011_CRIT_ENCER.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2011 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CON_AREA == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2011 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AREA == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2011 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AREA == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2011 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AREA == 9) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4: ncol(AUX)] <- apply(AUX[, 4: ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2011_LPI"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2011_LPI"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2011_LPI.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2011 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CON_AMBIEN == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2011 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2011 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2011 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 4) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_LEPTO_2011 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 9) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4: ncol(AUX)] <- apply(AUX[, 4: ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2011_AMBIENTE"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2011_AMBIENTE"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2011_AMBIENTE.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2011 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CON_AMBIEN == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2011 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2011 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 9) %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4: ncol(AUX)] <- apply(AUX[, 4: ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2011_TRABALHO"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2011_TRABALHO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2011_TRABALHO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2011 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          EVOLUCAO == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2011 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            EVOLUCAO == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2011 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            EVOLUCAO == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2011 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            EVOLUCAO == 9) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4: ncol(AUX)] <- apply(AUX[, 4: ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2011_EVOLUCAO"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2011_EVOLUCAO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2011_EVOLUCAO.csv"), 
           row.names = FALSE)

rm(SINAN_LEPTO_2011)


##################################################################################
#################   2012    ######################################################
##################################################################################

SINAN_LEPTO_2012 <- LEPTONET %>% filter(ID_REGIONA == ID_REG | ID_RG_RESI == ID_REG,
                                        NU_ANO == 2012)

assign(paste0("RS", RS, "_LEPTO_2012_SINAN"), SINAN_LEPTO_2012)

write.csv (assign(paste0("RS", RS, "_LEPTO_2012_SINAN"), SINAN_LEPTO_2012), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2012_SINAN.csv"), 
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
  
  AUX[which(AUX == i), 2] <- as.integer(SINAN_LEPTO_2012 %>%
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==201201)%>%
                                          count()
  )
  
  AUX[which(AUX == i), 3] <- as.integer(SINAN_LEPTO_2012 %>% 
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==201202) %>% 
                                          count()
  )
  
  AUX[which(AUX == i), 4] <- as.integer(SINAN_LEPTO_2012 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==201203) %>% 
                                          count()
  )
  
  AUX[which(AUX == i),5] <- as.integer(SINAN_LEPTO_2012 %>% 
                                         filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==201204) %>% 
                                         count()
  )
  
  AUX[which(AUX == i), 6] <- as.integer(SINAN_LEPTO_2012 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==201205) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 7] <- as.integer(SINAN_LEPTO_2012 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==201206) %>%
                                          count()
  )
  
  AUX[which(AUX == i), 8] <- as.integer(SINAN_LEPTO_2012 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==201207) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 9] <- as.integer(SINAN_LEPTO_2012 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==201208) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 10] <- as.integer(SINAN_LEPTO_2012 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201209) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 11] <- as.integer(SINAN_LEPTO_2012 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201210) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 12] <- as.integer(SINAN_LEPTO_2012 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201211) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 13] <- as.integer(SINAN_LEPTO_2012 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201212) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 14] <- as.integer(SINAN_LEPTO_2012 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201213) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 15] <- as.integer(SINAN_LEPTO_2012 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201214) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 16] <- as.integer(SINAN_LEPTO_2012 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201215) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 17] <- as.integer(SINAN_LEPTO_2012 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201216) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 18] <- as.integer(SINAN_LEPTO_2012 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201217) %>%  
                                           count() 
  )
  
  AUX[which(AUX == i), 19] <- as.integer(SINAN_LEPTO_2012 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201218) %>%      
                                           count() 
  )
  
  AUX[which(AUX == i), 20] <- as.integer(SINAN_LEPTO_2012 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201219) %>%
                                           count() 
  )
  
  AUX[which(AUX == i),  21] <- as.integer(SINAN_LEPTO_2012 %>% 
                                            filter(ID_MN_RESI ==i & 
                                                     SEM_PRI ==201220) %>%
                                            count() 
  )
  
  AUX[which(AUX == i), 22] <- as.integer(SINAN_LEPTO_2012 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201221) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 23] <- as.integer(SINAN_LEPTO_2012 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201222) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 24] <- as.integer(SINAN_LEPTO_2012 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201223) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 25] <- as.integer(SINAN_LEPTO_2012 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201224) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 26] <- as.integer(SINAN_LEPTO_2012 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201225) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 27] <- as.integer(SINAN_LEPTO_2012 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201226) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 28] <- as.integer(SINAN_LEPTO_2012 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201227) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 29] <- as.integer(SINAN_LEPTO_2012 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201228) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 30] <- as.integer(SINAN_LEPTO_2012 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201229) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 31] <- as.integer(SINAN_LEPTO_2012 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201230) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 32] <- as.integer(SINAN_LEPTO_2012 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201231) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 33] <- as.integer(SINAN_LEPTO_2012 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201232) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 34] <- as.integer(SINAN_LEPTO_2012 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201233) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 35] <- as.integer(SINAN_LEPTO_2012 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201234) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 36] <- as.integer(SINAN_LEPTO_2012 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201235) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 37] <- as.integer(SINAN_LEPTO_2012 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201236) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 38] <- as.integer(SINAN_LEPTO_2012 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201237) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 39] <- as.integer(SINAN_LEPTO_2012 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201238) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 40] <- as.integer(SINAN_LEPTO_2012 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201239) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 41] <- as.integer(SINAN_LEPTO_2012 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201240) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 42] <- as.integer(SINAN_LEPTO_2012 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201241) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 43] <- as.integer(SINAN_LEPTO_2012 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201242) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 44] <- as.integer(SINAN_LEPTO_2012 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201243) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 45] <- as.integer(SINAN_LEPTO_2012 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201244) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 46] <- as.integer(SINAN_LEPTO_2012 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201245) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 47] <- as.integer(SINAN_LEPTO_2012 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201246) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 48] <- as.integer(SINAN_LEPTO_2012 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201247) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 49] <- as.integer(SINAN_LEPTO_2012 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201248) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 50] <- as.integer(SINAN_LEPTO_2012 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201249) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 51] <- as.integer(SINAN_LEPTO_2012 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201250) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 52] <- as.integer(SINAN_LEPTO_2012 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201251) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 53] <- as.integer(SINAN_LEPTO_2012 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201252) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 54] <- as.integer(SINAN_LEPTO_2012 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201253) %>%
                                           count() 
  )
}

AUX[, 1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX[(nrow(AUX)+ 1), 2:54] <- apply(AUX[, 2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2012_SE_Notificados"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2012_SE_Notificados"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2012_SE_Notificados.csv"), 
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
  
  AUX[which(AUX == i), 2] <- as.integer(SINAN_LEPTO_2012 %>%
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==201201,
                                                 CLASSI_FIN == 1)%>%
                                          count()
  )
  
  AUX[which(AUX == i), 3] <- as.integer(SINAN_LEPTO_2012 %>% 
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==201202,
                                                 CLASSI_FIN == 1) %>% 
                                          count()
  )
  
  AUX[which(AUX == i), 4] <- as.integer(SINAN_LEPTO_2012 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==201203,
                                                 CLASSI_FIN == 1) %>% 
                                          count()
  )
  
  AUX[which(AUX == i),5] <- as.integer(SINAN_LEPTO_2012 %>% 
                                         filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==201204,
                                                CLASSI_FIN == 1) %>% 
                                         count()
  )
  
  AUX[which(AUX == i), 6] <- as.integer(SINAN_LEPTO_2012 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==201205,
                                                 CLASSI_FIN == 1) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 7] <- as.integer(SINAN_LEPTO_2012 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==201206,
                                                 CLASSI_FIN == 1) %>%
                                          count()
  )
  
  AUX[which(AUX == i), 8] <- as.integer(SINAN_LEPTO_2012 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==201207,
                                                 CLASSI_FIN == 1) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 9] <- as.integer(SINAN_LEPTO_2012 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==201208,
                                                 CLASSI_FIN == 1) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 10] <- as.integer(SINAN_LEPTO_2012 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201209,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 11] <- as.integer(SINAN_LEPTO_2012 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201210,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 12] <- as.integer(SINAN_LEPTO_2012 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201211,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 13] <- as.integer(SINAN_LEPTO_2012 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201212,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 14] <- as.integer(SINAN_LEPTO_2012 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201213,
                                                  CLASSI_FIN == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 15] <- as.integer(SINAN_LEPTO_2012 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201214,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 16] <- as.integer(SINAN_LEPTO_2012 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201215,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 17] <- as.integer(SINAN_LEPTO_2012 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201216,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 18] <- as.integer(SINAN_LEPTO_2012 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201217,
                                                  CLASSI_FIN == 1) %>%  
                                           count() 
  )
  
  AUX[which(AUX == i), 19] <- as.integer(SINAN_LEPTO_2012 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201218,
                                                  CLASSI_FIN == 1) %>%      
                                           count() 
  )
  
  AUX[which(AUX == i), 20] <- as.integer(SINAN_LEPTO_2012 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201219,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i),  21] <- as.integer(SINAN_LEPTO_2012 %>% 
                                            filter(ID_MN_RESI ==i & 
                                                     SEM_PRI ==201220,
                                                   CLASSI_FIN == 1) %>%
                                            count() 
  )
  
  AUX[which(AUX == i), 22] <- as.integer(SINAN_LEPTO_2012 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201221,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 23] <- as.integer(SINAN_LEPTO_2012 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201222,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 24] <- as.integer(SINAN_LEPTO_2012 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201223,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 25] <- as.integer(SINAN_LEPTO_2012 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201224,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 26] <- as.integer(SINAN_LEPTO_2012 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201225,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 27] <- as.integer(SINAN_LEPTO_2012 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201226,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 28] <- as.integer(SINAN_LEPTO_2012 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201227,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 29] <- as.integer(SINAN_LEPTO_2012 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201228,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 30] <- as.integer(SINAN_LEPTO_2012 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201229,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 31] <- as.integer(SINAN_LEPTO_2012 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201230,
                                                  CLASSI_FIN == 1) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 32] <- as.integer(SINAN_LEPTO_2012 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201231,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 33] <- as.integer(SINAN_LEPTO_2012 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201232,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 34] <- as.integer(SINAN_LEPTO_2012 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201233,
                                                  CLASSI_FIN == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 35] <- as.integer(SINAN_LEPTO_2012 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201234,
                                                  CLASSI_FIN == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 36] <- as.integer(SINAN_LEPTO_2012 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201235,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 37] <- as.integer(SINAN_LEPTO_2012 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201236,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 38] <- as.integer(SINAN_LEPTO_2012 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201237,
                                                  CLASSI_FIN == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 39] <- as.integer(SINAN_LEPTO_2012 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201238,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 40] <- as.integer(SINAN_LEPTO_2012 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201239,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 41] <- as.integer(SINAN_LEPTO_2012 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201240,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 42] <- as.integer(SINAN_LEPTO_2012 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201241,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 43] <- as.integer(SINAN_LEPTO_2012 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201242,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 44] <- as.integer(SINAN_LEPTO_2012 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201243,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 45] <- as.integer(SINAN_LEPTO_2012 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201244,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 46] <- as.integer(SINAN_LEPTO_2012 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201245,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 47] <- as.integer(SINAN_LEPTO_2012 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201246,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 48] <- as.integer(SINAN_LEPTO_2012 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201247,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 49] <- as.integer(SINAN_LEPTO_2012 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201248,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 50] <- as.integer(SINAN_LEPTO_2012 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201249,
                                                  CLASSI_FIN == 1) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 51] <- as.integer(SINAN_LEPTO_2012 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201250,
                                                  CLASSI_FIN == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 52] <- as.integer(SINAN_LEPTO_2012 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201251,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 53] <- as.integer(SINAN_LEPTO_2012 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201252,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 54] <- as.integer(SINAN_LEPTO_2012 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201253,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
}

AUX[, 1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX[(nrow(AUX)+ 1), 2:54] <- apply(AUX[, 2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2012_SE_Confirmados"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2012_SE_Confirmados"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2012_SE_Confirmados.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2012 %>% 
                                                   filter(ID_MN_RESI == i) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2012 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CLASSI_FIN == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2012 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CS_ZONA == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2012 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CS_ZONA == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_LEPTO_2012 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CS_ZONA == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_LEPTO_2012 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ZONA == 9 
                                                           |
                                                             is.na(CS_ZONA)) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_LEPTO_2012 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_SEXO == "F") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(SINAN_LEPTO_2012 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_SEXO == "M") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_LEPTO_2012 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             NU_IDADE_N <= 3012) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(SINAN_LEPTO_2012 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             NU_IDADE_N > 4000 
                                                           & 
                                                             NU_IDADE_N <= 4005) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_LEPTO_2012 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             NU_IDADE_N > 4005 
                                                           & 
                                                             NU_IDADE_N <= 4012) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 16] <- as.integer(SINAN_LEPTO_2012 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             NU_IDADE_N > 4012 
                                                           & 
                                                             NU_IDADE_N <= 4018) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 17] <- as.integer(SINAN_LEPTO_2012 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             NU_IDADE_N > 4018 
                                                           & 
                                                             NU_IDADE_N <= 4059) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 18] <- as.integer(SINAN_LEPTO_2012 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             NU_IDADE_N > 4059) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 19] <- as.integer(SINAN_LEPTO_2012 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "00") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 20] <- as.integer(SINAN_LEPTO_2012 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "01" 
                                                           | 
                                                             CS_ESCOL_N == "02" 
                                                           | 
                                                             CS_ESCOL_N == "03") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 21] <- as.integer(SINAN_LEPTO_2012 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "04") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 22] <- as.integer(SINAN_LEPTO_2012 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "05") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 23] <- as.integer(SINAN_LEPTO_2012 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "06") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 24] <- as.integer(SINAN_LEPTO_2012 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "07") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 25] <- as.integer(SINAN_LEPTO_2012 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "08") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 26] <- as.integer(SINAN_LEPTO_2012 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "10") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 27] <- as.integer(SINAN_LEPTO_2012 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "09") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 28] <- as.integer(SINAN_LEPTO_2012 %>% 
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
  
  AUX[which(AUX$COD_IBGE == i), 29] <- as.integer(SINAN_LEPTO_2012 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_GESTANT == "5") %>% 
                                                    count()
  )
  AUX[which(AUX$COD_IBGE == i), 30] <- as.integer(SINAN_LEPTO_2012 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_GESTANT == "6") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 31] <- as.integer(SINAN_LEPTO_2012 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_GESTANT == "9") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 32] <- as.integer(SINAN_LEPTO_2012 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "01") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 33] <- as.integer(SINAN_LEPTO_2012 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "2") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 34] <- as.integer(SINAN_LEPTO_2012 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "3") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 35] <- as.integer(SINAN_LEPTO_2012 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "4") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 36] <- as.integer(SINAN_LEPTO_2012 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "5") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 37] <- as.integer(SINAN_LEPTO_2012 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "9") %>% 
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2012_GERAL"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2012_GERAL"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2012_GERAL.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2012 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_CB_LAM == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2012 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_CB_CRI == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2012 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_CB_CAI == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2012 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_CB_FOS == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_LEPTO_2012 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_CB_SIN == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_LEPTO_2012 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_CB_PLA == 1) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_LEPTO_2012 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_CB_COR == 1) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(SINAN_LEPTO_2012 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             ANT_CB_ROE == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_LEPTO_2012 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             ANT_CB_GRA == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(SINAN_LEPTO_2012 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_CB_TER == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_LEPTO_2012 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_CB_LIX == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 16] <- as.integer(SINAN_LEPTO_2012 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_CB_OUT == 4018) %>% 
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2012_SIT_RISCO"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2012_SIT_RISCO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2012_SIT_RISCO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2012 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CLI_FEBRE == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2012 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CLI_MIALGI == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2012 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CLI_CEFALE == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2012 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CLI_PROST == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_LEPTO_2012 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CLI_CONGES == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_LEPTO_2012 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_PANTUR == 1) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_LEPTO_2012 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_VOMITO == 1) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(SINAN_LEPTO_2012 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             CLI_DIARRE == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_LEPTO_2012 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             CLI_ICTERI == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(SINAN_LEPTO_2012 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_RENAL == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_LEPTO_2012 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_RESPIR == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 16] <- as.integer(SINAN_LEPTO_2012 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_CARDIA == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 17] <- as.integer(SINAN_LEPTO_2012 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_HEMOPU == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 18] <- as.integer(SINAN_LEPTO_2012 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_HEMORR == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 19] <- as.integer(SINAN_LEPTO_2012 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_MENING == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 20] <- as.integer(SINAN_LEPTO_2012 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_OUTROS == 1) %>% 
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2012_SINTOMAS"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2012_SINTOMAS"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2012_SINTOMAS.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2012 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ATE_HOSP == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2012 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ATE_HOSP == 2) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2012_HOSPITALIZACAO"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2012_HOSPITALIZACAO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2012_HOSPITALIZACAO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2012 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CLASSI_FIN == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2012 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CLASSI_FIN == 2) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2012_CLASSI_FIN"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2012_CLASSI_FIN"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2012_CLASSI_FIN.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2012 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CRITERIO == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2012 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CRITERIO == 2) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2012_CRIT_ENCER"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2012_CRIT_ENCER"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2012_CRIT_ENCER.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2012 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CON_AREA == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2012 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AREA == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2012 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AREA == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2012 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AREA == 9) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4: ncol(AUX)] <- apply(AUX[, 4: ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2012_LPI"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2012_LPI"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2012_LPI.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2012 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CON_AMBIEN == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2012 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2012 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2012 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 4) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_LEPTO_2012 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 9) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4: ncol(AUX)] <- apply(AUX[, 4: ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2012_AMBIENTE"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2012_AMBIENTE"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2012_AMBIENTE.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2012 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CON_AMBIEN == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2012 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2012 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 9) %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4: ncol(AUX)] <- apply(AUX[, 4: ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2012_TRABALHO"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2012_TRABALHO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2012_TRABALHO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2012 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          EVOLUCAO == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2012 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            EVOLUCAO == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2012 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            EVOLUCAO == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2012 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            EVOLUCAO == 9) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4: ncol(AUX)] <- apply(AUX[, 4: ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2012_EVOLUCAO"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2012_EVOLUCAO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2012_EVOLUCAO.csv"), 
           row.names = FALSE)

rm(SINAN_LEPTO_2012)

##################################################################################
#################   2013    ######################################################
##################################################################################

SINAN_LEPTO_2013 <- LEPTONET %>% filter(ID_REGIONA == ID_REG | ID_RG_RESI == ID_REG,
                                        NU_ANO == 2013)

assign(paste0("RS", RS, "_LEPTO_2013_SINAN"), SINAN_LEPTO_2013)

write.csv (assign(paste0("RS", RS, "_LEPTO_2013_SINAN"), SINAN_LEPTO_2013), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2013_SINAN.csv"), 
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
  
  AUX[which(AUX == i), 2] <- as.integer(SINAN_LEPTO_2013 %>%
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==201301)%>%
                                          count()
  )
  
  AUX[which(AUX == i), 3] <- as.integer(SINAN_LEPTO_2013 %>% 
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==201302) %>% 
                                          count()
  )
  
  AUX[which(AUX == i), 4] <- as.integer(SINAN_LEPTO_2013 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==201303) %>% 
                                          count()
  )
  
  AUX[which(AUX == i),5] <- as.integer(SINAN_LEPTO_2013 %>% 
                                         filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==201304) %>% 
                                         count()
  )
  
  AUX[which(AUX == i), 6] <- as.integer(SINAN_LEPTO_2013 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==201305) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 7] <- as.integer(SINAN_LEPTO_2013 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==201306) %>%
                                          count()
  )
  
  AUX[which(AUX == i), 8] <- as.integer(SINAN_LEPTO_2013 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==201307) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 9] <- as.integer(SINAN_LEPTO_2013 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==201308) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 10] <- as.integer(SINAN_LEPTO_2013 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201309) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 11] <- as.integer(SINAN_LEPTO_2013 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201310) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 12] <- as.integer(SINAN_LEPTO_2013 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201311) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 13] <- as.integer(SINAN_LEPTO_2013 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201312) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 14] <- as.integer(SINAN_LEPTO_2013 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201313) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 15] <- as.integer(SINAN_LEPTO_2013 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201314) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 16] <- as.integer(SINAN_LEPTO_2013 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201315) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 17] <- as.integer(SINAN_LEPTO_2013 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201316) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 18] <- as.integer(SINAN_LEPTO_2013 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201317) %>%  
                                           count() 
  )
  
  AUX[which(AUX == i), 19] <- as.integer(SINAN_LEPTO_2013 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201318) %>%      
                                           count() 
  )
  
  AUX[which(AUX == i), 20] <- as.integer(SINAN_LEPTO_2013 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201319) %>%
                                           count() 
  )
  
  AUX[which(AUX == i),  21] <- as.integer(SINAN_LEPTO_2013 %>% 
                                            filter(ID_MN_RESI ==i & 
                                                     SEM_PRI ==201320) %>%
                                            count() 
  )
  
  AUX[which(AUX == i), 22] <- as.integer(SINAN_LEPTO_2013 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201321) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 23] <- as.integer(SINAN_LEPTO_2013 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201322) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 24] <- as.integer(SINAN_LEPTO_2013 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201323) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 25] <- as.integer(SINAN_LEPTO_2013 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201324) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 26] <- as.integer(SINAN_LEPTO_2013 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201325) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 27] <- as.integer(SINAN_LEPTO_2013 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201326) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 28] <- as.integer(SINAN_LEPTO_2013 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201327) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 29] <- as.integer(SINAN_LEPTO_2013 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201328) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 30] <- as.integer(SINAN_LEPTO_2013 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201329) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 31] <- as.integer(SINAN_LEPTO_2013 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201330) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 32] <- as.integer(SINAN_LEPTO_2013 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201331) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 33] <- as.integer(SINAN_LEPTO_2013 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201332) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 34] <- as.integer(SINAN_LEPTO_2013 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201333) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 35] <- as.integer(SINAN_LEPTO_2013 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201334) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 36] <- as.integer(SINAN_LEPTO_2013 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201335) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 37] <- as.integer(SINAN_LEPTO_2013 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201336) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 38] <- as.integer(SINAN_LEPTO_2013 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201337) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 39] <- as.integer(SINAN_LEPTO_2013 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201338) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 40] <- as.integer(SINAN_LEPTO_2013 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201339) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 41] <- as.integer(SINAN_LEPTO_2013 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201340) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 42] <- as.integer(SINAN_LEPTO_2013 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201341) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 43] <- as.integer(SINAN_LEPTO_2013 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201342) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 44] <- as.integer(SINAN_LEPTO_2013 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201343) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 45] <- as.integer(SINAN_LEPTO_2013 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201344) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 46] <- as.integer(SINAN_LEPTO_2013 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201345) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 47] <- as.integer(SINAN_LEPTO_2013 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201346) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 48] <- as.integer(SINAN_LEPTO_2013 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201347) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 49] <- as.integer(SINAN_LEPTO_2013 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201348) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 50] <- as.integer(SINAN_LEPTO_2013 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201349) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 51] <- as.integer(SINAN_LEPTO_2013 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201350) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 52] <- as.integer(SINAN_LEPTO_2013 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201351) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 53] <- as.integer(SINAN_LEPTO_2013 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201352) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 54] <- as.integer(SINAN_LEPTO_2013 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201353) %>%
                                           count() 
  )
}

AUX[, 1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX[(nrow(AUX)+ 1), 2:54] <- apply(AUX[, 2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2013_SE_Notificados"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2013_SE_Notificados"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2013_SE_Notificados.csv"), 
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
  
  AUX[which(AUX == i), 2] <- as.integer(SINAN_LEPTO_2013 %>%
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==201301,
                                                 CLASSI_FIN == 1)%>%
                                          count()
  )
  
  AUX[which(AUX == i), 3] <- as.integer(SINAN_LEPTO_2013 %>% 
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==201302,
                                                 CLASSI_FIN == 1) %>% 
                                          count()
  )
  
  AUX[which(AUX == i), 4] <- as.integer(SINAN_LEPTO_2013 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==201303,
                                                 CLASSI_FIN == 1) %>% 
                                          count()
  )
  
  AUX[which(AUX == i),5] <- as.integer(SINAN_LEPTO_2013 %>% 
                                         filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==201304,
                                                CLASSI_FIN == 1) %>% 
                                         count()
  )
  
  AUX[which(AUX == i), 6] <- as.integer(SINAN_LEPTO_2013 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==201305,
                                                 CLASSI_FIN == 1) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 7] <- as.integer(SINAN_LEPTO_2013 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==201306,
                                                 CLASSI_FIN == 1) %>%
                                          count()
  )
  
  AUX[which(AUX == i), 8] <- as.integer(SINAN_LEPTO_2013 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==201307,
                                                 CLASSI_FIN == 1) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 9] <- as.integer(SINAN_LEPTO_2013 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==201308,
                                                 CLASSI_FIN == 1) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 10] <- as.integer(SINAN_LEPTO_2013 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201309,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 11] <- as.integer(SINAN_LEPTO_2013 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201310,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 12] <- as.integer(SINAN_LEPTO_2013 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201311,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 13] <- as.integer(SINAN_LEPTO_2013 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201312,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 14] <- as.integer(SINAN_LEPTO_2013 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201313,
                                                  CLASSI_FIN == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 15] <- as.integer(SINAN_LEPTO_2013 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201314,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 16] <- as.integer(SINAN_LEPTO_2013 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201315,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 17] <- as.integer(SINAN_LEPTO_2013 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201316,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 18] <- as.integer(SINAN_LEPTO_2013 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201317,
                                                  CLASSI_FIN == 1) %>%  
                                           count() 
  )
  
  AUX[which(AUX == i), 19] <- as.integer(SINAN_LEPTO_2013 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201318,
                                                  CLASSI_FIN == 1) %>%      
                                           count() 
  )
  
  AUX[which(AUX == i), 20] <- as.integer(SINAN_LEPTO_2013 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201319,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i),  21] <- as.integer(SINAN_LEPTO_2013 %>% 
                                            filter(ID_MN_RESI ==i & 
                                                     SEM_PRI ==201320,
                                                   CLASSI_FIN == 1) %>%
                                            count() 
  )
  
  AUX[which(AUX == i), 22] <- as.integer(SINAN_LEPTO_2013 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201321,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 23] <- as.integer(SINAN_LEPTO_2013 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201322,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 24] <- as.integer(SINAN_LEPTO_2013 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201323,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 25] <- as.integer(SINAN_LEPTO_2013 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201324,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 26] <- as.integer(SINAN_LEPTO_2013 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201325,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 27] <- as.integer(SINAN_LEPTO_2013 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201326,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 28] <- as.integer(SINAN_LEPTO_2013 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201327,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 29] <- as.integer(SINAN_LEPTO_2013 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201328,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 30] <- as.integer(SINAN_LEPTO_2013 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201329,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 31] <- as.integer(SINAN_LEPTO_2013 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201330,
                                                  CLASSI_FIN == 1) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 32] <- as.integer(SINAN_LEPTO_2013 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201331,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 33] <- as.integer(SINAN_LEPTO_2013 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201332,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 34] <- as.integer(SINAN_LEPTO_2013 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201333,
                                                  CLASSI_FIN == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 35] <- as.integer(SINAN_LEPTO_2013 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201334,
                                                  CLASSI_FIN == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 36] <- as.integer(SINAN_LEPTO_2013 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201335,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 37] <- as.integer(SINAN_LEPTO_2013 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201336,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 38] <- as.integer(SINAN_LEPTO_2013 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201337,
                                                  CLASSI_FIN == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 39] <- as.integer(SINAN_LEPTO_2013 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201338,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 40] <- as.integer(SINAN_LEPTO_2013 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201339,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 41] <- as.integer(SINAN_LEPTO_2013 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201340,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 42] <- as.integer(SINAN_LEPTO_2013 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201341,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 43] <- as.integer(SINAN_LEPTO_2013 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201342,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 44] <- as.integer(SINAN_LEPTO_2013 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201343,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 45] <- as.integer(SINAN_LEPTO_2013 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201344,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 46] <- as.integer(SINAN_LEPTO_2013 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201345,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 47] <- as.integer(SINAN_LEPTO_2013 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201346,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 48] <- as.integer(SINAN_LEPTO_2013 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201347,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 49] <- as.integer(SINAN_LEPTO_2013 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201348,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 50] <- as.integer(SINAN_LEPTO_2013 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201349,
                                                  CLASSI_FIN == 1) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 51] <- as.integer(SINAN_LEPTO_2013 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201350,
                                                  CLASSI_FIN == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 52] <- as.integer(SINAN_LEPTO_2013 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201351,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 53] <- as.integer(SINAN_LEPTO_2013 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201352,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 54] <- as.integer(SINAN_LEPTO_2013 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201353,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
}

AUX[, 1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX[(nrow(AUX)+ 1), 2:54] <- apply(AUX[, 2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2013_SE_Confirmados"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2013_SE_Confirmados"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2013_SE_Confirmados.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2013 %>% 
                                                   filter(ID_MN_RESI == i) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2013 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CLASSI_FIN == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2013 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CS_ZONA == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2013 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CS_ZONA == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_LEPTO_2013 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CS_ZONA == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_LEPTO_2013 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ZONA == 9 
                                                           |
                                                             is.na(CS_ZONA)) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_LEPTO_2013 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_SEXO == "F") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(SINAN_LEPTO_2013 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_SEXO == "M") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_LEPTO_2013 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             NU_IDADE_N <= 3012) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(SINAN_LEPTO_2013 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             NU_IDADE_N > 4000 
                                                           & 
                                                             NU_IDADE_N <= 4005) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_LEPTO_2013 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             NU_IDADE_N > 4005 
                                                           & 
                                                             NU_IDADE_N <= 4012) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 16] <- as.integer(SINAN_LEPTO_2013 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             NU_IDADE_N > 4012 
                                                           & 
                                                             NU_IDADE_N <= 4018) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 17] <- as.integer(SINAN_LEPTO_2013 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             NU_IDADE_N > 4018 
                                                           & 
                                                             NU_IDADE_N <= 4059) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 18] <- as.integer(SINAN_LEPTO_2013 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             NU_IDADE_N > 4059) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 19] <- as.integer(SINAN_LEPTO_2013 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "00") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 20] <- as.integer(SINAN_LEPTO_2013 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "01" 
                                                           | 
                                                             CS_ESCOL_N == "02" 
                                                           | 
                                                             CS_ESCOL_N == "03") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 21] <- as.integer(SINAN_LEPTO_2013 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "04") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 22] <- as.integer(SINAN_LEPTO_2013 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "05") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 23] <- as.integer(SINAN_LEPTO_2013 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "06") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 24] <- as.integer(SINAN_LEPTO_2013 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "07") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 25] <- as.integer(SINAN_LEPTO_2013 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "08") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 26] <- as.integer(SINAN_LEPTO_2013 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "10") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 27] <- as.integer(SINAN_LEPTO_2013 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "09") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 28] <- as.integer(SINAN_LEPTO_2013 %>% 
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
  
  AUX[which(AUX$COD_IBGE == i), 29] <- as.integer(SINAN_LEPTO_2013 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_GESTANT == "5") %>% 
                                                    count()
  )
  AUX[which(AUX$COD_IBGE == i), 30] <- as.integer(SINAN_LEPTO_2013 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_GESTANT == "6") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 31] <- as.integer(SINAN_LEPTO_2013 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_GESTANT == "9") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 32] <- as.integer(SINAN_LEPTO_2013 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "01") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 33] <- as.integer(SINAN_LEPTO_2013 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "2") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 34] <- as.integer(SINAN_LEPTO_2013 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "3") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 35] <- as.integer(SINAN_LEPTO_2013 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "4") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 36] <- as.integer(SINAN_LEPTO_2013 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "5") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 37] <- as.integer(SINAN_LEPTO_2013 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "9") %>% 
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2013_GERAL"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2013_GERAL"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2013_GERAL.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2013 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_CB_LAM == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2013 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_CB_CRI == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2013 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_CB_CAI == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2013 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_CB_FOS == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_LEPTO_2013 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_CB_SIN == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_LEPTO_2013 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_CB_PLA == 1) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_LEPTO_2013 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_CB_COR == 1) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(SINAN_LEPTO_2013 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             ANT_CB_ROE == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_LEPTO_2013 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             ANT_CB_GRA == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(SINAN_LEPTO_2013 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_CB_TER == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_LEPTO_2013 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_CB_LIX == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 16] <- as.integer(SINAN_LEPTO_2013 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_CB_OUT == 4018) %>% 
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2013_SIT_RISCO"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2013_SIT_RISCO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2013_SIT_RISCO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2013 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CLI_FEBRE == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2013 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CLI_MIALGI == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2013 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CLI_CEFALE == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2013 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CLI_PROST == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_LEPTO_2013 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CLI_CONGES == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_LEPTO_2013 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_PANTUR == 1) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_LEPTO_2013 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_VOMITO == 1) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(SINAN_LEPTO_2013 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             CLI_DIARRE == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_LEPTO_2013 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             CLI_ICTERI == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(SINAN_LEPTO_2013 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_RENAL == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_LEPTO_2013 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_RESPIR == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 16] <- as.integer(SINAN_LEPTO_2013 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_CARDIA == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 17] <- as.integer(SINAN_LEPTO_2013 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_HEMOPU == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 18] <- as.integer(SINAN_LEPTO_2013 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_HEMORR == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 19] <- as.integer(SINAN_LEPTO_2013 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_MENING == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 20] <- as.integer(SINAN_LEPTO_2013 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_OUTROS == 1) %>% 
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2013_SINTOMAS"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2013_SINTOMAS"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2013_SINTOMAS.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2013 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ATE_HOSP == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2013 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ATE_HOSP == 2) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2013_HOSPITALIZACAO"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2013_HOSPITALIZACAO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2013_HOSPITALIZACAO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2013 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CLASSI_FIN == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2013 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CLASSI_FIN == 2) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2013_CLASSI_FIN"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2013_CLASSI_FIN"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2013_CLASSI_FIN.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2013 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CRITERIO == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2013 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CRITERIO == 2) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2013_CRIT_ENCER"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2013_CRIT_ENCER"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2013_CRIT_ENCER.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2013 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CON_AREA == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2013 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AREA == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2013 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AREA == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2013 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AREA == 9) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4: ncol(AUX)] <- apply(AUX[, 4: ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2013_LPI"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2013_LPI"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2013_LPI.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2013 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CON_AMBIEN == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2013 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2013 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2013 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 4) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_LEPTO_2013 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 9) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4: ncol(AUX)] <- apply(AUX[, 4: ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2013_AMBIENTE"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2013_AMBIENTE"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2013_AMBIENTE.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2013 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CON_AMBIEN == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2013 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2013 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 9) %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4: ncol(AUX)] <- apply(AUX[, 4: ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2013_TRABALHO"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2013_TRABALHO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2013_TRABALHO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2013 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          EVOLUCAO == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2013 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            EVOLUCAO == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2013 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            EVOLUCAO == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2013 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            EVOLUCAO == 9) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4: ncol(AUX)] <- apply(AUX[, 4: ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2013_EVOLUCAO"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2013_EVOLUCAO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2013_EVOLUCAO.csv"), 
           row.names = FALSE)

rm(SINAN_LEPTO_2013)

##################################################################################
#################   2014    ######################################################
##################################################################################

SINAN_LEPTO_2014 <- LEPTONET %>% filter(ID_REGIONA == ID_REG | ID_RG_RESI == ID_REG,
                                        NU_ANO == 2014)

assign(paste0("RS", RS, "_LEPTO_2014_SINAN"), SINAN_LEPTO_2014)

write.csv (assign(paste0("RS", RS, "_LEPTO_2014_SINAN"), SINAN_LEPTO_2014), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2014_SINAN.csv"), 
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
  
  AUX[which(AUX == i), 2] <- as.integer(SINAN_LEPTO_2014 %>%
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==201401)%>%
                                          count()
  )
  
  AUX[which(AUX == i), 3] <- as.integer(SINAN_LEPTO_2014 %>% 
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==201402) %>% 
                                          count()
  )
  
  AUX[which(AUX == i), 4] <- as.integer(SINAN_LEPTO_2014 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==201403) %>% 
                                          count()
  )
  
  AUX[which(AUX == i),5] <- as.integer(SINAN_LEPTO_2014 %>% 
                                         filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==201404) %>% 
                                         count()
  )
  
  AUX[which(AUX == i), 6] <- as.integer(SINAN_LEPTO_2014 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==201405) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 7] <- as.integer(SINAN_LEPTO_2014 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==201406) %>%
                                          count()
  )
  
  AUX[which(AUX == i), 8] <- as.integer(SINAN_LEPTO_2014 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==201407) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 9] <- as.integer(SINAN_LEPTO_2014 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==201408) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 10] <- as.integer(SINAN_LEPTO_2014 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201409) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 11] <- as.integer(SINAN_LEPTO_2014 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201410) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 12] <- as.integer(SINAN_LEPTO_2014 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201411) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 13] <- as.integer(SINAN_LEPTO_2014 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201412) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 14] <- as.integer(SINAN_LEPTO_2014 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201413) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 15] <- as.integer(SINAN_LEPTO_2014 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201414) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 16] <- as.integer(SINAN_LEPTO_2014 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201415) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 17] <- as.integer(SINAN_LEPTO_2014 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201416) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 18] <- as.integer(SINAN_LEPTO_2014 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201417) %>%  
                                           count() 
  )
  
  AUX[which(AUX == i), 19] <- as.integer(SINAN_LEPTO_2014 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201418) %>%      
                                           count() 
  )
  
  AUX[which(AUX == i), 20] <- as.integer(SINAN_LEPTO_2014 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201419) %>%
                                           count() 
  )
  
  AUX[which(AUX == i),  21] <- as.integer(SINAN_LEPTO_2014 %>% 
                                            filter(ID_MN_RESI ==i & 
                                                     SEM_PRI ==201420) %>%
                                            count() 
  )
  
  AUX[which(AUX == i), 22] <- as.integer(SINAN_LEPTO_2014 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201421) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 23] <- as.integer(SINAN_LEPTO_2014 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201422) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 24] <- as.integer(SINAN_LEPTO_2014 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201423) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 25] <- as.integer(SINAN_LEPTO_2014 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201424) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 26] <- as.integer(SINAN_LEPTO_2014 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201425) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 27] <- as.integer(SINAN_LEPTO_2014 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201426) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 28] <- as.integer(SINAN_LEPTO_2014 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201427) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 29] <- as.integer(SINAN_LEPTO_2014 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201428) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 30] <- as.integer(SINAN_LEPTO_2014 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201429) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 31] <- as.integer(SINAN_LEPTO_2014 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201430) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 32] <- as.integer(SINAN_LEPTO_2014 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201431) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 33] <- as.integer(SINAN_LEPTO_2014 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201432) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 34] <- as.integer(SINAN_LEPTO_2014 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201433) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 35] <- as.integer(SINAN_LEPTO_2014 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201434) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 36] <- as.integer(SINAN_LEPTO_2014 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201435) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 37] <- as.integer(SINAN_LEPTO_2014 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201436) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 38] <- as.integer(SINAN_LEPTO_2014 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201437) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 39] <- as.integer(SINAN_LEPTO_2014 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201438) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 40] <- as.integer(SINAN_LEPTO_2014 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201439) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 41] <- as.integer(SINAN_LEPTO_2014 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201440) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 42] <- as.integer(SINAN_LEPTO_2014 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201441) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 43] <- as.integer(SINAN_LEPTO_2014 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201442) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 44] <- as.integer(SINAN_LEPTO_2014 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201443) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 45] <- as.integer(SINAN_LEPTO_2014 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201444) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 46] <- as.integer(SINAN_LEPTO_2014 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201445) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 47] <- as.integer(SINAN_LEPTO_2014 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201446) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 48] <- as.integer(SINAN_LEPTO_2014 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201447) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 49] <- as.integer(SINAN_LEPTO_2014 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201448) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 50] <- as.integer(SINAN_LEPTO_2014 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201449) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 51] <- as.integer(SINAN_LEPTO_2014 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201450) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 52] <- as.integer(SINAN_LEPTO_2014 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201451) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 53] <- as.integer(SINAN_LEPTO_2014 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201452) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 54] <- as.integer(SINAN_LEPTO_2014 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201453) %>%
                                           count() 
  )
}

AUX[, 1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX[(nrow(AUX)+ 1), 2:54] <- apply(AUX[, 2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2014_SE_Notificados"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2014_SE_Notificados"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2014_SE_Notificados.csv"), 
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
  
  AUX[which(AUX == i), 2] <- as.integer(SINAN_LEPTO_2014 %>%
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==201401,
                                                 CLASSI_FIN == 1)%>%
                                          count()
  )
  
  AUX[which(AUX == i), 3] <- as.integer(SINAN_LEPTO_2014 %>% 
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==201402,
                                                 CLASSI_FIN == 1) %>% 
                                          count()
  )
  
  AUX[which(AUX == i), 4] <- as.integer(SINAN_LEPTO_2014 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==201403,
                                                 CLASSI_FIN == 1) %>% 
                                          count()
  )
  
  AUX[which(AUX == i),5] <- as.integer(SINAN_LEPTO_2014 %>% 
                                         filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==201404,
                                                CLASSI_FIN == 1) %>% 
                                         count()
  )
  
  AUX[which(AUX == i), 6] <- as.integer(SINAN_LEPTO_2014 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==201405,
                                                 CLASSI_FIN == 1) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 7] <- as.integer(SINAN_LEPTO_2014 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==201406,
                                                 CLASSI_FIN == 1) %>%
                                          count()
  )
  
  AUX[which(AUX == i), 8] <- as.integer(SINAN_LEPTO_2014 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==201407,
                                                 CLASSI_FIN == 1) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 9] <- as.integer(SINAN_LEPTO_2014 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==201408,
                                                 CLASSI_FIN == 1) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 10] <- as.integer(SINAN_LEPTO_2014 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201409,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 11] <- as.integer(SINAN_LEPTO_2014 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201410,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 12] <- as.integer(SINAN_LEPTO_2014 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201411,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 13] <- as.integer(SINAN_LEPTO_2014 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201412,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 14] <- as.integer(SINAN_LEPTO_2014 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201413,
                                                  CLASSI_FIN == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 15] <- as.integer(SINAN_LEPTO_2014 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201414,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 16] <- as.integer(SINAN_LEPTO_2014 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201415,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 17] <- as.integer(SINAN_LEPTO_2014 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201416,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 18] <- as.integer(SINAN_LEPTO_2014 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201417,
                                                  CLASSI_FIN == 1) %>%  
                                           count() 
  )
  
  AUX[which(AUX == i), 19] <- as.integer(SINAN_LEPTO_2014 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201418,
                                                  CLASSI_FIN == 1) %>%      
                                           count() 
  )
  
  AUX[which(AUX == i), 20] <- as.integer(SINAN_LEPTO_2014 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201419,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i),  21] <- as.integer(SINAN_LEPTO_2014 %>% 
                                            filter(ID_MN_RESI ==i & 
                                                     SEM_PRI ==201420,
                                                   CLASSI_FIN == 1) %>%
                                            count() 
  )
  
  AUX[which(AUX == i), 22] <- as.integer(SINAN_LEPTO_2014 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201421,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 23] <- as.integer(SINAN_LEPTO_2014 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201422,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 24] <- as.integer(SINAN_LEPTO_2014 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201423,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 25] <- as.integer(SINAN_LEPTO_2014 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201424,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 26] <- as.integer(SINAN_LEPTO_2014 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201425,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 27] <- as.integer(SINAN_LEPTO_2014 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201426,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 28] <- as.integer(SINAN_LEPTO_2014 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201427,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 29] <- as.integer(SINAN_LEPTO_2014 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201428,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 30] <- as.integer(SINAN_LEPTO_2014 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201429,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 31] <- as.integer(SINAN_LEPTO_2014 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201430,
                                                  CLASSI_FIN == 1) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 32] <- as.integer(SINAN_LEPTO_2014 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201431,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 33] <- as.integer(SINAN_LEPTO_2014 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201432,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 34] <- as.integer(SINAN_LEPTO_2014 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201433,
                                                  CLASSI_FIN == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 35] <- as.integer(SINAN_LEPTO_2014 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201434,
                                                  CLASSI_FIN == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 36] <- as.integer(SINAN_LEPTO_2014 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201435,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 37] <- as.integer(SINAN_LEPTO_2014 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201436,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 38] <- as.integer(SINAN_LEPTO_2014 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201437,
                                                  CLASSI_FIN == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 39] <- as.integer(SINAN_LEPTO_2014 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201438,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 40] <- as.integer(SINAN_LEPTO_2014 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201439,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 41] <- as.integer(SINAN_LEPTO_2014 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201440,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 42] <- as.integer(SINAN_LEPTO_2014 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201441,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 43] <- as.integer(SINAN_LEPTO_2014 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201442,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 44] <- as.integer(SINAN_LEPTO_2014 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201443,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 45] <- as.integer(SINAN_LEPTO_2014 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201444,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 46] <- as.integer(SINAN_LEPTO_2014 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201445,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 47] <- as.integer(SINAN_LEPTO_2014 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201446,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 48] <- as.integer(SINAN_LEPTO_2014 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201447,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 49] <- as.integer(SINAN_LEPTO_2014 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201448,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 50] <- as.integer(SINAN_LEPTO_2014 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201449,
                                                  CLASSI_FIN == 1) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 51] <- as.integer(SINAN_LEPTO_2014 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201450,
                                                  CLASSI_FIN == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 52] <- as.integer(SINAN_LEPTO_2014 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201451,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 53] <- as.integer(SINAN_LEPTO_2014 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201452,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 54] <- as.integer(SINAN_LEPTO_2014 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201453,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
}

AUX[, 1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX[(nrow(AUX)+ 1), 2:54] <- apply(AUX[, 2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2014_SE_Confirmados"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2014_SE_Confirmados"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2014_SE_Confirmados.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2014 %>% 
                                                   filter(ID_MN_RESI == i) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2014 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CLASSI_FIN == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2014 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CS_ZONA == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2014 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CS_ZONA == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_LEPTO_2014 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CS_ZONA == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_LEPTO_2014 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ZONA == 9 
                                                           |
                                                             is.na(CS_ZONA)) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_LEPTO_2014 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_SEXO == "F") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(SINAN_LEPTO_2014 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_SEXO == "M") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_LEPTO_2014 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             NU_IDADE_N <= 3012) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(SINAN_LEPTO_2014 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             NU_IDADE_N > 4000 
                                                           & 
                                                             NU_IDADE_N <= 4005) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_LEPTO_2014 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             NU_IDADE_N > 4005 
                                                           & 
                                                             NU_IDADE_N <= 4012) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 16] <- as.integer(SINAN_LEPTO_2014 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             NU_IDADE_N > 4012 
                                                           & 
                                                             NU_IDADE_N <= 4018) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 17] <- as.integer(SINAN_LEPTO_2014 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             NU_IDADE_N > 4018 
                                                           & 
                                                             NU_IDADE_N <= 4059) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 18] <- as.integer(SINAN_LEPTO_2014 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             NU_IDADE_N > 4059) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 19] <- as.integer(SINAN_LEPTO_2014 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "00") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 20] <- as.integer(SINAN_LEPTO_2014 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "01" 
                                                           | 
                                                             CS_ESCOL_N == "02" 
                                                           | 
                                                             CS_ESCOL_N == "03") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 21] <- as.integer(SINAN_LEPTO_2014 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "04") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 22] <- as.integer(SINAN_LEPTO_2014 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "05") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 23] <- as.integer(SINAN_LEPTO_2014 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "06") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 24] <- as.integer(SINAN_LEPTO_2014 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "07") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 25] <- as.integer(SINAN_LEPTO_2014 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "08") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 26] <- as.integer(SINAN_LEPTO_2014 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "10") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 27] <- as.integer(SINAN_LEPTO_2014 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "09") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 28] <- as.integer(SINAN_LEPTO_2014 %>% 
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
  
  AUX[which(AUX$COD_IBGE == i), 29] <- as.integer(SINAN_LEPTO_2014 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_GESTANT == "5") %>% 
                                                    count()
  )
  AUX[which(AUX$COD_IBGE == i), 30] <- as.integer(SINAN_LEPTO_2014 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_GESTANT == "6") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 31] <- as.integer(SINAN_LEPTO_2014 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_GESTANT == "9") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 32] <- as.integer(SINAN_LEPTO_2014 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "01") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 33] <- as.integer(SINAN_LEPTO_2014 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "2") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 34] <- as.integer(SINAN_LEPTO_2014 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "3") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 35] <- as.integer(SINAN_LEPTO_2014 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "4") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 36] <- as.integer(SINAN_LEPTO_2014 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "5") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 37] <- as.integer(SINAN_LEPTO_2014 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "9") %>% 
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2014_GERAL"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2014_GERAL"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2014_GERAL.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2014 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_CB_LAM == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2014 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_CB_CRI == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2014 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_CB_CAI == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2014 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_CB_FOS == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_LEPTO_2014 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_CB_SIN == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_LEPTO_2014 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_CB_PLA == 1) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_LEPTO_2014 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_CB_COR == 1) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(SINAN_LEPTO_2014 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             ANT_CB_ROE == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_LEPTO_2014 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             ANT_CB_GRA == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(SINAN_LEPTO_2014 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_CB_TER == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_LEPTO_2014 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_CB_LIX == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 16] <- as.integer(SINAN_LEPTO_2014 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_CB_OUT == 4018) %>% 
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2014_SIT_RISCO"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2014_SIT_RISCO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2014_SIT_RISCO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2014 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CLI_FEBRE == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2014 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CLI_MIALGI == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2014 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CLI_CEFALE == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2014 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CLI_PROST == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_LEPTO_2014 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CLI_CONGES == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_LEPTO_2014 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_PANTUR == 1) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_LEPTO_2014 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_VOMITO == 1) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(SINAN_LEPTO_2014 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             CLI_DIARRE == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_LEPTO_2014 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             CLI_ICTERI == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(SINAN_LEPTO_2014 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_RENAL == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_LEPTO_2014 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_RESPIR == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 16] <- as.integer(SINAN_LEPTO_2014 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_CARDIA == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 17] <- as.integer(SINAN_LEPTO_2014 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_HEMOPU == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 18] <- as.integer(SINAN_LEPTO_2014 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_HEMORR == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 19] <- as.integer(SINAN_LEPTO_2014 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_MENING == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 20] <- as.integer(SINAN_LEPTO_2014 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_OUTROS == 1) %>% 
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2014_SINTOMAS"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2014_SINTOMAS"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2014_SINTOMAS.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2014 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ATE_HOSP == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2014 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ATE_HOSP == 2) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2014_HOSPITALIZACAO"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2014_HOSPITALIZACAO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2014_HOSPITALIZACAO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2014 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CLASSI_FIN == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2014 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CLASSI_FIN == 2) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2014_CLASSI_FIN"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2014_CLASSI_FIN"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2014_CLASSI_FIN.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2014 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CRITERIO == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2014 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CRITERIO == 2) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2014_CRIT_ENCER"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2014_CRIT_ENCER"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2014_CRIT_ENCER.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2014 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CON_AREA == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2014 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AREA == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2014 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AREA == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2014 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AREA == 9) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4: ncol(AUX)] <- apply(AUX[, 4: ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2014_LPI"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2014_LPI"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2014_LPI.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2014 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CON_AMBIEN == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2014 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2014 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2014 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 4) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_LEPTO_2014 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 9) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4: ncol(AUX)] <- apply(AUX[, 4: ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2014_AMBIENTE"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2014_AMBIENTE"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2014_AMBIENTE.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2014 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CON_AMBIEN == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2014 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2014 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 9) %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4: ncol(AUX)] <- apply(AUX[, 4: ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2014_TRABALHO"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2014_TRABALHO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2014_TRABALHO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2014 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          EVOLUCAO == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2014 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            EVOLUCAO == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2014 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            EVOLUCAO == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2014 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            EVOLUCAO == 9) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4: ncol(AUX)] <- apply(AUX[, 4: ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2014_EVOLUCAO"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2014_EVOLUCAO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2014_EVOLUCAO.csv"), 
           row.names = FALSE)

rm(SINAN_LEPTO_2014)

##################################################################################
#################   2015    ######################################################
##################################################################################

SINAN_LEPTO_2015 <- LEPTONET %>% filter(ID_REGIONA == ID_REG | ID_RG_RESI == ID_REG,
                                        NU_ANO == 2015)

assign(paste0("RS", RS, "_LEPTO_2015_SINAN"), SINAN_LEPTO_2015)

write.csv (assign(paste0("RS", RS, "_LEPTO_2015_SINAN"), SINAN_LEPTO_2015), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2015_SINAN.csv"), 
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
  
  AUX[which(AUX == i), 2] <- as.integer(SINAN_LEPTO_2015 %>%
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==201501)%>%
                                          count()
  )
  
  AUX[which(AUX == i), 3] <- as.integer(SINAN_LEPTO_2015 %>% 
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==201502) %>% 
                                          count()
  )
  
  AUX[which(AUX == i), 4] <- as.integer(SINAN_LEPTO_2015 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==201503) %>% 
                                          count()
  )
  
  AUX[which(AUX == i),5] <- as.integer(SINAN_LEPTO_2015 %>% 
                                         filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==201504) %>% 
                                         count()
  )
  
  AUX[which(AUX == i), 6] <- as.integer(SINAN_LEPTO_2015 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==201505) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 7] <- as.integer(SINAN_LEPTO_2015 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==201506) %>%
                                          count()
  )
  
  AUX[which(AUX == i), 8] <- as.integer(SINAN_LEPTO_2015 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==201507) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 9] <- as.integer(SINAN_LEPTO_2015 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==201508) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 10] <- as.integer(SINAN_LEPTO_2015 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201509) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 11] <- as.integer(SINAN_LEPTO_2015 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201510) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 12] <- as.integer(SINAN_LEPTO_2015 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201511) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 13] <- as.integer(SINAN_LEPTO_2015 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201512) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 14] <- as.integer(SINAN_LEPTO_2015 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201513) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 15] <- as.integer(SINAN_LEPTO_2015 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201514) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 16] <- as.integer(SINAN_LEPTO_2015 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201515) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 17] <- as.integer(SINAN_LEPTO_2015 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201516) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 18] <- as.integer(SINAN_LEPTO_2015 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201517) %>%  
                                           count() 
  )
  
  AUX[which(AUX == i), 19] <- as.integer(SINAN_LEPTO_2015 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201518) %>%      
                                           count() 
  )
  
  AUX[which(AUX == i), 20] <- as.integer(SINAN_LEPTO_2015 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201519) %>%
                                           count() 
  )
  
  AUX[which(AUX == i),  21] <- as.integer(SINAN_LEPTO_2015 %>% 
                                            filter(ID_MN_RESI ==i & 
                                                     SEM_PRI ==201520) %>%
                                            count() 
  )
  
  AUX[which(AUX == i), 22] <- as.integer(SINAN_LEPTO_2015 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201521) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 23] <- as.integer(SINAN_LEPTO_2015 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201522) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 24] <- as.integer(SINAN_LEPTO_2015 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201523) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 25] <- as.integer(SINAN_LEPTO_2015 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201524) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 26] <- as.integer(SINAN_LEPTO_2015 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201525) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 27] <- as.integer(SINAN_LEPTO_2015 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201526) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 28] <- as.integer(SINAN_LEPTO_2015 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201527) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 29] <- as.integer(SINAN_LEPTO_2015 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201528) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 30] <- as.integer(SINAN_LEPTO_2015 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201529) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 31] <- as.integer(SINAN_LEPTO_2015 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201530) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 32] <- as.integer(SINAN_LEPTO_2015 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201531) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 33] <- as.integer(SINAN_LEPTO_2015 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201532) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 34] <- as.integer(SINAN_LEPTO_2015 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201533) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 35] <- as.integer(SINAN_LEPTO_2015 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201534) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 36] <- as.integer(SINAN_LEPTO_2015 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201535) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 37] <- as.integer(SINAN_LEPTO_2015 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201536) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 38] <- as.integer(SINAN_LEPTO_2015 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201537) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 39] <- as.integer(SINAN_LEPTO_2015 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201538) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 40] <- as.integer(SINAN_LEPTO_2015 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201539) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 41] <- as.integer(SINAN_LEPTO_2015 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201540) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 42] <- as.integer(SINAN_LEPTO_2015 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201541) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 43] <- as.integer(SINAN_LEPTO_2015 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201542) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 44] <- as.integer(SINAN_LEPTO_2015 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201543) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 45] <- as.integer(SINAN_LEPTO_2015 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201544) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 46] <- as.integer(SINAN_LEPTO_2015 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201545) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 47] <- as.integer(SINAN_LEPTO_2015 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201546) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 48] <- as.integer(SINAN_LEPTO_2015 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201547) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 49] <- as.integer(SINAN_LEPTO_2015 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201548) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 50] <- as.integer(SINAN_LEPTO_2015 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201549) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 51] <- as.integer(SINAN_LEPTO_2015 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201550) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 52] <- as.integer(SINAN_LEPTO_2015 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201551) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 53] <- as.integer(SINAN_LEPTO_2015 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201552) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 54] <- as.integer(SINAN_LEPTO_2015 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201553) %>%
                                           count() 
  )
}

AUX[, 1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX[(nrow(AUX)+ 1), 2:54] <- apply(AUX[, 2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2015_SE_Notificados"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2015_SE_Notificados"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2015_SE_Notificados.csv"), 
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
  
  AUX[which(AUX == i), 2] <- as.integer(SINAN_LEPTO_2015 %>%
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==201501,
                                                 CLASSI_FIN == 1)%>%
                                          count()
  )
  
  AUX[which(AUX == i), 3] <- as.integer(SINAN_LEPTO_2015 %>% 
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==201502,
                                                 CLASSI_FIN == 1) %>% 
                                          count()
  )
  
  AUX[which(AUX == i), 4] <- as.integer(SINAN_LEPTO_2015 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==201503,
                                                 CLASSI_FIN == 1) %>% 
                                          count()
  )
  
  AUX[which(AUX == i),5] <- as.integer(SINAN_LEPTO_2015 %>% 
                                         filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==201504,
                                                CLASSI_FIN == 1) %>% 
                                         count()
  )
  
  AUX[which(AUX == i), 6] <- as.integer(SINAN_LEPTO_2015 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==201505,
                                                 CLASSI_FIN == 1) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 7] <- as.integer(SINAN_LEPTO_2015 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==201506,
                                                 CLASSI_FIN == 1) %>%
                                          count()
  )
  
  AUX[which(AUX == i), 8] <- as.integer(SINAN_LEPTO_2015 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==201507,
                                                 CLASSI_FIN == 1) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 9] <- as.integer(SINAN_LEPTO_2015 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==201508,
                                                 CLASSI_FIN == 1) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 10] <- as.integer(SINAN_LEPTO_2015 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201509,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 11] <- as.integer(SINAN_LEPTO_2015 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201510,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 12] <- as.integer(SINAN_LEPTO_2015 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201511,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 13] <- as.integer(SINAN_LEPTO_2015 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201512,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 14] <- as.integer(SINAN_LEPTO_2015 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201513,
                                                  CLASSI_FIN == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 15] <- as.integer(SINAN_LEPTO_2015 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201514,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 16] <- as.integer(SINAN_LEPTO_2015 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201515,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 17] <- as.integer(SINAN_LEPTO_2015 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201516,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 18] <- as.integer(SINAN_LEPTO_2015 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201517,
                                                  CLASSI_FIN == 1) %>%  
                                           count() 
  )
  
  AUX[which(AUX == i), 19] <- as.integer(SINAN_LEPTO_2015 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201518,
                                                  CLASSI_FIN == 1) %>%      
                                           count() 
  )
  
  AUX[which(AUX == i), 20] <- as.integer(SINAN_LEPTO_2015 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201519,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i),  21] <- as.integer(SINAN_LEPTO_2015 %>% 
                                            filter(ID_MN_RESI ==i & 
                                                     SEM_PRI ==201520,
                                                   CLASSI_FIN == 1) %>%
                                            count() 
  )
  
  AUX[which(AUX == i), 22] <- as.integer(SINAN_LEPTO_2015 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201521,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 23] <- as.integer(SINAN_LEPTO_2015 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201522,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 24] <- as.integer(SINAN_LEPTO_2015 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201523,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 25] <- as.integer(SINAN_LEPTO_2015 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201524,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 26] <- as.integer(SINAN_LEPTO_2015 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201525,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 27] <- as.integer(SINAN_LEPTO_2015 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201526,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 28] <- as.integer(SINAN_LEPTO_2015 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201527,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 29] <- as.integer(SINAN_LEPTO_2015 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201528,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 30] <- as.integer(SINAN_LEPTO_2015 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201529,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 31] <- as.integer(SINAN_LEPTO_2015 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201530,
                                                  CLASSI_FIN == 1) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 32] <- as.integer(SINAN_LEPTO_2015 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201531,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 33] <- as.integer(SINAN_LEPTO_2015 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201532,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 34] <- as.integer(SINAN_LEPTO_2015 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201533,
                                                  CLASSI_FIN == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 35] <- as.integer(SINAN_LEPTO_2015 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201534,
                                                  CLASSI_FIN == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 36] <- as.integer(SINAN_LEPTO_2015 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201535,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 37] <- as.integer(SINAN_LEPTO_2015 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201536,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 38] <- as.integer(SINAN_LEPTO_2015 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201537,
                                                  CLASSI_FIN == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 39] <- as.integer(SINAN_LEPTO_2015 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201538,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 40] <- as.integer(SINAN_LEPTO_2015 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201539,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 41] <- as.integer(SINAN_LEPTO_2015 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201540,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 42] <- as.integer(SINAN_LEPTO_2015 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201541,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 43] <- as.integer(SINAN_LEPTO_2015 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201542,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 44] <- as.integer(SINAN_LEPTO_2015 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201543,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 45] <- as.integer(SINAN_LEPTO_2015 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201544,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 46] <- as.integer(SINAN_LEPTO_2015 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201545,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 47] <- as.integer(SINAN_LEPTO_2015 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201546,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 48] <- as.integer(SINAN_LEPTO_2015 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201547,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 49] <- as.integer(SINAN_LEPTO_2015 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201548,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 50] <- as.integer(SINAN_LEPTO_2015 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201549,
                                                  CLASSI_FIN == 1) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 51] <- as.integer(SINAN_LEPTO_2015 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201550,
                                                  CLASSI_FIN == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 52] <- as.integer(SINAN_LEPTO_2015 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201551,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 53] <- as.integer(SINAN_LEPTO_2015 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201552,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 54] <- as.integer(SINAN_LEPTO_2015 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201553,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
}

AUX[, 1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX[(nrow(AUX)+ 1), 2:54] <- apply(AUX[, 2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2015_SE_Confirmados"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2015_SE_Confirmados"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2015_SE_Confirmados.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2015 %>% 
                                                   filter(ID_MN_RESI == i) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2015 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CLASSI_FIN == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2015 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CS_ZONA == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2015 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CS_ZONA == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_LEPTO_2015 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CS_ZONA == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_LEPTO_2015 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ZONA == 9 
                                                           |
                                                             is.na(CS_ZONA)) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_LEPTO_2015 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_SEXO == "F") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(SINAN_LEPTO_2015 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_SEXO == "M") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_LEPTO_2015 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             NU_IDADE_N <= 3012) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(SINAN_LEPTO_2015 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             NU_IDADE_N > 4000 
                                                           & 
                                                             NU_IDADE_N <= 4005) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_LEPTO_2015 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             NU_IDADE_N > 4005 
                                                           & 
                                                             NU_IDADE_N <= 4012) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 16] <- as.integer(SINAN_LEPTO_2015 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             NU_IDADE_N > 4012 
                                                           & 
                                                             NU_IDADE_N <= 4018) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 17] <- as.integer(SINAN_LEPTO_2015 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             NU_IDADE_N > 4018 
                                                           & 
                                                             NU_IDADE_N <= 4059) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 18] <- as.integer(SINAN_LEPTO_2015 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             NU_IDADE_N > 4059) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 19] <- as.integer(SINAN_LEPTO_2015 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "00") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 20] <- as.integer(SINAN_LEPTO_2015 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "01" 
                                                           | 
                                                             CS_ESCOL_N == "02" 
                                                           | 
                                                             CS_ESCOL_N == "03") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 21] <- as.integer(SINAN_LEPTO_2015 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "04") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 22] <- as.integer(SINAN_LEPTO_2015 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "05") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 23] <- as.integer(SINAN_LEPTO_2015 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "06") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 24] <- as.integer(SINAN_LEPTO_2015 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "07") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 25] <- as.integer(SINAN_LEPTO_2015 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "08") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 26] <- as.integer(SINAN_LEPTO_2015 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "10") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 27] <- as.integer(SINAN_LEPTO_2015 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "09") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 28] <- as.integer(SINAN_LEPTO_2015 %>% 
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
  
  AUX[which(AUX$COD_IBGE == i), 29] <- as.integer(SINAN_LEPTO_2015 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_GESTANT == "5") %>% 
                                                    count()
  )
  AUX[which(AUX$COD_IBGE == i), 30] <- as.integer(SINAN_LEPTO_2015 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_GESTANT == "6") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 31] <- as.integer(SINAN_LEPTO_2015 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_GESTANT == "9") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 32] <- as.integer(SINAN_LEPTO_2015 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "01") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 33] <- as.integer(SINAN_LEPTO_2015 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "2") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 34] <- as.integer(SINAN_LEPTO_2015 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "3") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 35] <- as.integer(SINAN_LEPTO_2015 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "4") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 36] <- as.integer(SINAN_LEPTO_2015 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "5") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 37] <- as.integer(SINAN_LEPTO_2015 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "9") %>% 
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2015_GERAL"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2015_GERAL"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2015_GERAL.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2015 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_CB_LAM == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2015 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_CB_CRI == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2015 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_CB_CAI == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2015 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_CB_FOS == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_LEPTO_2015 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_CB_SIN == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_LEPTO_2015 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_CB_PLA == 1) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_LEPTO_2015 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_CB_COR == 1) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(SINAN_LEPTO_2015 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             ANT_CB_ROE == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_LEPTO_2015 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             ANT_CB_GRA == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(SINAN_LEPTO_2015 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_CB_TER == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_LEPTO_2015 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_CB_LIX == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 16] <- as.integer(SINAN_LEPTO_2015 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_CB_OUT == 4018) %>% 
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2015_SIT_RISCO"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2015_SIT_RISCO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2015_SIT_RISCO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2015 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CLI_FEBRE == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2015 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CLI_MIALGI == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2015 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CLI_CEFALE == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2015 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CLI_PROST == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_LEPTO_2015 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CLI_CONGES == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_LEPTO_2015 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_PANTUR == 1) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_LEPTO_2015 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_VOMITO == 1) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(SINAN_LEPTO_2015 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             CLI_DIARRE == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_LEPTO_2015 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             CLI_ICTERI == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(SINAN_LEPTO_2015 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_RENAL == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_LEPTO_2015 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_RESPIR == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 16] <- as.integer(SINAN_LEPTO_2015 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_CARDIA == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 17] <- as.integer(SINAN_LEPTO_2015 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_HEMOPU == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 18] <- as.integer(SINAN_LEPTO_2015 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_HEMORR == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 19] <- as.integer(SINAN_LEPTO_2015 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_MENING == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 20] <- as.integer(SINAN_LEPTO_2015 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_OUTROS == 1) %>% 
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2015_SINTOMAS"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2015_SINTOMAS"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2015_SINTOMAS.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2015 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ATE_HOSP == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2015 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ATE_HOSP == 2) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2015_HOSPITALIZACAO"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2015_HOSPITALIZACAO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2015_HOSPITALIZACAO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2015 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CLASSI_FIN == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2015 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CLASSI_FIN == 2) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2015_CLASSI_FIN"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2015_CLASSI_FIN"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2015_CLASSI_FIN.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2015 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CRITERIO == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2015 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CRITERIO == 2) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2015_CRIT_ENCER"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2015_CRIT_ENCER"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2015_CRIT_ENCER.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2015 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CON_AREA == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2015 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AREA == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2015 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AREA == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2015 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AREA == 9) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4: ncol(AUX)] <- apply(AUX[, 4: ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2015_LPI"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2015_LPI"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2015_LPI.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2015 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CON_AMBIEN == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2015 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2015 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2015 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 4) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_LEPTO_2015 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 9) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4: ncol(AUX)] <- apply(AUX[, 4: ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2015_AMBIENTE"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2015_AMBIENTE"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2015_AMBIENTE.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2015 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CON_AMBIEN == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2015 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2015 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 9) %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4: ncol(AUX)] <- apply(AUX[, 4: ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2015_TRABALHO"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2015_TRABALHO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2015_TRABALHO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2015 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          EVOLUCAO == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2015 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            EVOLUCAO == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2015 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            EVOLUCAO == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2015 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            EVOLUCAO == 9) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4: ncol(AUX)] <- apply(AUX[, 4: ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2015_EVOLUCAO"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2015_EVOLUCAO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2015_EVOLUCAO.csv"), 
           row.names = FALSE)

rm(SINAN_LEPTO_2015)

##################################################################################
#################   2016    ######################################################
##################################################################################

SINAN_LEPTO_2016 <- LEPTONET %>% filter(ID_REGIONA == ID_REG | ID_RG_RESI == ID_REG,
                                        NU_ANO == 2016)

assign(paste0("RS", RS, "_LEPTO_2016_SINAN"), SINAN_LEPTO_2016)

write.csv (assign(paste0("RS", RS, "_LEPTO_2016_SINAN"), SINAN_LEPTO_2016), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2016_SINAN.csv"), 
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
  
  AUX[which(AUX == i), 2] <- as.integer(SINAN_LEPTO_2016 %>%
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==201601)%>%
                                          count()
  )
  
  AUX[which(AUX == i), 3] <- as.integer(SINAN_LEPTO_2016 %>% 
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==201602) %>% 
                                          count()
  )
  
  AUX[which(AUX == i), 4] <- as.integer(SINAN_LEPTO_2016 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==201603) %>% 
                                          count()
  )
  
  AUX[which(AUX == i),5] <- as.integer(SINAN_LEPTO_2016 %>% 
                                         filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==201604) %>% 
                                         count()
  )
  
  AUX[which(AUX == i), 6] <- as.integer(SINAN_LEPTO_2016 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==201605) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 7] <- as.integer(SINAN_LEPTO_2016 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==201606) %>%
                                          count()
  )
  
  AUX[which(AUX == i), 8] <- as.integer(SINAN_LEPTO_2016 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==201607) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 9] <- as.integer(SINAN_LEPTO_2016 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==201608) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 10] <- as.integer(SINAN_LEPTO_2016 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201609) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 11] <- as.integer(SINAN_LEPTO_2016 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201610) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 12] <- as.integer(SINAN_LEPTO_2016 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201611) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 13] <- as.integer(SINAN_LEPTO_2016 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201612) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 14] <- as.integer(SINAN_LEPTO_2016 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201613) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 15] <- as.integer(SINAN_LEPTO_2016 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201614) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 16] <- as.integer(SINAN_LEPTO_2016 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201615) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 17] <- as.integer(SINAN_LEPTO_2016 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201616) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 18] <- as.integer(SINAN_LEPTO_2016 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201617) %>%  
                                           count() 
  )
  
  AUX[which(AUX == i), 19] <- as.integer(SINAN_LEPTO_2016 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201618) %>%      
                                           count() 
  )
  
  AUX[which(AUX == i), 20] <- as.integer(SINAN_LEPTO_2016 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201619) %>%
                                           count() 
  )
  
  AUX[which(AUX == i),  21] <- as.integer(SINAN_LEPTO_2016 %>% 
                                            filter(ID_MN_RESI ==i & 
                                                     SEM_PRI ==201620) %>%
                                            count() 
  )
  
  AUX[which(AUX == i), 22] <- as.integer(SINAN_LEPTO_2016 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201621) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 23] <- as.integer(SINAN_LEPTO_2016 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201622) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 24] <- as.integer(SINAN_LEPTO_2016 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201623) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 25] <- as.integer(SINAN_LEPTO_2016 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201624) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 26] <- as.integer(SINAN_LEPTO_2016 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201625) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 27] <- as.integer(SINAN_LEPTO_2016 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201626) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 28] <- as.integer(SINAN_LEPTO_2016 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201627) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 29] <- as.integer(SINAN_LEPTO_2016 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201628) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 30] <- as.integer(SINAN_LEPTO_2016 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201629) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 31] <- as.integer(SINAN_LEPTO_2016 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201630) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 32] <- as.integer(SINAN_LEPTO_2016 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201631) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 33] <- as.integer(SINAN_LEPTO_2016 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201632) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 34] <- as.integer(SINAN_LEPTO_2016 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201633) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 35] <- as.integer(SINAN_LEPTO_2016 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201634) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 36] <- as.integer(SINAN_LEPTO_2016 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201635) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 37] <- as.integer(SINAN_LEPTO_2016 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201636) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 38] <- as.integer(SINAN_LEPTO_2016 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201637) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 39] <- as.integer(SINAN_LEPTO_2016 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201638) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 40] <- as.integer(SINAN_LEPTO_2016 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201639) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 41] <- as.integer(SINAN_LEPTO_2016 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201640) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 42] <- as.integer(SINAN_LEPTO_2016 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201641) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 43] <- as.integer(SINAN_LEPTO_2016 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201642) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 44] <- as.integer(SINAN_LEPTO_2016 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201643) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 45] <- as.integer(SINAN_LEPTO_2016 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201644) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 46] <- as.integer(SINAN_LEPTO_2016 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201645) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 47] <- as.integer(SINAN_LEPTO_2016 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201646) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 48] <- as.integer(SINAN_LEPTO_2016 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201647) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 49] <- as.integer(SINAN_LEPTO_2016 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201648) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 50] <- as.integer(SINAN_LEPTO_2016 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201649) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 51] <- as.integer(SINAN_LEPTO_2016 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201650) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 52] <- as.integer(SINAN_LEPTO_2016 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201651) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 53] <- as.integer(SINAN_LEPTO_2016 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201652) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 54] <- as.integer(SINAN_LEPTO_2016 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201653) %>%
                                           count() 
  )
}

AUX[, 1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX[(nrow(AUX)+ 1), 2:54] <- apply(AUX[, 2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2016_SE_Notificados"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2016_SE_Notificados"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2016_SE_Notificados.csv"), 
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
  
  AUX[which(AUX == i), 2] <- as.integer(SINAN_LEPTO_2016 %>%
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==201601,
                                                 CLASSI_FIN == 1)%>%
                                          count()
  )
  
  AUX[which(AUX == i), 3] <- as.integer(SINAN_LEPTO_2016 %>% 
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==201602,
                                                 CLASSI_FIN == 1) %>% 
                                          count()
  )
  
  AUX[which(AUX == i), 4] <- as.integer(SINAN_LEPTO_2016 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==201603,
                                                 CLASSI_FIN == 1) %>% 
                                          count()
  )
  
  AUX[which(AUX == i),5] <- as.integer(SINAN_LEPTO_2016 %>% 
                                         filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==201604,
                                                CLASSI_FIN == 1) %>% 
                                         count()
  )
  
  AUX[which(AUX == i), 6] <- as.integer(SINAN_LEPTO_2016 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==201605,
                                                 CLASSI_FIN == 1) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 7] <- as.integer(SINAN_LEPTO_2016 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==201606,
                                                 CLASSI_FIN == 1) %>%
                                          count()
  )
  
  AUX[which(AUX == i), 8] <- as.integer(SINAN_LEPTO_2016 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==201607,
                                                 CLASSI_FIN == 1) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 9] <- as.integer(SINAN_LEPTO_2016 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==201608,
                                                 CLASSI_FIN == 1) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 10] <- as.integer(SINAN_LEPTO_2016 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201609,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 11] <- as.integer(SINAN_LEPTO_2016 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201610,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 12] <- as.integer(SINAN_LEPTO_2016 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201611,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 13] <- as.integer(SINAN_LEPTO_2016 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201612,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 14] <- as.integer(SINAN_LEPTO_2016 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201613,
                                                  CLASSI_FIN == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 15] <- as.integer(SINAN_LEPTO_2016 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201614,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 16] <- as.integer(SINAN_LEPTO_2016 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201615,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 17] <- as.integer(SINAN_LEPTO_2016 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201616,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 18] <- as.integer(SINAN_LEPTO_2016 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201617,
                                                  CLASSI_FIN == 1) %>%  
                                           count() 
  )
  
  AUX[which(AUX == i), 19] <- as.integer(SINAN_LEPTO_2016 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201618,
                                                  CLASSI_FIN == 1) %>%      
                                           count() 
  )
  
  AUX[which(AUX == i), 20] <- as.integer(SINAN_LEPTO_2016 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201619,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i),  21] <- as.integer(SINAN_LEPTO_2016 %>% 
                                            filter(ID_MN_RESI ==i & 
                                                     SEM_PRI ==201620,
                                                   CLASSI_FIN == 1) %>%
                                            count() 
  )
  
  AUX[which(AUX == i), 22] <- as.integer(SINAN_LEPTO_2016 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201621,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 23] <- as.integer(SINAN_LEPTO_2016 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201622,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 24] <- as.integer(SINAN_LEPTO_2016 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201623,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 25] <- as.integer(SINAN_LEPTO_2016 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201624,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 26] <- as.integer(SINAN_LEPTO_2016 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201625,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 27] <- as.integer(SINAN_LEPTO_2016 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201626,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 28] <- as.integer(SINAN_LEPTO_2016 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201627,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 29] <- as.integer(SINAN_LEPTO_2016 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201628,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 30] <- as.integer(SINAN_LEPTO_2016 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201629,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 31] <- as.integer(SINAN_LEPTO_2016 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201630,
                                                  CLASSI_FIN == 1) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 32] <- as.integer(SINAN_LEPTO_2016 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201631,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 33] <- as.integer(SINAN_LEPTO_2016 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201632,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 34] <- as.integer(SINAN_LEPTO_2016 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201633,
                                                  CLASSI_FIN == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 35] <- as.integer(SINAN_LEPTO_2016 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201634,
                                                  CLASSI_FIN == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 36] <- as.integer(SINAN_LEPTO_2016 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201635,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 37] <- as.integer(SINAN_LEPTO_2016 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201636,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 38] <- as.integer(SINAN_LEPTO_2016 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201637,
                                                  CLASSI_FIN == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 39] <- as.integer(SINAN_LEPTO_2016 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201638,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 40] <- as.integer(SINAN_LEPTO_2016 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201639,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 41] <- as.integer(SINAN_LEPTO_2016 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201640,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 42] <- as.integer(SINAN_LEPTO_2016 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201641,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 43] <- as.integer(SINAN_LEPTO_2016 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201642,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 44] <- as.integer(SINAN_LEPTO_2016 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201643,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 45] <- as.integer(SINAN_LEPTO_2016 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201644,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 46] <- as.integer(SINAN_LEPTO_2016 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201645,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 47] <- as.integer(SINAN_LEPTO_2016 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201646,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 48] <- as.integer(SINAN_LEPTO_2016 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201647,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 49] <- as.integer(SINAN_LEPTO_2016 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201648,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 50] <- as.integer(SINAN_LEPTO_2016 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201649,
                                                  CLASSI_FIN == 1) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 51] <- as.integer(SINAN_LEPTO_2016 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201650,
                                                  CLASSI_FIN == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 52] <- as.integer(SINAN_LEPTO_2016 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201651,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 53] <- as.integer(SINAN_LEPTO_2016 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201652,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 54] <- as.integer(SINAN_LEPTO_2016 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201653,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
}

AUX[, 1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX[(nrow(AUX)+ 1), 2:54] <- apply(AUX[, 2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2016_SE_Confirmados"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2016_SE_Confirmados"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2016_SE_Confirmados.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2016 %>% 
                                                   filter(ID_MN_RESI == i) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2016 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CLASSI_FIN == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2016 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CS_ZONA == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2016 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CS_ZONA == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_LEPTO_2016 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CS_ZONA == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_LEPTO_2016 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ZONA == 9 
                                                           |
                                                             is.na(CS_ZONA)) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_LEPTO_2016 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_SEXO == "F") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(SINAN_LEPTO_2016 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_SEXO == "M") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_LEPTO_2016 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             NU_IDADE_N <= 3012) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(SINAN_LEPTO_2016 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             NU_IDADE_N > 4000 
                                                           & 
                                                             NU_IDADE_N <= 4005) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_LEPTO_2016 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             NU_IDADE_N > 4005 
                                                           & 
                                                             NU_IDADE_N <= 4012) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 16] <- as.integer(SINAN_LEPTO_2016 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             NU_IDADE_N > 4012 
                                                           & 
                                                             NU_IDADE_N <= 4018) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 17] <- as.integer(SINAN_LEPTO_2016 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             NU_IDADE_N > 4018 
                                                           & 
                                                             NU_IDADE_N <= 4059) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 18] <- as.integer(SINAN_LEPTO_2016 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             NU_IDADE_N > 4059) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 19] <- as.integer(SINAN_LEPTO_2016 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "00") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 20] <- as.integer(SINAN_LEPTO_2016 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "01" 
                                                           | 
                                                             CS_ESCOL_N == "02" 
                                                           | 
                                                             CS_ESCOL_N == "03") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 21] <- as.integer(SINAN_LEPTO_2016 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "04") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 22] <- as.integer(SINAN_LEPTO_2016 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "05") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 23] <- as.integer(SINAN_LEPTO_2016 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "06") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 24] <- as.integer(SINAN_LEPTO_2016 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "07") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 25] <- as.integer(SINAN_LEPTO_2016 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "08") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 26] <- as.integer(SINAN_LEPTO_2016 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "10") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 27] <- as.integer(SINAN_LEPTO_2016 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "09") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 28] <- as.integer(SINAN_LEPTO_2016 %>% 
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
  
  AUX[which(AUX$COD_IBGE == i), 29] <- as.integer(SINAN_LEPTO_2016 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_GESTANT == "5") %>% 
                                                    count()
  )
  AUX[which(AUX$COD_IBGE == i), 30] <- as.integer(SINAN_LEPTO_2016 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_GESTANT == "6") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 31] <- as.integer(SINAN_LEPTO_2016 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_GESTANT == "9") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 32] <- as.integer(SINAN_LEPTO_2016 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "01") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 33] <- as.integer(SINAN_LEPTO_2016 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "2") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 34] <- as.integer(SINAN_LEPTO_2016 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "3") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 35] <- as.integer(SINAN_LEPTO_2016 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "4") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 36] <- as.integer(SINAN_LEPTO_2016 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "5") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 37] <- as.integer(SINAN_LEPTO_2016 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "9") %>% 
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2016_GERAL"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2016_GERAL"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2016_GERAL.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2016 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_CB_LAM == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2016 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_CB_CRI == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2016 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_CB_CAI == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2016 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_CB_FOS == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_LEPTO_2016 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_CB_SIN == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_LEPTO_2016 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_CB_PLA == 1) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_LEPTO_2016 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_CB_COR == 1) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(SINAN_LEPTO_2016 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             ANT_CB_ROE == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_LEPTO_2016 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             ANT_CB_GRA == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(SINAN_LEPTO_2016 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_CB_TER == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_LEPTO_2016 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_CB_LIX == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 16] <- as.integer(SINAN_LEPTO_2016 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_CB_OUT == 4018) %>% 
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2016_SIT_RISCO"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2016_SIT_RISCO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2016_SIT_RISCO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2016 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CLI_FEBRE == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2016 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CLI_MIALGI == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2016 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CLI_CEFALE == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2016 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CLI_PROST == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_LEPTO_2016 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CLI_CONGES == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_LEPTO_2016 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_PANTUR == 1) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_LEPTO_2016 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_VOMITO == 1) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(SINAN_LEPTO_2016 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             CLI_DIARRE == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_LEPTO_2016 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             CLI_ICTERI == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(SINAN_LEPTO_2016 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_RENAL == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_LEPTO_2016 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_RESPIR == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 16] <- as.integer(SINAN_LEPTO_2016 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_CARDIA == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 17] <- as.integer(SINAN_LEPTO_2016 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_HEMOPU == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 18] <- as.integer(SINAN_LEPTO_2016 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_HEMORR == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 19] <- as.integer(SINAN_LEPTO_2016 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_MENING == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 20] <- as.integer(SINAN_LEPTO_2016 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_OUTROS == 1) %>% 
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2016_SINTOMAS"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2016_SINTOMAS"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2016_SINTOMAS.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2016 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ATE_HOSP == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2016 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ATE_HOSP == 2) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2016_HOSPITALIZACAO"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2016_HOSPITALIZACAO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2016_HOSPITALIZACAO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2016 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CLASSI_FIN == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2016 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CLASSI_FIN == 2) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2016_CLASSI_FIN"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2016_CLASSI_FIN"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2016_CLASSI_FIN.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2016 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CRITERIO == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2016 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CRITERIO == 2) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2016_CRIT_ENCER"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2016_CRIT_ENCER"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2016_CRIT_ENCER.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2016 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CON_AREA == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2016 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AREA == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2016 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AREA == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2016 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AREA == 9) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4: ncol(AUX)] <- apply(AUX[, 4: ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2016_LPI"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2016_LPI"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2016_LPI.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2016 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CON_AMBIEN == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2016 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2016 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2016 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 4) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_LEPTO_2016 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 9) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4: ncol(AUX)] <- apply(AUX[, 4: ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2016_AMBIENTE"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2016_AMBIENTE"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2016_AMBIENTE.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2016 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CON_AMBIEN == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2016 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2016 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 9) %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4: ncol(AUX)] <- apply(AUX[, 4: ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2016_TRABALHO"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2016_TRABALHO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2016_TRABALHO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2016 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          EVOLUCAO == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2016 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            EVOLUCAO == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2016 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            EVOLUCAO == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2016 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            EVOLUCAO == 9) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4: ncol(AUX)] <- apply(AUX[, 4: ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2016_EVOLUCAO"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2016_EVOLUCAO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2016_EVOLUCAO.csv"), 
           row.names = FALSE)

rm(SINAN_LEPTO_2016)

##################################################################################
#################   2017    ######################################################
##################################################################################

SINAN_LEPTO_2017 <- LEPTONET %>% filter(ID_REGIONA == ID_REG | ID_RG_RESI == ID_REG,
                                        NU_ANO == 2017)

assign(paste0("RS", RS, "_LEPTO_2017_SINAN"), SINAN_LEPTO_2017)

write.csv (assign(paste0("RS", RS, "_LEPTO_2017_SINAN"), SINAN_LEPTO_2017), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2017_SINAN.csv"), 
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
  
  AUX[which(AUX == i), 2] <- as.integer(SINAN_LEPTO_2017 %>%
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==201701)%>%
                                          count()
  )
  
  AUX[which(AUX == i), 3] <- as.integer(SINAN_LEPTO_2017 %>% 
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==201702) %>% 
                                          count()
  )
  
  AUX[which(AUX == i), 4] <- as.integer(SINAN_LEPTO_2017 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==201703) %>% 
                                          count()
  )
  
  AUX[which(AUX == i),5] <- as.integer(SINAN_LEPTO_2017 %>% 
                                         filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==201704) %>% 
                                         count()
  )
  
  AUX[which(AUX == i), 6] <- as.integer(SINAN_LEPTO_2017 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==201705) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 7] <- as.integer(SINAN_LEPTO_2017 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==201706) %>%
                                          count()
  )
  
  AUX[which(AUX == i), 8] <- as.integer(SINAN_LEPTO_2017 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==201707) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 9] <- as.integer(SINAN_LEPTO_2017 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==201708) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 10] <- as.integer(SINAN_LEPTO_2017 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201709) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 11] <- as.integer(SINAN_LEPTO_2017 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201710) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 12] <- as.integer(SINAN_LEPTO_2017 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201711) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 13] <- as.integer(SINAN_LEPTO_2017 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201712) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 14] <- as.integer(SINAN_LEPTO_2017 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201713) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 15] <- as.integer(SINAN_LEPTO_2017 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201714) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 16] <- as.integer(SINAN_LEPTO_2017 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201715) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 17] <- as.integer(SINAN_LEPTO_2017 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201716) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 18] <- as.integer(SINAN_LEPTO_2017 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201717) %>%  
                                           count() 
  )
  
  AUX[which(AUX == i), 19] <- as.integer(SINAN_LEPTO_2017 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201718) %>%      
                                           count() 
  )
  
  AUX[which(AUX == i), 20] <- as.integer(SINAN_LEPTO_2017 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201719) %>%
                                           count() 
  )
  
  AUX[which(AUX == i),  21] <- as.integer(SINAN_LEPTO_2017 %>% 
                                            filter(ID_MN_RESI ==i & 
                                                     SEM_PRI ==201720) %>%
                                            count() 
  )
  
  AUX[which(AUX == i), 22] <- as.integer(SINAN_LEPTO_2017 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201721) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 23] <- as.integer(SINAN_LEPTO_2017 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201722) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 24] <- as.integer(SINAN_LEPTO_2017 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201723) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 25] <- as.integer(SINAN_LEPTO_2017 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201724) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 26] <- as.integer(SINAN_LEPTO_2017 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201725) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 27] <- as.integer(SINAN_LEPTO_2017 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201726) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 28] <- as.integer(SINAN_LEPTO_2017 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201727) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 29] <- as.integer(SINAN_LEPTO_2017 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201728) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 30] <- as.integer(SINAN_LEPTO_2017 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201729) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 31] <- as.integer(SINAN_LEPTO_2017 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201730) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 32] <- as.integer(SINAN_LEPTO_2017 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201731) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 33] <- as.integer(SINAN_LEPTO_2017 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201732) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 34] <- as.integer(SINAN_LEPTO_2017 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201733) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 35] <- as.integer(SINAN_LEPTO_2017 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201734) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 36] <- as.integer(SINAN_LEPTO_2017 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201735) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 37] <- as.integer(SINAN_LEPTO_2017 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201736) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 38] <- as.integer(SINAN_LEPTO_2017 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201737) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 39] <- as.integer(SINAN_LEPTO_2017 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201738) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 40] <- as.integer(SINAN_LEPTO_2017 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201739) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 41] <- as.integer(SINAN_LEPTO_2017 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201740) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 42] <- as.integer(SINAN_LEPTO_2017 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201741) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 43] <- as.integer(SINAN_LEPTO_2017 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201742) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 44] <- as.integer(SINAN_LEPTO_2017 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201743) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 45] <- as.integer(SINAN_LEPTO_2017 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201744) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 46] <- as.integer(SINAN_LEPTO_2017 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201745) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 47] <- as.integer(SINAN_LEPTO_2017 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201746) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 48] <- as.integer(SINAN_LEPTO_2017 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201747) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 49] <- as.integer(SINAN_LEPTO_2017 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201748) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 50] <- as.integer(SINAN_LEPTO_2017 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201749) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 51] <- as.integer(SINAN_LEPTO_2017 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201750) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 52] <- as.integer(SINAN_LEPTO_2017 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201751) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 53] <- as.integer(SINAN_LEPTO_2017 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201752) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 54] <- as.integer(SINAN_LEPTO_2017 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201753) %>%
                                           count() 
  )
}

AUX[, 1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX[(nrow(AUX)+ 1), 2:54] <- apply(AUX[, 2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2017_SE_Notificados"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2017_SE_Notificados"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2017_SE_Notificados.csv"), 
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
  
  AUX[which(AUX == i), 2] <- as.integer(SINAN_LEPTO_2017 %>%
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==201701,
                                                 CLASSI_FIN == 1)%>%
                                          count()
  )
  
  AUX[which(AUX == i), 3] <- as.integer(SINAN_LEPTO_2017 %>% 
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==201702,
                                                 CLASSI_FIN == 1) %>% 
                                          count()
  )
  
  AUX[which(AUX == i), 4] <- as.integer(SINAN_LEPTO_2017 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==201703,
                                                 CLASSI_FIN == 1) %>% 
                                          count()
  )
  
  AUX[which(AUX == i),5] <- as.integer(SINAN_LEPTO_2017 %>% 
                                         filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==201704,
                                                CLASSI_FIN == 1) %>% 
                                         count()
  )
  
  AUX[which(AUX == i), 6] <- as.integer(SINAN_LEPTO_2017 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==201705,
                                                 CLASSI_FIN == 1) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 7] <- as.integer(SINAN_LEPTO_2017 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==201706,
                                                 CLASSI_FIN == 1) %>%
                                          count()
  )
  
  AUX[which(AUX == i), 8] <- as.integer(SINAN_LEPTO_2017 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==201707,
                                                 CLASSI_FIN == 1) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 9] <- as.integer(SINAN_LEPTO_2017 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==201708,
                                                 CLASSI_FIN == 1) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 10] <- as.integer(SINAN_LEPTO_2017 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201709,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 11] <- as.integer(SINAN_LEPTO_2017 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201710,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 12] <- as.integer(SINAN_LEPTO_2017 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201711,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 13] <- as.integer(SINAN_LEPTO_2017 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201712,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 14] <- as.integer(SINAN_LEPTO_2017 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201713,
                                                  CLASSI_FIN == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 15] <- as.integer(SINAN_LEPTO_2017 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201714,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 16] <- as.integer(SINAN_LEPTO_2017 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201715,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 17] <- as.integer(SINAN_LEPTO_2017 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201716,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 18] <- as.integer(SINAN_LEPTO_2017 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201717,
                                                  CLASSI_FIN == 1) %>%  
                                           count() 
  )
  
  AUX[which(AUX == i), 19] <- as.integer(SINAN_LEPTO_2017 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201718,
                                                  CLASSI_FIN == 1) %>%      
                                           count() 
  )
  
  AUX[which(AUX == i), 20] <- as.integer(SINAN_LEPTO_2017 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201719,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i),  21] <- as.integer(SINAN_LEPTO_2017 %>% 
                                            filter(ID_MN_RESI ==i & 
                                                     SEM_PRI ==201720,
                                                   CLASSI_FIN == 1) %>%
                                            count() 
  )
  
  AUX[which(AUX == i), 22] <- as.integer(SINAN_LEPTO_2017 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201721,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 23] <- as.integer(SINAN_LEPTO_2017 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201722,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 24] <- as.integer(SINAN_LEPTO_2017 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201723,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 25] <- as.integer(SINAN_LEPTO_2017 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201724,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 26] <- as.integer(SINAN_LEPTO_2017 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201725,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 27] <- as.integer(SINAN_LEPTO_2017 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201726,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 28] <- as.integer(SINAN_LEPTO_2017 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201727,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 29] <- as.integer(SINAN_LEPTO_2017 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201728,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 30] <- as.integer(SINAN_LEPTO_2017 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201729,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 31] <- as.integer(SINAN_LEPTO_2017 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201730,
                                                  CLASSI_FIN == 1) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 32] <- as.integer(SINAN_LEPTO_2017 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201731,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 33] <- as.integer(SINAN_LEPTO_2017 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201732,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 34] <- as.integer(SINAN_LEPTO_2017 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201733,
                                                  CLASSI_FIN == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 35] <- as.integer(SINAN_LEPTO_2017 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201734,
                                                  CLASSI_FIN == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 36] <- as.integer(SINAN_LEPTO_2017 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201735,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 37] <- as.integer(SINAN_LEPTO_2017 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201736,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 38] <- as.integer(SINAN_LEPTO_2017 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201737,
                                                  CLASSI_FIN == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 39] <- as.integer(SINAN_LEPTO_2017 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201738,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 40] <- as.integer(SINAN_LEPTO_2017 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201739,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 41] <- as.integer(SINAN_LEPTO_2017 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201740,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 42] <- as.integer(SINAN_LEPTO_2017 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201741,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 43] <- as.integer(SINAN_LEPTO_2017 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201742,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 44] <- as.integer(SINAN_LEPTO_2017 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201743,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 45] <- as.integer(SINAN_LEPTO_2017 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201744,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 46] <- as.integer(SINAN_LEPTO_2017 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201745,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 47] <- as.integer(SINAN_LEPTO_2017 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201746,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 48] <- as.integer(SINAN_LEPTO_2017 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201747,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 49] <- as.integer(SINAN_LEPTO_2017 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201748,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 50] <- as.integer(SINAN_LEPTO_2017 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201749,
                                                  CLASSI_FIN == 1) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 51] <- as.integer(SINAN_LEPTO_2017 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201750,
                                                  CLASSI_FIN == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 52] <- as.integer(SINAN_LEPTO_2017 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201751,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 53] <- as.integer(SINAN_LEPTO_2017 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201752,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 54] <- as.integer(SINAN_LEPTO_2017 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201753,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
}

AUX[, 1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX[(nrow(AUX)+ 1), 2:54] <- apply(AUX[, 2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2017_SE_Confirmados"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2017_SE_Confirmados"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2017_SE_Confirmados.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2017 %>% 
                                                   filter(ID_MN_RESI == i) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2017 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CLASSI_FIN == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2017 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CS_ZONA == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2017 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CS_ZONA == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_LEPTO_2017 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CS_ZONA == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_LEPTO_2017 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ZONA == 9 
                                                           |
                                                             is.na(CS_ZONA)) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_LEPTO_2017 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_SEXO == "F") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(SINAN_LEPTO_2017 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_SEXO == "M") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_LEPTO_2017 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             NU_IDADE_N <= 3012) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(SINAN_LEPTO_2017 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             NU_IDADE_N > 4000 
                                                           & 
                                                             NU_IDADE_N <= 4005) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_LEPTO_2017 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             NU_IDADE_N > 4005 
                                                           & 
                                                             NU_IDADE_N <= 4012) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 16] <- as.integer(SINAN_LEPTO_2017 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             NU_IDADE_N > 4012 
                                                           & 
                                                             NU_IDADE_N <= 4018) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 17] <- as.integer(SINAN_LEPTO_2017 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             NU_IDADE_N > 4018 
                                                           & 
                                                             NU_IDADE_N <= 4059) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 18] <- as.integer(SINAN_LEPTO_2017 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             NU_IDADE_N > 4059) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 19] <- as.integer(SINAN_LEPTO_2017 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "00") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 20] <- as.integer(SINAN_LEPTO_2017 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "01" 
                                                           | 
                                                             CS_ESCOL_N == "02" 
                                                           | 
                                                             CS_ESCOL_N == "03") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 21] <- as.integer(SINAN_LEPTO_2017 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "04") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 22] <- as.integer(SINAN_LEPTO_2017 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "05") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 23] <- as.integer(SINAN_LEPTO_2017 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "06") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 24] <- as.integer(SINAN_LEPTO_2017 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "07") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 25] <- as.integer(SINAN_LEPTO_2017 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "08") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 26] <- as.integer(SINAN_LEPTO_2017 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "10") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 27] <- as.integer(SINAN_LEPTO_2017 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "09") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 28] <- as.integer(SINAN_LEPTO_2017 %>% 
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
  
  AUX[which(AUX$COD_IBGE == i), 29] <- as.integer(SINAN_LEPTO_2017 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_GESTANT == "5") %>% 
                                                    count()
  )
  AUX[which(AUX$COD_IBGE == i), 30] <- as.integer(SINAN_LEPTO_2017 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_GESTANT == "6") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 31] <- as.integer(SINAN_LEPTO_2017 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_GESTANT == "9") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 32] <- as.integer(SINAN_LEPTO_2017 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "01") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 33] <- as.integer(SINAN_LEPTO_2017 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "2") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 34] <- as.integer(SINAN_LEPTO_2017 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "3") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 35] <- as.integer(SINAN_LEPTO_2017 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "4") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 36] <- as.integer(SINAN_LEPTO_2017 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "5") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 37] <- as.integer(SINAN_LEPTO_2017 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "9") %>% 
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2017_GERAL"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2017_GERAL"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2017_GERAL.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2017 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_CB_LAM == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2017 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_CB_CRI == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2017 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_CB_CAI == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2017 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_CB_FOS == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_LEPTO_2017 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_CB_SIN == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_LEPTO_2017 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_CB_PLA == 1) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_LEPTO_2017 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_CB_COR == 1) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(SINAN_LEPTO_2017 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             ANT_CB_ROE == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_LEPTO_2017 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             ANT_CB_GRA == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(SINAN_LEPTO_2017 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_CB_TER == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_LEPTO_2017 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_CB_LIX == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 16] <- as.integer(SINAN_LEPTO_2017 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_CB_OUT == 4018) %>% 
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2017_SIT_RISCO"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2017_SIT_RISCO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2017_SIT_RISCO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2017 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CLI_FEBRE == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2017 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CLI_MIALGI == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2017 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CLI_CEFALE == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2017 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CLI_PROST == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_LEPTO_2017 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CLI_CONGES == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_LEPTO_2017 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_PANTUR == 1) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_LEPTO_2017 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_VOMITO == 1) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(SINAN_LEPTO_2017 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             CLI_DIARRE == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_LEPTO_2017 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             CLI_ICTERI == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(SINAN_LEPTO_2017 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_RENAL == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_LEPTO_2017 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_RESPIR == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 16] <- as.integer(SINAN_LEPTO_2017 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_CARDIA == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 17] <- as.integer(SINAN_LEPTO_2017 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_HEMOPU == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 18] <- as.integer(SINAN_LEPTO_2017 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_HEMORR == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 19] <- as.integer(SINAN_LEPTO_2017 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_MENING == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 20] <- as.integer(SINAN_LEPTO_2017 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_OUTROS == 1) %>% 
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2017_SINTOMAS"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2017_SINTOMAS"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2017_SINTOMAS.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2017 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ATE_HOSP == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2017 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ATE_HOSP == 2) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2017_HOSPITALIZACAO"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2017_HOSPITALIZACAO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2017_HOSPITALIZACAO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2017 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CLASSI_FIN == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2017 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CLASSI_FIN == 2) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2017_CLASSI_FIN"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2017_CLASSI_FIN"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2017_CLASSI_FIN.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2017 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CRITERIO == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2017 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CRITERIO == 2) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2017_CRIT_ENCER"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2017_CRIT_ENCER"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2017_CRIT_ENCER.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2017 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CON_AREA == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2017 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AREA == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2017 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AREA == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2017 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AREA == 9) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4: ncol(AUX)] <- apply(AUX[, 4: ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2017_LPI"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2017_LPI"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2017_LPI.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2017 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CON_AMBIEN == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2017 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2017 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2017 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 4) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_LEPTO_2017 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 9) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4: ncol(AUX)] <- apply(AUX[, 4: ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2017_AMBIENTE"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2017_AMBIENTE"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2017_AMBIENTE.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2017 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CON_AMBIEN == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2017 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2017 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 9) %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4: ncol(AUX)] <- apply(AUX[, 4: ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2017_TRABALHO"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2017_TRABALHO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2017_TRABALHO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2017 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          EVOLUCAO == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2017 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            EVOLUCAO == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2017 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            EVOLUCAO == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2017 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            EVOLUCAO == 9) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4: ncol(AUX)] <- apply(AUX[, 4: ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2017_EVOLUCAO"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2017_EVOLUCAO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2017_EVOLUCAO.csv"), 
           row.names = FALSE)

rm(SINAN_LEPTO_2017)

##################################################################################
#################   2018    ######################################################
##################################################################################

SINAN_LEPTO_2018 <- LEPTONET %>% filter(ID_REGIONA == ID_REG | ID_RG_RESI == ID_REG,
                                        NU_ANO == 2018)

assign(paste0("RS", RS, "_LEPTO_2018_SINAN"), SINAN_LEPTO_2018)

write.csv (assign(paste0("RS", RS, "_LEPTO_2018_SINAN"), SINAN_LEPTO_2018), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2018_SINAN.csv"), 
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
  
  AUX[which(AUX == i), 2] <- as.integer(SINAN_LEPTO_2018 %>%
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==201801)%>%
                                          count()
  )
  
  AUX[which(AUX == i), 3] <- as.integer(SINAN_LEPTO_2018 %>% 
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==201802) %>% 
                                          count()
  )
  
  AUX[which(AUX == i), 4] <- as.integer(SINAN_LEPTO_2018 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==201803) %>% 
                                          count()
  )
  
  AUX[which(AUX == i),5] <- as.integer(SINAN_LEPTO_2018 %>% 
                                         filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==201804) %>% 
                                         count()
  )
  
  AUX[which(AUX == i), 6] <- as.integer(SINAN_LEPTO_2018 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==201805) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 7] <- as.integer(SINAN_LEPTO_2018 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==201806) %>%
                                          count()
  )
  
  AUX[which(AUX == i), 8] <- as.integer(SINAN_LEPTO_2018 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==201807) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 9] <- as.integer(SINAN_LEPTO_2018 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==201808) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 10] <- as.integer(SINAN_LEPTO_2018 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201809) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 11] <- as.integer(SINAN_LEPTO_2018 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201810) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 12] <- as.integer(SINAN_LEPTO_2018 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201811) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 13] <- as.integer(SINAN_LEPTO_2018 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201812) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 14] <- as.integer(SINAN_LEPTO_2018 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201813) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 15] <- as.integer(SINAN_LEPTO_2018 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201814) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 16] <- as.integer(SINAN_LEPTO_2018 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201815) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 17] <- as.integer(SINAN_LEPTO_2018 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201816) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 18] <- as.integer(SINAN_LEPTO_2018 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201817) %>%  
                                           count() 
  )
  
  AUX[which(AUX == i), 19] <- as.integer(SINAN_LEPTO_2018 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201818) %>%      
                                           count() 
  )
  
  AUX[which(AUX == i), 20] <- as.integer(SINAN_LEPTO_2018 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201819) %>%
                                           count() 
  )
  
  AUX[which(AUX == i),  21] <- as.integer(SINAN_LEPTO_2018 %>% 
                                            filter(ID_MN_RESI ==i & 
                                                     SEM_PRI ==201820) %>%
                                            count() 
  )
  
  AUX[which(AUX == i), 22] <- as.integer(SINAN_LEPTO_2018 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201821) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 23] <- as.integer(SINAN_LEPTO_2018 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201822) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 24] <- as.integer(SINAN_LEPTO_2018 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201823) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 25] <- as.integer(SINAN_LEPTO_2018 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201824) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 26] <- as.integer(SINAN_LEPTO_2018 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201825) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 27] <- as.integer(SINAN_LEPTO_2018 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201826) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 28] <- as.integer(SINAN_LEPTO_2018 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201827) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 29] <- as.integer(SINAN_LEPTO_2018 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201828) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 30] <- as.integer(SINAN_LEPTO_2018 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201829) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 31] <- as.integer(SINAN_LEPTO_2018 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201830) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 32] <- as.integer(SINAN_LEPTO_2018 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201831) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 33] <- as.integer(SINAN_LEPTO_2018 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201832) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 34] <- as.integer(SINAN_LEPTO_2018 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201833) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 35] <- as.integer(SINAN_LEPTO_2018 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201834) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 36] <- as.integer(SINAN_LEPTO_2018 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201835) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 37] <- as.integer(SINAN_LEPTO_2018 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201836) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 38] <- as.integer(SINAN_LEPTO_2018 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201837) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 39] <- as.integer(SINAN_LEPTO_2018 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201838) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 40] <- as.integer(SINAN_LEPTO_2018 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201839) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 41] <- as.integer(SINAN_LEPTO_2018 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201840) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 42] <- as.integer(SINAN_LEPTO_2018 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201841) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 43] <- as.integer(SINAN_LEPTO_2018 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201842) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 44] <- as.integer(SINAN_LEPTO_2018 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201843) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 45] <- as.integer(SINAN_LEPTO_2018 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201844) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 46] <- as.integer(SINAN_LEPTO_2018 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201845) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 47] <- as.integer(SINAN_LEPTO_2018 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201846) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 48] <- as.integer(SINAN_LEPTO_2018 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201847) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 49] <- as.integer(SINAN_LEPTO_2018 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201848) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 50] <- as.integer(SINAN_LEPTO_2018 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201849) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 51] <- as.integer(SINAN_LEPTO_2018 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201850) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 52] <- as.integer(SINAN_LEPTO_2018 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201851) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 53] <- as.integer(SINAN_LEPTO_2018 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201852) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 54] <- as.integer(SINAN_LEPTO_2018 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201853) %>%
                                           count() 
  )
}

AUX[, 1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX[(nrow(AUX)+ 1), 2:54] <- apply(AUX[, 2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2018_SE_Notificados"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2018_SE_Notificados"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2018_SE_Notificados.csv"), 
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
  
  AUX[which(AUX == i), 2] <- as.integer(SINAN_LEPTO_2018 %>%
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==201801,
                                                 CLASSI_FIN == 1)%>%
                                          count()
  )
  
  AUX[which(AUX == i), 3] <- as.integer(SINAN_LEPTO_2018 %>% 
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==201802,
                                                 CLASSI_FIN == 1) %>% 
                                          count()
  )
  
  AUX[which(AUX == i), 4] <- as.integer(SINAN_LEPTO_2018 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==201803,
                                                 CLASSI_FIN == 1) %>% 
                                          count()
  )
  
  AUX[which(AUX == i),5] <- as.integer(SINAN_LEPTO_2018 %>% 
                                         filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==201804,
                                                CLASSI_FIN == 1) %>% 
                                         count()
  )
  
  AUX[which(AUX == i), 6] <- as.integer(SINAN_LEPTO_2018 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==201805,
                                                 CLASSI_FIN == 1) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 7] <- as.integer(SINAN_LEPTO_2018 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==201806,
                                                 CLASSI_FIN == 1) %>%
                                          count()
  )
  
  AUX[which(AUX == i), 8] <- as.integer(SINAN_LEPTO_2018 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==201807,
                                                 CLASSI_FIN == 1) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 9] <- as.integer(SINAN_LEPTO_2018 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==201808,
                                                 CLASSI_FIN == 1) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 10] <- as.integer(SINAN_LEPTO_2018 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201809,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 11] <- as.integer(SINAN_LEPTO_2018 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201810,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 12] <- as.integer(SINAN_LEPTO_2018 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201811,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 13] <- as.integer(SINAN_LEPTO_2018 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201812,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 14] <- as.integer(SINAN_LEPTO_2018 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201813,
                                                  CLASSI_FIN == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 15] <- as.integer(SINAN_LEPTO_2018 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201814,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 16] <- as.integer(SINAN_LEPTO_2018 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201815,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 17] <- as.integer(SINAN_LEPTO_2018 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201816,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 18] <- as.integer(SINAN_LEPTO_2018 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201817,
                                                  CLASSI_FIN == 1) %>%  
                                           count() 
  )
  
  AUX[which(AUX == i), 19] <- as.integer(SINAN_LEPTO_2018 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201818,
                                                  CLASSI_FIN == 1) %>%      
                                           count() 
  )
  
  AUX[which(AUX == i), 20] <- as.integer(SINAN_LEPTO_2018 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201819,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i),  21] <- as.integer(SINAN_LEPTO_2018 %>% 
                                            filter(ID_MN_RESI ==i & 
                                                     SEM_PRI ==201820,
                                                   CLASSI_FIN == 1) %>%
                                            count() 
  )
  
  AUX[which(AUX == i), 22] <- as.integer(SINAN_LEPTO_2018 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201821,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 23] <- as.integer(SINAN_LEPTO_2018 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201822,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 24] <- as.integer(SINAN_LEPTO_2018 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201823,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 25] <- as.integer(SINAN_LEPTO_2018 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201824,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 26] <- as.integer(SINAN_LEPTO_2018 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201825,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 27] <- as.integer(SINAN_LEPTO_2018 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201826,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 28] <- as.integer(SINAN_LEPTO_2018 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201827,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 29] <- as.integer(SINAN_LEPTO_2018 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201828,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 30] <- as.integer(SINAN_LEPTO_2018 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201829,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 31] <- as.integer(SINAN_LEPTO_2018 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201830,
                                                  CLASSI_FIN == 1) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 32] <- as.integer(SINAN_LEPTO_2018 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201831,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 33] <- as.integer(SINAN_LEPTO_2018 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201832,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 34] <- as.integer(SINAN_LEPTO_2018 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201833,
                                                  CLASSI_FIN == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 35] <- as.integer(SINAN_LEPTO_2018 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201834,
                                                  CLASSI_FIN == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 36] <- as.integer(SINAN_LEPTO_2018 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201835,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 37] <- as.integer(SINAN_LEPTO_2018 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201836,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 38] <- as.integer(SINAN_LEPTO_2018 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201837,
                                                  CLASSI_FIN == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 39] <- as.integer(SINAN_LEPTO_2018 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201838,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 40] <- as.integer(SINAN_LEPTO_2018 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201839,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 41] <- as.integer(SINAN_LEPTO_2018 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201840,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 42] <- as.integer(SINAN_LEPTO_2018 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201841,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 43] <- as.integer(SINAN_LEPTO_2018 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201842,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 44] <- as.integer(SINAN_LEPTO_2018 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201843,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 45] <- as.integer(SINAN_LEPTO_2018 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201844,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 46] <- as.integer(SINAN_LEPTO_2018 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201845,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 47] <- as.integer(SINAN_LEPTO_2018 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201846,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 48] <- as.integer(SINAN_LEPTO_2018 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201847,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 49] <- as.integer(SINAN_LEPTO_2018 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201848,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 50] <- as.integer(SINAN_LEPTO_2018 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201849,
                                                  CLASSI_FIN == 1) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 51] <- as.integer(SINAN_LEPTO_2018 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201850,
                                                  CLASSI_FIN == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 52] <- as.integer(SINAN_LEPTO_2018 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201851,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 53] <- as.integer(SINAN_LEPTO_2018 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201852,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 54] <- as.integer(SINAN_LEPTO_2018 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201853,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
}

AUX[, 1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX[(nrow(AUX)+ 1), 2:54] <- apply(AUX[, 2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2018_SE_Confirmados"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2018_SE_Confirmados"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2018_SE_Confirmados.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2018 %>% 
                                                   filter(ID_MN_RESI == i) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2018 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CLASSI_FIN == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2018 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CS_ZONA == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2018 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CS_ZONA == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_LEPTO_2018 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CS_ZONA == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_LEPTO_2018 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ZONA == 9 
                                                           |
                                                             is.na(CS_ZONA)) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_LEPTO_2018 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_SEXO == "F") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(SINAN_LEPTO_2018 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_SEXO == "M") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_LEPTO_2018 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             NU_IDADE_N <= 3012) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(SINAN_LEPTO_2018 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             NU_IDADE_N > 4000 
                                                           & 
                                                             NU_IDADE_N <= 4005) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_LEPTO_2018 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             NU_IDADE_N > 4005 
                                                           & 
                                                             NU_IDADE_N <= 4012) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 16] <- as.integer(SINAN_LEPTO_2018 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             NU_IDADE_N > 4012 
                                                           & 
                                                             NU_IDADE_N <= 4018) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 17] <- as.integer(SINAN_LEPTO_2018 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             NU_IDADE_N > 4018 
                                                           & 
                                                             NU_IDADE_N <= 4059) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 18] <- as.integer(SINAN_LEPTO_2018 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             NU_IDADE_N > 4059) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 19] <- as.integer(SINAN_LEPTO_2018 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "00") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 20] <- as.integer(SINAN_LEPTO_2018 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "01" 
                                                           | 
                                                             CS_ESCOL_N == "02" 
                                                           | 
                                                             CS_ESCOL_N == "03") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 21] <- as.integer(SINAN_LEPTO_2018 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "04") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 22] <- as.integer(SINAN_LEPTO_2018 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "05") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 23] <- as.integer(SINAN_LEPTO_2018 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "06") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 24] <- as.integer(SINAN_LEPTO_2018 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "07") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 25] <- as.integer(SINAN_LEPTO_2018 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "08") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 26] <- as.integer(SINAN_LEPTO_2018 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "10") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 27] <- as.integer(SINAN_LEPTO_2018 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "09") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 28] <- as.integer(SINAN_LEPTO_2018 %>% 
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
  
  AUX[which(AUX$COD_IBGE == i), 29] <- as.integer(SINAN_LEPTO_2018 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_GESTANT == "5") %>% 
                                                    count()
  )
  AUX[which(AUX$COD_IBGE == i), 30] <- as.integer(SINAN_LEPTO_2018 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_GESTANT == "6") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 31] <- as.integer(SINAN_LEPTO_2018 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_GESTANT == "9") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 32] <- as.integer(SINAN_LEPTO_2018 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "01") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 33] <- as.integer(SINAN_LEPTO_2018 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "2") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 34] <- as.integer(SINAN_LEPTO_2018 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "3") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 35] <- as.integer(SINAN_LEPTO_2018 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "4") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 36] <- as.integer(SINAN_LEPTO_2018 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "5") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 37] <- as.integer(SINAN_LEPTO_2018 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "9") %>% 
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2018_GERAL"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2018_GERAL"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2018_GERAL.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2018 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_CB_LAM == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2018 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_CB_CRI == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2018 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_CB_CAI == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2018 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_CB_FOS == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_LEPTO_2018 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_CB_SIN == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_LEPTO_2018 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_CB_PLA == 1) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_LEPTO_2018 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_CB_COR == 1) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(SINAN_LEPTO_2018 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             ANT_CB_ROE == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_LEPTO_2018 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             ANT_CB_GRA == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(SINAN_LEPTO_2018 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_CB_TER == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_LEPTO_2018 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_CB_LIX == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 16] <- as.integer(SINAN_LEPTO_2018 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_CB_OUT == 4018) %>% 
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2018_SIT_RISCO"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2018_SIT_RISCO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2018_SIT_RISCO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2018 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CLI_FEBRE == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2018 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CLI_MIALGI == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2018 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CLI_CEFALE == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2018 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CLI_PROST == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_LEPTO_2018 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CLI_CONGES == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_LEPTO_2018 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_PANTUR == 1) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_LEPTO_2018 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_VOMITO == 1) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(SINAN_LEPTO_2018 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             CLI_DIARRE == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_LEPTO_2018 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             CLI_ICTERI == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(SINAN_LEPTO_2018 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_RENAL == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_LEPTO_2018 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_RESPIR == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 16] <- as.integer(SINAN_LEPTO_2018 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_CARDIA == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 17] <- as.integer(SINAN_LEPTO_2018 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_HEMOPU == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 18] <- as.integer(SINAN_LEPTO_2018 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_HEMORR == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 19] <- as.integer(SINAN_LEPTO_2018 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_MENING == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 20] <- as.integer(SINAN_LEPTO_2018 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_OUTROS == 1) %>% 
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2018_SINTOMAS"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2018_SINTOMAS"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2018_SINTOMAS.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2018 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ATE_HOSP == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2018 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ATE_HOSP == 2) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2018_HOSPITALIZACAO"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2018_HOSPITALIZACAO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2018_HOSPITALIZACAO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2018 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CLASSI_FIN == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2018 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CLASSI_FIN == 2) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2018_CLASSI_FIN"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2018_CLASSI_FIN"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2018_CLASSI_FIN.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2018 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CRITERIO == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2018 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CRITERIO == 2) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2018_CRIT_ENCER"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2018_CRIT_ENCER"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2018_CRIT_ENCER.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2018 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CON_AREA == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2018 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AREA == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2018 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AREA == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2018 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AREA == 9) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4: ncol(AUX)] <- apply(AUX[, 4: ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2018_LPI"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2018_LPI"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2018_LPI.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2018 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CON_AMBIEN == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2018 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2018 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2018 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 4) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_LEPTO_2018 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 9) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4: ncol(AUX)] <- apply(AUX[, 4: ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2018_AMBIENTE"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2018_AMBIENTE"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2018_AMBIENTE.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2018 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CON_AMBIEN == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2018 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2018 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 9) %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4: ncol(AUX)] <- apply(AUX[, 4: ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2018_TRABALHO"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2018_TRABALHO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2018_TRABALHO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2018 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          EVOLUCAO == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2018 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            EVOLUCAO == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2018 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            EVOLUCAO == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2018 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            EVOLUCAO == 9) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4: ncol(AUX)] <- apply(AUX[, 4: ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2018_EVOLUCAO"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2018_EVOLUCAO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2018_EVOLUCAO.csv"), 
           row.names = FALSE)

rm(SINAN_LEPTO_2018)

##################################################################################
#################   2019    ######################################################
##################################################################################

SINAN_LEPTO_2019 <- LEPTONET %>% filter(ID_REGIONA == ID_REG | ID_RG_RESI == ID_REG,
                                        NU_ANO == 2019)

assign(paste0("RS", RS, "_LEPTO_2019_SINAN"), SINAN_LEPTO_2019)

write.csv (assign(paste0("RS", RS, "_LEPTO_2019_SINAN"), SINAN_LEPTO_2019), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2019_SINAN.csv"), 
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
  
  AUX[which(AUX == i), 2] <- as.integer(SINAN_LEPTO_2019 %>%
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==201901)%>%
                                          count()
  )
  
  AUX[which(AUX == i), 3] <- as.integer(SINAN_LEPTO_2019 %>% 
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==201902) %>% 
                                          count()
  )
  
  AUX[which(AUX == i), 4] <- as.integer(SINAN_LEPTO_2019 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==201903) %>% 
                                          count()
  )
  
  AUX[which(AUX == i),5] <- as.integer(SINAN_LEPTO_2019 %>% 
                                         filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==201904) %>% 
                                         count()
  )
  
  AUX[which(AUX == i), 6] <- as.integer(SINAN_LEPTO_2019 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==201905) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 7] <- as.integer(SINAN_LEPTO_2019 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==201906) %>%
                                          count()
  )
  
  AUX[which(AUX == i), 8] <- as.integer(SINAN_LEPTO_2019 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==201907) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 9] <- as.integer(SINAN_LEPTO_2019 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==201908) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 10] <- as.integer(SINAN_LEPTO_2019 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201909) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 11] <- as.integer(SINAN_LEPTO_2019 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201910) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 12] <- as.integer(SINAN_LEPTO_2019 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201911) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 13] <- as.integer(SINAN_LEPTO_2019 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201912) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 14] <- as.integer(SINAN_LEPTO_2019 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201913) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 15] <- as.integer(SINAN_LEPTO_2019 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201914) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 16] <- as.integer(SINAN_LEPTO_2019 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201915) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 17] <- as.integer(SINAN_LEPTO_2019 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201916) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 18] <- as.integer(SINAN_LEPTO_2019 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201917) %>%  
                                           count() 
  )
  
  AUX[which(AUX == i), 19] <- as.integer(SINAN_LEPTO_2019 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201918) %>%      
                                           count() 
  )
  
  AUX[which(AUX == i), 20] <- as.integer(SINAN_LEPTO_2019 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201919) %>%
                                           count() 
  )
  
  AUX[which(AUX == i),  21] <- as.integer(SINAN_LEPTO_2019 %>% 
                                            filter(ID_MN_RESI ==i & 
                                                     SEM_PRI ==201920) %>%
                                            count() 
  )
  
  AUX[which(AUX == i), 22] <- as.integer(SINAN_LEPTO_2019 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201921) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 23] <- as.integer(SINAN_LEPTO_2019 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201922) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 24] <- as.integer(SINAN_LEPTO_2019 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201923) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 25] <- as.integer(SINAN_LEPTO_2019 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201924) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 26] <- as.integer(SINAN_LEPTO_2019 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201925) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 27] <- as.integer(SINAN_LEPTO_2019 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201926) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 28] <- as.integer(SINAN_LEPTO_2019 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201927) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 29] <- as.integer(SINAN_LEPTO_2019 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201928) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 30] <- as.integer(SINAN_LEPTO_2019 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201929) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 31] <- as.integer(SINAN_LEPTO_2019 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201930) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 32] <- as.integer(SINAN_LEPTO_2019 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201931) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 33] <- as.integer(SINAN_LEPTO_2019 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201932) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 34] <- as.integer(SINAN_LEPTO_2019 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201933) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 35] <- as.integer(SINAN_LEPTO_2019 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201934) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 36] <- as.integer(SINAN_LEPTO_2019 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201935) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 37] <- as.integer(SINAN_LEPTO_2019 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201936) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 38] <- as.integer(SINAN_LEPTO_2019 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201937) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 39] <- as.integer(SINAN_LEPTO_2019 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201938) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 40] <- as.integer(SINAN_LEPTO_2019 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201939) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 41] <- as.integer(SINAN_LEPTO_2019 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201940) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 42] <- as.integer(SINAN_LEPTO_2019 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201941) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 43] <- as.integer(SINAN_LEPTO_2019 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201942) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 44] <- as.integer(SINAN_LEPTO_2019 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201943) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 45] <- as.integer(SINAN_LEPTO_2019 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201944) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 46] <- as.integer(SINAN_LEPTO_2019 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201945) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 47] <- as.integer(SINAN_LEPTO_2019 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201946) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 48] <- as.integer(SINAN_LEPTO_2019 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201947) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 49] <- as.integer(SINAN_LEPTO_2019 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201948) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 50] <- as.integer(SINAN_LEPTO_2019 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201949) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 51] <- as.integer(SINAN_LEPTO_2019 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201950) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 52] <- as.integer(SINAN_LEPTO_2019 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201951) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 53] <- as.integer(SINAN_LEPTO_2019 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201952) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 54] <- as.integer(SINAN_LEPTO_2019 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201953) %>%
                                           count() 
  )
}

AUX[, 1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX[(nrow(AUX)+ 1), 2:54] <- apply(AUX[, 2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2019_SE_Notificados"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2019_SE_Notificados"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2019_SE_Notificados.csv"), 
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
  
  AUX[which(AUX == i), 2] <- as.integer(SINAN_LEPTO_2019 %>%
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==201901,
                                                 CLASSI_FIN == 1)%>%
                                          count()
  )
  
  AUX[which(AUX == i), 3] <- as.integer(SINAN_LEPTO_2019 %>% 
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==201902,
                                                 CLASSI_FIN == 1) %>% 
                                          count()
  )
  
  AUX[which(AUX == i), 4] <- as.integer(SINAN_LEPTO_2019 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==201903,
                                                 CLASSI_FIN == 1) %>% 
                                          count()
  )
  
  AUX[which(AUX == i),5] <- as.integer(SINAN_LEPTO_2019 %>% 
                                         filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==201904,
                                                CLASSI_FIN == 1) %>% 
                                         count()
  )
  
  AUX[which(AUX == i), 6] <- as.integer(SINAN_LEPTO_2019 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==201905,
                                                 CLASSI_FIN == 1) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 7] <- as.integer(SINAN_LEPTO_2019 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==201906,
                                                 CLASSI_FIN == 1) %>%
                                          count()
  )
  
  AUX[which(AUX == i), 8] <- as.integer(SINAN_LEPTO_2019 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==201907,
                                                 CLASSI_FIN == 1) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 9] <- as.integer(SINAN_LEPTO_2019 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==201908,
                                                 CLASSI_FIN == 1) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 10] <- as.integer(SINAN_LEPTO_2019 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201909,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 11] <- as.integer(SINAN_LEPTO_2019 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201910,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 12] <- as.integer(SINAN_LEPTO_2019 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201911,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 13] <- as.integer(SINAN_LEPTO_2019 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201912,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 14] <- as.integer(SINAN_LEPTO_2019 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201913,
                                                  CLASSI_FIN == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 15] <- as.integer(SINAN_LEPTO_2019 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201914,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 16] <- as.integer(SINAN_LEPTO_2019 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201915,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 17] <- as.integer(SINAN_LEPTO_2019 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201916,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 18] <- as.integer(SINAN_LEPTO_2019 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201917,
                                                  CLASSI_FIN == 1) %>%  
                                           count() 
  )
  
  AUX[which(AUX == i), 19] <- as.integer(SINAN_LEPTO_2019 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201918,
                                                  CLASSI_FIN == 1) %>%      
                                           count() 
  )
  
  AUX[which(AUX == i), 20] <- as.integer(SINAN_LEPTO_2019 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201919,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i),  21] <- as.integer(SINAN_LEPTO_2019 %>% 
                                            filter(ID_MN_RESI ==i & 
                                                     SEM_PRI ==201920,
                                                   CLASSI_FIN == 1) %>%
                                            count() 
  )
  
  AUX[which(AUX == i), 22] <- as.integer(SINAN_LEPTO_2019 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201921,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 23] <- as.integer(SINAN_LEPTO_2019 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201922,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 24] <- as.integer(SINAN_LEPTO_2019 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201923,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 25] <- as.integer(SINAN_LEPTO_2019 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201924,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 26] <- as.integer(SINAN_LEPTO_2019 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201925,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 27] <- as.integer(SINAN_LEPTO_2019 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201926,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 28] <- as.integer(SINAN_LEPTO_2019 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201927,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 29] <- as.integer(SINAN_LEPTO_2019 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201928,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 30] <- as.integer(SINAN_LEPTO_2019 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201929,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 31] <- as.integer(SINAN_LEPTO_2019 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201930,
                                                  CLASSI_FIN == 1) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 32] <- as.integer(SINAN_LEPTO_2019 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201931,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 33] <- as.integer(SINAN_LEPTO_2019 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201932,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 34] <- as.integer(SINAN_LEPTO_2019 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201933,
                                                  CLASSI_FIN == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 35] <- as.integer(SINAN_LEPTO_2019 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201934,
                                                  CLASSI_FIN == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 36] <- as.integer(SINAN_LEPTO_2019 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201935,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 37] <- as.integer(SINAN_LEPTO_2019 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201936,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 38] <- as.integer(SINAN_LEPTO_2019 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201937,
                                                  CLASSI_FIN == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 39] <- as.integer(SINAN_LEPTO_2019 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201938,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 40] <- as.integer(SINAN_LEPTO_2019 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201939,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 41] <- as.integer(SINAN_LEPTO_2019 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201940,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 42] <- as.integer(SINAN_LEPTO_2019 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201941,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 43] <- as.integer(SINAN_LEPTO_2019 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201942,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 44] <- as.integer(SINAN_LEPTO_2019 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201943,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 45] <- as.integer(SINAN_LEPTO_2019 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201944,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 46] <- as.integer(SINAN_LEPTO_2019 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201945,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 47] <- as.integer(SINAN_LEPTO_2019 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201946,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 48] <- as.integer(SINAN_LEPTO_2019 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201947,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 49] <- as.integer(SINAN_LEPTO_2019 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201948,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 50] <- as.integer(SINAN_LEPTO_2019 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201949,
                                                  CLASSI_FIN == 1) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 51] <- as.integer(SINAN_LEPTO_2019 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201950,
                                                  CLASSI_FIN == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 52] <- as.integer(SINAN_LEPTO_2019 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201951,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 53] <- as.integer(SINAN_LEPTO_2019 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==201952,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 54] <- as.integer(SINAN_LEPTO_2019 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==201953,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
}

AUX[, 1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX[(nrow(AUX)+ 1), 2:54] <- apply(AUX[, 2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2019_SE_Confirmados"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2019_SE_Confirmados"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2019_SE_Confirmados.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2019 %>% 
                                                   filter(ID_MN_RESI == i) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2019 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CLASSI_FIN == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2019 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CS_ZONA == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2019 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CS_ZONA == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_LEPTO_2019 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CS_ZONA == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_LEPTO_2019 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ZONA == 9 
                                                           |
                                                             is.na(CS_ZONA)) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_LEPTO_2019 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_SEXO == "F") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(SINAN_LEPTO_2019 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_SEXO == "M") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_LEPTO_2019 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             NU_IDADE_N <= 3012) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(SINAN_LEPTO_2019 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             NU_IDADE_N > 4000 
                                                           & 
                                                             NU_IDADE_N <= 4005) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_LEPTO_2019 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             NU_IDADE_N > 4005 
                                                           & 
                                                             NU_IDADE_N <= 4012) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 16] <- as.integer(SINAN_LEPTO_2019 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             NU_IDADE_N > 4012 
                                                           & 
                                                             NU_IDADE_N <= 4018) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 17] <- as.integer(SINAN_LEPTO_2019 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             NU_IDADE_N > 4018 
                                                           & 
                                                             NU_IDADE_N <= 4059) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 18] <- as.integer(SINAN_LEPTO_2019 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             NU_IDADE_N > 4059) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 19] <- as.integer(SINAN_LEPTO_2019 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "00") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 20] <- as.integer(SINAN_LEPTO_2019 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "01" 
                                                           | 
                                                             CS_ESCOL_N == "02" 
                                                           | 
                                                             CS_ESCOL_N == "03") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 21] <- as.integer(SINAN_LEPTO_2019 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "04") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 22] <- as.integer(SINAN_LEPTO_2019 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "05") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 23] <- as.integer(SINAN_LEPTO_2019 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "06") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 24] <- as.integer(SINAN_LEPTO_2019 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "07") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 25] <- as.integer(SINAN_LEPTO_2019 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "08") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 26] <- as.integer(SINAN_LEPTO_2019 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "10") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 27] <- as.integer(SINAN_LEPTO_2019 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "09") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 28] <- as.integer(SINAN_LEPTO_2019 %>% 
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
  
  AUX[which(AUX$COD_IBGE == i), 29] <- as.integer(SINAN_LEPTO_2019 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_GESTANT == "5") %>% 
                                                    count()
  )
  AUX[which(AUX$COD_IBGE == i), 30] <- as.integer(SINAN_LEPTO_2019 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_GESTANT == "6") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 31] <- as.integer(SINAN_LEPTO_2019 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_GESTANT == "9") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 32] <- as.integer(SINAN_LEPTO_2019 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "01") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 33] <- as.integer(SINAN_LEPTO_2019 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "2") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 34] <- as.integer(SINAN_LEPTO_2019 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "3") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 35] <- as.integer(SINAN_LEPTO_2019 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "4") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 36] <- as.integer(SINAN_LEPTO_2019 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "5") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 37] <- as.integer(SINAN_LEPTO_2019 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "9") %>% 
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2019_GERAL"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2019_GERAL"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2019_GERAL.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2019 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_CB_LAM == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2019 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_CB_CRI == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2019 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_CB_CAI == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2019 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_CB_FOS == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_LEPTO_2019 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_CB_SIN == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_LEPTO_2019 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_CB_PLA == 1) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_LEPTO_2019 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_CB_COR == 1) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(SINAN_LEPTO_2019 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             ANT_CB_ROE == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_LEPTO_2019 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             ANT_CB_GRA == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(SINAN_LEPTO_2019 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_CB_TER == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_LEPTO_2019 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_CB_LIX == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 16] <- as.integer(SINAN_LEPTO_2019 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_CB_OUT == 4018) %>% 
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2019_SIT_RISCO"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2019_SIT_RISCO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2019_SIT_RISCO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2019 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CLI_FEBRE == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2019 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CLI_MIALGI == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2019 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CLI_CEFALE == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2019 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CLI_PROST == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_LEPTO_2019 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CLI_CONGES == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_LEPTO_2019 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_PANTUR == 1) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_LEPTO_2019 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_VOMITO == 1) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(SINAN_LEPTO_2019 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             CLI_DIARRE == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_LEPTO_2019 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             CLI_ICTERI == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(SINAN_LEPTO_2019 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_RENAL == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_LEPTO_2019 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_RESPIR == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 16] <- as.integer(SINAN_LEPTO_2019 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_CARDIA == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 17] <- as.integer(SINAN_LEPTO_2019 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_HEMOPU == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 18] <- as.integer(SINAN_LEPTO_2019 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_HEMORR == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 19] <- as.integer(SINAN_LEPTO_2019 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_MENING == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 20] <- as.integer(SINAN_LEPTO_2019 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_OUTROS == 1) %>% 
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2019_SINTOMAS"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2019_SINTOMAS"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2019_SINTOMAS.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2019 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ATE_HOSP == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2019 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ATE_HOSP == 2) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2019_HOSPITALIZACAO"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2019_HOSPITALIZACAO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2019_HOSPITALIZACAO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2019 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CLASSI_FIN == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2019 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CLASSI_FIN == 2) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2019_CLASSI_FIN"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2019_CLASSI_FIN"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2019_CLASSI_FIN.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2019 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CRITERIO == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2019 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CRITERIO == 2) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2019_CRIT_ENCER"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2019_CRIT_ENCER"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2019_CRIT_ENCER.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2019 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CON_AREA == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2019 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AREA == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2019 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AREA == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2019 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AREA == 9) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4: ncol(AUX)] <- apply(AUX[, 4: ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2019_LPI"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2019_LPI"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2019_LPI.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2019 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CON_AMBIEN == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2019 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2019 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2019 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 4) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_LEPTO_2019 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 9) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4: ncol(AUX)] <- apply(AUX[, 4: ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2019_AMBIENTE"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2019_AMBIENTE"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2019_AMBIENTE.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2019 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CON_AMBIEN == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2019 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2019 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 9) %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4: ncol(AUX)] <- apply(AUX[, 4: ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2019_TRABALHO"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2019_TRABALHO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2019_TRABALHO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2019 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          EVOLUCAO == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2019 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            EVOLUCAO == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2019 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            EVOLUCAO == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2019 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            EVOLUCAO == 9) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4: ncol(AUX)] <- apply(AUX[, 4: ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2019_EVOLUCAO"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2019_EVOLUCAO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2019_EVOLUCAO.csv"), 
           row.names = FALSE)

rm(SINAN_LEPTO_2019)

##################################################################################
#################   2020    ######################################################
##################################################################################

SINAN_LEPTO_2020 <- LEPTONET %>% filter(ID_REGIONA == ID_REG | ID_RG_RESI == ID_REG,
                                        NU_ANO == 2020)

assign(paste0("RS", RS, "_LEPTO_2020_SINAN"), SINAN_LEPTO_2020)

write.csv (assign(paste0("RS", RS, "_LEPTO_2020_SINAN"), SINAN_LEPTO_2020), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2020_SINAN.csv"), 
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
  
  AUX[which(AUX == i), 2] <- as.integer(SINAN_LEPTO_2020 %>%
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==202001)%>%
                                          count()
  )
  
  AUX[which(AUX == i), 3] <- as.integer(SINAN_LEPTO_2020 %>% 
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==202002) %>% 
                                          count()
  )
  
  AUX[which(AUX == i), 4] <- as.integer(SINAN_LEPTO_2020 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==202003) %>% 
                                          count()
  )
  
  AUX[which(AUX == i),5] <- as.integer(SINAN_LEPTO_2020 %>% 
                                         filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==202004) %>% 
                                         count()
  )
  
  AUX[which(AUX == i), 6] <- as.integer(SINAN_LEPTO_2020 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==202005) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 7] <- as.integer(SINAN_LEPTO_2020 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==202006) %>%
                                          count()
  )
  
  AUX[which(AUX == i), 8] <- as.integer(SINAN_LEPTO_2020 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==202007) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 9] <- as.integer(SINAN_LEPTO_2020 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==202008) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 10] <- as.integer(SINAN_LEPTO_2020 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202009) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 11] <- as.integer(SINAN_LEPTO_2020 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202010) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 12] <- as.integer(SINAN_LEPTO_2020 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202011) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 13] <- as.integer(SINAN_LEPTO_2020 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202012) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 14] <- as.integer(SINAN_LEPTO_2020 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202013) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 15] <- as.integer(SINAN_LEPTO_2020 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202014) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 16] <- as.integer(SINAN_LEPTO_2020 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202015) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 17] <- as.integer(SINAN_LEPTO_2020 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202016) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 18] <- as.integer(SINAN_LEPTO_2020 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202017) %>%  
                                           count() 
  )
  
  AUX[which(AUX == i), 19] <- as.integer(SINAN_LEPTO_2020 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202018) %>%      
                                           count() 
  )
  
  AUX[which(AUX == i), 20] <- as.integer(SINAN_LEPTO_2020 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202019) %>%
                                           count() 
  )
  
  AUX[which(AUX == i),  21] <- as.integer(SINAN_LEPTO_2020 %>% 
                                            filter(ID_MN_RESI ==i & 
                                                     SEM_PRI ==202020) %>%
                                            count() 
  )
  
  AUX[which(AUX == i), 22] <- as.integer(SINAN_LEPTO_2020 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202021) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 23] <- as.integer(SINAN_LEPTO_2020 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202022) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 24] <- as.integer(SINAN_LEPTO_2020 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202023) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 25] <- as.integer(SINAN_LEPTO_2020 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202024) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 26] <- as.integer(SINAN_LEPTO_2020 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202025) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 27] <- as.integer(SINAN_LEPTO_2020 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202026) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 28] <- as.integer(SINAN_LEPTO_2020 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202027) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 29] <- as.integer(SINAN_LEPTO_2020 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202028) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 30] <- as.integer(SINAN_LEPTO_2020 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202029) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 31] <- as.integer(SINAN_LEPTO_2020 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202030) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 32] <- as.integer(SINAN_LEPTO_2020 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202031) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 33] <- as.integer(SINAN_LEPTO_2020 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202032) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 34] <- as.integer(SINAN_LEPTO_2020 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202033) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 35] <- as.integer(SINAN_LEPTO_2020 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202034) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 36] <- as.integer(SINAN_LEPTO_2020 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202035) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 37] <- as.integer(SINAN_LEPTO_2020 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202036) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 38] <- as.integer(SINAN_LEPTO_2020 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202037) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 39] <- as.integer(SINAN_LEPTO_2020 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202038) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 40] <- as.integer(SINAN_LEPTO_2020 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202039) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 41] <- as.integer(SINAN_LEPTO_2020 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202040) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 42] <- as.integer(SINAN_LEPTO_2020 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202041) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 43] <- as.integer(SINAN_LEPTO_2020 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202042) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 44] <- as.integer(SINAN_LEPTO_2020 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202043) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 45] <- as.integer(SINAN_LEPTO_2020 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202044) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 46] <- as.integer(SINAN_LEPTO_2020 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202045) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 47] <- as.integer(SINAN_LEPTO_2020 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202046) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 48] <- as.integer(SINAN_LEPTO_2020 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202047) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 49] <- as.integer(SINAN_LEPTO_2020 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202048) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 50] <- as.integer(SINAN_LEPTO_2020 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202049) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 51] <- as.integer(SINAN_LEPTO_2020 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202050) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 52] <- as.integer(SINAN_LEPTO_2020 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202051) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 53] <- as.integer(SINAN_LEPTO_2020 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202052) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 54] <- as.integer(SINAN_LEPTO_2020 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202053) %>%
                                           count() 
  )
}

AUX[, 1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX[(nrow(AUX)+ 1), 2:54] <- apply(AUX[, 2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2020_SE_Notificados"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2020_SE_Notificados"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2020_SE_Notificados.csv"), 
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
  
  AUX[which(AUX == i), 2] <- as.integer(SINAN_LEPTO_2020 %>%
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==202001,
                                                 CLASSI_FIN == 1)%>%
                                          count()
  )
  
  AUX[which(AUX == i), 3] <- as.integer(SINAN_LEPTO_2020 %>% 
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==202002,
                                                 CLASSI_FIN == 1) %>% 
                                          count()
  )
  
  AUX[which(AUX == i), 4] <- as.integer(SINAN_LEPTO_2020 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==202003,
                                                 CLASSI_FIN == 1) %>% 
                                          count()
  )
  
  AUX[which(AUX == i),5] <- as.integer(SINAN_LEPTO_2020 %>% 
                                         filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==202004,
                                                CLASSI_FIN == 1) %>% 
                                         count()
  )
  
  AUX[which(AUX == i), 6] <- as.integer(SINAN_LEPTO_2020 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==202005,
                                                 CLASSI_FIN == 1) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 7] <- as.integer(SINAN_LEPTO_2020 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==202006,
                                                 CLASSI_FIN == 1) %>%
                                          count()
  )
  
  AUX[which(AUX == i), 8] <- as.integer(SINAN_LEPTO_2020 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==202007,
                                                 CLASSI_FIN == 1) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 9] <- as.integer(SINAN_LEPTO_2020 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==202008,
                                                 CLASSI_FIN == 1) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 10] <- as.integer(SINAN_LEPTO_2020 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202009,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 11] <- as.integer(SINAN_LEPTO_2020 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202010,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 12] <- as.integer(SINAN_LEPTO_2020 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202011,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 13] <- as.integer(SINAN_LEPTO_2020 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202012,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 14] <- as.integer(SINAN_LEPTO_2020 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202013,
                                                  CLASSI_FIN == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 15] <- as.integer(SINAN_LEPTO_2020 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202014,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 16] <- as.integer(SINAN_LEPTO_2020 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202015,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 17] <- as.integer(SINAN_LEPTO_2020 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202016,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 18] <- as.integer(SINAN_LEPTO_2020 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202017,
                                                  CLASSI_FIN == 1) %>%  
                                           count() 
  )
  
  AUX[which(AUX == i), 19] <- as.integer(SINAN_LEPTO_2020 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202018,
                                                  CLASSI_FIN == 1) %>%      
                                           count() 
  )
  
  AUX[which(AUX == i), 20] <- as.integer(SINAN_LEPTO_2020 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202019,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i),  21] <- as.integer(SINAN_LEPTO_2020 %>% 
                                            filter(ID_MN_RESI ==i & 
                                                     SEM_PRI ==202020,
                                                   CLASSI_FIN == 1) %>%
                                            count() 
  )
  
  AUX[which(AUX == i), 22] <- as.integer(SINAN_LEPTO_2020 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202021,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 23] <- as.integer(SINAN_LEPTO_2020 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202022,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 24] <- as.integer(SINAN_LEPTO_2020 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202023,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 25] <- as.integer(SINAN_LEPTO_2020 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202024,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 26] <- as.integer(SINAN_LEPTO_2020 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202025,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 27] <- as.integer(SINAN_LEPTO_2020 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202026,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 28] <- as.integer(SINAN_LEPTO_2020 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202027,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 29] <- as.integer(SINAN_LEPTO_2020 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202028,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 30] <- as.integer(SINAN_LEPTO_2020 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202029,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 31] <- as.integer(SINAN_LEPTO_2020 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202030,
                                                  CLASSI_FIN == 1) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 32] <- as.integer(SINAN_LEPTO_2020 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202031,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 33] <- as.integer(SINAN_LEPTO_2020 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202032,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 34] <- as.integer(SINAN_LEPTO_2020 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202033,
                                                  CLASSI_FIN == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 35] <- as.integer(SINAN_LEPTO_2020 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202034,
                                                  CLASSI_FIN == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 36] <- as.integer(SINAN_LEPTO_2020 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202035,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 37] <- as.integer(SINAN_LEPTO_2020 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202036,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 38] <- as.integer(SINAN_LEPTO_2020 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202037,
                                                  CLASSI_FIN == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 39] <- as.integer(SINAN_LEPTO_2020 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202038,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 40] <- as.integer(SINAN_LEPTO_2020 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202039,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 41] <- as.integer(SINAN_LEPTO_2020 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202040,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 42] <- as.integer(SINAN_LEPTO_2020 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202041,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 43] <- as.integer(SINAN_LEPTO_2020 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202042,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 44] <- as.integer(SINAN_LEPTO_2020 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202043,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 45] <- as.integer(SINAN_LEPTO_2020 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202044,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 46] <- as.integer(SINAN_LEPTO_2020 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202045,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 47] <- as.integer(SINAN_LEPTO_2020 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202046,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 48] <- as.integer(SINAN_LEPTO_2020 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202047,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 49] <- as.integer(SINAN_LEPTO_2020 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202048,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 50] <- as.integer(SINAN_LEPTO_2020 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202049,
                                                  CLASSI_FIN == 1) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 51] <- as.integer(SINAN_LEPTO_2020 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202050,
                                                  CLASSI_FIN == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 52] <- as.integer(SINAN_LEPTO_2020 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202051,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 53] <- as.integer(SINAN_LEPTO_2020 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202052,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 54] <- as.integer(SINAN_LEPTO_2020 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202053,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
}

AUX[, 1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX[(nrow(AUX)+ 1), 2:54] <- apply(AUX[, 2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2020_SE_Confirmados"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2020_SE_Confirmados"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2020_SE_Confirmados.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2020 %>% 
                                                   filter(ID_MN_RESI == i) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2020 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CLASSI_FIN == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2020 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CS_ZONA == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2020 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CS_ZONA == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_LEPTO_2020 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CS_ZONA == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_LEPTO_2020 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ZONA == 9 
                                                           |
                                                             is.na(CS_ZONA)) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_LEPTO_2020 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_SEXO == "F") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(SINAN_LEPTO_2020 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_SEXO == "M") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_LEPTO_2020 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             NU_IDADE_N <= 3012) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(SINAN_LEPTO_2020 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             NU_IDADE_N > 4000 
                                                           & 
                                                             NU_IDADE_N <= 4005) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_LEPTO_2020 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             NU_IDADE_N > 4005 
                                                           & 
                                                             NU_IDADE_N <= 4012) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 16] <- as.integer(SINAN_LEPTO_2020 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             NU_IDADE_N > 4012 
                                                           & 
                                                             NU_IDADE_N <= 4018) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 17] <- as.integer(SINAN_LEPTO_2020 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             NU_IDADE_N > 4018 
                                                           & 
                                                             NU_IDADE_N <= 4059) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 18] <- as.integer(SINAN_LEPTO_2020 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             NU_IDADE_N > 4059) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 19] <- as.integer(SINAN_LEPTO_2020 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "00") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 20] <- as.integer(SINAN_LEPTO_2020 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "01" 
                                                           | 
                                                             CS_ESCOL_N == "02" 
                                                           | 
                                                             CS_ESCOL_N == "03") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 21] <- as.integer(SINAN_LEPTO_2020 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "04") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 22] <- as.integer(SINAN_LEPTO_2020 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "05") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 23] <- as.integer(SINAN_LEPTO_2020 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "06") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 24] <- as.integer(SINAN_LEPTO_2020 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "07") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 25] <- as.integer(SINAN_LEPTO_2020 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "08") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 26] <- as.integer(SINAN_LEPTO_2020 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "10") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 27] <- as.integer(SINAN_LEPTO_2020 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "09") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 28] <- as.integer(SINAN_LEPTO_2020 %>% 
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
  
  AUX[which(AUX$COD_IBGE == i), 29] <- as.integer(SINAN_LEPTO_2020 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_GESTANT == "5") %>% 
                                                    count()
  )
  AUX[which(AUX$COD_IBGE == i), 30] <- as.integer(SINAN_LEPTO_2020 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_GESTANT == "6") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 31] <- as.integer(SINAN_LEPTO_2020 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_GESTANT == "9") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 32] <- as.integer(SINAN_LEPTO_2020 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "01") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 33] <- as.integer(SINAN_LEPTO_2020 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "2") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 34] <- as.integer(SINAN_LEPTO_2020 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "3") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 35] <- as.integer(SINAN_LEPTO_2020 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "4") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 36] <- as.integer(SINAN_LEPTO_2020 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "5") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 37] <- as.integer(SINAN_LEPTO_2020 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "9") %>% 
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2020_GERAL"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2020_GERAL"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2020_GERAL.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2020 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_CB_LAM == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2020 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_CB_CRI == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2020 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_CB_CAI == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2020 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_CB_FOS == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_LEPTO_2020 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_CB_SIN == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_LEPTO_2020 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_CB_PLA == 1) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_LEPTO_2020 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_CB_COR == 1) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(SINAN_LEPTO_2020 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             ANT_CB_ROE == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_LEPTO_2020 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             ANT_CB_GRA == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(SINAN_LEPTO_2020 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_CB_TER == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_LEPTO_2020 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_CB_LIX == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 16] <- as.integer(SINAN_LEPTO_2020 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_CB_OUT == 4018) %>% 
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2020_SIT_RISCO"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2020_SIT_RISCO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2020_SIT_RISCO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2020 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CLI_FEBRE == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2020 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CLI_MIALGI == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2020 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CLI_CEFALE == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2020 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CLI_PROST == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_LEPTO_2020 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CLI_CONGES == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_LEPTO_2020 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_PANTUR == 1) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_LEPTO_2020 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_VOMITO == 1) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(SINAN_LEPTO_2020 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             CLI_DIARRE == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_LEPTO_2020 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             CLI_ICTERI == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(SINAN_LEPTO_2020 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_RENAL == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_LEPTO_2020 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_RESPIR == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 16] <- as.integer(SINAN_LEPTO_2020 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_CARDIA == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 17] <- as.integer(SINAN_LEPTO_2020 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_HEMOPU == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 18] <- as.integer(SINAN_LEPTO_2020 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_HEMORR == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 19] <- as.integer(SINAN_LEPTO_2020 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_MENING == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 20] <- as.integer(SINAN_LEPTO_2020 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_OUTROS == 1) %>% 
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2020_SINTOMAS"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2020_SINTOMAS"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2020_SINTOMAS.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2020 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ATE_HOSP == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2020 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ATE_HOSP == 2) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2020_HOSPITALIZACAO"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2020_HOSPITALIZACAO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2020_HOSPITALIZACAO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2020 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CLASSI_FIN == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2020 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CLASSI_FIN == 2) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2020_CLASSI_FIN"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2020_CLASSI_FIN"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2020_CLASSI_FIN.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2020 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CRITERIO == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2020 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CRITERIO == 2) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2020_CRIT_ENCER"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2020_CRIT_ENCER"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2020_CRIT_ENCER.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2020 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CON_AREA == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2020 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AREA == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2020 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AREA == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2020 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AREA == 9) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4: ncol(AUX)] <- apply(AUX[, 4: ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2020_LPI"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2020_LPI"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2020_LPI.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2020 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CON_AMBIEN == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2020 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2020 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2020 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 4) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_LEPTO_2020 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 9) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4: ncol(AUX)] <- apply(AUX[, 4: ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2020_AMBIENTE"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2020_AMBIENTE"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2020_AMBIENTE.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2020 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CON_AMBIEN == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2020 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2020 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 9) %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4: ncol(AUX)] <- apply(AUX[, 4: ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2020_TRABALHO"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2020_TRABALHO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2020_TRABALHO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2020 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          EVOLUCAO == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2020 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            EVOLUCAO == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2020 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            EVOLUCAO == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2020 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            EVOLUCAO == 9) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4: ncol(AUX)] <- apply(AUX[, 4: ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2020_EVOLUCAO"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2020_EVOLUCAO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2020_EVOLUCAO.csv"), 
           row.names = FALSE)

rm(SINAN_LEPTO_2020)

##################################################################################
#################   2021    ######################################################
##################################################################################

SINAN_LEPTO_2021 <- LEPTONET %>% filter(ID_REGIONA == ID_REG | ID_RG_RESI == ID_REG,
                                        NU_ANO == 2021)

assign(paste0("RS", RS, "_LEPTO_2021_SINAN"), SINAN_LEPTO_2021)

write.csv (assign(paste0("RS", RS, "_LEPTO_2021_SINAN"), SINAN_LEPTO_2021), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2021_SINAN.csv"), 
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
  
  AUX[which(AUX == i), 2] <- as.integer(SINAN_LEPTO_2021 %>%
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==202101)%>%
                                          count()
  )
  
  AUX[which(AUX == i), 3] <- as.integer(SINAN_LEPTO_2021 %>% 
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==202102) %>% 
                                          count()
  )
  
  AUX[which(AUX == i), 4] <- as.integer(SINAN_LEPTO_2021 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==202103) %>% 
                                          count()
  )
  
  AUX[which(AUX == i),5] <- as.integer(SINAN_LEPTO_2021 %>% 
                                         filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==202104) %>% 
                                         count()
  )
  
  AUX[which(AUX == i), 6] <- as.integer(SINAN_LEPTO_2021 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==202105) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 7] <- as.integer(SINAN_LEPTO_2021 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==202106) %>%
                                          count()
  )
  
  AUX[which(AUX == i), 8] <- as.integer(SINAN_LEPTO_2021 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==202107) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 9] <- as.integer(SINAN_LEPTO_2021 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==202108) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 10] <- as.integer(SINAN_LEPTO_2021 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202109) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 11] <- as.integer(SINAN_LEPTO_2021 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202110) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 12] <- as.integer(SINAN_LEPTO_2021 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202111) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 13] <- as.integer(SINAN_LEPTO_2021 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202112) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 14] <- as.integer(SINAN_LEPTO_2021 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202113) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 15] <- as.integer(SINAN_LEPTO_2021 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202114) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 16] <- as.integer(SINAN_LEPTO_2021 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202115) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 17] <- as.integer(SINAN_LEPTO_2021 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202116) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 18] <- as.integer(SINAN_LEPTO_2021 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202117) %>%  
                                           count() 
  )
  
  AUX[which(AUX == i), 19] <- as.integer(SINAN_LEPTO_2021 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202118) %>%      
                                           count() 
  )
  
  AUX[which(AUX == i), 20] <- as.integer(SINAN_LEPTO_2021 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202119) %>%
                                           count() 
  )
  
  AUX[which(AUX == i),  21] <- as.integer(SINAN_LEPTO_2021 %>% 
                                            filter(ID_MN_RESI ==i & 
                                                     SEM_PRI ==202120) %>%
                                            count() 
  )
  
  AUX[which(AUX == i), 22] <- as.integer(SINAN_LEPTO_2021 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202121) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 23] <- as.integer(SINAN_LEPTO_2021 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202122) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 24] <- as.integer(SINAN_LEPTO_2021 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202123) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 25] <- as.integer(SINAN_LEPTO_2021 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202124) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 26] <- as.integer(SINAN_LEPTO_2021 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202125) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 27] <- as.integer(SINAN_LEPTO_2021 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202126) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 28] <- as.integer(SINAN_LEPTO_2021 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202127) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 29] <- as.integer(SINAN_LEPTO_2021 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202128) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 30] <- as.integer(SINAN_LEPTO_2021 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202129) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 31] <- as.integer(SINAN_LEPTO_2021 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202130) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 32] <- as.integer(SINAN_LEPTO_2021 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202131) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 33] <- as.integer(SINAN_LEPTO_2021 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202132) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 34] <- as.integer(SINAN_LEPTO_2021 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202133) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 35] <- as.integer(SINAN_LEPTO_2021 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202134) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 36] <- as.integer(SINAN_LEPTO_2021 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202135) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 37] <- as.integer(SINAN_LEPTO_2021 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202136) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 38] <- as.integer(SINAN_LEPTO_2021 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202137) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 39] <- as.integer(SINAN_LEPTO_2021 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202138) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 40] <- as.integer(SINAN_LEPTO_2021 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202139) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 41] <- as.integer(SINAN_LEPTO_2021 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202140) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 42] <- as.integer(SINAN_LEPTO_2021 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202141) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 43] <- as.integer(SINAN_LEPTO_2021 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202142) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 44] <- as.integer(SINAN_LEPTO_2021 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202143) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 45] <- as.integer(SINAN_LEPTO_2021 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202144) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 46] <- as.integer(SINAN_LEPTO_2021 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202145) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 47] <- as.integer(SINAN_LEPTO_2021 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202146) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 48] <- as.integer(SINAN_LEPTO_2021 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202147) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 49] <- as.integer(SINAN_LEPTO_2021 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202148) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 50] <- as.integer(SINAN_LEPTO_2021 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202149) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 51] <- as.integer(SINAN_LEPTO_2021 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202150) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 52] <- as.integer(SINAN_LEPTO_2021 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202151) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 53] <- as.integer(SINAN_LEPTO_2021 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202152) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 54] <- as.integer(SINAN_LEPTO_2021 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202153) %>%
                                           count() 
  )
}

AUX[, 1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX[(nrow(AUX)+ 1), 2:54] <- apply(AUX[, 2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2021_SE_Notificados"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2021_SE_Notificados"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2021_SE_Notificados.csv"), 
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
  
  AUX[which(AUX == i), 2] <- as.integer(SINAN_LEPTO_2021 %>%
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==202101,
                                                 CLASSI_FIN == 1)%>%
                                          count()
  )
  
  AUX[which(AUX == i), 3] <- as.integer(SINAN_LEPTO_2021 %>% 
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==202102,
                                                 CLASSI_FIN == 1) %>% 
                                          count()
  )
  
  AUX[which(AUX == i), 4] <- as.integer(SINAN_LEPTO_2021 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==202103,
                                                 CLASSI_FIN == 1) %>% 
                                          count()
  )
  
  AUX[which(AUX == i),5] <- as.integer(SINAN_LEPTO_2021 %>% 
                                         filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==202104,
                                                CLASSI_FIN == 1) %>% 
                                         count()
  )
  
  AUX[which(AUX == i), 6] <- as.integer(SINAN_LEPTO_2021 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==202105,
                                                 CLASSI_FIN == 1) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 7] <- as.integer(SINAN_LEPTO_2021 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==202106,
                                                 CLASSI_FIN == 1) %>%
                                          count()
  )
  
  AUX[which(AUX == i), 8] <- as.integer(SINAN_LEPTO_2021 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==202107,
                                                 CLASSI_FIN == 1) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 9] <- as.integer(SINAN_LEPTO_2021 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==202108,
                                                 CLASSI_FIN == 1) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 10] <- as.integer(SINAN_LEPTO_2021 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202109,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 11] <- as.integer(SINAN_LEPTO_2021 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202110,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 12] <- as.integer(SINAN_LEPTO_2021 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202111,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 13] <- as.integer(SINAN_LEPTO_2021 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202112,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 14] <- as.integer(SINAN_LEPTO_2021 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202113,
                                                  CLASSI_FIN == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 15] <- as.integer(SINAN_LEPTO_2021 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202114,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 16] <- as.integer(SINAN_LEPTO_2021 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202115,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 17] <- as.integer(SINAN_LEPTO_2021 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202116,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 18] <- as.integer(SINAN_LEPTO_2021 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202117,
                                                  CLASSI_FIN == 1) %>%  
                                           count() 
  )
  
  AUX[which(AUX == i), 19] <- as.integer(SINAN_LEPTO_2021 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202118,
                                                  CLASSI_FIN == 1) %>%      
                                           count() 
  )
  
  AUX[which(AUX == i), 20] <- as.integer(SINAN_LEPTO_2021 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202119,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i),  21] <- as.integer(SINAN_LEPTO_2021 %>% 
                                            filter(ID_MN_RESI ==i & 
                                                     SEM_PRI ==202120,
                                                   CLASSI_FIN == 1) %>%
                                            count() 
  )
  
  AUX[which(AUX == i), 22] <- as.integer(SINAN_LEPTO_2021 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202121,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 23] <- as.integer(SINAN_LEPTO_2021 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202122,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 24] <- as.integer(SINAN_LEPTO_2021 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202123,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 25] <- as.integer(SINAN_LEPTO_2021 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202124,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 26] <- as.integer(SINAN_LEPTO_2021 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202125,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 27] <- as.integer(SINAN_LEPTO_2021 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202126,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 28] <- as.integer(SINAN_LEPTO_2021 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202127,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 29] <- as.integer(SINAN_LEPTO_2021 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202128,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 30] <- as.integer(SINAN_LEPTO_2021 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202129,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 31] <- as.integer(SINAN_LEPTO_2021 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202130,
                                                  CLASSI_FIN == 1) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 32] <- as.integer(SINAN_LEPTO_2021 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202131,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 33] <- as.integer(SINAN_LEPTO_2021 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202132,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 34] <- as.integer(SINAN_LEPTO_2021 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202133,
                                                  CLASSI_FIN == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 35] <- as.integer(SINAN_LEPTO_2021 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202134,
                                                  CLASSI_FIN == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 36] <- as.integer(SINAN_LEPTO_2021 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202135,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 37] <- as.integer(SINAN_LEPTO_2021 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202136,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 38] <- as.integer(SINAN_LEPTO_2021 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202137,
                                                  CLASSI_FIN == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 39] <- as.integer(SINAN_LEPTO_2021 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202138,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 40] <- as.integer(SINAN_LEPTO_2021 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202139,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 41] <- as.integer(SINAN_LEPTO_2021 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202140,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 42] <- as.integer(SINAN_LEPTO_2021 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202141,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 43] <- as.integer(SINAN_LEPTO_2021 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202142,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 44] <- as.integer(SINAN_LEPTO_2021 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202143,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 45] <- as.integer(SINAN_LEPTO_2021 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202144,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 46] <- as.integer(SINAN_LEPTO_2021 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202145,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 47] <- as.integer(SINAN_LEPTO_2021 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202146,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 48] <- as.integer(SINAN_LEPTO_2021 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202147,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 49] <- as.integer(SINAN_LEPTO_2021 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202148,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 50] <- as.integer(SINAN_LEPTO_2021 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202149,
                                                  CLASSI_FIN == 1) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 51] <- as.integer(SINAN_LEPTO_2021 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202150,
                                                  CLASSI_FIN == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 52] <- as.integer(SINAN_LEPTO_2021 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202151,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 53] <- as.integer(SINAN_LEPTO_2021 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202152,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 54] <- as.integer(SINAN_LEPTO_2021 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202153,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
}

AUX[, 1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX[(nrow(AUX)+ 1), 2:54] <- apply(AUX[, 2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2021_SE_Confirmados"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2021_SE_Confirmados"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2021_SE_Confirmados.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2021 %>% 
                                                   filter(ID_MN_RESI == i) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2021 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CLASSI_FIN == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2021 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CS_ZONA == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2021 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CS_ZONA == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_LEPTO_2021 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CS_ZONA == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_LEPTO_2021 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ZONA == 9 
                                                           |
                                                             is.na(CS_ZONA)) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_LEPTO_2021 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_SEXO == "F") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(SINAN_LEPTO_2021 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_SEXO == "M") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_LEPTO_2021 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             NU_IDADE_N <= 3012) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(SINAN_LEPTO_2021 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             NU_IDADE_N > 4000 
                                                           & 
                                                             NU_IDADE_N <= 4005) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_LEPTO_2021 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             NU_IDADE_N > 4005 
                                                           & 
                                                             NU_IDADE_N <= 4012) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 16] <- as.integer(SINAN_LEPTO_2021 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             NU_IDADE_N > 4012 
                                                           & 
                                                             NU_IDADE_N <= 4018) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 17] <- as.integer(SINAN_LEPTO_2021 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             NU_IDADE_N > 4018 
                                                           & 
                                                             NU_IDADE_N <= 4059) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 18] <- as.integer(SINAN_LEPTO_2021 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             NU_IDADE_N > 4059) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 19] <- as.integer(SINAN_LEPTO_2021 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "00") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 20] <- as.integer(SINAN_LEPTO_2021 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "01" 
                                                           | 
                                                             CS_ESCOL_N == "02" 
                                                           | 
                                                             CS_ESCOL_N == "03") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 21] <- as.integer(SINAN_LEPTO_2021 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "04") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 22] <- as.integer(SINAN_LEPTO_2021 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "05") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 23] <- as.integer(SINAN_LEPTO_2021 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "06") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 24] <- as.integer(SINAN_LEPTO_2021 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "07") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 25] <- as.integer(SINAN_LEPTO_2021 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "08") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 26] <- as.integer(SINAN_LEPTO_2021 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "10") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 27] <- as.integer(SINAN_LEPTO_2021 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "09") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 28] <- as.integer(SINAN_LEPTO_2021 %>% 
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
  
  AUX[which(AUX$COD_IBGE == i), 29] <- as.integer(SINAN_LEPTO_2021 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_GESTANT == "5") %>% 
                                                    count()
  )
  AUX[which(AUX$COD_IBGE == i), 30] <- as.integer(SINAN_LEPTO_2021 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_GESTANT == "6") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 31] <- as.integer(SINAN_LEPTO_2021 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_GESTANT == "9") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 32] <- as.integer(SINAN_LEPTO_2021 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "01") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 33] <- as.integer(SINAN_LEPTO_2021 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "2") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 34] <- as.integer(SINAN_LEPTO_2021 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "3") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 35] <- as.integer(SINAN_LEPTO_2021 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "4") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 36] <- as.integer(SINAN_LEPTO_2021 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "5") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 37] <- as.integer(SINAN_LEPTO_2021 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "9") %>% 
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2021_GERAL"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2021_GERAL"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2021_GERAL.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2021 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_CB_LAM == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2021 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_CB_CRI == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2021 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_CB_CAI == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2021 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_CB_FOS == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_LEPTO_2021 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_CB_SIN == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_LEPTO_2021 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_CB_PLA == 1) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_LEPTO_2021 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_CB_COR == 1) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(SINAN_LEPTO_2021 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             ANT_CB_ROE == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_LEPTO_2021 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             ANT_CB_GRA == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(SINAN_LEPTO_2021 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_CB_TER == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_LEPTO_2021 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_CB_LIX == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 16] <- as.integer(SINAN_LEPTO_2021 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_CB_OUT == 4018) %>% 
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2021_SIT_RISCO"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2021_SIT_RISCO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2021_SIT_RISCO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2021 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CLI_FEBRE == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2021 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CLI_MIALGI == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2021 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CLI_CEFALE == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2021 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CLI_PROST == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_LEPTO_2021 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CLI_CONGES == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_LEPTO_2021 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_PANTUR == 1) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_LEPTO_2021 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_VOMITO == 1) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(SINAN_LEPTO_2021 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             CLI_DIARRE == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_LEPTO_2021 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             CLI_ICTERI == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(SINAN_LEPTO_2021 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_RENAL == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_LEPTO_2021 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_RESPIR == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 16] <- as.integer(SINAN_LEPTO_2021 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_CARDIA == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 17] <- as.integer(SINAN_LEPTO_2021 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_HEMOPU == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 18] <- as.integer(SINAN_LEPTO_2021 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_HEMORR == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 19] <- as.integer(SINAN_LEPTO_2021 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_MENING == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 20] <- as.integer(SINAN_LEPTO_2021 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_OUTROS == 1) %>% 
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2021_SINTOMAS"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2021_SINTOMAS"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2021_SINTOMAS.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2021 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ATE_HOSP == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2021 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ATE_HOSP == 2) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2021_HOSPITALIZACAO"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2021_HOSPITALIZACAO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2021_HOSPITALIZACAO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2021 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CLASSI_FIN == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2021 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CLASSI_FIN == 2) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2021_CLASSI_FIN"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2021_CLASSI_FIN"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2021_CLASSI_FIN.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2021 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CRITERIO == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2021 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CRITERIO == 2) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2021_CRIT_ENCER"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2021_CRIT_ENCER"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2021_CRIT_ENCER.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2021 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CON_AREA == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2021 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AREA == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2021 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AREA == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2021 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AREA == 9) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4: ncol(AUX)] <- apply(AUX[, 4: ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2021_LPI"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2021_LPI"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2021_LPI.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2021 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CON_AMBIEN == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2021 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2021 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2021 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 4) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_LEPTO_2021 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 9) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4: ncol(AUX)] <- apply(AUX[, 4: ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2021_AMBIENTE"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2021_AMBIENTE"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2021_AMBIENTE.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2021 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CON_AMBIEN == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2021 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2021 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 9) %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4: ncol(AUX)] <- apply(AUX[, 4: ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2021_TRABALHO"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2021_TRABALHO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2021_TRABALHO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2021 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          EVOLUCAO == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2021 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            EVOLUCAO == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2021 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            EVOLUCAO == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2021 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            EVOLUCAO == 9) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4: ncol(AUX)] <- apply(AUX[, 4: ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2021_EVOLUCAO"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2021_EVOLUCAO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2021_EVOLUCAO.csv"), 
           row.names = FALSE)

rm(SINAN_LEPTO_2021)

##################################################################################
#################   2022    ######################################################
##################################################################################

SINAN_LEPTO_2022 <- LEPTONET %>% filter(ID_REGIONA == ID_REG | ID_RG_RESI == ID_REG,
                                        NU_ANO == 2022)

assign(paste0("RS", RS, "_LEPTO_2022_SINAN"), SINAN_LEPTO_2022)

write.csv (assign(paste0("RS", RS, "_LEPTO_2022_SINAN"), SINAN_LEPTO_2022), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2022_SINAN.csv"), 
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
  
  AUX[which(AUX == i), 2] <- as.integer(SINAN_LEPTO_2022 %>%
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==202201)%>%
                                          count()
  )
  
  AUX[which(AUX == i), 3] <- as.integer(SINAN_LEPTO_2022 %>% 
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==202202) %>% 
                                          count()
  )
  
  AUX[which(AUX == i), 4] <- as.integer(SINAN_LEPTO_2022 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==202203) %>% 
                                          count()
  )
  
  AUX[which(AUX == i),5] <- as.integer(SINAN_LEPTO_2022 %>% 
                                         filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==202204) %>% 
                                         count()
  )
  
  AUX[which(AUX == i), 6] <- as.integer(SINAN_LEPTO_2022 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==202205) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 7] <- as.integer(SINAN_LEPTO_2022 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==202206) %>%
                                          count()
  )
  
  AUX[which(AUX == i), 8] <- as.integer(SINAN_LEPTO_2022 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==202207) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 9] <- as.integer(SINAN_LEPTO_2022 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==202208) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 10] <- as.integer(SINAN_LEPTO_2022 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202209) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 11] <- as.integer(SINAN_LEPTO_2022 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202210) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 12] <- as.integer(SINAN_LEPTO_2022 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202211) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 13] <- as.integer(SINAN_LEPTO_2022 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202212) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 14] <- as.integer(SINAN_LEPTO_2022 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202213) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 15] <- as.integer(SINAN_LEPTO_2022 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202214) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 16] <- as.integer(SINAN_LEPTO_2022 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202215) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 17] <- as.integer(SINAN_LEPTO_2022 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202216) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 18] <- as.integer(SINAN_LEPTO_2022 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202217) %>%  
                                           count() 
  )
  
  AUX[which(AUX == i), 19] <- as.integer(SINAN_LEPTO_2022 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202218) %>%      
                                           count() 
  )
  
  AUX[which(AUX == i), 20] <- as.integer(SINAN_LEPTO_2022 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202219) %>%
                                           count() 
  )
  
  AUX[which(AUX == i),  21] <- as.integer(SINAN_LEPTO_2022 %>% 
                                            filter(ID_MN_RESI ==i & 
                                                     SEM_PRI ==202220) %>%
                                            count() 
  )
  
  AUX[which(AUX == i), 22] <- as.integer(SINAN_LEPTO_2022 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202221) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 23] <- as.integer(SINAN_LEPTO_2022 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202222) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 24] <- as.integer(SINAN_LEPTO_2022 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202223) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 25] <- as.integer(SINAN_LEPTO_2022 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202224) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 26] <- as.integer(SINAN_LEPTO_2022 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202225) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 27] <- as.integer(SINAN_LEPTO_2022 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202226) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 28] <- as.integer(SINAN_LEPTO_2022 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202227) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 29] <- as.integer(SINAN_LEPTO_2022 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202228) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 30] <- as.integer(SINAN_LEPTO_2022 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202229) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 31] <- as.integer(SINAN_LEPTO_2022 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202230) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 32] <- as.integer(SINAN_LEPTO_2022 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202231) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 33] <- as.integer(SINAN_LEPTO_2022 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202232) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 34] <- as.integer(SINAN_LEPTO_2022 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202233) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 35] <- as.integer(SINAN_LEPTO_2022 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202234) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 36] <- as.integer(SINAN_LEPTO_2022 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202235) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 37] <- as.integer(SINAN_LEPTO_2022 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202236) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 38] <- as.integer(SINAN_LEPTO_2022 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202237) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 39] <- as.integer(SINAN_LEPTO_2022 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202238) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 40] <- as.integer(SINAN_LEPTO_2022 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202239) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 41] <- as.integer(SINAN_LEPTO_2022 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202240) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 42] <- as.integer(SINAN_LEPTO_2022 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202241) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 43] <- as.integer(SINAN_LEPTO_2022 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202242) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 44] <- as.integer(SINAN_LEPTO_2022 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202243) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 45] <- as.integer(SINAN_LEPTO_2022 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202244) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 46] <- as.integer(SINAN_LEPTO_2022 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202245) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 47] <- as.integer(SINAN_LEPTO_2022 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202246) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 48] <- as.integer(SINAN_LEPTO_2022 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202247) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 49] <- as.integer(SINAN_LEPTO_2022 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202248) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 50] <- as.integer(SINAN_LEPTO_2022 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202249) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 51] <- as.integer(SINAN_LEPTO_2022 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202250) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 52] <- as.integer(SINAN_LEPTO_2022 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202251) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 53] <- as.integer(SINAN_LEPTO_2022 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202252) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 54] <- as.integer(SINAN_LEPTO_2022 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202253) %>%
                                           count() 
  )
}

AUX[, 1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX[(nrow(AUX)+ 1), 2:54] <- apply(AUX[, 2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2022_SE_Notificados"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2022_SE_Notificados"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2022_SE_Notificados.csv"), 
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
  
  AUX[which(AUX == i), 2] <- as.integer(SINAN_LEPTO_2022 %>%
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==202201,
                                                 CLASSI_FIN == 1)%>%
                                          count()
  )
  
  AUX[which(AUX == i), 3] <- as.integer(SINAN_LEPTO_2022 %>% 
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==202202,
                                                 CLASSI_FIN == 1) %>% 
                                          count()
  )
  
  AUX[which(AUX == i), 4] <- as.integer(SINAN_LEPTO_2022 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==202203,
                                                 CLASSI_FIN == 1) %>% 
                                          count()
  )
  
  AUX[which(AUX == i),5] <- as.integer(SINAN_LEPTO_2022 %>% 
                                         filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==202204,
                                                CLASSI_FIN == 1) %>% 
                                         count()
  )
  
  AUX[which(AUX == i), 6] <- as.integer(SINAN_LEPTO_2022 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==202205,
                                                 CLASSI_FIN == 1) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 7] <- as.integer(SINAN_LEPTO_2022 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==202206,
                                                 CLASSI_FIN == 1) %>%
                                          count()
  )
  
  AUX[which(AUX == i), 8] <- as.integer(SINAN_LEPTO_2022 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==202207,
                                                 CLASSI_FIN == 1) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 9] <- as.integer(SINAN_LEPTO_2022 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==202208,
                                                 CLASSI_FIN == 1) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 10] <- as.integer(SINAN_LEPTO_2022 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202209,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 11] <- as.integer(SINAN_LEPTO_2022 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202210,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 12] <- as.integer(SINAN_LEPTO_2022 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202211,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 13] <- as.integer(SINAN_LEPTO_2022 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202212,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 14] <- as.integer(SINAN_LEPTO_2022 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202213,
                                                  CLASSI_FIN == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 15] <- as.integer(SINAN_LEPTO_2022 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202214,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 16] <- as.integer(SINAN_LEPTO_2022 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202215,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 17] <- as.integer(SINAN_LEPTO_2022 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202216,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 18] <- as.integer(SINAN_LEPTO_2022 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202217,
                                                  CLASSI_FIN == 1) %>%  
                                           count() 
  )
  
  AUX[which(AUX == i), 19] <- as.integer(SINAN_LEPTO_2022 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202218,
                                                  CLASSI_FIN == 1) %>%      
                                           count() 
  )
  
  AUX[which(AUX == i), 20] <- as.integer(SINAN_LEPTO_2022 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202219,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i),  21] <- as.integer(SINAN_LEPTO_2022 %>% 
                                            filter(ID_MN_RESI ==i & 
                                                     SEM_PRI ==202220,
                                                   CLASSI_FIN == 1) %>%
                                            count() 
  )
  
  AUX[which(AUX == i), 22] <- as.integer(SINAN_LEPTO_2022 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202221,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 23] <- as.integer(SINAN_LEPTO_2022 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202222,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 24] <- as.integer(SINAN_LEPTO_2022 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202223,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 25] <- as.integer(SINAN_LEPTO_2022 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202224,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 26] <- as.integer(SINAN_LEPTO_2022 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202225,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 27] <- as.integer(SINAN_LEPTO_2022 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202226,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 28] <- as.integer(SINAN_LEPTO_2022 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202227,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 29] <- as.integer(SINAN_LEPTO_2022 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202228,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 30] <- as.integer(SINAN_LEPTO_2022 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202229,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 31] <- as.integer(SINAN_LEPTO_2022 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202230,
                                                  CLASSI_FIN == 1) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 32] <- as.integer(SINAN_LEPTO_2022 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202231,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 33] <- as.integer(SINAN_LEPTO_2022 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202232,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 34] <- as.integer(SINAN_LEPTO_2022 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202233,
                                                  CLASSI_FIN == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 35] <- as.integer(SINAN_LEPTO_2022 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202234,
                                                  CLASSI_FIN == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 36] <- as.integer(SINAN_LEPTO_2022 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202235,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 37] <- as.integer(SINAN_LEPTO_2022 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202236,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 38] <- as.integer(SINAN_LEPTO_2022 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202237,
                                                  CLASSI_FIN == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 39] <- as.integer(SINAN_LEPTO_2022 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202238,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 40] <- as.integer(SINAN_LEPTO_2022 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202239,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 41] <- as.integer(SINAN_LEPTO_2022 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202240,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 42] <- as.integer(SINAN_LEPTO_2022 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202241,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 43] <- as.integer(SINAN_LEPTO_2022 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202242,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 44] <- as.integer(SINAN_LEPTO_2022 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202243,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 45] <- as.integer(SINAN_LEPTO_2022 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202244,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 46] <- as.integer(SINAN_LEPTO_2022 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202245,
                                                  CLASSI_FIN == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 47] <- as.integer(SINAN_LEPTO_2022 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202246,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 48] <- as.integer(SINAN_LEPTO_2022 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202247,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 49] <- as.integer(SINAN_LEPTO_2022 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202248,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 50] <- as.integer(SINAN_LEPTO_2022 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202249,
                                                  CLASSI_FIN == 1) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 51] <- as.integer(SINAN_LEPTO_2022 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202250,
                                                  CLASSI_FIN == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 52] <- as.integer(SINAN_LEPTO_2022 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202251,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 53] <- as.integer(SINAN_LEPTO_2022 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==202252,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 54] <- as.integer(SINAN_LEPTO_2022 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==202253,
                                                  CLASSI_FIN == 1) %>%
                                           count() 
  )
}

AUX[, 1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX[(nrow(AUX)+ 1), 2:54] <- apply(AUX[, 2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2022_SE_Confirmados"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2022_SE_Confirmados"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2022_SE_Confirmados.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2022 %>% 
                                                   filter(ID_MN_RESI == i) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2022 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CLASSI_FIN == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2022 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CS_ZONA == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2022 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CS_ZONA == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_LEPTO_2022 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CS_ZONA == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_LEPTO_2022 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ZONA == 9 
                                                           |
                                                             is.na(CS_ZONA)) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_LEPTO_2022 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_SEXO == "F") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(SINAN_LEPTO_2022 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_SEXO == "M") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_LEPTO_2022 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             NU_IDADE_N <= 3012) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(SINAN_LEPTO_2022 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             NU_IDADE_N > 4000 
                                                           & 
                                                             NU_IDADE_N <= 4005) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_LEPTO_2022 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             NU_IDADE_N > 4005 
                                                           & 
                                                             NU_IDADE_N <= 4012) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 16] <- as.integer(SINAN_LEPTO_2022 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             NU_IDADE_N > 4012 
                                                           & 
                                                             NU_IDADE_N <= 4018) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 17] <- as.integer(SINAN_LEPTO_2022 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             NU_IDADE_N > 4018 
                                                           & 
                                                             NU_IDADE_N <= 4059) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 18] <- as.integer(SINAN_LEPTO_2022 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             NU_IDADE_N > 4059) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 19] <- as.integer(SINAN_LEPTO_2022 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "00") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 20] <- as.integer(SINAN_LEPTO_2022 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "01" 
                                                           | 
                                                             CS_ESCOL_N == "02" 
                                                           | 
                                                             CS_ESCOL_N == "03") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 21] <- as.integer(SINAN_LEPTO_2022 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "04") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 22] <- as.integer(SINAN_LEPTO_2022 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "05") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 23] <- as.integer(SINAN_LEPTO_2022 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "06") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 24] <- as.integer(SINAN_LEPTO_2022 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "07") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 25] <- as.integer(SINAN_LEPTO_2022 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "08") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 26] <- as.integer(SINAN_LEPTO_2022 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "10") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 27] <- as.integer(SINAN_LEPTO_2022 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "09") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 28] <- as.integer(SINAN_LEPTO_2022 %>% 
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
  
  AUX[which(AUX$COD_IBGE == i), 29] <- as.integer(SINAN_LEPTO_2022 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_GESTANT == "5") %>% 
                                                    count()
  )
  AUX[which(AUX$COD_IBGE == i), 30] <- as.integer(SINAN_LEPTO_2022 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_GESTANT == "6") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 31] <- as.integer(SINAN_LEPTO_2022 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_GESTANT == "9") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 32] <- as.integer(SINAN_LEPTO_2022 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "01") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 33] <- as.integer(SINAN_LEPTO_2022 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "2") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 34] <- as.integer(SINAN_LEPTO_2022 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "3") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 35] <- as.integer(SINAN_LEPTO_2022 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "4") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 36] <- as.integer(SINAN_LEPTO_2022 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "5") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 37] <- as.integer(SINAN_LEPTO_2022 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "9") %>% 
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2022_GERAL"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2022_GERAL"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2022_GERAL.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2022 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_CB_LAM == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2022 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_CB_CRI == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2022 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_CB_CAI == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2022 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_CB_FOS == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_LEPTO_2022 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_CB_SIN == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_LEPTO_2022 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_CB_PLA == 1) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_LEPTO_2022 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_CB_COR == 1) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(SINAN_LEPTO_2022 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             ANT_CB_ROE == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_LEPTO_2022 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             ANT_CB_GRA == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(SINAN_LEPTO_2022 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_CB_TER == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_LEPTO_2022 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_CB_LIX == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 16] <- as.integer(SINAN_LEPTO_2022 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_CB_OUT == 4018) %>% 
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2022_SIT_RISCO"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2022_SIT_RISCO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2022_SIT_RISCO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2022 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CLI_FEBRE == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2022 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CLI_MIALGI == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2022 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CLI_CEFALE == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2022 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CLI_PROST == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_LEPTO_2022 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CLI_CONGES == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_LEPTO_2022 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_PANTUR == 1) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_LEPTO_2022 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_VOMITO == 1) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(SINAN_LEPTO_2022 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             CLI_DIARRE == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_LEPTO_2022 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             CLI_ICTERI == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(SINAN_LEPTO_2022 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_RENAL == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_LEPTO_2022 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_RESPIR == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 16] <- as.integer(SINAN_LEPTO_2022 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_CARDIA == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 17] <- as.integer(SINAN_LEPTO_2022 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_HEMOPU == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 18] <- as.integer(SINAN_LEPTO_2022 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_HEMORR == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 19] <- as.integer(SINAN_LEPTO_2022 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_MENING == 1) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 20] <- as.integer(SINAN_LEPTO_2022 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CLI_OUTROS == 1) %>% 
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2022_SINTOMAS"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2022_SINTOMAS"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2022_SINTOMAS.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2022 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ATE_HOSP == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2022 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ATE_HOSP == 2) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2022_HOSPITALIZACAO"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2022_HOSPITALIZACAO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2022_HOSPITALIZACAO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2022 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CLASSI_FIN == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2022 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CLASSI_FIN == 2) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2022_CLASSI_FIN"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2022_CLASSI_FIN"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2022_CLASSI_FIN.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2022 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CRITERIO == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2022 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CRITERIO == 2) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2022_CRIT_ENCER"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2022_CRIT_ENCER"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2022_CRIT_ENCER.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2022 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CON_AREA == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2022 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AREA == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2022 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AREA == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2022 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AREA == 9) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4: ncol(AUX)] <- apply(AUX[, 4: ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2022_LPI"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2022_LPI"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2022_LPI.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2022 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CON_AMBIEN == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2022 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2022 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2022 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 4) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_LEPTO_2022 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 9) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4: ncol(AUX)] <- apply(AUX[, 4: ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2022_AMBIENTE"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2022_AMBIENTE"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2022_AMBIENTE.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2022 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CON_AMBIEN == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2022 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2022 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CON_AMBIEN == 9) %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4: ncol(AUX)] <- apply(AUX[, 4: ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2022_TRABALHO"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2022_TRABALHO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2022_TRABALHO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2022 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          EVOLUCAO == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2022 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            EVOLUCAO == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2022 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            EVOLUCAO == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2022 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            EVOLUCAO == 9) %>%   
                                                   count()
  )
  
} 

AUX[(nrow(AUX) +1), 4: ncol(AUX)] <- apply(AUX[, 4: ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2022_EVOLUCAO"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2022_EVOLUCAO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2022_EVOLUCAO.csv"), 
           row.names = FALSE)

rm(SINAN_LEPTO_2022)




