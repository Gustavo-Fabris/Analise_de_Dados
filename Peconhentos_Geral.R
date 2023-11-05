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

##################################################################
###############    2009   ########################################
##################################################################

####Criando um objeto com a base DBF 2009 do SINAN#################

PECONHENTO2009<- read.dbf(file = "Base_de_Dados/DBF/ANIMPNET2009.DBF",
                    as.is = FALSE)

SINAN_PECONHENTOS_2009 <- PECONHENTO2009 %>% filter(ID_REGIONA == ID_REG | ID_RG_RESI == ID_REG,
                                        NU_ANO == 2009)

assign(paste0("RS", RS, "_PECONHENTOS_2009_SINAN"), SINAN_PECONHENTOS_2009)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2009_SINAN"), SINAN_PECONHENTOS_2009), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2009_SINAN.csv"), 
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
  
  AUX[which(AUX == i), 2] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==200901)%>%
                                          count()
  )
  
  AUX[which(AUX == i), 3] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==200902) %>% 
                                          count()
  )
  
  AUX[which(AUX == i), 4] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==200903) %>% 
                                          count()
  )
  
  AUX[which(AUX == i),5] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                         filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==200904) %>% 
                                         count()
  )
  
  AUX[which(AUX == i), 6] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==200905) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 7] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==200906) %>%
                                          count()
  )
  
  AUX[which(AUX == i), 8] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==200907) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 9] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==200908) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 10] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200909) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 11] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200910) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 12] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200911) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 13] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200912) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 14] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200913) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 15] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200914) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 16] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200915) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 17] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200916) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 18] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200917) %>%  
                                           count() 
  )
  
  AUX[which(AUX == i), 19] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200918) %>%      
                                           count() 
  )
  
  AUX[which(AUX == i), 20] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200919) %>%
                                           count() 
  )
  
  AUX[which(AUX == i),  21] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                            filter(ID_MN_RESI ==i & 
                                                     SEM_PRI ==200920) %>%
                                            count() 
  )
  
  AUX[which(AUX == i), 22] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200921) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 23] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200922) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 24] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200923) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 25] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200924) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 26] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200925) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 27] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200926) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 28] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200927) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 29] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200928) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 30] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200929) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 31] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200930) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 32] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200931) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 33] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200932) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 34] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200933) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 35] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200934) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 36] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200935) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 37] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200936) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 38] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200937) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 39] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200938) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 40] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200939) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 41] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200940) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 42] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200941) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 43] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200942) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 44] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200943) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 45] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200944) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 46] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200945) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 47] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200946) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 48] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200947) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 49] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200948) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 50] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200949) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 51] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200950) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 52] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200951) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 53] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200952) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 54] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200953) %>%
                                           count() 
  )
}

AUX[, 1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX[(nrow(AUX)+ 1), 2:54] <- apply(AUX[, 2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2009_SE_Notificados"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2009_SE_Notificados"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2009_SE_Notificados.csv"), 
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
  
  AUX[which(AUX == i), 2] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==200901,
                                                 TP_ACIDENT == 1)%>%
                                          count()
  )
  
  AUX[which(AUX == i), 3] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==200902,
                                                 TP_ACIDENT == 1) %>% 
                                          count()
  )
  
  AUX[which(AUX == i), 4] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==200903,
                                                 TP_ACIDENT == 1) %>% 
                                          count()
  )
  
  AUX[which(AUX == i),5] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                         filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==200904,
                                                TP_ACIDENT == 1) %>% 
                                         count()
  )
  
  AUX[which(AUX == i), 6] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==200905,
                                                 TP_ACIDENT == 1) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 7] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==200906,
                                                 TP_ACIDENT == 1) %>%
                                          count()
  )
  
  AUX[which(AUX == i), 8] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==200907,
                                                 TP_ACIDENT == 1) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 9] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==200908,
                                                 TP_ACIDENT == 1) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 10] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200909,
                                                  TP_ACIDENT == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 11] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200910,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 12] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200911,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 13] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200912,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 14] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200913,
                                                  TP_ACIDENT == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 15] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200914,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 16] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200915,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 17] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200916,
                                                  TP_ACIDENT == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 18] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200917,
                                                  TP_ACIDENT == 1) %>%  
                                           count() 
  )
  
  AUX[which(AUX == i), 19] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200918,
                                                  TP_ACIDENT == 1) %>%      
                                           count() 
  )
  
  AUX[which(AUX == i), 20] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200919,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i),  21] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                            filter(ID_MN_RESI ==i & 
                                                     SEM_PRI ==200920,
                                                   TP_ACIDENT == 1) %>%
                                            count() 
  )
  
  AUX[which(AUX == i), 22] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200921,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 23] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200922,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 24] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200923,
                                                  TP_ACIDENT == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 25] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200924,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 26] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200925,
                                                  TP_ACIDENT == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 27] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200926,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 28] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200927,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 29] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200928,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 30] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200929,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 31] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200930,
                                                  TP_ACIDENT == 1) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 32] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200931,
                                                  TP_ACIDENT == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 33] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200932,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 34] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200933,
                                                  TP_ACIDENT == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 35] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200934,
                                                  TP_ACIDENT == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 36] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200935,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 37] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200936,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 38] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200937,
                                                  TP_ACIDENT == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 39] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200938,
                                                  TP_ACIDENT == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 40] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200939,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 41] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200940,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 42] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200941,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 43] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200942,
                                                  TP_ACIDENT == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 44] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200943,
                                                  TP_ACIDENT == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 45] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200944,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 46] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200945,
                                                  TP_ACIDENT == 1) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 47] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200946,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 48] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200947,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 49] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200948,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 50] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200949,
                                                  TP_ACIDENT == 1) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 51] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200950,
                                                  TP_ACIDENT == 1) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 52] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200951,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 53] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200952,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 54] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200953,
                                                  TP_ACIDENT == 1) %>%
                                           count() 
  )
}

AUX[, 1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX[(nrow(AUX)+ 1), 2:54] <- apply(AUX[, 2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2009_SE_SERPENTES_Notificados"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2009_SE_SERPENTES_Notificados"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2009_SE_SERPENTES_Notificados.csv"), 
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
  
  AUX[which(AUX == i), 2] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==200901,
                                                 TP_ACIDENT == 2)%>%
                                          count()
  )
  
  AUX[which(AUX == i), 3] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==200902,
                                                 TP_ACIDENT == 2) %>% 
                                          count()
  )
  
  AUX[which(AUX == i), 4] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==200903,
                                                 TP_ACIDENT == 2) %>% 
                                          count()
  )
  
  AUX[which(AUX == i),5] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                         filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==200904,
                                                TP_ACIDENT == 2) %>% 
                                         count()
  )
  
  AUX[which(AUX == i), 6] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==200905,
                                                 TP_ACIDENT == 2) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 7] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==200906,
                                                 TP_ACIDENT == 2) %>%
                                          count()
  )
  
  AUX[which(AUX == i), 8] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==200907,
                                                 TP_ACIDENT == 2) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 9] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==200908,
                                                 TP_ACIDENT == 2) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 10] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200909,
                                                  TP_ACIDENT == 2) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 11] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200910,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 12] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200911,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 13] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200912,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 14] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200913,
                                                  TP_ACIDENT == 2) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 15] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200914,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 16] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200915,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 17] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200916,
                                                  TP_ACIDENT == 2) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 18] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200917,
                                                  TP_ACIDENT == 2) %>%  
                                           count() 
  )
  
  AUX[which(AUX == i), 19] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200918,
                                                  TP_ACIDENT == 2) %>%      
                                           count() 
  )
  
  AUX[which(AUX == i), 20] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200919,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i),  21] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                            filter(ID_MN_RESI ==i & 
                                                     SEM_PRI ==200920,
                                                   TP_ACIDENT == 2) %>%
                                            count() 
  )
  
  AUX[which(AUX == i), 22] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200921,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 23] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200922,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 24] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200923,
                                                  TP_ACIDENT == 2) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 25] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200924,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 26] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200925,
                                                  TP_ACIDENT == 2) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 27] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200926,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 28] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200927,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 29] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200928,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 30] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200929,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 31] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200930,
                                                  TP_ACIDENT == 2) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 32] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200931,
                                                  TP_ACIDENT == 2) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 33] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200932,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 34] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200933,
                                                  TP_ACIDENT == 2) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 35] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200934,
                                                  TP_ACIDENT == 2) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 36] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200935,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 37] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200936,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 38] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200937,
                                                  TP_ACIDENT == 2) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 39] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200938,
                                                  TP_ACIDENT == 2) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 40] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200939,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 41] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200940,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 42] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200941,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 43] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200942,
                                                  TP_ACIDENT == 2) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 44] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200943,
                                                  TP_ACIDENT == 2) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 45] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200944,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 46] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200945,
                                                  TP_ACIDENT == 2) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 47] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200946,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 48] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200947,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 49] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200948,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 50] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200949,
                                                  TP_ACIDENT == 2) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 51] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200950,
                                                  TP_ACIDENT == 2) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 52] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200951,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 53] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200952,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 54] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200953,
                                                  TP_ACIDENT == 2) %>%
                                           count() 
  )
}

AUX[, 1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX[(nrow(AUX)+ 1), 2:54] <- apply(AUX[, 2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2009_SE_ARANHAS_Notificados"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2009_SE_ARANHAS_Notificados"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2009_SE_ARANHAS_Notificados.csv"), 
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
  
  AUX[which(AUX == i), 2] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==200901,
                                                 TP_ACIDENT == 3)%>%
                                          count()
  )
  
  AUX[which(AUX == i), 3] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==200902,
                                                 TP_ACIDENT == 3) %>% 
                                          count()
  )
  
  AUX[which(AUX == i), 4] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==200903,
                                                 TP_ACIDENT == 3) %>% 
                                          count()
  )
  
  AUX[which(AUX == i),5] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                         filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==200904,
                                                TP_ACIDENT == 3) %>% 
                                         count()
  )
  
  AUX[which(AUX == i), 6] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                          filter(ID_MN_RESI ==i &
                                                   SEM_PRI ==200905,
                                                 TP_ACIDENT == 3) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 7] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==200906,
                                                 TP_ACIDENT == 3) %>%
                                          count()
  )
  
  AUX[which(AUX == i), 8] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==200907,
                                                 TP_ACIDENT == 3) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 9] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                          filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==200908,
                                                 TP_ACIDENT == 3) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 10] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200909,
                                                  TP_ACIDENT == 3) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 11] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200910,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 12] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200911,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 13] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200912,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 14] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200913,
                                                  TP_ACIDENT == 3) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 15] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200914,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 16] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200915,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 17] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200916,
                                                  TP_ACIDENT == 3) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 18] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200917,
                                                  TP_ACIDENT == 3) %>%  
                                           count() 
  )
  
  AUX[which(AUX == i), 19] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200918,
                                                  TP_ACIDENT == 3) %>%      
                                           count() 
  )
  
  AUX[which(AUX == i), 20] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200919,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i),  21] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                            filter(ID_MN_RESI ==i & 
                                                     SEM_PRI ==200920,
                                                   TP_ACIDENT == 3) %>%
                                            count() 
  )
  
  AUX[which(AUX == i), 22] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200921,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 23] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200922,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 24] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200923,
                                                  TP_ACIDENT == 3) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 25] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200924,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 26] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200925,
                                                  TP_ACIDENT == 3) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 27] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200926,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 28] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200927,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 29] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200928,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 30] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200929,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 31] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200930,
                                                  TP_ACIDENT == 3) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 32] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200931,
                                                  TP_ACIDENT == 3) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 33] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200932,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 34] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200933,
                                                  TP_ACIDENT == 3) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 35] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200934,
                                                  TP_ACIDENT == 3) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 36] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200935,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 37] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200936,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 38] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200937,
                                                  TP_ACIDENT == 3) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 39] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200938,
                                                  TP_ACIDENT == 3) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 40] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200939,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 41] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200940,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 42] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200941,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 43] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200942,
                                                  TP_ACIDENT == 3) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 44] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200943,
                                                  TP_ACIDENT == 3) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 45] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200944,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 46] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200945,
                                                  TP_ACIDENT == 3) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 47] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200946,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 48] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200947,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 49] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200948,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 50] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200949,
                                                  TP_ACIDENT == 3) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 51] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200950,
                                                  TP_ACIDENT == 3) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 52] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200951,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 53] <- as.integer(SINAN_PECONHENTOS_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                    SEM_PRI ==200952,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 54] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                    SEM_PRI ==200953,
                                                  TP_ACIDENT == 3) %>%
                                           count() 
  )
}

AUX[, 1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX[(nrow(AUX)+ 1), 2:54] <- apply(AUX[, 2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2009_SE_ESCORPIOES_Notificados"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2009_SE_ESCORPIOES_Notificados"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2009_SE_ESCORPIOES_Notificados.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI == i) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CS_ZONA == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CS_ZONA == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            CS_ZONA == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ZONA == 9 
                                                           |
                                                             is.na(CS_ZONA)) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_SEXO == "F") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_SEXO == "M") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             NU_IDADE_N <= 3012) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                             NU_IDADE_N > 4000 
                                                           & 
                                                             NU_IDADE_N <= 4005) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             NU_IDADE_N > 4005 
                                                           & 
                                                             NU_IDADE_N <= 4012) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             NU_IDADE_N > 4012 
                                                           & 
                                                             NU_IDADE_N <= 4018) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 16] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             NU_IDADE_N > 4018 
                                                           & 
                                                             NU_IDADE_N <= 4059) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 17] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             NU_IDADE_N > 4059) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 18] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "00") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 19] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "01" 
                                                           | 
                                                             CS_ESCOL_N == "02" 
                                                           | 
                                                             CS_ESCOL_N == "03") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 20] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "04") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 21] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "05") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 22] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "06") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 23] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "07") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 24] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "08") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 25] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "10") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 26] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_ESCOL_N == "09") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 27] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
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
  
  AUX[which(AUX$COD_IBGE == i), 28] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_GESTANT == "5") %>% 
                                                    count()
  )
  AUX[which(AUX$COD_IBGE == i), 29] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_GESTANT == "6") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 30] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_GESTANT == "9") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 31] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "01") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 32] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "2") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 33] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "3") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 34] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "4") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 35] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "5") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 36] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             CS_RACA == "9") %>% 
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2009_GERAL"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2009_GERAL"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2009_GERAL.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_TEMPO_ == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_TEMPO_ == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_TEMPO_ == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_TEMPO_ == 4) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_TEMPO_ == 5) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_TEMPO_ == 6) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_TEMPO_ == 9) %>%   
                                                    count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2009_TEMPO_ATEND"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2009_TEMPO_ATEND"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2009_TEMPO_ATEND.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_ZONA == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_ZONA == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_ZONA == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_ZONA == 9) %>%   
                                                   count()
  )

}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2009_ZONA_ACIDENTE"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2009_ZONA_ACIDENTE"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2009_ZONA_ACIDENTE.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_LOCA_1 == "01") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_LOCA_1 == "02") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_LOCA_1 == "03") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            ANT_LOCA_1 == "04") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                              ANT_LOCA_1 == "05") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_LOCA_1 == "06") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_LOCA_1 == "07") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_LOCA_1 == "08") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_LOCA_1 == "09") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_LOCA_1 == 10) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             ANT_LOCA_1 == 99) %>%   
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2009_LOCAL_PICADA"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2009_LOCAL_PICADA"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2009_LOCAL_PICADA.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 4) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 5) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             TP_ACIDENT == 6) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                             TP_ACIDENT == 9) %>%   
                                                    count()
  )
  }

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2009_TIPO_ACID"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2009_TIPO_ACID"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2009_TIPO_ACID.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 1,
                                                          TRA_CLASSI == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 1,
                                                          TRA_CLASSI == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 1,
                                                          TRA_CLASSI == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 1,
                                                          TRA_CLASSI == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2009_CLASSI_CASO_SERPENTE"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2009_CLASSI_CASO_SERPENTE"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2009_CLASSI_CASO_SERPENTE.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 1,
                                                          ANI_SERPEN == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 1,
                                                          ANI_SERPEN == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 1,
                                                          ANI_SERPEN == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 1,
                                                          ANI_SERPEN == 4) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 1,
                                                          ANI_SERPEN == 5) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 1,
                                                          ANI_SERPEN == 9) %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2009_TIPO_ACID_SERPENTE"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2009_TIPO_ACID_SERPENTE"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2009_TIPO_ACID_SERPENTE.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 1,
                                                          CON_SOROTE == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 1,
                                                          CON_SOROTE == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 1,
                                                          CON_SOROTE == 9) %>%   
                                                   count()
  )

}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2009_SOROTERAPIA_SERPENTE"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2009_SOROTERAPIA_SERPENTE"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2009_SOROTERAPIA_SERPENTE.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 1,
                                                          COM_LOC == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 1,
                                                          COM_LOC == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 1,
                                                          COM_LOC == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2009_COMP_LOCAIS_SERPENTE"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2009_COMP_LOCAIS_SERPENTE"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2009_COMP_LOCAIS_SERPENTE.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 1,
                                                          COM_SISTEM == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 1,
                                                          COM_SISTEM == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 1,
                                                          COM_SISTEM == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2009_COMP_SIST_SERPENTE"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2009_COMP_SIST_SERPENTE"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2009_COMP_SIST_SERPENTE.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 1,
                                                          MCLI_LOCAL == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 1,
                                                          MCLI_LOCAL == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 1,
                                                          MCLI_LOCAL == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2009_MAN_LOCAIS_SERPENTE"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2009_MAN_LOCAIS_SERPENTE"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2009_MAN_LOCAIS_SERPENTE.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 1,
                                                          CLI_DOR == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 1,
                                                          CLI_EDEMA == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 1,
                                                          CLI_EQUIMO == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 1,
                                                          CLI_NECROS == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 1,
                                                          CLI_LOCAL_ == 1) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2009_MAN_LOCAIS_ESP_SERPENTE"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2009_MAN_LOCAIS_ESP_SERPENTE"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2009_MAN_LOCAIS_ESP_SERPENTE.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 1,
                                                          MCLI_SIST == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 1,
                                                          MCLI_SIST == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 1,
                                                          MCLI_SIST == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2009_MAN_SIST_SERPENTE"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2009_MAN_SIST_SERPENTE"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2009_MAN_SIST_SERPENTE.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 1,
                                                          CLI_NEURO == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 1,
                                                          CLI_HEMORR == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 1,
                                                          CLI_VAGAIS == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 1,
                                                          CLI_MIOLIT == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 1,
                                                          CLI_RENAL == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 1,
                                                          CLI_OUTR_2 == 1) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2009_MAN_SIST_ESP_SERPENTE"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2009_MAN_SIST_ESP_SERPENTE"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2009_MAN_SIST_ESP_SERPENTE.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 2,
                                                          TRA_CLASSI == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 2,
                                                          TRA_CLASSI == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 2,
                                                          TRA_CLASSI == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 2,
                                                          TRA_CLASSI == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2009_CLASSI_CASO_ARANHA"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2009_CLASSI_CASO_ARANHA"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2009_CLASSI_CASO_ARANHA.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 2,
                                                          ANI_ARANHA == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 2,
                                                          ANI_ARANHA == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 2,
                                                          ANI_ARANHA == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 2,
                                                          ANI_ARANHA == 4) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 2,
                                                          ANI_ARANHA == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2009_TIPO_ACID_ARANHA"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2009_TIPO_ACID_ARANHA"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2009_TIPO_ACID_ARANHA.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 2,
                                                          CON_SOROTE == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 2,
                                                          CON_SOROTE == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 2,
                                                          CON_SOROTE == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2009_SOROTERAPIA_ARANHA"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2009_SOROTERAPIA_ARANHA"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2009_SOROTERAPIA_ARANHA.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 2,
                                                          COM_LOC == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 2,
                                                          COM_LOC == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 2,
                                                          COM_LOC == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2009_COMP_LOCAIS_ARANHA"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2009_COMP_LOCAIS_ARANHA"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2009_COMP_LOCAIS_ARANHA.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 2,
                                                          COM_SISTEM == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 2,
                                                          COM_SISTEM == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 2,
                                                          COM_SISTEM == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2009_COMP_SIST_ARANHA"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2009_COMP_SIST_ARANHA"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2009_COMP_SIST_ARANHA.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 2,
                                                          MCLI_LOCAL == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 2,
                                                          MCLI_LOCAL == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 2,
                                                          MCLI_LOCAL == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2009_MAN_LOCAIS_ARANHA"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2009_MAN_LOCAIS_ARANHA"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2009_MAN_LOCAIS_ARANHA.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 2,
                                                          CLI_DOR == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 2,
                                                          CLI_EDEMA == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 2,
                                                          CLI_EQUIMO == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 2,
                                                          CLI_NECROS == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 2,
                                                          CLI_LOCAL_ == 1) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2009_MAN_LOCAIS_ESP_ARANHA"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2009_MAN_LOCAIS_ESP_ARANHA"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2009_MAN_LOCAIS_ESP_ARANHA.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 2,
                                                          MCLI_SIST == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 2,
                                                          MCLI_SIST == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 2,
                                                          MCLI_SIST == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2009_MAN_SIST_ARANHA"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2009_MAN_SIST_ARANHA"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2009_MAN_SIST_ARANHA.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 2,
                                                          CLI_NEURO == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 2,
                                                          CLI_HEMORR == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 2,
                                                          CLI_VAGAIS == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 2,
                                                          CLI_MIOLIT == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 2,
                                                          CLI_RENAL == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 2,
                                                          CLI_OUTR_2 == 1) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2009_MAN_SIST_ESP_ARANHA"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2009_MAN_SIST_ESP_ARANHA"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2009_MAN_SIST_ESP_ARANHA.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 3,
                                                          TRA_CLASSI == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 3,
                                                          TRA_CLASSI == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 3,
                                                          TRA_CLASSI == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 3,
                                                          TRA_CLASSI == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2009_CLASSI_CASO_ESCORPIAO"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2009_CLASSI_CASO_ESCORPIAO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2009_CLASSI_CASO_ESCORPIAO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 3,
                                                          CON_SOROTE == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 3,
                                                          CON_SOROTE == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 3,
                                                          CON_SOROTE == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2009_SOROTERAPIA_ESCORPIAO"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2009_SOROTERAPIA_ESCORPIAO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2009_SOROTERAPIA_ESCORPIAO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 3,
                                                          COM_LOC == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 3,
                                                          COM_LOC == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 3,
                                                          COM_LOC == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2009_COMP_LOCAIS_ESCORPIAO"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2009_COMP_LOCAIS_ESCORPIAO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2009_COMP_LOCAIS_ESCORPIAO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 3,
                                                          COM_SISTEM == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 3,
                                                          COM_SISTEM == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 3,
                                                          COM_SISTEM == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2009_COMP_SIST_ESCORPIAO"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2009_COMP_SIST_ESCORPIAO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2009_COMP_SIST_ESCORPIAO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 3,
                                                          MCLI_LOCAL == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 3,
                                                          MCLI_LOCAL == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 3,
                                                          MCLI_LOCAL == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2009_MAN_LOCAIS_ESCORPIAO"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2009_MAN_LOCAIS_ESCORPIAO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2009_MAN_LOCAIS_ESCORPIAO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 3,
                                                          CLI_DOR == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 3,
                                                          CLI_EDEMA == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 3,
                                                          CLI_EQUIMO == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 3,
                                                          CLI_NECROS == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 3,
                                                          CLI_LOCAL_ == 1) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2009_MAN_LOCAIS_ESP_ESCORPIAO"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2009_MAN_LOCAIS_ESP_ESCORPIAO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2009_MAN_LOCAIS_ESP_ESCORPIAO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 3,
                                                          MCLI_SIST == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 3,
                                                          MCLI_SIST == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 3,
                                                          MCLI_SIST == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2009_MAN_SIST_ESCORPIAO"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2009_MAN_SIST_ESCORPIAO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2009_MAN_SIST_ESCORPIAO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 3,
                                                          CLI_NEURO == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 3,
                                                          CLI_HEMORR == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 3,
                                                          CLI_VAGAIS == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 3,
                                                          CLI_MIOLIT == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 3,
                                                          CLI_RENAL == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 3,
                                                          CLI_OUTR_2 == 1) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2009_MAN_SIST_ESP_ESCORPIAO"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2009_MAN_SIST_ESP_ESCORPIAO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2009_MAN_SIST_ESP_ESCORPIAO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 4,
                                                          TRA_CLASSI == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 4,
                                                          TRA_CLASSI == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 4,
                                                          TRA_CLASSI == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 4,
                                                          TRA_CLASSI == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2009_CLASSI_CASO_LAGARTA"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2009_CLASSI_CASO_LAGARTA"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2009_CLASSI_CASO_LAGARTA.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 4,
                                                          ANI_LAGART == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 4,
                                                          ANI_LAGART == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 4,
                                                          ANI_LAGART == 9) %>%   
                                                   count()
  )

}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2009_TIPO_ACID_LAGARTA"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2009_TIPO_ACID_LAGARTA"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2009_TIPO_ACID_LAGARTA.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 4,
                                                          CON_SOROTE == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 4,
                                                          CON_SOROTE == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 4,
                                                          CON_SOROTE == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2009_SOROTERAPIA_LAGARTA"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2009_SOROTERAPIA_LAGARTA"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2009_SOROTERAPIA_LAGARTA.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 4,
                                                          COM_LOC == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 4,
                                                          COM_LOC == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 4,
                                                          COM_LOC == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2009_COMP_LOCAIS_LAGARTA"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2009_COMP_LOCAIS_LAGARTA"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2009_COMP_LOCAIS_LAGARTA.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 4,
                                                          COM_SISTEM == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 4,
                                                          COM_SISTEM == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 4,
                                                          COM_SISTEM == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2009_COMP_SIST_LAGARTA"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2009_COMP_SIST_LAGARTA"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2009_COMP_SIST_LAGARTA.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 4,
                                                          MCLI_LOCAL == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 4,
                                                          MCLI_LOCAL == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 4,
                                                          MCLI_LOCAL == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2009_MAN_LOCAIS_LAGARTA"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2009_MAN_LOCAIS_LAGARTA"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2009_MAN_LOCAIS_LAGARTA.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 4,
                                                          CLI_DOR == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 4,
                                                          CLI_EDEMA == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 4,
                                                          CLI_EQUIMO == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 4,
                                                          CLI_NECROS == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 4,
                                                          CLI_LOCAL_ == 1) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2009_MAN_LOCAIS_ESP_LAGARTA"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2009_MAN_LOCAIS_ESP_LAGARTA"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2009_MAN_LOCAIS_ESP_LAGARTA.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 4,
                                                          MCLI_SIST == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 4,
                                                          MCLI_SIST == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 4,
                                                          MCLI_SIST == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2009_MAN_SIST_LAGARTA"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2009_MAN_SIST_LAGARTA"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2009_MAN_SIST_LAGARTA.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 4,
                                                          CLI_NEURO == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 4,
                                                          CLI_HEMORR == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 4,
                                                          CLI_VAGAIS == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 4,
                                                          CLI_MIOLIT == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 4,
                                                          CLI_RENAL == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 4,
                                                          CLI_OUTR_2 == 1) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2009_MAN_SIST_ESP_LAGARTA"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2009_MAN_SIST_ESP_LAGARTA"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2009_MAN_SIST_ESP_LAGARTA.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 5,
                                                          TRA_CLASSI == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 5,
                                                          TRA_CLASSI == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 5,
                                                          TRA_CLASSI == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 5,
                                                          TRA_CLASSI == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2009_CLASSI_CASO_ABELHA"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2009_CLASSI_CASO_ABELHA"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2009_CLASSI_CASO_ABELHA.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 5,
                                                          CON_SOROTE == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 5,
                                                          CON_SOROTE == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 5,
                                                          CON_SOROTE == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2009_SOROTERAPIA_ABELHA"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2009_SOROTERAPIA_ABELHA"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2009_SOROTERAPIA_ABELHA.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 5,
                                                          COM_LOC == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 5,
                                                          COM_LOC == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 5,
                                                          COM_LOC == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2009_COMP_LOCAIS_ABELHA"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2009_COMP_LOCAIS_ABELHA"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2009_COMP_LOCAIS_ABELHA.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 5,
                                                          COM_SISTEM == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 5,
                                                          COM_SISTEM == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 5,
                                                          COM_SISTEM == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2009_COMP_SIST_ABELHA"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2009_COMP_SIST_ABELHA"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2009_COMP_SIST_ABELHA.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 5,
                                                          MCLI_LOCAL == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 5,
                                                          MCLI_LOCAL == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 5,
                                                          MCLI_LOCAL == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2009_MAN_LOCAIS_ABELHA"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2009_MAN_LOCAIS_ABELHA"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2009_MAN_LOCAIS_ABELHA.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 5,
                                                          CLI_DOR == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 5,
                                                          CLI_EDEMA == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 5,
                                                          CLI_EQUIMO == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 5,
                                                          CLI_NECROS == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 5,
                                                          CLI_LOCAL_ == 1) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2009_MAN_LOCAIS_ESP_ABELHA"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2009_MAN_LOCAIS_ESP_ABELHA"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2009_MAN_LOCAIS_ESP_ABELHA.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 5,
                                                          MCLI_SIST == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 5,
                                                          MCLI_SIST == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 5,
                                                          MCLI_SIST == 9) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2009_MAN_SIST_ABELHA"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2009_MAN_SIST_ABELHA"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2009_MAN_SIST_ABELHA.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TP_ACIDENT == 5,
                                                          CLI_NEURO == 1) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 5,
                                                          CLI_HEMORR == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 5,
                                                          CLI_VAGAIS == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 5,
                                                          CLI_MIOLIT == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 5,
                                                          CLI_RENAL == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_PECONHENTOS_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                            TP_ACIDENT == 5,
                                                          CLI_OUTR_2 == 1) %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:ncol(AUX)] <- apply(AUX[, 4:ncol(AUX)], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PECONHENTOS_2009_MAN_SIST_ESP_ABELHA"), AUX)

write.csv (assign(paste0("RS", RS, "_PECONHENTOS_2009_MAN_SIST_ESP_ABELHA"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Peconhentos/RS", RS, "_PECONHENTOS_2009_MAN_SIST_ESP_ABELHA.csv"), 
           row.names = FALSE)






