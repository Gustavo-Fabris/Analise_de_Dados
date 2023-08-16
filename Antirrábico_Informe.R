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

PR_ANTRAB_BASE_Serie_Historica <- read.csv(file = "Base_de_Dados/Tabulacoes_R/Raiva/PR_ANTRAB_Serie_Historica.csv",
                                      header = TRUE,
                                      sep = ",")

RS_ANTRAB_BASE_Serie_Historica <- read.csv(file = "Base_de_Dados/Tabulacoes_R/Raiva/RS_ANTRAB_BASE_Serie_Historica.csv",
                                           header = TRUE,
                                           sep = ",")

RS_CE_BASE_ANTRAB <- read.csv(file = "Base_de_Dados/Tabulacoes_R/Raiva/RS_CE_Base_ANTRAB.csv",
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

####   Estabelecendo o número de municípios em cada RS

nrow <- NROW(BASE_IBGE[which(BASE_IBGE$RS == RS), 1])

###################       2023      #########################################

####Criando um objeto com a base DBF do SINAN#################

ANTRAB2023 <- read.dbf(file = "Base_de_Dados/DBF/ANTRANET2023.DBF",
                       as.is = FALSE)


####Alterando o formato da coluna ID_MN_RESI de forma que ela seja passível de vincular no for loop#################

ANTRAB2023$ID_MN_RESI <- as.numeric(as.character(ANTRAB2023$ID_MN_RESI))

#####################################################################################################################
#################  Realizando contagem dos casos do Paraná para realizar a série histórica do Estado  ###############

PR_ANTRAB_BASE_Serie_Historica[12, 2] <- as.integer(ANTRAB2023 %>% 
                                                 count()
)

write.csv (PR_ANTRAB_BASE_Serie_Historica, 
           "Base_de_Dados/Tabulacoes_R/Raiva/PR_ANTRAB_Serie_Historica.csv", 
           row.names = FALSE)

#####Filtrando os dados da Base DBF do SINAN com os dados de notificação somente da 22RS###########

SINAN_ANTRAB_2023 <- ANTRAB2023 %>% 
  filter(ID_REGIONA == ID_REG | ID_RG_RESI == ID_REG)

assign(paste0("RS", RS, "_ANTRAB_2023_SINAN"), SINAN_ANTRAB_2023)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2023_SINAN"), SINAN_ANTRAB_2023), 
           paste0("Base_de_Dados/Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2023_SINAN.csv"), 
           row.names = FALSE)

################################################################################################################
############      Filtrando os dados por SE para elaborar o Canal Endêmico   ###################################

AUX <- matrix(data = NA, 
              nrow = nrow, 
              ncol = 54)

AUX <- as.data.frame(AUX)

colnames(AUX)[1] <- "Município" 

AUX[,1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

colnames (AUX)[2:54] <- c(1:53)

for (i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX == i), 2] <- as.integer(SINAN_ANTRAB_2023 %>%
                                          filter(ID_MN_RESI == i,
                                                 SEM_PRI ==202301)%>%
                                          count()
  )
  
  AUX[which(AUX == i), 3] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                          filter(ID_MN_RESI == i, 
                                                 SEM_PRI ==202302) %>% 
                                          count()
  )
  
  AUX[which(AUX == i), 4] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                          filter(ID_MN_RESI == i,
                                                 SEM_PRI ==202303) %>% 
                                          count()
  )
  
  AUX[which(AUX == i),5] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                         filter(ID_MN_RESI == i,
                                                SEM_PRI ==202304) %>% 
                                         count()
  )
  
  AUX[which(AUX == i), 6] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                          filter(ID_MN_RESI == i,
                                                 SEM_PRI ==202305) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 7] <- as.integer(SINAN_ANTRAB_2023 %>%
                                          filter(ID_MN_RESI == i, 
                                                 SEM_PRI ==202306) %>%
                                          count()
  )
  
  AUX[which(AUX == i), 8] <- as.integer(SINAN_ANTRAB_2023 %>%
                                          filter(ID_MN_RESI == i, 
                                                 SEM_PRI ==202307) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 9] <- as.integer(SINAN_ANTRAB_2023 %>%
                                          filter(ID_MN_RESI == i, 
                                                 SEM_PRI ==202308) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 10] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202309) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 11] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202310) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 12] <- as.integer(SINAN_ANTRAB_2023 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202311) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 13] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202312) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 14] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202313) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 15] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202314) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 16] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202315) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 17] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202316) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 18] <- as.integer(SINAN_ANTRAB_2023 %>%
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202317) %>%  
                                           count() 
  )
  
  AUX[which(AUX == i), 19] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202318) %>%      
                                           count() 
  )
  
  AUX[which(AUX == i), 20] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202319) %>%
                                           count() 
  )
  
  AUX[which(AUX == i),  21] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                            filter(ID_MN_RESI == i, 
                                                   SEM_PRI ==202320) %>%
                                            count() 
  )
  
  AUX[which(AUX == i), 22] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202321) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 23] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202322) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 24] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202323) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 25] <- as.integer(SINAN_ANTRAB_2023 %>%
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202324) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 26] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202325) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 27] <- as.integer(SINAN_ANTRAB_2023 %>%
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202326) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 28] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202327) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 29] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202328) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 30] <- as.integer(SINAN_ANTRAB_2023 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202329) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 31] <- as.integer(SINAN_ANTRAB_2023 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202330) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 32] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202331) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 33] <- as.integer(SINAN_ANTRAB_2023 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202332) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 34] <- as.integer(SINAN_ANTRAB_2023 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202333) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 35] <- as.integer(SINAN_ANTRAB_2023 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202334) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 36] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202335) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 37] <- as.integer(SINAN_ANTRAB_2023 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202336) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 38] <- as.integer(SINAN_ANTRAB_2023 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202337) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 39] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202338) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 40] <- as.integer(SINAN_ANTRAB_2023 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202339) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 41] <- as.integer(SINAN_ANTRAB_2023 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202340) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 42] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202341) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 43] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202342) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 44] <- as.integer(SINAN_ANTRAB_2023 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202343) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 45] <- as.integer(SINAN_ANTRAB_2023 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202344) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 46] <- as.integer(SINAN_ANTRAB_2023 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202345) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 47] <- as.integer(SINAN_ANTRAB_2023 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202346) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 48] <- as.integer(SINAN_ANTRAB_2023 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202347) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 49] <- as.integer(SINAN_ANTRAB_2023 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202348) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 50] <- as.integer(SINAN_ANTRAB_2023 %>%
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202349) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 51] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202350) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 52] <- as.integer(SINAN_ANTRAB_2023 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202351) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 53] <- as.integer(SINAN_ANTRAB_2023 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202352) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 54] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202353) %>%
                                           count() 
  )
}

AUX[, 1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX[(nrow(AUX)+ 1), 2:54] <- apply(AUX[, 2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2023_SE_Notificados"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2023_SE_Notificados"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2023_SE_Notificados.csv"), 
           row.names = FALSE)

#####FIltrando os dados por município e construindo uma tabela geral dos dados de atendimentos antirrábicos#####

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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                   filter(ID_MN_RESI == i) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CS_ZONA == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CS_ZONA == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CS_ZONA == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CS_ZONA == 9 
                                                          |
                                                            is.na(CS_ZONA)) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_SEXO == "F") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_SEXO == "M") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                    filter(ID_MN_RESI == i,  
                                                           NU_IDADE_N <=3012) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                    filter(ID_MN_RESI == i,  
                                                           NU_IDADE_N > 4000 
                                                           & 
                                                             NU_IDADE_N <=4005) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           NU_IDADE_N > 4005 
                                                           & 
                                                             NU_IDADE_N <=4012) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           NU_IDADE_N > 4012 
                                                           & 
                                                             NU_IDADE_N <=4018) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 16] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           NU_IDADE_N > 4018 
                                                           & 
                                                             NU_IDADE_N <=4059) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 17] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           NU_IDADE_N > 4059) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 18] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "00") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 19] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "01" 
                                                           | 
                                                             CS_ESCOL_N == "02" 
                                                           | 
                                                             CS_ESCOL_N == "03") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 20] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "04") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 21] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "05") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 22] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "06") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 23] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "07") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 24] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "08") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 25] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "10") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 26] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "09") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 27] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_GESTANT == "1" 
                                                           |
                                                             CS_GESTANT== "2" 
                                                           | 
                                                             CS_GESTANT == "3" 
                                                           | 
                                                             CS_GESTANT == "4") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 28] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_GESTANT == "5") %>% 
                                                    count()
  )
  AUX[which(AUX$COD_IBGE == i), 29] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_GESTANT == "6") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 30] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_GESTANT == "9") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 31] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "01") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 32] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "2") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 33] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "3") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 34] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "4") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 35] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "5") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 36] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "9") %>% 
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:36] <- apply(AUX[, 4:36], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2023_GERAL"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2023_GERAL"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2023_GERAL.csv"), 
           row.names = FALSE)


####Criando uma tabela dos dados de exposição compo 32 da ficha do sinan############

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Contato_Indireto <- NA

AUX$Arranhadura <- NA

AUX$Lambedura <- NA

AUX$Mordedura <- NA

AUX$Outro <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_CONTAT == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_ARRANH == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_LAMBED == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_MORDED == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_OUTRO_ == "1") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:9] <- apply(AUX[, 4:9], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2023_EXPOSICAO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2023_EXPOSICAO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2023_EXPOSICAO.csv"), 
           row.names = FALSE)

#####Criando uma tabela com dados de localização da lesão (campo 33 do SINAN)##########

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Mucosa <- NA

AUX$Cabeca_Pescoco <- NA

AUX$Maos_Pes <- NA

AUX$Tronco <- NA

AUX$Membros_Superiores <- NA

AUX$Membros_Inferiores <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_MUCOSA == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_CABECA == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_MAOS == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_TRONCO == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_MEMBRO== "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           ANT_MEMB_1== "1") %>%   
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:10] <- apply(AUX[, 4:10], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2023_LOCALIZACAO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2023_LOCALIZACAO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2023_LOCALIZACAO.csv"), 
           row.names = FALSE)

#####Tabela Ferimento (Campo 34 do SINAN)

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Unico <- NA

AUX$Multiplo <- NA

AUX$Sem_Ferimento <- NA

AUX$Ignorado <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FERIMENTO == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FERIMENTO == "2") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FERIMENTO == "3") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FERIMENTO == "9") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:8] <- apply(AUX[, 4:8], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2023_FERIMENTO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2023_FERIMENTO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2023_FERIMENTO.csv"), 
           row.names = FALSE)

#####Tabela Tipo de Ferimanto (Campo 35 do SINAN)########

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Profundo <- NA

AUX$Superficial <- NA

AUX$Dilacerante <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_PROFUN == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_SUPERF == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_DILACE == "1") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:7] <- apply(AUX[, 4:7], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2023_TIPO_FERIMENTO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2023_TIPO_FERIMENTO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2023_TIPO_FERIMENTO.csv"), 
           row.names = FALSE)

#####Tabela Antecedentes de Tratamento (Campo 37 sinan)####

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[, c(4, 1, 2, 3)]

AUX$Pre_Exp <- NA

AUX$Pos_Exp <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          PRE_EXPOS == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          POS_EXPOS == "1") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:6] <- apply(AUX[, 4:6], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2023_ANTECEDENTE_ANTR"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2023_ANTECEDENTE_ANTR"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2023_ANTECEDENTE_ANTR.csv"), 
           row.names = FALSE)

####TAbela Animal Agressor (campo 40 SINAN)####

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[, c(4, 1, 2, 3)]

AUX$Canina <- NA

AUX$Felina <- NA

AUX$Quiroptera <- NA

AUX$Primata <- NA

AUX$Raposa <- NA

AUX$Herbivaro_Domestico <- NA

AUX$Outra <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANIMAL == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANIMAL == "2") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANIMAL == "3") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANIMAL == "4") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANIMAL == "5") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           ANIMAL == "6") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           ANIMAL == "7") %>%   
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:11] <- apply(AUX[, 4:11], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2023_AGRESSOR"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2023_AGRESSOR"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2023_AGRESSOR.csv"), 
           row.names = FALSE)

####Tabela Condição do animal no acidente (campo 41 do sinan)####

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[, c(4, 1, 2, 3)]

AUX$Sadio <- NA

AUX$Suspeito <- NA

AUX$Raivoso <- NA

AUX$Morto_Desaparecido <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CONDIC_ANI == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CONDIC_ANI == "2") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CONDIC_ANI == "3") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CONDIC_ANI == "4") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:8] <- apply(AUX[, 4:8], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2023_COND_ANIMAL_ACID"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2023_COND_ANIMAL_ACID"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2023_COND_ANIMAL_ACID.csv"), 
           row.names = FALSE)

####Tabela Animal Observável (campo 42 SINAN)###

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[, c(4, 1, 2, 3)]

AUX$Sim <- NA

AUX$Nao <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          OBSERVACAO == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          OBSERVACAO == "2") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:6] <- apply(AUX[, 4:6], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2023_OBSERVAVEL"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2023_OBSERVAVEL"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2023_OBSERVAVEL.csv"), 
           row.names = FALSE)

###Tabela Tratamento (campo 43 do SINAN)####

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Dispensa_Tratamento <- NA

AUX$Observação_Animal <- NA

AUX$Observação_Vacina <- NA

AUX$Vacina <- NA

AUX$Soro_Vacina <- NA

AUX$Esquema_Reexposicao <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRAT_ATUAL == "2") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRAT_ATUAL == "3") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRAT_ATUAL == "4") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRAT_ATUAL == "5") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRAT_ATUAL == "6") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           TRAT_ATUAL == "7") %>%   
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:10] <- apply(AUX[, 4:10], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2023_TRATAMENTO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2023_TRATAMENTO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2023_TRATAMENTO.csv"), 
           row.names = FALSE)

###Tabela Condição final do Animal (campo 48 do SINAN)###

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX<- AUX[,c(4, 1, 2, 3)]

AUX$Negativo_Clinico <- NA

AUX$Negativo_Lab <- NA

AUX$Positivo_Clinico <- NA

AUX$Positivo_Lab <- NA

AUX$Morto_SemDiagnostico <- NA

AUX$Ignorado <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FIM_ANIMAL == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FIM_ANIMAL == "2") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FIM_ANIMAL == "3") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FIM_ANIMAL == "4") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FIM_ANIMAL == "5") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           FIM_ANIMAL == "9") %>%   
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:10] <- apply(AUX[, 4:10], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2023_COND_FINAL_ANIMAL"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2023_COND_FINAL_ANIMAL"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2023_COND_FINAL_ANIMAL.csv"), 
           row.names = FALSE)

###Tabela Interrupção de tratamento (campo 49 do SINAN)####

SINAN_ANTRAB_2023_INTERRUPCAO_TRAT <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

SINAN_ANTRAB_2023_INTERRUPCAO_TRAT$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

SINAN_ANTRAB_2023_INTERRUPCAO_TRAT$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

SINAN_ANTRAB_2023_INTERRUPCAO_TRAT$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

SINAN_ANTRAB_2023_INTERRUPCAO_TRAT <- SINAN_ANTRAB_2023_INTERRUPCAO_TRAT[,c(4, 1, 2, 3)]

SINAN_ANTRAB_2023_INTERRUPCAO_TRAT$Sim <- NA

SINAN_ANTRAB_2023_INTERRUPCAO_TRAT$Nao <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  SINAN_ANTRAB_2023_INTERRUPCAO_TRAT[which(SINAN_ANTRAB_2023_INTERRUPCAO_TRAT$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                                                                                 filter(ID_MN_RESI == i,
                                                                                                                        TRA_INTERR == "1") %>%   
                                                                                                                 count()
  )    
  
  SINAN_ANTRAB_2023_INTERRUPCAO_TRAT[which(SINAN_ANTRAB_2023_INTERRUPCAO_TRAT$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                                                                                 filter(ID_MN_RESI == i,
                                                                                                                        TRA_INTERR == "2") %>%   
                                                                                                                 count()
  )
}

AUX[(nrow(AUX) +1), 4:6] <- apply(AUX[, 4:6], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2023_INTERRUPCAO_TRAT"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2023_INTERRUPCAO_TRAT"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2023_INTERRUPCAO_TRAT.csv"), 
           row.names = FALSE)

####Tabela Soroterapia (campo 53 do SINAN)###

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Sim <- NA

AUX$Nao <- NA

AUX$Ignorado <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRA_INDI_N == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRA_INDI_N == "2") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRA_INDI_N == "9") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:7] <- apply(AUX[, 4:7], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2023_SOROTERAPIA"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2023_SOROTERAPIA"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2023_SOROTERAPIA.csv"), 
           row.names = FALSE)

###Tabela tipo de imunobiologico (campo 55 SINAN)####

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX<- AUX[,c(4, 1, 2, 3)]

AUX$SAR <- NA

AUX$IGHAR <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TIP_SORO == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TIP_SORO== "2") %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:6] <- apply(AUX[, 4:6], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2023_TIPO_IMUNOBIOLOGICO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2023_TIPO_IMUNOBIOLOGICO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2023_TIPO_IMUNOBIOLOGICO.csv"), 
           row.names = FALSE)

###Tabela Infiltração (campo 56 do SINAN)###

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX  <- AUX [,c(4, 1, 2, 3)]

AUX$TOTAL <- NA

AUX$PARCIAL <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRA_INFILT == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2023 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRA_INFI_1 == "1") %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:6] <- apply(AUX[, 4:6], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2023_SORO_INFILTRACAO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2023_SORO_INFILTRACAO"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2023_SORO_INFILTRACAO.csv"), 
           row.names = FALSE)

rm(SINAN_ANTRAB_2023, ANTRAB2023)

######   Série Histórica   ########

RS_ANTRAB_BASE_Serie_Historica[nrow(RS_ANTRAB_BASE_Serie_Historica), 1] <- RS22_ANTRAB_2023_GERAL[17, 5]
RS_ANTRAB_BASE_Serie_Historica[nrow(RS_ANTRAB_BASE_Serie_Historica), 2] <- RS22_ANTRAB_2023_GERAL[17, 6]
RS_ANTRAB_BASE_Serie_Historica[nrow(RS_ANTRAB_BASE_Serie_Historica), 3] <- RS22_ANTRAB_2023_GERAL[17, 7]
RS_ANTRAB_BASE_Serie_Historica[nrow(RS_ANTRAB_BASE_Serie_Historica), 4] <- RS22_ANTRAB_2023_GERAL[17, 8]
RS_ANTRAB_BASE_Serie_Historica[nrow(RS_ANTRAB_BASE_Serie_Historica), 5] <- RS22_ANTRAB_2023_GERAL[17, 9]
RS_ANTRAB_BASE_Serie_Historica[nrow(RS_ANTRAB_BASE_Serie_Historica), 6] <- RS22_ANTRAB_2023_GERAL[17, 10]
RS_ANTRAB_BASE_Serie_Historica[nrow(RS_ANTRAB_BASE_Serie_Historica), 7] <- RS22_ANTRAB_2023_GERAL[17, 11]
RS_ANTRAB_BASE_Serie_Historica[nrow(RS_ANTRAB_BASE_Serie_Historica), 8] <- RS22_ANTRAB_2023_GERAL[17, 12]
RS_ANTRAB_BASE_Serie_Historica[nrow(RS_ANTRAB_BASE_Serie_Historica), 9] <- RS22_ANTRAB_2023_GERAL[17, 13]
RS_ANTRAB_BASE_Serie_Historica[nrow(RS_ANTRAB_BASE_Serie_Historica), 10] <- RS22_ANTRAB_2023_GERAL[17, 14]
RS_ANTRAB_BASE_Serie_Historica[nrow(RS_ANTRAB_BASE_Serie_Historica), 11] <- RS22_ANTRAB_2023_GERAL[17, 15]
RS_ANTRAB_BASE_Serie_Historica[nrow(RS_ANTRAB_BASE_Serie_Historica), 12] <- RS22_ANTRAB_2023_GERAL[17, 16]
RS_ANTRAB_BASE_Serie_Historica[nrow(RS_ANTRAB_BASE_Serie_Historica), 13] <- RS22_ANTRAB_2023_GERAL[17, 17]
RS_ANTRAB_BASE_Serie_Historica[nrow(RS_ANTRAB_BASE_Serie_Historica), 14] <- RS22_ANTRAB_2023_GERAL[17, 18]
RS_ANTRAB_BASE_Serie_Historica[nrow(RS_ANTRAB_BASE_Serie_Historica), 15] <- RS22_ANTRAB_2023_GERAL[17, 19]
RS_ANTRAB_BASE_Serie_Historica[nrow(RS_ANTRAB_BASE_Serie_Historica), 16] <- RS22_ANTRAB_2023_GERAL[17, 20]
RS_ANTRAB_BASE_Serie_Historica[nrow(RS_ANTRAB_BASE_Serie_Historica), 17] <- RS22_ANTRAB_2023_GERAL[17, 21]
RS_ANTRAB_BASE_Serie_Historica[nrow(RS_ANTRAB_BASE_Serie_Historica), 18] <- RS22_ANTRAB_2023_GERAL[17, 22]
RS_ANTRAB_BASE_Serie_Historica[nrow(RS_ANTRAB_BASE_Serie_Historica), 19] <- RS22_ANTRAB_2023_GERAL[17, 23]
RS_ANTRAB_BASE_Serie_Historica[nrow(RS_ANTRAB_BASE_Serie_Historica), 20] <- RS22_ANTRAB_2023_GERAL[17, 24]
RS_ANTRAB_BASE_Serie_Historica[nrow(RS_ANTRAB_BASE_Serie_Historica), 21] <- RS22_ANTRAB_2023_GERAL[17, 25]
RS_ANTRAB_BASE_Serie_Historica[nrow(RS_ANTRAB_BASE_Serie_Historica), 22] <- RS22_ANTRAB_2023_GERAL[17, 26]
RS_ANTRAB_BASE_Serie_Historica[nrow(RS_ANTRAB_BASE_Serie_Historica), 23] <- RS22_ANTRAB_2023_GERAL[17, 27]
RS_ANTRAB_BASE_Serie_Historica[nrow(RS_ANTRAB_BASE_Serie_Historica), 24] <- RS22_ANTRAB_2023_GERAL[17, 28]

write.csv(assign(paste0("RS", RS, "_ANTRAB_Serie_Historica"), RS_ANTRAB_BASE_Serie_Historica),
          paste0("Base_de_Dados/Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_Serie_Historica.csv"),
          row.names = FALSE)

####################################################################################################################
################Trabalhando as tabelas base do Canal Endêmico   ####################################################
####################################################################################################################

######     Canal Endêmico    NOTIFICADOS#####

RS_CE_BASE_ANTRAB[(nrow(RS_CE_BASE_ANTRAB) +1), 1] <- "2023"
RS_CE_BASE_ANTRAB[nrow(RS_CE_BASE_ANTRAB), 2:54] <- as.integer(data.frame(RS22_ANTRAB_2023_SE_Notificados[nrow(RS22_ANTRAB_2023_SE_Notificados), 2:54]))

#####################################################################################################################
#####                   Utilizando objetos auxiliares porque se transpor o data frame                   #############
#####                   direto ele transforma as variáveis em caracter.                                 #############
#####                                                                                                   #############         
#####                     NÃO FOI DESCARTADO AINDA OS PERÍODOS EPIDÊMICOS                               #############
#####               VERIFICAR SE PODE-SE UTILIZAR A MÉDIA COMO LIMITE INFERIOR.                         #############
#####################################################################################################################

AUX <- RS_CE_BASE_ANTRAB[, -1]

AUX <- t(AUX)

AUX2 <- RS_CE_BASE_ANTRAB[, 1]

colnames(AUX) <- AUX2

RS_CE_ANTRAB <- AUX

######        Criando a coluna de média no data.frame            #####################

AUX <- apply(RS_CE_ANTRAB[, 1: (ncol(RS_CE_ANTRAB)-1)], 1 , mean)

RS_CE_ANTRAB <- as.data.frame(RS_CE_ANTRAB)

RS_CE_ANTRAB$Media <- AUX

######              Criando a coluna de Desvio Padrão no data frame                ###############

AUX <- apply(RS_CE_ANTRAB[, 1: (ncol(RS_CE_ANTRAB) -2)], 1 , sd)

RS_CE_ANTRAB$Desvio_Padrao <- AUX

######       Criando a coluna de Média + 2(DP)    ######################

AUX <- RS_CE_ANTRAB[, (ncol(RS_CE_ANTRAB)-1):ncol(RS_CE_ANTRAB)]

AUX <- AUX %>% mutate(Lim_Superior = (Media + 1.96 * Desvio_Padrao))

RS_CE_ANTRAB$Lim_Superior <- AUX$Lim_Superior

RS_CE_ANTRAB[, (ncol(RS_CE_ANTRAB)+1)] <- rownames(RS_CE_ANTRAB)

RS_CE_ANTRAB <- RS_CE_ANTRAB[, c(ncol(RS_CE_ANTRAB), 1:(ncol(RS_CE_ANTRAB) -1))]

RS_CE_ANTRAB[, 1] <- c(1:53)

colnames(RS_CE_ANTRAB)[1] <- "Semana_Epidemiológica"

rownames(RS_CE_ANTRAB) <- c(1: nrow(RS_CE_ANTRAB))

rm(AUX, AUX2, RS_CE_Base_ANTRAB)

write.csv (RS_CE_ANTRAB, 
           paste0("Base_de_Dados/Tabulacoes_R/Raiva/RS", RS, "_CE_ANTRAB.csv"), 
           row.names = FALSE)

###    CANAL ENDÊMICO NOTIFICADOS     ####

###    Puxando os dados da tabela RS22_CE_Notificados e excluindo os períodos epidêmicos:   ######
###                             2015/16, 2019/20 e 2021/22                                  ######

AUX_GRAF <- RS_CE_ANTRAB[,]

###Criando uma coluna de ordem das se para o R não colocar em ordem numérica.

AUX_GRAF$Sem_EPI <-as.character(c("2023/01",  "2023/02", "2023/03", 
                                  "2023/04",  "2023/05",  "2023/06",  
                                  "2023/07",  "2023/08",  "2023/09",  
                                  "2023/10",  "2023/11",  "2023/12",  
                                  "2023/13",  "2023/14",  "2023/15",  
                                  "2023/16",  "2023/17",  "2023/18",  
                                  "2023/19",  "2023/20",  "2023/21",  
                                  "2023/22",  "2023/23",  "2023/24",  
                                  "2023/25",  "2023/26",  "2023/27",  
                                  "2023/28",  "2023/29",  "2023/30",  
                                  "2023/31",  "2023/32",  "2023/33", 
                                  "2023/34",  "2023/35",  "2023/36",  
                                  "2023/37",  "2023/38",  "2023/39",  
                                  "2023/40",  "2023/41",  "2023/42",  
                                  "2023/43",  "2023/44",  "2023/45",  
                                  "2023/46",  "2023/47",  "2023/48",  
                                  "2023/49",  "2023/50",  "2023/51",  
                                  "2023/52",  "2023/53")
)

RS_22_23_GRAF_CE_Notificados <- ggplot(AUX_GRAF, aes(Semana_Epidemiológica))  +
  theme(axis.text.x = element_text(angle = 85, 
                                   vjust = .5,
                                   face = "bold")) +
  labs(caption = "Fonte",
       title = "Canal Endêmico Casos Notificados - 2022/23") +
  theme(
    panel.grid.major = element_line(color = "#C0C0C0"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#DC143C"),
    plot.title = element_text(face = "bold",
                              size = 19)
  ) +
  geom_area(aes(,Lim_Superior), fill = "#F0E68C",alpha = 0.9) +
  geom_area(aes(,Media), fill = "#556B2F") +
  geom_line(aes(,`2023`), stat = "identity", color = "black", linewidth = 1.5) +
  xlab("Semana Epidemiológica") +
  ylab("Número de Casos") +
  scale_x_continuous(breaks = c(1:53), label = AUX_GRAF$Sem_EPI) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))



