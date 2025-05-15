rm(list =ls())
###############################################################################################################
###############################################################################################################
##                                                                                                           ##
##    Script elaborado em ambiente LINUX. Para replicação deve ser ajustado os caminhos dos diretórios       ##
##    ################################################################################################       ##
##                                                                                                           ##
##    Até o ponto em que é necessario autenticação de acesso à internet todos os passos abaixo são           ##
##    reprodutíveis em qualquer computador desde que os itens abaixo sejam executados corretamente.          ##
##    #############################################################################################          ## 
##                                                                                                           ##
##    /home/gustavo/Área de trabalho/Análise_de_Dados/Arboviroses_Geral deve ser previamente rodado          ##
##    para que o script abaixo funcione.                                                                     ##
##    ############################################################################################           ##
##                                                                                                           ##
###############################################################################################################
###############################################################################################################
############################################################################################
####   Definindo o objeto RS para servir de apoio para    ##################################
####    buscar dados de todas as RS. Usar 1, 2, 3..., 21, 22    ############################
############################################################################################

RS <- 22   #####  Deve-se colocar AQUI a Regional
###########################         Localizações   #########################

###   Linhas          Assunto                         Linhas              Assunto
###   83 - 97         libraries                 ### 2459 - 2599         Canais Endêmicos
###   104 - 154       Bases de dados            ### 
###   156 - 2170      Tabelas período atual
###   2184 - 2444     Decodificação SINAN
###   2455 - 2504     Série histórica

#####      Definindo diretório de trabalho, caso tenha que trabalhar em Windows, acertar o diretório       ####

setwd("/home/gustavo/Área de Trabalho/Análise_de_Dados/")

###############################################################################################################
###############################################################################################################
##                                                                                                           ##
##               OS PASSOS ABAIXO (ATÉ LINHA 80) DEVEM SER SEGUIDOS PARA FUNCIONAMENTO!!!                    ##
##    ################################################################################################       ##
##                                                                                                           ##
##            Lembrar que OBRIGATORIAMENTE deve ser baixados as bases DBF de 2009 até 2024                   ##
##            As bases DBF devem ser salvas no formato DENGON2009, DENGON2012... até DENGON2024              ##
##            A base DENGON2024 deve ser baixada diariamente e salva no local correto para que               ## 
##            o sistema esteja sempre atualizado!!!                                                          ##
##            Os dados do LACEN devem ser baixados da GAL                                                    ##
##            Estes arquivos devem ser alocados no diretório abaixo:                                         ##
##            /home/gustavo/Área de trabalho/Análise_de_Dados/Base_de_Dados/DBF ou /Base_de_Dados/LACEN      ##
##    ############################################################################################           ##
##                                                                                                           ##
###############################################################################################################
###############################################################################################################
library(patchwork)
library(foreign)
library (dplyr)
library (googlesheets4)
library (ggplot2)
###Não sei usar o httpuv!!!###
library (httpuv)
library(stringr)
library(lubridate)
#library(xlsx)
library(geobr)
library(ggspatial)
library(ggplot2)
library(tidyr)
###################       2025      #########################################

BASE_IBGE<-read.csv(file="Base_de_Dados/Auxiliares/Planilha_Base_IBGE.csv", 
                    header=TRUE, 
                    sep=",")

PR_ANTRAB_Serie_Historica <- read.csv(file = "Tabulacoes_R/Raiva/PR_ANTRAB_BASE_Serie_Historica.csv",
                                      header = TRUE,
                                      sep = ",")

RS_ANTRAB_Serie_Historica <- read.csv(file = "Tabulacoes_R/Raiva/RS_ANTRAB_BASE_Serie_Historica.csv",
                                      header = TRUE,
                                      sep = ",")

RS_CE_BASE_ANTRAB <- read.csv(file = "Tabulacoes_R/Raiva/RS_CE_Base_ANTRAB.csv",
                              header = TRUE,
                              sep = ",")

######   Criando objeto ID_REG. Será utilizado para selecionar
######   RS no DBF do SINAN ONLINE.

ID_REG <- as.data.frame(BASE_IBGE[which(BASE_IBGE$RS == RS), 6])

ID_REG <- as.numeric(ID_REG[1,1])

####   Estabelecendo o número de municípios em cada RS

nrow <- NROW(BASE_IBGE[which(BASE_IBGE$RS == RS), 1])


####Criando um objeto com a base DBF do SINAN#################

ANTRAB2025 <- read.dbf(file = "Base_de_Dados/DBF/ANTRANET2025.DBF",
                       as.is = FALSE)

####Alterando o formato da coluna ID_MN_RESI de forma que ela seja passível de vincular no for loop#################

ANTRAB2025$ID_MN_RESI <- as.numeric(as.character(ANTRAB2025$ID_MN_RESI))

#####################################################################################################################
#################  Realizando contagem dos casos do Paraná para realizar a série histórica do Estado  ###############

PR_ANTRAB_Serie_Historica[nrow(PR_ANTRAB_Serie_Historica), 2] <- as.integer(ANTRAB2025 %>% 
                                                 count()
)
#####Filtrando os dados da Base DBF do SINAN com os dados de notificação somente da 22RS###########

SINAN_ANTRAB_2025 <- ANTRAB2025 %>% 
  filter(ID_REGIONA == ID_REG | ID_RG_RESI == ID_REG)

assign(paste0("RS", RS, "_ANTRAB_2025_SINAN"), SINAN_ANTRAB_2025)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2025_SINAN"), SINAN_ANTRAB_2025), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2025_SINAN.csv"), 
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
  
  AUX[which(AUX == i), 2] <- as.integer(SINAN_ANTRAB_2025 %>%
                                          filter(ID_MN_RESI == i,
                                                 SEM_PRI ==202501)%>%
                                          count()
  )
  
  AUX[which(AUX == i), 3] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                          filter(ID_MN_RESI == i, 
                                                 SEM_PRI ==202502) %>% 
                                          count()
  )
  
  AUX[which(AUX == i), 4] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                          filter(ID_MN_RESI == i,
                                                 SEM_PRI ==202503) %>% 
                                          count()
  )
  
  AUX[which(AUX == i),5] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                         filter(ID_MN_RESI == i,
                                                SEM_PRI ==202504) %>% 
                                         count()
  )
  
  AUX[which(AUX == i), 6] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                          filter(ID_MN_RESI == i,
                                                 SEM_PRI ==202505) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 7] <- as.integer(SINAN_ANTRAB_2025 %>%
                                          filter(ID_MN_RESI == i, 
                                                 SEM_PRI ==202506) %>%
                                          count()
  )
  
  AUX[which(AUX == i), 8] <- as.integer(SINAN_ANTRAB_2025 %>%
                                          filter(ID_MN_RESI == i, 
                                                 SEM_PRI ==202507) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 9] <- as.integer(SINAN_ANTRAB_2025 %>%
                                          filter(ID_MN_RESI == i, 
                                                 SEM_PRI ==202508) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 10] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202509) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 11] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202510) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 12] <- as.integer(SINAN_ANTRAB_2025 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202511) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 13] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202512) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 14] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202513) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 15] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202514) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 16] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202515) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 17] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202516) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 18] <- as.integer(SINAN_ANTRAB_2025 %>%
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202517) %>%  
                                           count() 
  )
  
  AUX[which(AUX == i), 19] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202518) %>%      
                                           count() 
  )
  
  AUX[which(AUX == i), 20] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202519) %>%
                                           count() 
  )
  
  AUX[which(AUX == i),  21] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                            filter(ID_MN_RESI == i, 
                                                   SEM_PRI ==202520) %>%
                                            count() 
  )
  
  AUX[which(AUX == i), 22] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202521) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 23] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202522) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 24] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202523) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 25] <- as.integer(SINAN_ANTRAB_2025 %>%
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202524) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 26] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202525) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 27] <- as.integer(SINAN_ANTRAB_2025 %>%
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202526) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 28] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202527) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 29] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202528) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 30] <- as.integer(SINAN_ANTRAB_2025 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202529) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 31] <- as.integer(SINAN_ANTRAB_2025 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202530) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 32] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202531) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 33] <- as.integer(SINAN_ANTRAB_2025 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202532) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 34] <- as.integer(SINAN_ANTRAB_2025 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202533) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 35] <- as.integer(SINAN_ANTRAB_2025 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202534) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 36] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202535) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 37] <- as.integer(SINAN_ANTRAB_2025 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202536) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 38] <- as.integer(SINAN_ANTRAB_2025 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202537) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 39] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202538) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 40] <- as.integer(SINAN_ANTRAB_2025 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202539) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 41] <- as.integer(SINAN_ANTRAB_2025 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202540) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 42] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202541) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 43] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202542) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 44] <- as.integer(SINAN_ANTRAB_2025 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202543) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 45] <- as.integer(SINAN_ANTRAB_2025 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202544) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 46] <- as.integer(SINAN_ANTRAB_2025 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202545) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 47] <- as.integer(SINAN_ANTRAB_2025 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202546) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 48] <- as.integer(SINAN_ANTRAB_2025 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202547) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 49] <- as.integer(SINAN_ANTRAB_2025 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202548) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 50] <- as.integer(SINAN_ANTRAB_2025 %>%
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202549) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 51] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202550) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 52] <- as.integer(SINAN_ANTRAB_2025 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202551) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 53] <- as.integer(SINAN_ANTRAB_2025 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202552) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 54] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202553) %>%
                                           count() 
  )
}

AUX[, 1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX[(nrow(AUX)+ 1), 2:54] <- apply(AUX[, 2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2025_SE_Notificados"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2025_SE_Notificados"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2025_SE_Notificados.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                   filter(ID_MN_RESI == i) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CS_ZONA == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CS_ZONA == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CS_ZONA == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CS_ZONA == 9 
                                                          |
                                                            is.na(CS_ZONA)) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_SEXO == "F") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_SEXO == "M") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                    filter(ID_MN_RESI == i,  
                                                           NU_IDADE_N <=3012) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                    filter(ID_MN_RESI == i,  
                                                           NU_IDADE_N > 4000 
                                                           & 
                                                             NU_IDADE_N <=4005) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           NU_IDADE_N > 4005 
                                                           & 
                                                             NU_IDADE_N <=4012) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           NU_IDADE_N > 4012 
                                                           & 
                                                             NU_IDADE_N <=4018) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 16] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           NU_IDADE_N > 4018 
                                                           & 
                                                             NU_IDADE_N <=4059) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 17] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           NU_IDADE_N > 4059) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 18] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "00") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 19] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "01" 
                                                           | 
                                                             CS_ESCOL_N == "02" 
                                                           | 
                                                             CS_ESCOL_N == "03") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 20] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "04") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 21] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "05") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 22] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "06") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 23] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "07") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 24] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "08") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 25] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "10") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 26] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "09") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 27] <- as.integer(SINAN_ANTRAB_2025 %>% 
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
  
  AUX[which(AUX$COD_IBGE == i), 28] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_GESTANT == "5") %>% 
                                                    count()
  )
  AUX[which(AUX$COD_IBGE == i), 29] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_GESTANT == "6") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 30] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_GESTANT == "9") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 31] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "01") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 32] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "2") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 33] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "3") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 34] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "4") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 35] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "5") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 36] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "9") %>% 
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:36] <- apply(AUX[, 4:36], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2025_GERAL"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2025_GERAL"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2025_GERAL.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_CONTAT == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_ARRANH == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_LAMBED == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_MORDED == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_OUTRO_ == "1") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:9] <- apply(AUX[, 4:9], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2025_EXPOSICAO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2025_EXPOSICAO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2025_EXPOSICAO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_MUCOSA == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_CABECA == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_MAOS == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_TRONCO == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_MEMBRO== "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           ANT_MEMB_1== "1") %>%   
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:10] <- apply(AUX[, 4:10], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2025_LOCALIZACAO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2025_LOCALIZACAO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2025_LOCALIZACAO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FERIMENTO == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FERIMENTO == "2") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FERIMENTO == "3") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FERIMENTO == "9") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:8] <- apply(AUX[, 4:8], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2025_FERIMENTO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2025_FERIMENTO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2025_FERIMENTO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_PROFUN == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_SUPERF == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_DILACE == "1") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:7] <- apply(AUX[, 4:7], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2025_TIPO_FERIMENTO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2025_TIPO_FERIMENTO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2025_TIPO_FERIMENTO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          PRE_EXPOS == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          POS_EXPOS == "1") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:6] <- apply(AUX[, 4:6], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2025_ANTECEDENTE_ANTR"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2025_ANTECEDENTE_ANTR"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2025_ANTECEDENTE_ANTR.csv"), 
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

AUX$Herbivoro_Domestico <- NA

AUX$Outra <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANIMAL == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANIMAL == "2") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANIMAL == "3") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANIMAL == "4") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANIMAL == "5") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           ANIMAL == "6") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           ANIMAL == "7") %>%   
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:11] <- apply(AUX[, 4:11], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2025_AGRESSOR"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2025_AGRESSOR"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2025_AGRESSOR.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CONDIC_ANI == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CONDIC_ANI == "2") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CONDIC_ANI == "3") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CONDIC_ANI == "4") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:8] <- apply(AUX[, 4:8], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2025_COND_ANIMAL_ACID"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2025_COND_ANIMAL_ACID"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2025_COND_ANIMAL_ACID.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          OBSERVACAO == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          OBSERVACAO == "2") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:6] <- apply(AUX[, 4:6], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2025_OBSERVAVEL"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2025_OBSERVAVEL"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2025_OBSERVAVEL.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRAT_ATUAL == "2") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRAT_ATUAL == "3") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRAT_ATUAL == "4") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRAT_ATUAL == "5") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRAT_ATUAL == "6") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           TRAT_ATUAL == "7") %>%   
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:10] <- apply(AUX[, 4:10], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2025_TRATAMENTO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2025_TRATAMENTO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2025_TRATAMENTO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FIM_ANIMAL == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FIM_ANIMAL == "2") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FIM_ANIMAL == "3") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FIM_ANIMAL == "4") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FIM_ANIMAL == "5") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           FIM_ANIMAL == "9") %>%   
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:10] <- apply(AUX[, 4:10], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2025_COND_FINAL_ANIMAL"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2025_COND_FINAL_ANIMAL"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2025_COND_FINAL_ANIMAL.csv"), 
           row.names = FALSE)

###Tabela Interrupção de tratamento (campo 49 do SINAN)####

SINAN_ANTRAB_2025_INTERRUPCAO_TRAT <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

SINAN_ANTRAB_2025_INTERRUPCAO_TRAT$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

SINAN_ANTRAB_2025_INTERRUPCAO_TRAT$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

SINAN_ANTRAB_2025_INTERRUPCAO_TRAT$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

SINAN_ANTRAB_2025_INTERRUPCAO_TRAT <- SINAN_ANTRAB_2025_INTERRUPCAO_TRAT[,c(4, 1, 2, 3)]

SINAN_ANTRAB_2025_INTERRUPCAO_TRAT$Sim <- NA

SINAN_ANTRAB_2025_INTERRUPCAO_TRAT$Nao <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  SINAN_ANTRAB_2025_INTERRUPCAO_TRAT[which(SINAN_ANTRAB_2025_INTERRUPCAO_TRAT$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                                                                                 filter(ID_MN_RESI == i,
                                                                                                                        TRA_INTERR == "1") %>%   
                                                                                                                 count()
  )    
  
  SINAN_ANTRAB_2025_INTERRUPCAO_TRAT[which(SINAN_ANTRAB_2025_INTERRUPCAO_TRAT$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                                                                                 filter(ID_MN_RESI == i,
                                                                                                                        TRA_INTERR == "2") %>%   
                                                                                                                 count()
  )
}

AUX[(nrow(AUX) +1), 4:6] <- apply(AUX[, 4:6], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2025_INTERRUPCAO_TRAT"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2025_INTERRUPCAO_TRAT"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2025_INTERRUPCAO_TRAT.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRA_INDI_N == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRA_INDI_N == "2") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRA_INDI_N == "9") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:7] <- apply(AUX[, 4:7], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2025_SOROTERAPIA"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2025_SOROTERAPIA"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2025_SOROTERAPIA.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TIP_SORO == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TIP_SORO== "2") %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:6] <- apply(AUX[, 4:6], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2025_TIPO_IMUNOBIOLOGICO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2025_TIPO_IMUNOBIOLOGICO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2025_TIPO_IMUNOBIOLOGICO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRA_INFILT == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRA_INFI_1 == "1") %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:6] <- apply(AUX[, 4:6], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2025_SORO_INFILTRACAO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2025_SORO_INFILTRACAO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2025_SORO_INFILTRACAO.csv"), 
           row.names = FALSE)

rm(SINAN_ANTRAB_2025, ANTRAB2025)

######   Série Histórica   ########

RS_ANTRAB_Serie_Historica[nrow(RS_ANTRAB_Serie_Historica), 1] <- RS22_ANTRAB_2025_GERAL[17, 5]
RS_ANTRAB_Serie_Historica[nrow(RS_ANTRAB_Serie_Historica), 2] <- RS22_ANTRAB_2025_GERAL[17, 6]
RS_ANTRAB_Serie_Historica[nrow(RS_ANTRAB_Serie_Historica), 3] <- RS22_ANTRAB_2025_GERAL[17, 7]
RS_ANTRAB_Serie_Historica[nrow(RS_ANTRAB_Serie_Historica), 4] <- RS22_ANTRAB_2025_GERAL[17, 8]
RS_ANTRAB_Serie_Historica[nrow(RS_ANTRAB_Serie_Historica), 5] <- RS22_ANTRAB_2025_GERAL[17, 9]
RS_ANTRAB_Serie_Historica[nrow(RS_ANTRAB_Serie_Historica), 6] <- RS22_ANTRAB_2025_GERAL[17, 10]
RS_ANTRAB_Serie_Historica[nrow(RS_ANTRAB_Serie_Historica), 7] <- RS22_ANTRAB_2025_GERAL[17, 11]
RS_ANTRAB_Serie_Historica[nrow(RS_ANTRAB_Serie_Historica), 8] <- RS22_ANTRAB_2025_GERAL[17, 12]
RS_ANTRAB_Serie_Historica[nrow(RS_ANTRAB_Serie_Historica), 9] <- RS22_ANTRAB_2025_GERAL[17, 13]
RS_ANTRAB_Serie_Historica[nrow(RS_ANTRAB_Serie_Historica), 10] <- RS22_ANTRAB_2025_GERAL[17, 14]
RS_ANTRAB_Serie_Historica[nrow(RS_ANTRAB_Serie_Historica), 11] <- RS22_ANTRAB_2025_GERAL[17, 15]
RS_ANTRAB_Serie_Historica[nrow(RS_ANTRAB_Serie_Historica), 12] <- RS22_ANTRAB_2025_GERAL[17, 16]
RS_ANTRAB_Serie_Historica[nrow(RS_ANTRAB_Serie_Historica), 13] <- RS22_ANTRAB_2025_GERAL[17, 17]
RS_ANTRAB_Serie_Historica[nrow(RS_ANTRAB_Serie_Historica), 14] <- RS22_ANTRAB_2025_GERAL[17, 18]
RS_ANTRAB_Serie_Historica[nrow(RS_ANTRAB_Serie_Historica), 15] <- RS22_ANTRAB_2025_GERAL[17, 19]
RS_ANTRAB_Serie_Historica[nrow(RS_ANTRAB_Serie_Historica), 16] <- RS22_ANTRAB_2025_GERAL[17, 20]
RS_ANTRAB_Serie_Historica[nrow(RS_ANTRAB_Serie_Historica), 17] <- RS22_ANTRAB_2025_GERAL[17, 21]
RS_ANTRAB_Serie_Historica[nrow(RS_ANTRAB_Serie_Historica), 18] <- RS22_ANTRAB_2025_GERAL[17, 22]
RS_ANTRAB_Serie_Historica[nrow(RS_ANTRAB_Serie_Historica), 19] <- RS22_ANTRAB_2025_GERAL[17, 23]
RS_ANTRAB_Serie_Historica[nrow(RS_ANTRAB_Serie_Historica), 20] <- RS22_ANTRAB_2025_GERAL[17, 24]
RS_ANTRAB_Serie_Historica[nrow(RS_ANTRAB_Serie_Historica), 21] <- RS22_ANTRAB_2025_GERAL[17, 25]
RS_ANTRAB_Serie_Historica[nrow(RS_ANTRAB_Serie_Historica), 22] <- RS22_ANTRAB_2025_GERAL[17, 26]
RS_ANTRAB_Serie_Historica[nrow(RS_ANTRAB_Serie_Historica), 23] <- RS22_ANTRAB_2025_GERAL[17, 27]
RS_ANTRAB_Serie_Historica[nrow(RS_ANTRAB_Serie_Historica), 24] <- RS22_ANTRAB_2025_GERAL[17, 28]

RS_ANTRAB_Serie_Historica[, 25] <- as.factor(RS_ANTRAB_Serie_Historica[, 25])

####################################################################################################################
################Trabalhando as tabelas base do Canal Endêmico   ####################################################
####################################################################################################################

######     Canal Endêmico    NOTIFICADOS#####

RS_CE_BASE_ANTRAB[(nrow(RS_CE_BASE_ANTRAB) +1), 1] <- "2025"
RS_CE_BASE_ANTRAB[nrow(RS_CE_BASE_ANTRAB), 2:54] <- as.integer(data.frame(RS22_ANTRAB_2025_SE_Notificados[nrow(RS22_ANTRAB_2025_SE_Notificados), 2:54]))

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

rm(AUX, AUX2, RS_CE_BASE_ANTRAB)

write.csv (RS_CE_ANTRAB, 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_CE_ANTRAB.csv"), 
           row.names = FALSE)

###    CANAL ENDÊMICO NOTIFICADOS     ####

###    Puxando os dados da tabela RS22_CE_Notificados e excluindo os períodos epidêmicos:   ######
###                             2015/16, 2019/20 e 2021/22                                  ######

AUX_GRAF <- RS_CE_ANTRAB[,]

###Criando uma coluna de ordem das se para o R não colocar em ordem numérica.

AUX_GRAF$Sem_EPI <-as.character(c("2025/01",  "2025/02", "2025/03", 
                                  "2025/04",  "2025/05",  "2025/06",  
                                  "2025/07",  "2025/08",  "2025/09",  
                                  "2025/10",  "2025/11",  "2025/12",  
                                  "2025/13",  "2025/14",  "2025/15",  
                                  "2025/16",  "2025/17",  "2025/18",  
                                  "2025/19",  "2025/20",  "2025/21",  
                                  "2025/22",  "2025/23",  "2025/24",  
                                  "2025/25",  "2025/26",  "2025/27",  
                                  "2025/28",  "2025/29",  "2025/30",  
                                  "2025/31",  "2025/32",  "2025/33", 
                                  "2025/34",  "2025/35",  "2025/36",  
                                  "2025/37",  "2025/38",  "2025/39",  
                                  "2025/40",  "2025/41",  "2025/42",  
                                  "2025/43",  "2025/44",  "2025/45",  
                                  "2025/46",  "2025/47",  "2025/48",  
                                  "2025/49",  "2025/50",  "2025/51",  
                                  "2025/52",  "2025/53")
)

RS_GRAF_2025_CE_Notificados <- ggplot(AUX_GRAF, aes(Semana_Epidemiológica))  +
  theme(axis.text.x = element_text(angle = 85, 
                                   vjust = .5,
                                   face = "bold")) +
  labs(caption = "Fonte",
       title = "Diagrama de Controle - 2025",
       subtitle = "Atendimentos Antirrábicos NOTIFICADOS") +
  theme(
    panel.grid.major = element_line(color = "#C0C0C0"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#DC143C"),
    plot.title = element_text(face = "bold",
                              size = 19)
  ) +
  geom_area(aes(y = Lim_Superior), 
            fill = "#F0E68C",
            alpha = 0.9) +
  geom_area(aes(y = Media), 
            fill = "#556B2F") +
  geom_line(aes(y = `2025`), 
            stat = "identity", 
            color = "black", 
            linewidth = 1.5) +
  xlab("Semana Epidemiológica") +
  ylab("Número de Casos") +
  scale_x_continuous(breaks = c(1:53), 
                     label = AUX_GRAF$Sem_EPI) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

png(filename = "/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/RAIVA/Diagrama_de_Controle.png", 
    width = 33,
    height = 20,
    units = "cm", pointsize = 8, res = 300)

RS_GRAF_2025_CE_Notificados

dev.off()

##############   Gráficos Gerais    ######

#####     Série histórica Paraná    #####

PR_ANTRAB_Serie_Historica$Ano <- as.factor(PR_ANTRAB_Serie_Historica$Ano)

PR_GRAF_Serie_Historica <- ggplot(PR_ANTRAB_Serie_Historica, aes(x = Ano, 
                                      y = Notificados)) +
  theme(axis.text.x = element_text(angle = 0, 
                                   vjust = .5,
                                   face = "bold"),
        axis.text.y = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 0.5,
                                   face = "bold")) +
  labs(caption = "Fonte", 
       x = "Anos",
       y = "Número de Casos",
       title = "Série Histórica - PARANÁ",
       subtitle = "Casos Notificados (2012 - 2025)") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 19,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#EEE8AA") + 
  geom_label(aes(label = Notificados), 
             size = 3, 
             alpha = 0.5,
             vjust = 0.3) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

png(filename = "/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/RAIVA/Seria_Historica_PARANA.png", 
    width = 33,
    height = 20,
    units = "cm", pointsize = 8, res = 300)

PR_GRAF_Serie_Historica

dev.off()

####   Serie Histórica regional   ######

RS_GRAF_Serie_Historica <- ggplot(RS_ANTRAB_Serie_Historica, aes(x = V25, 
                                      y = Notificados)) +
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
  labs(caption = "Fonte", 
       x = "Anos",
       y = "Número de Casos",
       title = "Série Histórica - 22ªRS",
       subtitle = "Casos Notificados (2012 - 2025)") +
  geom_bar(stat = "identity",
           color = "black",
           fill = "green") +
  geom_label(aes(label = Notificados), 
             size = 3, 
             alpha = 0.5,
             nudge_y = 5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

png(filename = "/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/RAIVA/Seria_Historica_PARANA.png", 
    width = 33,
    height = 20,
    units = "cm", pointsize = 8, res = 300)

RS_GRAF_Serie_Historica

dev.off()

#####    Faixa Etária  ####

AUX <- RS22_ANTRAB_2025_GERAL[17, 12:17]

AUX[2,] <- as.factor(colnames(AUX))

AUX <- as.data.frame(t(AUX))

AUX[,3] <- as.factor(c(1: nrow(AUX)))

colnames(AUX) <- c("Casos", "Label", "Ordem")

AUX[,2] <- c("< 1 ano", "1 |-- 5 anos", "5 |-- 12 anos", 
                  "12 |-- 18 anos", "18 |-- 60 anos", "≥ 60 anos")

AUX[, 1] <- as.numeric(AUX[, 1])

ggplot(AUX, aes(x = Ordem, y = Casos)) +
  theme(axis.text.x = element_text(angle = 0, 
                                   vjust = 0.5,
                                   face = "bold",
                                   size = 12),
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
  labs(caption = "Fonte", 
       x = NULL,
       y = "Número de Casos",
       title = "FAIXA ETÁRIA CASOS NOTIFICADOS - 22ªRS") +
  geom_bar(stat = "identity",
           color = "black",
           fill = "green") +
  geom_label(aes(label = Casos), 
             size = 3, 
             alpha = 0.5,
             vjust = 0.1) +
  scale_x_discrete(breaks = c(1:6),
                   labels = AUX$Label) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

#####    Escolariadade  ####

AUX <- RS22_ANTRAB_2025_GERAL[17, 18:26]

AUX[2,] <- as.factor(colnames(AUX))

AUX <- as.data.frame(t(AUX))

AUX[,3] <- as.factor(c(1: nrow(AUX)))

colnames(AUX) <- c("Casos", "Label", "Ordem")

AUX[, 2] <- c("Analfabeto", "Fundamental 
Incompleto", "Fundamental", "Médio 
Incompleto", 
                  "Médio", "Superior 
Incompleto", "Superior", "Não se Aplica", "Ignorado")

AUX[, 1] <- as.numeric(AUX[, 1])

ggplot(AUX, aes(x = Ordem, y = Casos)) +
  theme(axis.text.x = element_text(angle = 0, 
                                   vjust = 0.5,
                                   face = "bold",
                                   size = 12),
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
  labs(caption = "Fonte", 
       x = NULL,
       y = "Número de Casos",
       title = "ESCOLARIDADE CASOS NOTIFICADOS - 22ªRS") +
  geom_bar(stat = "identity",
           color = "black",
           fill = "green") +
  geom_label(aes(label = Casos), 
             size = 3, 
             alpha = 0.5,
             vjust = 0.1) +
  scale_x_discrete(breaks = c(1:9),
                   labels = AUX$Label) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

#####    Zona de ocorrência  ####

AUX <- RS22_ANTRAB_2025_GERAL[17, 6:9]

AUX[2,] <- as.factor(colnames(AUX))

AUX <- as.data.frame(t(AUX))

AUX[,3] <- as.factor(c(1: nrow(AUX)))

colnames(AUX) <- c("Casos", "Label", "Ordem")

AUX[, 2] <- c("Urbana", "Rural", "Periurbana", "Ignorados")

AUX[, 1] <- as.numeric(AUX[, 1])

ggplot(AUX, aes(x = Ordem, y = Casos)) +
  theme(axis.text.x = element_text(angle = 0, 
                                   vjust = 0.5,
                                   face = "bold",
                                   size = 12),
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
  labs(caption = "Fonte", 
       x = NULL,
       y = "Número de Casos",
       title = "ZONA DE OCORRÊNCIA CASOS NOTIFICADOS - 22ªRS") +
  geom_bar(stat = "identity",
           color = "black",
           fill = "green") +
  geom_label(aes(label = Casos), 
             size = 3, 
             alpha = 0.5,
             vjust = 0.1) +
  scale_x_discrete(breaks = c(1:4),
                   labels = AUX$Label) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

#####    Sexo    ####

AUX <- RS22_ANTRAB_2025_GERAL[17, 10:11]

AUX[2,] <- as.factor(colnames(AUX))

AUX <- as.data.frame(t(AUX))

AUX[,3] <- as.factor(c(1: nrow(AUX)))

colnames(AUX) <- c("Casos", "Label", "Ordem")

AUX[, 2] <- c("Feminino", "Masculino")

AUX[, 1] <- as.numeric(AUX[, 1])

ggplot(AUX, aes(x = Ordem, y = Casos)) +
  theme(axis.text.x = element_text(angle = 0, 
                                   vjust = 0.5,
                                   face = "bold",
                                   size = 12),
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
  labs(caption = "Fonte", 
       x = NULL,
       y = "Número de Casos",
       title = "ZONA DE OCORRÊNCIA CASOS NOTIFICADOS - 22ªRS") +
  geom_bar(stat = "identity",
           color = "black",
           fill = "green") +
  geom_label(aes(label = Casos), 
             size = 3, 
             alpha = 0.5,
             vjust = 0.1) +
  scale_x_discrete(breaks = c(1:2),
                   labels = AUX$Label) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

#####    Espécie Agressora    ####

AUX <- RS22_ANTRAB_2025_AGRESSOR[17, 5:11]

AUX[2,] <- as.factor(colnames(AUX))

AUX <- as.data.frame(t(AUX))

AUX[,3] <- as.factor(c(1: nrow(AUX)))

colnames(AUX) <- c("Casos", "Label", "Ordem")

AUX[, 2] <- c("Canina", "Felina", "Quiróptera", "Primata", "Raposa", "Herbívoro 
Doméstico", "Outra")

AUX[, 1] <- as.numeric(AUX[, 1])

ggplot(AUX, aes(x = Ordem, y = Casos)) +
  theme(axis.text.x = element_text(angle = 0, 
                                   vjust = 0.5,
                                   face = "bold",
                                   size = 12),
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
  labs(caption = "Fonte", 
       x = NULL,
       y = "Número de Casos",
       title = "ESPÉCIE AGRESSORA - 22ªRS") +
  geom_bar(stat = "identity",
           color = "black",
           fill = "green") +
  geom_label(aes(label = Casos), 
             size = 3, 
             alpha = 0.5,
             vjust = 0.1) +
  scale_x_discrete(breaks = c(1:7),
                   labels = AUX$Label) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

#####    cONDIÇÃO DO ANIMAL    ####

AUX <- RS22_ANTRAB_2025_COND_ANIMAL_ACID[17, 5:8]

AUX[2,] <- as.factor(colnames(AUX))

AUX <- as.data.frame(t(AUX))

AUX[,3] <- as.factor(c(1: nrow(AUX)))

colnames(AUX) <- c("Casos", "Label", "Ordem")

AUX[, 2] <- c("Sadio", "Suspeito", "Raivoso", "Morto/Desaparecido")

AUX[, 1] <- as.numeric(AUX[, 1])

ggplot(AUX, aes(x = Ordem, y = Casos)) +
  theme(axis.text.x = element_text(angle = 0, 
                                   vjust = 0.5,
                                   face = "bold",
                                   size = 12),
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
  labs(caption = "Fonte", 
       x = NULL,
       y = "Número de Casos",
       title = "CONDIÇÃO DO ANIMAL AGRESSOR - 22ªRS") +
  geom_bar(stat = "identity",
           color = "black",
           fill = "green") +
  geom_label(aes(label = Casos), 
             size = 3, 
             alpha = 0.5,
             vjust = 0.1) +
  scale_x_discrete(breaks = c(1:4),
                   labels = AUX$Label) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

#####    cONDIÇÃO DO ANIMAL    ####

AUX <- RS22_ANTRAB_2025_COND_ANIMAL_ACID[17, 5:8]

AUX[2,] <- as.factor(colnames(AUX))

AUX <- as.data.frame(t(AUX))

AUX[,3] <- as.factor(c(1: nrow(AUX)))

colnames(AUX) <- c("Casos", "Label", "Ordem")

AUX[, 2] <- c("Sadio", "Suspeito", "Raivoso", "Morto/Desaparecido")

AUX[, 1] <- as.numeric(AUX[, 1])

ggplot(AUX, aes(x = Ordem, y = Casos)) +
  theme(axis.text.x = element_text(angle = 0, 
                                   vjust = 0.5,
                                   face = "bold",
                                   size = 12),
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
  labs(caption = "Fonte", 
       x = NULL,
       y = "Número de Casos",
       title = "CONDIÇÃO DO ANIMAL AGRESSOR - 22ªRS") +
  geom_bar(stat = "identity",
           color = "black",
           fill = "green") +
  geom_label(aes(label = Casos), 
             size = 3, 
             alpha = 0.5,
             vjust = 0.1) +
  scale_x_discrete(breaks = c(1:4),
                   labels = AUX$Label) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

#####    Tratamento Indicado    ####

AUX <- RS22_ANTRAB_2025_TRATAMENTO[17, 5:10]

AUX[2,] <- as.factor(colnames(AUX))

AUX <- as.data.frame(t(AUX))

AUX[,3] <- as.factor(c(1: nrow(AUX)))

colnames(AUX) <- c("Casos", "Label", "Ordem")

AUX[, 2] <- c("Dispensa de 
Tratamento", "Observação do 
Animal", "Observação do 
Animal e Vacina", "Vacina", "Sorovacinação", "Esquema de 
Reexposição")

AUX[, 1] <- as.numeric(AUX[, 1])

ggplot(AUX, aes(x = Ordem, y = Casos)) +
  theme(axis.text.x = element_text(angle = 0, 
                                   vjust = 0.5,
                                   face = "bold",
                                   size = 12),
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
  labs(caption = "Fonte", 
       x = NULL,
       y = "Número de Casos",
       title = "TRATAMENTO REALIZADO - 22ªRS") +
  geom_bar(stat = "identity",
           color = "black",
           fill = "green") +
  geom_label(aes(label = Casos), 
             size = 3, 
             alpha = 0.5,
             vjust = 0.1) +
  scale_x_discrete(breaks = c(1:6),
                   labels = AUX$Label) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

#####    Tipo de Exposição    ####

AUX <- RS22_ANTRAB_2025_EXPOSICAO[17, 5:9]

AUX[2,] <- as.factor(colnames(AUX))

AUX <- as.data.frame(t(AUX))

AUX[,3] <- as.factor(c(1: nrow(AUX)))

colnames(AUX) <- c("Casos", "Label", "Ordem")

AUX[, 2] <- c("Contato
Indireto", "Arranhadura", "Lambedura", "Mordedura", "Outro")

AUX[, 1] <- as.numeric(AUX[, 1])

ggplot(AUX, aes(x = Ordem, y = Casos)) +
  theme(axis.text.x = element_text(angle = 0, 
                                   vjust = 0.5,
                                   face = "bold",
                                   size = 12),
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
  labs(caption = "Fonte", 
       x = NULL,
       y = "Número de Casos",
       title = "TIPO DE FERIMENTO - 22ªRS") +
  geom_bar(stat = "identity",
           color = "black",
           fill = "green") +
  geom_label(aes(label = Casos), 
             size = 3, 
             alpha = 0.5,
             vjust = 0.1) +
  scale_x_discrete(breaks = c(1:5),
                   labels = AUX$Label) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

#####    Animal Observável    ####

AUX <- RS22_ANTRAB_2025_OBSERVAVEL[17, 5:6]

AUX[2,] <- as.factor(colnames(AUX))

AUX <- as.data.frame(t(AUX))

AUX[,3] <- as.factor(c(1: nrow(AUX)))

colnames(AUX) <- c("Casos", "Label", "Ordem")

AUX[, 2] <- c("Sim", "Não")

AUX[, 1] <- as.numeric(AUX[, 1])

ggplot(AUX, aes(x = Ordem, y = Casos)) +
  theme(axis.text.x = element_text(angle = 0, 
                                   vjust = 0.5,
                                   face = "bold",
                                   size = 12),
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
  labs(caption = "Fonte", 
       x = NULL,
       y = "Número de Casos",
       title = "ANIMAL OBSERVÁVEL - 22ªRS") +
  geom_bar(stat = "identity",
           color = "black",
           fill = "green") +
  geom_label(aes(label = Casos), 
             size = 3, 
             alpha = 0.5,
             vjust = 0.1) +
  scale_x_discrete(breaks = c(1:2),
                   labels = AUX$Label) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))


#####    Soroterapia    ####

AUX <- RS22_ANTRAB_2025_SOROTERAPIA[17, 5:7]

AUX[2,] <- as.factor(colnames(AUX))

AUX <- as.data.frame(t(AUX))

AUX[,3] <- as.factor(c(1: nrow(AUX)))

colnames(AUX) <- c("Casos", "Label", "Ordem")

AUX[, 2] <- c("Sim", "Não", "Ignorado")

AUX[, 1] <- as.numeric(AUX[, 1])

ggplot(AUX, aes(x = Ordem, y = Casos)) +
  theme(axis.text.x = element_text(angle = 0, 
                                   vjust = 0.5,
                                   face = "bold",
                                   size = 12),
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
  labs(caption = "Fonte", 
       x = NULL,
       y = "Número de Casos",
       title = "UTILIZAÇÃO DE SORO/IMUNOGLOBULINA - 22ªRS") +
  geom_bar(stat = "identity",
           color = "black",
           fill = "green") +
  geom_label(aes(label = Casos), 
             size = 3, 
             alpha = 0.5,
             vjust = 0.1) +
  scale_x_discrete(breaks = c(1:3),
                   labels = AUX$Label) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

#####   Notificados por município   ####

AUX <- RS22_ANTRAB_2025_GERAL[1 : (nrow(RS22_ANTRAB_2025_GERAL) -1), c(2, 5)]

ggplot(AUX, aes(x = Município, y = Notificados)) +
  theme(axis.text.x = element_text(angle = 70, 
                                   vjust = 0.5,
                                   face = "bold",
                                   size = 12),
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
  labs(caption = "Fonte", 
       x = NULL,
       y = "Número de Casos",
       title = "UTILIZAÇÃO DE SORO/IMUNOGLOBULINA - 22ªRS") +
  geom_bar(stat = "identity",
           color = "black",
           fill = "green") +
  geom_label(aes(label = Notificados), 
             size = 3, 
             alpha = 0.5,
             vjust = 0.1) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

rm(AUX, AUX_GRAF, BASE_IBGE, RS_CE_ANTRAB)
