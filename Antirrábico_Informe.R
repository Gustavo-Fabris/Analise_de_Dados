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

##########################################################################

RS <- 22   #####  Deve-se colocar AQUI a Regional

######################  Definindo o Objeto Fonte   ######################

Fonte <- "Base DBF. Acessada em 22/07//2025. Dados sujeitos a alteração"

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

library(foreign)
library (dplyr)
library(stringr)
library(lubridate)
library(ggspatial)
library(ggplot2)
library(tidyr)
library(gt)
library(kableExtra)
library(sf)
library(ggspatial)

###################       2025      #########################################

BASE_IBGE <- read.csv(file="Base_de_Dados/Auxiliares/Planilha_Base_IBGE.csv", 
                    header=TRUE, 
                    sep=",")

RS_ANTRAB_Serie_Historica <- read.csv(file = "Tabulacoes_R/Raiva/RS_ANTRAB_BASE_Serie_Historica.csv",
                                      header = TRUE,
                                      sep = ",")

RS_CE_BASE_ANTRAB <- read.csv(file = "Tabulacoes_R/Raiva/RS_CE_Base_ANTRAB.csv",
                              header = TRUE,
                              sep = ",")

RS_Historico_Agressor <- read.csv(file = "Tabulacoes_R/Raiva/RS_Historico_Agressor.csv",
                                  header = TRUE,
                                  sep = ",")

RS_Historico_Exposicao <- read.csv(file = "Tabulacoes_R/Raiva/RS_Historico_Exposicao.csv",
                                  header = TRUE,
                                  sep = ",")

RS_Historico_Ferimento <- read.csv(file = "Tabulacoes_R/Raiva/RS_Historico_Ferimento.csv",
                                  header = TRUE,
                                  sep = ",")

RS_Historico_Localizacao <- read.csv(file = "Tabulacoes_R/Raiva/RS_Historico_Localizacao.csv",
                                  header = TRUE,
                                  sep = ",")

RS_Historico_Tipo_Ferimento <- read.csv(file = "Tabulacoes_R/Raiva/RS_Historico_Tipo_Ferimento.csv",
                                  header = TRUE,
                                  sep = ",")

RS_Historico_Tratamento <- read.csv(file = "Tabulacoes_R/Raiva/RS_Historico_Tratamento.csv",
                                  header = TRUE,
                                  sep = ",")

RS_SINAN_Piramide <- read.csv(file = "Tabulacoes_R/Raiva/RS_SINAN_Piramide.csv",
                                    header = TRUE,
                                    sep = ",")

RS_Historico_Raca <- read.csv(file = "Tabulacoes_R/Raiva/RS_Historico_Raca.csv",
                              header = TRUE,
                              sep = ",")

RS_Historico_Zona <- read.csv(file = "Tabulacoes_R/Raiva/RS_Historico_Zona.csv",
                              header = TRUE,
                              sep = ",")

RS_Historico_Sexo <- read.csv(file = "Tabulacoes_R/Raiva/RS_Historico_Sexo.csv",
                              header = TRUE,
                              sep = ",")

RS_Historico_Escolaridade <- read.csv(file = "Tabulacoes_R/Raiva/RS_Historico_Escolaridade.csv",
                              header = TRUE,
                              sep = ",")

RS_GAL_Animal <- read.csv(file = "Base_de_Dados/LACEN/Animal/LACEN_ANIMAL.csv",
                                      header = TRUE,
                                      sep = ",")

SHAPEFILE_REGIONAL <- st_read("Shapefiles/22ª_Regional_de_Saúde/22ª_Regional_de_Saúde.shp")

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

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Sim <- NA

AUX$Nao <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2025 %>% 
                                                                                                                 filter(ID_MN_RESI == i,
                                                                                                                        TRA_INTERR == "1") %>%   
                                                                                                                 count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2025 %>% 
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

###################################   Vigilância Laboratorial    ###########################################

###########  FIltrando o programa da raiva   ###########

RS_GAL_Animal <- RS_GAL_Animal %>% 
  filter(Investigacao == "Investiga��o de Raiva")

### stringr para separar data de hora da coluna Dt_Cadastro ####

AUX <- as.data.frame(str_split(RS_GAL_Animal$Cadastro, 
                               pattern = " ")
)

AUX <- t(AUX)

colnames(AUX)<- c("Data", "Hora")

AUX <- as.data.frame(AUX)

AUX$Data <- dmy(AUX$Data)

RS_GAL_Animal$Cadastro <- AUX$Data

RS_GAL_Animal[, 16] <- str_sub(RS_GAL_Animal$Cadastro, start = 1, end = 4)

colnames(RS_GAL_Animal)[16] <- "Ano"

####################  uSANDO STRINGR PARA ACENTUAR A TABELA DO lacen TORNANDO-A COMPATÍVEL COM A base_ibge   ###################################

RS_GAL_Animal$Municipio <- str_replace(RS_GAL_Animal$Municipio, "ARAPUA", "ARAPUÃ")

RS_GAL_Animal$Municipio <- str_replace(RS_GAL_Animal$Municipio, "ARIRANHA DO IVAI", "ARIRANHA DO IVAÍ")

RS_GAL_Animal$Municipio <- str_replace(RS_GAL_Animal$Municipio, "CANDIDO DE ABREU", "CÂNDIDO DE ABREU")

RS_GAL_Animal$Municipio <- str_replace(RS_GAL_Animal$Municipio, "IVAIPORA", "IVAIPORÃ")

RS_GAL_Animal$Municipio <- str_replace(RS_GAL_Animal$Municipio, "LIDIANOPOLIS", "LIDIANÓPOLIS")

RS_GAL_Animal$Municipio <- str_replace(RS_GAL_Animal$Municipio, "RIO BRANCO DO IVAI", "RIO BRANCO DO IVAÍ")

RS_GAL_Animal$Municipio <- str_replace(RS_GAL_Animal$Municipio, "ROSARIO DO IVAI", "ROSÁRIO DO IVAÍ")

RS_GAL_Animal$Municipio <- str_replace(RS_GAL_Animal$Municipio, "SAO JOAO DO IVAI", "SÃO JOÃO DO IVAÍ")

###################    Separando por município   ###############

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX[, 2] <- as.factor(AUX[, 2])

RS_GAL_Animal$Municipio <- as.factor(RS_GAL_Animal$Municipio)

RS_GAL_Animal$Protocolo <- as.character(RS_GAL_Animal$Protocolo)

RS_GAL_Animal$Protocolo <- as.factor(RS_GAL_Animal$Protocolo)

AUX1 <- RS_GAL_Animal[!duplicated(RS_GAL_Animal$Protocolo),]

for (i in AUX$Município){
  AUX[which(AUX$Município== i), 5] <- as.integer(AUX1 %>%
                                                   filter(Municipio == i) %>%
                                                   count()
                                                 )
}

AUX[, 6] <- (AUX$V5/AUX$Populacao) *100000

for (i in AUX$Município){
  AUX[which(AUX$Município== i), 7] <- as.integer(AUX1 %>%
                                                   filter(Municipio == i,
                                                          Ano == "2025") %>%
                                                   count()
  )
}

AUX1 <- RS_GAL_Animal %>% filter(Municipio == "JARDIM ALEGRE")


#################################################################################################################
##############################FIM FIM FIM FIM FIM FIM FIM ####################################################
################################################################################################################

################################################################################################################
#######################   GRÁFICOS, TABELAS E MAPAS    ##########################################################

Theme <- function() {theme(  panel.grid.major = element_line(color = "#C0C0C0"),
                             panel.grid.minor = element_blank(),
                             panel.background = element_rect(fill = "#F5F5F5"),
                             plot.title = element_text(face = "bold",
                                                       size = 22,
                                                       colour = "black")
)
  }
  
########################   Gráficos   ########################

######   Série Histórica Regional  ########

AUX <- RS_ANTRAB_Serie_Historica[c(5:13), c(1:3)]

AUX[nrow(AUX) +1, 1] <- "2025"

AUX[nrow(AUX), c(2,3)] <- RS22_ANTRAB_2025_GERAL[nrow(RS22_ANTRAB_2025_GERAL), c(4,5)]

RS_ANTRAB_GRAF_Serie_Historica <- ggplot(AUX, 
                                         aes(x = Ano, 
                                             y = Notificados,
                                             group = 1)) +
  geom_line(linewidth = 1.8,
            colour = "black") +
  geom_point(fill = "grey",
             size = 7,
             shape = 21) +
  geom_label(aes(label = Notificados), 
             size = 6, 
             alpha = 0.5,
             vjust = -0.1) +
  labs(caption = Fonte, 
       x = "Ano",
       y = "Notificados",
       title = "Série Histórica - 22ªRS",
       subtitle = "Notificações (2016 - 2025)") +
  Theme() +
  theme(axis.text.x = element_text(angle = 0,
                                   size = 16,
                                   vjust = 1,
                                   face = "bold"),
        plot.caption = element_text(size = 12,
                                    hjust = 0),
        axis.text.y = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 0.5,
                                   face = "bold"),
          )
  
###############  Série Histórica (INCIDÊNCIA) Municípios   #############

RS_ANTRAB_INCID_2022 <- read.csv(file = "Tabulacoes_R/Raiva/RS22_ANTRAB_2022_GERAL.csv",
                                    header = TRUE,
                                    sep = ",")

RS_ANTRAB_INCID_2022  <- RS_ANTRAB_INCID_2022[-nrow(RS_ANTRAB_INCID_2022), c(2, 4, 5)]

RS_ANTRAB_INCID_2022 <- RS_ANTRAB_INCID_2022 %>%
  mutate('2022' = ((RS_ANTRAB_INCID_2022$Notificados/RS_ANTRAB_INCID_2022$Populacao)*100000))

RS_ANTRAB_INCID_2022$'2022' <- format(round(RS_ANTRAB_INCID_2022$'2022', 2))

RS_ANTRAB_INCID_2023 <- read.csv(file = "Tabulacoes_R/Raiva/RS22_ANTRAB_2023_GERAL.csv",
                                 header = TRUE,
                                 sep = ",")

RS_ANTRAB_INCID_2023  <- RS_ANTRAB_INCID_2023[-nrow(RS_ANTRAB_INCID_2023), c(2, 4, 5)]

RS_ANTRAB_INCID_2023 <- RS_ANTRAB_INCID_2023 %>%
  mutate('2023' = ((RS_ANTRAB_INCID_2023$Notificados/RS_ANTRAB_INCID_2023$Populacao)*100000))

RS_ANTRAB_INCID_2023$'2023' <- format(round(RS_ANTRAB_INCID_2023$'2023', 2))

RS_ANTRAB_INCID_2024 <- read.csv(file = "Tabulacoes_R/Raiva/RS22_ANTRAB_2024_GERAL.csv",
                                 header = TRUE,
                                 sep = ",")

RS_ANTRAB_INCID_2024  <- RS_ANTRAB_INCID_2024[-nrow(RS_ANTRAB_INCID_2024), c(2, 4, 5)]

RS_ANTRAB_INCID_2024 <- RS_ANTRAB_INCID_2024 %>%
  mutate('2024' = ((RS_ANTRAB_INCID_2024$Notificados/RS_ANTRAB_INCID_2024$Populacao)*100000))

RS_ANTRAB_INCID_2024$'2024' <- format(round(RS_ANTRAB_INCID_2024$'2024', 2))

RS_ANTRAB_INCID_2025 <- read.csv(file = "Tabulacoes_R/Raiva/RS22_ANTRAB_2025_GERAL.csv",
                                 header = TRUE,
                                 sep = ",")

RS_ANTRAB_INCID_2025  <- RS_ANTRAB_INCID_2025[-nrow(RS_ANTRAB_INCID_2025), c(2, 4, 5)]

RS_ANTRAB_INCID_2025 <- RS_ANTRAB_INCID_2025 %>%
  mutate('2025' = ((RS_ANTRAB_INCID_2025$Notificados/RS_ANTRAB_INCID_2025$Populacao)*100000))

RS_ANTRAB_INCID_2025$'2025' <- format(round(RS_ANTRAB_INCID_2025$'2025', 2))

AUX <- RS_ANTRAB_INCID_2022[, c(1, 4)]

AUX[, 3] <- as.factor(RS_ANTRAB_INCID_2023[, 4])

AUX[, 4] <- as.factor(RS_ANTRAB_INCID_2024[, 4])

AUX[, 5] <- as.factor(RS_ANTRAB_INCID_2025[, 4])

colnames(AUX)[c(3, 4, 5)] <- c('2023', '2024', '2025')

AUX <- pivot_longer(AUX, 
               2:5, 
               names_to = "Ano", 
               values_to = "Incidencia")

AUX[, 3] <- as.double(AUX$Incidencia)

rm(RS_ANTRAB_INCID_2022, RS_ANTRAB_INCID_2023, RS_ANTRAB_INCID_2024, RS_ANTRAB_INCID_2025)

RS_ANTRAB_GRAF_Serie_Historica_Incidencia_I <- ggplot(AUX[c(1:32),], 
                                                      aes(x = Município,
                                                          y = Incidencia)) +
  geom_bar(aes(fill = Ano),
           stat = "identity",
           position = "dodge",
           color = "black",
           linewidth = 0.8) +
  geom_label(aes(y = Incidencia,
                 label = Incidencia),
             position = position_dodge2(width = 0.9),
             size = 2.3, 
             alpha = 0.5,
             vjust = 0.1) +
  scale_fill_manual(name = "",
                    values = c("2022" = "#6495ED", 
                               "2023" = "#5F9EA0",
                               "2024" = "#008B8B",
                               "2025" = "#8FBC8F")) +
  Theme() +
  theme(legend.position = "bottom",  
        legend.title = element_text(face = "bold",
                                    size = 14), 
        axis.text.x = element_text(angle = 30,
                                   vjust = 0.5),
        legend.text = element_text(size = 14), 
        plot.subtitle = element_text(hjust = 0,
                                     size = 12),
        plot.caption = element_text(size = 12,
                                    hjust = 0),
        plot.title = element_text(hjust = 0, 
                                  face = "bold",
                                  size = 20)    
  ) 
 
RS_ANTRAB_GRAF_Serie_Historica_Incidencia_II <- ggplot(AUX[c(33:64),], 
                                                       aes(x = Município,    
                                                           y = Incidencia)) +
  geom_bar(aes(fill = Ano),
           stat = "identity",
           position = "dodge") +
  geom_label(aes(y = Incidencia,
                 label = Incidencia),
             position = position_dodge2(width = 0.9),
             size = 2.3, 
             alpha = 0.5,
             vjust = 0.1) +
  scale_fill_manual(name = "",
                    values = c("2022" = "#6495ED", 
                               "2023" = "#5F9EA0",
                               "2024" = "#008B8B",
                               "2025" = "#8FBC8F")) +
  Theme() +
  theme(legend.position = "bottom",  
        legend.title = element_text(face = "bold",
                                    size = 14), 
        axis.text.x = element_text(angle = 45,
                                   vjust = 0.5),
        legend.text = element_text(size = 14), 
        plot.subtitle = element_text(hjust = 0,
                                     size = 12),
        plot.caption = element_text(size = 12,
                                    hjust = 0),
        plot.title = element_text(hjust = 0, 
                                  face = "bold",
                                  size = 20)    
  ) 

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

AUX <- apply(RS_CE_ANTRAB[, 1: (ncol(RS_CE_ANTRAB)-1)], 1 , median)

RS_CE_ANTRAB <- as.data.frame(RS_CE_ANTRAB)

RS_CE_ANTRAB$Mediana <- AUX

######              Criando a coluna de Desvio Padrão no data frame                ###############

AUX <- apply(RS_CE_ANTRAB[, 1: (ncol(RS_CE_ANTRAB) -2)], 1 , sd)

RS_CE_ANTRAB$Desvio_Padrao <- AUX

######       Criando a coluna de Média + 2(DP)    ######################

AUX <- RS_CE_ANTRAB[, (ncol(RS_CE_ANTRAB)-1):ncol(RS_CE_ANTRAB)]

AUX <- AUX %>% mutate(Lim_Superior = (Mediana + 1.96 * Desvio_Padrao))

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

RS_ANTRAB_GRAF_2025_CE_Notificados <- ggplot(AUX_GRAF, aes(Semana_Epidemiológica))  +
  theme(axis.text.x = element_text(angle = 85, 
                                   vjust = .5,
                                   face = "bold")) +
  labs(caption = Fonte,
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
  geom_area(aes(y = Mediana), 
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

##############   Gráficos Gerais    ######

#####################   Notificações por município (2025)   #######################

AUX <- RS22_ANTRAB_2025_GERAL[-nrow(RS22_ANTRAB_2025_GERAL), c(2,5)]

colnames(AUX) <- c("Municípios", "Casos")

RS_ANTRAB_GRAF_2025_Notificacoes_Municipios <- ggplot(AUX, 
                                                      aes(x = Municípios, 
                                                          y = Casos)) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "green") +
  geom_label(aes(label = Casos), 
             size = 3, 
             alpha = 0.5,
             vjust = 0.1) +
  labs(caption = Fonte, 
       x = NULL,
       y = "Número de Casos",
       title = "Casos Notificados por Município (2025)") +
  Theme() +
  theme(legend.position = "bottom",  
        legend.title = element_text(face = "bold",
                                    size = 14), 
        axis.text.x = element_text(angle = 65,
                                   vjust = 0.5,
                                   face = "bold"),
        legend.text = element_text(size = 14), 
        plot.subtitle = element_text(hjust = 0,
                                     size = 12),
        plot.caption = element_text(size = 12,
                                    hjust = 0),
        plot.title = element_text(hjust = 0, 
                                  face = "bold",
                                  size = 20)) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

#####    Escolariadade  ####

AUX <- tibble(Analfabeto = (RS22_ANTRAB_2025_GERAL[nrow(RS22_ANTRAB_2025_GERAL), 18]/RS22_ANTRAB_2025_GERAL[nrow(RS22_ANTRAB_2025_GERAL), 5]) * 100)

AUX[, 2] <- tibble(Fund_inc = (RS22_ANTRAB_2025_GERAL[nrow(RS22_ANTRAB_2025_GERAL), 19]/RS22_ANTRAB_2025_GERAL[nrow(RS22_ANTRAB_2025_GERAL), 5]) * 100)

AUX[, 3] <- tibble(Fund = (RS22_ANTRAB_2025_GERAL[nrow(RS22_ANTRAB_2025_GERAL), 20]/RS22_ANTRAB_2025_GERAL[nrow(RS22_ANTRAB_2025_GERAL), 5]) * 100)

AUX[, 4] <- tibble(Medio_Inc = (RS22_ANTRAB_2025_GERAL[nrow(RS22_ANTRAB_2025_GERAL), 21]/RS22_ANTRAB_2025_GERAL[nrow(RS22_ANTRAB_2025_GERAL), 5]) * 100)

AUX[, 5] <- tibble(Medio = (RS22_ANTRAB_2025_GERAL[nrow(RS22_ANTRAB_2025_GERAL), 22]/RS22_ANTRAB_2025_GERAL[nrow(RS22_ANTRAB_2025_GERAL), 5]) * 100)

AUX[, 6] <- tibble(Sup_Inc = (RS22_ANTRAB_2025_GERAL[nrow(RS22_ANTRAB_2025_GERAL), 23]/RS22_ANTRAB_2025_GERAL[nrow(RS22_ANTRAB_2025_GERAL), 5]) * 100)

AUX[, 7] <- tibble(Sup = (RS22_ANTRAB_2025_GERAL[nrow(RS22_ANTRAB_2025_GERAL), 24]/RS22_ANTRAB_2025_GERAL[nrow(RS22_ANTRAB_2025_GERAL), 5]) * 100)

AUX[, 8] <- tibble(N_S_A = (RS22_ANTRAB_2025_GERAL[nrow(RS22_ANTRAB_2025_GERAL), 25]/RS22_ANTRAB_2025_GERAL[nrow(RS22_ANTRAB_2025_GERAL), 5]) * 100)

AUX[, 9] <- tibble(Ignorado = (RS22_ANTRAB_2025_GERAL[nrow(RS22_ANTRAB_2025_GERAL), 26]/RS22_ANTRAB_2025_GERAL[nrow(RS22_ANTRAB_2025_GERAL), 5]) * 100)

AUX <- as.data.frame(t(AUX))

AUX[, 2] <- tibble(as.factor(rownames(AUX)))

AUX[,3] <- as.factor(c(1: nrow(AUX)))

colnames(AUX) <- c("Casos", "Label", "Ordem")

AUX[, 2] <- c("Analfabeto", "Fundamental 
Incompleto", "Fundamental", "Médio 
Incompleto", 
                  "Médio", "Superior 
Incompleto", "Superior", "Não se Aplica", "Ignorado")

AUX$Casos <- format(round(AUX$Casos, 2))

AUX[, 1] <- as.numeric(AUX[, 1])

ggplot(AUX, aes(x = Ordem, y = Casos)) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "green") +
  geom_label(aes(label = Casos), 
             size = 3, 
             alpha = 0.5,
             vjust = 0.1) +
  labs(caption = "Fonte", 
       x = NULL,
       y = "Número de Casos",
       title = "ESCOLARIDADE CASOS NOTIFICADOS - 22ªRS",
       subtitle = paste0("n = ", RS22_ANTRAB_2025_GERAL[nrow(RS22_ANTRAB_2025_GERAL), 5])) +
  theme(axis.text.x = element_text(angle = 0, 
                                   vjust = 0.5,
                                   face = "bold",
                                   size = 12),
        axis.text.y = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 0.5,
                                   face = "bold")) +
  scale_x_discrete(breaks = c(1:9),
                   labels = AUX$Label) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  Theme()

#####    Escolariadade (2016 - 2024) ####

RS_Historico_Escolaridade [, 11] <- apply(RS_Historico_Escolaridade[, -ncol(RS_Historico_Escolaridade)], 1, sum)

AUX <- tibble(Analfabeto = (RS_Historico_Escolaridade[nrow(RS_Historico_Escolaridade), 1]/RS_Historico_Escolaridade[nrow(RS_Historico_Escolaridade), 11]) * 100)

AUX[, 2] <- tibble(Fund_inc = (RS_Historico_Escolaridade[nrow(RS_Historico_Escolaridade), 2]/RS_Historico_Escolaridade[nrow(RS_Historico_Escolaridade), 11]) * 100)

AUX[, 3] <- tibble(Fund = (RS_Historico_Escolaridade[nrow(RS_Historico_Escolaridade), 3]/RS_Historico_Escolaridade[nrow(RS_Historico_Escolaridade), 11]) * 100)

AUX[, 4] <- tibble(Medio_Inc = (RS_Historico_Escolaridade[nrow(RS_Historico_Escolaridade), 4]/RS_Historico_Escolaridade[nrow(RS_Historico_Escolaridade), 11]) * 100)

AUX[, 5] <- tibble(Medio = (RS_Historico_Escolaridade[nrow(RS_Historico_Escolaridade), 5]/RS_Historico_Escolaridade[nrow(RS_Historico_Escolaridade), 11]) * 100)

AUX[, 6] <- tibble(Sup_Inc = (RS_Historico_Escolaridade[nrow(RS_Historico_Escolaridade), 6]/RS_Historico_Escolaridade[nrow(RS_Historico_Escolaridade), 11]) * 100)

AUX[, 7] <- tibble(Sup = (RS_Historico_Escolaridade[nrow(RS_Historico_Escolaridade), 7]/RS_Historico_Escolaridade[nrow(RS_Historico_Escolaridade), 11]) * 100)

AUX[, 8] <- tibble(N_S_A = (RS_Historico_Escolaridade[nrow(RS_Historico_Escolaridade), 8]/RS_Historico_Escolaridade[nrow(RS_Historico_Escolaridade), 11]) * 100)

AUX[, 9] <- tibble(Ignorado = (RS_Historico_Escolaridade[nrow(RS_Historico_Escolaridade), 9]/RS_Historico_Escolaridade[nrow(RS_Historico_Escolaridade), 11]) * 100)

AUX <- as.data.frame(t(AUX))

AUX[, 2] <- tibble(as.factor(rownames(AUX)))

AUX[,3] <- as.factor(c(1: nrow(AUX)))

colnames(AUX) <- c("Casos", "Label", "Ordem")

AUX[, 2] <- c("Analfabeto", "Fundamental 
Incompleto", "Fundamental", "Médio 
Incompleto", 
              "Médio", "Superior 
Incompleto", "Superior", "Não se Aplica", "Ignorado")

AUX$Casos <- format(round(AUX$Casos, 2))

AUX[, 1] <- as.numeric(AUX[, 1])

ggplot(AUX, 
       aes(x = Ordem, 
           y = Casos)) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "green") +
  geom_label(aes(label = Casos), 
             size = 3, 
             alpha = 0.5,
             vjust = 0.1) +
  labs(caption = Fonte, 
       x = NULL,
       y = "Percentual de Ocorrência",
       title = "Escolaridade (%) dos Casos Notificados (2016 - 2024)",
       subtitle = paste0("n = ", RS_Historico_Escolaridade[nrow(RS_Historico_Escolaridade), 11])) +
  theme(axis.text.x = element_text(angle = 0, 
                                   vjust = 0.5,
                                   face = "bold",
                                   size = 12),
        axis.text.y = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 0.5,
                                   face = "bold")) +
  scale_x_discrete(breaks = c(1:9),
                   labels = AUX$Label) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  Theme()

#####    Zona de ocorrência  ####

AUX <- tibble(Urbana = (RS22_ANTRAB_2025_GERAL[nrow(RS22_ANTRAB_2025_GERAL), 6]/RS22_ANTRAB_2025_GERAL[nrow(RS22_ANTRAB_2025_GERAL), 5]) * 100)

AUX[, 2] <- tibble(Rural = (RS22_ANTRAB_2025_GERAL[nrow(RS22_ANTRAB_2025_GERAL), 7]/RS22_ANTRAB_2025_GERAL[nrow(RS22_ANTRAB_2025_GERAL), 5]) * 100)

AUX[, 3] <- tibble(Periurbana = (RS22_ANTRAB_2025_GERAL[nrow(RS22_ANTRAB_2025_GERAL), 8]/RS22_ANTRAB_2025_GERAL[nrow(RS22_ANTRAB_2025_GERAL), 5]) * 100)

AUX[, 4] <- tibble(Ignorado = (RS22_ANTRAB_2025_GERAL[nrow(RS22_ANTRAB_2025_GERAL), 9]/RS22_ANTRAB_2025_GERAL[nrow(RS22_ANTRAB_2025_GERAL), 5]) * 100)

AUX <- as.data.frame(AUX)

AUX[2,] <- as.factor(colnames(AUX))

AUX <- as.data.frame(t(AUX))

AUX[,3] <- as.factor(c(1: nrow(AUX)))

colnames(AUX) <- c("Casos", "Label", "Ordem")

AUX[, 1] <- as.numeric(AUX[, 1])

AUX$Casos <- format(round(AUX$Casos, 2))

AUX$Casos <- as.numeric(AUX$Casos)

ggplot(AUX, aes(x = Ordem, y = Casos)) +
   labs(caption = "Fonte", 
       x = NULL,
       y = "Percentual de ocorrência",
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
  Theme()

# #####    Sexo    ####
# 
# AUX <- RS22_ANTRAB_2025_GERAL[17, 10:11]
# 
# AUX[2,] <- as.factor(colnames(AUX))
# 
# AUX <- as.data.frame(t(AUX))
# 
# AUX[,3] <- as.factor(c(1: nrow(AUX)))
# 
# colnames(AUX) <- c("Casos", "Label", "Ordem")
# 
# AUX[, 2] <- c("Feminino", "Masculino")
# 
# AUX[, 1] <- as.numeric(AUX[, 1])
# 
# ggplot(AUX, aes(x = Ordem, y = Casos)) +
#   theme(axis.text.x = element_text(angle = 0, 
#                                    vjust = 0.5,
#                                    face = "bold",
#                                    size = 12),
#         axis.text.y = element_text(angle = 90,
#                                    vjust = 0.5,
#                                    hjust = 0.5,
#                                    face = "bold"),
#         panel.grid.major = element_line(color = "#C0C0C0"),
#         panel.grid.minor = element_blank(),
#         panel.background = element_rect(fill = "#F5F5F5"),
#         plot.title = element_text(face = "bold",
#                                   size = 19,
#                                   colour = "#556B2F")) +
#   labs(caption = "Fonte", 
#        x = NULL,
#        y = "Número de Casos",
#        title = "ZONA DE OCORRÊNCIA CASOS NOTIFICADOS - 22ªRS") +
#   geom_bar(stat = "identity",
#            color = "black",
#            fill = "green") +
#   geom_label(aes(label = Casos), 
#              size = 3, 
#              alpha = 0.5,
#              vjust = 0.1) +
#   scale_x_discrete(breaks = c(1:2),
#                    labels = AUX$Label) +
#   scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

#####    Espécie Agressora    ####

AUX <- tibble(Canina = (RS22_ANTRAB_2025_AGRESSOR[nrow(RS22_ANTRAB_2025_AGRESSOR), 5]/apply(RS22_ANTRAB_2025_AGRESSOR[nrow(RS22_ANTRAB_2025_AGRESSOR), 5:11], 1, sum)*100))

AUX[, 2] <- tibble(Felina = (RS22_ANTRAB_2025_AGRESSOR[nrow(RS22_ANTRAB_2025_AGRESSOR), 6]/apply(RS22_ANTRAB_2025_AGRESSOR[nrow(RS22_ANTRAB_2025_AGRESSOR), 5:11], 1, sum)*100))

AUX[, 3] <- tibble(Quiroptero = (RS22_ANTRAB_2025_AGRESSOR[nrow(RS22_ANTRAB_2025_AGRESSOR), 7]/apply(RS22_ANTRAB_2025_AGRESSOR[nrow(RS22_ANTRAB_2025_AGRESSOR), 5:11], 1, sum)*100))

AUX[, 4] <- tibble(Primata = (RS22_ANTRAB_2025_AGRESSOR[nrow(RS22_ANTRAB_2025_AGRESSOR), 8]/apply(RS22_ANTRAB_2025_AGRESSOR[nrow(RS22_ANTRAB_2025_AGRESSOR), 5:11], 1, sum)*100))

AUX[, 5] <- tibble(Raposa = (RS22_ANTRAB_2025_AGRESSOR[nrow(RS22_ANTRAB_2025_AGRESSOR), 9]/apply(RS22_ANTRAB_2025_AGRESSOR[nrow(RS22_ANTRAB_2025_AGRESSOR), 5:11], 1, sum)*100))

AUX[, 6] <- tibble(Herbivoro_Domestico = (RS22_ANTRAB_2025_AGRESSOR[nrow(RS22_ANTRAB_2025_AGRESSOR), 10]/apply(RS22_ANTRAB_2025_AGRESSOR[nrow(RS22_ANTRAB_2025_AGRESSOR), 5:11], 1, sum)*100))

AUX[, 7] <- tibble(Outra = (RS22_ANTRAB_2025_AGRESSOR[nrow(RS22_ANTRAB_2025_AGRESSOR), 11]/apply(RS22_ANTRAB_2025_AGRESSOR[nrow(RS22_ANTRAB_2025_AGRESSOR), 5:11], 1, sum)*100))

AUX <- as.data.frame(t(AUX))

AUX[, 2] <- tibble(as.factor(colnames(AUX)))

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
  scale_y_discrete(expand = expansion(mult = c(0, 0.05))) +
  Theme()

#####    cONDIÇÃO DO ANIMAL    ####

AUX <- tibble(Sadio = (RS22_ANTRAB_2025_COND_ANIMAL_ACID[nrow(RS22_ANTRAB_2025_COND_ANIMAL_ACID), 5]/apply(RS22_ANTRAB_2025_COND_ANIMAL_ACID[nrow(RS22_ANTRAB_2025_COND_ANIMAL_ACID), 5:8], 1, sum)*100))

AUX[, 2] <- tibble(Suspeito = (RS22_ANTRAB_2025_COND_ANIMAL_ACID[nrow(RS22_ANTRAB_2025_COND_ANIMAL_ACID), 6]/apply(RS22_ANTRAB_2025_COND_ANIMAL_ACID[nrow(RS22_ANTRAB_2025_COND_ANIMAL_ACID), 5:8], 1, sum)*100))

AUX[, 3] <- tibble(Raivoso = (RS22_ANTRAB_2025_COND_ANIMAL_ACID[nrow(RS22_ANTRAB_2025_COND_ANIMAL_ACID), 7]/apply(RS22_ANTRAB_2025_COND_ANIMAL_ACID[nrow(RS22_ANTRAB_2025_COND_ANIMAL_ACID), 5:8], 1, sum)*100))

AUX[, 4] <- tibble(Morto_Desaparecido = (RS22_ANTRAB_2025_COND_ANIMAL_ACID[nrow(RS22_ANTRAB_2025_COND_ANIMAL_ACID), 8]/apply(RS22_ANTRAB_2025_COND_ANIMAL_ACID[nrow(RS22_ANTRAB_2025_COND_ANIMAL_ACID), 5:8], 1, sum)*100))

AUX <- as.data.frame(t(AUX))

AUX[, 2] <- as.factor(colnames(AUX))

AUX[, 3] <- as.factor(c(1: nrow(AUX)))

colnames(AUX) <- c("Casos", "Label", "Ordem")

AUX[, 2] <- c("Sadio", "Suspeito", "Raivoso", "Morto/
Desaparecido")

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

AUX <- tibble(Dispensa_Tratamento = (RS22_ANTRAB_2025_TRATAMENTO[nrow(RS22_ANTRAB_2025_TRATAMENTO), 5]/apply(RS22_ANTRAB_2025_TRATAMENTO[nrow(RS22_ANTRAB_2025_TRATAMENTO), 5:10], 1, sum)*100))

AUX[, 2] <- tibble(Observacao = (RS22_ANTRAB_2025_TRATAMENTO[nrow(RS22_ANTRAB_2025_TRATAMENTO), 6]/apply(RS22_ANTRAB_2025_TRATAMENTO[nrow(RS22_ANTRAB_2025_TRATAMENTO), 5:10], 1, sum)*100))

AUX[, 3] <- tibble(Observacao_vacina = (RS22_ANTRAB_2025_TRATAMENTO[nrow(RS22_ANTRAB_2025_TRATAMENTO), 7]/apply(RS22_ANTRAB_2025_TRATAMENTO[nrow(RS22_ANTRAB_2025_TRATAMENTO), 5:10], 1, sum)*100))

AUX[, 4] <- tibble(Vacina = (RS22_ANTRAB_2025_TRATAMENTO[nrow(RS22_ANTRAB_2025_TRATAMENTO), 8]/apply(RS22_ANTRAB_2025_TRATAMENTO[nrow(RS22_ANTRAB_2025_TRATAMENTO), 5:10], 1, sum)*100))

AUX[, 5] <- tibble(Soro_vacina = (RS22_ANTRAB_2025_TRATAMENTO[nrow(RS22_ANTRAB_2025_TRATAMENTO), 9]/apply(RS22_ANTRAB_2025_TRATAMENTO[nrow(RS22_ANTRAB_2025_TRATAMENTO), 5:10], 1, sum)*100))

AUX[, 6] <- tibble(Reexposicao = (RS22_ANTRAB_2025_TRATAMENTO[nrow(RS22_ANTRAB_2025_TRATAMENTO), 10]/apply(RS22_ANTRAB_2025_TRATAMENTO[nrow(RS22_ANTRAB_2025_TRATAMENTO), 5:10], 1, sum)*100))

AUX <- as.data.frame(t(AUX))

AUX[0, 2] <- as.factor(colnames(AUX))


AUX[, 3] <- as.factor(c(1: nrow(AUX)))

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

AUX <- tibble(Contato_indireto = (RS22_ANTRAB_2025_EXPOSICAO[nrow(RS22_ANTRAB_2025_EXPOSICAO), 5]/apply(RS22_ANTRAB_2025_EXPOSICAO[nrow(RS22_ANTRAB_2025_EXPOSICAO), 5:9], 1, sum)*100))

AUX[, 2] <- tibble(Arranhadura = (RS22_ANTRAB_2025_EXPOSICAO[nrow(RS22_ANTRAB_2025_EXPOSICAO), 6]/apply(RS22_ANTRAB_2025_EXPOSICAO[nrow(RS22_ANTRAB_2025_EXPOSICAO), 5:9], 1, sum)*100))

AUX[, 3] <- tibble(Lambedura = (RS22_ANTRAB_2025_EXPOSICAO[nrow(RS22_ANTRAB_2025_EXPOSICAO), 7]/apply(RS22_ANTRAB_2025_EXPOSICAO[nrow(RS22_ANTRAB_2025_EXPOSICAO), 5:9], 1, sum)*100))

AUX[, 4] <- tibble(Mordedura = (RS22_ANTRAB_2025_EXPOSICAO[nrow(RS22_ANTRAB_2025_EXPOSICAO), 8]/apply(RS22_ANTRAB_2025_EXPOSICAO[nrow(RS22_ANTRAB_2025_EXPOSICAO), 5:9], 1, sum)*100))

AUX[, 5] <- tibble(Outro = (RS22_ANTRAB_2025_EXPOSICAO[nrow(RS22_ANTRAB_2025_EXPOSICAO), 9]/apply(RS22_ANTRAB_2025_EXPOSICAO[nrow(RS22_ANTRAB_2025_EXPOSICAO), 5:9], 1, sum)*100))

AUX <- as.data.frame(t(AUX))

AUX[, 2] <- as.factor(colnames(AUX))

AUX[, 3] <- as.factor(c(1: nrow(AUX)))

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
       title = "TIPO DE Exposição - 22ªRS") +
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

#####    Ferimento    ####

AUX <- tibble(Unico = (RS22_ANTRAB_2025_FERIMENTO[nrow(RS22_ANTRAB_2025_FERIMENTO), 5]/apply(RS22_ANTRAB_2025_FERIMENTO[nrow(RS22_ANTRAB_2025_FERIMENTO), 5:8], 1, sum)*100))

AUX[, 2] <- tibble(Multiplo = (RS22_ANTRAB_2025_FERIMENTO[nrow(RS22_ANTRAB_2025_FERIMENTO), 6]/apply(RS22_ANTRAB_2025_FERIMENTO[nrow(RS22_ANTRAB_2025_FERIMENTO), 5:8], 1, sum)*100))

AUX[, 3] <- tibble(Sem_Ferimento = (RS22_ANTRAB_2025_FERIMENTO[nrow(RS22_ANTRAB_2025_FERIMENTO), 7]/apply(RS22_ANTRAB_2025_FERIMENTO[nrow(RS22_ANTRAB_2025_FERIMENTO), 5:8], 1, sum)*100))

AUX[, 4] <- tibble(Ignorado = (RS22_ANTRAB_2025_FERIMENTO[nrow(RS22_ANTRAB_2025_FERIMENTO), 8]/apply(RS22_ANTRAB_2025_FERIMENTO[nrow(RS22_ANTRAB_2025_FERIMENTO), 5:8], 1, sum)*100))

AUX <- as.data.frame(t(AUX))

AUX[, 2] <- as.factor(colnames(AUX))

AUX[,3] <- as.factor(c(1: nrow(AUX)))

colnames(AUX) <- c("Casos", "Label", "Ordem")

AUX[, 2] <- c("Único", "Múltiplo", "Sem Ferimento", "Ignorado")

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
       title = "Localização da Lesão - 22ªRS") +
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

#####    Tipo Ferimento    ####

AUX <- tibble(Profundo = (RS22_ANTRAB_2025_TIPO_FERIMENTO[nrow(RS22_ANTRAB_2025_TIPO_FERIMENTO), 5]/apply(RS22_ANTRAB_2025_TIPO_FERIMENTO[nrow(RS22_ANTRAB_2025_TIPO_FERIMENTO), 5:7], 1, sum)*100))

AUX[, 2] <- tibble(Superficial = (RS22_ANTRAB_2025_TIPO_FERIMENTO[nrow(RS22_ANTRAB_2025_TIPO_FERIMENTO), 6]/apply(RS22_ANTRAB_2025_TIPO_FERIMENTO[nrow(RS22_ANTRAB_2025_TIPO_FERIMENTO), 5:7], 1, sum)*100))

AUX[, 3] <- tibble(Dilacerante = (RS22_ANTRAB_2025_TIPO_FERIMENTO[nrow(RS22_ANTRAB_2025_TIPO_FERIMENTO), 7]/apply(RS22_ANTRAB_2025_TIPO_FERIMENTO[nrow(RS22_ANTRAB_2025_TIPO_FERIMENTO), 5:7], 1, sum)*100))

AUX <- as.data.frame(t(AUX))

AUX[, 2] <- as.factor(colnames(AUX))

AUX[, 3] <- as.factor(c(1: nrow(AUX)))

colnames(AUX) <- c("Casos", "Label", "Ordem")

AUX[, 2] <- c("Profundo", "Superficial", "Dilacerante")

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
       title = "Localização da Lesão - 22ªRS") +
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

#####    Localização da lesão    ####

AUX <- tibble(Mucosa = (RS22_ANTRAB_2025_LOCALIZACAO[nrow(RS22_ANTRAB_2025_LOCALIZACAO), 5]/apply(RS22_ANTRAB_2025_LOCALIZACAO[nrow(RS22_ANTRAB_2025_LOCALIZACAO), 5:10], 1, sum)*100))

AUX[, 2] <- tibble(Cabeca_Pescoco = (RS22_ANTRAB_2025_LOCALIZACAO[nrow(RS22_ANTRAB_2025_LOCALIZACAO), 6]/apply(RS22_ANTRAB_2025_LOCALIZACAO[nrow(RS22_ANTRAB_2025_LOCALIZACAO), 5:10], 1, sum)*100))

AUX[, 3] <- tibble(Maos_Pes = (RS22_ANTRAB_2025_LOCALIZACAO[nrow(RS22_ANTRAB_2025_LOCALIZACAO), 7]/apply(RS22_ANTRAB_2025_LOCALIZACAO[nrow(RS22_ANTRAB_2025_LOCALIZACAO), 5:10], 1, sum)*100))

AUX[, 4] <- tibble(Tronco = (RS22_ANTRAB_2025_LOCALIZACAO[nrow(RS22_ANTRAB_2025_LOCALIZACAO), 8]/apply(RS22_ANTRAB_2025_LOCALIZACAO[nrow(RS22_ANTRAB_2025_LOCALIZACAO), 5:10], 1, sum)*100))

AUX[, 5] <- tibble(Membros_Superiores = (RS22_ANTRAB_2025_LOCALIZACAO[nrow(RS22_ANTRAB_2025_LOCALIZACAO), 9]/apply(RS22_ANTRAB_2025_LOCALIZACAO[nrow(RS22_ANTRAB_2025_LOCALIZACAO), 5:10], 1, sum)*100))

AUX[, 6] <- tibble(Membros_Superiores = (RS22_ANTRAB_2025_LOCALIZACAO[nrow(RS22_ANTRAB_2025_LOCALIZACAO), 10]/apply(RS22_ANTRAB_2025_LOCALIZACAO[nrow(RS22_ANTRAB_2025_LOCALIZACAO), 5:10], 1, sum)*100))

AUX <- RS22_ANTRAB_2025_LOCALIZACAO[17, 5:10]

AUX[2,] <- as.factor(colnames(AUX))

AUX <- as.data.frame(t(AUX))

AUX[,3] <- as.factor(c(1: nrow(AUX)))

colnames(AUX) <- c("Casos", "Label", "Ordem")

AUX[, 2] <- c("Mucosa", "Cabeça/Pescoço", "Mãos/Pés", "Tronco", "Mem. Superiores", "Mem. Inferiores")

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
       title = "Localização da Lesão - 22ªRS") +
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

#####    Animal Observável    ####

AUX <- tibble(Sim = (RS22_ANTRAB_2025_OBSERVAVEL[nrow(RS22_ANTRAB_2025_OBSERVAVEL), 5]/apply(RS22_ANTRAB_2025_OBSERVAVEL[nrow(RS22_ANTRAB_2025_OBSERVAVEL), 5:6], 1, sum)*100))

AUX[, 2] <- tibble(Nao = (RS22_ANTRAB_2025_OBSERVAVEL[nrow(RS22_ANTRAB_2025_OBSERVAVEL), 6]/apply(RS22_ANTRAB_2025_OBSERVAVEL[nrow(RS22_ANTRAB_2025_OBSERVAVEL), 5:6], 1, sum)*100))

AUX <- as.data.frame(t(AUX))

AUX[, 2] <- as.factor(colnames(AUX))

AUX[, 3] <- as.factor(c(1: nrow(AUX)))

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
       title = "ANIMAL OBSERVÁVEL - 22ªRS",
       subtitle = "Apenas Cães e Gatos") +
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

AUX <- tibble(Sim = (RS22_ANTRAB_2025_SOROTERAPIA[nrow(RS22_ANTRAB_2025_SOROTERAPIA), 5]/apply(RS22_ANTRAB_2025_SOROTERAPIA[nrow(RS22_ANTRAB_2025_SOROTERAPIA), 5:7], 1, sum)*100))

AUX[, 2] <- tibble(Nao = (RS22_ANTRAB_2025_SOROTERAPIA[nrow(RS22_ANTRAB_2025_SOROTERAPIA), 6]/apply(RS22_ANTRAB_2025_SOROTERAPIA[nrow(RS22_ANTRAB_2025_SOROTERAPIA), 5:7], 1, sum)*100))

AUX[, 3] <- tibble(Ignorado = (RS22_ANTRAB_2025_SOROTERAPIA[nrow(RS22_ANTRAB_2025_SOROTERAPIA), 7]/apply(RS22_ANTRAB_2025_SOROTERAPIA[nrow(RS22_ANTRAB_2025_SOROTERAPIA), 5:7], 1, sum)*100))

AUX <- as.data.frame(t(AUX))

AUX[, 2] <- as.factor(colnames(AUX))

AUX[, 3] <- as.factor(c(1: nrow(AUX)))

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

#####    Tipo Imunobiológico    ####

AUX <- tibble(SAR = (RS22_ANTRAB_2025_TIPO_IMUNOBIOLOGICO[nrow(RS22_ANTRAB_2025_TIPO_IMUNOBIOLOGICO), 5]/apply(RS22_ANTRAB_2025_TIPO_IMUNOBIOLOGICO[nrow(RS22_ANTRAB_2025_TIPO_IMUNOBIOLOGICO), 5:6], 1, sum)*100))

AUX[, 2] <- tibble(IGHAR = (RS22_ANTRAB_2025_TIPO_IMUNOBIOLOGICO[nrow(RS22_ANTRAB_2025_TIPO_IMUNOBIOLOGICO), 6]/apply(RS22_ANTRAB_2025_TIPO_IMUNOBIOLOGICO[nrow(RS22_ANTRAB_2025_TIPO_IMUNOBIOLOGICO), 5:6], 1, sum)*100))

AUX <- as.data.frame(t(AUX))

AUX[, 2] <- as.factor(colnames(AUX))

AUX[, 3] <- as.factor(c(1: nrow(AUX)))

colnames(AUX) <- c("Casos", "Label", "Ordem")

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
       title = "Tipo de Imunobiológico Utilizado - 22ªRS") +
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
       title = "Notificações por Município - 22ªRS") +
  geom_bar(stat = "identity",
           color = "black",
           fill = "green") +
  geom_label(aes(label = Notificados), 
             size = 3, 
             alpha = 0.5,
             vjust = 0.1) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

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

###################### Pirâmide Etária   #######################

AUX <- c("< 01", "< 01", "01 - 04", "01 - 04", "05 - 09", "05 - 09", "10 - 14", "10 - 14", "15 - 19", "15 - 19", "20 - 24", "20 - 24", 
         "25 - 29", "25 - 29", "30 - 34", "30 - 34", "35 - 39", "35 - 39", "40 - 44", "40 - 44", "45 - 49", "45 - 49", "50 - 54", "50 - 54",
         "55 - 59", "55 - 59", "60 - 64", "60 - 64", "65 - 69", "65 - 69", "70 - 74",  "70 - 74", "75 - 79", "75 - 79", "80 - 84", 
         "80 - 84", "> 84", "> 84")

AUX <- as.data.frame(AUX)

AUX[, 2] <- c("M", "F", "M", "F", "M", "F", "M", "F", "M", "F", "M", "F", "M", "F", "M", "F", "M", "F", 
              "M", "F", "M", "F", "M", "F", "M", "F", "M", "F", "M", "F", "M", "F", "M", "F", "M", "F", "M", "F")

AUX[1, 3] <- RS22_ANTRAB_2025_SINAN[, 17:18] %>%
  filter(NU_IDADE_N <= 4000 & CS_SEXO == "M") %>%
  count()

AUX[2, 3] <- RS22_ANTRAB_2025_SINAN[, 17:18] %>%
  filter(NU_IDADE_N <= 4000 & CS_SEXO == "F") %>%
  count()

AUX[3, 3] <- RS22_ANTRAB_2025_SINAN[, 17:18] %>%
  filter(NU_IDADE_N > 4000 & NU_IDADE_N <= 4004 & CS_SEXO == "M") %>%
  count()

AUX[4, 3] <- RS22_ANTRAB_2025_SINAN[, 17:18] %>%
  filter(NU_IDADE_N > 4000 & NU_IDADE_N <= 4004 & CS_SEXO == "F") %>%
  count()

AUX[5, 3] <- RS22_ANTRAB_2025_SINAN[, 17:18] %>%
  filter(NU_IDADE_N > 4004 & NU_IDADE_N <= 4009 & CS_SEXO == "M") %>%
  count()

AUX[6, 3] <- RS22_ANTRAB_2025_SINAN[, 17:18] %>%
  filter(NU_IDADE_N > 4004 & NU_IDADE_N <= 4009 & CS_SEXO == "F") %>%
  count()

AUX[7, 3] <- RS22_ANTRAB_2025_SINAN[, 17:18] %>%
  filter(NU_IDADE_N > 4009 & NU_IDADE_N <= 4014 & CS_SEXO == "M") %>%
  count()

AUX[8, 3] <- RS22_ANTRAB_2025_SINAN[, 17:18] %>%
  filter(NU_IDADE_N > 4009 & NU_IDADE_N <= 4014 & CS_SEXO == "F") %>%
  count()

AUX[9, 3] <- RS22_ANTRAB_2025_SINAN[, 17:18] %>%
  filter(NU_IDADE_N > 4014 & NU_IDADE_N <= 4019 & CS_SEXO == "M") %>%
  count()

AUX[10, 3] <- RS22_ANTRAB_2025_SINAN[, 17:18] %>%
  filter(NU_IDADE_N > 4014 & NU_IDADE_N <= 4019 & CS_SEXO == "F") %>%
  count()

AUX[11, 3] <- RS22_ANTRAB_2025_SINAN[, 17:18] %>%
  filter(NU_IDADE_N > 4019 & NU_IDADE_N <= 4024 & CS_SEXO == "M") %>%
  count()

AUX[12, 3] <- RS22_ANTRAB_2025_SINAN[, 17:18] %>%
  filter(NU_IDADE_N > 4019 & NU_IDADE_N <= 4024 & CS_SEXO == "F") %>%
  count()

AUX[13, 3] <- RS22_ANTRAB_2025_SINAN[, 17:18] %>%
  filter(NU_IDADE_N > 4024 & NU_IDADE_N <= 4029 & CS_SEXO == "M") %>%
  count()

AUX[14, 3] <- RS22_ANTRAB_2025_SINAN[, 17:18] %>%
  filter(NU_IDADE_N > 4024 & NU_IDADE_N <= 4029 & CS_SEXO == "F") %>%
  count()

AUX[15, 3] <- RS22_ANTRAB_2025_SINAN[, 17:18] %>%
  filter(NU_IDADE_N > 4029 & NU_IDADE_N <= 4034 & CS_SEXO == "M") %>%
  count()

AUX[16, 3] <- RS22_ANTRAB_2025_SINAN[, 17:18] %>%
  filter(NU_IDADE_N > 4029 & NU_IDADE_N <= 4034 & CS_SEXO == "F") %>%
  count()

AUX[17, 3] <- RS22_ANTRAB_2025_SINAN[, 17:18] %>%
  filter(NU_IDADE_N > 4034 & NU_IDADE_N <= 4039 & CS_SEXO == "M") %>%
  count()

AUX[18, 3] <- RS22_ANTRAB_2025_SINAN[, 17:18] %>%
  filter(NU_IDADE_N > 4034 & NU_IDADE_N <= 4039 & CS_SEXO == "F") %>%
  count()

AUX[19, 3] <- RS22_ANTRAB_2025_SINAN[, 17:18] %>%
  filter(NU_IDADE_N > 4039 & NU_IDADE_N <= 4044 & CS_SEXO == "M") %>%
  count()

AUX[20, 3] <- RS22_ANTRAB_2025_SINAN[, 17:18] %>%
  filter(NU_IDADE_N > 4039 & NU_IDADE_N <= 4044 & CS_SEXO == "F") %>%
  count()

AUX[21, 3] <- RS22_ANTRAB_2025_SINAN[, 17:18] %>%
  filter(NU_IDADE_N > 4044 & NU_IDADE_N <= 4049 & CS_SEXO == "M") %>%
  count()

AUX[22, 3] <- RS22_ANTRAB_2025_SINAN[, 17:18] %>%
  filter(NU_IDADE_N > 4044 & NU_IDADE_N <= 4049 & CS_SEXO == "F") %>%
  count()

AUX[23, 3] <- RS22_ANTRAB_2025_SINAN[, 17:18] %>%
  filter(NU_IDADE_N > 4049 & NU_IDADE_N <= 4054 & CS_SEXO == "M") %>%
  count()

AUX[24, 3] <- RS22_ANTRAB_2025_SINAN[, 17:18] %>%
  filter(NU_IDADE_N > 4049 & NU_IDADE_N <= 4054 & CS_SEXO == "F") %>%
  count()

AUX[25, 3] <- RS22_ANTRAB_2025_SINAN[, 17:18] %>%
  filter(NU_IDADE_N > 4054 & NU_IDADE_N <= 4059 & CS_SEXO == "M") %>%
  count()

AUX[26, 3] <- RS22_ANTRAB_2025_SINAN[, 17:18] %>%
  filter(NU_IDADE_N > 4054 & NU_IDADE_N <= 4059 & CS_SEXO == "F") %>%
  count()

AUX[27, 3] <- RS22_ANTRAB_2025_SINAN[, 17:18] %>%
  filter(NU_IDADE_N > 4059 & NU_IDADE_N <= 4064 & CS_SEXO == "M") %>%
  count()

AUX[28, 3] <- RS22_ANTRAB_2025_SINAN[, 17:18] %>%
  filter(NU_IDADE_N > 4059 & NU_IDADE_N <= 4064 & CS_SEXO == "F") %>%
  count()

AUX[29, 3] <- RS22_ANTRAB_2025_SINAN[, 17:18] %>%
  filter(NU_IDADE_N > 4064 & NU_IDADE_N <= 4069 & CS_SEXO == "M") %>%
  count()

AUX[30, 3] <- RS22_ANTRAB_2025_SINAN[, 17:18] %>%
  filter(NU_IDADE_N > 4064 & NU_IDADE_N <= 4069 & CS_SEXO == "F") %>%
  count()

AUX[31, 3] <- RS22_ANTRAB_2025_SINAN[, 17:18] %>%
  filter(NU_IDADE_N > 4069 & NU_IDADE_N <= 4074 & CS_SEXO == "M") %>%
  count()

AUX[32, 3] <- RS22_ANTRAB_2025_SINAN[, 17:18] %>%
  filter(NU_IDADE_N > 4069 & NU_IDADE_N <= 4074 & CS_SEXO == "F") %>%
  count()

AUX[33, 3] <- RS22_ANTRAB_2025_SINAN[, 17:18] %>%
  filter(NU_IDADE_N > 4074 & NU_IDADE_N <= 4079 & CS_SEXO == "M") %>%
  count()

AUX[34, 3] <- RS22_ANTRAB_2025_SINAN[, 17:18] %>%
  filter(NU_IDADE_N > 4074 & NU_IDADE_N <= 4079 & CS_SEXO == "F") %>%
  count()

AUX[35, 3] <- RS22_ANTRAB_2025_SINAN[, 17:18] %>%
  filter(NU_IDADE_N > 4079 & NU_IDADE_N <= 4084 & CS_SEXO == "M") %>%
  count()

AUX[36, 3] <- RS22_ANTRAB_2025_SINAN[, 17:18] %>%
  filter(NU_IDADE_N > 4079 & NU_IDADE_N <= 4084 & CS_SEXO == "F") %>%
  count()

AUX[37, 3] <- RS22_ANTRAB_2025_SINAN[, 17:18] %>%
  filter(NU_IDADE_N > 4084 & CS_SEXO == "M") %>%
  count()

AUX[38, 3] <- RS22_ANTRAB_2025_SINAN[, 17:18] %>%
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
       caption = "Fonte") +
  scale_x_continuous(labels = abs) +
  #  Theme() +
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


############################################################################################################
#########################    Tabelas   #####################################################################

##############   Agressor por município (2025)   ###########################

gt(RS22_ANTRAB_2025_AGRESSOR[-nrow(RS22_ANTRAB_2025_AGRESSOR), -c(1, 3, 4)]) %>%
  tab_header(title = md("**Animal Agressor por Município**"),
             subtitle = "(Campo 40 SINAN)") %>%
  tab_options(heading.align = "left",
              column_labels.border.top.color = "black",
              column_labels.border.top.width = px(3)) %>%
  tab_spanner(label = "Agressor",
              columns = c(2:8)) %>%
  cols_align(align = "center", columns = c(2:8)
  ) %>%
  cols_label(Canina = "Canino",
             Felina = "Felino",
             Quiroptera = "Quiróptero",
             Herbivoro_Domestico = "Herbívoro 
doméstico") %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels(everything()
            )
  ) %>%
  tab_footnote(footnote = Fonte) %>%
  tab_options(table.font.size = "small")

#############  Uso de Soro/Imunoglobulina por Município (2025)  #######################

gt(RS22_ANTRAB_2025_SOROTERAPIA[-nrow(RS22_ANTRAB_2025_SOROTERAPIA), -c(1, 3, 4)]) %>%
  tab_header(title = md("**Uso de Soro/imunoglobulina por Município**"),
             subtitle = "(Campo 40 SINAN)") %>%
  tab_options(heading.align = "left",
              column_labels.border.top.color = "black",
              column_labels.border.top.width = px(3)) %>%
  tab_spanner(label = "Uso de Imunobiológico",
              columns = c(2:4)) %>%
  cols_align(align = "center", columns = c(2:4)
  ) %>%
    tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels(everything()
            )
  ) %>%
  tab_footnote(footnote = Fonte) %>%
  tab_options(table.font.size = "small")

#############  Tipo de Soro/Imunoglobulina por Município (2025)  #######################

gt(RS22_ANTRAB_2025_TIPO_IMUNOBIOLOGICO[-nrow(RS22_ANTRAB_2025_TIPO_IMUNOBIOLOGICO), -c(1, 3, 4)]) %>%
  tab_header(title = md("**Tipo de Imunobiológico Utilizado**"),
             subtitle = "(Campo 40 SINAN)") %>%
  tab_options(heading.align = "left",
              column_labels.border.top.color = "black",
              column_labels.border.top.width = px(3)) %>%
  tab_spanner(label = "Tipo",
              columns = c(2:3)) %>%
  cols_align(align = "center", columns = c(2:3)
  ) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels(everything()
            )
  ) %>%
  tab_footnote(footnote = Fonte) %>%
  tab_footnote(footnote = "SAR = Soro Antirrábico") %>%
  tab_footnote(footnote = "IGHAR = Imunoglobulína Antirrábica") %>%
  tab_options(table.font.size = "small")

##############   Localização (2025)   ###########################

gt(RS22_ANTRAB_2025_LOCALIZACAO[-nrow(RS22_ANTRAB_2025_LOCALIZACAO), -c(1, 3, 4)]) %>%
  tab_header(title = md("**TLocalização da Lesão por Município**"),
             subtitle = "(Campo 40 SINAN)") %>%
  tab_options(heading.align = "left",
              column_labels.border.top.color = "black",
              column_labels.border.top.width = px(3)) %>%
  tab_spanner(label = "Localização",
              columns = c(2:7)) %>%
  cols_align(align = "center", 
             columns = c(2:7)
  ) %>%
  cols_label(Cabeca_Pescoco = "Cabeça/Pescoço",
             Maos_Pes = "Mãos/Pés",
             Membros_Superiores = "Membros
superiores",
             Membros_Inferiores = "Membros
Inferiores") %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels(everything()
            )
  ) %>%
  tab_footnote(footnote = Fonte) %>%
  tab_options(table.font.size = "small")

##############   Exposição (2025)   ###########################

gt(RS22_ANTRAB_2025_EXPOSICAO[-nrow(RS22_ANTRAB_2025_EXPOSICAO), -c(1, 3, 4)]) %>%
  tab_header(title = md("**Tipo de Exposição por Município**"),
             subtitle = "(Campo 40 SINAN)") %>%
  tab_options(heading.align = "left",
              column_labels.border.top.color = "black",
              column_labels.border.top.width = px(3)) %>%
  tab_spanner(label = "Exposição",
              columns = c(2:6)) %>%
  cols_align(align = "center", 
             columns = c(2:6)
  ) %>%
  cols_label(Contato_Indireto = "Indireta") %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels(everything()
            )
  ) %>%
  tab_footnote(footnote = Fonte) %>%
  tab_options(table.font.size = "small")

##############   Ferimento (2025)   ###########################

gt(RS22_ANTRAB_2025_FERIMENTO[-nrow(RS22_ANTRAB_2025_FERIMENTO), -c(1, 3, 4)]) %>%
  tab_header(title = md("**Ferimento por Município**"),
             subtitle = "(Campo 40 SINAN)") %>%
  tab_options(heading.align = "left",
              column_labels.border.top.color = "black",
              column_labels.border.top.width = px(3)) %>%
  tab_spanner(label = "Ferimento",
              columns = c(2:5)) %>%
  cols_align(align = "center", 
             columns = c(2:5)
  ) %>%
  cols_label(Unico = "Único",
             Multiplo = "Múltiplo",
             Sem_Ferimento = "Sem 
Ferimento") %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels(everything()
            )
  ) %>%
  tab_footnote(footnote = Fonte) %>%
  tab_options(table.font.size = "small")

##############   Tratamento (2025)   ###########################

gt(RS22_ANTRAB_2025_TRATAMENTO[-nrow(RS22_ANTRAB_2025_TRATAMENTO), -c(1, 3, 4)]) %>%
  tab_header(title = md("**Esquema de Tratamento por Município**"),
             subtitle = "(Campo 40 SINAN)") %>%
  tab_options(heading.align = "left",
              column_labels.border.top.color = "black",
              column_labels.border.top.width = px(3)) %>%
  tab_spanner(label = "Esquema de Tratamento",
              columns = c(2:7)) %>%
  cols_align(align = "center", 
             columns = c(2:7)
  ) %>%
  cols_label(Dispensa_Tratamento = "Dispensa de
Tratamento",
             Observação_Animal = "Observação do 
Animal",
             Observação_Vacina = "Observação e
Vacina",
             Soro_Vacina = "Soro e
Vacina",
             Esquema_Reexposicao = "Reesposição") %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels(everything()
            )
  ) %>%
  tab_footnote(footnote = Fonte) %>%
  tab_options(table.font.size = "small")

##############   Condição do animal (2025)   ###########################

gt(RS22_ANTRAB_2025_COND_ANIMAL_ACID[-nrow(RS22_ANTRAB_2025_COND_ANIMAL_ACID), -c(1, 3, 4)]) %>%
  tab_header(title = md("**Condição do Animal Causador do Acidente por Município**"),
             subtitle = "(Campo 40 SINAN)") %>%
  tab_options(heading.align = "left",
              column_labels.border.top.color = "black",
              column_labels.border.top.width = px(3)) %>%
  tab_spanner(label = "Condição do Animal",
              columns = c(2:5)) %>%
  cols_align(align = "center", 
             columns = c(2:5)
  ) %>%
  cols_label(Morto_Desaparecido = "Morto 
Desaparecido") %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels(everything()
            )
  ) %>%
  tab_footnote(footnote = Fonte) %>%
  tab_options(table.font.size = "small")

##############   Observável (2025)   ###########################

gt(RS22_ANTRAB_2025_OBSERVAVEL[-nrow(RS22_ANTRAB_2025_OBSERVAVEL), -c(1, 3, 4)]) %>%
  tab_header(title = md("**Animal Observável (cães/gatos) por Município**"),
             subtitle = "(Campo 40 SINAN)") %>%
  tab_options(heading.align = "left",
              column_labels.border.top.color = "black",
              column_labels.border.top.width = px(3)) %>%
  tab_spanner(label = "Observável",
              columns = c(2:3)) %>%
  cols_align(align = "center", 
             columns = c(2:3)
  )  %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels(everything()
            )
  ) %>%
  tab_footnote(footnote = Fonte) %>%
  tab_options(table.font.size = "small")

rm(AUX, AUX_GRAF, BASE_IBGE, RS_CE_ANTRAB)
