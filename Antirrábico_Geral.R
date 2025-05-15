rm(list =ls())
####Indicando Diretório de Trabalho.#####

setwd("/home/gustavo/Área de Trabalho/Análise_de_Dados/")

###Libraries###

library(foreign)
library (dplyr)

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

#####################################################################################################################
#################  Realizando contagem dos casos do Paraná para realizar a série histórica do Estado  ###############

PR_ANTRAB_Serie_Historica <- data.frame(Ano = 
                                          c("2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023", "2024", "2025"),
                                        Notificados = NA)

####Criando um objeto com a base DBF do SINAN#################

ANTRAB2012 <- read.dbf(file = "Base_de_Dados/DBF/ANTRANET2012.DBF",
                       as.is = FALSE)


####Alterando o formato da coluna ID_MN_RESI de forma que ela seja passível de vincular no for loop#################

ANTRAB2012$ID_MN_RESI <- as.numeric(as.character(ANTRAB2012$ID_MN_RESI))

############################     2012   ########################################

PR_ANTRAB_Serie_Historica[1, 2] <- as.integer(ANTRAB2012 %>% 
                                                count()
)

#####################################################################################################################
#################  Realizando contagem dos casos do Paraná para realizar a série histórica do Estado  ###############

#PR_ANTRAB_Serie_Historica <- data.frame(Ano = 
#                                          c("2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023"),
 #                                       Notificados = NA)

#PR_ANTRAB_Serie_Historica[1, 2] <- as.integer(ANTRAB2012 %>% 
#                                                count()
#)
#####Filtrando os dados da Base DBF do SINAN com os dados de notificação somente da 22RS###########

SINAN_ANTRAB_2012 <- ANTRAB2012 %>% 
  filter(ID_REGIONA == ID_REG | ID_RG_RESI == ID_REG)

assign(paste0("RS", RS, "_ANTRAB_2012_SINAN"), SINAN_ANTRAB_2012)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2012_SINAN"), SINAN_ANTRAB_2012), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2012_SINAN.csv"), 
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
  
  AUX[which(AUX == i), 2] <- as.integer(SINAN_ANTRAB_2012 %>%
                                          filter(ID_MN_RESI == i,
                                                 SEM_PRI ==201201)%>%
                                          count()
  )
  
  AUX[which(AUX == i), 3] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                          filter(ID_MN_RESI == i, 
                                                 SEM_PRI ==201202) %>% 
                                          count()
  )
  
  AUX[which(AUX == i), 4] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                          filter(ID_MN_RESI == i,
                                                 SEM_PRI ==201203) %>% 
                                          count()
  )
  
  AUX[which(AUX == i),5] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                         filter(ID_MN_RESI == i,
                                                SEM_PRI ==201204) %>% 
                                         count()
  )
  
  AUX[which(AUX == i), 6] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                          filter(ID_MN_RESI == i,
                                                 SEM_PRI ==201205) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 7] <- as.integer(SINAN_ANTRAB_2012 %>%
                                          filter(ID_MN_RESI == i, 
                                                 SEM_PRI ==201206) %>%
                                          count()
  )
  
  AUX[which(AUX == i), 8] <- as.integer(SINAN_ANTRAB_2012 %>%
                                          filter(ID_MN_RESI == i, 
                                                 SEM_PRI ==201207) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 9] <- as.integer(SINAN_ANTRAB_2012 %>%
                                          filter(ID_MN_RESI == i, 
                                                 SEM_PRI ==201208) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 10] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201209) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 11] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201210) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 12] <- as.integer(SINAN_ANTRAB_2012 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201211) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 13] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201212) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 14] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201213) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 15] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201214) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 16] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201215) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 17] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201216) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 18] <- as.integer(SINAN_ANTRAB_2012 %>%
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201217) %>%  
                                           count() 
  )
  
  AUX[which(AUX == i), 19] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201218) %>%      
                                           count() 
  )
  
  AUX[which(AUX == i), 20] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201219) %>%
                                           count() 
  )
  
  AUX[which(AUX == i),  21] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                            filter(ID_MN_RESI == i, 
                                                   SEM_PRI ==201220) %>%
                                            count() 
  )
  
  AUX[which(AUX == i), 22] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201221) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 23] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201222) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 24] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201223) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 25] <- as.integer(SINAN_ANTRAB_2012 %>%
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201224) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 26] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201225) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 27] <- as.integer(SINAN_ANTRAB_2012 %>%
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201226) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 28] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201227) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 29] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201228) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 30] <- as.integer(SINAN_ANTRAB_2012 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201229) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 31] <- as.integer(SINAN_ANTRAB_2012 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201230) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 32] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201231) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 33] <- as.integer(SINAN_ANTRAB_2012 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201232) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 34] <- as.integer(SINAN_ANTRAB_2012 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201233) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 35] <- as.integer(SINAN_ANTRAB_2012 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201234) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 36] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201235) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 37] <- as.integer(SINAN_ANTRAB_2012 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201236) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 38] <- as.integer(SINAN_ANTRAB_2012 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201237) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 39] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201238) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 40] <- as.integer(SINAN_ANTRAB_2012 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201239) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 41] <- as.integer(SINAN_ANTRAB_2012 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201240) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 42] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201241) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 43] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201242) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 44] <- as.integer(SINAN_ANTRAB_2012 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201243) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 45] <- as.integer(SINAN_ANTRAB_2012 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201244) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 46] <- as.integer(SINAN_ANTRAB_2012 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201245) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 47] <- as.integer(SINAN_ANTRAB_2012 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201246) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 48] <- as.integer(SINAN_ANTRAB_2012 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201247) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 49] <- as.integer(SINAN_ANTRAB_2012 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201248) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 50] <- as.integer(SINAN_ANTRAB_2012 %>%
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201249) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 51] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201250) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 52] <- as.integer(SINAN_ANTRAB_2012 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201251) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 53] <- as.integer(SINAN_ANTRAB_2012 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201252) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 54] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201253) %>%
                                           count() 
  )
}

AUX[, 1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX[(nrow(AUX)+ 1), 2:54] <- apply(AUX[, 2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2012_SE_Notificados"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2012_SE_Notificados"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2012_SE_Notificados.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                   filter(ID_MN_RESI == i) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CS_ZONA == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CS_ZONA == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CS_ZONA == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CS_ZONA == 9 
                                                          |
                                                            is.na(CS_ZONA)) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_SEXO == "F") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_SEXO == "M") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                    filter(ID_MN_RESI == i,  
                                                           NU_IDADE_N <=3012) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                    filter(ID_MN_RESI == i,  
                                                           NU_IDADE_N > 4000 
                                                           & 
                                                             NU_IDADE_N <=4005) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           NU_IDADE_N > 4005 
                                                           & 
                                                             NU_IDADE_N <=4012) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           NU_IDADE_N > 4012 
                                                           & 
                                                             NU_IDADE_N <=4018) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 16] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           NU_IDADE_N > 4018 
                                                           & 
                                                             NU_IDADE_N <=4059) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 17] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           NU_IDADE_N > 4059) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 18] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "00") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 19] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "01" 
                                                           | 
                                                             CS_ESCOL_N == "02" 
                                                           | 
                                                             CS_ESCOL_N == "03") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 20] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "04") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 21] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "05") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 22] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "06") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 23] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "07") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 24] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "08") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 25] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "10") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 26] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "09") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 27] <- as.integer(SINAN_ANTRAB_2012 %>% 
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
  
  AUX[which(AUX$COD_IBGE == i), 28] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_GESTANT == "5") %>% 
                                                    count()
  )
  AUX[which(AUX$COD_IBGE == i), 29] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_GESTANT == "6") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 30] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_GESTANT == "9") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 31] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "01") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 32] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "2") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 33] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "3") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 34] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "4") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 35] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "5") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 36] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "9") %>% 
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:36] <- apply(AUX[, 4:36], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2012_GERAL"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2012_GERAL"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2012_GERAL.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_CONTAT == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_ARRANH == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_LAMBED == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_MORDED == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_OUTRO_ == "1") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:9] <- apply(AUX[, 4:9], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2012_EXPOSICAO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2012_EXPOSICAO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2012_EXPOSICAO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_MUCOSA == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_CABECA == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_MAOS == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_TRONCO == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_MEMBRO== "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           ANT_MEMB_1== "1") %>%   
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:10] <- apply(AUX[, 4:10], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2012_LOCALIZACAO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2012_LOCALIZACAO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2012_LOCALIZACAO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FERIMENTO == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FERIMENTO == "2") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FERIMENTO == "3") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FERIMENTO == "9") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:8] <- apply(AUX[, 4:8], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2012_FERIMENTO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2012_FERIMENTO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2012_FERIMENTO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_PROFUN == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_SUPERF == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_DILACE == "1") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:7] <- apply(AUX[, 4:7], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2012_TIPO_FERIMENTO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2012_TIPO_FERIMENTO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2012_TIPO_FERIMENTO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          PRE_EXPOS == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          POS_EXPOS == "1") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:6] <- apply(AUX[, 4:6], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2012_ANTECEDENTE_ANTR"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2012_ANTECEDENTE_ANTR"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2012_ANTECEDENTE_ANTR.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANIMAL == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANIMAL == "2") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANIMAL == "3") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANIMAL == "4") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANIMAL == "5") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           ANIMAL == "6") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           ANIMAL == "7") %>%   
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:11] <- apply(AUX[, 4:11], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2012_AGRESSOR"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2012_AGRESSOR"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2012_AGRESSOR.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CONDIC_ANI == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CONDIC_ANI == "2") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CONDIC_ANI == "3") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CONDIC_ANI == "4") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:8] <- apply(AUX[, 4:8], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2012_COND_ANIMAL_ACID"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2012_COND_ANIMAL_ACID"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2012_COND_ANIMAL_ACID.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          OBSERVACAO == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          OBSERVACAO == "2") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:6] <- apply(AUX[, 4:6], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2012_OBSERVAVEL"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2012_OBSERVAVEL"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2012_OBSERVAVEL.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRAT_ATUAL == "2") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRAT_ATUAL == "3") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRAT_ATUAL == "4") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRAT_ATUAL == "5") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRAT_ATUAL == "6") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           TRAT_ATUAL == "7") %>%   
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:10] <- apply(AUX[, 4:10], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2012_TRATAMENTO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2012_TRATAMENTO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2012_TRATAMENTO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FIM_ANIMAL == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FIM_ANIMAL == "2") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FIM_ANIMAL == "3") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FIM_ANIMAL == "4") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FIM_ANIMAL == "5") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           FIM_ANIMAL == "9") %>%   
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:10] <- apply(AUX[, 4:10], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2012_COND_FINAL_ANIMAL"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2012_COND_FINAL_ANIMAL"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2012_COND_FINAL_ANIMAL.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRA_INTERR == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRA_INTERR == "2") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:6] <- apply(AUX[, 4:6], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2012_INTERRUPCAO_TRAT"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2012_INTERRUPCAO_TRAT"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2012_INTERRUPCAO_TRAT.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRA_INDI_N == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRA_INDI_N == "2") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRA_INDI_N == "9") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:7] <- apply(AUX[, 4:7], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2012_SOROTERAPIA"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2012_SOROTERAPIA"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2012_SOROTERAPIA.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TIP_SORO == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TIP_SORO== "2") %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:6] <- apply(AUX[, 4:6], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2012_TIPO_IMUNOBIOLOGICO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2012_TIPO_IMUNOBIOLOGICO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2012_TIPO_IMUNOBIOLOGICO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRA_INFILT == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRA_INFI_1 == "1") %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:6] <- apply(AUX[, 4:6], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2012_SORO_INFILTRACAO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2012_SORO_INFILTRACAO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2012_SORO_INFILTRACAO.csv"), 
           row.names = FALSE)

rm(SINAN_ANTRAB_2012, ANTRAB2012)

#######################   2013  ##############################################################

####Criando um objeto com a base DBF do SINAN#################

ANTRAB2013 <- read.dbf(file = "Base_de_Dados/DBF/ANTRANET2013.DBF",
                       as.is = FALSE)


####Alterando o formato da coluna ID_MN_RESI de forma que ela seja passível de vincular no for loop#################

ANTRAB2013$ID_MN_RESI <- as.numeric(as.character(ANTRAB2013$ID_MN_RESI))

#####################################################################################################################
#################  Realizando contagem dos casos do Paraná para realizar a série histórica do Estado  ###############


PR_ANTRAB_Serie_Historica[2, 2] <- as.integer(ANTRAB2013 %>% 
                                                count()
)
#####Filtrando os dados da Base DBF do SINAN com os dados de notificação somente da 22RS###########

SINAN_ANTRAB_2013 <- ANTRAB2013 %>% 
  filter(ID_REGIONA == ID_REG | ID_RG_RESI == ID_REG)

assign(paste0("RS", RS, "_ANTRAB_2013_SINAN"), SINAN_ANTRAB_2013)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2013_SINAN"), SINAN_ANTRAB_2013), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2013_SINAN.csv"), 
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
  
  AUX[which(AUX == i), 2] <- as.integer(SINAN_ANTRAB_2013 %>%
                                          filter(ID_MN_RESI == i,
                                                 SEM_PRI ==201301)%>%
                                          count()
  )
  
  AUX[which(AUX == i), 3] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                          filter(ID_MN_RESI == i, 
                                                 SEM_PRI ==201302) %>% 
                                          count()
  )
  
  AUX[which(AUX == i), 4] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                          filter(ID_MN_RESI == i,
                                                 SEM_PRI ==201303) %>% 
                                          count()
  )
  
  AUX[which(AUX == i),5] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                         filter(ID_MN_RESI == i,
                                                SEM_PRI ==201304) %>% 
                                         count()
  )
  
  AUX[which(AUX == i), 6] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                          filter(ID_MN_RESI == i,
                                                 SEM_PRI ==201305) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 7] <- as.integer(SINAN_ANTRAB_2013 %>%
                                          filter(ID_MN_RESI == i, 
                                                 SEM_PRI ==201306) %>%
                                          count()
  )
  
  AUX[which(AUX == i), 8] <- as.integer(SINAN_ANTRAB_2013 %>%
                                          filter(ID_MN_RESI == i, 
                                                 SEM_PRI ==201307) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 9] <- as.integer(SINAN_ANTRAB_2013 %>%
                                          filter(ID_MN_RESI == i, 
                                                 SEM_PRI ==201308) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 10] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201309) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 11] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201310) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 12] <- as.integer(SINAN_ANTRAB_2013 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201311) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 13] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201312) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 14] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201313) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 15] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201314) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 16] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201315) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 17] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201316) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 18] <- as.integer(SINAN_ANTRAB_2013 %>%
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201317) %>%  
                                           count() 
  )
  
  AUX[which(AUX == i), 19] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201318) %>%      
                                           count() 
  )
  
  AUX[which(AUX == i), 20] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201319) %>%
                                           count() 
  )
  
  AUX[which(AUX == i),  21] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                            filter(ID_MN_RESI == i, 
                                                   SEM_PRI ==201320) %>%
                                            count() 
  )
  
  AUX[which(AUX == i), 22] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201321) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 23] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201322) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 24] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201323) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 25] <- as.integer(SINAN_ANTRAB_2013 %>%
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201324) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 26] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201325) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 27] <- as.integer(SINAN_ANTRAB_2013 %>%
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201326) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 28] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201327) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 29] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201328) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 30] <- as.integer(SINAN_ANTRAB_2013 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201329) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 31] <- as.integer(SINAN_ANTRAB_2013 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201330) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 32] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201331) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 33] <- as.integer(SINAN_ANTRAB_2013 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201332) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 34] <- as.integer(SINAN_ANTRAB_2013 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201333) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 35] <- as.integer(SINAN_ANTRAB_2013 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201334) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 36] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201335) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 37] <- as.integer(SINAN_ANTRAB_2013 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201336) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 38] <- as.integer(SINAN_ANTRAB_2013 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201337) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 39] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201338) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 40] <- as.integer(SINAN_ANTRAB_2013 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201339) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 41] <- as.integer(SINAN_ANTRAB_2013 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201340) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 42] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201341) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 43] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201342) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 44] <- as.integer(SINAN_ANTRAB_2013 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201343) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 45] <- as.integer(SINAN_ANTRAB_2013 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201344) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 46] <- as.integer(SINAN_ANTRAB_2013 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201345) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 47] <- as.integer(SINAN_ANTRAB_2013 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201346) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 48] <- as.integer(SINAN_ANTRAB_2013 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201347) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 49] <- as.integer(SINAN_ANTRAB_2013 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201348) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 50] <- as.integer(SINAN_ANTRAB_2013 %>%
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201349) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 51] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201350) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 52] <- as.integer(SINAN_ANTRAB_2013 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201351) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 53] <- as.integer(SINAN_ANTRAB_2013 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201352) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 54] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201353) %>%
                                           count() 
  )
}

AUX[, 1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX[(nrow(AUX)+ 1), 2:54] <- apply(AUX[, 2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2013_SE_Notificados"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2013_SE_Notificados"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2013_SE_Notificados.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                   filter(ID_MN_RESI == i) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CS_ZONA == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CS_ZONA == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CS_ZONA == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CS_ZONA == 9 
                                                          |
                                                            is.na(CS_ZONA)) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_SEXO == "F") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_SEXO == "M") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                    filter(ID_MN_RESI == i,  
                                                           NU_IDADE_N <=3012) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                    filter(ID_MN_RESI == i,  
                                                           NU_IDADE_N > 4000 
                                                           & 
                                                             NU_IDADE_N <=4005) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           NU_IDADE_N > 4005 
                                                           & 
                                                             NU_IDADE_N <=4012) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           NU_IDADE_N > 4012 
                                                           & 
                                                             NU_IDADE_N <=4018) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 16] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           NU_IDADE_N > 4018 
                                                           & 
                                                             NU_IDADE_N <=4059) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 17] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           NU_IDADE_N > 4059) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 18] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "00") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 19] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "01" 
                                                           | 
                                                             CS_ESCOL_N == "02" 
                                                           | 
                                                             CS_ESCOL_N == "03") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 20] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "04") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 21] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "05") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 22] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "06") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 23] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "07") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 24] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "08") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 25] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "10") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 26] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "09") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 27] <- as.integer(SINAN_ANTRAB_2013 %>% 
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
  
  AUX[which(AUX$COD_IBGE == i), 28] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_GESTANT == "5") %>% 
                                                    count()
  )
  AUX[which(AUX$COD_IBGE == i), 29] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_GESTANT == "6") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 30] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_GESTANT == "9") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 31] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "01") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 32] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "2") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 33] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "3") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 34] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "4") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 35] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "5") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 36] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "9") %>% 
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:36] <- apply(AUX[, 4:36], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2013_GERAL"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2013_GERAL"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2013_GERAL.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_CONTAT == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_ARRANH == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_LAMBED == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_MORDED == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_OUTRO_ == "1") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:9] <- apply(AUX[, 4:9], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2013_EXPOSICAO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2013_EXPOSICAO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2013_EXPOSICAO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_MUCOSA == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_CABECA == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_MAOS == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_TRONCO == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_MEMBRO== "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           ANT_MEMB_1== "1") %>%   
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:10] <- apply(AUX[, 4:10], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2013_LOCALIZACAO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2013_LOCALIZACAO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2013_LOCALIZACAO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FERIMENTO == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FERIMENTO == "2") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FERIMENTO == "3") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FERIMENTO == "9") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:8] <- apply(AUX[, 4:8], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2013_FERIMENTO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2013_FERIMENTO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2013_FERIMENTO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_PROFUN == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_SUPERF == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_DILACE == "1") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:7] <- apply(AUX[, 4:7], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2013_TIPO_FERIMENTO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2013_TIPO_FERIMENTO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2013_TIPO_FERIMENTO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          PRE_EXPOS == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          POS_EXPOS == "1") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:6] <- apply(AUX[, 4:6], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2013_ANTECEDENTE_ANTR"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2013_ANTECEDENTE_ANTR"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2013_ANTECEDENTE_ANTR.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANIMAL == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANIMAL == "2") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANIMAL == "3") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANIMAL == "4") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANIMAL == "5") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           ANIMAL == "6") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           ANIMAL == "7") %>%   
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:11] <- apply(AUX[, 4:11], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2013_AGRESSOR"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2013_AGRESSOR"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2013_AGRESSOR.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CONDIC_ANI == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CONDIC_ANI == "2") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CONDIC_ANI == "3") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CONDIC_ANI == "4") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:8] <- apply(AUX[, 4:8], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2013_COND_ANIMAL_ACID"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2013_COND_ANIMAL_ACID"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2013_COND_ANIMAL_ACID.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          OBSERVACAO == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          OBSERVACAO == "2") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:6] <- apply(AUX[, 4:6], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2013_OBSERVAVEL"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2013_OBSERVAVEL"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2013_OBSERVAVEL.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRAT_ATUAL == "2") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRAT_ATUAL == "3") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRAT_ATUAL == "4") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRAT_ATUAL == "5") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRAT_ATUAL == "6") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           TRAT_ATUAL == "7") %>%   
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:10] <- apply(AUX[, 4:10], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2013_TRATAMENTO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2013_TRATAMENTO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2013_TRATAMENTO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FIM_ANIMAL == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FIM_ANIMAL == "2") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FIM_ANIMAL == "3") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FIM_ANIMAL == "4") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FIM_ANIMAL == "5") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           FIM_ANIMAL == "9") %>%   
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:10] <- apply(AUX[, 4:10], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2013_COND_FINAL_ANIMAL"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2013_COND_FINAL_ANIMAL"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2013_COND_FINAL_ANIMAL.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRA_INTERR == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRA_INTERR == "2") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:6] <- apply(AUX[, 4:6], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2013_INTERRUPCAO_TRAT"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2013_INTERRUPCAO_TRAT"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2013_INTERRUPCAO_TRAT.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRA_INDI_N == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRA_INDI_N == "2") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRA_INDI_N == "9") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:7] <- apply(AUX[, 4:7], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2013_SOROTERAPIA"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2013_SOROTERAPIA"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2013_SOROTERAPIA.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TIP_SORO == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TIP_SORO== "2") %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:6] <- apply(AUX[, 4:6], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2013_TIPO_IMUNOBIOLOGICO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2013_TIPO_IMUNOBIOLOGICO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2013_TIPO_IMUNOBIOLOGICO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRA_INFILT == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2013 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRA_INFI_1 == "1") %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:6] <- apply(AUX[, 4:6], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2013_SORO_INFILTRACAO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2013_SORO_INFILTRACAO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2013_SORO_INFILTRACAO.csv"), 
           row.names = FALSE)

rm(SINAN_ANTRAB_2013, ANTRAB2013)

################################################     2014     ################################

####Criando um objeto com a base DBF do SINAN#################

ANTRAB2014 <- read.dbf(file = "Base_de_Dados/DBF/ANTRANET2014.DBF",
                       as.is = FALSE)


####Alterando o formato da coluna ID_MN_RESI de forma que ela seja passível de vincular no for loop#################

ANTRAB2014$ID_MN_RESI <- as.numeric(as.character(ANTRAB2014$ID_MN_RESI))

#####################################################################################################################
#################  Realizando contagem dos casos do Paraná para realizar a série histórica do Estado  ###############

PR_ANTRAB_Serie_Historica[3, 2] <- as.integer(ANTRAB2014 %>% 
                                                count()
)
#####Filtrando os dados da Base DBF do SINAN com os dados de notificação somente da 22RS###########

SINAN_ANTRAB_2014 <- ANTRAB2014 %>% 
  filter(ID_REGIONA == ID_REG | ID_RG_RESI == ID_REG)

assign(paste0("RS", RS, "_ANTRAB_2014_SINAN"), SINAN_ANTRAB_2014)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2014_SINAN"), SINAN_ANTRAB_2014), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2014_SINAN.csv"), 
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
  
  AUX[which(AUX == i), 2] <- as.integer(SINAN_ANTRAB_2014 %>%
                                          filter(ID_MN_RESI == i,
                                                 SEM_PRI ==201401)%>%
                                          count()
  )
  
  AUX[which(AUX == i), 3] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                          filter(ID_MN_RESI == i, 
                                                 SEM_PRI ==201402) %>% 
                                          count()
  )
  
  AUX[which(AUX == i), 4] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                          filter(ID_MN_RESI == i,
                                                 SEM_PRI ==201403) %>% 
                                          count()
  )
  
  AUX[which(AUX == i),5] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                         filter(ID_MN_RESI == i,
                                                SEM_PRI ==201404) %>% 
                                         count()
  )
  
  AUX[which(AUX == i), 6] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                          filter(ID_MN_RESI == i,
                                                 SEM_PRI ==201405) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 7] <- as.integer(SINAN_ANTRAB_2014 %>%
                                          filter(ID_MN_RESI == i, 
                                                 SEM_PRI ==201406) %>%
                                          count()
  )
  
  AUX[which(AUX == i), 8] <- as.integer(SINAN_ANTRAB_2014 %>%
                                          filter(ID_MN_RESI == i, 
                                                 SEM_PRI ==201407) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 9] <- as.integer(SINAN_ANTRAB_2014 %>%
                                          filter(ID_MN_RESI == i, 
                                                 SEM_PRI ==201408) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 10] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201409) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 11] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201410) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 12] <- as.integer(SINAN_ANTRAB_2014 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201411) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 13] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201412) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 14] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201413) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 15] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201414) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 16] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201415) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 17] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201416) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 18] <- as.integer(SINAN_ANTRAB_2014 %>%
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201417) %>%  
                                           count() 
  )
  
  AUX[which(AUX == i), 19] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201418) %>%      
                                           count() 
  )
  
  AUX[which(AUX == i), 20] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201419) %>%
                                           count() 
  )
  
  AUX[which(AUX == i),  21] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                            filter(ID_MN_RESI == i, 
                                                   SEM_PRI ==201420) %>%
                                            count() 
  )
  
  AUX[which(AUX == i), 22] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201421) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 23] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201422) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 24] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201423) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 25] <- as.integer(SINAN_ANTRAB_2014 %>%
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201424) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 26] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201425) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 27] <- as.integer(SINAN_ANTRAB_2014 %>%
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201426) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 28] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201427) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 29] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201428) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 30] <- as.integer(SINAN_ANTRAB_2014 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201429) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 31] <- as.integer(SINAN_ANTRAB_2014 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201430) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 32] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201431) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 33] <- as.integer(SINAN_ANTRAB_2014 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201432) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 34] <- as.integer(SINAN_ANTRAB_2014 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201433) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 35] <- as.integer(SINAN_ANTRAB_2014 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201434) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 36] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201435) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 37] <- as.integer(SINAN_ANTRAB_2014 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201436) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 38] <- as.integer(SINAN_ANTRAB_2014 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201437) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 39] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201438) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 40] <- as.integer(SINAN_ANTRAB_2014 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201439) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 41] <- as.integer(SINAN_ANTRAB_2014 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201440) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 42] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201441) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 43] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201442) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 44] <- as.integer(SINAN_ANTRAB_2014 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201443) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 45] <- as.integer(SINAN_ANTRAB_2014 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201444) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 46] <- as.integer(SINAN_ANTRAB_2014 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201445) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 47] <- as.integer(SINAN_ANTRAB_2014 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201446) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 48] <- as.integer(SINAN_ANTRAB_2014 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201447) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 49] <- as.integer(SINAN_ANTRAB_2014 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201448) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 50] <- as.integer(SINAN_ANTRAB_2014 %>%
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201449) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 51] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201450) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 52] <- as.integer(SINAN_ANTRAB_2014 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201451) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 53] <- as.integer(SINAN_ANTRAB_2014 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201452) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 54] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201453) %>%
                                           count() 
  )
}

AUX[, 1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX[(nrow(AUX)+ 1), 2:54] <- apply(AUX[, 2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2014_SE_Notificados"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2014_SE_Notificados"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2014_SE_Notificados.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                   filter(ID_MN_RESI == i) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CS_ZONA == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CS_ZONA == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CS_ZONA == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CS_ZONA == 9 
                                                          |
                                                            is.na(CS_ZONA)) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_SEXO == "F") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_SEXO == "M") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                    filter(ID_MN_RESI == i,  
                                                           NU_IDADE_N <=3012) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                    filter(ID_MN_RESI == i,  
                                                           NU_IDADE_N > 4000 
                                                           & 
                                                             NU_IDADE_N <=4005) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           NU_IDADE_N > 4005 
                                                           & 
                                                             NU_IDADE_N <=4012) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           NU_IDADE_N > 4012 
                                                           & 
                                                             NU_IDADE_N <=4018) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 16] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           NU_IDADE_N > 4018 
                                                           & 
                                                             NU_IDADE_N <=4059) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 17] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           NU_IDADE_N > 4059) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 18] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "00") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 19] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "01" 
                                                           | 
                                                             CS_ESCOL_N == "02" 
                                                           | 
                                                             CS_ESCOL_N == "03") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 20] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "04") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 21] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "05") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 22] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "06") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 23] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "07") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 24] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "08") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 25] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "10") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 26] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "09") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 27] <- as.integer(SINAN_ANTRAB_2014 %>% 
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
  
  AUX[which(AUX$COD_IBGE == i), 28] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_GESTANT == "5") %>% 
                                                    count()
  )
  AUX[which(AUX$COD_IBGE == i), 29] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_GESTANT == "6") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 30] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_GESTANT == "9") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 31] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "01") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 32] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "2") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 33] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "3") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 34] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "4") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 35] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "5") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 36] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "9") %>% 
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:36] <- apply(AUX[, 4:36], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2014_GERAL"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2014_GERAL"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2014_GERAL.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_CONTAT == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_ARRANH == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_LAMBED == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_MORDED == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_OUTRO_ == "1") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:9] <- apply(AUX[, 4:9], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2014_EXPOSICAO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2014_EXPOSICAO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2014_EXPOSICAO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_MUCOSA == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_CABECA == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_MAOS == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_TRONCO == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_MEMBRO== "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           ANT_MEMB_1== "1") %>%   
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:10] <- apply(AUX[, 4:10], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2014_LOCALIZACAO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2014_LOCALIZACAO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2014_LOCALIZACAO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FERIMENTO == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FERIMENTO == "2") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FERIMENTO == "3") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FERIMENTO == "9") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:8] <- apply(AUX[, 4:8], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2014_FERIMENTO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2014_FERIMENTO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2014_FERIMENTO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_PROFUN == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_SUPERF == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_DILACE == "1") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:7] <- apply(AUX[, 4:7], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2014_TIPO_FERIMENTO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2014_TIPO_FERIMENTO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2014_TIPO_FERIMENTO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          PRE_EXPOS == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          POS_EXPOS == "1") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:6] <- apply(AUX[, 4:6], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2014_ANTECEDENTE_ANTR"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2014_ANTECEDENTE_ANTR"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2014_ANTECEDENTE_ANTR.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANIMAL == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANIMAL == "2") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANIMAL == "3") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANIMAL == "4") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANIMAL == "5") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           ANIMAL == "6") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           ANIMAL == "7") %>%   
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:11] <- apply(AUX[, 4:11], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2014_AGRESSOR"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2014_AGRESSOR"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2014_AGRESSOR.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CONDIC_ANI == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CONDIC_ANI == "2") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CONDIC_ANI == "3") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CONDIC_ANI == "4") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:8] <- apply(AUX[, 4:8], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2014_COND_ANIMAL_ACID"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2014_COND_ANIMAL_ACID"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2014_COND_ANIMAL_ACID.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          OBSERVACAO == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          OBSERVACAO == "2") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:6] <- apply(AUX[, 4:6], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2014_OBSERVAVEL"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2014_OBSERVAVEL"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2014_OBSERVAVEL.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRAT_ATUAL == "2") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRAT_ATUAL == "3") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRAT_ATUAL == "4") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRAT_ATUAL == "5") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRAT_ATUAL == "6") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           TRAT_ATUAL == "7") %>%   
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:10] <- apply(AUX[, 4:10], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2014_TRATAMENTO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2014_TRATAMENTO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2014_TRATAMENTO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FIM_ANIMAL == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FIM_ANIMAL == "2") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FIM_ANIMAL == "3") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FIM_ANIMAL == "4") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FIM_ANIMAL == "5") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           FIM_ANIMAL == "9") %>%   
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:10] <- apply(AUX[, 4:10], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2014_COND_FINAL_ANIMAL"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2014_COND_FINAL_ANIMAL"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2014_COND_FINAL_ANIMAL.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRA_INTERR == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRA_INTERR == "2") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:6] <- apply(AUX[, 4:6], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2014_INTERRUPCAO_TRAT"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2014_INTERRUPCAO_TRAT"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2014_INTERRUPCAO_TRAT.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRA_INDI_N == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRA_INDI_N == "2") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRA_INDI_N == "9") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:7] <- apply(AUX[, 4:7], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2014_SOROTERAPIA"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2014_SOROTERAPIA"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2014_SOROTERAPIA.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TIP_SORO == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TIP_SORO== "2") %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:6] <- apply(AUX[, 4:6], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2014_TIPO_IMUNOBIOLOGICO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2014_TIPO_IMUNOBIOLOGICO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2014_TIPO_IMUNOBIOLOGICO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRA_INFILT == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2014 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRA_INFI_1 == "1") %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:6] <- apply(AUX[, 4:6], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2014_SORO_INFILTRACAO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2014_SORO_INFILTRACAO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2014_SORO_INFILTRACAO.csv"), 
           row.names = FALSE)

rm(SINAN_ANTRAB_2014, ANTRAB2014)

############################     2015     ############################

####Criando um objeto com a base DBF do SINAN#################

ANTRAB2015 <- read.dbf(file = "Base_de_Dados/DBF/ANTRANET2015.DBF",
                       as.is = FALSE)


####Alterando o formato da coluna ID_MN_RESI de forma que ela seja passível de vincular no for loop#################

ANTRAB2015$ID_MN_RESI <- as.numeric(as.character(ANTRAB2015$ID_MN_RESI))

#####################################################################################################################
#################  Realizando contagem dos casos do Paraná para realizar a série histórica do Estado  ###############

PR_ANTRAB_Serie_Historica[4, 2] <- as.integer(ANTRAB2015 %>% 
                                                count()
)
#####Filtrando os dados da Base DBF do SINAN com os dados de notificação somente da 22RS###########

SINAN_ANTRAB_2015 <- ANTRAB2015 %>% 
  filter(ID_REGIONA == ID_REG | ID_RG_RESI == ID_REG)

assign(paste0("RS", RS, "_ANTRAB_2015_SINAN"), SINAN_ANTRAB_2015)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2015_SINAN"), SINAN_ANTRAB_2015), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2015_SINAN.csv"), 
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
  
  AUX[which(AUX == i), 2] <- as.integer(SINAN_ANTRAB_2015 %>%
                                          filter(ID_MN_RESI == i,
                                                 SEM_PRI ==201501)%>%
                                          count()
  )
  
  AUX[which(AUX == i), 3] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                          filter(ID_MN_RESI == i, 
                                                 SEM_PRI ==201502) %>% 
                                          count()
  )
  
  AUX[which(AUX == i), 4] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                          filter(ID_MN_RESI == i,
                                                 SEM_PRI ==201503) %>% 
                                          count()
  )
  
  AUX[which(AUX == i),5] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                         filter(ID_MN_RESI == i,
                                                SEM_PRI ==201504) %>% 
                                         count()
  )
  
  AUX[which(AUX == i), 6] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                          filter(ID_MN_RESI == i,
                                                 SEM_PRI ==201505) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 7] <- as.integer(SINAN_ANTRAB_2015 %>%
                                          filter(ID_MN_RESI == i, 
                                                 SEM_PRI ==201506) %>%
                                          count()
  )
  
  AUX[which(AUX == i), 8] <- as.integer(SINAN_ANTRAB_2015 %>%
                                          filter(ID_MN_RESI == i, 
                                                 SEM_PRI ==201507) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 9] <- as.integer(SINAN_ANTRAB_2015 %>%
                                          filter(ID_MN_RESI == i, 
                                                 SEM_PRI ==201508) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 10] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201509) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 11] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201510) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 12] <- as.integer(SINAN_ANTRAB_2015 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201511) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 13] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201512) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 14] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201513) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 15] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201514) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 16] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201515) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 17] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201516) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 18] <- as.integer(SINAN_ANTRAB_2015 %>%
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201517) %>%  
                                           count() 
  )
  
  AUX[which(AUX == i), 19] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201518) %>%      
                                           count() 
  )
  
  AUX[which(AUX == i), 20] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201519) %>%
                                           count() 
  )
  
  AUX[which(AUX == i),  21] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                            filter(ID_MN_RESI == i, 
                                                   SEM_PRI ==201520) %>%
                                            count() 
  )
  
  AUX[which(AUX == i), 22] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201521) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 23] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201522) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 24] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201523) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 25] <- as.integer(SINAN_ANTRAB_2015 %>%
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201524) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 26] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201525) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 27] <- as.integer(SINAN_ANTRAB_2015 %>%
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201526) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 28] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201527) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 29] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201528) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 30] <- as.integer(SINAN_ANTRAB_2015 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201529) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 31] <- as.integer(SINAN_ANTRAB_2015 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201530) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 32] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201531) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 33] <- as.integer(SINAN_ANTRAB_2015 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201532) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 34] <- as.integer(SINAN_ANTRAB_2015 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201533) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 35] <- as.integer(SINAN_ANTRAB_2015 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201534) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 36] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201535) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 37] <- as.integer(SINAN_ANTRAB_2015 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201536) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 38] <- as.integer(SINAN_ANTRAB_2015 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201537) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 39] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201538) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 40] <- as.integer(SINAN_ANTRAB_2015 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201539) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 41] <- as.integer(SINAN_ANTRAB_2015 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201540) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 42] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201541) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 43] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201542) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 44] <- as.integer(SINAN_ANTRAB_2015 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201543) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 45] <- as.integer(SINAN_ANTRAB_2015 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201544) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 46] <- as.integer(SINAN_ANTRAB_2015 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201545) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 47] <- as.integer(SINAN_ANTRAB_2015 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201546) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 48] <- as.integer(SINAN_ANTRAB_2015 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201547) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 49] <- as.integer(SINAN_ANTRAB_2015 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201548) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 50] <- as.integer(SINAN_ANTRAB_2015 %>%
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201549) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 51] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201550) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 52] <- as.integer(SINAN_ANTRAB_2015 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201551) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 53] <- as.integer(SINAN_ANTRAB_2015 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201552) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 54] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201553) %>%
                                           count() 
  )
}

AUX[, 1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX[(nrow(AUX)+ 1), 2:54] <- apply(AUX[, 2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2015_SE_Notificados"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2015_SE_Notificados"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2015_SE_Notificados.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                   filter(ID_MN_RESI == i) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CS_ZONA == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CS_ZONA == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CS_ZONA == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CS_ZONA == 9 
                                                          |
                                                            is.na(CS_ZONA)) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_SEXO == "F") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_SEXO == "M") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                    filter(ID_MN_RESI == i,  
                                                           NU_IDADE_N <=3012) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                    filter(ID_MN_RESI == i,  
                                                           NU_IDADE_N > 4000 
                                                           & 
                                                             NU_IDADE_N <=4005) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           NU_IDADE_N > 4005 
                                                           & 
                                                             NU_IDADE_N <=4012) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           NU_IDADE_N > 4012 
                                                           & 
                                                             NU_IDADE_N <=4018) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 16] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           NU_IDADE_N > 4018 
                                                           & 
                                                             NU_IDADE_N <=4059) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 17] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           NU_IDADE_N > 4059) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 18] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "00") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 19] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "01" 
                                                           | 
                                                             CS_ESCOL_N == "02" 
                                                           | 
                                                             CS_ESCOL_N == "03") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 20] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "04") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 21] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "05") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 22] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "06") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 23] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "07") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 24] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "08") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 25] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "10") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 26] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "09") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 27] <- as.integer(SINAN_ANTRAB_2015 %>% 
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
  
  AUX[which(AUX$COD_IBGE == i), 28] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_GESTANT == "5") %>% 
                                                    count()
  )
  AUX[which(AUX$COD_IBGE == i), 29] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_GESTANT == "6") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 30] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_GESTANT == "9") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 31] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "01") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 32] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "2") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 33] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "3") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 34] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "4") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 35] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "5") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 36] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "9") %>% 
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:36] <- apply(AUX[, 4:36], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2015_GERAL"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2015_GERAL"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2015_GERAL.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_CONTAT == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_ARRANH == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_LAMBED == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_MORDED == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_OUTRO_ == "1") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:9] <- apply(AUX[, 4:9], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2015_EXPOSICAO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2015_EXPOSICAO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2015_EXPOSICAO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_MUCOSA == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_CABECA == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_MAOS == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_TRONCO == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_MEMBRO== "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           ANT_MEMB_1== "1") %>%   
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:10] <- apply(AUX[, 4:10], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2015_LOCALIZACAO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2015_LOCALIZACAO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2015_LOCALIZACAO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FERIMENTO == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FERIMENTO == "2") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FERIMENTO == "3") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FERIMENTO == "9") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:8] <- apply(AUX[, 4:8], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2015_FERIMENTO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2015_FERIMENTO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2015_FERIMENTO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_PROFUN == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_SUPERF == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_DILACE == "1") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:7] <- apply(AUX[, 4:7], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2015_TIPO_FERIMENTO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2015_TIPO_FERIMENTO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2015_TIPO_FERIMENTO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          PRE_EXPOS == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          POS_EXPOS == "1") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:6] <- apply(AUX[, 4:6], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2015_ANTECEDENTE_ANTR"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2015_ANTECEDENTE_ANTR"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2015_ANTECEDENTE_ANTR.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANIMAL == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANIMAL == "2") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANIMAL == "3") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANIMAL == "4") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANIMAL == "5") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           ANIMAL == "6") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           ANIMAL == "7") %>%   
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:11] <- apply(AUX[, 4:11], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2015_AGRESSOR"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2015_AGRESSOR"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2015_AGRESSOR.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CONDIC_ANI == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CONDIC_ANI == "2") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CONDIC_ANI == "3") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CONDIC_ANI == "4") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:8] <- apply(AUX[, 4:8], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2015_COND_ANIMAL_ACID"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2015_COND_ANIMAL_ACID"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2015_COND_ANIMAL_ACID.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          OBSERVACAO == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          OBSERVACAO == "2") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:6] <- apply(AUX[, 4:6], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2015_OBSERVAVEL"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2015_OBSERVAVEL"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2015_OBSERVAVEL.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRAT_ATUAL == "2") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRAT_ATUAL == "3") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRAT_ATUAL == "4") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRAT_ATUAL == "5") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRAT_ATUAL == "6") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           TRAT_ATUAL == "7") %>%   
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:10] <- apply(AUX[, 4:10], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2015_TRATAMENTO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2015_TRATAMENTO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2015_TRATAMENTO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FIM_ANIMAL == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FIM_ANIMAL == "2") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FIM_ANIMAL == "3") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FIM_ANIMAL == "4") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FIM_ANIMAL == "5") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           FIM_ANIMAL == "9") %>%   
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:10] <- apply(AUX[, 4:10], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2015_COND_FINAL_ANIMAL"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2015_COND_FINAL_ANIMAL"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2015_COND_FINAL_ANIMAL.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRA_INTERR == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRA_INTERR == "2") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:6] <- apply(AUX[, 4:6], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2015_INTERRUPCAO_TRAT"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2015_INTERRUPCAO_TRAT"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2015_INTERRUPCAO_TRAT.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRA_INDI_N == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRA_INDI_N == "2") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRA_INDI_N == "9") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:7] <- apply(AUX[, 4:7], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2015_SOROTERAPIA"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2015_SOROTERAPIA"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2015_SOROTERAPIA.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TIP_SORO == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TIP_SORO== "2") %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:6] <- apply(AUX[, 4:6], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2015_TIPO_IMUNOBIOLOGICO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2015_TIPO_IMUNOBIOLOGICO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2015_TIPO_IMUNOBIOLOGICO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRA_INFILT == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2015 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRA_INFI_1 == "1") %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:6] <- apply(AUX[, 4:6], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2015_SORO_INFILTRACAO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2015_SORO_INFILTRACAO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2015_SORO_INFILTRACAO.csv"), 
           row.names = FALSE)

rm(SINAN_ANTRAB_2015, ANTRAB2015)

###################       2016      #########################################

####Criando um objeto com a base DBF do SINAN#################

ANTRAB2016 <- read.dbf(file = "Base_de_Dados/DBF/ANTRANET2016.DBF",
                       as.is = FALSE)


####Alterando o formato da coluna ID_MN_RESI de forma que ela seja passível de vincular no for loop#################

ANTRAB2016$ID_MN_RESI <- as.numeric(as.character(ANTRAB2016$ID_MN_RESI))

#####################################################################################################################
#################  Realizando contagem dos casos do Paraná para realizar a série histórica do Estado  ###############

PR_ANTRAB_Serie_Historica[5, 2] <- as.integer(ANTRAB2016 %>% 
                                                count()
)
#####Filtrando os dados da Base DBF do SINAN com os dados de notificação somente da 22RS###########

SINAN_ANTRAB_2016 <- ANTRAB2016 %>% 
  filter(ID_REGIONA == ID_REG | ID_RG_RESI == ID_REG)

assign(paste0("RS", RS, "_ANTRAB_2016_SINAN"), SINAN_ANTRAB_2016)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2016_SINAN"), SINAN_ANTRAB_2016), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2016_SINAN.csv"), 
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
  
  AUX[which(AUX == i), 2] <- as.integer(SINAN_ANTRAB_2016 %>%
                                          filter(ID_MN_RESI == i,
                                                 SEM_PRI ==201601)%>%
                                          count()
  )
  
  AUX[which(AUX == i), 3] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                          filter(ID_MN_RESI == i, 
                                                 SEM_PRI ==201602) %>% 
                                          count()
  )
  
  AUX[which(AUX == i), 4] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                          filter(ID_MN_RESI == i,
                                                 SEM_PRI ==201603) %>% 
                                          count()
  )
  
  AUX[which(AUX == i),5] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                         filter(ID_MN_RESI == i,
                                                SEM_PRI ==201604) %>% 
                                         count()
  )
  
  AUX[which(AUX == i), 6] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                          filter(ID_MN_RESI == i,
                                                 SEM_PRI ==201605) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 7] <- as.integer(SINAN_ANTRAB_2016 %>%
                                          filter(ID_MN_RESI == i, 
                                                 SEM_PRI ==201606) %>%
                                          count()
  )
  
  AUX[which(AUX == i), 8] <- as.integer(SINAN_ANTRAB_2016 %>%
                                          filter(ID_MN_RESI == i, 
                                                 SEM_PRI ==201607) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 9] <- as.integer(SINAN_ANTRAB_2016 %>%
                                          filter(ID_MN_RESI == i, 
                                                 SEM_PRI ==201608) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 10] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201609) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 11] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201610) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 12] <- as.integer(SINAN_ANTRAB_2016 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201611) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 13] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201612) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 14] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201613) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 15] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201614) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 16] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201615) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 17] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201616) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 18] <- as.integer(SINAN_ANTRAB_2016 %>%
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201617) %>%  
                                           count() 
  )
  
  AUX[which(AUX == i), 19] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201618) %>%      
                                           count() 
  )
  
  AUX[which(AUX == i), 20] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201619) %>%
                                           count() 
  )
  
  AUX[which(AUX == i),  21] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                            filter(ID_MN_RESI == i, 
                                                   SEM_PRI ==201620) %>%
                                            count() 
  )
  
  AUX[which(AUX == i), 22] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201621) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 23] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201622) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 24] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201623) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 25] <- as.integer(SINAN_ANTRAB_2016 %>%
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201624) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 26] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201625) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 27] <- as.integer(SINAN_ANTRAB_2016 %>%
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201626) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 28] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201627) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 29] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201628) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 30] <- as.integer(SINAN_ANTRAB_2016 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201629) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 31] <- as.integer(SINAN_ANTRAB_2016 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201630) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 32] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201631) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 33] <- as.integer(SINAN_ANTRAB_2016 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201632) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 34] <- as.integer(SINAN_ANTRAB_2016 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201633) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 35] <- as.integer(SINAN_ANTRAB_2016 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201634) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 36] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201635) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 37] <- as.integer(SINAN_ANTRAB_2016 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201636) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 38] <- as.integer(SINAN_ANTRAB_2016 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201637) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 39] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201638) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 40] <- as.integer(SINAN_ANTRAB_2016 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201639) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 41] <- as.integer(SINAN_ANTRAB_2016 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201640) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 42] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201641) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 43] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201642) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 44] <- as.integer(SINAN_ANTRAB_2016 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201643) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 45] <- as.integer(SINAN_ANTRAB_2016 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201644) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 46] <- as.integer(SINAN_ANTRAB_2016 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201645) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 47] <- as.integer(SINAN_ANTRAB_2016 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201646) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 48] <- as.integer(SINAN_ANTRAB_2016 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201647) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 49] <- as.integer(SINAN_ANTRAB_2016 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201648) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 50] <- as.integer(SINAN_ANTRAB_2016 %>%
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201649) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 51] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201650) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 52] <- as.integer(SINAN_ANTRAB_2016 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201651) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 53] <- as.integer(SINAN_ANTRAB_2016 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201652) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 54] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201653) %>%
                                           count() 
  )
}

AUX[, 1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX[(nrow(AUX)+ 1), 2:54] <- apply(AUX[, 2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2016_SE_Notificados"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2016_SE_Notificados"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2016_SE_Notificados.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                   filter(ID_MN_RESI == i) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CS_ZONA == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CS_ZONA == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CS_ZONA == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CS_ZONA == 9 
                                                          |
                                                            is.na(CS_ZONA)) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_SEXO == "F") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_SEXO == "M") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                    filter(ID_MN_RESI == i,  
                                                           NU_IDADE_N <=3012) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                    filter(ID_MN_RESI == i,  
                                                           NU_IDADE_N > 4000 
                                                           & 
                                                             NU_IDADE_N <=4005) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           NU_IDADE_N > 4005 
                                                           & 
                                                             NU_IDADE_N <=4012) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           NU_IDADE_N > 4012 
                                                           & 
                                                             NU_IDADE_N <=4018) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 16] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           NU_IDADE_N > 4018 
                                                           & 
                                                             NU_IDADE_N <=4059) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 17] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           NU_IDADE_N > 4059) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 18] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "00") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 19] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "01" 
                                                           | 
                                                             CS_ESCOL_N == "02" 
                                                           | 
                                                             CS_ESCOL_N == "03") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 20] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "04") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 21] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "05") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 22] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "06") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 23] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "07") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 24] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "08") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 25] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "10") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 26] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "09") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 27] <- as.integer(SINAN_ANTRAB_2016 %>% 
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
  
  AUX[which(AUX$COD_IBGE == i), 28] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_GESTANT == "5") %>% 
                                                    count()
  )
  AUX[which(AUX$COD_IBGE == i), 29] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_GESTANT == "6") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 30] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_GESTANT == "9") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 31] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "01") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 32] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "2") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 33] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "3") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 34] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "4") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 35] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "5") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 36] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "9") %>% 
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:36] <- apply(AUX[, 4:36], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2016_GERAL"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2016_GERAL"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2016_GERAL.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_CONTAT == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_ARRANH == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_LAMBED == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_MORDED == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_OUTRO_ == "1") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:9] <- apply(AUX[, 4:9], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2016_EXPOSICAO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2016_EXPOSICAO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2016_EXPOSICAO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_MUCOSA == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_CABECA == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_MAOS == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_TRONCO == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_MEMBRO== "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           ANT_MEMB_1== "1") %>%   
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:10] <- apply(AUX[, 4:10], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2016_LOCALIZACAO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2016_LOCALIZACAO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2016_LOCALIZACAO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FERIMENTO == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FERIMENTO == "2") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FERIMENTO == "3") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FERIMENTO == "9") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:8] <- apply(AUX[, 4:8], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2016_FERIMENTO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2016_FERIMENTO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2016_FERIMENTO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_PROFUN == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_SUPERF == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_DILACE == "1") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:7] <- apply(AUX[, 4:7], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2016_TIPO_FERIMENTO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2016_TIPO_FERIMENTO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2016_TIPO_FERIMENTO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          PRE_EXPOS == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          POS_EXPOS == "1") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:6] <- apply(AUX[, 4:6], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2016_ANTECEDENTE_ANTR"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2016_ANTECEDENTE_ANTR"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2016_ANTECEDENTE_ANTR.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANIMAL == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANIMAL == "2") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANIMAL == "3") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANIMAL == "4") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANIMAL == "5") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           ANIMAL == "6") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           ANIMAL == "7") %>%   
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:11] <- apply(AUX[, 4:11], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2016_AGRESSOR"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2016_AGRESSOR"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2016_AGRESSOR.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CONDIC_ANI == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CONDIC_ANI == "2") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CONDIC_ANI == "3") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CONDIC_ANI == "4") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:8] <- apply(AUX[, 4:8], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2016_COND_ANIMAL_ACID"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2016_COND_ANIMAL_ACID"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2016_COND_ANIMAL_ACID.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          OBSERVACAO == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          OBSERVACAO == "2") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:6] <- apply(AUX[, 4:6], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2016_OBSERVAVEL"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2016_OBSERVAVEL"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2016_OBSERVAVEL.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRAT_ATUAL == "2") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRAT_ATUAL == "3") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRAT_ATUAL == "4") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRAT_ATUAL == "5") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRAT_ATUAL == "6") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           TRAT_ATUAL == "7") %>%   
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:10] <- apply(AUX[, 4:10], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2016_TRATAMENTO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2016_TRATAMENTO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2016_TRATAMENTO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FIM_ANIMAL == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FIM_ANIMAL == "2") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FIM_ANIMAL == "3") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FIM_ANIMAL == "4") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FIM_ANIMAL == "5") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           FIM_ANIMAL == "9") %>%   
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:10] <- apply(AUX[, 4:10], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2016_COND_FINAL_ANIMAL"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2016_COND_FINAL_ANIMAL"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2016_COND_FINAL_ANIMAL.csv"), 
           row.names = FALSE)

###Tabela Interrupção de tratamento (campo 49 do SINAN)####

SINAN_ANTRAB_2016_INTERRUPCAO_TRAT <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

SINAN_ANTRAB_2016_INTERRUPCAO_TRAT$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

SINAN_ANTRAB_2016_INTERRUPCAO_TRAT$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

SINAN_ANTRAB_2016_INTERRUPCAO_TRAT$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

SINAN_ANTRAB_2016_INTERRUPCAO_TRAT <- SINAN_ANTRAB_2016_INTERRUPCAO_TRAT[,c(4, 1, 2, 3)]

SINAN_ANTRAB_2016_INTERRUPCAO_TRAT$Sim <- NA

SINAN_ANTRAB_2016_INTERRUPCAO_TRAT$Nao <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  SINAN_ANTRAB_2016_INTERRUPCAO_TRAT[which(SINAN_ANTRAB_2016_INTERRUPCAO_TRAT$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                                                                                 filter(ID_MN_RESI == i,
                                                                                                                        TRA_INTERR == "1") %>%   
                                                                                                                 count()
  )    
  
  SINAN_ANTRAB_2016_INTERRUPCAO_TRAT[which(SINAN_ANTRAB_2016_INTERRUPCAO_TRAT$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                                                                                 filter(ID_MN_RESI == i,
                                                                                                                        TRA_INTERR == "2") %>%   
                                                                                                                 count()
  )
}

SINAN_ANTRAB_2016_INTERRUPCAO_TRAT[(nrow(SINAN_ANTRAB_2016_INTERRUPCAO_TRAT) +1), 4:6] <- apply(SINAN_ANTRAB_2016_INTERRUPCAO_TRAT[, 4:6], 2, sum)
SINAN_ANTRAB_2016_INTERRUPCAO_TRAT[nrow(SINAN_ANTRAB_2016_INTERRUPCAO_TRAT), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2016_INTERRUPCAO_TRAT"), SINAN_ANTRAB_2016_INTERRUPCAO_TRAT)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2016_INTERRUPCAO_TRAT"), SINAN_ANTRAB_2016_INTERRUPCAO_TRAT), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2016_INTERRUPCAO_TRAT.csv"), 
           row.names = FALSE)

rm(SINAN_ANTRAB_2016_INTERRUPCAO_TRAT)

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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRA_INDI_N == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRA_INDI_N == "2") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRA_INDI_N == "9") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:7] <- apply(AUX[, 4:7], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2016_SOROTERAPIA"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2016_SOROTERAPIA"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2016_SOROTERAPIA.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TIP_SORO == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TIP_SORO== "2") %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:6] <- apply(AUX[, 4:6], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2016_TIPO_IMUNOBIOLOGICO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2016_TIPO_IMUNOBIOLOGICO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2016_TIPO_IMUNOBIOLOGICO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRA_INFILT == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2016 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRA_INFI_1 == "1") %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:6] <- apply(AUX[, 4:6], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2016_SORO_INFILTRACAO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2016_SORO_INFILTRACAO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2016_SORO_INFILTRACAO.csv"), 
           row.names = FALSE)

rm(SINAN_ANTRAB_2016, ANTRAB2016)
##################       2017      #########################################

####Criando um objeto com a base DBF do SINAN#################

ANTRAB2017 <- read.dbf(file = "Base_de_Dados/DBF/ANTRANET2017.DBF",
                       as.is = FALSE)


####Alterando o formato da coluna ID_MN_RESI de forma que ela seja passível de vincular no for loop#################

ANTRAB2017$ID_MN_RESI <- as.numeric(as.character(ANTRAB2017$ID_MN_RESI))

#####################################################################################################################
#################  Realizando contagem dos casos do Paraná para realizar a série histórica do Estado  ###############

PR_ANTRAB_Serie_Historica[6, 2] <- as.integer(ANTRAB2017 %>% 
                                                count()
)
#####Filtrando os dados da Base DBF do SINAN com os dados de notificação somente da 22RS###########

SINAN_ANTRAB_2017 <- ANTRAB2017 %>% 
  filter(ID_REGIONA == ID_REG | ID_RG_RESI == ID_REG)

assign(paste0("RS", RS, "_ANTRAB_2017_SINAN"), SINAN_ANTRAB_2017)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2017_SINAN"), SINAN_ANTRAB_2017), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2017_SINAN.csv"), 
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
  
  AUX[which(AUX == i), 2] <- as.integer(SINAN_ANTRAB_2017 %>%
                                          filter(ID_MN_RESI == i,
                                                 SEM_PRI ==201701)%>%
                                          count()
  )
  
  AUX[which(AUX == i), 3] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                          filter(ID_MN_RESI == i, 
                                                 SEM_PRI ==201702) %>% 
                                          count()
  )
  
  AUX[which(AUX == i), 4] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                          filter(ID_MN_RESI == i,
                                                 SEM_PRI ==201703) %>% 
                                          count()
  )
  
  AUX[which(AUX == i),5] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                         filter(ID_MN_RESI == i,
                                                SEM_PRI ==201704) %>% 
                                         count()
  )
  
  AUX[which(AUX == i), 6] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                          filter(ID_MN_RESI == i,
                                                 SEM_PRI ==201705) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 7] <- as.integer(SINAN_ANTRAB_2017 %>%
                                          filter(ID_MN_RESI == i, 
                                                 SEM_PRI ==201706) %>%
                                          count()
  )
  
  AUX[which(AUX == i), 8] <- as.integer(SINAN_ANTRAB_2017 %>%
                                          filter(ID_MN_RESI == i, 
                                                 SEM_PRI ==201707) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 9] <- as.integer(SINAN_ANTRAB_2017 %>%
                                          filter(ID_MN_RESI == i, 
                                                 SEM_PRI ==201708) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 10] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201709) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 11] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201710) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 12] <- as.integer(SINAN_ANTRAB_2017 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201711) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 13] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201712) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 14] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201713) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 15] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201714) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 16] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201715) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 17] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201716) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 18] <- as.integer(SINAN_ANTRAB_2017 %>%
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201717) %>%  
                                           count() 
  )
  
  AUX[which(AUX == i), 19] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201718) %>%      
                                           count() 
  )
  
  AUX[which(AUX == i), 20] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201719) %>%
                                           count() 
  )
  
  AUX[which(AUX == i),  21] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                            filter(ID_MN_RESI == i, 
                                                   SEM_PRI ==201720) %>%
                                            count() 
  )
  
  AUX[which(AUX == i), 22] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201721) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 23] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201722) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 24] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201723) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 25] <- as.integer(SINAN_ANTRAB_2017 %>%
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201724) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 26] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201725) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 27] <- as.integer(SINAN_ANTRAB_2017 %>%
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201726) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 28] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201727) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 29] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201728) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 30] <- as.integer(SINAN_ANTRAB_2017 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201729) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 31] <- as.integer(SINAN_ANTRAB_2017 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201730) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 32] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201731) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 33] <- as.integer(SINAN_ANTRAB_2017 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201732) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 34] <- as.integer(SINAN_ANTRAB_2017 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201733) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 35] <- as.integer(SINAN_ANTRAB_2017 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201734) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 36] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201735) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 37] <- as.integer(SINAN_ANTRAB_2017 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201736) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 38] <- as.integer(SINAN_ANTRAB_2017 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201737) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 39] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201738) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 40] <- as.integer(SINAN_ANTRAB_2017 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201739) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 41] <- as.integer(SINAN_ANTRAB_2017 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201740) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 42] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201741) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 43] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201742) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 44] <- as.integer(SINAN_ANTRAB_2017 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201743) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 45] <- as.integer(SINAN_ANTRAB_2017 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201744) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 46] <- as.integer(SINAN_ANTRAB_2017 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201745) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 47] <- as.integer(SINAN_ANTRAB_2017 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201746) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 48] <- as.integer(SINAN_ANTRAB_2017 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201747) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 49] <- as.integer(SINAN_ANTRAB_2017 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201748) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 50] <- as.integer(SINAN_ANTRAB_2017 %>%
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201749) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 51] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201750) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 52] <- as.integer(SINAN_ANTRAB_2017 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201751) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 53] <- as.integer(SINAN_ANTRAB_2017 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201752) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 54] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201753) %>%
                                           count() 
  )
}

AUX[, 1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX[(nrow(AUX)+ 1), 2:54] <- apply(AUX[, 2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2017_SE_Notificados"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2017_SE_Notificados"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2017_SE_Notificados.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                   filter(ID_MN_RESI == i) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CS_ZONA == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CS_ZONA == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CS_ZONA == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CS_ZONA == 9 
                                                          |
                                                            is.na(CS_ZONA)) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_SEXO == "F") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_SEXO == "M") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                    filter(ID_MN_RESI == i,  
                                                           NU_IDADE_N <=3012) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                    filter(ID_MN_RESI == i,  
                                                           NU_IDADE_N > 4000 
                                                           & 
                                                             NU_IDADE_N <=4005) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           NU_IDADE_N > 4005 
                                                           & 
                                                             NU_IDADE_N <=4012) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           NU_IDADE_N > 4012 
                                                           & 
                                                             NU_IDADE_N <=4018) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 16] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           NU_IDADE_N > 4018 
                                                           & 
                                                             NU_IDADE_N <=4059) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 17] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           NU_IDADE_N > 4059) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 18] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "00") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 19] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "01" 
                                                           | 
                                                             CS_ESCOL_N == "02" 
                                                           | 
                                                             CS_ESCOL_N == "03") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 20] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "04") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 21] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "05") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 22] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "06") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 23] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "07") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 24] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "08") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 25] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "10") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 26] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "09") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 27] <- as.integer(SINAN_ANTRAB_2017 %>% 
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
  
  AUX[which(AUX$COD_IBGE == i), 28] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_GESTANT == "5") %>% 
                                                    count()
  )
  AUX[which(AUX$COD_IBGE == i), 29] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_GESTANT == "6") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 30] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_GESTANT == "9") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 31] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "01") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 32] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "2") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 33] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "3") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 34] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "4") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 35] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "5") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 36] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "9") %>% 
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:36] <- apply(AUX[, 4:36], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2017_GERAL"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2017_GERAL"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2017_GERAL.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_CONTAT == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_ARRANH == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_LAMBED == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_MORDED == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_OUTRO_ == "1") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:9] <- apply(AUX[, 4:9], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2017_EXPOSICAO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2017_EXPOSICAO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2017_EXPOSICAO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_MUCOSA == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_CABECA == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_MAOS == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_TRONCO == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_MEMBRO== "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           ANT_MEMB_1== "1") %>%   
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:10] <- apply(AUX[, 4:10], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2017_LOCALIZACAO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2017_LOCALIZACAO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2017_LOCALIZACAO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FERIMENTO == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FERIMENTO == "2") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FERIMENTO == "3") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FERIMENTO == "9") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:8] <- apply(AUX[, 4:8], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2017_FERIMENTO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2017_FERIMENTO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2017_FERIMENTO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_PROFUN == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_SUPERF == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_DILACE == "1") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:7] <- apply(AUX[, 4:7], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2017_TIPO_FERIMENTO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2017_TIPO_FERIMENTO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2017_TIPO_FERIMENTO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          PRE_EXPOS == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          POS_EXPOS == "1") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:6] <- apply(AUX[, 4:6], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2017_ANTECEDENTE_ANTR"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2017_ANTECEDENTE_ANTR"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2017_ANTECEDENTE_ANTR.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANIMAL == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANIMAL == "2") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANIMAL == "3") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANIMAL == "4") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANIMAL == "5") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           ANIMAL == "6") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           ANIMAL == "7") %>%   
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:11] <- apply(AUX[, 4:11], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2017_AGRESSOR"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2017_AGRESSOR"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2017_AGRESSOR.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CONDIC_ANI == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CONDIC_ANI == "2") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CONDIC_ANI == "3") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CONDIC_ANI == "4") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:8] <- apply(AUX[, 4:8], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2017_COND_ANIMAL_ACID"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2017_COND_ANIMAL_ACID"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2017_COND_ANIMAL_ACID.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          OBSERVACAO == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          OBSERVACAO == "2") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:6] <- apply(AUX[, 4:6], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2017_OBSERVAVEL"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2017_OBSERVAVEL"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2017_OBSERVAVEL.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRAT_ATUAL == "2") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRAT_ATUAL == "3") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRAT_ATUAL == "4") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRAT_ATUAL == "5") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRAT_ATUAL == "6") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           TRAT_ATUAL == "7") %>%   
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:10] <- apply(AUX[, 4:10], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2017_TRATAMENTO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2017_TRATAMENTO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2017_TRATAMENTO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FIM_ANIMAL == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FIM_ANIMAL == "2") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FIM_ANIMAL == "3") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FIM_ANIMAL == "4") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FIM_ANIMAL == "5") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           FIM_ANIMAL == "9") %>%   
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:10] <- apply(AUX[, 4:10], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2017_COND_FINAL_ANIMAL"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2017_COND_FINAL_ANIMAL"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2017_COND_FINAL_ANIMAL.csv"), 
           row.names = FALSE)

###Tabela Interrupção de tratamento (campo 49 do SINAN)####

SINAN_ANTRAB_2017_INTERRUPCAO_TRAT <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

SINAN_ANTRAB_2017_INTERRUPCAO_TRAT$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

SINAN_ANTRAB_2017_INTERRUPCAO_TRAT$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

SINAN_ANTRAB_2017_INTERRUPCAO_TRAT$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

SINAN_ANTRAB_2017_INTERRUPCAO_TRAT <- SINAN_ANTRAB_2017_INTERRUPCAO_TRAT[,c(4, 1, 2, 3)]

SINAN_ANTRAB_2017_INTERRUPCAO_TRAT$Sim <- NA

SINAN_ANTRAB_2017_INTERRUPCAO_TRAT$Nao <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  SINAN_ANTRAB_2017_INTERRUPCAO_TRAT[which(SINAN_ANTRAB_2017_INTERRUPCAO_TRAT$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                                                                                 filter(ID_MN_RESI == i,
                                                                                                                        TRA_INTERR == "1") %>%   
                                                                                                                 count()
  )    
  
  SINAN_ANTRAB_2017_INTERRUPCAO_TRAT[which(SINAN_ANTRAB_2017_INTERRUPCAO_TRAT$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                                                                                 filter(ID_MN_RESI == i,
                                                                                                                        TRA_INTERR == "2") %>%   
                                                                                                                 count()
  )
}

SINAN_ANTRAB_2017_INTERRUPCAO_TRAT[(nrow(SINAN_ANTRAB_2017_INTERRUPCAO_TRAT) +1), 4:6] <- apply(SINAN_ANTRAB_2017_INTERRUPCAO_TRAT[, 4:6], 2, sum)
SINAN_ANTRAB_2017_INTERRUPCAO_TRAT[nrow(SINAN_ANTRAB_2017_INTERRUPCAO_TRAT), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2017_INTERRUPCAO_TRAT"), SINAN_ANTRAB_2017_INTERRUPCAO_TRAT)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2017_INTERRUPCAO_TRAT"), SINAN_ANTRAB_2017_INTERRUPCAO_TRAT), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2017_INTERRUPCAO_TRAT.csv"), 
           row.names = FALSE)

rm(SINAN_ANTRAB_2017_INTERRUPCAO_TRAT)

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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRA_INDI_N == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRA_INDI_N == "2") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRA_INDI_N == "9") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:7] <- apply(AUX[, 4:7], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2017_SOROTERAPIA"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2017_SOROTERAPIA"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2017_SOROTERAPIA.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TIP_SORO == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TIP_SORO== "2") %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:6] <- apply(AUX[, 4:6], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2017_TIPO_IMUNOBIOLOGICO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2017_TIPO_IMUNOBIOLOGICO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2017_TIPO_IMUNOBIOLOGICO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRA_INFILT == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2017 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRA_INFI_1 == "1") %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:6] <- apply(AUX[, 4:6], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2017_SORO_INFILTRACAO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2017_SORO_INFILTRACAO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2017_SORO_INFILTRACAO.csv"), 
           row.names = FALSE)

rm(SINAN_ANTRAB_2017, ANTRAB2017)
###################       2018      #########################################

####Criando um objeto com a base DBF do SINAN#################

ANTRAB2018 <- read.dbf(file = "Base_de_Dados/DBF/ANTRANET2018.DBF",
                       as.is = FALSE)


####Alterando o formato da coluna ID_MN_RESI de forma que ela seja passível de vincular no for loop#################

ANTRAB2018$ID_MN_RESI <- as.numeric(as.character(ANTRAB2018$ID_MN_RESI))

#####################################################################################################################
#################  Realizando contagem dos casos do Paraná para realizar a série histórica do Estado  ###############

PR_ANTRAB_Serie_Historica[7, 2] <- as.integer(ANTRAB2018 %>% 
                                                count()
)
#####Filtrando os dados da Base DBF do SINAN com os dados de notificação somente da 22RS###########

SINAN_ANTRAB_2018 <- ANTRAB2018 %>% 
  filter(ID_REGIONA == ID_REG | ID_RG_RESI == ID_REG)

assign(paste0("RS", RS, "_ANTRAB_2018_SINAN"), SINAN_ANTRAB_2018)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2018_SINAN"), SINAN_ANTRAB_2018), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2018_SINAN.csv"), 
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
  
  AUX[which(AUX == i), 2] <- as.integer(SINAN_ANTRAB_2018 %>%
                                          filter(ID_MN_RESI == i,
                                                 SEM_PRI ==201801)%>%
                                          count()
  )
  
  AUX[which(AUX == i), 3] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                          filter(ID_MN_RESI == i, 
                                                 SEM_PRI ==201802) %>% 
                                          count()
  )
  
  AUX[which(AUX == i), 4] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                          filter(ID_MN_RESI == i,
                                                 SEM_PRI ==201803) %>% 
                                          count()
  )
  
  AUX[which(AUX == i),5] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                         filter(ID_MN_RESI == i,
                                                SEM_PRI ==201804) %>% 
                                         count()
  )
  
  AUX[which(AUX == i), 6] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                          filter(ID_MN_RESI == i,
                                                 SEM_PRI ==201805) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 7] <- as.integer(SINAN_ANTRAB_2018 %>%
                                          filter(ID_MN_RESI == i, 
                                                 SEM_PRI ==201806) %>%
                                          count()
  )
  
  AUX[which(AUX == i), 8] <- as.integer(SINAN_ANTRAB_2018 %>%
                                          filter(ID_MN_RESI == i, 
                                                 SEM_PRI ==201807) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 9] <- as.integer(SINAN_ANTRAB_2018 %>%
                                          filter(ID_MN_RESI == i, 
                                                 SEM_PRI ==201808) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 10] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201809) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 11] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201810) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 12] <- as.integer(SINAN_ANTRAB_2018 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201811) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 13] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201812) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 14] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201813) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 15] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201814) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 16] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201815) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 17] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201816) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 18] <- as.integer(SINAN_ANTRAB_2018 %>%
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201817) %>%  
                                           count() 
  )
  
  AUX[which(AUX == i), 19] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201818) %>%      
                                           count() 
  )
  
  AUX[which(AUX == i), 20] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201819) %>%
                                           count() 
  )
  
  AUX[which(AUX == i),  21] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                            filter(ID_MN_RESI == i, 
                                                   SEM_PRI ==201820) %>%
                                            count() 
  )
  
  AUX[which(AUX == i), 22] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201821) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 23] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201822) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 24] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201823) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 25] <- as.integer(SINAN_ANTRAB_2018 %>%
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201824) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 26] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201825) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 27] <- as.integer(SINAN_ANTRAB_2018 %>%
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201826) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 28] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201827) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 29] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201828) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 30] <- as.integer(SINAN_ANTRAB_2018 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201829) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 31] <- as.integer(SINAN_ANTRAB_2018 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201830) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 32] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201831) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 33] <- as.integer(SINAN_ANTRAB_2018 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201832) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 34] <- as.integer(SINAN_ANTRAB_2018 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201833) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 35] <- as.integer(SINAN_ANTRAB_2018 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201834) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 36] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201835) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 37] <- as.integer(SINAN_ANTRAB_2018 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201836) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 38] <- as.integer(SINAN_ANTRAB_2018 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201837) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 39] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201838) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 40] <- as.integer(SINAN_ANTRAB_2018 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201839) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 41] <- as.integer(SINAN_ANTRAB_2018 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201840) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 42] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201841) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 43] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201842) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 44] <- as.integer(SINAN_ANTRAB_2018 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201843) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 45] <- as.integer(SINAN_ANTRAB_2018 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201844) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 46] <- as.integer(SINAN_ANTRAB_2018 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201845) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 47] <- as.integer(SINAN_ANTRAB_2018 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201846) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 48] <- as.integer(SINAN_ANTRAB_2018 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201847) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 49] <- as.integer(SINAN_ANTRAB_2018 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201848) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 50] <- as.integer(SINAN_ANTRAB_2018 %>%
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201849) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 51] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201850) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 52] <- as.integer(SINAN_ANTRAB_2018 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201851) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 53] <- as.integer(SINAN_ANTRAB_2018 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201852) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 54] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201853) %>%
                                           count() 
  )
}

AUX[, 1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX[(nrow(AUX)+ 1), 2:54] <- apply(AUX[, 2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2018_SE_Notificados"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2018_SE_Notificados"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2018_SE_Notificados.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                   filter(ID_MN_RESI == i) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CS_ZONA == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CS_ZONA == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CS_ZONA == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CS_ZONA == 9 
                                                          |
                                                            is.na(CS_ZONA)) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_SEXO == "F") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_SEXO == "M") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                    filter(ID_MN_RESI == i,  
                                                           NU_IDADE_N <=3012) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                    filter(ID_MN_RESI == i,  
                                                           NU_IDADE_N > 4000 
                                                           & 
                                                             NU_IDADE_N <=4005) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           NU_IDADE_N > 4005 
                                                           & 
                                                             NU_IDADE_N <=4012) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           NU_IDADE_N > 4012 
                                                           & 
                                                             NU_IDADE_N <=4018) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 16] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           NU_IDADE_N > 4018 
                                                           & 
                                                             NU_IDADE_N <=4059) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 17] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           NU_IDADE_N > 4059) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 18] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "00") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 19] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "01" 
                                                           | 
                                                             CS_ESCOL_N == "02" 
                                                           | 
                                                             CS_ESCOL_N == "03") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 20] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "04") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 21] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "05") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 22] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "06") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 23] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "07") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 24] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "08") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 25] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "10") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 26] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "09") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 27] <- as.integer(SINAN_ANTRAB_2018 %>% 
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
  
  AUX[which(AUX$COD_IBGE == i), 28] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_GESTANT == "5") %>% 
                                                    count()
  )
  AUX[which(AUX$COD_IBGE == i), 29] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_GESTANT == "6") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 30] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_GESTANT == "9") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 31] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "01") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 32] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "2") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 33] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "3") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 34] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "4") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 35] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "5") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 36] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "9") %>% 
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:36] <- apply(AUX[, 4:36], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2018_GERAL"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2018_GERAL"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2018_GERAL.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_CONTAT == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_ARRANH == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_LAMBED == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_MORDED == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_OUTRO_ == "1") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:9] <- apply(AUX[, 4:9], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2018_EXPOSICAO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2018_EXPOSICAO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2018_EXPOSICAO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_MUCOSA == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_CABECA == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_MAOS == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_TRONCO == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_MEMBRO== "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           ANT_MEMB_1== "1") %>%   
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:10] <- apply(AUX[, 4:10], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2018_LOCALIZACAO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2018_LOCALIZACAO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2018_LOCALIZACAO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FERIMENTO == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FERIMENTO == "2") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FERIMENTO == "3") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FERIMENTO == "9") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:8] <- apply(AUX[, 4:8], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2018_FERIMENTO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2018_FERIMENTO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2018_FERIMENTO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_PROFUN == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_SUPERF == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_DILACE == "1") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:7] <- apply(AUX[, 4:7], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2018_TIPO_FERIMENTO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2018_TIPO_FERIMENTO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2018_TIPO_FERIMENTO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          PRE_EXPOS == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          POS_EXPOS == "1") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:6] <- apply(AUX[, 4:6], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2018_ANTECEDENTE_ANTR"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2018_ANTECEDENTE_ANTR"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2018_ANTECEDENTE_ANTR.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANIMAL == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANIMAL == "2") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANIMAL == "3") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANIMAL == "4") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANIMAL == "5") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           ANIMAL == "6") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           ANIMAL == "7") %>%   
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:11] <- apply(AUX[, 4:11], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2018_AGRESSOR"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2018_AGRESSOR"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2018_AGRESSOR.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CONDIC_ANI == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CONDIC_ANI == "2") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CONDIC_ANI == "3") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CONDIC_ANI == "4") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:8] <- apply(AUX[, 4:8], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2018_COND_ANIMAL_ACID"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2018_COND_ANIMAL_ACID"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2018_COND_ANIMAL_ACID.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          OBSERVACAO == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          OBSERVACAO == "2") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:6] <- apply(AUX[, 4:6], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2018_OBSERVAVEL"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2018_OBSERVAVEL"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2018_OBSERVAVEL.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRAT_ATUAL == "2") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRAT_ATUAL == "3") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRAT_ATUAL == "4") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRAT_ATUAL == "5") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRAT_ATUAL == "6") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           TRAT_ATUAL == "7") %>%   
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:10] <- apply(AUX[, 4:10], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2018_TRATAMENTO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2018_TRATAMENTO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2018_TRATAMENTO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FIM_ANIMAL == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FIM_ANIMAL == "2") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FIM_ANIMAL == "3") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FIM_ANIMAL == "4") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FIM_ANIMAL == "5") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           FIM_ANIMAL == "9") %>%   
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:10] <- apply(AUX[, 4:10], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2018_COND_FINAL_ANIMAL"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2018_COND_FINAL_ANIMAL"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2018_COND_FINAL_ANIMAL.csv"), 
           row.names = FALSE)

###Tabela Interrupção de tratamento (campo 49 do SINAN)####

SINAN_ANTRAB_2018_INTERRUPCAO_TRAT <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

SINAN_ANTRAB_2018_INTERRUPCAO_TRAT$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

SINAN_ANTRAB_2018_INTERRUPCAO_TRAT$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

SINAN_ANTRAB_2018_INTERRUPCAO_TRAT$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

SINAN_ANTRAB_2018_INTERRUPCAO_TRAT <- SINAN_ANTRAB_2018_INTERRUPCAO_TRAT[,c(4, 1, 2, 3)]

SINAN_ANTRAB_2018_INTERRUPCAO_TRAT$Sim <- NA

SINAN_ANTRAB_2018_INTERRUPCAO_TRAT$Nao <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  SINAN_ANTRAB_2018_INTERRUPCAO_TRAT[which(SINAN_ANTRAB_2018_INTERRUPCAO_TRAT$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                                                                                 filter(ID_MN_RESI == i,
                                                                                                                        TRA_INTERR == "1") %>%   
                                                                                                                 count()
  )    
  
  SINAN_ANTRAB_2018_INTERRUPCAO_TRAT[which(SINAN_ANTRAB_2018_INTERRUPCAO_TRAT$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                                                                                 filter(ID_MN_RESI == i,
                                                                                                                        TRA_INTERR == "2") %>%   
                                                                                                                 count()
  )
}

SINAN_ANTRAB_2018_INTERRUPCAO_TRAT[(nrow(SINAN_ANTRAB_2018_INTERRUPCAO_TRAT) +1), 4:6] <- apply(SINAN_ANTRAB_2018_INTERRUPCAO_TRAT[, 4:6], 2, sum)
SINAN_ANTRAB_2018_INTERRUPCAO_TRAT[nrow(SINAN_ANTRAB_2018_INTERRUPCAO_TRAT), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2018_INTERRUPCAO_TRAT"), SINAN_ANTRAB_2018_INTERRUPCAO_TRAT)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2018_INTERRUPCAO_TRAT"), SINAN_ANTRAB_2018_INTERRUPCAO_TRAT), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2018_INTERRUPCAO_TRAT.csv"), 
           row.names = FALSE)

rm(SINAN_ANTRAB_2018_INTERRUPCAO_TRAT)

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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRA_INDI_N == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRA_INDI_N == "2") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRA_INDI_N == "9") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:7] <- apply(AUX[, 4:7], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2018_SOROTERAPIA"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2018_SOROTERAPIA"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2018_SOROTERAPIA.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TIP_SORO == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TIP_SORO== "2") %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:6] <- apply(AUX[, 4:6], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2018_TIPO_IMUNOBIOLOGICO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2018_TIPO_IMUNOBIOLOGICO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2018_TIPO_IMUNOBIOLOGICO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRA_INFILT == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2018 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRA_INFI_1 == "1") %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:6] <- apply(AUX[, 4:6], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2018_SORO_INFILTRACAO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2018_SORO_INFILTRACAO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2018_SORO_INFILTRACAO.csv"), 
           row.names = FALSE)

rm(SINAN_ANTRAB_2018, ANTRAB2018)

###################       2019      #########################################

####Criando um objeto com a base DBF do SINAN#################

ANTRAB2019 <- read.dbf(file = "Base_de_Dados/DBF/ANTRANET2019.DBF",
                       as.is = FALSE)


####Alterando o formato da coluna ID_MN_RESI de forma que ela seja passível de vincular no for loop#################

ANTRAB2019$ID_MN_RESI <- as.numeric(as.character(ANTRAB2019$ID_MN_RESI))

#####################################################################################################################
#################  Realizando contagem dos casos do Paraná para realizar a série histórica do Estado  ###############

PR_ANTRAB_Serie_Historica[8, 2] <- as.integer(ANTRAB2019 %>% 
                                                count()
)
#####Filtrando os dados da Base DBF do SINAN com os dados de notificação somente da 22RS###########

SINAN_ANTRAB_2019 <- ANTRAB2019 %>% 
  filter(ID_REGIONA == ID_REG | ID_RG_RESI == ID_REG)

assign(paste0("RS", RS, "_ANTRAB_2019_SINAN"), SINAN_ANTRAB_2019)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2019_SINAN"), SINAN_ANTRAB_2019), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2019_SINAN.csv"), 
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
  
  AUX[which(AUX == i), 2] <- as.integer(SINAN_ANTRAB_2019 %>%
                                          filter(ID_MN_RESI == i,
                                                 SEM_PRI ==201901)%>%
                                          count()
  )
  
  AUX[which(AUX == i), 3] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                          filter(ID_MN_RESI == i, 
                                                 SEM_PRI ==201902) %>% 
                                          count()
  )
  
  AUX[which(AUX == i), 4] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                          filter(ID_MN_RESI == i,
                                                 SEM_PRI ==201903) %>% 
                                          count()
  )
  
  AUX[which(AUX == i),5] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                         filter(ID_MN_RESI == i,
                                                SEM_PRI ==201904) %>% 
                                         count()
  )
  
  AUX[which(AUX == i), 6] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                          filter(ID_MN_RESI == i,
                                                 SEM_PRI ==201905) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 7] <- as.integer(SINAN_ANTRAB_2019 %>%
                                          filter(ID_MN_RESI == i, 
                                                 SEM_PRI ==201906) %>%
                                          count()
  )
  
  AUX[which(AUX == i), 8] <- as.integer(SINAN_ANTRAB_2019 %>%
                                          filter(ID_MN_RESI == i, 
                                                 SEM_PRI ==201907) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 9] <- as.integer(SINAN_ANTRAB_2019 %>%
                                          filter(ID_MN_RESI == i, 
                                                 SEM_PRI ==201908) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 10] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201909) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 11] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201910) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 12] <- as.integer(SINAN_ANTRAB_2019 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201911) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 13] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201912) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 14] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201913) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 15] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201914) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 16] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201915) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 17] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201916) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 18] <- as.integer(SINAN_ANTRAB_2019 %>%
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201917) %>%  
                                           count() 
  )
  
  AUX[which(AUX == i), 19] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201918) %>%      
                                           count() 
  )
  
  AUX[which(AUX == i), 20] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201919) %>%
                                           count() 
  )
  
  AUX[which(AUX == i),  21] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                            filter(ID_MN_RESI == i, 
                                                   SEM_PRI ==201920) %>%
                                            count() 
  )
  
  AUX[which(AUX == i), 22] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201921) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 23] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201922) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 24] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201923) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 25] <- as.integer(SINAN_ANTRAB_2019 %>%
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201924) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 26] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201925) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 27] <- as.integer(SINAN_ANTRAB_2019 %>%
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201926) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 28] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201927) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 29] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201928) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 30] <- as.integer(SINAN_ANTRAB_2019 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201929) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 31] <- as.integer(SINAN_ANTRAB_2019 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201930) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 32] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201931) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 33] <- as.integer(SINAN_ANTRAB_2019 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201932) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 34] <- as.integer(SINAN_ANTRAB_2019 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201933) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 35] <- as.integer(SINAN_ANTRAB_2019 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201934) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 36] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201935) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 37] <- as.integer(SINAN_ANTRAB_2019 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201936) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 38] <- as.integer(SINAN_ANTRAB_2019 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201937) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 39] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201938) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 40] <- as.integer(SINAN_ANTRAB_2019 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201939) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 41] <- as.integer(SINAN_ANTRAB_2019 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201940) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 42] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201941) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 43] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201942) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 44] <- as.integer(SINAN_ANTRAB_2019 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201943) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 45] <- as.integer(SINAN_ANTRAB_2019 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201944) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 46] <- as.integer(SINAN_ANTRAB_2019 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201945) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 47] <- as.integer(SINAN_ANTRAB_2019 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201946) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 48] <- as.integer(SINAN_ANTRAB_2019 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201947) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 49] <- as.integer(SINAN_ANTRAB_2019 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201948) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 50] <- as.integer(SINAN_ANTRAB_2019 %>%
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201949) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 51] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201950) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 52] <- as.integer(SINAN_ANTRAB_2019 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201951) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 53] <- as.integer(SINAN_ANTRAB_2019 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==201952) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 54] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==201953) %>%
                                           count() 
  )
}

AUX[, 1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX[(nrow(AUX)+ 1), 2:54] <- apply(AUX[, 2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2019_SE_Notificados"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2019_SE_Notificados"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2019_SE_Notificados.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                   filter(ID_MN_RESI == i) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CS_ZONA == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CS_ZONA == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CS_ZONA == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CS_ZONA == 9 
                                                          |
                                                            is.na(CS_ZONA)) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_SEXO == "F") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_SEXO == "M") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                    filter(ID_MN_RESI == i,  
                                                           NU_IDADE_N <=3012) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                    filter(ID_MN_RESI == i,  
                                                           NU_IDADE_N > 4000 
                                                           & 
                                                             NU_IDADE_N <=4005) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           NU_IDADE_N > 4005 
                                                           & 
                                                             NU_IDADE_N <=4012) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           NU_IDADE_N > 4012 
                                                           & 
                                                             NU_IDADE_N <=4018) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 16] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           NU_IDADE_N > 4018 
                                                           & 
                                                             NU_IDADE_N <=4059) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 17] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           NU_IDADE_N > 4059) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 18] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "00") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 19] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "01" 
                                                           | 
                                                             CS_ESCOL_N == "02" 
                                                           | 
                                                             CS_ESCOL_N == "03") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 20] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "04") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 21] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "05") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 22] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "06") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 23] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "07") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 24] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "08") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 25] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "10") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 26] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "09") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 27] <- as.integer(SINAN_ANTRAB_2019 %>% 
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
  
  AUX[which(AUX$COD_IBGE == i), 28] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_GESTANT == "5") %>% 
                                                    count()
  )
  AUX[which(AUX$COD_IBGE == i), 29] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_GESTANT == "6") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 30] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_GESTANT == "9") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 31] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "01") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 32] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "2") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 33] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "3") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 34] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "4") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 35] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "5") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 36] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "9") %>% 
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:36] <- apply(AUX[, 4:36], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2019_GERAL"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2019_GERAL"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2019_GERAL.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_CONTAT == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_ARRANH == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_LAMBED == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_MORDED == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_OUTRO_ == "1") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:9] <- apply(AUX[, 4:9], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2019_EXPOSICAO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2019_EXPOSICAO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2019_EXPOSICAO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_MUCOSA == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_CABECA == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_MAOS == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_TRONCO == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_MEMBRO== "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           ANT_MEMB_1== "1") %>%   
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:10] <- apply(AUX[, 4:10], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2019_LOCALIZACAO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2019_LOCALIZACAO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2019_LOCALIZACAO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FERIMENTO == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FERIMENTO == "2") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FERIMENTO == "3") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FERIMENTO == "9") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:8] <- apply(AUX[, 4:8], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2019_FERIMENTO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2019_FERIMENTO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2019_FERIMENTO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_PROFUN == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_SUPERF == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_DILACE == "1") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:7] <- apply(AUX[, 4:7], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2019_TIPO_FERIMENTO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2019_TIPO_FERIMENTO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2019_TIPO_FERIMENTO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          PRE_EXPOS == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          POS_EXPOS == "1") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:6] <- apply(AUX[, 4:6], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2019_ANTECEDENTE_ANTR"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2019_ANTECEDENTE_ANTR"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2019_ANTECEDENTE_ANTR.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANIMAL == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANIMAL == "2") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANIMAL == "3") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANIMAL == "4") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANIMAL == "5") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           ANIMAL == "6") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           ANIMAL == "7") %>%   
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:11] <- apply(AUX[, 4:11], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2019_AGRESSOR"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2019_AGRESSOR"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2019_AGRESSOR.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CONDIC_ANI == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CONDIC_ANI == "2") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CONDIC_ANI == "3") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CONDIC_ANI == "4") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:8] <- apply(AUX[, 4:8], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2019_COND_ANIMAL_ACID"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2019_COND_ANIMAL_ACID"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2019_COND_ANIMAL_ACID.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          OBSERVACAO == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          OBSERVACAO == "2") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:6] <- apply(AUX[, 4:6], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2019_OBSERVAVEL"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2019_OBSERVAVEL"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2019_OBSERVAVEL.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRAT_ATUAL == "2") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRAT_ATUAL == "3") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRAT_ATUAL == "4") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRAT_ATUAL == "5") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRAT_ATUAL == "6") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           TRAT_ATUAL == "7") %>%   
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:10] <- apply(AUX[, 4:10], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2019_TRATAMENTO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2019_TRATAMENTO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2019_TRATAMENTO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FIM_ANIMAL == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FIM_ANIMAL == "2") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FIM_ANIMAL == "3") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FIM_ANIMAL == "4") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FIM_ANIMAL == "5") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           FIM_ANIMAL == "9") %>%   
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:10] <- apply(AUX[, 4:10], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2019_COND_FINAL_ANIMAL"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2019_COND_FINAL_ANIMAL"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2019_COND_FINAL_ANIMAL.csv"), 
           row.names = FALSE)

###Tabela Interrupção de tratamento (campo 49 do SINAN)####

SINAN_ANTRAB_2019_INTERRUPCAO_TRAT <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

SINAN_ANTRAB_2019_INTERRUPCAO_TRAT$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

SINAN_ANTRAB_2019_INTERRUPCAO_TRAT$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

SINAN_ANTRAB_2019_INTERRUPCAO_TRAT$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

SINAN_ANTRAB_2019_INTERRUPCAO_TRAT <- SINAN_ANTRAB_2019_INTERRUPCAO_TRAT[,c(4, 1, 2, 3)]

SINAN_ANTRAB_2019_INTERRUPCAO_TRAT$Sim <- NA

SINAN_ANTRAB_2019_INTERRUPCAO_TRAT$Nao <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  SINAN_ANTRAB_2019_INTERRUPCAO_TRAT[which(SINAN_ANTRAB_2019_INTERRUPCAO_TRAT$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                                                                                 filter(ID_MN_RESI == i,
                                                                                                                        TRA_INTERR == "1") %>%   
                                                                                                                 count()
  )    
  
  SINAN_ANTRAB_2019_INTERRUPCAO_TRAT[which(SINAN_ANTRAB_2019_INTERRUPCAO_TRAT$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                                                                                 filter(ID_MN_RESI == i,
                                                                                                                        TRA_INTERR == "2") %>%   
                                                                                                                 count()
  )
}

SINAN_ANTRAB_2019_INTERRUPCAO_TRAT[(nrow(SINAN_ANTRAB_2019_INTERRUPCAO_TRAT) +1), 4:6] <- apply(SINAN_ANTRAB_2019_INTERRUPCAO_TRAT[, 4:6], 2, sum)
SINAN_ANTRAB_2019_INTERRUPCAO_TRAT[nrow(SINAN_ANTRAB_2019_INTERRUPCAO_TRAT), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2019_INTERRUPCAO_TRAT"), SINAN_ANTRAB_2019_INTERRUPCAO_TRAT)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2019_INTERRUPCAO_TRAT"), SINAN_ANTRAB_2019_INTERRUPCAO_TRAT), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2019_INTERRUPCAO_TRAT.csv"), 
           row.names = FALSE)

rm(SINAN_ANTRAB_2019_INTERRUPCAO_TRAT)

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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRA_INDI_N == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRA_INDI_N == "2") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRA_INDI_N == "9") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:7] <- apply(AUX[, 4:7], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2019_SOROTERAPIA"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2019_SOROTERAPIA"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2019_SOROTERAPIA.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TIP_SORO == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TIP_SORO== "2") %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:6] <- apply(AUX[, 4:6], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2019_TIPO_IMUNOBIOLOGICO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2019_TIPO_IMUNOBIOLOGICO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2019_TIPO_IMUNOBIOLOGICO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRA_INFILT == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2019 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRA_INFI_1 == "1") %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:6] <- apply(AUX[, 4:6], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2019_SORO_INFILTRACAO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2019_SORO_INFILTRACAO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2019_SORO_INFILTRACAO.csv"), 
           row.names = FALSE)

rm(SINAN_ANTRAB_2019, ANTRAB2019)

###################       2020      #########################################

####Criando um objeto com a base DBF do SINAN#################

ANTRAB2020 <- read.dbf(file = "Base_de_Dados/DBF/ANTRANET2020.DBF",
                       as.is = FALSE)


####Alterando o formato da coluna ID_MN_RESI de forma que ela seja passível de vincular no for loop#################

ANTRAB2020$ID_MN_RESI <- as.numeric(as.character(ANTRAB2020$ID_MN_RESI))

#####################################################################################################################
#################  Realizando contagem dos casos do Paraná para realizar a série histórica do Estado  ###############

PR_ANTRAB_Serie_Historica[9, 2] <- as.integer(ANTRAB2020 %>% 
                                                count()
)
#####Filtrando os dados da Base DBF do SINAN com os dados de notificação somente da 22RS###########

SINAN_ANTRAB_2020 <- ANTRAB2020 %>% 
  filter(ID_REGIONA == ID_REG | ID_RG_RESI == ID_REG)

assign(paste0("RS", RS, "_ANTRAB_2020_SINAN"), SINAN_ANTRAB_2020)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2020_SINAN"), SINAN_ANTRAB_2020), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2020_SINAN.csv"), 
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
  
  AUX[which(AUX == i), 2] <- as.integer(SINAN_ANTRAB_2020 %>%
                                          filter(ID_MN_RESI == i,
                                                 SEM_PRI ==202001)%>%
                                          count()
  )
  
  AUX[which(AUX == i), 3] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                          filter(ID_MN_RESI == i, 
                                                 SEM_PRI ==202002) %>% 
                                          count()
  )
  
  AUX[which(AUX == i), 4] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                          filter(ID_MN_RESI == i,
                                                 SEM_PRI ==202003) %>% 
                                          count()
  )
  
  AUX[which(AUX == i),5] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                         filter(ID_MN_RESI == i,
                                                SEM_PRI ==202004) %>% 
                                         count()
  )
  
  AUX[which(AUX == i), 6] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                          filter(ID_MN_RESI == i,
                                                 SEM_PRI ==202005) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 7] <- as.integer(SINAN_ANTRAB_2020 %>%
                                          filter(ID_MN_RESI == i, 
                                                 SEM_PRI ==202006) %>%
                                          count()
  )
  
  AUX[which(AUX == i), 8] <- as.integer(SINAN_ANTRAB_2020 %>%
                                          filter(ID_MN_RESI == i, 
                                                 SEM_PRI ==202007) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 9] <- as.integer(SINAN_ANTRAB_2020 %>%
                                          filter(ID_MN_RESI == i, 
                                                 SEM_PRI ==202008) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 10] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202009) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 11] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202010) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 12] <- as.integer(SINAN_ANTRAB_2020 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202011) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 13] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202012) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 14] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202013) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 15] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202014) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 16] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202015) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 17] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202016) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 18] <- as.integer(SINAN_ANTRAB_2020 %>%
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202017) %>%  
                                           count() 
  )
  
  AUX[which(AUX == i), 19] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202018) %>%      
                                           count() 
  )
  
  AUX[which(AUX == i), 20] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202019) %>%
                                           count() 
  )
  
  AUX[which(AUX == i),  21] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                            filter(ID_MN_RESI == i, 
                                                   SEM_PRI ==202020) %>%
                                            count() 
  )
  
  AUX[which(AUX == i), 22] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202021) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 23] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202022) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 24] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202023) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 25] <- as.integer(SINAN_ANTRAB_2020 %>%
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202024) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 26] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202025) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 27] <- as.integer(SINAN_ANTRAB_2020 %>%
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202026) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 28] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202027) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 29] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202028) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 30] <- as.integer(SINAN_ANTRAB_2020 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202029) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 31] <- as.integer(SINAN_ANTRAB_2020 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202030) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 32] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202031) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 33] <- as.integer(SINAN_ANTRAB_2020 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202032) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 34] <- as.integer(SINAN_ANTRAB_2020 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202033) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 35] <- as.integer(SINAN_ANTRAB_2020 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202034) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 36] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202035) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 37] <- as.integer(SINAN_ANTRAB_2020 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202036) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 38] <- as.integer(SINAN_ANTRAB_2020 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202037) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 39] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202038) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 40] <- as.integer(SINAN_ANTRAB_2020 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202039) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 41] <- as.integer(SINAN_ANTRAB_2020 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202040) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 42] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202041) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 43] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202042) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 44] <- as.integer(SINAN_ANTRAB_2020 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202043) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 45] <- as.integer(SINAN_ANTRAB_2020 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202044) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 46] <- as.integer(SINAN_ANTRAB_2020 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202045) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 47] <- as.integer(SINAN_ANTRAB_2020 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202046) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 48] <- as.integer(SINAN_ANTRAB_2020 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202047) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 49] <- as.integer(SINAN_ANTRAB_2020 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202048) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 50] <- as.integer(SINAN_ANTRAB_2020 %>%
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202049) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 51] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202050) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 52] <- as.integer(SINAN_ANTRAB_2020 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202051) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 53] <- as.integer(SINAN_ANTRAB_2020 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202052) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 54] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202053) %>%
                                           count() 
  )
}

AUX[, 1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX[(nrow(AUX)+ 1), 2:54] <- apply(AUX[, 2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2020_SE_Notificados"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2020_SE_Notificados"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2020_SE_Notificados.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                   filter(ID_MN_RESI == i) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CS_ZONA == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CS_ZONA == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CS_ZONA == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CS_ZONA == 9 
                                                          |
                                                            is.na(CS_ZONA)) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_SEXO == "F") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_SEXO == "M") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                    filter(ID_MN_RESI == i,  
                                                           NU_IDADE_N <=3012) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                    filter(ID_MN_RESI == i,  
                                                           NU_IDADE_N > 4000 
                                                           & 
                                                             NU_IDADE_N <=4005) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           NU_IDADE_N > 4005 
                                                           & 
                                                             NU_IDADE_N <=4012) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           NU_IDADE_N > 4012 
                                                           & 
                                                             NU_IDADE_N <=4018) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 16] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           NU_IDADE_N > 4018 
                                                           & 
                                                             NU_IDADE_N <=4059) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 17] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           NU_IDADE_N > 4059) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 18] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "00") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 19] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "01" 
                                                           | 
                                                             CS_ESCOL_N == "02" 
                                                           | 
                                                             CS_ESCOL_N == "03") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 20] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "04") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 21] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "05") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 22] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "06") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 23] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "07") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 24] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "08") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 25] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "10") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 26] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "09") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 27] <- as.integer(SINAN_ANTRAB_2020 %>% 
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
  
  AUX[which(AUX$COD_IBGE == i), 28] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_GESTANT == "5") %>% 
                                                    count()
  )
  AUX[which(AUX$COD_IBGE == i), 29] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_GESTANT == "6") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 30] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_GESTANT == "9") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 31] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "01") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 32] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "2") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 33] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "3") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 34] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "4") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 35] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "5") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 36] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "9") %>% 
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:36] <- apply(AUX[, 4:36], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2020_GERAL"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2020_GERAL"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2020_GERAL.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_CONTAT == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_ARRANH == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_LAMBED == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_MORDED == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_OUTRO_ == "1") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:9] <- apply(AUX[, 4:9], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2020_EXPOSICAO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2020_EXPOSICAO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2020_EXPOSICAO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_MUCOSA == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_CABECA == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_MAOS == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_TRONCO == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_MEMBRO== "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           ANT_MEMB_1== "1") %>%   
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:10] <- apply(AUX[, 4:10], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2020_LOCALIZACAO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2020_LOCALIZACAO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2020_LOCALIZACAO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FERIMENTO == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FERIMENTO == "2") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FERIMENTO == "3") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FERIMENTO == "9") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:8] <- apply(AUX[, 4:8], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2020_FERIMENTO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2020_FERIMENTO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2020_FERIMENTO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_PROFUN == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_SUPERF == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_DILACE == "1") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:7] <- apply(AUX[, 4:7], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2020_TIPO_FERIMENTO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2020_TIPO_FERIMENTO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2020_TIPO_FERIMENTO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          PRE_EXPOS == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          POS_EXPOS == "1") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:6] <- apply(AUX[, 4:6], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2020_ANTECEDENTE_ANTR"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2020_ANTECEDENTE_ANTR"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2020_ANTECEDENTE_ANTR.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANIMAL == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANIMAL == "2") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANIMAL == "3") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANIMAL == "4") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANIMAL == "5") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           ANIMAL == "6") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           ANIMAL == "7") %>%   
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:11] <- apply(AUX[, 4:11], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2020_AGRESSOR"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2020_AGRESSOR"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2020_AGRESSOR.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CONDIC_ANI == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CONDIC_ANI == "2") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CONDIC_ANI == "3") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CONDIC_ANI == "4") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:8] <- apply(AUX[, 4:8], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2020_COND_ANIMAL_ACID"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2020_COND_ANIMAL_ACID"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2020_COND_ANIMAL_ACID.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          OBSERVACAO == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          OBSERVACAO == "2") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:6] <- apply(AUX[, 4:6], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2020_OBSERVAVEL"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2020_OBSERVAVEL"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2020_OBSERVAVEL.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRAT_ATUAL == "2") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRAT_ATUAL == "3") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRAT_ATUAL == "4") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRAT_ATUAL == "5") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRAT_ATUAL == "6") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           TRAT_ATUAL == "7") %>%   
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:10] <- apply(AUX[, 4:10], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2020_TRATAMENTO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2020_TRATAMENTO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2020_TRATAMENTO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FIM_ANIMAL == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FIM_ANIMAL == "2") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FIM_ANIMAL == "3") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FIM_ANIMAL == "4") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FIM_ANIMAL == "5") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           FIM_ANIMAL == "9") %>%   
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:10] <- apply(AUX[, 4:10], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2020_COND_FINAL_ANIMAL"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2020_COND_FINAL_ANIMAL"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2020_COND_FINAL_ANIMAL.csv"), 
           row.names = FALSE)

###Tabela Interrupção de tratamento (campo 49 do SINAN)####

SINAN_ANTRAB_2020_INTERRUPCAO_TRAT <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

SINAN_ANTRAB_2020_INTERRUPCAO_TRAT$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

SINAN_ANTRAB_2020_INTERRUPCAO_TRAT$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

SINAN_ANTRAB_2020_INTERRUPCAO_TRAT$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

SINAN_ANTRAB_2020_INTERRUPCAO_TRAT <- SINAN_ANTRAB_2020_INTERRUPCAO_TRAT[,c(4, 1, 2, 3)]

SINAN_ANTRAB_2020_INTERRUPCAO_TRAT$Sim <- NA

SINAN_ANTRAB_2020_INTERRUPCAO_TRAT$Nao <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  SINAN_ANTRAB_2020_INTERRUPCAO_TRAT[which(SINAN_ANTRAB_2020_INTERRUPCAO_TRAT$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                                                                                 filter(ID_MN_RESI == i,
                                                                                                                        TRA_INTERR == "1") %>%   
                                                                                                                 count()
  )    
  
  SINAN_ANTRAB_2020_INTERRUPCAO_TRAT[which(SINAN_ANTRAB_2020_INTERRUPCAO_TRAT$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                                                                                 filter(ID_MN_RESI == i,
                                                                                                                        TRA_INTERR == "2") %>%   
                                                                                                                 count()
  )
}

SINAN_ANTRAB_2020_INTERRUPCAO_TRAT[(nrow(SINAN_ANTRAB_2020_INTERRUPCAO_TRAT) +1), 4:6] <- apply(SINAN_ANTRAB_2020_INTERRUPCAO_TRAT[, 4:6], 2, sum)
SINAN_ANTRAB_2020_INTERRUPCAO_TRAT[nrow(SINAN_ANTRAB_2020_INTERRUPCAO_TRAT), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2020_INTERRUPCAO_TRAT"), SINAN_ANTRAB_2020_INTERRUPCAO_TRAT)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2020_INTERRUPCAO_TRAT"), SINAN_ANTRAB_2020_INTERRUPCAO_TRAT), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2020_INTERRUPCAO_TRAT.csv"), 
           row.names = FALSE)

rm(SINAN_ANTRAB_2020_INTERRUPCAO_TRAT)

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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRA_INDI_N == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRA_INDI_N == "2") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRA_INDI_N == "9") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:7] <- apply(AUX[, 4:7], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2020_SOROTERAPIA"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2020_SOROTERAPIA"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2020_SOROTERAPIA.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TIP_SORO == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TIP_SORO== "2") %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:6] <- apply(AUX[, 4:6], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2020_TIPO_IMUNOBIOLOGICO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2020_TIPO_IMUNOBIOLOGICO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2020_TIPO_IMUNOBIOLOGICO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRA_INFILT == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2020 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRA_INFI_1 == "1") %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:6] <- apply(AUX[, 4:6], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2020_SORO_INFILTRACAO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2020_SORO_INFILTRACAO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2020_SORO_INFILTRACAO.csv"), 
           row.names = FALSE)

rm(SINAN_ANTRAB_2020, ANTRAB2020)

###################       2021      #########################################

####Criando um objeto com a base DBF do SINAN#################

ANTRAB2021 <- read.dbf(file = "Base_de_Dados/DBF/ANTRANET2021.DBF",
                       as.is = FALSE)


####Alterando o formato da coluna ID_MN_RESI de forma que ela seja passível de vincular no for loop#################

ANTRAB2021$ID_MN_RESI <- as.numeric(as.character(ANTRAB2021$ID_MN_RESI))

#####################################################################################################################
#################  Realizando contagem dos casos do Paraná para realizar a série histórica do Estado  ###############

PR_ANTRAB_Serie_Historica[10, 2] <- as.integer(ANTRAB2021 %>% 
                                                 count()
)
#####Filtrando os dados da Base DBF do SINAN com os dados de notificação somente da 22RS###########

SINAN_ANTRAB_2021 <- ANTRAB2021 %>% 
  filter(ID_REGIONA == ID_REG | ID_RG_RESI == ID_REG)

assign(paste0("RS", RS, "_ANTRAB_2021_SINAN"), SINAN_ANTRAB_2021)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2021_SINAN"), SINAN_ANTRAB_2021), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2021_SINAN.csv"), 
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
  
  AUX[which(AUX == i), 2] <- as.integer(SINAN_ANTRAB_2021 %>%
                                          filter(ID_MN_RESI == i,
                                                 SEM_PRI ==202101)%>%
                                          count()
  )
  
  AUX[which(AUX == i), 3] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                          filter(ID_MN_RESI == i, 
                                                 SEM_PRI ==202102) %>% 
                                          count()
  )
  
  AUX[which(AUX == i), 4] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                          filter(ID_MN_RESI == i,
                                                 SEM_PRI ==202103) %>% 
                                          count()
  )
  
  AUX[which(AUX == i),5] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                         filter(ID_MN_RESI == i,
                                                SEM_PRI ==202104) %>% 
                                         count()
  )
  
  AUX[which(AUX == i), 6] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                          filter(ID_MN_RESI == i,
                                                 SEM_PRI ==202105) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 7] <- as.integer(SINAN_ANTRAB_2021 %>%
                                          filter(ID_MN_RESI == i, 
                                                 SEM_PRI ==202106) %>%
                                          count()
  )
  
  AUX[which(AUX == i), 8] <- as.integer(SINAN_ANTRAB_2021 %>%
                                          filter(ID_MN_RESI == i, 
                                                 SEM_PRI ==202107) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 9] <- as.integer(SINAN_ANTRAB_2021 %>%
                                          filter(ID_MN_RESI == i, 
                                                 SEM_PRI ==202108) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 10] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202109) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 11] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202110) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 12] <- as.integer(SINAN_ANTRAB_2021 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202111) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 13] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202112) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 14] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202113) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 15] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202114) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 16] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202115) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 17] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202116) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 18] <- as.integer(SINAN_ANTRAB_2021 %>%
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202117) %>%  
                                           count() 
  )
  
  AUX[which(AUX == i), 19] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202118) %>%      
                                           count() 
  )
  
  AUX[which(AUX == i), 20] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202119) %>%
                                           count() 
  )
  
  AUX[which(AUX == i),  21] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                            filter(ID_MN_RESI == i, 
                                                   SEM_PRI ==202120) %>%
                                            count() 
  )
  
  AUX[which(AUX == i), 22] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202121) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 23] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202122) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 24] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202123) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 25] <- as.integer(SINAN_ANTRAB_2021 %>%
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202124) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 26] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202125) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 27] <- as.integer(SINAN_ANTRAB_2021 %>%
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202126) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 28] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202127) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 29] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202128) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 30] <- as.integer(SINAN_ANTRAB_2021 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202129) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 31] <- as.integer(SINAN_ANTRAB_2021 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202130) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 32] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202131) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 33] <- as.integer(SINAN_ANTRAB_2021 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202132) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 34] <- as.integer(SINAN_ANTRAB_2021 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202133) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 35] <- as.integer(SINAN_ANTRAB_2021 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202134) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 36] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202135) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 37] <- as.integer(SINAN_ANTRAB_2021 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202136) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 38] <- as.integer(SINAN_ANTRAB_2021 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202137) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 39] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202138) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 40] <- as.integer(SINAN_ANTRAB_2021 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202139) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 41] <- as.integer(SINAN_ANTRAB_2021 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202140) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 42] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202141) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 43] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202142) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 44] <- as.integer(SINAN_ANTRAB_2021 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202143) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 45] <- as.integer(SINAN_ANTRAB_2021 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202144) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 46] <- as.integer(SINAN_ANTRAB_2021 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202145) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 47] <- as.integer(SINAN_ANTRAB_2021 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202146) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 48] <- as.integer(SINAN_ANTRAB_2021 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202147) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 49] <- as.integer(SINAN_ANTRAB_2021 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202148) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 50] <- as.integer(SINAN_ANTRAB_2021 %>%
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202149) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 51] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202150) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 52] <- as.integer(SINAN_ANTRAB_2021 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202151) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 53] <- as.integer(SINAN_ANTRAB_2021 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202152) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 54] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202153) %>%
                                           count() 
  )
}

AUX[, 1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX[(nrow(AUX)+ 1), 2:54] <- apply(AUX[, 2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2021_SE_Notificados"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2021_SE_Notificados"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2021_SE_Notificados.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                   filter(ID_MN_RESI == i) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CS_ZONA == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CS_ZONA == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CS_ZONA == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CS_ZONA == 9 
                                                          |
                                                            is.na(CS_ZONA)) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_SEXO == "F") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_SEXO == "M") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                    filter(ID_MN_RESI == i,  
                                                           NU_IDADE_N <=3012) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                    filter(ID_MN_RESI == i,  
                                                           NU_IDADE_N > 4000 
                                                           & 
                                                             NU_IDADE_N <=4005) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           NU_IDADE_N > 4005 
                                                           & 
                                                             NU_IDADE_N <=4012) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           NU_IDADE_N > 4012 
                                                           & 
                                                             NU_IDADE_N <=4018) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 16] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           NU_IDADE_N > 4018 
                                                           & 
                                                             NU_IDADE_N <=4059) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 17] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           NU_IDADE_N > 4059) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 18] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "00") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 19] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "01" 
                                                           | 
                                                             CS_ESCOL_N == "02" 
                                                           | 
                                                             CS_ESCOL_N == "03") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 20] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "04") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 21] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "05") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 22] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "06") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 23] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "07") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 24] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "08") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 25] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "10") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 26] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "09") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 27] <- as.integer(SINAN_ANTRAB_2021 %>% 
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
  
  AUX[which(AUX$COD_IBGE == i), 28] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_GESTANT == "5") %>% 
                                                    count()
  )
  AUX[which(AUX$COD_IBGE == i), 29] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_GESTANT == "6") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 30] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_GESTANT == "9") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 31] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "01") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 32] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "2") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 33] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "3") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 34] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "4") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 35] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "5") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 36] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "9") %>% 
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:36] <- apply(AUX[, 4:36], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2021_GERAL"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2021_GERAL"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2021_GERAL.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_CONTAT == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_ARRANH == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_LAMBED == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_MORDED == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_OUTRO_ == "1") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:9] <- apply(AUX[, 4:9], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2021_EXPOSICAO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2021_EXPOSICAO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2021_EXPOSICAO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_MUCOSA == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_CABECA == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_MAOS == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_TRONCO == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_MEMBRO== "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           ANT_MEMB_1== "1") %>%   
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:10] <- apply(AUX[, 4:10], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2021_LOCALIZACAO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2021_LOCALIZACAO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2021_LOCALIZACAO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FERIMENTO == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FERIMENTO == "2") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FERIMENTO == "3") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FERIMENTO == "9") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:8] <- apply(AUX[, 4:8], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2021_FERIMENTO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2021_FERIMENTO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2021_FERIMENTO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_PROFUN == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_SUPERF == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_DILACE == "1") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:7] <- apply(AUX[, 4:7], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2021_TIPO_FERIMENTO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2021_TIPO_FERIMENTO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2021_TIPO_FERIMENTO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          PRE_EXPOS == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          POS_EXPOS == "1") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:6] <- apply(AUX[, 4:6], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2021_ANTECEDENTE_ANTR"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2021_ANTECEDENTE_ANTR"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2021_ANTECEDENTE_ANTR.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANIMAL == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANIMAL == "2") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANIMAL == "3") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANIMAL == "4") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANIMAL == "5") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           ANIMAL == "6") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           ANIMAL == "7") %>%   
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:11] <- apply(AUX[, 4:11], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2021_AGRESSOR"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2021_AGRESSOR"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2021_AGRESSOR.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CONDIC_ANI == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CONDIC_ANI == "2") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CONDIC_ANI == "3") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CONDIC_ANI == "4") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:8] <- apply(AUX[, 4:8], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2021_COND_ANIMAL_ACID"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2021_COND_ANIMAL_ACID"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2021_COND_ANIMAL_ACID.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          OBSERVACAO == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          OBSERVACAO == "2") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:6] <- apply(AUX[, 4:6], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2021_OBSERVAVEL"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2021_OBSERVAVEL"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2021_OBSERVAVEL.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRAT_ATUAL == "2") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRAT_ATUAL == "3") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRAT_ATUAL == "4") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRAT_ATUAL == "5") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRAT_ATUAL == "6") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           TRAT_ATUAL == "7") %>%   
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:10] <- apply(AUX[, 4:10], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2021_TRATAMENTO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2021_TRATAMENTO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2021_TRATAMENTO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FIM_ANIMAL == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FIM_ANIMAL == "2") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FIM_ANIMAL == "3") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FIM_ANIMAL == "4") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FIM_ANIMAL == "5") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           FIM_ANIMAL == "9") %>%   
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:10] <- apply(AUX[, 4:10], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2021_COND_FINAL_ANIMAL"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2021_COND_FINAL_ANIMAL"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2021_COND_FINAL_ANIMAL.csv"), 
           row.names = FALSE)

###Tabela Interrupção de tratamento (campo 49 do SINAN)####

SINAN_ANTRAB_2021_INTERRUPCAO_TRAT <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

SINAN_ANTRAB_2021_INTERRUPCAO_TRAT$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

SINAN_ANTRAB_2021_INTERRUPCAO_TRAT$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

SINAN_ANTRAB_2021_INTERRUPCAO_TRAT$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

SINAN_ANTRAB_2021_INTERRUPCAO_TRAT <- SINAN_ANTRAB_2021_INTERRUPCAO_TRAT[,c(4, 1, 2, 3)]

SINAN_ANTRAB_2021_INTERRUPCAO_TRAT$Sim <- NA

SINAN_ANTRAB_2021_INTERRUPCAO_TRAT$Nao <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  SINAN_ANTRAB_2021_INTERRUPCAO_TRAT[which(SINAN_ANTRAB_2021_INTERRUPCAO_TRAT$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                                                                                 filter(ID_MN_RESI == i,
                                                                                                                        TRA_INTERR == "1") %>%   
                                                                                                                 count()
  )    
  
  SINAN_ANTRAB_2021_INTERRUPCAO_TRAT[which(SINAN_ANTRAB_2021_INTERRUPCAO_TRAT$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                                                                                 filter(ID_MN_RESI == i,
                                                                                                                        TRA_INTERR == "2") %>%   
                                                                                                                 count()
  )
}

SINAN_ANTRAB_2021_INTERRUPCAO_TRAT[(nrow(SINAN_ANTRAB_2021_INTERRUPCAO_TRAT) +1), 4:6] <- apply(SINAN_ANTRAB_2021_INTERRUPCAO_TRAT[, 4:6], 2, sum)
SINAN_ANTRAB_2021_INTERRUPCAO_TRAT[nrow(SINAN_ANTRAB_2021_INTERRUPCAO_TRAT), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2021_INTERRUPCAO_TRAT"), SINAN_ANTRAB_2021_INTERRUPCAO_TRAT)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2021_INTERRUPCAO_TRAT"), SINAN_ANTRAB_2021_INTERRUPCAO_TRAT), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2021_INTERRUPCAO_TRAT.csv"), 
           row.names = FALSE)

rm(SINAN_ANTRAB_2021_INTERRUPCAO_TRAT)

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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRA_INDI_N == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRA_INDI_N == "2") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRA_INDI_N == "9") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:7] <- apply(AUX[, 4:7], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2021_SOROTERAPIA"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2021_SOROTERAPIA"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2021_SOROTERAPIA.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TIP_SORO == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TIP_SORO== "2") %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:6] <- apply(AUX[, 4:6], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2021_TIPO_IMUNOBIOLOGICO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2021_TIPO_IMUNOBIOLOGICO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2021_TIPO_IMUNOBIOLOGICO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRA_INFILT == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2021 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRA_INFI_1 == "1") %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:6] <- apply(AUX[, 4:6], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2021_SORO_INFILTRACAO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2021_SORO_INFILTRACAO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2021_SORO_INFILTRACAO.csv"), 
           row.names = FALSE)

rm(SINAN_ANTRAB_2021, ANTRAB2021)

###################       2022      #########################################

####Criando um objeto com a base DBF do SINAN#################

ANTRAB2022 <- read.dbf(file = "Base_de_Dados/DBF/ANTRANET2022.DBF",
                       as.is = FALSE)


####Alterando o formato da coluna ID_MN_RESI de forma que ela seja passível de vincular no for loop#################

ANTRAB2022$ID_MN_RESI <- as.numeric(as.character(ANTRAB2022$ID_MN_RESI))

#####################################################################################################################
#################  Realizando contagem dos casos do Paraná para realizar a série histórica do Estado  ###############

PR_ANTRAB_Serie_Historica[11, 2] <- as.integer(ANTRAB2022 %>% 
                                                 count()
)
#####Filtrando os dados da Base DBF do SINAN com os dados de notificação somente da 22RS###########

SINAN_ANTRAB_2022 <- ANTRAB2022 %>% 
  filter(ID_REGIONA == ID_REG | ID_RG_RESI == ID_REG)

assign(paste0("RS", RS, "_ANTRAB_2022_SINAN"), SINAN_ANTRAB_2022)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2022_SINAN"), SINAN_ANTRAB_2022), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2022_SINAN.csv"), 
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
  
  AUX[which(AUX == i), 2] <- as.integer(SINAN_ANTRAB_2022 %>%
                                          filter(ID_MN_RESI == i,
                                                 SEM_PRI ==202201)%>%
                                          count()
  )
  
  AUX[which(AUX == i), 3] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                          filter(ID_MN_RESI == i, 
                                                 SEM_PRI ==202202) %>% 
                                          count()
  )
  
  AUX[which(AUX == i), 4] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                          filter(ID_MN_RESI == i,
                                                 SEM_PRI ==202203) %>% 
                                          count()
  )
  
  AUX[which(AUX == i),5] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                         filter(ID_MN_RESI == i,
                                                SEM_PRI ==202204) %>% 
                                         count()
  )
  
  AUX[which(AUX == i), 6] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                          filter(ID_MN_RESI == i,
                                                 SEM_PRI ==202205) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 7] <- as.integer(SINAN_ANTRAB_2022 %>%
                                          filter(ID_MN_RESI == i, 
                                                 SEM_PRI ==202206) %>%
                                          count()
  )
  
  AUX[which(AUX == i), 8] <- as.integer(SINAN_ANTRAB_2022 %>%
                                          filter(ID_MN_RESI == i, 
                                                 SEM_PRI ==202207) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 9] <- as.integer(SINAN_ANTRAB_2022 %>%
                                          filter(ID_MN_RESI == i, 
                                                 SEM_PRI ==202208) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 10] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202209) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 11] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202210) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 12] <- as.integer(SINAN_ANTRAB_2022 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202211) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 13] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202212) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 14] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202213) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 15] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202214) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 16] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202215) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 17] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202216) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 18] <- as.integer(SINAN_ANTRAB_2022 %>%
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202217) %>%  
                                           count() 
  )
  
  AUX[which(AUX == i), 19] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202218) %>%      
                                           count() 
  )
  
  AUX[which(AUX == i), 20] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202219) %>%
                                           count() 
  )
  
  AUX[which(AUX == i),  21] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                            filter(ID_MN_RESI == i, 
                                                   SEM_PRI ==202220) %>%
                                            count() 
  )
  
  AUX[which(AUX == i), 22] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202221) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 23] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202222) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 24] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202223) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 25] <- as.integer(SINAN_ANTRAB_2022 %>%
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202224) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 26] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202225) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 27] <- as.integer(SINAN_ANTRAB_2022 %>%
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202226) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 28] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202227) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 29] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202228) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 30] <- as.integer(SINAN_ANTRAB_2022 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202229) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 31] <- as.integer(SINAN_ANTRAB_2022 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202230) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 32] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202231) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 33] <- as.integer(SINAN_ANTRAB_2022 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202232) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 34] <- as.integer(SINAN_ANTRAB_2022 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202233) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 35] <- as.integer(SINAN_ANTRAB_2022 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202234) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 36] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202235) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 37] <- as.integer(SINAN_ANTRAB_2022 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202236) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 38] <- as.integer(SINAN_ANTRAB_2022 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202237) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 39] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202238) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 40] <- as.integer(SINAN_ANTRAB_2022 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202239) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 41] <- as.integer(SINAN_ANTRAB_2022 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202240) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 42] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202241) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 43] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202242) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 44] <- as.integer(SINAN_ANTRAB_2022 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202243) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 45] <- as.integer(SINAN_ANTRAB_2022 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202244) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 46] <- as.integer(SINAN_ANTRAB_2022 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202245) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 47] <- as.integer(SINAN_ANTRAB_2022 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202246) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 48] <- as.integer(SINAN_ANTRAB_2022 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202247) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 49] <- as.integer(SINAN_ANTRAB_2022 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202248) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 50] <- as.integer(SINAN_ANTRAB_2022 %>%
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202249) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 51] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202250) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 52] <- as.integer(SINAN_ANTRAB_2022 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202251) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 53] <- as.integer(SINAN_ANTRAB_2022 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202252) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 54] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202253) %>%
                                           count() 
  )
}

AUX[, 1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX[(nrow(AUX)+ 1), 2:54] <- apply(AUX[, 2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2022_SE_Notificados"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2022_SE_Notificados"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2022_SE_Notificados.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                   filter(ID_MN_RESI == i) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CS_ZONA == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CS_ZONA == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CS_ZONA == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CS_ZONA == 9 
                                                          |
                                                            is.na(CS_ZONA)) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_SEXO == "F") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_SEXO == "M") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                    filter(ID_MN_RESI == i,  
                                                           NU_IDADE_N <=3012) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                    filter(ID_MN_RESI == i,  
                                                           NU_IDADE_N > 4000 
                                                           & 
                                                             NU_IDADE_N <=4005) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           NU_IDADE_N > 4005 
                                                           & 
                                                             NU_IDADE_N <=4012) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           NU_IDADE_N > 4012 
                                                           & 
                                                             NU_IDADE_N <=4018) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 16] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           NU_IDADE_N > 4018 
                                                           & 
                                                             NU_IDADE_N <=4059) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 17] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           NU_IDADE_N > 4059) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 18] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "00") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 19] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "01" 
                                                           | 
                                                             CS_ESCOL_N == "02" 
                                                           | 
                                                             CS_ESCOL_N == "03") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 20] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "04") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 21] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "05") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 22] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "06") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 23] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "07") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 24] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "08") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 25] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "10") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 26] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "09") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 27] <- as.integer(SINAN_ANTRAB_2022 %>% 
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
  
  AUX[which(AUX$COD_IBGE == i), 28] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_GESTANT == "5") %>% 
                                                    count()
  )
  AUX[which(AUX$COD_IBGE == i), 29] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_GESTANT == "6") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 30] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_GESTANT == "9") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 31] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "01") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 32] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "2") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 33] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "3") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 34] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "4") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 35] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "5") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 36] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "9") %>% 
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:36] <- apply(AUX[, 4:36], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2022_GERAL"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2022_GERAL"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2022_GERAL.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_CONTAT == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_ARRANH == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_LAMBED == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_MORDED == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_OUTRO_ == "1") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:9] <- apply(AUX[, 4:9], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2022_EXPOSICAO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2022_EXPOSICAO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2022_EXPOSICAO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_MUCOSA == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_CABECA == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_MAOS == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_TRONCO == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_MEMBRO== "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           ANT_MEMB_1== "1") %>%   
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:10] <- apply(AUX[, 4:10], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2022_LOCALIZACAO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2022_LOCALIZACAO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2022_LOCALIZACAO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FERIMENTO == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FERIMENTO == "2") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FERIMENTO == "3") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FERIMENTO == "9") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:8] <- apply(AUX[, 4:8], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2022_FERIMENTO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2022_FERIMENTO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2022_FERIMENTO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_PROFUN == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_SUPERF == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_DILACE == "1") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:7] <- apply(AUX[, 4:7], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2022_TIPO_FERIMENTO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2022_TIPO_FERIMENTO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2022_TIPO_FERIMENTO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          PRE_EXPOS == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          POS_EXPOS == "1") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:6] <- apply(AUX[, 4:6], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2022_ANTECEDENTE_ANTR"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2022_ANTECEDENTE_ANTR"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2022_ANTECEDENTE_ANTR.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANIMAL == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANIMAL == "2") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANIMAL == "3") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANIMAL == "4") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANIMAL == "5") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           ANIMAL == "6") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           ANIMAL == "7") %>%   
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:11] <- apply(AUX[, 4:11], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2022_AGRESSOR"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2022_AGRESSOR"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2022_AGRESSOR.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CONDIC_ANI == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CONDIC_ANI == "2") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CONDIC_ANI == "3") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CONDIC_ANI == "4") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:8] <- apply(AUX[, 4:8], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2022_COND_ANIMAL_ACID"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2022_COND_ANIMAL_ACID"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2022_COND_ANIMAL_ACID.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          OBSERVACAO == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          OBSERVACAO == "2") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:6] <- apply(AUX[, 4:6], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2022_OBSERVAVEL"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2022_OBSERVAVEL"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2022_OBSERVAVEL.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRAT_ATUAL == "2") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRAT_ATUAL == "3") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRAT_ATUAL == "4") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRAT_ATUAL == "5") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRAT_ATUAL == "6") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           TRAT_ATUAL == "7") %>%   
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:10] <- apply(AUX[, 4:10], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2022_TRATAMENTO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2022_TRATAMENTO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2022_TRATAMENTO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FIM_ANIMAL == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FIM_ANIMAL == "2") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FIM_ANIMAL == "3") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FIM_ANIMAL == "4") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FIM_ANIMAL == "5") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           FIM_ANIMAL == "9") %>%   
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:10] <- apply(AUX[, 4:10], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2022_COND_FINAL_ANIMAL"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2022_COND_FINAL_ANIMAL"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2022_COND_FINAL_ANIMAL.csv"), 
           row.names = FALSE)

###Tabela Interrupção de tratamento (campo 49 do SINAN)####

SINAN_ANTRAB_2022_INTERRUPCAO_TRAT <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

SINAN_ANTRAB_2022_INTERRUPCAO_TRAT$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

SINAN_ANTRAB_2022_INTERRUPCAO_TRAT$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

SINAN_ANTRAB_2022_INTERRUPCAO_TRAT$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

SINAN_ANTRAB_2022_INTERRUPCAO_TRAT <- SINAN_ANTRAB_2022_INTERRUPCAO_TRAT[,c(4, 1, 2, 3)]

SINAN_ANTRAB_2022_INTERRUPCAO_TRAT$Sim <- NA

SINAN_ANTRAB_2022_INTERRUPCAO_TRAT$Nao <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  SINAN_ANTRAB_2022_INTERRUPCAO_TRAT[which(SINAN_ANTRAB_2022_INTERRUPCAO_TRAT$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                                                                                 filter(ID_MN_RESI == i,
                                                                                                                        TRA_INTERR == "1") %>%   
                                                                                                                 count()
  )    
  
  SINAN_ANTRAB_2022_INTERRUPCAO_TRAT[which(SINAN_ANTRAB_2022_INTERRUPCAO_TRAT$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                                                                                 filter(ID_MN_RESI == i,
                                                                                                                        TRA_INTERR == "2") %>%   
                                                                                                                 count()
  )
}

SINAN_ANTRAB_2022_INTERRUPCAO_TRAT[(nrow(SINAN_ANTRAB_2022_INTERRUPCAO_TRAT) +1), 4:6] <- apply(SINAN_ANTRAB_2022_INTERRUPCAO_TRAT[, 4:6], 2, sum)
SINAN_ANTRAB_2022_INTERRUPCAO_TRAT[nrow(SINAN_ANTRAB_2022_INTERRUPCAO_TRAT), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2022_INTERRUPCAO_TRAT"), SINAN_ANTRAB_2022_INTERRUPCAO_TRAT)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2022_INTERRUPCAO_TRAT"), SINAN_ANTRAB_2022_INTERRUPCAO_TRAT), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2022_INTERRUPCAO_TRAT.csv"), 
           row.names = FALSE)

rm(SINAN_ANTRAB_2022_INTERRUPCAO_TRAT)

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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRA_INDI_N == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRA_INDI_N == "2") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRA_INDI_N == "9") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:7] <- apply(AUX[, 4:7], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2022_SOROTERAPIA"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2022_SOROTERAPIA"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2022_SOROTERAPIA.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TIP_SORO == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TIP_SORO== "2") %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:6] <- apply(AUX[, 4:6], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2022_TIPO_IMUNOBIOLOGICO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2022_TIPO_IMUNOBIOLOGICO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2022_TIPO_IMUNOBIOLOGICO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRA_INFILT == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2022 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRA_INFI_1 == "1") %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:6] <- apply(AUX[, 4:6], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2022_SORO_INFILTRACAO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2022_SORO_INFILTRACAO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2022_SORO_INFILTRACAO.csv"), 
           row.names = FALSE)

rm(SINAN_ANTRAB_2022, ANTRAB2022)

###################       2023      #########################################

####Criando um objeto com a base DBF do SINAN#################

ANTRAB2023 <- read.dbf(file = "Base_de_Dados/DBF/ANTRANET2023.DBF",
                       as.is = FALSE)


####Alterando o formato da coluna ID_MN_RESI de forma que ela seja passível de vincular no for loop#################

ANTRAB2023$ID_MN_RESI <- as.numeric(as.character(ANTRAB2023$ID_MN_RESI))

#####################################################################################################################
#################  Realizando contagem dos casos do Paraná para realizar a série histórica do Estado  ###############

PR_ANTRAB_Serie_Historica[12, 2] <- as.integer(ANTRAB2023 %>% 
                                                 count()
)
#####Filtrando os dados da Base DBF do SINAN com os dados de notificação somente da 22RS###########

SINAN_ANTRAB_2023 <- ANTRAB2023 %>% 
  filter(ID_REGIONA == ID_REG | ID_RG_RESI == ID_REG)

assign(paste0("RS", RS, "_ANTRAB_2023_SINAN"), SINAN_ANTRAB_2023)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2023_SINAN"), SINAN_ANTRAB_2023), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2023_SINAN.csv"), 
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
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2023_SE_Notificados.csv"), 
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
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2023_GERAL.csv"), 
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
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2023_EXPOSICAO.csv"), 
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
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2023_LOCALIZACAO.csv"), 
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
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2023_FERIMENTO.csv"), 
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
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2023_TIPO_FERIMENTO.csv"), 
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
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2023_ANTECEDENTE_ANTR.csv"), 
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
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2023_AGRESSOR.csv"), 
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
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2023_COND_ANIMAL_ACID.csv"), 
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
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2023_OBSERVAVEL.csv"), 
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
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2023_TRATAMENTO.csv"), 
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
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2023_COND_FINAL_ANIMAL.csv"), 
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

SINAN_ANTRAB_2023_INTERRUPCAO_TRAT[(nrow(SINAN_ANTRAB_2023_INTERRUPCAO_TRAT) +1), 4:6] <- apply(SINAN_ANTRAB_2023_INTERRUPCAO_TRAT[, 4:6], 2, sum)
SINAN_ANTRAB_2023_INTERRUPCAO_TRAT[nrow(SINAN_ANTRAB_2023_INTERRUPCAO_TRAT), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2023_INTERRUPCAO_TRAT"), SINAN_ANTRAB_2023_INTERRUPCAO_TRAT)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2023_INTERRUPCAO_TRAT"), SINAN_ANTRAB_2023_INTERRUPCAO_TRAT), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2023_INTERRUPCAO_TRAT.csv"), 
           row.names = FALSE)

rm(SINAN_ANTRAB_2023_INTERRUPCAO_TRAT)

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
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2023_SOROTERAPIA.csv"), 
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
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2023_TIPO_IMUNOBIOLOGICO.csv"), 
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
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2023_SORO_INFILTRACAO.csv"), 
           row.names = FALSE)

rm(SINAN_ANTRAB_2023, ANTRAB2023)

###################       2024      #########################################

####Criando um objeto com a base DBF do SINAN#################

ANTRAB2024 <- read.dbf(file = "Base_de_Dados/DBF/ANTRANET2024.DBF",
                       as.is = FALSE)


####Alterando o formato da coluna ID_MN_RESI de forma que ela seja passível de vincular no for loop#################

ANTRAB2024$ID_MN_RESI <- as.numeric(as.character(ANTRAB2024$ID_MN_RESI))

#####################################################################################################################
#################  Realizando contagem dos casos do Paraná para realizar a série histórica do Estado  ###############

PR_ANTRAB_Serie_Historica[13, 2] <- as.integer(ANTRAB2024 %>% 
                                                 count()
)
#####Filtrando os dados da Base DBF do SINAN com os dados de notificação somente da 22RS###########

SINAN_ANTRAB_2024 <- ANTRAB2024 %>% 
  filter(ID_REGIONA == ID_REG | ID_RG_RESI == ID_REG)

assign(paste0("RS", RS, "_ANTRAB_2024_SINAN"), SINAN_ANTRAB_2024)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2024_SINAN"), SINAN_ANTRAB_2024), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2024_SINAN.csv"), 
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
  
  AUX[which(AUX == i), 2] <- as.integer(SINAN_ANTRAB_2024 %>%
                                          filter(ID_MN_RESI == i,
                                                 SEM_PRI ==202401)%>%
                                          count()
  )
  
  AUX[which(AUX == i), 3] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                          filter(ID_MN_RESI == i, 
                                                 SEM_PRI ==202402) %>% 
                                          count()
  )
  
  AUX[which(AUX == i), 4] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                          filter(ID_MN_RESI == i,
                                                 SEM_PRI ==202403) %>% 
                                          count()
  )
  
  AUX[which(AUX == i),5] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                         filter(ID_MN_RESI == i,
                                                SEM_PRI ==202404) %>% 
                                         count()
  )
  
  AUX[which(AUX == i), 6] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                          filter(ID_MN_RESI == i,
                                                 SEM_PRI ==202405) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 7] <- as.integer(SINAN_ANTRAB_2024 %>%
                                          filter(ID_MN_RESI == i, 
                                                 SEM_PRI ==202406) %>%
                                          count()
  )
  
  AUX[which(AUX == i), 8] <- as.integer(SINAN_ANTRAB_2024 %>%
                                          filter(ID_MN_RESI == i, 
                                                 SEM_PRI ==202407) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 9] <- as.integer(SINAN_ANTRAB_2024 %>%
                                          filter(ID_MN_RESI == i, 
                                                 SEM_PRI ==202408) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 10] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202409) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 11] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202410) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 12] <- as.integer(SINAN_ANTRAB_2024 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202411) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 13] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202412) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 14] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202413) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 15] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202414) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 16] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202415) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 17] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202416) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 18] <- as.integer(SINAN_ANTRAB_2024 %>%
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202417) %>%  
                                           count() 
  )
  
  AUX[which(AUX == i), 19] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202418) %>%      
                                           count() 
  )
  
  AUX[which(AUX == i), 20] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202419) %>%
                                           count() 
  )
  
  AUX[which(AUX == i),  21] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                            filter(ID_MN_RESI == i, 
                                                   SEM_PRI ==202420) %>%
                                            count() 
  )
  
  AUX[which(AUX == i), 22] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202421) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 23] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202422) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 24] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202423) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 25] <- as.integer(SINAN_ANTRAB_2024 %>%
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202424) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 26] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202425) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 27] <- as.integer(SINAN_ANTRAB_2024 %>%
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202426) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 28] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202427) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 29] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202428) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 30] <- as.integer(SINAN_ANTRAB_2024 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202429) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 31] <- as.integer(SINAN_ANTRAB_2024 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202430) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 32] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202431) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 33] <- as.integer(SINAN_ANTRAB_2024 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202432) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 34] <- as.integer(SINAN_ANTRAB_2024 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202433) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 35] <- as.integer(SINAN_ANTRAB_2024 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202434) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 36] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202435) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 37] <- as.integer(SINAN_ANTRAB_2024 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202436) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 38] <- as.integer(SINAN_ANTRAB_2024 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202437) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 39] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202438) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 40] <- as.integer(SINAN_ANTRAB_2024 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202439) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 41] <- as.integer(SINAN_ANTRAB_2024 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202440) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 42] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202441) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 43] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202442) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 44] <- as.integer(SINAN_ANTRAB_2024 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202443) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 45] <- as.integer(SINAN_ANTRAB_2024 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202444) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 46] <- as.integer(SINAN_ANTRAB_2024 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202445) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 47] <- as.integer(SINAN_ANTRAB_2024 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202446) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 48] <- as.integer(SINAN_ANTRAB_2024 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202447) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 49] <- as.integer(SINAN_ANTRAB_2024 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202448) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 50] <- as.integer(SINAN_ANTRAB_2024 %>%
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202449) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 51] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202450) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 52] <- as.integer(SINAN_ANTRAB_2024 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202451) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 53] <- as.integer(SINAN_ANTRAB_2024 %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202452) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 54] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202453) %>%
                                           count() 
  )
}

AUX[, 1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX[(nrow(AUX)+ 1), 2:54] <- apply(AUX[, 2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2024_SE_Notificados"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2024_SE_Notificados"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2024_SE_Notificados.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                   filter(ID_MN_RESI == i) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CS_ZONA == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CS_ZONA == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CS_ZONA == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CS_ZONA == 9 
                                                          |
                                                            is.na(CS_ZONA)) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_SEXO == "F") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_SEXO == "M") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                    filter(ID_MN_RESI == i,  
                                                           NU_IDADE_N <=3012) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                    filter(ID_MN_RESI == i,  
                                                           NU_IDADE_N > 4000 
                                                           & 
                                                             NU_IDADE_N <=4005) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           NU_IDADE_N > 4005 
                                                           & 
                                                             NU_IDADE_N <=4012) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           NU_IDADE_N > 4012 
                                                           & 
                                                             NU_IDADE_N <=4018) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 16] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           NU_IDADE_N > 4018 
                                                           & 
                                                             NU_IDADE_N <=4059) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 17] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           NU_IDADE_N > 4059) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 18] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "00") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 19] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "01" 
                                                           | 
                                                             CS_ESCOL_N == "02" 
                                                           | 
                                                             CS_ESCOL_N == "03") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 20] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "04") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 21] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "05") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 22] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "06") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 23] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "07") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 24] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "08") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 25] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "10") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 26] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ESCOL_N == "09") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 27] <- as.integer(SINAN_ANTRAB_2024 %>% 
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
  
  AUX[which(AUX$COD_IBGE == i), 28] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_GESTANT == "5") %>% 
                                                    count()
  )
  AUX[which(AUX$COD_IBGE == i), 29] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_GESTANT == "6") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 30] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_GESTANT == "9") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 31] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "01") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 32] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "2") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 33] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "3") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 34] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "4") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 35] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "5") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 36] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_RACA == "9") %>% 
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:36] <- apply(AUX[, 4:36], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2024_GERAL"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2024_GERAL"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2024_GERAL.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_CONTAT == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_ARRANH == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_LAMBED == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_MORDED == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_OUTRO_ == "1") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:9] <- apply(AUX[, 4:9], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2024_EXPOSICAO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2024_EXPOSICAO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2024_EXPOSICAO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_MUCOSA == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_CABECA == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_MAOS == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_TRONCO == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_MEMBRO== "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           ANT_MEMB_1== "1") %>%   
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:10] <- apply(AUX[, 4:10], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2024_LOCALIZACAO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2024_LOCALIZACAO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2024_LOCALIZACAO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FERIMENTO == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FERIMENTO == "2") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FERIMENTO == "3") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FERIMENTO == "9") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:8] <- apply(AUX[, 4:8], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2024_FERIMENTO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2024_FERIMENTO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2024_FERIMENTO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_PROFUN == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_SUPERF == "1") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANT_DILACE == "1") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:7] <- apply(AUX[, 4:7], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2024_TIPO_FERIMENTO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2024_TIPO_FERIMENTO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2024_TIPO_FERIMENTO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          PRE_EXPOS == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          POS_EXPOS == "1") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:6] <- apply(AUX[, 4:6], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2024_ANTECEDENTE_ANTR"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2024_ANTECEDENTE_ANTR"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2024_ANTECEDENTE_ANTR.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANIMAL == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANIMAL == "2") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANIMAL == "3") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANIMAL == "4") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          ANIMAL == "5") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           ANIMAL == "6") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           ANIMAL == "7") %>%   
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:11] <- apply(AUX[, 4:11], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2024_AGRESSOR"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2024_AGRESSOR"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2024_AGRESSOR.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CONDIC_ANI == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CONDIC_ANI == "2") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CONDIC_ANI == "3") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          CONDIC_ANI == "4") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:8] <- apply(AUX[, 4:8], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2024_COND_ANIMAL_ACID"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2024_COND_ANIMAL_ACID"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2024_COND_ANIMAL_ACID.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          OBSERVACAO == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          OBSERVACAO == "2") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:6] <- apply(AUX[, 4:6], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2024_OBSERVAVEL"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2024_OBSERVAVEL"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2024_OBSERVAVEL.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRAT_ATUAL == "2") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRAT_ATUAL == "3") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRAT_ATUAL == "4") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRAT_ATUAL == "5") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRAT_ATUAL == "6") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           TRAT_ATUAL == "7") %>%   
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:10] <- apply(AUX[, 4:10], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2024_TRATAMENTO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2024_TRATAMENTO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2024_TRATAMENTO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FIM_ANIMAL == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FIM_ANIMAL == "2") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FIM_ANIMAL == "3") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FIM_ANIMAL == "4") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          FIM_ANIMAL == "5") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                    filter(ID_MN_RESI == i,
                                                           FIM_ANIMAL == "9") %>%   
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:10] <- apply(AUX[, 4:10], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2024_COND_FINAL_ANIMAL"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2024_COND_FINAL_ANIMAL"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2024_COND_FINAL_ANIMAL.csv"), 
           row.names = FALSE)

###Tabela Interrupção de tratamento (campo 49 do SINAN)####

SINAN_ANTRAB_2024_INTERRUPCAO_TRAT <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

SINAN_ANTRAB_2024_INTERRUPCAO_TRAT$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

SINAN_ANTRAB_2024_INTERRUPCAO_TRAT$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

SINAN_ANTRAB_2024_INTERRUPCAO_TRAT$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

SINAN_ANTRAB_2024_INTERRUPCAO_TRAT <- SINAN_ANTRAB_2024_INTERRUPCAO_TRAT[,c(4, 1, 2, 3)]

SINAN_ANTRAB_2024_INTERRUPCAO_TRAT$Sim <- NA

SINAN_ANTRAB_2024_INTERRUPCAO_TRAT$Nao <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  SINAN_ANTRAB_2024_INTERRUPCAO_TRAT[which(SINAN_ANTRAB_2024_INTERRUPCAO_TRAT$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                                                                                 filter(ID_MN_RESI == i,
                                                                                                                        TRA_INTERR == "1") %>%   
                                                                                                                 count()
  )    
  
  SINAN_ANTRAB_2024_INTERRUPCAO_TRAT[which(SINAN_ANTRAB_2024_INTERRUPCAO_TRAT$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                                                                                 filter(ID_MN_RESI == i,
                                                                                                                        TRA_INTERR == "2") %>%   
                                                                                                                 count()
  )
}

SINAN_ANTRAB_2024_INTERRUPCAO_TRAT[(nrow(SINAN_ANTRAB_2024_INTERRUPCAO_TRAT) +1), 4:6] <- apply(SINAN_ANTRAB_2024_INTERRUPCAO_TRAT[, 4:6], 2, sum)
SINAN_ANTRAB_2024_INTERRUPCAO_TRAT[nrow(SINAN_ANTRAB_2024_INTERRUPCAO_TRAT), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2024_INTERRUPCAO_TRAT"), SINAN_ANTRAB_2024_INTERRUPCAO_TRAT)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2024_INTERRUPCAO_TRAT"), SINAN_ANTRAB_2024_INTERRUPCAO_TRAT), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2024_INTERRUPCAO_TRAT.csv"), 
           row.names = FALSE)

rm(SINAN_ANTRAB_2024_INTERRUPCAO_TRAT)

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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRA_INDI_N == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRA_INDI_N == "2") %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRA_INDI_N == "9") %>%   
                                                   count()
  )
}

AUX[(nrow(AUX) +1), 4:7] <- apply(AUX[, 4:7], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2024_SOROTERAPIA"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2024_SOROTERAPIA"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2024_SOROTERAPIA.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TIP_SORO == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TIP_SORO== "2") %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:6] <- apply(AUX[, 4:6], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2024_TIPO_IMUNOBIOLOGICO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2024_TIPO_IMUNOBIOLOGICO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2024_TIPO_IMUNOBIOLOGICO.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRA_INFILT == "1") %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2024 %>% 
                                                   filter(ID_MN_RESI == i,
                                                          TRA_INFI_1 == "1") %>%   
                                                   count()
  )
  
}

AUX[(nrow(AUX) +1), 4:6] <- apply(AUX[, 4:6], 2, sum)
AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_ANTRAB_2024_SORO_INFILTRACAO"), AUX)

write.csv (assign(paste0("RS", RS, "_ANTRAB_2024_SORO_INFILTRACAO"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", RS, "_ANTRAB_2024_SORO_INFILTRACAO.csv"), 
           row.names = FALSE)

rm(SINAN_ANTRAB_2024, ANTRAB2024)

########   Salvando o objeto PR_ANTRAB_Serie_Historica    ######

write.csv (PR_ANTRAB_Serie_Historica, 
           "Tabulacoes_R/Raiva/PR_ANTRAB_BASE_Serie_Historica.csv", 
           row.names = FALSE)

################################################################################################################################
################################################################################################################################

###Tabela Série Histórica Regional###

AUX <- data.frame(RS22 = "RS22", RS22_2012 = NA, RS22_2013 = NA, 
                  RS22_2014 = NA, RS22_2015 = NA, RS22_2016 = NA, 
                  RS22_2017 = NA, RS22_2018 = NA, RS22_2019 = NA, 
                  RS22_2020 = NA, RS22_2021 = NA, RS22_2022 = NA, 
                  RS22_2023 = NA, RS22_2024 = NA, RS22_2025 = NA)

AUX[1, 1] <- "Notificados"
AUX[2, 1] <- "Zona_Urbana"
AUX[3, 1] <- "Zona_Rural"
AUX[4, 1] <- "Zona_Periurbana"
AUX[5, 1] <- "Zona_Ignorados"
AUX[6, 1] <- "Feminino"
AUX[7, 1] <- "Masculino"
AUX[8, 1] <- "Menos_1_ano"
AUX[9, 1] <- "Um_a_cinco"
AUX[10, 1] <- "Cinco_a_doze"
AUX[11, 1] <- "Doze_dezoito"
AUX[12, 1] <- "Dezoito_cinquentaenove"
AUX[13, 1] <- "Mais_de_Sessenta"
AUX[14, 1] <- "Analfabeto"
AUX[15, 1] <- "Fundamental_Incompleto"
AUX[16, 1] <- "Fundamental_Completo"
AUX[17, 1] <- "Ensino_medio_Incompleto"
AUX[18, 1] <- "Ensino_Medio_Completo"
AUX[19, 1] <- "Superior_Incompleto"
AUX[20, 1] <- "Superior_Completo"
AUX[21, 1] <- "ESC_NA"
AUX[22, 1] <- "Esc_Ignorado"
AUX[23, 1] <- "Gestante"
AUX[24, 1] <- "Não_Gestante"

AUX[1, 2] <- RS22_ANTRAB_2012_GERAL[17, 5]
AUX[2, 2] <- RS22_ANTRAB_2012_GERAL[17, 6]
AUX[3, 2] <- RS22_ANTRAB_2012_GERAL[17, 7]
AUX[4, 2] <- RS22_ANTRAB_2012_GERAL[17, 8]
AUX[5, 2] <- RS22_ANTRAB_2012_GERAL[17, 9]
AUX[6, 2] <- RS22_ANTRAB_2012_GERAL[17, 10]
AUX[7, 2] <- RS22_ANTRAB_2012_GERAL[17, 11]
AUX[8, 2] <- RS22_ANTRAB_2012_GERAL[17, 12]
AUX[9, 2] <- RS22_ANTRAB_2012_GERAL[17, 13]
AUX[10, 2] <- RS22_ANTRAB_2012_GERAL[17, 14]
AUX[11, 2] <- RS22_ANTRAB_2012_GERAL[17, 15]
AUX[12, 2] <- RS22_ANTRAB_2012_GERAL[17, 16]
AUX[13, 2] <- RS22_ANTRAB_2012_GERAL[17, 17]
AUX[14, 2] <- RS22_ANTRAB_2012_GERAL[17, 18]
AUX[15, 2] <- RS22_ANTRAB_2012_GERAL[17, 19]
AUX[16, 2] <- RS22_ANTRAB_2012_GERAL[17, 20]
AUX[17, 2] <- RS22_ANTRAB_2012_GERAL[17, 21]
AUX[18, 2] <- RS22_ANTRAB_2012_GERAL[17, 22]
AUX[19, 2] <- RS22_ANTRAB_2012_GERAL[17, 23]
AUX[20, 2] <- RS22_ANTRAB_2012_GERAL[17, 24]
AUX[21, 2] <- RS22_ANTRAB_2012_GERAL[17, 25]
AUX[22, 2] <- RS22_ANTRAB_2012_GERAL[17, 26]
AUX[23, 2] <- RS22_ANTRAB_2012_GERAL[17, 27]
AUX[24, 2] <- RS22_ANTRAB_2012_GERAL[17, 28]

AUX[1, 3] <- RS22_ANTRAB_2013_GERAL[17, 5]
AUX[2, 3] <- RS22_ANTRAB_2013_GERAL[17, 6]
AUX[3, 3] <- RS22_ANTRAB_2013_GERAL[17, 7]
AUX[4, 3] <- RS22_ANTRAB_2013_GERAL[17, 8]
AUX[5, 3] <- RS22_ANTRAB_2013_GERAL[17, 9]
AUX[6, 3] <- RS22_ANTRAB_2013_GERAL[17, 10]
AUX[7, 3] <- RS22_ANTRAB_2013_GERAL[17, 11]
AUX[8, 3] <- RS22_ANTRAB_2013_GERAL[17, 12]
AUX[9, 3] <- RS22_ANTRAB_2013_GERAL[17, 13]
AUX[10, 3] <- RS22_ANTRAB_2013_GERAL[17, 14]
AUX[11, 3] <- RS22_ANTRAB_2013_GERAL[17, 15]
AUX[12, 3] <- RS22_ANTRAB_2013_GERAL[17, 16]
AUX[13, 3] <- RS22_ANTRAB_2013_GERAL[17, 17]
AUX[14, 3] <- RS22_ANTRAB_2013_GERAL[17, 18]
AUX[15, 3] <- RS22_ANTRAB_2013_GERAL[17, 19]
AUX[16, 3] <- RS22_ANTRAB_2013_GERAL[17, 20]
AUX[17, 3] <- RS22_ANTRAB_2013_GERAL[17, 21]
AUX[18, 3] <- RS22_ANTRAB_2013_GERAL[17, 22]
AUX[19, 3] <- RS22_ANTRAB_2013_GERAL[17, 23]
AUX[20, 3] <- RS22_ANTRAB_2013_GERAL[17, 24]
AUX[21, 3] <- RS22_ANTRAB_2013_GERAL[17, 25]
AUX[22, 3] <- RS22_ANTRAB_2013_GERAL[17, 26]
AUX[23, 3] <- RS22_ANTRAB_2013_GERAL[17, 27]
AUX[24, 3] <- RS22_ANTRAB_2013_GERAL[17, 28]

AUX[1, 4] <- RS22_ANTRAB_2014_GERAL[17, 5]
AUX[2, 4] <- RS22_ANTRAB_2014_GERAL[17, 6]
AUX[3, 4] <- RS22_ANTRAB_2014_GERAL[17, 7]
AUX[4, 4] <- RS22_ANTRAB_2014_GERAL[17, 8]
AUX[5, 4] <- RS22_ANTRAB_2014_GERAL[17, 9]
AUX[6, 4] <- RS22_ANTRAB_2014_GERAL[17, 10]
AUX[7, 4] <- RS22_ANTRAB_2014_GERAL[17, 11]
AUX[8, 4] <- RS22_ANTRAB_2014_GERAL[17, 12]
AUX[9, 4] <- RS22_ANTRAB_2014_GERAL[17, 13]
AUX[10, 4] <- RS22_ANTRAB_2014_GERAL[17, 14]
AUX[11, 4] <- RS22_ANTRAB_2014_GERAL[17, 15]
AUX[12, 4] <- RS22_ANTRAB_2014_GERAL[17, 16]
AUX[13, 4] <- RS22_ANTRAB_2014_GERAL[17, 17]
AUX[14, 4] <- RS22_ANTRAB_2014_GERAL[17, 18]
AUX[15, 4] <- RS22_ANTRAB_2014_GERAL[17, 19]
AUX[16, 4] <- RS22_ANTRAB_2014_GERAL[17, 20]
AUX[17, 4] <- RS22_ANTRAB_2014_GERAL[17, 21]
AUX[18, 4] <- RS22_ANTRAB_2014_GERAL[17, 22]
AUX[19, 4] <- RS22_ANTRAB_2014_GERAL[17, 23]
AUX[20, 4] <- RS22_ANTRAB_2014_GERAL[17, 24]
AUX[21, 4] <- RS22_ANTRAB_2014_GERAL[17, 25]
AUX[22, 4] <- RS22_ANTRAB_2014_GERAL[17, 26]
AUX[23, 4] <- RS22_ANTRAB_2014_GERAL[17, 27]
AUX[24, 4] <- RS22_ANTRAB_2014_GERAL[17, 28]

AUX[1, 5] <- RS22_ANTRAB_2015_GERAL[17, 5]
AUX[2, 5] <- RS22_ANTRAB_2015_GERAL[17, 6]
AUX[3, 5] <- RS22_ANTRAB_2015_GERAL[17, 7]
AUX[4, 5] <- RS22_ANTRAB_2015_GERAL[17, 8]
AUX[5, 5] <- RS22_ANTRAB_2015_GERAL[17, 9]
AUX[6, 5] <- RS22_ANTRAB_2015_GERAL[17, 10]
AUX[7, 5] <- RS22_ANTRAB_2015_GERAL[17, 11]
AUX[8, 5] <- RS22_ANTRAB_2015_GERAL[17, 12]
AUX[9, 5] <- RS22_ANTRAB_2015_GERAL[17, 13]
AUX[10, 5] <- RS22_ANTRAB_2015_GERAL[17, 14]
AUX[11, 5] <- RS22_ANTRAB_2015_GERAL[17, 15]
AUX[12, 5] <- RS22_ANTRAB_2015_GERAL[17, 16]
AUX[13, 5] <- RS22_ANTRAB_2015_GERAL[17, 17]
AUX[14, 5] <- RS22_ANTRAB_2015_GERAL[17, 18]
AUX[15, 5] <- RS22_ANTRAB_2015_GERAL[17, 19]
AUX[16, 5] <- RS22_ANTRAB_2015_GERAL[17, 20]
AUX[17, 5] <- RS22_ANTRAB_2015_GERAL[17, 21]
AUX[18, 5] <- RS22_ANTRAB_2015_GERAL[17, 22]
AUX[19, 5] <- RS22_ANTRAB_2015_GERAL[17, 23]
AUX[20, 5] <- RS22_ANTRAB_2015_GERAL[17, 24]
AUX[21, 5] <- RS22_ANTRAB_2015_GERAL[17, 25]
AUX[22, 5] <- RS22_ANTRAB_2015_GERAL[17, 26]
AUX[23, 5] <- RS22_ANTRAB_2015_GERAL[17, 27]
AUX[24, 5] <- RS22_ANTRAB_2015_GERAL[17, 28]

AUX[1, 6] <- RS22_ANTRAB_2016_GERAL[17, 5]
AUX[2, 6] <- RS22_ANTRAB_2016_GERAL[17, 6]
AUX[3, 6] <- RS22_ANTRAB_2016_GERAL[17, 7]
AUX[4, 6] <- RS22_ANTRAB_2016_GERAL[17, 8]
AUX[5, 6] <- RS22_ANTRAB_2016_GERAL[17, 9]
AUX[6, 6] <- RS22_ANTRAB_2016_GERAL[17, 10]
AUX[7, 6] <- RS22_ANTRAB_2016_GERAL[17, 11]
AUX[8, 6] <- RS22_ANTRAB_2016_GERAL[17, 12]
AUX[9, 6] <- RS22_ANTRAB_2016_GERAL[17, 13]
AUX[10, 6] <- RS22_ANTRAB_2016_GERAL[17, 14]
AUX[11, 6] <- RS22_ANTRAB_2016_GERAL[17, 15]
AUX[12, 6] <- RS22_ANTRAB_2016_GERAL[17, 16]
AUX[13, 6] <- RS22_ANTRAB_2016_GERAL[17, 17]
AUX[14, 6] <- RS22_ANTRAB_2016_GERAL[17, 18]
AUX[15, 6] <- RS22_ANTRAB_2016_GERAL[17, 19]
AUX[16, 6] <- RS22_ANTRAB_2016_GERAL[17, 20]
AUX[17, 6] <- RS22_ANTRAB_2016_GERAL[17, 21]
AUX[18, 6] <- RS22_ANTRAB_2016_GERAL[17, 22]
AUX[19, 6] <- RS22_ANTRAB_2016_GERAL[17, 23]
AUX[20, 6] <- RS22_ANTRAB_2016_GERAL[17, 24]
AUX[21, 6] <- RS22_ANTRAB_2016_GERAL[17, 25]
AUX[22, 6] <- RS22_ANTRAB_2016_GERAL[17, 26]
AUX[23, 6] <- RS22_ANTRAB_2016_GERAL[17, 27]
AUX[24, 6] <- RS22_ANTRAB_2016_GERAL[17, 28]

AUX[1, 7] <- RS22_ANTRAB_2017_GERAL[17, 5]
AUX[2, 7] <- RS22_ANTRAB_2017_GERAL[17, 6]
AUX[3, 7] <- RS22_ANTRAB_2017_GERAL[17, 7]
AUX[4, 7] <- RS22_ANTRAB_2017_GERAL[17, 8]
AUX[5, 7] <- RS22_ANTRAB_2017_GERAL[17, 9]
AUX[6, 7] <- RS22_ANTRAB_2017_GERAL[17, 10]
AUX[7, 7] <- RS22_ANTRAB_2017_GERAL[17, 11]
AUX[8, 7] <- RS22_ANTRAB_2017_GERAL[17, 12]
AUX[9, 7] <- RS22_ANTRAB_2017_GERAL[17, 13]
AUX[10, 7] <- RS22_ANTRAB_2017_GERAL[17, 14]
AUX[11, 7] <- RS22_ANTRAB_2017_GERAL[17, 15]
AUX[12, 7] <- RS22_ANTRAB_2017_GERAL[17, 16]
AUX[13, 7] <- RS22_ANTRAB_2017_GERAL[17, 17]
AUX[14, 7] <- RS22_ANTRAB_2017_GERAL[17, 18]
AUX[15, 7] <- RS22_ANTRAB_2017_GERAL[17, 19]
AUX[16, 7] <- RS22_ANTRAB_2017_GERAL[17, 20]
AUX[17, 7] <- RS22_ANTRAB_2017_GERAL[17, 21]
AUX[18, 7] <- RS22_ANTRAB_2017_GERAL[17, 22]
AUX[19, 7] <- RS22_ANTRAB_2017_GERAL[17, 23]
AUX[20, 7] <- RS22_ANTRAB_2017_GERAL[17, 24]
AUX[21, 7] <- RS22_ANTRAB_2017_GERAL[17, 25]
AUX[22, 7] <- RS22_ANTRAB_2017_GERAL[17, 26]
AUX[23, 7] <- RS22_ANTRAB_2017_GERAL[17, 27]
AUX[24, 7] <- RS22_ANTRAB_2017_GERAL[17, 28]

AUX[1, 8] <- RS22_ANTRAB_2018_GERAL[17, 5]
AUX[2, 8] <- RS22_ANTRAB_2018_GERAL[17, 6]
AUX[3, 8] <- RS22_ANTRAB_2018_GERAL[17, 7]
AUX[4, 8] <- RS22_ANTRAB_2018_GERAL[17, 8]
AUX[5, 8] <- RS22_ANTRAB_2018_GERAL[17, 9]
AUX[6, 8] <- RS22_ANTRAB_2018_GERAL[17, 10]
AUX[7, 8] <- RS22_ANTRAB_2018_GERAL[17, 11]
AUX[8, 8] <- RS22_ANTRAB_2018_GERAL[17, 12]
AUX[9, 8] <- RS22_ANTRAB_2018_GERAL[17, 13]
AUX[10, 8] <- RS22_ANTRAB_2018_GERAL[17, 14]
AUX[11, 8] <- RS22_ANTRAB_2018_GERAL[17, 15]
AUX[12, 8] <- RS22_ANTRAB_2018_GERAL[17, 16]
AUX[13, 8] <- RS22_ANTRAB_2018_GERAL[17, 17]
AUX[14, 8] <- RS22_ANTRAB_2018_GERAL[17, 18]
AUX[15, 8] <- RS22_ANTRAB_2018_GERAL[17, 19]
AUX[16, 8] <- RS22_ANTRAB_2018_GERAL[17, 20]
AUX[17, 8] <- RS22_ANTRAB_2018_GERAL[17, 21]
AUX[18, 8] <- RS22_ANTRAB_2018_GERAL[17, 22]
AUX[19, 8] <- RS22_ANTRAB_2018_GERAL[17, 23]
AUX[20, 8] <- RS22_ANTRAB_2018_GERAL[17, 24]
AUX[21, 8] <- RS22_ANTRAB_2018_GERAL[17, 25]
AUX[22, 8] <- RS22_ANTRAB_2018_GERAL[17, 26]
AUX[23, 8] <- RS22_ANTRAB_2018_GERAL[17, 27]
AUX[24, 8] <- RS22_ANTRAB_2018_GERAL[17, 28]

AUX[1, 9] <- RS22_ANTRAB_2019_GERAL[17, 5]
AUX[2, 9] <- RS22_ANTRAB_2019_GERAL[17, 6]
AUX[3, 9] <- RS22_ANTRAB_2019_GERAL[17, 7]
AUX[4, 9] <- RS22_ANTRAB_2019_GERAL[17, 8]
AUX[5, 9] <- RS22_ANTRAB_2019_GERAL[17, 9]
AUX[6, 9] <- RS22_ANTRAB_2019_GERAL[17, 10]
AUX[7, 9] <- RS22_ANTRAB_2019_GERAL[17, 11]
AUX[8, 9] <- RS22_ANTRAB_2019_GERAL[17, 12]
AUX[9, 9] <- RS22_ANTRAB_2019_GERAL[17, 13]
AUX[10, 9] <- RS22_ANTRAB_2019_GERAL[17, 14]
AUX[11, 9] <- RS22_ANTRAB_2019_GERAL[17, 15]
AUX[12, 9] <- RS22_ANTRAB_2019_GERAL[17, 16]
AUX[13, 9] <- RS22_ANTRAB_2019_GERAL[17, 17]
AUX[14, 9] <- RS22_ANTRAB_2019_GERAL[17, 18]
AUX[15, 9] <- RS22_ANTRAB_2019_GERAL[17, 19]
AUX[16, 9] <- RS22_ANTRAB_2019_GERAL[17, 20]
AUX[17, 9] <- RS22_ANTRAB_2019_GERAL[17, 21]
AUX[18, 9] <- RS22_ANTRAB_2019_GERAL[17, 22]
AUX[19, 9] <- RS22_ANTRAB_2019_GERAL[17, 23]
AUX[20, 9] <- RS22_ANTRAB_2019_GERAL[17, 24]
AUX[21, 9] <- RS22_ANTRAB_2019_GERAL[17, 25]
AUX[22, 9] <- RS22_ANTRAB_2019_GERAL[17, 26]
AUX[23, 9] <- RS22_ANTRAB_2019_GERAL[17, 27]
AUX[24, 9] <- RS22_ANTRAB_2019_GERAL[17, 28]

AUX[1, 10] <- RS22_ANTRAB_2020_GERAL[17, 5]
AUX[2, 10] <- RS22_ANTRAB_2020_GERAL[17, 6]
AUX[3, 10] <- RS22_ANTRAB_2020_GERAL[17, 7]
AUX[4, 10] <- RS22_ANTRAB_2020_GERAL[17, 8]
AUX[5, 10] <- RS22_ANTRAB_2020_GERAL[17, 9]
AUX[6, 10] <- RS22_ANTRAB_2020_GERAL[17, 10]
AUX[7, 10] <- RS22_ANTRAB_2020_GERAL[17, 11]
AUX[8, 10] <- RS22_ANTRAB_2020_GERAL[17, 12]
AUX[9, 10] <- RS22_ANTRAB_2020_GERAL[17, 13]
AUX[10, 10] <- RS22_ANTRAB_2020_GERAL[17, 14]
AUX[11, 10] <- RS22_ANTRAB_2020_GERAL[17, 15]
AUX[12, 10] <- RS22_ANTRAB_2020_GERAL[17, 16]
AUX[13, 10] <- RS22_ANTRAB_2020_GERAL[17, 17]
AUX[14, 10] <- RS22_ANTRAB_2020_GERAL[17, 18]
AUX[15, 10] <- RS22_ANTRAB_2020_GERAL[17, 19]
AUX[16, 10] <- RS22_ANTRAB_2020_GERAL[17, 20]
AUX[17, 10] <- RS22_ANTRAB_2020_GERAL[17, 21]
AUX[18, 10] <- RS22_ANTRAB_2020_GERAL[17, 22]
AUX[19, 10] <- RS22_ANTRAB_2020_GERAL[17, 23]
AUX[20, 10] <- RS22_ANTRAB_2020_GERAL[17, 24]
AUX[21, 10] <- RS22_ANTRAB_2020_GERAL[17, 25]
AUX[22, 10] <- RS22_ANTRAB_2020_GERAL[17, 26]
AUX[23, 10] <- RS22_ANTRAB_2020_GERAL[17, 27]
AUX[24, 10] <- RS22_ANTRAB_2020_GERAL[17, 28]

AUX[1, 11] <- RS22_ANTRAB_2021_GERAL[17, 5]
AUX[2, 11] <- RS22_ANTRAB_2021_GERAL[17, 6]
AUX[3, 11] <- RS22_ANTRAB_2021_GERAL[17, 7]
AUX[4, 11] <- RS22_ANTRAB_2021_GERAL[17, 8]
AUX[5, 11] <- RS22_ANTRAB_2021_GERAL[17, 9]
AUX[6, 11] <- RS22_ANTRAB_2021_GERAL[17, 10]
AUX[7, 11] <- RS22_ANTRAB_2021_GERAL[17, 11]
AUX[8, 11] <- RS22_ANTRAB_2021_GERAL[17, 12]
AUX[9, 11] <- RS22_ANTRAB_2021_GERAL[17, 13]
AUX[10, 11] <- RS22_ANTRAB_2021_GERAL[17, 14]
AUX[11, 11] <- RS22_ANTRAB_2021_GERAL[17, 15]
AUX[12, 11] <- RS22_ANTRAB_2021_GERAL[17, 16]
AUX[13, 11] <- RS22_ANTRAB_2021_GERAL[17, 17]
AUX[14, 11] <- RS22_ANTRAB_2021_GERAL[17, 18]
AUX[15, 11] <- RS22_ANTRAB_2021_GERAL[17, 19]
AUX[16, 11] <- RS22_ANTRAB_2021_GERAL[17, 20]
AUX[17, 11] <- RS22_ANTRAB_2021_GERAL[17, 21]
AUX[18, 11] <- RS22_ANTRAB_2021_GERAL[17, 22]
AUX[19, 11] <- RS22_ANTRAB_2021_GERAL[17, 23]
AUX[20, 11] <- RS22_ANTRAB_2021_GERAL[17, 24]
AUX[21, 11] <- RS22_ANTRAB_2021_GERAL[17, 25]
AUX[22, 11] <- RS22_ANTRAB_2021_GERAL[17, 26]
AUX[23, 11] <- RS22_ANTRAB_2021_GERAL[17, 27]
AUX[24, 11] <- RS22_ANTRAB_2021_GERAL[17, 28]

AUX[1, 12] <- RS22_ANTRAB_2022_GERAL[17, 5]
AUX[2, 12] <- RS22_ANTRAB_2022_GERAL[17, 6]
AUX[3, 12] <- RS22_ANTRAB_2022_GERAL[17, 7]
AUX[4, 12] <- RS22_ANTRAB_2022_GERAL[17, 8]
AUX[5, 12] <- RS22_ANTRAB_2022_GERAL[17, 9]
AUX[6, 12] <- RS22_ANTRAB_2022_GERAL[17, 10]
AUX[7, 12] <- RS22_ANTRAB_2022_GERAL[17, 11]
AUX[8, 12] <- RS22_ANTRAB_2022_GERAL[17, 12]
AUX[9, 12] <- RS22_ANTRAB_2022_GERAL[17, 13]
AUX[10, 12] <- RS22_ANTRAB_2022_GERAL[17, 14]
AUX[11, 12] <- RS22_ANTRAB_2022_GERAL[17, 15]
AUX[12, 12] <- RS22_ANTRAB_2022_GERAL[17, 16]
AUX[13, 12] <- RS22_ANTRAB_2022_GERAL[17, 17]
AUX[14, 12] <- RS22_ANTRAB_2022_GERAL[17, 18]
AUX[15, 12] <- RS22_ANTRAB_2022_GERAL[17, 19]
AUX[16, 12] <- RS22_ANTRAB_2022_GERAL[17, 20]
AUX[17, 12] <- RS22_ANTRAB_2022_GERAL[17, 21]
AUX[18, 12] <- RS22_ANTRAB_2022_GERAL[17, 22]
AUX[19, 12] <- RS22_ANTRAB_2022_GERAL[17, 23]
AUX[20, 12] <- RS22_ANTRAB_2022_GERAL[17, 24]
AUX[21, 12] <- RS22_ANTRAB_2022_GERAL[17, 25]
AUX[22, 12] <- RS22_ANTRAB_2022_GERAL[17, 26]
AUX[23, 12] <- RS22_ANTRAB_2022_GERAL[17, 27]
AUX[24, 12] <- RS22_ANTRAB_2022_GERAL[17, 28]

AUX[1, 13] <- RS22_ANTRAB_2023_GERAL[17, 5]
AUX[2, 13] <- RS22_ANTRAB_2023_GERAL[17, 6]
AUX[3, 13] <- RS22_ANTRAB_2023_GERAL[17, 7]
AUX[4, 13] <- RS22_ANTRAB_2023_GERAL[17, 8]
AUX[5, 13] <- RS22_ANTRAB_2023_GERAL[17, 9]
AUX[6, 13] <- RS22_ANTRAB_2023_GERAL[17, 10]
AUX[7, 13] <- RS22_ANTRAB_2023_GERAL[17, 11]
AUX[8, 13] <- RS22_ANTRAB_2023_GERAL[17, 12]
AUX[9, 13] <- RS22_ANTRAB_2023_GERAL[17, 13]
AUX[10, 13] <- RS22_ANTRAB_2023_GERAL[17, 14]
AUX[11, 13] <- RS22_ANTRAB_2023_GERAL[17, 15]
AUX[12, 13] <- RS22_ANTRAB_2023_GERAL[17, 16]
AUX[13, 13] <- RS22_ANTRAB_2023_GERAL[17, 17]
AUX[14, 13] <- RS22_ANTRAB_2023_GERAL[17, 18]
AUX[15, 13] <- RS22_ANTRAB_2023_GERAL[17, 19]
AUX[16, 13] <- RS22_ANTRAB_2023_GERAL[17, 20]
AUX[17, 13] <- RS22_ANTRAB_2023_GERAL[17, 21]
AUX[18, 13] <- RS22_ANTRAB_2023_GERAL[17, 22]
AUX[19, 13] <- RS22_ANTRAB_2023_GERAL[17, 23]
AUX[20, 13] <- RS22_ANTRAB_2023_GERAL[17, 24]
AUX[21, 13] <- RS22_ANTRAB_2023_GERAL[17, 25]
AUX[22, 13] <- RS22_ANTRAB_2023_GERAL[17, 26]
AUX[23, 13] <- RS22_ANTRAB_2023_GERAL[17, 27]
AUX[24, 13] <- RS22_ANTRAB_2023_GERAL[17, 28]

AUX[1, 14] <- RS22_ANTRAB_2024_GERAL[17, 5]
AUX[2, 14] <- RS22_ANTRAB_2024_GERAL[17, 6]
AUX[3, 14] <- RS22_ANTRAB_2024_GERAL[17, 7]
AUX[4, 14] <- RS22_ANTRAB_2024_GERAL[17, 8]
AUX[5, 14] <- RS22_ANTRAB_2024_GERAL[17, 9]
AUX[6, 14] <- RS22_ANTRAB_2024_GERAL[17, 10]
AUX[7, 14] <- RS22_ANTRAB_2024_GERAL[17, 11]
AUX[8, 14] <- RS22_ANTRAB_2024_GERAL[17, 12]
AUX[9, 14] <- RS22_ANTRAB_2024_GERAL[17, 13]
AUX[10, 14] <- RS22_ANTRAB_2024_GERAL[17, 14]
AUX[11, 14] <- RS22_ANTRAB_2024_GERAL[17, 15]
AUX[12, 14] <- RS22_ANTRAB_2024_GERAL[17, 16]
AUX[13, 14] <- RS22_ANTRAB_2024_GERAL[17, 17]
AUX[14, 14] <- RS22_ANTRAB_2024_GERAL[17, 18]
AUX[15, 14] <- RS22_ANTRAB_2024_GERAL[17, 19]
AUX[16, 14] <- RS22_ANTRAB_2024_GERAL[17, 20]
AUX[17, 14] <- RS22_ANTRAB_2024_GERAL[17, 21]
AUX[18, 14] <- RS22_ANTRAB_2024_GERAL[17, 22]
AUX[19, 14] <- RS22_ANTRAB_2024_GERAL[17, 23]
AUX[20, 14] <- RS22_ANTRAB_2024_GERAL[17, 24]
AUX[21, 14] <- RS22_ANTRAB_2024_GERAL[17, 25]
AUX[22, 14] <- RS22_ANTRAB_2024_GERAL[17, 26]
AUX[23, 14] <- RS22_ANTRAB_2024_GERAL[17, 27]
AUX[24, 14] <- RS22_ANTRAB_2024_GERAL[17, 28]

AUX <- as.data.frame(t(AUX))

colnames(AUX) <- AUX[1, ]

AUX <- AUX[-1, ] 

AUX[, 1] <- as.numeric(AUX[, 1])

AUX[, 25] <- c("2012", "2013", "2014", "2015", "2016", "2017", 
               "2018", "2019", "2020", "2021", "2022", "2023", "2024", "2025")

write.csv (assign(paste0("RS", "_ANTRAB_BASE_Serie_Historica"), AUX), 
           paste0("Tabulacoes_R/Raiva/RS", "_ANTRAB_BASE_Serie_Historica.csv"), 
           row.names = FALSE)

#####  Compilado 

AUX <- RS22_ANTRAB_2012_GERAL[17,]
AUX[1, 2] <- "2012"
AUX[2,] <- RS22_ANTRAB_2013_GERAL[17,]
AUX[2, 2] <- "2013"
AUX[3,] <- RS22_ANTRAB_2014_GERAL[17,]
AUX[3, 2] <- "2014"
AUX[4,] <- RS22_ANTRAB_2015_GERAL[17,]
AUX[4, 2] <- "2015"
AUX[5,] <- RS22_ANTRAB_2016_GERAL[17,]
AUX[5, 2] <- "2016"
AUX[6,] <- RS22_ANTRAB_2017_GERAL[17,]
AUX[6, 2] <- "2017"
AUX[7,] <- RS22_ANTRAB_2018_GERAL[17,]
AUX[7, 2] <- "2018"
AUX[8,] <- RS22_ANTRAB_2019_GERAL[17,]
AUX[8, 2] <- "2019"
AUX[9,] <- RS22_ANTRAB_2020_GERAL[17,]
AUX[9, 2] <- "2020"
AUX[10,] <- RS22_ANTRAB_2021_GERAL[17,]
AUX[10, 2] <- "2021"
AUX[11,] <- RS22_ANTRAB_2022_GERAL[17,]
AUX[11, 2] <- "2022"
AUX[12,] <- RS22_ANTRAB_2023_GERAL[17,]
AUX[12, 2] <- "2023"
AUX[13,] <- RS22_ANTRAB_2024_GERAL[17,]
AUX[13, 2] <- "2024"

AUX[nrow(AUX) +1, 4:35 ] <- apply(AUX[,4:35], 2, sum)

AUX01 <- as.data.frame(AUX[nrow(AUX), 5]/AUX[nrow(AUX), 4])


colnames(AUX)[2] <- "Ano"

AUX <- AUX[,-3]

####  Canal Endêmico  ###

RS_CE_Base_ANTRAB <- data.frame("ANO" = NA)

RS_CE_Base_ANTRAB[1, 1] <- "2012"
RS_CE_Base_ANTRAB[1, 2:54] <- as.integer(data.frame(RS22_ANTRAB_2012_SE_Notificados[nrow(RS22_ANTRAB_2012_SE_Notificados), 2:54]))

RS_CE_Base_ANTRAB[(nrow(RS_CE_Base_ANTRAB)+1), 1] <- "2013"
RS_CE_Base_ANTRAB[nrow(RS_CE_Base_ANTRAB), 2:54] <- as.integer(data.frame(RS22_ANTRAB_2013_SE_Notificados[nrow(RS22_ANTRAB_2013_SE_Notificados), 2:54]))

RS_CE_Base_ANTRAB[(nrow(RS_CE_Base_ANTRAB)+1), 1] <- "2014"
RS_CE_Base_ANTRAB[nrow(RS_CE_Base_ANTRAB), 2:54] <- as.integer(data.frame(RS22_ANTRAB_2014_SE_Notificados[nrow(RS22_ANTRAB_2014_SE_Notificados), 2:54]))

RS_CE_Base_ANTRAB[(nrow(RS_CE_Base_ANTRAB)+1), 1] <- "2015"
RS_CE_Base_ANTRAB[nrow(RS_CE_Base_ANTRAB), 2:54] <- as.integer(data.frame(RS22_ANTRAB_2015_SE_Notificados[nrow(RS22_ANTRAB_2015_SE_Notificados), 2:54]))

RS_CE_Base_ANTRAB[(nrow(RS_CE_Base_ANTRAB)+1), 1] <- "2016"
RS_CE_Base_ANTRAB[nrow(RS_CE_Base_ANTRAB), 2:54] <- as.integer(data.frame(RS22_ANTRAB_2016_SE_Notificados[nrow(RS22_ANTRAB_2016_SE_Notificados), 2:54]))

RS_CE_Base_ANTRAB[(nrow(RS_CE_Base_ANTRAB)+1), 1] <- "2017"
RS_CE_Base_ANTRAB[nrow(RS_CE_Base_ANTRAB), 2:54] <- as.integer(data.frame(RS22_ANTRAB_2017_SE_Notificados[nrow(RS22_ANTRAB_2017_SE_Notificados), 2:54]))

RS_CE_Base_ANTRAB[(nrow(RS_CE_Base_ANTRAB)+1), 1] <- "2018"
RS_CE_Base_ANTRAB[nrow(RS_CE_Base_ANTRAB), 2:54] <- as.integer(data.frame(RS22_ANTRAB_2018_SE_Notificados[nrow(RS22_ANTRAB_2018_SE_Notificados), 2:54]))

RS_CE_Base_ANTRAB[(nrow(RS_CE_Base_ANTRAB)+1), 1] <- "2019"
RS_CE_Base_ANTRAB[nrow(RS_CE_Base_ANTRAB), 2:54] <- as.integer(data.frame(RS22_ANTRAB_2019_SE_Notificados[nrow(RS22_ANTRAB_2019_SE_Notificados), 2:54]))

RS_CE_Base_ANTRAB[(nrow(RS_CE_Base_ANTRAB)+1), 1] <- "2020"
RS_CE_Base_ANTRAB[nrow(RS_CE_Base_ANTRAB), 2:54] <- as.integer(data.frame(RS22_ANTRAB_2020_SE_Notificados[nrow(RS22_ANTRAB_2020_SE_Notificados), 2:54]))

RS_CE_Base_ANTRAB[(nrow(RS_CE_Base_ANTRAB)+1), 1] <- "2021"
RS_CE_Base_ANTRAB[nrow(RS_CE_Base_ANTRAB), 2:54] <- as.integer(data.frame(RS22_ANTRAB_2021_SE_Notificados[nrow(RS22_ANTRAB_2021_SE_Notificados), 2:54]))

RS_CE_Base_ANTRAB[(nrow(RS_CE_Base_ANTRAB)+1), 1] <- "2022"
RS_CE_Base_ANTRAB[nrow(RS_CE_Base_ANTRAB), 2:54] <- as.integer(data.frame(RS22_ANTRAB_2022_SE_Notificados[nrow(RS22_ANTRAB_2022_SE_Notificados), 2:54]))

RS_CE_Base_ANTRAB[(nrow(RS_CE_Base_ANTRAB)+1), 1] <- "2023"
RS_CE_Base_ANTRAB[nrow(RS_CE_Base_ANTRAB), 2:54] <- as.integer(data.frame(RS22_ANTRAB_2023_SE_Notificados[nrow(RS22_ANTRAB_2023_SE_Notificados), 2:54]))

RS_CE_Base_ANTRAB[(nrow(RS_CE_Base_ANTRAB)+1), 1] <- "2024"
RS_CE_Base_ANTRAB[nrow(RS_CE_Base_ANTRAB), 2:54] <- as.integer(data.frame(RS22_ANTRAB_2024_SE_Notificados[nrow(RS22_ANTRAB_2024_SE_Notificados), 2:54]))

colnames (RS_CE_Base_ANTRAB)[2:54] <- c(1:53)

write.csv (RS_CE_Base_ANTRAB, 
           "Tabulacoes_R/Raiva/RS_CE_Base_ANTRAB.csv", 
           row.names = FALSE)

rm(AUX, BASE_IBGE, i, ID_REG, nrow, RS)
