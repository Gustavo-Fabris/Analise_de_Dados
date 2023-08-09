

####   Série Histórica, lines 2455 - 2504



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

###########################         Localizações   #########################

###   Linhas          Assunto                         Linhas              Assunto
###   83 - 97         libraries                 ### 2459 - 2599         Canais Endêmicos
###   104 - 154       Bases de dados            ### 
###   156 - 2170      Tabelas período atual
###   2184 - 2444     Decodificação SINAN
###   2455 - 2504     Série histórica

#####      Definindo diretório de trabalho, caso tenha que trabalhar em Windows, acertar o diretório       ####

setwd("/home/gustavo/Área de trabalho/Análise_de_Dados/")

###############################################################################################################
###############################################################################################################
##                                                                                                           ##
##               OS PASSOS ABAIXO (ATÉ LINHA 80) DEVEM SER SEGUIDOS PARA FUNCIONAMENTO!!!                    ##
##    ################################################################################################       ##
##                                                                                                           ##
##            Lembrar que OBRIGATORIAMENTE deve ser baixados as bases DBF de 2009 até 2023                   ##
##            As bases DBF devem ser salvas no formato DENGON2009, DENGON2012... até DENGON2023              ##
##            A base DENGON2023 deve ser baixada diariamente e salva no local correto para que               ## 
##            o sistema esteja sempre atualizado!!!                                                          ##
##            Os dados do LACEN devem ser baixados da GAL                                                    ##
##            Estes arquivos devem ser alocados no diretório abaixo:                                         ##
##            /home/gustavo/Área de trabalho/Análise_de_Dados/Base_de_Dados/DBF ou /Base_de_Dados/LACEN      ##
##    ############################################################################################           ##
##                                                                                                           ##
###############################################################################################################
###############################################################################################################

#############################################################################################################
###################################DEFININDO A SEMANA EPIDEMIOLÓGICA#########################################
#######################################E DATA DA BASE DBF####################################################
#############################################################################################################

#####   Definir fonte para ser utilizada nos gráficos (ggplot ira buscar o objeto     ####
#####   Fonte para "labs(caption = Fonte...")                                         ####
#####   Importante para os gráficos terem a DATA em que a base DBF foi acessada       ####

Fonte <- "Fonte: SINAN. BASE DBF acessada em 25/07/2023"   ##### Fonte dos gráficos relacionados ao SINAN

Fonte_1 <- "Fonte: Lacen. Acesso em 28/07/2023"            ##### Fonte dos gráficos relacionados ao LACEN

Fonte_2 <- "Fonte: Planilhas de Controle Municipais. Acesso em 28/07/2023"     ##### Fonte dos gráficos relacionados às Planilhas Municipais

####     Objeto SE irá ser utilizado como auxiliar definidor de ponto                   ####
####     a partir do qual os histogramas de casos Notificados/Confirmados/Prováveis     ####
####     nas últimas 10 semanas irá buscar os dados.                                    ####

SE <- as.data.frame("30")  ### Colocar a Semana Epidemiológica atual

SE <- as.numeric(SE)

############################################################################################
####   Definindo o objeto RS para servir de apoio para    ##################################
####    buscar dados de todas as RS. Usar 1, 2, 3..., 21, 22    ############################
############################################################################################

RS <- 22   #####  Deve-se colocar AQUI a Regional

#############################################################################################################
#############################################################################################################

###########################################################################################################
###########   Definindo Períodos Epidemícos para serem excluídos do   #####################################
###########   cálculo para os canais endêmicos Regionais              #####################################
###########        COMENTAR AS LINHAS DOS PERÍODOS EPIDÊMICOS         #####################################
###########################################################################################################

Periodos_Epidêmicos_RS <- c("2009/10",
                         "2010/11",
                         "2011/12",
                         "2012/13",
                         "2013/14",
                         "2014/15",
                     #   "2015/16",
                         "2016/17",
                         "2017/18",
                         "2018/19",
                     #   "2019/20",
                         "2020/21",
                     #   "2021/22",
                         "2022/23"
)

####  libraries a serem utilizadas  ###

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

####  Importando as bases de dados para formulação do Informe Epidemiológico      ####
####       As Bases IBGE são planilhas contendo os nomes do municípios e o        ####
####       código do IBGE (entre outros dados).                                   ####
####       São as bases para que os for loops funcionem.                          ####

BASE_IBGE<-read.table(file="Base_de_Dados/Planilha_Base_IBGE.csv", 
                      header=TRUE, 
                      sep=",")

BASE_IBGE_BRASIL <- read.csv (file = "Base_de_Dados/Planilha_Base_IBGE_BRASIL.csv",
                              header = TRUE,
                              sep = ",")

######   Criando objeto ID_REG. Será utilizado para selecionar
######   RS no DBF do SINAN ONLINE.

ID_REG <- as.data.frame(BASE_IBGE[which(BASE_IBGE$RS == RS), 6])

ID_REG <- as.numeric(ID_REG[1,1])

####   Estabelecendo o número de municípios em cada RS

nrow <- NROW(BASE_IBGE[which(BASE_IBGE$RS == RS), 1])

####  Base DBF do SINAN. Deve-se baixá-las, renomeá-las e salvá-las no diretório correto  ######

DENGON2022 <- read.dbf(file = "Base_de_Dados/DBF/DENGON2022.dbf", 
                       as.is = FALSE) %>% select(ID_REGIONA, NU_NOTIFIC, ID_GEO1,ID_GEO2, ID_AGRAVO, ID_REGIONA, DT_NOTIFIC, NU_ANO, SEM_NOT, DT_SIN_PRI, SEM_PRI,  SG_UF_NOT, ID_MUNICIP, NM_PACIENT, DT_NASC, NU_IDADE_N, CS_SEXO, CS_GESTANT, CS_ESCOL_N, NM_MAE_PAC, ID_MN_RESI, SG_UF, ID_RG_RESI, NM_LOGRADO, NU_NUMERO, NM_BAIRRO, NU_CEP, CS_ZONA, DT_DIGITA, DT_INVEST, FEBRE, MIALGIA, CEFALEIA, EXANTEMA, VOMITO, NAUSEA, DOR_COSTAS, CONJUNTVIT, ARTRITE, ARTRALGIA, PETEQUIA_N, LEUCOPENIA, LACO, DOR_RETRO, DIABETES, HEMATOLOG, HEPATOPAT, RENAL, HIPERTENSA, ACIDO_PEPT, AUTO_IMUNE, DT_SORO, RESUL_SORO, DT_PCR, RESUL_PCR_, SOROTIPO, CLASSI_FIN, CRITERIO, TPAUTOCTO, COUFINF, COMUNINF, CO_BAINF, EVOLUCAO, HOSPITALIZ, DT_INTERNA, DT_OBITO, DT_ENCERRA, DT_ALRM, ALRM_LETAR, ALRM_HEPAT, ALRM_LIQ, ALRM_HIPOT, ALRM_PLAQ, ALRM_VOM, ALRM_SANG, ALRM_HEMAT, ALRM_ABDOM, DT_GRAV, GRAV_PULSO, GRAV_CONV, GRAV_ENCH, GRAV_INSUF, GRAV_TAQUI, GRAV_EXTRE, GRAV_HIPOT, GRAV_HEMAT, GRAV_MELEN, GRAV_METRO, GRAV_SANG, GRAV_AST, GRAV_MIOC, GRAV_CONSC, GRAV_ORGAO, MANI_HEMOR, EPISTAXE, GENGIVO, METRO, DS_OBS)

DENGON2023 <- read.dbf(file = "Base_de_Dados/DBF/DENGON2023.dbf", 
                       as.is = FALSE) %>% select(ID_REGIONA, NU_NOTIFIC, ID_GEO1,ID_GEO2, ID_AGRAVO, ID_REGIONA, DT_NOTIFIC, NU_ANO, SEM_NOT, DT_SIN_PRI, SEM_PRI,  SG_UF_NOT, ID_MUNICIP, NM_PACIENT, DT_NASC, NU_IDADE_N, CS_SEXO, CS_GESTANT, CS_ESCOL_N, NM_MAE_PAC, ID_MN_RESI, SG_UF, ID_RG_RESI, NM_LOGRADO, NU_NUMERO, NM_BAIRRO, NU_CEP, CS_ZONA, DT_DIGITA, DT_INVEST, FEBRE, MIALGIA, CEFALEIA, EXANTEMA, VOMITO, NAUSEA, DOR_COSTAS, CONJUNTVIT, ARTRITE, ARTRALGIA, PETEQUIA_N, LEUCOPENIA, LACO, DOR_RETRO, DIABETES, HEMATOLOG, HEPATOPAT, RENAL, HIPERTENSA, ACIDO_PEPT, AUTO_IMUNE, DT_SORO, RESUL_SORO, DT_PCR, RESUL_PCR_, SOROTIPO, CLASSI_FIN, CRITERIO, TPAUTOCTO, COUFINF, COMUNINF, CO_BAINF, EVOLUCAO, HOSPITALIZ, DT_INTERNA, DT_OBITO, DT_ENCERRA, DT_ALRM, ALRM_LETAR, ALRM_HEPAT, ALRM_LIQ, ALRM_HIPOT, ALRM_PLAQ, ALRM_VOM, ALRM_SANG, ALRM_HEMAT, ALRM_ABDOM, DT_GRAV, GRAV_PULSO, GRAV_CONV, GRAV_ENCH, GRAV_INSUF, GRAV_TAQUI, GRAV_EXTRE, GRAV_HIPOT, GRAV_HEMAT, GRAV_MELEN, GRAV_METRO, GRAV_SANG, GRAV_AST, GRAV_MIOC, GRAV_CONSC, GRAV_ORGAO, MANI_HEMOR, EPISTAXE, GENGIVO, METRO, DS_OBS)


CHIKON2022 <- read.dbf("Base_de_Dados/DBF/CHIKON2022.dbf",
                       as.is = FALSE) %>% select(ID_REGIONA, NU_NOTIFIC, ID_GEO1,ID_GEO2, ID_AGRAVO, ID_REGIONA, DT_NOTIFIC, NU_ANO, SEM_NOT, DT_SIN_PRI, SEM_PRI,  SG_UF_NOT, ID_MUNICIP, NM_PACIENT, DT_NASC, NU_IDADE_N, CS_SEXO, CS_GESTANT, CS_ESCOL_N, NM_MAE_PAC, ID_MN_RESI, SG_UF, ID_RG_RESI, NM_LOGRADO, NU_NUMERO, NM_BAIRRO, NU_CEP, CS_ZONA, DT_DIGITA, DT_INVEST, FEBRE, MIALGIA, CEFALEIA, EXANTEMA, VOMITO, NAUSEA, DOR_COSTAS, CONJUNTVIT, ARTRITE, ARTRALGIA, PETEQUIA_N, LEUCOPENIA, LACO, DOR_RETRO, DIABETES, HEMATOLOG, HEPATOPAT, RENAL, HIPERTENSA, ACIDO_PEPT, AUTO_IMUNE, DT_SORO, RESUL_SORO, DT_PCR, RESUL_PCR_, SOROTIPO, CLASSI_FIN, CRITERIO, TPAUTOCTO, COUFINF, COMUNINF, CO_BAINF, EVOLUCAO, HOSPITALIZ, DT_INTERNA, DT_OBITO, DT_ENCERRA, DT_ALRM, ALRM_LETAR, ALRM_HEPAT, ALRM_LIQ, ALRM_HIPOT, ALRM_PLAQ, ALRM_VOM, ALRM_SANG, ALRM_HEMAT, ALRM_ABDOM, DT_GRAV, GRAV_PULSO, GRAV_CONV, GRAV_ENCH, GRAV_INSUF, GRAV_TAQUI, GRAV_EXTRE, GRAV_HIPOT, GRAV_HEMAT, GRAV_MELEN, GRAV_METRO, GRAV_SANG, GRAV_AST, GRAV_MIOC, GRAV_CONSC, GRAV_ORGAO, MANI_HEMOR, EPISTAXE, GENGIVO, METRO, DS_OBS)

CHIKON2023 <- read.dbf("Base_de_Dados/DBF/CHIKON2023.dbf",
                       as.is = FALSE) %>% select(ID_REGIONA, NU_NOTIFIC, ID_GEO1,ID_GEO2, ID_AGRAVO, ID_REGIONA, DT_NOTIFIC, NU_ANO, SEM_NOT, DT_SIN_PRI, SEM_PRI,  SG_UF_NOT, ID_MUNICIP, NM_PACIENT, DT_NASC, NU_IDADE_N, CS_SEXO, CS_GESTANT, CS_ESCOL_N, NM_MAE_PAC, ID_MN_RESI, SG_UF, ID_RG_RESI, NM_LOGRADO, NU_NUMERO, NM_BAIRRO, NU_CEP, CS_ZONA, DT_DIGITA, DT_INVEST, FEBRE, MIALGIA, CEFALEIA, EXANTEMA, VOMITO, NAUSEA, DOR_COSTAS, CONJUNTVIT, ARTRITE, ARTRALGIA, PETEQUIA_N, LEUCOPENIA, LACO, DOR_RETRO, DIABETES, HEMATOLOG, HEPATOPAT, RENAL, HIPERTENSA, ACIDO_PEPT, AUTO_IMUNE, DT_SORO, RESUL_SORO, DT_PCR, RESUL_PCR_, SOROTIPO, CLASSI_FIN, CRITERIO, TPAUTOCTO, COUFINF, COMUNINF, CO_BAINF, EVOLUCAO, HOSPITALIZ, DT_INTERNA, DT_OBITO, DT_ENCERRA, DT_ALRM, ALRM_LETAR, ALRM_HEPAT, ALRM_LIQ, ALRM_HIPOT, ALRM_PLAQ, ALRM_VOM, ALRM_SANG, ALRM_HEMAT, ALRM_ABDOM, DT_GRAV, GRAV_PULSO, GRAV_CONV, GRAV_ENCH, GRAV_INSUF, GRAV_TAQUI, GRAV_EXTRE, GRAV_HIPOT, GRAV_HEMAT, GRAV_MELEN, GRAV_METRO, GRAV_SANG, GRAV_AST, GRAV_MIOC, GRAV_CONSC, GRAV_ORGAO, MANI_HEMOR, EPISTAXE, GENGIVO, METRO, DS_OBS)

RS22_Serie_Historica_Base <- read.csv(file = "Base_de_Dados/Tabulacoes_R/Arboviroses/RS22_Serie_Historica_Base.csv",
                                      header = TRUE,
                                      sep = ",")

RS22_CE_Notificados_Base <- read.csv(file = "Base_de_Dados/Tabulacoes_R/Arboviroses/RS22_CE_Notificados_Base.csv",
                                     header = TRUE,
                                     sep = ",")

RS22_CE_Confirmados_Base <- read.csv(file = "Base_de_Dados/Tabulacoes_R/Arboviroses/RS22_CE_Confirmados_Base.csv",
                                     header = TRUE,
                                     sep = ",")

RS22_CE_Notificados_IVAIPORÃ_Base <- read.csv(file = "Base_de_Dados/Tabulacoes_R/Arboviroses/RS22_CE_Notificados_IVAIPORÃ_Base.csv",
                                              header = TRUE,
                                              sep = ",")

RS22_CE_Confirmados_IVAIPORÃ_Base <- read.csv(file = "Base_de_Dados/Tabulacoes_R/Arboviroses/RS22_CE_Confirmados_IVAIPORÃ_Base.csv",
                                              header = TRUE,
                                              sep = ",")

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
DENGON2022$SEM_PRI <-as.numeric(as.character(DENGON2022$SEM_PRI))   ###############################
###
DENGON2023$SEM_PRI <-as.numeric(as.character(DENGON2023$SEM_PRI))   ###############################
###
CHIKON2022$SEM_PRI <-as.numeric(as.character(CHIKON2022$SEM_PRI))   ###############################
###
CHIKON2023$SEM_PRI <-as.numeric(as.character(CHIKON2023$SEM_PRI))   ###############################
###################################################################################################
###################################################################################################

#####################################################################################################################
########################################         2022/23        ###################################################
#####################################################################################################################

#####################################################################################################################
######### A partir deste ponto do script, o código terá como função, além de criar a ################################
######### a base do período sazonal atual, servirá para, via ctrl -c / ctrl -v, ser  ################################
######### incluído no script contido na Tabulação GERAL para servir de base histórica################################
######### do Programa Regional de Controle das Arboviroses da 22ª RS. Falta trabalhar################################
######### com a BASE DBF Chikungunya e Zika. ########################################################################
#####################################################################################################################

################ O código a ser copiado para a Tabulação Geral irá até o ponto determinado com FIM ##################
#####################################################################################################################

####    Tabela de notificações SINAN    ###

###################   Usando rbind para juntar o segundo semestre de um ano  ########################################
###################   ao primeiro semestro do ano seguinte.                  ########################################
###################   Criando assim o período sazonal                        ########################################

AUX01 <- DENGON2022 %>% 
  filter(ID_REGIONA == ID_REG | ID_RG_RESI == ID_REG, 
         SEM_PRI >= 202231)

AUX02 <- DENGON2023 %>% 
  filter(ID_REGIONA == ID_REG | ID_RG_RESI == ID_REG, 
         SEM_PRI <=202330)

SINAN_DENGUE_RS <- rbind(AUX01, 
                         AUX02)

assign(paste0("RS", RS, "_22_23_SINAN"), 
       SINAN_DENGUE_RS) 

###############   Fazendo o mesmo com as bases DBF de chikungunya   ################################################

AUX01 <- CHIKON2022 %>% 
  filter(SEM_PRI >= 202231,
         ID_REGIONA == ID_REG | ID_RG_RESI == ID_REG)

AUX02 <- CHIKON2023 %>% 
  filter(SEM_PRI <= 202330,
         ID_REGIONA == ID_REG | ID_RG_RESI == ID_REG)

SINAN_CHIK_RS <- rbind(AUX01, 
                       AUX02)

###      Removendo tabela DENGON2022 e DENGON2023 já utilizada     ###

#rm (DENGON2022, 
#    DENGON2023, 
#    CHIKON2022, 
#    CHIKON2023)

#################################################################################################################
###     Construindo um for loop para realizar a tabela de notificados por semana epidemiológica               ###
#################################################################################################################

AUX <- matrix(data = NA, 
              nrow = nrow, 
              ncol = 54)

AUX <- as.data.frame(AUX)

colnames(AUX)[1] <- "Município" 

AUX[,1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

colnames (AUX)[2:24] <- c(31:53)

colnames (AUX)[25:54] <- c(1:30)

for (i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX == i), 2] <- as.integer(SINAN_DENGUE_RS %>%
                                          filter(ID_MN_RESI == i,
                                                 SEM_PRI ==202231)%>%
                                          count()
                                        
  )
  
  AUX[which(AUX == i), 3] <- as.integer(SINAN_DENGUE_RS %>% 
                                          filter(ID_MN_RESI == i, 
                                                 SEM_PRI ==202232) %>% 
                                          count()
  )
  
  AUX[which(AUX == i), 4] <- as.integer(SINAN_DENGUE_RS %>% 
                                          filter(ID_MN_RESI == i,
                                                 SEM_PRI ==202233) %>% 
                                          count()
  )
  
  AUX[which(AUX == i),5] <- as.integer(SINAN_DENGUE_RS %>% 
                                         filter(ID_MN_RESI == i,
                                                SEM_PRI ==202234) %>% 
                                         count()
  )
  
  AUX[which(AUX == i), 6] <- as.integer(SINAN_DENGUE_RS %>% 
                                          filter(ID_MN_RESI == i,
                                                 SEM_PRI ==202235) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 7] <- as.integer(SINAN_DENGUE_RS %>%
                                          filter(ID_MN_RESI == i, 
                                                 SEM_PRI ==202236) %>%
                                          count()
  )
  
  AUX[which(AUX == i), 8] <- as.integer(SINAN_DENGUE_RS %>%
                                          filter(ID_MN_RESI == i, 
                                                 SEM_PRI ==202237) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 9] <- as.integer(SINAN_DENGUE_RS %>%
                                          filter(ID_MN_RESI == i, 
                                                 SEM_PRI ==202238) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 10] <- as.integer(SINAN_DENGUE_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202239) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 11] <- as.integer(SINAN_DENGUE_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202240) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 12] <- as.integer(SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202241) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 13] <- as.integer(SINAN_DENGUE_RS %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202242) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 14] <- as.integer(SINAN_DENGUE_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202243) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 15] <- as.integer(SINAN_DENGUE_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202244) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 16] <- as.integer(SINAN_DENGUE_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202245) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 17] <- as.integer(SINAN_DENGUE_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202246) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 18] <- as.integer(SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202247) %>%  
                                           count() 
  )
  
  AUX[which(AUX == i), 19] <- as.integer(SINAN_DENGUE_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202248) %>%      
                                           count() 
  )
  
  AUX[which(AUX == i), 20] <- as.integer(SINAN_DENGUE_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202249) %>%
                                           count() 
  )
  
  AUX[which(AUX == i),  21] <- as.integer(SINAN_DENGUE_RS %>% 
                                            filter(ID_MN_RESI == i, 
                                                   SEM_PRI ==202250) %>%
                                            count() 
  )
  
  AUX[which(AUX == i), 22] <- as.integer(SINAN_DENGUE_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202251) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 23] <- as.integer(SINAN_DENGUE_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202252) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 24] <- as.integer(SINAN_DENGUE_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202253) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 25] <- as.integer(SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202301) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 26] <- as.integer(SINAN_DENGUE_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202302) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 27] <- as.integer(SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202303) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 28] <- as.integer(SINAN_DENGUE_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202304) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 29] <- as.integer(SINAN_DENGUE_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202305) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 30] <- as.integer(SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202306) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 31] <- as.integer(SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202307) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 32] <- as.integer(SINAN_DENGUE_RS %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202308) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 33] <- as.integer(SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202309) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 34] <- as.integer(SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202310) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 35] <- as.integer(SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202311) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 36] <- as.integer(SINAN_DENGUE_RS %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202312) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 37] <- as.integer(SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202313) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 38] <- as.integer(SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202314) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 39] <- as.integer(SINAN_DENGUE_RS %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202315) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 40] <- as.integer(SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202316) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 41] <- as.integer(SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202317) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 42] <- as.integer(SINAN_DENGUE_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202318) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 43] <- as.integer(SINAN_DENGUE_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202319) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 44] <- as.integer(SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202320) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 45] <- as.integer(SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202321) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 46] <- as.integer(SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202322) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 47] <- as.integer(SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202323) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 48] <- as.integer(SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202324) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 49] <- as.integer(SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202325) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 50] <- as.integer(SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202326) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 51] <- as.integer(SINAN_DENGUE_RS %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202327) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 52] <- as.integer(SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202328) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 53] <- as.integer(SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202329) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 54] <- as.integer(SINAN_DENGUE_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202330) %>%
                                           count() 
  )
}

AUX[,1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX[(nrow(AUX)+ 1),2:54] <- apply(AUX[,2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_22_23_SE_Notificados"), AUX)

########################################################################################################
###     Construindo um for loop para realizar a tabela de Confirmados por semana epidemiológica      ###
########################################################################################################

AUX <- matrix(data = NA, 
              nrow = nrow, 
              ncol = 54)

AUX <- as.data.frame(AUX)

colnames(AUX)[1] <- "Município" 

AUX[,1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

colnames (AUX)[2:24] <- c(31:53)

colnames (AUX)[25:54] <- c(1:30)

for (i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX == i), 2] <- as.integer(SINAN_DENGUE_RS %>%
                                          filter(ID_MN_RESI == i,
                                                 CLASSI_FIN == 10 
                                                 | 
                                                   CLASSI_FIN == 11 
                                                 |
                                                   CLASSI_FIN == 12,
                                                 SEM_PRI ==202231)%>%
                                          count()
  )
  
  AUX[which(AUX == i), 3] <- as.integer(SINAN_DENGUE_RS %>% 
                                          filter(ID_MN_RESI == i, 
                                                 CLASSI_FIN == 10 
                                                 | 
                                                   CLASSI_FIN == 11 
                                                 |
                                                   CLASSI_FIN == 12,
                                                 SEM_PRI ==202232) %>% 
                                          count()
  )
  
  AUX[which(AUX == i), 4] <- as.integer(SINAN_DENGUE_RS %>% 
                                          filter(ID_MN_RESI == i,
                                                 CLASSI_FIN == 10 
                                                 | 
                                                   CLASSI_FIN == 11 
                                                 |
                                                   CLASSI_FIN == 12,
                                                 SEM_PRI ==202233) %>% 
                                          count()
  )
  
  AUX[which(AUX == i),5] <- as.integer(SINAN_DENGUE_RS %>% 
                                         filter(ID_MN_RESI == i,
                                                CLASSI_FIN == 10 
                                                | 
                                                  CLASSI_FIN == 11 
                                                |
                                                  CLASSI_FIN == 12,
                                                SEM_PRI ==202234) %>% 
                                         count()
  )
  
  AUX[which(AUX == i), 6] <- as.integer(SINAN_DENGUE_RS %>% 
                                          filter(ID_MN_RESI == i,
                                                 CLASSI_FIN == 10 
                                                 | 
                                                   CLASSI_FIN == 11 
                                                 |
                                                   CLASSI_FIN == 12,
                                                 SEM_PRI ==202235) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 7] <- as.integer(SINAN_DENGUE_RS %>%
                                          filter(ID_MN_RESI == i, 
                                                 CLASSI_FIN == 10 
                                                 | 
                                                   CLASSI_FIN == 11 
                                                 |
                                                   CLASSI_FIN == 12,
                                                 SEM_PRI ==202236) %>%
                                          count()
  )
  
  AUX[which(AUX == i), 8] <- as.integer(SINAN_DENGUE_RS %>%
                                          filter(ID_MN_RESI == i, 
                                                 CLASSI_FIN == 10 
                                                 | 
                                                   CLASSI_FIN == 11 
                                                 |
                                                   CLASSI_FIN == 12,
                                                 SEM_PRI ==202237) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 9] <- as.integer(SINAN_DENGUE_RS %>%
                                          filter(ID_MN_RESI == i, 
                                                 CLASSI_FIN == 10 
                                                 | 
                                                   CLASSI_FIN == 11 
                                                 |
                                                   CLASSI_FIN == 12,
                                                 SEM_PRI ==202238) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 10] <- as.integer(SINAN_DENGUE_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  CLASSI_FIN == 10 
                                                  | 
                                                    CLASSI_FIN == 11 
                                                  |
                                                    CLASSI_FIN == 12,
                                                  SEM_PRI ==202239) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 11] <- as.integer(SINAN_DENGUE_RS %>% 
                                           filter(ID_MN_RESI == i,
                                                  CLASSI_FIN == 10 
                                                  | 
                                                    CLASSI_FIN == 11 
                                                  |
                                                    CLASSI_FIN == 12,
                                                  SEM_PRI ==202240) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 12] <- as.integer(SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  CLASSI_FIN == 10 
                                                  | 
                                                    CLASSI_FIN == 11 
                                                  |
                                                    CLASSI_FIN == 12,
                                                  SEM_PRI ==202241) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 13] <- as.integer(SINAN_DENGUE_RS %>% 
                                           filter(ID_MN_RESI == i,
                                                  CLASSI_FIN == 10 
                                                  | 
                                                    CLASSI_FIN == 11 
                                                  |
                                                    CLASSI_FIN == 12,
                                                  SEM_PRI ==202242) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 14] <- as.integer(SINAN_DENGUE_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  CLASSI_FIN == 10 
                                                  | 
                                                    CLASSI_FIN == 11 
                                                  |
                                                    CLASSI_FIN == 12,
                                                  SEM_PRI ==202243) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 15] <- as.integer(SINAN_DENGUE_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  CLASSI_FIN == 10 
                                                  | 
                                                    CLASSI_FIN == 11 
                                                  |
                                                    CLASSI_FIN == 12,
                                                  SEM_PRI ==202244) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 16] <- as.integer(SINAN_DENGUE_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  CLASSI_FIN == 10 
                                                  | 
                                                    CLASSI_FIN == 11 
                                                  |
                                                    CLASSI_FIN == 12,
                                                  SEM_PRI ==202245) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 17] <- as.integer(SINAN_DENGUE_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  CLASSI_FIN == 10 
                                                  | 
                                                    CLASSI_FIN == 11 
                                                  |
                                                    CLASSI_FIN == 12,
                                                  SEM_PRI ==202246) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 18] <- as.integer(SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i, 
                                                  CLASSI_FIN == 10 
                                                  | 
                                                    CLASSI_FIN == 11 
                                                  |
                                                    CLASSI_FIN == 12,
                                                  SEM_PRI ==202247) %>%    
                                           count() 
  )
  
  AUX[which(AUX == i), 19] <- as.integer(SINAN_DENGUE_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  CLASSI_FIN == 10 
                                                  | 
                                                    CLASSI_FIN == 11 
                                                  |
                                                    CLASSI_FIN == 12,
                                                  SEM_PRI ==202248) %>%  
                                           count() 
  )
  
  AUX[which(AUX == i), 20] <- as.integer(SINAN_DENGUE_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  CLASSI_FIN == 10 
                                                  | 
                                                    CLASSI_FIN == 11 
                                                  |
                                                    CLASSI_FIN == 12,
                                                  SEM_PRI ==202249) %>%
                                           count() 
  )
  
  AUX[which(AUX == i),  21] <- as.integer(SINAN_DENGUE_RS %>% 
                                            filter(ID_MN_RESI == i,
                                                   CLASSI_FIN == 10 
                                                   | 
                                                     CLASSI_FIN == 11 
                                                   |
                                                     CLASSI_FIN == 12,
                                                   SEM_PRI ==202250) %>%
                                            count() 
  )
  
  AUX[which(AUX == i), 22] <- as.integer(SINAN_DENGUE_RS %>% 
                                           filter(ID_MN_RESI == i,
                                                  CLASSI_FIN == 10 
                                                  | 
                                                    CLASSI_FIN == 11 
                                                  |
                                                    CLASSI_FIN == 12,
                                                  SEM_PRI ==202251) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 23] <- as.integer(SINAN_DENGUE_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  CLASSI_FIN == 10 
                                                  | 
                                                    CLASSI_FIN == 11 
                                                  |
                                                    CLASSI_FIN == 12,
                                                  SEM_PRI ==202252) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 24] <- as.integer(SINAN_DENGUE_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  CLASSI_FIN == 10 
                                                  | 
                                                    CLASSI_FIN == 11 
                                                  |
                                                    CLASSI_FIN == 12,
                                                  SEM_PRI ==202253) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 25] <- as.integer(SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i, 
                                                  CLASSI_FIN == 10 
                                                  | 
                                                    CLASSI_FIN == 11 
                                                  |
                                                    CLASSI_FIN == 12,
                                                  SEM_PRI ==202301) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 26] <- as.integer(SINAN_DENGUE_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  CLASSI_FIN == 10 
                                                  | 
                                                    CLASSI_FIN == 11 
                                                  |
                                                    CLASSI_FIN == 12,
                                                  SEM_PRI ==202302) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 27] <- as.integer(SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i, 
                                                  CLASSI_FIN == 10 
                                                  | 
                                                    CLASSI_FIN == 11 
                                                  |
                                                    CLASSI_FIN == 12,
                                                  SEM_PRI ==202303) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 28] <- as.integer(SINAN_DENGUE_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  CLASSI_FIN == 10 
                                                  | 
                                                    CLASSI_FIN == 11 
                                                  |
                                                    CLASSI_FIN == 12,
                                                  SEM_PRI ==202304) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 29] <- as.integer(SINAN_DENGUE_RS %>% 
                                           filter(ID_MN_RESI == i,
                                                  CLASSI_FIN == 10 
                                                  | 
                                                    CLASSI_FIN == 11 
                                                  |
                                                    CLASSI_FIN == 12,
                                                  SEM_PRI ==202305) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 30] <- as.integer(SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  CLASSI_FIN == 10 
                                                  | 
                                                    CLASSI_FIN == 11 
                                                  |
                                                    CLASSI_FIN == 12,
                                                  SEM_PRI ==202306) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 31] <- as.integer(SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  CLASSI_FIN == 10 
                                                  | 
                                                    CLASSI_FIN == 11 
                                                  |
                                                    CLASSI_FIN == 12,
                                                  SEM_PRI ==202307) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 32] <- as.integer(SINAN_DENGUE_RS %>% 
                                           filter(ID_MN_RESI == i,
                                                  CLASSI_FIN == 10 
                                                  | 
                                                    CLASSI_FIN == 11 
                                                  |
                                                    CLASSI_FIN == 12,
                                                  SEM_PRI ==202308) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 33] <- as.integer(SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  CLASSI_FIN == 10 
                                                  | 
                                                    CLASSI_FIN == 11 
                                                  |
                                                    CLASSI_FIN == 12,
                                                  SEM_PRI ==202309) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 34] <- as.integer(SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  CLASSI_FIN == 10 
                                                  | 
                                                    CLASSI_FIN == 11 
                                                  |
                                                    CLASSI_FIN == 12,
                                                  SEM_PRI ==202310) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 35] <- as.integer(SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  CLASSI_FIN == 10 
                                                  | 
                                                    CLASSI_FIN == 11 
                                                  |
                                                    CLASSI_FIN == 12,
                                                  SEM_PRI ==202311) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 36] <- as.integer(SINAN_DENGUE_RS %>% 
                                           filter(ID_MN_RESI == i,
                                                  CLASSI_FIN == 10 
                                                  | 
                                                    CLASSI_FIN == 11 
                                                  |
                                                    CLASSI_FIN == 12,
                                                  SEM_PRI ==202312) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 37] <- as.integer(SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  CLASSI_FIN == 10 
                                                  | 
                                                    CLASSI_FIN == 11 
                                                  |
                                                    CLASSI_FIN == 12,
                                                  SEM_PRI ==202313) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 38] <- as.integer(SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  CLASSI_FIN == 10 
                                                  | 
                                                    CLASSI_FIN == 11 
                                                  |
                                                    CLASSI_FIN == 12,
                                                  SEM_PRI ==202314) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 39] <- as.integer(SINAN_DENGUE_RS %>% 
                                           filter(ID_MN_RESI == i,
                                                  CLASSI_FIN == 10 
                                                  | 
                                                    CLASSI_FIN == 11 
                                                  |
                                                    CLASSI_FIN == 12,
                                                  SEM_PRI ==202315) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 40] <- as.integer(SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  CLASSI_FIN == 10 
                                                  | 
                                                    CLASSI_FIN == 11 
                                                  |
                                                    CLASSI_FIN == 12,
                                                  SEM_PRI ==202316) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 41] <- as.integer(SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  CLASSI_FIN == 10 
                                                  | 
                                                    CLASSI_FIN == 11 
                                                  |
                                                    CLASSI_FIN == 12,
                                                  SEM_PRI ==202317) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 42] <- as.integer(SINAN_DENGUE_RS %>% 
                                           filter(ID_MN_RESI == i,
                                                  CLASSI_FIN == 10 
                                                  | 
                                                    CLASSI_FIN == 11 
                                                  |
                                                    CLASSI_FIN == 12,
                                                  SEM_PRI ==202318) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 43] <- as.integer(SINAN_DENGUE_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  CLASSI_FIN == 10 
                                                  | 
                                                    CLASSI_FIN == 11 
                                                  |
                                                    CLASSI_FIN == 12,
                                                  SEM_PRI ==202319) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 44] <- as.integer(SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  CLASSI_FIN == 10 
                                                  | 
                                                    CLASSI_FIN == 11 
                                                  |
                                                    CLASSI_FIN == 12,
                                                  SEM_PRI ==202320) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 45] <- as.integer(SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  CLASSI_FIN == 10 
                                                  | 
                                                    CLASSI_FIN == 11 
                                                  |
                                                    CLASSI_FIN == 12,
                                                  SEM_PRI ==202321) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 46] <- as.integer(SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  CLASSI_FIN == 10 
                                                  | 
                                                    CLASSI_FIN == 11 
                                                  |
                                                    CLASSI_FIN == 12,
                                                  SEM_PRI ==202322) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 47] <- as.integer(SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  CLASSI_FIN == 10 
                                                  | 
                                                    CLASSI_FIN == 11 
                                                  |
                                                    CLASSI_FIN == 12,
                                                  SEM_PRI ==202323) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 48] <- as.integer(SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  CLASSI_FIN == 10 
                                                  | 
                                                    CLASSI_FIN == 11 
                                                  |
                                                    CLASSI_FIN == 12,
                                                  SEM_PRI ==202324) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 49] <- as.integer(SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  CLASSI_FIN == 10 
                                                  | 
                                                    CLASSI_FIN == 11 
                                                  |
                                                    CLASSI_FIN == 12,
                                                  SEM_PRI ==202325) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 50] <- as.integer(SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i, 
                                                  CLASSI_FIN == 10 
                                                  | 
                                                    CLASSI_FIN == 11 
                                                  |
                                                    CLASSI_FIN == 12,
                                                  SEM_PRI ==202326) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 51] <- as.integer(SINAN_DENGUE_RS %>% 
                                           filter(ID_MN_RESI == i,
                                                  CLASSI_FIN == 10 
                                                  | 
                                                    CLASSI_FIN == 11 
                                                  |
                                                    CLASSI_FIN == 12,
                                                  SEM_PRI ==202327) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 52] <- as.integer(SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  CLASSI_FIN == 10 
                                                  | 
                                                    CLASSI_FIN == 11 
                                                  |
                                                    CLASSI_FIN == 12,
                                                  SEM_PRI ==202328) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 53] <- as.integer(SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  CLASSI_FIN == 10 
                                                  | 
                                                    CLASSI_FIN == 11 
                                                  |
                                                    CLASSI_FIN == 12,
                                                  SEM_PRI ==202329) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 54] <- as.integer(SINAN_DENGUE_RS %>% 
                                           filter(ID_MN_RESI == i,
                                                  CLASSI_FIN == 10 
                                                  | 
                                                    CLASSI_FIN == 11 
                                                  |
                                                    CLASSI_FIN == 12,
                                                  SEM_PRI ==202330) %>%
                                           count() 
  )
}

AUX[,1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX[(nrow(AUX)+ 1),2:54] <- apply(AUX[,2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_22_23_SE_Confirmados"), AUX)

###################################################################################################
####       Elaborando for loop para criar tabela de dados gerais de notificação da 22ª RS      ####
###################################################################################################

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Notificados <- NA

AUX$Dengue <- NA

AUX$D_S_A <- NA

AUX$Dengue_Grave <- NA

AUX$Descartados <- NA

AUX$Autoctones <- NA

AUX$Incidencia <- NA

AUX$Criterio_Encerramento_Lab <- NA

AUX$Criterio_Encerramento_Clin_Epid <- NA

AUX$DENV_I <- NA

AUX$DENV_II <- NA

AUX$DENV_III <- NA

AUX$DENV_IV <- NA

AUX$Hospitalizacao <- NA

AUX$Obitos <- NA

AUX$Inconclusivos <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  ###Notiicações###  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_DENGUE_RS %>% 
                                                   filter(ID_MN_RESI == i) %>%   
                                                   count()
  )    
  
  ###Dengue###
  
  AUX[which(AUX$COD_IBGE == i), 6] <-as.integer(SINAN_DENGUE_RS %>% 
                                                  filter(CLASSI_FIN == 10, 
                                                         ID_MN_RESI == i) %>%
                                                  count() 
  )
  ###D.S.A.###
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_DENGUE_RS %>%  
                                                   filter(CLASSI_FIN == 11, 
                                                          ID_MN_RESI == i) %>% 
                                                   count()
  )
  
  ###Dengue Grave###
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_DENGUE_RS %>%  
                                                   filter(CLASSI_FIN == 12, 
                                                          ID_MN_RESI == i) %>% 
                                                   count()
  )
  
  ###Descartados###
  
  
  
  AUX[which(AUX$COD_IBGE == i), 9]<- as.integer(SINAN_DENGUE_RS %>% 
                                                  filter(CLASSI_FIN == 5,
                                                         ID_MN_RESI == i) %>% 
                                                  count()
  )  
  
  ###Autóctones###
  
  
  AUX[which(AUX$COD_IBGE == i), 10]<- as.integer(SINAN_DENGUE_RS %>% 
                                                   filter(ID_MN_RESI == i, 
                                                          TPAUTOCTO == 1,
                                                          CLASSI_FIN == 10) %>% 
                                                   count() 
  )
  
  ###Encerrados Laboratório###
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(SINAN_DENGUE_RS %>% 
                                                    filter(ID_MN_RESI == i, 
                                                           CRITERIO == 1) %>% 
                                                    count() 
  )
  
  ###Encerrados Clínico-Epidemiológico###
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_DENGUE_RS %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CRITERIO == 2) %>% 
                                                    count() 
  )
  
  ###DENV I###
  
  AUX[which(AUX$COD_IBGE == i), 14]<- as.integer(SINAN_DENGUE_RS %>% 
                                                   filter(ID_MN_RESI == i, 
                                                          SOROTIPO == 1) %>% 
                                                   count() 
  )
  
  ###DENV II###
  
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_DENGUE_RS %>% 
                                                    filter(ID_MN_RESI == i, 
                                                           SOROTIPO == 2) %>% 
                                                    count() 
  )
  
  ###DENV III###
  
  AUX[which(AUX$COD_IBGE == i), 16] <- as.integer(SINAN_DENGUE_RS %>% 
                                                    filter(ID_MN_RESI == i, 
                                                           SOROTIPO == 3) %>% 
                                                    count() 
  )
  ###DENV IV###                                     
  
  AUX[which(AUX$COD_IBGE == i), 17]<- as.integer(SINAN_DENGUE_RS %>% 
                                                   filter(ID_MN_RESI == i, 
                                                          SOROTIPO == 4) %>% 
                                                   count() 
  )
  ###Hospitalização###
  
  AUX[which(AUX$COD_IBGE == i), 18] <- as.integer(SINAN_DENGUE_RS %>% 
                                                    filter(ID_MN_RESI == i, 
                                                           HOSPITALIZ == 1) %>% 
                                                    count() 
  )
  ###Óbitos###
  
  AUX[which(AUX$COD_IBGE == i), 19] <- as.integer(SINAN_DENGUE_RS %>% 
                                                    filter(ID_MN_RESI == i, 
                                                           EVOLUCAO == 2) %>% 
                                                    count() 
  )
  
  ###Inconclusivos###
  
  AUX[which(AUX$COD_IBGE == i), 20] <- as.integer(SINAN_DENGUE_RS %>% 
                                                    filter(ID_MN_RESI == i, 
                                                           CLASSI_FIN == 8) %>% 
                                                    count() 
  )
}

#########################
###    Incidência     ###
#########################

AUX$Incidencia <- (AUX$Autoctones/AUX$Populacao)*100000  
AUX$Incidencia <- format(round(AUX$Incidencia, 2))
AUX$Incidencia <- as.numeric(AUX$Incidencia)

###################################################################################################
####                Incluindo coluna de CASOS EM INVESTIGAÇÂO na tabela RS22_GERAL             ####
####                Esta coluna só tem sentido no período sazonal atual                        ####
####                Casos em investigação de períodos anteriores são                           ####
####                INCONCLUSIVOS.                                                             ####
###################################################################################################

AUX$Em_Investigacao <- as.integer(AUX$Notificados) - (as.integer(AUX$Dengue + AUX$D_S_A + AUX$Dengue_Grave + AUX$Descartados))

AUX$Sorotipos <- NA

assign(paste0("RS", RS, "_22_23_GERAL"), AUX)

####################################################################################################
####      Elaborando Quadro com dados de sexo, idade, zona de moradia e escolaridade           #####
####################################################################################################

AUX <- data.frame(RS = BASE_IBGE[which(BASE_IBGE$RS == RS), 1])

AUX$Municipio <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Menos_1_ano <- NA

AUX$Um_a_Cinco_Anos <- NA

AUX$Cinco_a_Doze_Anos <- NA

AUX$Doze_a_Dezoito_Anos <- NA

AUX$Dezoito_a_Cinq_Nove <- NA

AUX$Maior_Sessenta <- NA

AUX$Area_Urbana <- NA

AUX$Area_Rural <- NA

AUX$Sexo_Feminino <- NA

AUX$Sexo_Masculino <- NA

AUX$Analfabeto <- NA

AUX$Fundamental_Incompleto <- NA

AUX$Fundamental <- NA

AUX$Ens_Medio_Incompleto <- NA

AUX$Ens_Medio<- NA

AUX$Ens_Superior_Incompleto<- NA

AUX$Ens_Superior<- NA

AUX$Escolaridade_Ignorada<- NA

###      For Loop para geração da tabela RS22_Extra       ###

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 4] <- as.integer(SINAN_DENGUE_RS %>% 
                                                   filter(ID_MN_RESI == i,  
                                                          NU_IDADE_N <=3012) %>% 
                                                   count() 
  )
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_DENGUE_RS %>% 
                                                   filter(ID_MN_RESI == i,  
                                                          NU_IDADE_N > 4000 
                                                          & 
                                                            NU_IDADE_N <=4005) %>% 
                                                   count() 
  )
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_DENGUE_RS %>% 
                                                   filter(ID_MN_RESI == i,
                                                          NU_IDADE_N > 4005 
                                                          & 
                                                            NU_IDADE_N <=4012) %>% 
                                                   count() 
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_DENGUE_RS %>% 
                                                   filter(ID_MN_RESI == i, 
                                                          NU_IDADE_N > 4012 
                                                          & 
                                                            NU_IDADE_N <=4018) %>% 
                                                   count() 
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_DENGUE_RS %>% 
                                                   filter(ID_MN_RESI == i, 
                                                          NU_IDADE_N > 4018 
                                                          & 
                                                            NU_IDADE_N <= 4059) %>%
                                                   count() 
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_DENGUE_RS %>% 
                                                   filter(ID_MN_RESI == i, 
                                                          NU_IDADE_N > 4059 ) %>%
                                                   count() 
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_DENGUE_RS %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ZONA == 1) %>% 
                                                    count() 
  )
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_DENGUE_RS %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ZONA == 2) %>% 
                                                    count() 
  )
  
  AUX[which(AUX$COD_IBGE == i), 12]  <- as.integer(SINAN_DENGUE_RS %>% 
                                                     filter(ID_MN_RESI == i, 
                                                            CS_SEXO == "F") %>% 
                                                     count() 
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_DENGUE_RS %>% 
                                                    filter(ID_MN_RESI == i, 
                                                           CS_SEXO == "M") %>% 
                                                    count() 
  )
  
  AUX[which(AUX$COD_IBGE == i), 14]<- as.integer(SINAN_DENGUE_RS %>% 
                                                   filter(ID_MN_RESI == i, 
                                                          CS_ESCOL_N == 0) %>% 
                                                   count() 
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_DENGUE_RS %>% 
                                                    filter(ID_MN_RESI == i, 
                                                           CS_ESCOL_N == 1 
                                                           | 
                                                             CS_ESCOL_N == 2 
                                                           | 
                                                             CS_ESCOL_N == 3) %>%
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 16] <- as.integer(SINAN_DENGUE_RS %>% 
                                                    filter(ID_MN_RESI == i, 
                                                           CS_ESCOL_N == 4) %>% 
                                                    count() 
  )
  
  AUX[which(AUX$COD_IBGE == i), 17] <- as.integer(SINAN_DENGUE_RS %>% 
                                                    filter(ID_MN_RESI == i, 
                                                           CS_ESCOL_N == 5) %>% 
                                                    count() 
  )
  
  AUX[which(AUX$COD_IBGE == i), 18] <- as.integer(SINAN_DENGUE_RS %>% 
                                                    filter(ID_MN_RESI == i, 
                                                           CS_ESCOL_N == 6) %>% 
                                                    count() 
  )
  
  AUX[which(AUX$COD_IBGE == i), 19] <- as.integer(SINAN_DENGUE_RS %>% 
                                                    filter(ID_MN_RESI == i, 
                                                           CS_ESCOL_N == 7) %>% 
                                                    count() 
  )
  
  AUX[which(AUX$COD_IBGE == i), 20] <- as.integer(SINAN_DENGUE_RS %>% 
                                                    filter(ID_MN_RESI == i, 
                                                           CS_ESCOL_N == 8) %>% 
                                                    count() 
  )
  
  
  AUX[which(AUX$COD_IBGE == i), 21]<- as.integer(SINAN_DENGUE_RS %>% 
                                                   filter(ID_MN_RESI == i, 
                                                          CS_ESCOL_N == 9) %>% 
                                                   count() 
  )
}                                             

assign(paste0("RS", RS, "_22_23_EXTRA"), AUX)

######################################################################################################
###         Elaborando tabelas de sinais e sintomas. Possível somente a partir de 2015.            ###
######################################################################################################

AUX <- data.frame(RS = BASE_IBGE[which(BASE_IBGE$RS == RS), 1])

AUX$Municipio <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Febre <- NA

AUX$Cefaleia <- NA

AUX$Mialgia <- NA

AUX$Exantema <- NA

AUX$Vomitos <- NA

AUX$Nausea <- NA

AUX$Dor_nas_Costas <- NA

AUX$Conjuntivite <- NA

AUX$Artrite  <- NA

AUX$Artralgia <- NA

AUX$Petequias <- NA

AUX$Leucopenia <- NA

AUX$Dor_Retroorbital <- NA

AUX$Prova_do_Laco_Positiva <- NA

###Elaborando for loop para sinais e sintomas.###

for (i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 4] <- as.integer(SINAN_DENGUE_RS %>%
                                                   filter(ID_MN_RESI == i,
                                                          FEBRE == 1) %>%
                                                   count()
  )
  
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_DENGUE_RS %>%
                                                   filter(ID_MN_RESI == i,
                                                          CEFALEIA == 1) %>%
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_DENGUE_RS %>%
                                                   filter(ID_MN_RESI == i,
                                                          MIALGIA == 1) %>%
                                                   count()
  )
  
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_DENGUE_RS %>%
                                                   filter(ID_MN_RESI == i,
                                                          EXANTEMA == 1) %>%
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_DENGUE_RS %>%
                                                   filter(ID_MN_RESI == i,
                                                          VOMITO == 1) %>%
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9]<- as.integer(SINAN_DENGUE_RS %>%
                                                  filter(ID_MN_RESI == i,
                                                         NAUSEA == 1) %>%
                                                  count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10]<- as.integer(SINAN_DENGUE_RS %>%
                                                   filter(ID_MN_RESI == i,
                                                          DOR_COSTAS == 1) %>%
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_DENGUE_RS %>%
                                                    filter(ID_MN_RESI == i,
                                                           CONJUNTVIT == 1) %>%
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(SINAN_DENGUE_RS %>%
                                                    filter(ID_MN_RESI == i,
                                                           ARTRITE == 1) %>%
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_DENGUE_RS %>%
                                                    filter(ID_MN_RESI == i,
                                                           ARTRALGIA == 1) %>%
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(SINAN_DENGUE_RS %>%
                                                    filter(ID_MN_RESI == i,
                                                           PETEQUIA_N == 1) %>%
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_DENGUE_RS %>%
                                                    filter(ID_MN_RESI == i,
                                                           LEUCOPENIA == 1) %>%
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 16] <- as.integer(SINAN_DENGUE_RS %>%
                                                    filter(ID_MN_RESI == i,
                                                           DOR_RETRO == 1) %>%
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 17] <- as.integer(SINAN_DENGUE_RS %>%
                                                    filter(ID_MN_RESI == i,
                                                           LACO == 1) %>%
                                                    count()
  )
}

assign(paste0("RS", RS, "_22_23_SINAIS_Notificados"), AUX)

###    Elaborando tabelas de sinais e sintomas. Possível somente a partir de 2015.         ###

AUX <- data.frame(RS = BASE_IBGE[which(BASE_IBGE$RS == RS), 1])

AUX$Municipio <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Febre <- NA

AUX$Cefaleia <- NA

AUX$Mialgia <- NA

AUX$Exantema <- NA

AUX$Vomitos <- NA

AUX$Nausea <- NA

AUX$Dor_nas_Costas <- NA

AUX$Conjuntivite <- NA

AUX$Artrite  <- NA

AUX$Artralgia <- NA

AUX$Petequias <- NA

AUX$Leucopenia <- NA

AUX$Dor_Retroorbital <- NA

AUX$Prova_do_Laco_Positiva <- NA

###   Elaborando for loop para sinais e sintomas.   ###

for (i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 4] <- as.integer(SINAN_DENGUE_RS %>%
                                                   filter(ID_MN_RESI == i,
                                                          CLASSI_FIN == 10 
                                                          | 
                                                            CLASSI_FIN == 11 
                                                          |
                                                            CLASSI_FIN == 12,
                                                          FEBRE == 1) %>%
                                                   count()
  )
  
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_DENGUE_RS %>%
                                                   filter(ID_MN_RESI == i,
                                                          CLASSI_FIN == 10 
                                                          | 
                                                            CLASSI_FIN == 11 
                                                          |
                                                            CLASSI_FIN == 12,
                                                          CEFALEIA == 1) %>%
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_DENGUE_RS %>%
                                                   filter(ID_MN_RESI == i,
                                                          CLASSI_FIN == 10 
                                                          | 
                                                            CLASSI_FIN == 11 
                                                          |
                                                            CLASSI_FIN == 12,
                                                          MIALGIA == 1) %>%
                                                   count()
  )
  
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_DENGUE_RS %>%
                                                   filter(ID_MN_RESI == i,
                                                          CLASSI_FIN == 10 
                                                          | 
                                                            CLASSI_FIN == 11 
                                                          |
                                                            CLASSI_FIN == 12,
                                                          EXANTEMA == 1) %>%
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_DENGUE_RS %>%
                                                   filter(ID_MN_RESI == i,
                                                          CLASSI_FIN == 10 
                                                          | 
                                                            CLASSI_FIN == 11 
                                                          |
                                                            CLASSI_FIN == 12,
                                                          VOMITO == 1) %>%
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9]<- as.integer(SINAN_DENGUE_RS %>%
                                                  filter(ID_MN_RESI == i,
                                                         CLASSI_FIN == 10 
                                                         | 
                                                           CLASSI_FIN == 11 
                                                         |
                                                           CLASSI_FIN == 12,
                                                         NAUSEA == 1) %>%
                                                  count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10]<- as.integer(SINAN_DENGUE_RS %>%
                                                   filter(ID_MN_RESI == i,
                                                          CLASSI_FIN == 10 
                                                          | 
                                                            CLASSI_FIN == 11 
                                                          |
                                                            CLASSI_FIN == 12,
                                                          DOR_COSTAS == 1) %>%
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_DENGUE_RS %>%
                                                    filter(ID_MN_RESI == i,
                                                           CLASSI_FIN == 10 
                                                           | 
                                                             CLASSI_FIN == 11 
                                                           |
                                                             CLASSI_FIN == 12,
                                                           CONJUNTVIT == 1) %>%
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(SINAN_DENGUE_RS %>%
                                                    filter(ID_MN_RESI == i,
                                                           CLASSI_FIN == 10 
                                                           | 
                                                             CLASSI_FIN == 11 
                                                           |
                                                             CLASSI_FIN == 12,
                                                           ARTRITE == 1) %>%
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_DENGUE_RS %>%
                                                    filter(ID_MN_RESI == i,
                                                           CLASSI_FIN == 10 
                                                           | 
                                                             CLASSI_FIN == 11 
                                                           |
                                                             CLASSI_FIN == 12,
                                                           ARTRALGIA == 1) %>%
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(SINAN_DENGUE_RS %>%
                                                    filter(ID_MN_RESI == i,
                                                           CLASSI_FIN == 10 
                                                           | 
                                                             CLASSI_FIN == 11 
                                                           |
                                                             CLASSI_FIN == 12,
                                                           PETEQUIA_N == 1) %>%
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_DENGUE_RS %>%
                                                    filter(ID_MN_RESI == i,
                                                           CLASSI_FIN == 10 
                                                           | 
                                                             CLASSI_FIN == 11 
                                                           |
                                                             CLASSI_FIN == 12,
                                                           LEUCOPENIA == 1) %>%
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 16] <- as.integer(SINAN_DENGUE_RS %>%
                                                    filter(ID_MN_RESI == i,
                                                           CLASSI_FIN == 10 
                                                           | 
                                                             CLASSI_FIN == 11 
                                                           |
                                                             CLASSI_FIN == 12,
                                                           DOR_RETRO == 1) %>%
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 17] <- as.integer(SINAN_DENGUE_RS %>%
                                                    filter(ID_MN_RESI == i,
                                                           CLASSI_FIN == 10 
                                                           | 
                                                             CLASSI_FIN == 11 
                                                           |
                                                             CLASSI_FIN == 12,
                                                           LACO == 1) %>%
                                                    count()
  )
}

assign(paste0("RS", RS, "_22_23_SINAIS_Confirmados"), AUX)

###    Montando tabela de doenças pré-existentes    ###

AUX <- data.frame(RS = BASE_IBGE[which(BASE_IBGE$RS == RS), 1])

AUX$Município <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Diabetes <- NA

AUX$Doencas_Hematologicas <- NA

AUX$Hepatopatias <- NA

AUX$DRC <- NA

AUX$Hipertensao <- NA

AUX$Doenca_Acido_Peptica <- NA

AUX$Doenca_Auto_Imune <- NA

###    Construindo o for loop    ###

for (i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 4] <- as.integer(SINAN_DENGUE_RS %>%
                                                   filter(ID_MN_RESI == i, 
                                                          DIABETES == 1) %>%
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_DENGUE_RS %>%
                                                   filter(ID_MN_RESI == i,
                                                          HEMATOLOG == 1) %>%
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_DENGUE_RS %>%
                                                   filter(ID_MN_RESI == i,
                                                          HEPATOPAT == 1) %>%
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7]<- as.integer(SINAN_DENGUE_RS %>%
                                                  filter(ID_MN_RESI == i,
                                                         RENAL == 1) %>%
                                                  count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8]<- as.integer(SINAN_DENGUE_RS %>%
                                                  filter(ID_MN_RESI == i,
                                                         HIPERTENSA == 1) %>%
                                                  count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_DENGUE_RS %>%
                                                   filter(ID_MN_RESI == i,
                                                          ACIDO_PEPT == 1) %>%
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_DENGUE_RS %>%
                                                    filter(ID_MN_RESI == i,
                                                           AUTO_IMUNE == 1) %>%
                                                    count()
  )
}

assign(paste0("RS", RS, "_22_23_DOENCAS_PRE_EXISTENTES"), AUX)

###     Construindo tabela sinais de alarme     ###

AUX <- data.frame(RS = BASE_IBGE[which(BASE_IBGE$RS == RS), 1])

AUX$Municipio <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Hipotensao_Lipotimia <- NA

AUX$Queda_Abrupta_Plaquetas <- NA

AUX$Vomitos_Persistentes <- NA

AUX$Dor_Abdominal <- NA

AUX$Letargia <- NA

AUX$Aumento_Hematocrito <- NA

AUX$hemorragias <- NA

AUX$Hepatomegalia <- NA

AUX$Acumulo_Liquidos <- NA

###    Construindo o for loop     ###

for (i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 4] <- as.integer(SINAN_DENGUE_RS %>%
                                                   filter(ID_MN_RESI == i,
                                                          ALRM_HIPOT == 1) %>%
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_DENGUE_RS %>%
                                                   filter(ID_MN_RESI == i,
                                                          ALRM_PLAQ == 1) %>%
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_DENGUE_RS %>%
                                                   filter(ID_MN_RESI == i,
                                                          ALRM_VOM == 1) %>%
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_DENGUE_RS %>%
                                                   filter(ID_MN_RESI == i,
                                                          ALRM_ABDOM == 1) %>%
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_DENGUE_RS %>%
                                                   filter(ID_MN_RESI == i,
                                                          ALRM_LETAR == 1) %>%
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_DENGUE_RS %>%
                                                   filter(ID_MN_RESI == i,
                                                          ALRM_HEMAT == 1) %>%
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_DENGUE_RS %>%
                                                    filter(ID_MN_RESI == i,
                                                           ALRM_SANG == 1) %>%
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_DENGUE_RS %>%
                                                    filter(ID_MN_RESI == i,
                                                           ALRM_HEPAT == 1) %>%
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(SINAN_DENGUE_RS %>%
                                                    filter(ID_MN_RESI == i,
                                                           ALRM_LIQ == 1) %>%
                                                    count()
                                                  
  )
}

assign(paste0("RS", RS, "_22_23_SINAIS_DE_ALARME"), AUX)

###     Construindo tabela Dengue Grave      ###

AUX <- data.frame(RS = BASE_IBGE[which(BASE_IBGE$RS == RS), 1])

AUX$Municipio <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Pulso_Debil <- NA

AUX$PA_Convergente <- NA

AUX$TPC <- NA

AUX$Acumulo_Liquidos_Insuf_Respiratoria <- NA

AUX$Taquicardia <- NA

AUX$Extremidades_Frias <- NA

AUX$Hipotensão_Arterial <- NA

AUX$Hematemese <- NA

AUX$Melena <- NA

AUX$Metrorragia <- NA

AUX$Sangramento_SNC <- NA

AUX$Aumento_ALT_AST <- NA

AUX$Miocardite <- NA

AUX$Alteracao_Consciencia <- NA

###      Construindo o for loop        ###

for (i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 4] <- as.integer(SINAN_DENGUE_RS %>%
                                                   filter(ID_MN_RESI == i,
                                                          GRAV_PULSO == 1) %>%
                                                   count()
  )     
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_DENGUE_RS %>%
                                                   filter(ID_MN_RESI == i,
                                                          GRAV_CONV == 1) %>%
                                                   count()
  )   
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_DENGUE_RS %>%
                                                   filter(ID_MN_RESI == i,
                                                          GRAV_ENCH == 1) %>%
                                                   count()
  )  
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_DENGUE_RS %>%
                                                   filter(ID_MN_RESI == i,
                                                          GRAV_INSUF == 1) %>%
                                                   count()
  ) 
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_DENGUE_RS %>%
                                                   filter(ID_MN_RESI == i,
                                                          GRAV_TAQUI == 1) %>%
                                                   count()
  ) 
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_DENGUE_RS %>%
                                                   filter(ID_MN_RESI == i,
                                                          GRAV_EXTRE == 1) %>%
                                                   count()
  ) 
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_DENGUE_RS %>%
                                                    filter(ID_MN_RESI == i,
                                                           GRAV_HIPOT == 1) %>%
                                                    count()
  ) 
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_DENGUE_RS %>%
                                                    filter(ID_MN_RESI == i,
                                                           GRAV_HEMAT == 1) %>%
                                                    count()
  ) 
  
  AUX[which(AUX$COD_IBGE == i), 12]<- as.integer(SINAN_DENGUE_RS %>%
                                                   filter(ID_MN_RESI == i,
                                                          GRAV_MELEN == 1) %>%
                                                   count()
  ) 
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_DENGUE_RS %>%
                                                    filter(ID_MN_RESI == i,
                                                           GRAV_METRO == 1) %>%
                                                    count()
  ) 
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(SINAN_DENGUE_RS %>%
                                                    filter(ID_MN_RESI == i,
                                                           GRAV_SANG == 1) %>%
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_DENGUE_RS %>%
                                                    filter(ID_MN_RESI == i,
                                                           GRAV_AST == 1) %>%
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 16] <- as.integer(SINAN_DENGUE_RS %>%
                                                    filter(ID_MN_RESI == i,
                                                           GRAV_MIOC == 1) %>%
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 17] <- as.integer(SINAN_DENGUE_RS %>%
                                                    filter(ID_MN_RESI == i,
                                                           GRAV_CONSC == 1) %>%
                                                    count()
  )
}

assign(paste0("RS", RS, "_22_23_DENGUE_GRAVE"), AUX)


#########################################################################################################################
####################    FIM FIM FIM FIM FIM FIM FIM FIM FIM FIM FIM FIM FIM FIM #########################################
#################### Daqui para baixo são tabulações que servirão somente para  #########################################
#################### o Informe Epidemiológico.                                  #########################################
#########################################################################################################################

#######################################################################################
####       Trabalhando com a tabela RS22_SINAN do período atual. Realizando a    ######
####      decodificação dos fatores em linguagem mais acessível aos municípios   ######
#######################################################################################

AUX <- SINAN_DENGUE_RS

AUX$ID_AGRAVO <- factor(AUX$ID_AGRAVO,
                        label = c("Dengue", "Chikungunya"), 
                        levels = c("A90", "A92")
)

###Sintomas###
AUX$FEBRE <- factor(AUX$FEBRE,
                    label = c("SIM", "NÃO"), 
                    levels = c(1, 2)
)

AUX$MIALGIA <- factor(AUX$MIALGIA,
                      label = c("SIM", "NÃO"), 
                      levels = c(1, 2)
)

AUX$CEFALEIA <- factor(AUX$CEFALEIA,
                       label = c("SIM", "NÃO"), 
                       levels = c(1, 2)
)

AUX$EXANTEMA <- factor(AUX$EXANTEMA,
                       label = c("SIM", "NÃO"), 
                       levels = c(1, 2)
)

AUX$VOMITO <- factor(AUX$VOMITO,
                     label = c("SIM", "NÃO"), 
                     levels = c(1, 2)
)

AUX$NAUSEA <- factor(AUX$NAUSEA,
                     label = c("SIM", "NÃO"), 
                     levels = c(1, 2)
)

AUX$DOR_COSTAS <- factor(AUX$DOR_COSTAS,
                         label = c("SIM", "NÃO"), 
                         levels = c(1, 2)
)

AUX$DOR_RETRO <- factor(AUX$DOR_RETRO,
                        label = c("SIM", "NÃO"), 
                        levels = c(1, 2)
)

AUX$CONJUNTVIT <- factor(AUX$CONJUNTVIT,
                         label = c("SIM", "NÃO"), 
                         levels = c(1, 2)
)

AUX$ARTRALGIA <- factor(AUX$ARTRALGIA,
                        label = c("SIM", "NÃO"), 
                        levels = c(1, 2)
)

AUX$ARTRITE <- factor(AUX$ARTRITE,
                      label = c("SIM", "NÃO"), 
                      levels = c(1, 2)
)

AUX$PETEQUIA_N <- factor(AUX$PETEQUIA_N,
                         label = c("SIM", "NÃO"), 
                         levels = c(1, 2)
)

AUX$LEUCOPENIA <- factor(AUX$LEUCOPENIA,
                         label = c("SIM", "NÃO"), 
                         levels = c(1, 2)
)

AUX$LACO <- factor(AUX$LACO,
                   label = c("SIM", "NÃO"), 
                   levels = c(1, 2)
)

###Doenças Pré-existentes

AUX$DIABETES <- factor(AUX$DIABETES,
                       label = c("SIM", "NÃO"), 
                       levels = c(1, 2)
)

AUX$HEMATOLOG <- factor(AUX$HEMATOLOG,
                        label = c("SIM", "NÃO"), 
                        levels = c(1, 2)
)

AUX$HEPATOPAT <- factor(AUX$HEPATOPAT,
                        label = c("SIM", "NÃO"), 
                        levels = c(1, 2)
)

AUX$RENAL <- factor(AUX$RENAL,
                    label = c("SIM", "NÃO"), 
                    levels = c(1, 2)
)

AUX$HIPERTENSA <- factor(AUX$HIPERTENSA,
                         label = c("SIM", "NÃO"), 
                         levels = c(1, 2)
)

AUX$ACIDO_PEPT <- factor(AUX$ACIDO_PEPT,
                         label = c("SIM", "NÃO"), 
                         levels = c(1, 2)
)

AUX$AUTO_IMUNE <- factor(AUX$AUTO_IMUNE,
                         label = c("SIM", "NÃO"), 
                         levels = c(1, 2)
)

####Outros####

AUX$CS_GESTANT <- factor(AUX$CS_GESTANT,
                         label = c("1_TRI", "2_TRI", "3_TRI", "IDADE GESTACIONAL IGN", "NÃO", "NÃO SE APLICA", "IGNORADO"), 
                         levels = c(1, 2, 3, 4, 5, 6, 9)
)

AUX$CS_ESCOL_N <- factor(AUX$CS_ESCOL_N,
                         label = c("ANALFABETO", "1 a 4 SÉRIE DO FUNDAMENTAL INCOMPLETA", "4 SÉRIE DO FUNDAMENTAL COMPLETA", "5 a 8 SÉRIE DO FUNDAMENTAL INCOMPLETA", "FUNDAMENTAL COMPLETO", "ENSINO MÉDIO INCOMPLETO", "ENSINO MÉDIO COMPLETO", "SUPERIOR INCONPLETO", "SUPERIOR COMPLETO", "IGNORADO", "NÃO SE APLICA"), 
                         levels = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
)

AUX$RESUL_SORO <- factor(AUX$RESUL_SORO,
                         label = c("REAGENTE", "NÃO REAGENTE", "INCONCLUSIVO", "NÃO REALIZADO"), 
                         levels = c(1, 2, 3, 4)
)


AUX$RESUL_PCR_ <- factor(AUX$RESUL_PCR_,
                         label = c("DETECTÁVEL", "NÃO DETECTÀVEL", "INCONCLUSIVO", "NÃO REALIZADO"), 
                         levels = c(1, 2, 3, 4)
)

AUX$SOROTIPO <- factor(AUX$SOROTIPO,
                       label = c("I", "II", "III", "IV"), 
                       levels = c(1, 2, 3, 4)
)

AUX$CLASSI_FIN <- factor(AUX$CLASSI_FIN,
                         label = c("DESCARTADO", "INCONCLUSIVO", "DENGUE", "D.S.A.", "IDENGUE_GRAVE", "CHIKUNGUNYA"), 
                         levels = c(5, 8, 10, 11, 12, 13)
)

AUX$CRITERIO <- factor(AUX$CRITERIO,
                       label = c("LABORATORIAL", "CLÍNICO-EPIDEMIOLÓGICO", "EM INVESTIGAÇÃO"), 
                       levels = c(1, 2, 3)
)

AUX$TPAUTOCTO <- factor(AUX$TPAUTOCTO,
                        label = c("SIM", "NÃO"), 
                        levels = c(1, 2)
)

AUX$HOSPITALIZ <- factor(AUX$HOSPITALIZ,
                         label = c("SIM", "NÃO"), 
                         levels = c(1, 2)
)

AUX$EVOLUCAO <- factor(AUX$EVOLUCAO,
                       label = c("CURA", "ÓBITO PELO AGRAVO", "ÓBITO POR OUTRAS CAUSAS","ÓBITO EM INVESTIGAÇÃO", "INDETERMINADO"), 
                       levels = c(1, 2, 3, 4, 9)
)

AUX$CS_ZONA <- factor(AUX$CS_ZONA,
                      label = c("URBANA", "RURAL", "PERIURBANA","INDETERMINADO"), 
                      levels = c(1, 2, 3, 9)
)

AUX$ALRM_LETAR <- factor(AUX$ALRM_LETAR,
                         label = c("SIM", "NÃO"), 
                         levels = c(1, 2)
)

AUX$ALRM_HEPAT <- factor(AUX$ALRM_HEPAT,
                         label = c("SIM", "NÃO"), 
                         levels = c(1, 2)
)

AUX$ALRM_LIQ <- factor(AUX$ALRM_LIQ,
                       label = c("SIM", "NÃO"), 
                       levels = c(1, 2)
)

AUX$ALRM_HIPOT <- factor(AUX$ALRM_HIPOT,
                         label = c("SIM", "NÃO"), 
                         levels = c(1, 2)
)

AUX$ALRM_PLAQ <- factor(AUX$ALRM_PLAQ,
                        label = c("SIM", "NÃO"), 
                        levels = c(1, 2)
)

AUX$ALRM_VOM <- factor(AUX$ALRM_VOM,
                       label = c("SIM", "NÃO"), 
                       levels = c(1, 2)
)

AUX$ALRM_SANG <- factor(AUX$ALRM_SANG,
                        label = c("SIM", "NÃO"), 
                        levels = c(1, 2)
)

AUX$ALRM_HEMAT <- factor(AUX$ALRM_LETAR,
                         label = c("SIM", "NÃO"), 
                         levels = c(1, 2)
)

AUX$ALRM_ABDOM <- factor(AUX$ALRM_ABDOM,
                         label = c("SIM", "NÃO"), 
                         levels = c(1, 2)
)
####AUX$Municipio 

AUX01 <- data.frame(COD = AUX[,12], 
                    Municipio = NA)

for (i in AUX[,12]){
  AUX01[which(AUX01$COD == i), 2] <- BASE_IBGE_BRASIL[which(BASE_IBGE_BRASIL$Código.Município.Completo == i),13]
  
}

AUX[,12] <- AUX01[, 2]

####Município de Residência

AUX02 <- data.frame(COD = AUX[,20], 
                    Municipio = NA)

for (i in AUX[,20]){
  AUX02[which(AUX02$COD == i), 2] <- BASE_IBGE_BRASIL[which(BASE_IBGE_BRASIL$Código.Município.Completo == i),13]
  
}

AUX[,20] <- AUX02[, 2]

colnames(AUX)<- c("RS", "SINAN", "Latitude", "Longitude", 
                  "Agravo", "Data_Notificacao", "ANO", "SE_Notificacao", 
                  "Data_Primeiros_Sintomas", "SE_Primeiros_Sintomas", "UF_Notificacao", 
                  "Municipio", "Nome", "Data_Nascimento", "Idade", "Sexo", "Gestante", 
                  "Escolaridade", "Nome_Mae", "Municipio_Residencia", "UF_Residencia", 
                  "RS_Residencia", "Logradouro", "Numero", "Bairro", "CEP", "Zona", 
                  "Data_Digitacao", "Data_Investigacao", "Febre", "Mialgia", "Cefaleia", 
                  "Exantema", "Vomito", "Nausea", "Dor_nas_Costas", "Conjuntivite", "Artrite", 
                  "Artralgia_Intensa", "Petequias", "Leucopenia", "Prova_do_Laco_Positiva", 
                  "Dor_retroorbital", "Diabetes", "Doenca_Hematologica", "Hepatopatia", 
                  "Doenca_Renal", "Hipertensao", "Doenca_Acido_Peptica", "Doenca_Auto_Imune", 
                  "Data_Sorologia", "Resultado_Sorologia", "Data_PCR", "Resultado_PCR", "Sorotipo", 
                  "Classificacao_Final", "Critério_Encerramento", "Autoctone", "UF_Infeccao", 
                  "Municipio_Infeccao", "Bairro_Infeccao", "Evolucao", "Hospitalizado", 
                  "Data_Internamento", "Data_Obito", "Data_Encerramento", "Data_SNA", "Letargia", 
                  "Hepatomegalia", "Acumulo_Liquidos", "Hipotensao_Lipotimia", "Queda_Abrupta_Plaquetas", 
                  "Vomitos_Persistentes", "Hemorragias", "Aumento_Hematocrito", "Dor_Abdominal", 
                  "Data_Dengue_Grave", "Pulso_Debil", "PA_Convergente", "TPC", "Acumulo_Liq_Insuficiencia_Resp", 
                  "Taquicardia", "Extremidades_Frias", "Hipotensao", "Hematemese", "Melena", "Metrorragia_", 
                  "Sangramento_SNC", "Aumento_AST_ALT", "Miocardite", "Alteracao_Consciencia", "Outros_Orgaos", 
                  "Manifestacao_Hemorragica", "Epistaxe", "Gengivorragia", "Metrorragia", "Observacoes" )

assign(paste0("RS", RS, "_22_23_SINAN_DECODIFICADO"), AUX) 

#########################################################################################################################
##################  Trabalhando os arquivos de série histórica   ########################################################
#########################################################################################################################

####      Adicionando os dados do período atual na tabela Série Histórica     ####

RS22_Serie_Historica_Base[1, 15] <- sum(RS22_22_23_GERAL$Notificados)
RS22_Serie_Historica_Base[2, 15] <- sum(RS22_22_23_GERAL$Dengue)
RS22_Serie_Historica_Base[3, 15] <- sum(RS22_22_23_GERAL$D_S_A)
RS22_Serie_Historica_Base[4, 15] <- sum(RS22_22_23_GERAL$Dengue_Grave)
RS22_Serie_Historica_Base[5, 15] <- sum(RS22_22_23_GERAL$Hospitalizacao)
RS22_Serie_Historica_Base[6, 15] <- sum(RS22_22_23_GERAL$Autoctones)
RS22_Serie_Historica_Base[7, 15] <- sum(RS22_22_23_GERAL$DENV_I)
RS22_Serie_Historica_Base[8, 15] <- sum(RS22_22_23_GERAL$DENV_II)
RS22_Serie_Historica_Base[9, 15] <- sum(RS22_22_23_GERAL$DENV_III)
RS22_Serie_Historica_Base[10, 15] <- sum(RS22_22_23_GERAL$DENV_IV)
RS22_Serie_Historica_Base[11, 15] <- sum(RS22_22_23_GERAL$Obitos)


AUX <- as.data.frame(t(RS22_Serie_Historica_Base))

colnames(AUX) <- AUX[1,]

AUX <- AUX[-1,]

AUX[,12] <- c("2009/10", "2010/11", "2011/12", "2012/13", "2013/14", 
              "2014/15", "2015/16", "2016/17", "2017/18", "2018/19", "2019/20", 
              "2020/21", "2021/22", "2022/23")

colnames(AUX)[12] <- "Periodo"

AUX <- AUX[,c(12, 1:11)]

rownames(AUX) <- c(1:14)

RS22_Serie_Historica <- AUX

rm(AUX, RS22_Serie_Historica_Base)

RS22_Serie_Historica[,2] <- as.numeric(RS22_Serie_Historica[,2])
RS22_Serie_Historica[,3] <- as.numeric(RS22_Serie_Historica[,3])
RS22_Serie_Historica[,4] <- as.numeric(RS22_Serie_Historica[,4])
RS22_Serie_Historica[,5] <- as.numeric(RS22_Serie_Historica[,5])
RS22_Serie_Historica[,6] <- as.numeric(RS22_Serie_Historica[,6])
RS22_Serie_Historica[,7] <- as.numeric(RS22_Serie_Historica[,7])
RS22_Serie_Historica[,8] <- as.numeric(RS22_Serie_Historica[,8])
RS22_Serie_Historica[,9] <- as.numeric(RS22_Serie_Historica[,9])
RS22_Serie_Historica[,10] <- as.numeric(RS22_Serie_Historica[,10])
RS22_Serie_Historica[,11] <- as.numeric(RS22_Serie_Historica[,11])
RS22_Serie_Historica[,12] <- as.numeric(RS22_Serie_Historica[,12])

####################################################################################################################
################Trabalhando as tabelas base dos Canais Endêmicos####################################################
####################################################################################################################

######     Canal Endêmico    NOTIFICADOS#####

RS22_CE_Notificados_Base[14, 1] <- "2022/23"
RS22_CE_Notificados_Base[14, 2:54] <- as.integer(data.frame(RS22_22_23_SE_Notificados[17, 2:54]))

#####################################################################################################################
#####                   Utilizando objetos auxiliares porque se transpor o data frame                   #############
#####                   direto ele transforma as variáveis em caracter.                                 #############
#####                                                                                                   #############         
#####                     NÃO FOI DESCARTADO AINDA OS PERÍODOS EPIDÊMICOS                               #############
#####               VERIFICAR SE PODE-SE UTILIZAR A MÉDIA COMO LIMITE INFERIOR.                         #############
#####################################################################################################################

AUX <- RS22_CE_Notificados_Base[,-1]

AUX <- t(AUX)

AUX2 <- RS22_CE_Notificados_Base[,1]

colnames(AUX) <- AUX2

RS22_CE_Notificados <- AUX

######        Criando a coluna de média no data.frame            #####################

AUX <- apply(RS22_CE_Notificados[,], 1 , mean)

RS22_CE_Notificados <- as.data.frame(RS22_CE_Notificados)

RS22_CE_Notificados$Media <- AUX

######              Criando a coluna de Desvio Padrão no data frame                ###############

AUX <- apply(RS22_CE_Notificados[,], 1 , sd)

RS22_CE_Notificados$Desvio_Padrao <- AUX

######       Criando a coluna de Média + 2(DP)    ######################

AUX <- RS22_CE_Notificados[, 15:16]

AUX <- AUX %>% mutate(Lim_Superior = (Media + 1.96 * Desvio_Padrao))

RS22_CE_Notificados$Lim_Superior <- AUX$Lim_Superior

RS22_CE_Notificados[,18] <- rownames(RS22_CE_Notificados)

RS22_CE_Notificados <- RS22_CE_Notificados[, c(18, 1:17)]

RS22_CE_Notificados[,1] <- c(31:53, 1:30)

colnames(RS22_CE_Notificados)[1] <- "Semana_Epidemiológica"

rownames(RS22_CE_Notificados) <- c(1:nrow(RS22_CE_Notificados))

rm(AUX, AUX2, RS22_CE_Notificados_Base)

write.csv (RS22_CE_Notificados, 
           "/home/gustavo/Área de trabalho/Análise_de_Dados/Base_de_Dados/Tabulacoes_R/Arboviroses/RS22_CE_Notificados.csv", 
           row.names = FALSE)

####         Canal Endêmico CONFIRMADOS              ####

RS22_CE_Confirmados_Base[14, 1] <- "2022/23"
RS22_CE_Confirmados_Base[14, 2:54] <- as.integer(data.frame(RS22_22_23_SE_Confirmados[17, 2:54]))

#####################################################################################################################
#####                   Utilizando objetos auxiliares porque se transpor o data frame                   #############
#####                   direto ele transforma as variáveis em caracter.                                 #############
#####                                                                                                   #############         
#####                     NÃO FOI DESCARTADO AINDA OS PERÍODOS EPIDÊMICOS                               #############
#####               VERIFICAR SE PODE-SE UTILIZAR A MÉDIA COMO LIMITE INFERIOR.                         #############
#####################################################################################################################

AUX <- RS22_CE_Confirmados_Base[,-1]

AUX <- t(AUX)

AUX2 <- RS22_CE_Confirmados_Base[,1]

colnames(AUX) <- AUX2

RS22_CE_Confirmados <- AUX

######     Criando a coluna de média no data.frame     #####################

AUX <- apply(RS22_CE_Confirmados[,], 1 , mean)

RS22_CE_Confirmados <- as.data.frame(RS22_CE_Confirmados)

RS22_CE_Confirmados$Media <- AUX

######     Criando a coluna de Desvio Padrão no data frame     ###############

AUX <- apply(RS22_CE_Confirmados[,], 1 , sd)

RS22_CE_Confirmados$Desvio_Padrao <- AUX

######      Criando a coluna de Média + 2(DP)     ###########

AUX <- RS22_CE_Confirmados[, 15:16]

AUX <- AUX %>% mutate(Lim_Superior = (Media + 1.96 * Desvio_Padrao))

RS22_CE_Confirmados$Lim_Superior <- AUX$Lim_Superior

RS22_CE_Confirmados[,18] <- rownames(RS22_CE_Confirmados)

RS22_CE_Confirmados <- RS22_CE_Confirmados[, c(18, 1:17)]

RS22_CE_Confirmados[,1] <- c(31:53, 1:30)

colnames(RS22_CE_Confirmados)[1] <- "Semana_Epidemiológica"

rownames(RS22_CE_Confirmados) <- c(1:nrow(RS22_CE_Confirmados))

rm(AUX, AUX2, RS22_CE_Confirmados_Base)

write.csv (RS22_CE_Confirmados, 
           "/home/gustavo/Área de trabalho/Análise_de_Dados/Base_de_Dados/Tabulacoes_R/Arboviroses/RS22_CE_Confirmados.csv", 
           row.names = FALSE)

