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

setwd("/home/gustavo/Área de Trabalho/Análise_de_Dados/")

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

SE <- as.data.frame("50")  ### Colocar a Semana Epidemiológica atual

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

Periodos_Epidêmicos_SEDE <- c("2009/10",
                            "2010/11",
                            "2011/12",
                            "2012/13",
                            "2013/14",
                            "2014/15",
                            "2015/16",
                            "2016/17",
                            "2017/18",
                            "2018/19",
                        #   "2019/20",
                            "2020/21",
                            "2021/22",
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

DENGON2023 <- read.dbf(file = "Base_de_Dados/DBF/DENGON2023.dbf", 
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


CHIKON2022 <- read.dbf("Base_de_Dados/DBF/CHIKON2022.dbf",
                       as.is = FALSE) %>% select(ID_REGIONA, NU_NOTIFIC, ID_GEO1,ID_GEO2, ID_AGRAVO, ID_REGIONA, DT_NOTIFIC, NU_ANO, SEM_NOT,
                                                 DT_SIN_PRI, SEM_PRI,  SG_UF_NOT, ID_MUNICIP, NM_PACIENT, DT_NASC, NU_IDADE_N, CS_SEXO, 
                                                 CS_GESTANT, CS_ESCOL_N, NM_MAE_PAC, ID_MN_RESI, SG_UF, ID_RG_RESI, NM_LOGRADO, NU_NUMERO, 
                                                 NM_BAIRRO, NU_CEP, CS_ZONA, DT_DIGITA, DT_INVEST, FEBRE, MIALGIA, CEFALEIA, EXANTEMA, VOMITO, 
                                                 NAUSEA, DOR_COSTAS, CONJUNTVIT, ARTRITE, ARTRALGIA, PETEQUIA_N, LEUCOPENIA, LACO, DOR_RETRO,
                                                 DIABETES, HEMATOLOG, HEPATOPAT, RENAL, HIPERTENSA, ACIDO_PEPT, AUTO_IMUNE, DT_SORO, RESUL_SORO, 
                                                 DT_PCR, RESUL_PCR_, SOROTIPO, CLASSI_FIN, CRITERIO, TPAUTOCTO, COUFINF, COMUNINF, CO_BAINF, EVOLUCAO,
                                                 HOSPITALIZ, DT_INTERNA, DT_OBITO, DT_ENCERRA, DT_ALRM, ALRM_LETAR, ALRM_HEPAT, ALRM_LIQ, ALRM_HIPOT, 
                                                 ALRM_PLAQ, ALRM_VOM, ALRM_SANG, ALRM_HEMAT, ALRM_ABDOM, DT_GRAV, GRAV_PULSO, GRAV_CONV, GRAV_ENCH, 
                                                 GRAV_INSUF, GRAV_TAQUI, GRAV_EXTRE, GRAV_HIPOT, GRAV_HEMAT, GRAV_MELEN, GRAV_METRO, GRAV_SANG, 
                                                 GRAV_AST, GRAV_MIOC, GRAV_CONSC, GRAV_ORGAO, MANI_HEMOR, EPISTAXE, GENGIVO, METRO, DS_OBS)

CHIKON2023 <- read.dbf("Base_de_Dados/DBF/CHIKON2023.dbf",
                       as.is = FALSE) %>% select(ID_REGIONA, NU_NOTIFIC, ID_GEO1,ID_GEO2, ID_AGRAVO, ID_REGIONA, DT_NOTIFIC, NU_ANO, SEM_NOT, 
                                                 DT_SIN_PRI, SEM_PRI,  SG_UF_NOT, ID_MUNICIP, NM_PACIENT, DT_NASC, NU_IDADE_N, CS_SEXO, CS_GESTANT, 
                                                 CS_ESCOL_N, NM_MAE_PAC, ID_MN_RESI, SG_UF, ID_RG_RESI, NM_LOGRADO, NU_NUMERO, NM_BAIRRO, NU_CEP, 
                                                 CS_ZONA, DT_DIGITA, DT_INVEST, FEBRE, MIALGIA, CEFALEIA, EXANTEMA, VOMITO, NAUSEA, DOR_COSTAS, 
                                                 CONJUNTVIT, ARTRITE, ARTRALGIA, PETEQUIA_N, LEUCOPENIA, LACO, DOR_RETRO, DIABETES, HEMATOLOG, 
                                                 HEPATOPAT, RENAL, HIPERTENSA, ACIDO_PEPT, AUTO_IMUNE, DT_SORO, RESUL_SORO, DT_PCR, RESUL_PCR_, 
                                                 SOROTIPO, CLASSI_FIN, CRITERIO, TPAUTOCTO, COUFINF, COMUNINF, CO_BAINF, EVOLUCAO, HOSPITALIZ, 
                                                 DT_INTERNA, DT_OBITO, DT_ENCERRA, DT_ALRM, ALRM_LETAR, ALRM_HEPAT, ALRM_LIQ, ALRM_HIPOT, ALRM_PLAQ, 
                                                 ALRM_VOM, ALRM_SANG, ALRM_HEMAT, ALRM_ABDOM, DT_GRAV, GRAV_PULSO, GRAV_CONV, GRAV_ENCH, GRAV_INSUF, 
                                                 GRAV_TAQUI, GRAV_EXTRE, GRAV_HIPOT, GRAV_HEMAT, GRAV_MELEN, GRAV_METRO, GRAV_SANG, GRAV_AST, 
                                                 GRAV_MIOC, GRAV_CONSC, GRAV_ORGAO, MANI_HEMOR, EPISTAXE, GENGIVO, METRO, DS_OBS)

RS_Serie_Historica_Base <- read.csv(file = paste0("Base_de_Dados/Tabulacoes_R/Arboviroses/RS", RS, "_Serie_Historica_Base.csv"),
                                      header = TRUE,
                                      sep = ",")

RS_CE_Notificados_Base <- read.csv(file = paste0("Base_de_Dados/Tabulacoes_R/Arboviroses/RS", RS, "_CE_Notificados_Base.csv"),
                                     header = TRUE,
                                     sep = ",")

RS_CE_Confirmados_Base <- read.csv(file = paste0("Base_de_Dados/Tabulacoes_R/Arboviroses/RS", RS, "_CE_Confirmados_Base.csv"),
                                     header = TRUE,
                                     sep = ",")

RS_CE_Notificados_SEDE_Base <- read.csv(file = paste0("Base_de_Dados/Tabulacoes_R/Arboviroses/RS", RS, "_CE_Notificados_Sede_Base.csv"),
                                              header = TRUE,
                                              sep = ",")

RS_CE_Confirmados_SEDE_Base <- read.csv(file = paste0("Base_de_Dados/Tabulacoes_R/Arboviroses/RS", RS, "_CE_Confirmados_Sede_BASE.csv"),
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

assign("RS_22_23_SE_Notificados", AUX)

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

assign("RS_22_23_SE_Confirmados", AUX)

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

assign("RS_22_23_GERAL", AUX)

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

RS_Serie_Historica_Base[1, 15] <- sum(RS_22_23_GERAL$Notificados)
RS_Serie_Historica_Base[2, 15] <- sum(RS_22_23_GERAL$Dengue)
RS_Serie_Historica_Base[3, 15] <- sum(RS_22_23_GERAL$D_S_A)
RS_Serie_Historica_Base[4, 15] <- sum(RS_22_23_GERAL$Dengue_Grave)
RS_Serie_Historica_Base[5, 15] <- sum(RS_22_23_GERAL$Hospitalizacao)
RS_Serie_Historica_Base[6, 15] <- sum(RS_22_23_GERAL$Autoctones)
RS_Serie_Historica_Base[7, 15] <- sum(RS_22_23_GERAL$DENV_I)
RS_Serie_Historica_Base[8, 15] <- sum(RS_22_23_GERAL$DENV_II)
RS_Serie_Historica_Base[9, 15] <- sum(RS_22_23_GERAL$DENV_III)
RS_Serie_Historica_Base[10, 15] <- sum(RS_22_23_GERAL$DENV_IV)
RS_Serie_Historica_Base[11, 15] <- sum(RS_22_23_GERAL$Obitos)


AUX <- as.data.frame(t(RS_Serie_Historica_Base))

colnames(AUX) <- AUX[1,]

AUX <- AUX[-1,]

AUX[,12] <- c("2009/10", "2010/11", "2011/12", "2012/13", "2013/14", 
              "2014/15", "2015/16", "2016/17", "2017/18", "2018/19", "2019/20", 
              "2020/21", "2021/22", "2022/23")

colnames(AUX)[12] <- "Periodo"

AUX <- AUX[,c(12, 1:11)]

rownames(AUX) <- c(1:14)

RS_Serie_Historica <- AUX

RS_Serie_Historica[,2] <- as.numeric(RS_Serie_Historica[,2])
RS_Serie_Historica[,3] <- as.numeric(RS_Serie_Historica[,3])
RS_Serie_Historica[,4] <- as.numeric(RS_Serie_Historica[,4])
RS_Serie_Historica[,5] <- as.numeric(RS_Serie_Historica[,5])
RS_Serie_Historica[,6] <- as.numeric(RS_Serie_Historica[,6])
RS_Serie_Historica[,7] <- as.numeric(RS_Serie_Historica[,7])
RS_Serie_Historica[,8] <- as.numeric(RS_Serie_Historica[,8])
RS_Serie_Historica[,9] <- as.numeric(RS_Serie_Historica[,9])
RS_Serie_Historica[,10] <- as.numeric(RS_Serie_Historica[,10])
RS_Serie_Historica[,11] <- as.numeric(RS_Serie_Historica[,11])
RS_Serie_Historica[,12] <- as.numeric(RS_Serie_Historica[,12])

assign(paste0("RS", RS, "_Serie_Historica"), RS_Serie_Historica) 

rm(AUX, RS_Serie_Historica_Base, RS_22_23_GERAL, RS_Serie_Historica)

####################################################################################################################
################Trabalhando as tabelas base dos Canais Endêmicos####################################################
####################################################################################################################

######     Canal Endêmico    NOTIFICADOS#####

RS_CE_Notificados_Base[(nrow(RS_CE_Notificados_Base) +1), 1] <- "2022/23"
RS_CE_Notificados_Base[nrow(RS_CE_Notificados_Base), 2:54] <- as.integer(data.frame(RS22_22_23_SE_Notificados[nrow(RS22_22_23_SE_Notificados), 2:54]))

#####################################################################################################################
#####                   Utilizando objetos auxiliares porque se transpor o data frame                   #############
#####                   direto ele transforma as variáveis em caracter.                                 #############
#####                                                                                                   #############         
#####                     NÃO FOI DESCARTADO AINDA OS PERÍODOS EPIDÊMICOS                               #############
#####               VERIFICAR SE PODE-SE UTILIZAR A MÉDIA COMO LIMITE INFERIOR.                         #############
#####################################################################################################################

AUX <- RS_CE_Notificados_Base[, -1]

AUX <- t(AUX)

AUX2 <- RS_CE_Notificados_Base[, 1]

colnames(AUX) <- AUX2

RS_CE_Notificados <- AUX

######        Criando a coluna de média no data.frame            #####################

AUX <- apply(RS_CE_Notificados[, 1: (ncol(RS_CE_Notificados)-1)], 1 , mean)

RS_CE_Notificados <- as.data.frame(RS_CE_Notificados)

RS_CE_Notificados$Media <- AUX

######              Criando a coluna de Desvio Padrão no data frame                ###############

AUX <- apply(RS_CE_Notificados[, 1: (ncol(RS_CE_Notificados) -2)], 1 , sd)

RS_CE_Notificados$Desvio_Padrao <- AUX

######       Criando a coluna de Média + 2(DP)    ######################

AUX <- RS_CE_Notificados[, (ncol(RS_CE_Notificados)-1):ncol(RS_CE_Notificados)]

AUX <- AUX %>% mutate(Lim_Superior = (Media + 1.96 * Desvio_Padrao))

RS_CE_Notificados$Lim_Superior <- AUX$Lim_Superior

RS_CE_Notificados[, (ncol(RS_CE_Notificados)+1)] <- rownames(RS_CE_Notificados)

RS_CE_Notificados <- RS_CE_Notificados[, c(ncol(RS_CE_Notificados), 1:(ncol(RS_CE_Notificados) -1))]

RS_CE_Notificados[, 1] <- c(31:53, 1:30)

colnames(RS_CE_Notificados)[1] <- "Semana_Epidemiológica"

rownames(RS_CE_Notificados) <- c(1: nrow(RS_CE_Notificados))

rm(AUX, AUX2, RS_CE_Notificados_Base)

write.csv (RS_CE_Notificados, 
           paste0("Base_de_Dados/Tabulacoes_R/Arboviroses/RS", RS, "_CE_Notificados.csv"), 
           row.names = FALSE)

###    CANAL ENDÊMICO NOTIFICADOS     ####

###    Puxando os dados da tabela RS22_CE_Notificados e excluindo os períodos epidêmicos:   ######
###                             2015/16, 2019/20 e 2021/22                                  ######

AUX_GRAF <- RS_CE_Notificados[, Periodos_Epidêmicos_RS]

###      Usando apply para tirar a média por semana epidemiológica      ####

AUX_GRAF$Media <- apply(AUX_GRAF[, -ncol(AUX_GRAF)], 1 , mean)

###       Usando apply para tirar o desvio padrão por semana epidemiológica      #####

AUX_GRAF$Desvio_Padrao <- apply(AUX_GRAF[, 1:(ncol(AUX_GRAF)-2)], 1 , sd)

######      Criando a coluna de Média + 2(DP)    ###############

AUX_GRAF <- AUX_GRAF[, c((ncol(AUX_GRAF) -1): ncol(AUX_GRAF))]

AUX_GRAF$Lim_Superior <- NA

AUX_GRAF <- AUX_GRAF %>% mutate(Lim_Superior = (Media + 1.96 * Desvio_Padrao))

###Criando uma coluna de ordem das se para o R não colocar em ordem numérica.

AUX_GRAF$Ordem <- c(1: nrow(RS_CE_Notificados))

###Puxando o período sazonal atual para o gráfico de linhas

AUX_GRAF$`2022/23` <- RS_CE_Notificados$`2022/23`

AUX_GRAF$Sem_Epidemiológica <- RS_CE_Notificados$Semana_Epidemiológica

AUX_GRAF$Sem_EPI <-as.character(c("2022/31",  "2022/32", "2022/33", 
                                  "2022/34",  "2022/35",  "2022/36",  
                                  "2022/37",  "2022/38",  "2022/39",  
                                  "2022/40",  "2022/41",  "2022/42",  
                                  "2022/43",  "2022/44",  "2022/45",  
                                  "2022/46",  "2022/47",  "2022/48",  
                                  "2022/49",  "2022/50",  "2022/51",  
                                  "2022/52",  "2022/53",  "2023/01",  
                                  "2023/02",  "2023/03",  "2023/04",  
                                  "2023/05",  "2023/06",  "2023/07",  
                                  "2023/08",  "2023/09",  "2023/10", 
                                  "2023/11",  "2023/12",  "2023/13",  
                                  "2023/14",  "2023/15",  "2023/16",  
                                  "2023/17",  "2023/18",  "2023/19",  
                                  "2023/20",  "2023/21",  "2023/22",  
                                  "2023/23",  "2023/24",  "2023/25",  
                                  "2023/26",  "2023/27",  "2023/28",  
                                  "2023/29",  "2023/30")
)

RS_22_23_GRAF_CE_Notificados <- ggplot(AUX_GRAF, aes(Ordem))  +
  theme(axis.text.x = element_text(angle = 85, 
                                   vjust = .5,
                                   face = "bold")) +
  labs(caption = Fonte,
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
  geom_line(aes(,`2022/23`), stat = "identity", color = "black", linewidth = 1.5) +
  xlab("Semana Epidemiológica") +
  ylab("Número de Casos") +
  scale_x_continuous(breaks = c(1:53), label = AUX_GRAF$Sem_EPI) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))


####         Canal Endêmico CONFIRMADOS              ####

RS_CE_Confirmados_Base[(nrow(RS_CE_Confirmados_Base) +1), 1] <- "2022/23"
RS_CE_Confirmados_Base[nrow(RS_CE_Confirmados_Base), 2:54] <- as.integer(data.frame(RS22_22_23_SE_Confirmados[17, 2:54]))

#####################################################################################################################
#####                   Utilizando objetos auxiliares porque se transpor o data frame                   #############
#####                   direto ele transforma as variáveis em caracter.                                 #############
#####                                                                                                   #############         
#####                     NÃO FOI DESCARTADO AINDA OS PERÍODOS EPIDÊMICOS                               #############
#####               VERIFICAR SE PODE-SE UTILIZAR A MÉDIA COMO LIMITE INFERIOR.                         #############
#####################################################################################################################

AUX <- RS_CE_Confirmados_Base[, -1]

AUX <- t(AUX)

AUX2 <- RS_CE_Confirmados_Base[, 1]

colnames(AUX) <- AUX2

RS_CE_Confirmados <- AUX

######     Criando a coluna de média no data.frame     #####################

AUX <- apply(RS_CE_Confirmados[, 1: (ncol(RS_CE_Confirmados)-1)], 1 , mean)

RS_CE_Confirmados <- as.data.frame(RS_CE_Confirmados)

RS_CE_Confirmados$Media <- AUX

######     Criando a coluna de Desvio Padrão no data frame     ###############

AUX <- apply(RS_CE_Confirmados[, 1:(ncol(RS_CE_Confirmados)-2)], 1 , sd)

RS_CE_Confirmados$Desvio_Padrao <- AUX

######      Criando a coluna de Média + 2(DP)     ###########

AUX <- RS_CE_Confirmados[, (ncol(RS_CE_Confirmados)-1):ncol(RS_CE_Confirmados)]

AUX <- AUX %>% mutate(Lim_Superior = (Media + 1.96 * Desvio_Padrao))

RS_CE_Confirmados$Lim_Superior <- AUX$Lim_Superior

RS_CE_Confirmados[, (ncol(RS_CE_Confirmados)+1)] <- rownames(RS_CE_Confirmados)

RS_CE_Confirmados <- RS_CE_Confirmados[, c(ncol(RS_CE_Confirmados), 1:(ncol(RS_CE_Confirmados) -1))]

RS_CE_Confirmados[, 1] <- c(31:53, 1:30)

colnames(RS_CE_Confirmados)[1] <- "Semana_Epidemiológica"

rownames(RS_CE_Confirmados) <- c(1:nrow(RS_CE_Confirmados))

rm(AUX, AUX2, RS_CE_Confirmados_Base)

write.csv (RS_CE_Confirmados, 
           paste0("Base_de_Dados/Tabulacoes_R/Arboviroses/RS", RS, "_CE_Confirmados.csv"), 
           row.names = FALSE)

###     CANAL ENDÊMICO CONFIRMADOS      ####

###          Puxando os dados da tabela RS22_CE_Confirmados e excluindo os períodos epidêmicos: 
###                                    2015/16, 2019/20 e 2021/22

AUX_GRAF <- RS_CE_Confirmados[, Periodos_Epidêmicos_RS]

###                   Usando apply para tirar a média por semana epidemiológica

AUX_GRAF$Media <- apply(AUX_GRAF[,-ncol(AUX_GRAF)], 1 , mean)

###                Usando apply para tirar o desvio padrão por semana epidemiológica

AUX_GRAF$Desvio_Padrao <- apply(AUX_GRAF[, 1:(ncol(AUX_GRAF)-2)], 1 , sd)

######                   Criando a coluna de Média + 2(DP)

AUX_GRAF <- AUX_GRAF[, c((ncol(AUX_GRAF)-1):ncol(AUX_GRAF))]

AUX_GRAF$Lim_Superior <- NA

AUX_GRAF <- AUX_GRAF %>% mutate(Lim_Superior = (Media + 1.96 * Desvio_Padrao))

###                      Criando uma coluna de ordem das se para o R não colocar em ordem numérica.

AUX_GRAF$Ordem <- c(1: nrow(RS_CE_Confirmados))

###                        Puxando o período sazonal atual para o gráfico de linhas

AUX_GRAF$`2022/23` <- RS_CE_Confirmados$`2022/23`

AUX_GRAF$Sem_Epidemiológica <- RS_CE_Confirmados$Semana_Epidemiológica

AUX_GRAF$Sem_EPI <-as.character(c("2022/31",  "2022/32", "2022/33",  
                                  "2022/34",  "2022/35",  "2022/36", 
                                  "2022/37",  "2022/38",  "2022/39", 
                                  "2022/40",  "2022/41",  "2022/42", 
                                  "2022/43",  "2022/44",  "2022/45",  
                                  "2022/46",  "2022/47",  "2022/48", 
                                  "2022/49",  "2022/50",  "2022/51", 
                                  "2022/52",  "2022/53",  "2023/01", 
                                  "2023/02",  "2023/03",  "2023/04", 
                                  "2023/05",  "2023/06",  "2023/07", 
                                  "2023/08",  "2023/09",  "2023/10", 
                                  "2023/11",  "2023/12",  "2023/13", 
                                  "2023/14",  "2023/15",  "2023/16", 
                                  "2023/17",  "2023/18",  "2023/19", 
                                  "2023/20",  "2023/21",  "2023/22", 
                                  "2023/23",  "2023/24",  "2023/25", 
                                  "2023/26",  "2023/27",  "2023/28", 
                                  "2023/29",  "2023/30")
)

RS_22_23_GRAF_CE_Confirmados <- ggplot(AUX_GRAF, aes(Ordem))  +
  theme(axis.text.x = element_text(angle = 85, 
                                   vjust = .5,
                                   face = "bold")) +
  labs(caption = Fonte,
       title = "Canal Endêmico Casos CONFIRMADOS - 2022/23") +
  theme(
    panel.grid.major = element_line(color = "#C0C0C0"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#DC143C"),
    plot.title = element_text(face = "bold",
                              size = 19)
  ) +
  geom_area(aes(,Lim_Superior), fill = "#F0E68C",alpha = 0.9) +
  geom_area(aes(,Media), fill = "#556B2F") +
  geom_line(aes(,`2022/23`), stat = "identity", color = "black", linewidth = 1.5) +
  xlab("Semana Epidemiológica") +
  ylab("Número de Casos") +
  scale_x_continuous(breaks = c(1:53), label = AUX_GRAF$Sem_EPI) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

#####################################################################################################
###      Construindo um for loop para realizar a tabela de Prováveis por semana epidemiológica    ###
###          Será utilizado para os histogramas  e canais endêmicos de casos prováveis.           ###
#####################################################################################################

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
                                        -
                                          SINAN_DENGUE_RS %>%
                                          filter(ID_MN_RESI == i,
                                                 SEM_PRI == 202231,
                                                 CLASSI_FIN == 5) %>%
                                          count()
  )                                                                                       
  
  AUX[which(AUX == i), 3] <- as.integer(SINAN_DENGUE_RS %>%
                                          filter(ID_MN_RESI == i,
                                                 SEM_PRI ==202231)%>%
                                          count()
                                        -
                                          SINAN_DENGUE_RS %>%
                                          filter(ID_MN_RESI == i,
                                                 SEM_PRI == 202231,
                                                 CLASSI_FIN == 5) %>%
                                          count()
  )  
  
  AUX[which(AUX == i), 4] <- as.integer(SINAN_DENGUE_RS %>% 
                                          filter(ID_MN_RESI == i,
                                                 SEM_PRI ==202233) %>% 
                                          count()
                                        -
                                          SINAN_DENGUE_RS %>%
                                          filter(ID_MN_RESI == i,
                                                 SEM_PRI == 202233,
                                                 CLASSI_FIN == 5) %>%
                                          count()
  )   
  
  AUX[which(AUX == i),5] <- as.integer(SINAN_DENGUE_RS %>% 
                                         filter(ID_MN_RESI == i,
                                                SEM_PRI ==202234) %>% 
                                         count()
                                       -
                                         SINAN_DENGUE_RS %>%
                                         filter(ID_MN_RESI == i,
                                                SEM_PRI == 202234,
                                                CLASSI_FIN == 5) %>%
                                         count()
  )   
  
  AUX[which(AUX == i), 6] <- as.integer(SINAN_DENGUE_RS %>% 
                                          filter(ID_MN_RESI == i,
                                                 SEM_PRI ==202235) %>% 
                                          count()
                                        -
                                          SINAN_DENGUE_RS %>%
                                          filter(ID_MN_RESI == i,
                                                 SEM_PRI == 202235,
                                                 CLASSI_FIN == 5) %>%
                                          count()
  )   
  
  AUX[which(AUX == i), 7] <- as.integer(SINAN_DENGUE_RS %>%
                                          filter(ID_MN_RESI == i, 
                                                 SEM_PRI ==202236) %>%
                                          count()
                                        -
                                          SINAN_DENGUE_RS %>%
                                          filter(ID_MN_RESI == i,
                                                 SEM_PRI == 202236,
                                                 CLASSI_FIN == 5) %>%
                                          count()
  ) 
  
  AUX[which(AUX == i), 8] <- as.integer(SINAN_DENGUE_RS %>%
                                          filter(ID_MN_RESI == i, 
                                                 SEM_PRI ==202237) %>% 
                                          count() 
                                        -
                                          SINAN_DENGUE_RS %>%
                                          filter(ID_MN_RESI == i,
                                                 SEM_PRI == 202237,
                                                 CLASSI_FIN == 5) %>%
                                          count()
  )   
  
  AUX[which(AUX == i), 9] <- as.integer(SINAN_DENGUE_RS %>%
                                          filter(ID_MN_RESI == i, 
                                                 SEM_PRI ==202238) %>% 
                                          count() 
                                        -
                                          SINAN_DENGUE_RS %>%
                                          filter(ID_MN_RESI == i,
                                                 SEM_PRI == 202238,
                                                 CLASSI_FIN == 5) %>%
                                          count()
  )   
  
  AUX[which(AUX == i), 10] <- as.integer(SINAN_DENGUE_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202239) %>%
                                           count()
                                         -
                                           SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202239,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  )   
  
  AUX[which(AUX == i), 11] <- as.integer(SINAN_DENGUE_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202240) %>%
                                           count()
                                         -
                                           SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202240,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  )   
  
  AUX[which(AUX == i), 12] <- as.integer(SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202241) %>%
                                           count()
                                         -
                                           SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202241,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  )   
  
  AUX[which(AUX == i), 13] <- as.integer(SINAN_DENGUE_RS %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202242) %>%
                                           count() 
                                         -
                                           SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202242,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  )  
  
  AUX[which(AUX == i), 14] <- as.integer(SINAN_DENGUE_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202243) %>% 
                                           count() 
                                         -
                                           SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202243,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  )   
  
  
  AUX[which(AUX == i), 15] <- as.integer(SINAN_DENGUE_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202244) %>%
                                           count() 
                                         -
                                           SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202244,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  )   
  
  AUX[which(AUX == i), 16] <- as.integer(SINAN_DENGUE_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202245) %>%
                                           count() 
                                         -
                                           SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202245,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  )  
  
  AUX[which(AUX == i), 17] <- as.integer(SINAN_DENGUE_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202246) %>%
                                           count()
                                         -
                                           SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202246,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  )   
  
  AUX[which(AUX == i), 18] <- as.integer(SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202247) %>%  
                                           count() 
                                         -
                                           SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202247,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  ) 
  
  AUX[which(AUX == i), 19] <- as.integer(SINAN_DENGUE_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202248) %>%   
                                           count() 
                                         -
                                           SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202248,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  )   
  
  
  AUX[which(AUX == i), 20] <- as.integer(SINAN_DENGUE_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202249) %>%
                                           count() 
                                         -
                                           SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202249,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  )   
  
  AUX[which(AUX == i),  21] <- as.integer(SINAN_DENGUE_RS %>% 
                                            filter(ID_MN_RESI == i, 
                                                   SEM_PRI ==202250) %>%
                                            count() 
                                          -
                                            SINAN_DENGUE_RS %>%
                                            filter(ID_MN_RESI == i,
                                                   SEM_PRI == 202250,
                                                   CLASSI_FIN == 5) %>%
                                            count()
  )   
  
  AUX[which(AUX == i), 22] <- as.integer(SINAN_DENGUE_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202251) %>%
                                           count() 
                                         -
                                           SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202251,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  )   
  
  AUX[which(AUX == i), 23] <- as.integer(SINAN_DENGUE_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202252) %>%
                                           count() 
                                         -
                                           SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202252,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  )   
  
  AUX[which(AUX == i), 24] <- as.integer(SINAN_DENGUE_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202253) %>%
                                           count()
                                         -
                                           SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202253,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  )   
  
  AUX[which(AUX == i), 25] <- as.integer(SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202301) %>%
                                           count() 
                                         -
                                           SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202301,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  )   
  
  AUX[which(AUX == i), 26] <- as.integer(SINAN_DENGUE_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202302) %>%
                                           count()
                                         -
                                           SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202302,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  )   
  
  AUX[which(AUX == i), 27] <- as.integer(SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202303) %>%
                                           count() 
                                         -
                                           SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202303,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  )   
  
  AUX[which(AUX == i), 28] <- as.integer(SINAN_DENGUE_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202304) %>%
                                           count() 
                                         -
                                           SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202304,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  )   
  
  AUX[which(AUX == i), 29] <- as.integer(SINAN_DENGUE_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202305) %>%
                                           count() 
                                         -
                                           SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202305,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  )   
  
  AUX[which(AUX == i), 30] <- as.integer(SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202306) %>%
                                           count() 
                                         -
                                           SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202306,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  )   
  
  AUX[which(AUX == i), 31] <- as.integer(SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202307) %>% 
                                           count()
                                         -
                                           SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202307,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  ) 
  
  AUX[which(AUX == i), 32] <- as.integer(SINAN_DENGUE_RS %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202308) %>%
                                           count()
                                         -
                                           SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202308,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  ) 
  
  AUX[which(AUX == i), 33] <- as.integer(SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202309) %>%
                                           count() 
                                         -
                                           SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202309,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  ) 
  
  AUX[which(AUX == i), 34] <- as.integer(SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202310) %>% 
                                           count() 
                                         -
                                           SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202310,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  ) 
  
  AUX[which(AUX == i), 35] <- as.integer(SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202311) %>% 
                                           count() 
                                         -
                                           SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202311,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  ) 
  
  AUX[which(AUX == i), 36] <- as.integer(SINAN_DENGUE_RS %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202312) %>%
                                           count() 
                                         -
                                           SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202312,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  ) 
  
  AUX[which(AUX == i), 37] <- as.integer(SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202313) %>%
                                           count() 
                                         -
                                           SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202313,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 38] <- as.integer(SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202314) %>% 
                                           count() 
                                         -
                                           SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202314,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 39] <- as.integer(SINAN_DENGUE_RS %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202315) %>%
                                           count()
                                         -
                                           SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202315,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 40] <- as.integer(SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202316) %>%
                                           count() 
                                         -
                                           SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202316,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 41] <- as.integer(SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202317) %>%
                                           count() 
                                         -
                                           SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202317,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 42] <- as.integer(SINAN_DENGUE_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202318) %>%
                                           count() 
                                         -
                                           SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202318,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  ) 
  
  AUX[which(AUX == i), 43] <- as.integer(SINAN_DENGUE_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202319) %>%
                                           count()
                                         -
                                           SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202319,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 44] <- as.integer(SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202320) %>%
                                           count()
                                         -
                                           SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202320,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 45] <- as.integer(SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202321) %>%
                                           count() 
                                         -
                                           SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202321,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 46] <- as.integer(SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202322) %>%
                                           count()
                                         -
                                           SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202322,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 47] <- as.integer(SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202323) %>%
                                           count() 
                                         -
                                           SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202323,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 48] <- as.integer(SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202324) %>%
                                           count() 
                                         -
                                           SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202324,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 49] <- as.integer(SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202325) %>%
                                           count() 
                                         -
                                           SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202325,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 50] <- as.integer(SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202326) %>% 
                                           count()
                                         -
                                           SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202326,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  ) 
  
  AUX[which(AUX == i), 51] <- as.integer(SINAN_DENGUE_RS %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202327) %>% 
                                           count() 
                                         -
                                           SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202327,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 52] <- as.integer(SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202328) %>%
                                           count() 
                                         -
                                           SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202328,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 53] <- as.integer(SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202329) %>%
                                           count() 
                                         -
                                           SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202329,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  ) 
  
  AUX[which(AUX == i), 54] <- as.integer(SINAN_DENGUE_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202330) %>%
                                           count() 
                                         -
                                           SINAN_DENGUE_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202330,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  ) 
}

AUX[, 1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX[(nrow(AUX)+ 1), 2:54] <- apply(AUX[, 2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

####Casos prováveis por semana epidemiológica. Este objeto será apagado assim que for incluso no AUX_GRAF####

assign(paste0("RS", "_22_23_SE_Provaveis"), AUX[nrow(AUX), 2:54])

assign(paste0("RS", RS, "_22_23_SE_Provaveis"), AUX)

write.csv (assign(paste0("RS", RS, "_22_23_SE_Provaveis"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Arboviroses/RS", RS, "_22_23_SE_Descartados.csv"), 
           row.names = FALSE)

###     CANAL ENDÊMICO Prováveis         ####

####         Puxando os dados da tabela RS22_CE_Notificados e excluindo os períodos epidêmicos:
####                                2015/16, 2019/20 e 2021/22

AUX_GRAF <- RS_CE_Confirmados[, Periodos_Epidêmicos_RS]

###             Usando apply para tirar a média por semana epidemiológica

AUX_GRAF$Media <- apply(AUX_GRAF[, -ncol(AUX_GRAF)], 1 , mean)

###             Usando apply para tirar o desvio padrão por semana epidemiológica

AUX_GRAF$Desvio_Padrao <- apply(AUX_GRAF[, 1:(ncol(AUX_GRAF)-2)], 1 , sd)

######     Criando a coluna de Média + 2(DP)

AUX_GRAF <- AUX_GRAF[, c((ncol(AUX_GRAF) -1): ncol(AUX_GRAF))]

AUX_GRAF$Lim_Superior <- NA

AUX_GRAF <- AUX_GRAF %>% mutate(Lim_Superior = (Media + 1.96 * Desvio_Padrao))

###Criando uma coluna de ordem das se para o R não colocar em ordem numérica.
AUX_GRAF$Ordem <- c(1: nrow(RS_CE_Confirmados))

###Puxando o período sazonal atual para o gráfico de linhas

AUX_GRAF$`2022/23` <- RS_CE_Notificados$`2022/23`

AUX_GRAF$Sem_Epidemiológica <- RS_CE_Confirmados$Semana_Epidemiológica

AUX_GRAF$Sem_EPI <-as.character(c("2022/31",  "2022/32", "2022/33",  
                                  "2022/34",  "2022/35",  "2022/36", 
                                  "2022/37",  "2022/38",  "2022/39", 
                                  "2022/40",  "2022/41",  "2022/42", 
                                  "2022/43",  "2022/44",  "2022/45",  
                                  "2022/46",  "2022/47",  "2022/48",  
                                  "2022/49",  "2022/50",  "2022/51",  
                                  "2022/52",  "2022/53",  "2023/01", 
                                  "2023/02",  "2023/03",  "2023/04",  
                                  "2023/05",  "2023/06",  "2023/07",  
                                  "2023/08",  "2023/09",  "2023/10", 
                                  "2023/11",  "2023/12",  "2023/13",  
                                  "2023/14",  "2023/15",  "2023/16",  
                                  "2023/17",  "2023/18",  "2023/19", 
                                  "2023/20",  "2023/21",  "2023/22", 
                                  "2023/23",  "2023/24",  "2023/25",  
                                  "2023/26",  "2023/27",  "2023/28",  
                                  "2023/29",  "2023/30")
)

AUX_GRAF[, 8] <- t(RS_22_23_SE_Provaveis)

colnames(AUX_GRAF)[8] <- "Provaveis"

RS_22_23_GRAF_CE_Provaveis <- ggplot(AUX_GRAF, aes(Ordem))  +
  theme(axis.text.x = element_text(angle = 85, 
                                   vjust = .5,
                                   face = "bold")) +
  labs(caption = Fonte,
       title = "Canal Endêmico Casos PROVÁVEIS - 2022/23") +
  theme(
    panel.grid.major = element_line(color = "#C0C0C0"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#DC143C"),
    plot.title = element_text(face = "bold",
                              size = 19)
  ) +
  geom_area(aes(,Lim_Superior), fill = "#F0E68C",alpha = 0.9) +
  geom_area(aes(,Media), fill = "#556B2F") +
  geom_line(aes(,Provaveis), stat = "identity", color = "black", linewidth = 1.5) +
  xlab("Semana Epidemiológica") +
  ylab("Número de Casos") +
  scale_x_continuous(breaks = c(1:53), label = AUX_GRAF$Sem_EPI) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

rm(RS_22_23_SE_Provaveis)

#######################################################
######     Histogramas Municipais   ###################
#######################################################

###       NOTIFICADOS     ########

AUX_GRAF <- as.data.frame(RS_22_23_SE_Notificados$Município)

AUX_GRAF[, 2] <- as.data.frame(RS_22_23_SE_Notificados[, which(colnames(RS_22_23_SE_Notificados) == SE) - 9])
AUX_GRAF[, 3] <- as.data.frame(RS_22_23_SE_Notificados[, which(colnames(RS_22_23_SE_Notificados) == SE) - 8])
AUX_GRAF[, 4] <- as.data.frame(RS_22_23_SE_Notificados[, which(colnames(RS_22_23_SE_Notificados) == SE) - 7])
AUX_GRAF[, 5] <- as.data.frame(RS_22_23_SE_Notificados[, which(colnames(RS_22_23_SE_Notificados) == SE) - 6])
AUX_GRAF[, 6] <- as.data.frame(RS_22_23_SE_Notificados[, which(colnames(RS_22_23_SE_Notificados) == SE) - 5])
AUX_GRAF[, 7] <- as.data.frame(RS_22_23_SE_Notificados[, which(colnames(RS_22_23_SE_Notificados) == SE) - 4])
AUX_GRAF[, 8] <- as.data.frame(RS_22_23_SE_Notificados[, which(colnames(RS_22_23_SE_Notificados) == SE) - 3])
AUX_GRAF[, 9] <- as.data.frame(RS_22_23_SE_Notificados[, which(colnames(RS_22_23_SE_Notificados) == SE) - 2])
AUX_GRAF[, 10] <- as.data.frame(RS_22_23_SE_Notificados[, which(colnames(RS_22_23_SE_Notificados) == SE) - 1])
AUX_GRAF[, 11] <- as.data.frame(RS_22_23_SE_Notificados[, which(colnames(RS_22_23_SE_Notificados) == SE)])

colnames(AUX_GRAF)[1] <- "Municipios"
colnames(AUX_GRAF)[2] <- colnames(RS_22_23_SE_Notificados)[which(colnames(RS_22_23_SE_Notificados) == SE) - 9]
colnames(AUX_GRAF)[3] <- colnames(RS_22_23_SE_Notificados)[which(colnames(RS_22_23_SE_Notificados) == SE) - 8]
colnames(AUX_GRAF)[4] <- colnames(RS_22_23_SE_Notificados)[which(colnames(RS_22_23_SE_Notificados) == SE) - 7]
colnames(AUX_GRAF)[5] <- colnames(RS_22_23_SE_Notificados)[which(colnames(RS_22_23_SE_Notificados) == SE) - 6]
colnames(AUX_GRAF)[6] <- colnames(RS_22_23_SE_Notificados)[which(colnames(RS_22_23_SE_Notificados) == SE) - 5]
colnames(AUX_GRAF)[7] <- colnames(RS_22_23_SE_Notificados)[which(colnames(RS_22_23_SE_Notificados) == SE) - 4]
colnames(AUX_GRAF)[8] <- colnames(RS_22_23_SE_Notificados)[which(colnames(RS_22_23_SE_Notificados) == SE) - 3]
colnames(AUX_GRAF)[9] <- colnames(RS_22_23_SE_Notificados)[which(colnames(RS_22_23_SE_Notificados) == SE) - 2]
colnames(AUX_GRAF)[10] <- colnames(RS_22_23_SE_Notificados)[which(colnames(RS_22_23_SE_Notificados) == SE) - 1]
colnames(AUX_GRAF)[11] <- colnames(RS_22_23_SE_Notificados)[which(colnames(RS_22_23_SE_Notificados) == SE)]

AUX_GRAF[nrow(AUX_GRAF),] <- colnames(AUX_GRAF)

AUX_GRAF <- AUX_GRAF[c(17, 1:16),]

AUX_GRAF <- t(AUX_GRAF)

colnames(AUX_GRAF) <- AUX_GRAF[1,]

AUX_GRAF <- AUX_GRAF[-1,]

colnames(AUX_GRAF)[1] <- "SE"

AUX_GRAF <- as.data.frame(AUX_GRAF)

###############  Realizando um for loop para construção dos gráficos   ######

for(i in colnames(AUX_GRAF[-1])){

AUX <- ggplot (AUX_GRAF, 
                              aes(x = reorder(SE, as.numeric(SE)),
                                  y = as.numeric(AUX_GRAF[, i]))) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = paste0(i, " - Notificados")) +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           position = "dodge",
           color = "black",
           fill = "#8FBC8F") + 
  geom_label(aes(label = AUX_GRAF[, i]),
             alpha = 0.5,
             vjust = 0.1) +
  scale_y_continuous(expand = expansion(mult = c(0, 1)))

assign(paste0("SE_HIST_NOT_", i), AUX)}

plot (`

SE_HIST_NOT_ARIRANHA <- ggplot (AUX_GRAF, 
                                aes(x = reorder(SE, as.numeric(SE)), 
                                    y = Ariranha_do_Ivaí)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "ARIRANHA DO IVAÍ - Notificados") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#8FBC8F") + 
  geom_label(aes(label = Ariranha_do_Ivaí),
             alpha = 0.5,
             nudge_y = 0.01) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

SE_HIST_NOT_CANDIDO <- ggplot (AUX_GRAF, 
                               aes(x = reorder(SE, as.numeric(SE)), 
                                   y = Cândido_de_Abreu)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "CÂNDIDO DE ABREU - Notificados") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#8FBC8F") + 
  geom_label(aes(label = Cândido_de_Abreu),
             alpha = 0.5,
             nudge_y = 0.01) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

SE_HIST_NOT_CRUZMALTINA <- ggplot (AUX_GRAF, 
                                   aes(x = reorder(SE, as.numeric(SE)),
                                       y = Cruzmaltina)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "CRUZMALTINA - Notificados") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#8FBC8F") + 
  geom_label(aes(label = Cruzmaltina),
             alpha = 0.5,
             nudge_y = 0.01) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

SE_HIST_NOT_GODOY <- ggplot (AUX_GRAF, 
                             aes(x = reorder(SE, as.numeric(SE)), 
                                 y = Godoy_Moreira)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "GODOY MOREIRA - Notificados") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#8FBC8F") + 
  geom_label(aes(label = Godoy_Moreira),
             alpha = 0.5,
             nudge_y = 0.01) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

SE_HIST_NOT_IVAIPORA <- ggplot (AUX_GRAF, 
                                aes(x = reorder(SE, as.numeric(SE)),
                                    y = Ivaiporã)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "IVAIPORÃ - Notificados") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#8FBC8F") + 
  geom_label(aes(label = Ivaiporã),
             alpha = 0.5,
             nudge_y = 0.01) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

SE_HIST_NOT_JARDIM <- ggplot (AUX_GRAF, 
                              aes(x = reorder(SE, as.numeric(SE)), 
                                  y = Jardim_Alegre)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "JARDIM ALEGRE - Notificados") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#8FBC8F") + 
  geom_label(aes(label = Jardim_Alegre),
             alpha = 0.5,
             nudge_y = 0.01) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

SE_HIST_NOT_LIDIANÓPOLIS <- ggplot (AUX_GRAF, 
                                    aes(x = reorder(SE, as.numeric(SE)),
                                        y = Lidianópolis)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "LIDIANÓPOLIS - Notificados") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#8FBC8F") + 
  geom_label(aes(label = Lidianópolis),
             alpha = 0.5,
             nudge_y = 0.01) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

RS22_22_23_GRAF_Histograma_Notificados_01 <- (SE_HIST_NOT_ARAPUÃ + SE_HIST_NOT_ARIRANHA) / (SE_HIST_NOT_CANDIDO + SE_HIST_NOT_CRUZMALTINA) / (SE_HIST_NOT_GODOY + SE_HIST_NOT_IVAIPORA) / (SE_HIST_NOT_JARDIM + SE_HIST_NOT_LIDIANÓPOLIS) 

SE_HIST_NOT_LUNARDELLI <- ggplot (AUX_GRAF, 
                                  aes(x = reorder(SE, as.numeric(SE)), 
                                      y = Lunardelli)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "LUNARDELLI - Notificados") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#8FBC8F") + 
  geom_label(aes(label = Lunardelli),
             alpha = 0.5,
             nudge_y = 0.01) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

SE_HIST_NOT_MANOEL_RIBAS <- ggplot (AUX_GRAF, 
                                    aes(x = reorder(SE, as.numeric(SE)), 
                                        y = Manoel_Ribas)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "MANOEL RIBAS - Notificados") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#8FBC8F") + 
  geom_label(aes(label = Manoel_Ribas),
             alpha = 0.5,
             nudge_y = 0.01) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

SE_HIST_NOT_MATO_RICO <- ggplot (AUX_GRAF, 
                                 aes(x = reorder(SE, as.numeric(SE)), 
                                     y = Mato_Rico)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "MATO RICO - Notificados") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#8FBC8F") + 
  geom_label(aes(label = Mato_Rico),
             alpha = 0.5,
             nudge_y = 0.01) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

SE_HIST_NOT_NOVA_TEBAS <- ggplot (AUX_GRAF, 
                                  aes(x = reorder(SE, as.numeric(SE)),
                                      y = Nova_Tebas)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "NOVA TEBAS - Notificados") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#8FBC8F") + 
  geom_label(aes(label = Nova_Tebas),
             alpha = 0.5,
             nudge_y = 0.01) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

SE_HIST_NOT_RBI <- ggplot (AUX_GRAF, 
                           aes(x = reorder(SE, as.numeric(SE)), 
                               y = Rio_Branco_do_Ivaí)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "RIO BRANCO DO IVAÍ - Notificados") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#8FBC8F") + 
  geom_label(aes(label = Rio_Branco_do_Ivaí),
             alpha = 0.5,
             nudge_y = 0.01) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

SE_HIST_NOT_RSI <- ggplot (AUX_GRAF, 
                           aes(x = reorder(SE, as.numeric(SE)),
                               y = Rosário_do_Ivaí)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "ROSÁRIO DO IVAÍ - Notificados") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#8FBC8F") + 
  geom_label(aes(label = Rosário_do_Ivaí),
             alpha = 0.5,
             nudge_y = 0.01) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

SE_HIST_NOT_SMO <- ggplot (AUX_GRAF, 
                           aes(x = reorder(SE, as.numeric(SE)), 
                               y = Santa_Maria_do_Oeste)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "SANTA MARIA DO OESTE - Notificados") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#8FBC8F") + 
  geom_label(aes(label = Santa_Maria_do_Oeste),
             alpha = 0.5,
             nudge_y = 0.01) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

SE_HIST_NOT_SJI <- ggplot (AUX_GRAF, 
                           aes(x = reorder(SE, as.numeric(SE)), 
                               y = São_João_do_Ivaí)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "SÃO JOÃO DO IVAÍ - Notificados") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#8FBC8F") + 
  geom_label(aes(label = São_João_do_Ivaí),
             alpha = 0.5,
             nudge_y = 0.01) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

RS22_22_23_GRAF_Histograma_Notificados_02 <- (SE_HIST_NOT_LUNARDELLI + SE_HIST_NOT_MANOEL_RIBAS) / (SE_HIST_NOT_MATO_RICO + SE_HIST_NOT_NOVA_TEBAS) / (SE_HIST_NOT_RBI + SE_HIST_NOT_RSI) / (SE_HIST_NOT_SMO + SE_HIST_NOT_SJI) 

###     Confirmados    #####

AUX_GRAF <- as.data.frame(RS22_22_23_SE_Confirmados$Município)

AUX_GRAF[, 2] <- as.data.frame(RS22_22_23_SE_Confirmados[, which(colnames(RS22_22_23_SE_Confirmados) == SE) - 9])
AUX_GRAF[, 3] <- as.data.frame(RS22_22_23_SE_Confirmados[, which(colnames(RS22_22_23_SE_Confirmados) == SE) - 8])
AUX_GRAF[, 4] <- as.data.frame(RS22_22_23_SE_Confirmados[, which(colnames(RS22_22_23_SE_Confirmados) == SE) - 7])
AUX_GRAF[, 5] <- as.data.frame(RS22_22_23_SE_Confirmados[, which(colnames(RS22_22_23_SE_Confirmados) == SE) - 6])
AUX_GRAF[, 6] <- as.data.frame(RS22_22_23_SE_Confirmados[, which(colnames(RS22_22_23_SE_Confirmados) == SE) - 5])
AUX_GRAF[, 7] <- as.data.frame(RS22_22_23_SE_Confirmados[, which(colnames(RS22_22_23_SE_Confirmados) == SE) - 4])
AUX_GRAF[, 8] <- as.data.frame(RS22_22_23_SE_Confirmados[, which(colnames(RS22_22_23_SE_Confirmados) == SE) - 3])
AUX_GRAF[, 9] <- as.data.frame(RS22_22_23_SE_Confirmados[, which(colnames(RS22_22_23_SE_Confirmados) == SE) - 2])
AUX_GRAF[, 10] <- as.data.frame(RS22_22_23_SE_Confirmados[, which(colnames(RS22_22_23_SE_Confirmados) == SE) - 1])
AUX_GRAF[, 11] <- as.data.frame(RS22_22_23_SE_Confirmados[, which(colnames(RS22_22_23_SE_Confirmados) == SE)])

colnames(AUX_GRAF)[1] <- "Municipios"
colnames(AUX_GRAF)[2] <- colnames(RS22_22_23_SE_Confirmados)[which(colnames(RS22_22_23_SE_Confirmados) == SE) - 9]
colnames(AUX_GRAF)[3] <- colnames(RS22_22_23_SE_Confirmados)[which(colnames(RS22_22_23_SE_Confirmados) == SE) - 8]
colnames(AUX_GRAF)[4] <- colnames(RS22_22_23_SE_Confirmados)[which(colnames(RS22_22_23_SE_Confirmados) == SE) - 7]
colnames(AUX_GRAF)[5] <- colnames(RS22_22_23_SE_Confirmados)[which(colnames(RS22_22_23_SE_Confirmados) == SE) - 6]
colnames(AUX_GRAF)[6] <- colnames(RS22_22_23_SE_Confirmados)[which(colnames(RS22_22_23_SE_Confirmados) == SE) - 5]
colnames(AUX_GRAF)[7] <- colnames(RS22_22_23_SE_Confirmados)[which(colnames(RS22_22_23_SE_Confirmados) == SE) - 4]
colnames(AUX_GRAF)[8] <- colnames(RS22_22_23_SE_Confirmados)[which(colnames(RS22_22_23_SE_Confirmados) == SE) - 3]
colnames(AUX_GRAF)[9] <- colnames(RS22_22_23_SE_Confirmados)[which(colnames(RS22_22_23_SE_Confirmados) == SE) - 2]
colnames(AUX_GRAF)[10] <- colnames(RS22_22_23_SE_Confirmados)[which(colnames(RS22_22_23_SE_Confirmados) == SE) - 1]
colnames(AUX_GRAF)[11] <- colnames(RS22_22_23_SE_Confirmados)[which(colnames(RS22_22_23_SE_Confirmados) == SE)]

AUX_GRAF[17,] <- colnames(AUX_GRAF)

AUX_GRAF <- AUX_GRAF[c(17, 1:16),]

AUX_GRAF <- t(AUX_GRAF)

colnames(AUX_GRAF) <- AUX_GRAF[1,]

AUX_GRAF <- AUX_GRAF[-1,]

colnames(AUX_GRAF)[1] <- "SE"

AUX_GRAF <- data.frame(SE = AUX_GRAF[, 1],
                       Arapuã = as.numeric(AUX_GRAF[, 2]),
                       Ariranha_do_Ivaí = as.numeric(AUX_GRAF[, 3]),
                       Cândido_de_Abreu = as.numeric(AUX_GRAF[, 4]),
                       Cruzmaltina = as.numeric(AUX_GRAF[, 5]),
                       Godoy_Moreira = as.numeric(AUX_GRAF[, 6]),
                       Ivaiporã = as.numeric(AUX_GRAF[, 7]),
                       Jardim_Alegre = as.numeric(AUX_GRAF[, 8]),
                       Lidianópolis = as.numeric(AUX_GRAF[, 9]),
                       Lunardelli = as.numeric(AUX_GRAF[, 10]),
                       Manoel_Ribas = as.numeric(AUX_GRAF[, 11]),
                       Mato_Rico = as.numeric(AUX_GRAF[, 12]),
                       Nova_Tebas = as.numeric(AUX_GRAF[, 13]),
                       Rio_Branco_do_Ivaí = as.numeric(AUX_GRAF[, 14]),
                       Rosário_do_Ivaí = as.numeric(AUX_GRAF[, 15]),
                       Santa_Maria_do_Oeste = as.numeric(AUX_GRAF[, 16]),
                       São_João_do_Ivaí = as.numeric(AUX_GRAF[, 17]))

SE_HIST_CONF_ARAPUÃ <- ggplot (AUX_GRAF, 
                               aes(x = reorder(SE, as.numeric(SE)),
                                   y = Arapuã)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "ARAPUÃ - Confirmados") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           position = "dodge",
           color = "black",
           fill = "#B0C4DE") + 
  geom_label(aes(label = Arapuã),
             alpha = 0.5,
             nudge_y = 0.01) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

SE_HIST_CONF_ARIRANHA <- ggplot (AUX_GRAF, 
                                 aes(x = reorder(SE, as.numeric(SE)), 
                                     y = Ariranha_do_Ivaí)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "ARIRANHA DO IVAÍ - Confirmados") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#B0C4DE") + 
  geom_label(aes(label = Ariranha_do_Ivaí),
             alpha = 0.5,
             nudge_y = 0.01) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

SE_HIST_CONF_CANDIDO <- ggplot (AUX_GRAF, 
                                aes(x = reorder(SE, as.numeric(SE)), 
                                    y = Cândido_de_Abreu)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "CÂNDIDO DE ABREU - Confirmados") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#B0C4DE") + 
  geom_label(aes(label = Cândido_de_Abreu),
             alpha = 0.5,
             nudge_y = 0.01) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

SE_HIST_CONF_CRUZMALTINA <- ggplot (AUX_GRAF, 
                                    aes(x = reorder(SE, as.numeric(SE)),
                                        y = Cruzmaltina)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "CRUZMALTINA - Confirmados") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#B0C4DE") + 
  geom_label(aes(label = Cruzmaltina),
             alpha = 0.5,
             nudge_y = 0.01) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

SE_HIST_CONF_GODOY <- ggplot (AUX_GRAF, 
                              aes(x = reorder(SE, as.numeric(SE)), 
                                  y = Godoy_Moreira)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "GODOY MOREIRA - Confirmados") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#B0C4DE") + 
  geom_label(aes(label = Godoy_Moreira),
             alpha = 0.5,
             nudge_y = 0.01) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

SE_HIST_CONF_IVAIPORA <- ggplot (AUX_GRAF, 
                                 aes(x = reorder(SE, as.numeric(SE)),
                                     y = Ivaiporã)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "IVAIPORÃ - Confirmados") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#B0C4DE") + 
  geom_label(aes(label = Ivaiporã),
             alpha = 0.5,
             nudge_y = 0.01) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

SE_HIST_CONF_JARDIM <- ggplot (AUX_GRAF, 
                               aes(x = reorder(SE, as.numeric(SE)), 
                                   y = Jardim_Alegre)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "JARDIM ALEGRE - Confirmados") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#B0C4DE") + 
  geom_label(aes(label = Jardim_Alegre),
             alpha = 0.5,
             nudge_y = 0.01) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

SE_HIST_CONF_LIDIANÓPOLIS <- ggplot (AUX_GRAF, 
                                     aes(x = reorder(SE, as.numeric(SE)),
                                         y = Lidianópolis)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "LIDIANÓPOLIS - Confirmados") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#B0C4DE") + 
  geom_label(aes(label = Lidianópolis),
             alpha = 0.5,
             nudge_y = 0.01) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

RS22_22_23_GRAF_Histograma_Confirmados_01 <- (SE_HIST_CONF_ARAPUÃ + SE_HIST_CONF_ARIRANHA) / (SE_HIST_CONF_CANDIDO + SE_HIST_CONF_CRUZMALTINA) / (SE_HIST_CONF_GODOY + SE_HIST_CONF_IVAIPORA) / (SE_HIST_CONF_JARDIM + SE_HIST_CONF_LIDIANÓPOLIS) 

SE_HIST_CONF_LUNARDELLI <- ggplot (AUX_GRAF, 
                                   aes(x = reorder(SE, as.numeric(SE)), 
                                       y = Lunardelli)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "LUNARDELLI - Confirmados") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#B0C4DE") + 
  geom_label(aes(label = Lunardelli),
             alpha = 0.5,
             nudge_y = 0.01) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

SE_HIST_CONF_MANOEL_RIBAS <- ggplot (AUX_GRAF, 
                                     aes(x = reorder(SE, as.numeric(SE)), 
                                         y = Manoel_Ribas)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "MANOEL RIBAS - Confirmados") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#B0C4DE") + 
  geom_label(aes(label = Manoel_Ribas),
             alpha = 0.5,
             nudge_y = 0.01) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

SE_HIST_CONF_MATO_RICO <- ggplot (AUX_GRAF, 
                                  aes(x = reorder(SE, as.numeric(SE)), 
                                      y = Mato_Rico)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "MATO RICO - Confirmados") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#B0C4DE") + 
  geom_label(aes(label = Mato_Rico),
             alpha = 0.5,
             nudge_y = 0.01) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

SE_HIST_CONF_NOVA_TEBAS <- ggplot (AUX_GRAF, 
                                   aes(x = reorder(SE, as.numeric(SE)),
                                       y = Nova_Tebas)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "NOVA TEBAS - Confirmados") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#B0C4DE") + 
  geom_label(aes(label = Nova_Tebas),
             alpha = 0.5,
             nudge_y = 0.01) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

SE_HIST_CONF_RBI <- ggplot (AUX_GRAF, 
                            aes(x = reorder(SE, as.numeric(SE)), 
                                y = Rio_Branco_do_Ivaí)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "RIO BRANCO DO IVAÍ - Confirmados") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#B0C4DE") + 
  geom_label(aes(label = Rio_Branco_do_Ivaí),
             alpha = 0.5,
             nudge_y = 0.01) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

SE_HIST_CONF_RSI <- ggplot (AUX_GRAF, 
                            aes(x = reorder(SE, as.numeric(SE)),
                                y = Rosário_do_Ivaí)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "ROSÁRIO DO IVAÍ - Confirmados") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#B0C4DE") + 
  geom_label(aes(label = Rosário_do_Ivaí),
             alpha = 0.5,
             nudge_y = 0.01) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

SE_HIST_CONF_SMO <- ggplot (AUX_GRAF, 
                            aes(x = reorder(SE, as.numeric(SE)), 
                                y = Santa_Maria_do_Oeste)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "SANTA MARIA DO OESTE - Confirmados") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#B0C4DE") + 
  geom_label(aes(label = Santa_Maria_do_Oeste),
             alpha = 0.5,
             nudge_y = 0.01) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

SE_HIST_CONF_SJI <- ggplot (AUX_GRAF, 
                            aes(x = reorder(SE, as.numeric(SE)), 
                                y = São_João_do_Ivaí)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "SÃO JOÃO DO IVAÍ - Confirmados") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#B0C4DE") + 
  geom_label(aes(label = São_João_do_Ivaí),
             alpha = 0.5,
             nudge_y = 0.01) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

RS22_22_23_GRAF_Histograma_Confirmados_02 <- (SE_HIST_CONF_LUNARDELLI + SE_HIST_CONF_MANOEL_RIBAS) / (SE_HIST_CONF_MATO_RICO + SE_HIST_CONF_NOVA_TEBAS) / (SE_HIST_CONF_RBI + SE_HIST_CONF_RSI) / (SE_HIST_CONF_SMO + SE_HIST_CONF_SJI) 

######Histogramas

###Provaveis

AUX_GRAF <- as.data.frame(RS22_22_23_SE_Provaveis$Município)

AUX_GRAF[, 2] <- as.data.frame(RS22_22_23_SE_Provaveis[, which(colnames(RS22_22_23_SE_Provaveis) == SE) - 9])
AUX_GRAF[, 3] <- as.data.frame(RS22_22_23_SE_Provaveis[, which(colnames(RS22_22_23_SE_Provaveis) == SE) - 8])
AUX_GRAF[, 4] <- as.data.frame(RS22_22_23_SE_Provaveis[, which(colnames(RS22_22_23_SE_Provaveis) == SE) - 7])
AUX_GRAF[, 5] <- as.data.frame(RS22_22_23_SE_Provaveis[, which(colnames(RS22_22_23_SE_Provaveis) == SE) - 6])
AUX_GRAF[, 6] <- as.data.frame(RS22_22_23_SE_Provaveis[, which(colnames(RS22_22_23_SE_Provaveis) == SE) - 5])
AUX_GRAF[, 7] <- as.data.frame(RS22_22_23_SE_Provaveis[, which(colnames(RS22_22_23_SE_Provaveis) == SE) - 4])
AUX_GRAF[, 8] <- as.data.frame(RS22_22_23_SE_Provaveis[, which(colnames(RS22_22_23_SE_Provaveis) == SE) - 3])
AUX_GRAF[, 9] <- as.data.frame(RS22_22_23_SE_Provaveis[, which(colnames(RS22_22_23_SE_Provaveis) == SE) - 2])
AUX_GRAF[, 10] <- as.data.frame(RS22_22_23_SE_Provaveis[, which(colnames(RS22_22_23_SE_Provaveis) == SE) - 1])
AUX_GRAF[, 11] <- as.data.frame(RS22_22_23_SE_Provaveis[, which(colnames(RS22_22_23_SE_Provaveis) == SE)])

colnames(AUX_GRAF)[1] <- "Municipios"
colnames(AUX_GRAF)[2] <- colnames(RS22_22_23_SE_Provaveis)[which(colnames(RS22_22_23_SE_Provaveis) == SE) - 9]
colnames(AUX_GRAF)[3] <- colnames(RS22_22_23_SE_Provaveis)[which(colnames(RS22_22_23_SE_Provaveis) == SE) - 8]
colnames(AUX_GRAF)[4] <- colnames(RS22_22_23_SE_Provaveis)[which(colnames(RS22_22_23_SE_Provaveis) == SE) - 7]
colnames(AUX_GRAF)[5] <- colnames(RS22_22_23_SE_Provaveis)[which(colnames(RS22_22_23_SE_Provaveis) == SE) - 6]
colnames(AUX_GRAF)[6] <- colnames(RS22_22_23_SE_Provaveis)[which(colnames(RS22_22_23_SE_Provaveis) == SE) - 5]
colnames(AUX_GRAF)[7] <- colnames(RS22_22_23_SE_Provaveis)[which(colnames(RS22_22_23_SE_Provaveis) == SE) - 4]
colnames(AUX_GRAF)[8] <- colnames(RS22_22_23_SE_Provaveis)[which(colnames(RS22_22_23_SE_Provaveis) == SE) - 3]
colnames(AUX_GRAF)[9] <- colnames(RS22_22_23_SE_Provaveis)[which(colnames(RS22_22_23_SE_Provaveis) == SE) - 2]
colnames(AUX_GRAF)[10] <- colnames(RS22_22_23_SE_Provaveis)[which(colnames(RS22_22_23_SE_Provaveis) == SE) - 1]
colnames(AUX_GRAF)[11] <- colnames(RS22_22_23_SE_Provaveis)[which(colnames(RS22_22_23_SE_Provaveis) == SE)]

AUX_GRAF[17,] <- colnames(AUX_GRAF)

AUX_GRAF <- AUX_GRAF[c(17, 1:16),]

AUX_GRAF <- t(AUX_GRAF)

colnames(AUX_GRAF) <- AUX_GRAF[1,]

AUX_GRAF <- AUX_GRAF[-1,]

colnames(AUX_GRAF)[1] <- "SE"

AUX_GRAF <- data.frame(SE = AUX_GRAF[, 1],
                       Arapuã = as.numeric(AUX_GRAF[, 2]),
                       Ariranha_do_Ivaí = as.numeric(AUX_GRAF[, 3]),
                       Cândido_de_Abreu = as.numeric(AUX_GRAF[, 4]),
                       Cruzmaltina = as.numeric(AUX_GRAF[, 5]),
                       Godoy_Moreira = as.numeric(AUX_GRAF[, 6]),
                       Ivaiporã = as.numeric(AUX_GRAF[, 7]),
                       Jardim_Alegre = as.numeric(AUX_GRAF[, 8]),
                       Lidianópolis = as.numeric(AUX_GRAF[, 9]),
                       Lunardelli = as.numeric(AUX_GRAF[, 10]),
                       Manoel_Ribas = as.numeric(AUX_GRAF[, 11]),
                       Mato_Rico = as.numeric(AUX_GRAF[, 12]),
                       Nova_Tebas = as.numeric(AUX_GRAF[, 13]),
                       Rio_Branco_do_Ivaí = as.numeric(AUX_GRAF[, 14]),
                       Rosário_do_Ivaí = as.numeric(AUX_GRAF[, 15]),
                       Santa_Maria_do_Oeste = as.numeric(AUX_GRAF[, 16]),
                       São_João_do_Ivaí = as.numeric(AUX_GRAF[, 17]))

SE_HIST_PROV_ARAPUÃ <- ggplot (AUX_GRAF, 
                               aes(x = reorder(SE, as.numeric(SE)),
                                   y = Arapuã)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica", 
       y = "Número de Casos",
       title = "ARAPUÃ - Prováveis") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           position = "dodge",
           color = "black",
           fill = "#708090") + 
  geom_label(aes(label = Arapuã),
             alpha = 0.5,
             nudge_y = 0.01) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

SE_HIST_PROV_ARIRANHA <- ggplot (AUX_GRAF, 
                                 aes(x = reorder(SE, as.numeric(SE)), 
                                     y = Ariranha_do_Ivaí)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "ARIRANHA DO IVAÍ - Prováveis") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#708090") + 
  geom_label(aes(label = Ariranha_do_Ivaí),
             alpha = 0.5,
             nudge_y = 0.01) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

SE_HIST_PROV_CANDIDO <- ggplot (AUX_GRAF, 
                                aes(x = reorder(SE, as.numeric(SE)), 
                                    y = Cândido_de_Abreu)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "CÂNDIDO DE ABREU - Prováveis") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#708090") + 
  geom_label(aes(label = Cândido_de_Abreu),
             alpha = 0.5,
             nudge_y = 1) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

SE_HIST_PROV_CRUZMALTINA <- ggplot (AUX_GRAF, 
                                    aes(x = reorder(SE, as.numeric(SE)),
                                        y = Cruzmaltina)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "CRUZMALTINA - Prováveis") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#708090") + 
  geom_label(aes(label = Cruzmaltina),
             alpha = 0.5,
             nudge_y = 0.01) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

SE_HIST_PROV_GODOY <- ggplot (AUX_GRAF, 
                              aes(x = reorder(SE, as.numeric(SE)), 
                                  y = Godoy_Moreira)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "GODOY MOREIRA - Prováveis") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#708090") + 
  geom_label(aes(label = Godoy_Moreira),
             alpha = 0.5,
             nudge_y = 0.01) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

SE_HIST_PROV_IVAIPORA <- ggplot (AUX_GRAF, 
                                 aes(x = reorder(SE, as.numeric(SE)),
                                     y = Ivaiporã)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "IVAIPORÃ - Prováveis") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#708090") + 
  geom_label(aes(label = Ivaiporã),
             alpha = 0.5,
             nudge_y = 0.01) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

SE_HIST_PROV_JARDIM <- ggplot (AUX_GRAF, 
                               aes(x = reorder(SE, as.numeric(SE)), 
                                   y = Jardim_Alegre)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "JARDIM ALEGRE - Prováveis") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#708090") + 
  geom_label(aes(label = Jardim_Alegre),
             alpha = 0.5,
             nudge_y = 0.01) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

SE_HIST_PROV_LIDIANÓPOLIS <- ggplot (AUX_GRAF, 
                                     aes(x = reorder(SE, as.numeric(SE)),
                                         y = Lidianópolis)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "LIDIANÓPOLIS - Prováveis") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#708090") + 
  geom_label(aes(label = Lidianópolis),
             alpha = 0.5,
             nudge_y = 0.01) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

RS22_22_23_GRAF_Histograma_Provaveis_01 <- (SE_HIST_PROV_ARAPUÃ + SE_HIST_PROV_ARIRANHA) / (SE_HIST_PROV_CANDIDO + SE_HIST_PROV_CRUZMALTINA) / (SE_HIST_PROV_GODOY + SE_HIST_PROV_IVAIPORA) / (SE_HIST_PROV_JARDIM + SE_HIST_PROV_LIDIANÓPOLIS) 

SE_HIST_PROV_LUNARDELLI <- ggplot (AUX_GRAF, 
                                   aes(x = reorder(SE, as.numeric(SE)), 
                                       y = Lunardelli)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "LUNARDELLI - Prováveis") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#708090") + 
  geom_label(aes(label = Lunardelli),
             alpha = 0.5,
             nudge_y = 0.01) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

SE_HIST_PROV_MANOEL_RIBAS <- ggplot (AUX_GRAF, 
                                     aes(x = reorder(SE, as.numeric(SE)), 
                                         y = Manoel_Ribas)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "MANOEL RIBAS - Prováveis") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#708090") + 
  geom_label(aes(label = Manoel_Ribas),
             alpha = 0.5,
             nudge_y = 0.01) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

SE_HIST_PROV_MATO_RICO <- ggplot (AUX_GRAF, 
                                  aes(x = reorder(SE, as.numeric(SE)), 
                                      y = Mato_Rico)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "MATO RICO - Prováveis") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#708090") + 
  geom_label(aes(label = Mato_Rico),
             alpha = 0.5,
             nudge_y = 0.01) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

SE_HIST_PROV_NOVA_TEBAS <- ggplot (AUX_GRAF, 
                                   aes(x = reorder(SE, as.numeric(SE)),
                                       y = Nova_Tebas)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "NOVA TEBAS - Prováveis") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#708090") + 
  geom_label(aes(label = Nova_Tebas),
             alpha = 0.5,
             nudge_y = 0.01) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

SE_HIST_PROV_RBI <- ggplot (AUX_GRAF, 
                            aes(x = reorder(SE, as.numeric(SE)), 
                                y = Rio_Branco_do_Ivaí)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "RIO BRANCO DO IVAÍ - Prováveis") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#708090") + 
  geom_label(aes(label = Rio_Branco_do_Ivaí),
             alpha = 0.5,
             nudge_y = 0.01) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

SE_HIST_PROV_RSI <- ggplot (AUX_GRAF, 
                            aes(x = reorder(SE, as.numeric(SE)),
                                y = Rosário_do_Ivaí)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "ROSÁRIO DO IVAÍ - Prováveis") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#708090") + 
  geom_label(aes(label = Rosário_do_Ivaí),
             alpha = 0.5,
             nudge_y = 0.01) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

SE_HIST_PROV_SMO <- ggplot (AUX_GRAF, 
                            aes(x = reorder(SE, as.numeric(SE)), 
                                y = Santa_Maria_do_Oeste)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "SANTA MARIA DO OESTE - Prováveis") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#708090") + 
  geom_label(aes(label = Santa_Maria_do_Oeste),
             alpha = 0.5,
             nudge_y = 0.01) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

SE_HIST_PROV_SJI <- ggplot (AUX_GRAF, 
                            aes(x = reorder(SE, as.numeric(SE)), 
                                y = São_João_do_Ivaí)) + 
  theme(axis.text.x = element_text(face = "bold")) +
  labs(caption = Fonte, 
       x = "Semana Epidemiológica",
       y = "Número de Casos",
       title = "SÃO JOÃO DO IVAÍ - Prováveis") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#708090") + 
  geom_label(aes(label = São_João_do_Ivaí),
             alpha = 0.5,
             nudge_y = 0.01) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

RS22_22_23_GRAF_Histograma_Provaveis_02 <- (SE_HIST_PROV_LUNARDELLI + SE_HIST_PROV_MANOEL_RIBAS) / (SE_HIST_PROV_MATO_RICO + SE_HIST_PROV_NOVA_TEBAS) / (SE_HIST_PROV_RBI + SE_HIST_PROV_RSI) / (SE_HIST_PROV_SMO + SE_HIST_PROV_SJI) 

rm(AUX_GRAF)


################################################################
################################################################
######      Gráficos para serem utilizados no Informe       ####
################################################################
################################################################

####     Construção de data frame auxiliar para             ###
####    ser utilizado pelo R na construção dos gráficos     ###

AUX_GRAF <- as.data.frame(RS_Serie_Historica$Periodo)

AUX_GRAF <- AUX_GRAF %>% mutate(Notificados = RS_Serie_Historica$Notificados)

AUX_GRAF <- AUX_GRAF %>% mutate(Confirmados = RS_Serie_Historica$Dengue 
                                + 
                                  RS_Serie_Historica$D.S.A. 
                                + 
                                  RS_Serie_Historica$Dengue_Grave)

colnames(AUX_GRAF) <- c("Periodo", "Notificados", "Confirmados")

#####################################################################################################################
####        Criando gráfico de barras lado a lado Série Histórica Notificados e Confirmados.    #####################
#####################################################################################################################

RS22_Serie_Historica_GRAF_Not_Conf <- ggplot (AUX_GRAF, 
                                              aes(x = Periodo)) + 
  theme(axis.text.x = element_text(angle = 50, 
                                   vjust = .5,
                                   face = "bold",
                                   size = 14)) +
  labs(caption = Fonte, 
       x = "Período Sazonal",
       y = "Número de Casos",
       title = "CASOS NOTIFICADOS/CONFIRMADOS 22ªRS (2009/10 - 2022/23)") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 24,
                                   colour = "#556B2F")) +
  geom_bar(
    aes( y = Notificados, fill = "Notificados"),
    stat = "identity",
    color = "black",
    width = .4,
    position = position_nudge(x = -.20)) + 
  geom_label(aes(y = Notificados,
                 label = Notificados),
             size = 3, 
             alpha = 0.5,
             nudge_x = -.20,
             nudge_y = 30) + 
  scale_fill_manual(name = "", values = c("Notificados" = "#556B2F", "Confirmados" = "#FF6347")) +
  theme(legend.position = "bottom") +
  geom_bar(
    aes( y = Confirmados, fill = "Confirmados"),
    stat = "identity",
    color = "black",
    width = .4,
    position = position_nudge(x = .20)) +
  geom_label(aes(y = Confirmados,
                 label = Confirmados),
             size = 3, 
             alpha = 0.5,
             nudge_x = .20,
             nudge_y = 30) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

###############################################################
#####     Série Histórica de casos Hospitalizados       #######
###############################################################

RS22_Serie_Historica_GRAF_Hospitalizados <- ggplot (RS_Serie_Historica, 
                                                    aes(x = Periodo, 
                                                        y = Hospitalizados)) + 
  theme(axis.text.x = element_text(angle = 50, 
                                   vjust = .5,
                                   face = "bold",
                                   size = 12)) +
  labs(caption = Fonte, 
       x = "Período Sazonal",
       y = "Número de Casos",
       title = "CASOS HOSPITALIZADOS 22ªRS (2009/10 - 2022/23)") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 14,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity", 
           fill = "#0F815D",
           color = "black") + 
  geom_label(aes(label = Hospitalizados),
             alpha = 0.5,
             size =3,
             nudge_y = 3) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

############################################################
#####        Série Histórica Sorotipo Circulante      ######
############################################################

RS22_Serie_Historica_GRAF_Sorotipo <- ggplot (RS_Serie_Historica, 
                                              aes(x = Periodo)) + 
  theme(axis.text.x = element_text(angle = 50, 
                                   vjust = .5,
                                   face = "bold",
                                   size = 12)) +
  labs(caption = Fonte, 
       x = "Período Sazonal",
       y = "Número de Casos",
       title = "SOROTIPO CIRCULANTE 22ªRS (2009/10 - 2022/23)") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 14,
                                   colour = "#556B2F")) +
  geom_bar(
    aes( y = DENV_I, fill = "DENV I"),
    stat = "identity",
    color = "black",
    width = .4,
    position = position_nudge(x = -.20)) + 
  geom_label(aes(y = DENV_I,
                 label = DENV_I),
             size = 3, 
             alpha = 0.5,
             nudge_x = -.20,
             nudge_y = .5) + 
  scale_fill_manual(name = "", 
                    values = c("DENV I" = "#3CB371", "DENV II" = "#2F657E")) +
  theme(legend.position = "bottom") +
  geom_bar(
    aes( y = DENV_II, 
         fill = "DENV II"),
    stat = "identity",
    color = "black",
    width = .4,
    position = position_nudge(x = .20)) +
  geom_label(aes(y = DENV_II,
                 label = DENV_II),
             size = 3, 
             alpha = 0.5,
             nudge_x = .20,
             nudge_y = .5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

#########################################
#####   Critério de Encerramento   ######
#########################################

RS22_Serie_Historica_GRAF_Encerramento <- ggplot (RS22_22_23_GERAL, 
                                                  aes(x = Município)) + 
  theme(axis.text.x = element_text(angle = 85, 
                                   vjust = .5,
                                   face = "bold")
  ) +
  labs(caption = Fonte, 
       y = "Número de Casos",
       title = "CRITÉRIO DE ENCERRAMENTO/MUNICÍPIO") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 14,
                                   colour = "#556B2F")) +
  geom_bar(
    aes( y = Criterio_Encerramento_Lab, fill = "Laboratorial"),
    stat = "identity",
    color = "black",
    width = .4,
    position = position_nudge(x = -.20)) + 
  geom_label(aes(y = Criterio_Encerramento_Lab,
                 label = Criterio_Encerramento_Lab),
             size = 3, 
             alpha = 0.5,
             nudge_x = -.20,
             nudge_y = .5) + 
  scale_fill_manual(name = "", 
                    values = c("Laboratorial" = "#3CB371", 
                               "Clínico-epidemiológico" = "#2F657E")
  ) +
  theme(legend.position = "bottom") +
  geom_bar(
    aes( y = Criterio_Encerramento_Clin_Epid, 
         fill = "Clínico-epidemiológico"),
    stat = "identity",
    color = "black",
    width = .4,
    position = position_nudge(x = .20)) +
  geom_label(aes(y = Criterio_Encerramento_Clin_Epid,
                 label = Criterio_Encerramento_Clin_Epid),
             size = 3, 
             alpha = 0.5,
             nudge_x = .20,
             nudge_y = .5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) 

####################################################
###        Sintomas Confirmados e Notificados    ###
####################################################

AUX_GRAF <- data.frame(Sintomas = colnames(RS22_22_23_SINAIS_Notificados)[-c(1, 2, 3)],
                       Notificados = NA,
                       Confirmados = NA)
AUX_GRAF[1,2] <- sum(RS22_22_23_SINAIS_Notificados[, 4])
AUX_GRAF[2,2] <- sum(RS22_22_23_SINAIS_Notificados[, 5])
AUX_GRAF[3,2] <- sum(RS22_22_23_SINAIS_Notificados[, 6])
AUX_GRAF[4,2] <- sum(RS22_22_23_SINAIS_Notificados[, 7])
AUX_GRAF[5,2] <- sum(RS22_22_23_SINAIS_Notificados[, 8])
AUX_GRAF[6,2] <- sum(RS22_22_23_SINAIS_Notificados[, 9])
AUX_GRAF[7,2] <- sum(RS22_22_23_SINAIS_Notificados[, 10])
AUX_GRAF[8,2] <- sum(RS22_22_23_SINAIS_Notificados[, 11])
AUX_GRAF[9,2] <- sum(RS22_22_23_SINAIS_Notificados[, 12])
AUX_GRAF[10,2] <- sum(RS22_22_23_SINAIS_Notificados[, 13])
AUX_GRAF[11,2] <- sum(RS22_22_23_SINAIS_Notificados[, 14])
AUX_GRAF[12,2] <- sum(RS22_22_23_SINAIS_Notificados[, 15])
AUX_GRAF[13,2] <- sum(RS22_22_23_SINAIS_Notificados[, 16])
AUX_GRAF[14,2] <- sum(RS22_22_23_SINAIS_Notificados[, 17])

AUX_GRAF[1,3] <- sum(RS22_22_23_SINAIS_Confirmados[, 4])
AUX_GRAF[2,3] <- sum(RS22_22_23_SINAIS_Confirmados[, 5])
AUX_GRAF[3,3] <- sum(RS22_22_23_SINAIS_Confirmados[, 6])
AUX_GRAF[4,3] <- sum(RS22_22_23_SINAIS_Confirmados[, 7])
AUX_GRAF[5,3] <- sum(RS22_22_23_SINAIS_Confirmados[, 8])
AUX_GRAF[6,3] <- sum(RS22_22_23_SINAIS_Confirmados[, 9])
AUX_GRAF[7,3] <- sum(RS22_22_23_SINAIS_Confirmados[, 10])
AUX_GRAF[8,3] <- sum(RS22_22_23_SINAIS_Confirmados[, 11])
AUX_GRAF[9,3] <- sum(RS22_22_23_SINAIS_Confirmados[, 12])
AUX_GRAF[10,3] <- sum(RS22_22_23_SINAIS_Confirmados[, 13])
AUX_GRAF[11,3] <- sum(RS22_22_23_SINAIS_Confirmados[, 14])
AUX_GRAF[12,3] <- sum(RS22_22_23_SINAIS_Confirmados[, 15])
AUX_GRAF[13,3] <- sum(RS22_22_23_SINAIS_Confirmados[, 16])
AUX_GRAF[14,3] <- sum(RS22_22_23_SINAIS_Confirmados[, 17])

RS22_22_23_GRAF_SINAIS <- ggplot (AUX_GRAF, 
                                  aes(x = Sintomas)) + 
  theme(axis.text.x = element_text(angle = 80, 
                                   vjust = .5,
                                   face = "bold",
                                   size = 14)) +
  labs(caption = Fonte, 
       x = "Sintomas",
       y = "Número de Casos",
       title = "PREVALÊNCIA DE SINTOMAS NOS CASOS NOTIFICADOS/CONFIRMADOS 22ªRS - 2022/23",
       subtitle = "Sinais Clínicos em Notificações de DENGUE Assinalados no Campo 33 da Ficha do SINAN") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 19,
                                   colour = "#556B2F")) +
  geom_bar(
    aes( y = Notificados, fill = "Notificados"),
    stat = "identity",
    color = "black",
    width = .4,
    position = position_nudge(x = -.20)) + 
  geom_label(aes(y = Notificados,
                 label = Notificados),
             size = 3, 
             alpha = 0.5,
             nudge_x = -.20,
             nudge_y = .5) + 
  scale_fill_manual(name = "", 
                    values = c("Notificados" = "#4D5656", 
                               "Confirmados" = "#B03A2E")) +
  theme(legend.position = "bottom") +
  geom_bar(
    aes( y = Confirmados, 
         fill = "Confirmados"),
    stat = "identity",
    color = "black",
    width = .4,
    position = position_nudge(x = .20)) +
  geom_label(aes(y = Confirmados,
                 label = Confirmados),
             size = 3, 
             alpha = 0.5,
             nudge_x = .20,
             nudge_y = .2) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

###########################################################################
#####      Casos Notificados por município - Período sazonal atual     ####
###########################################################################

RS22_22_23_GRAF_Notificados <- ggplot (RS22_22_23_GERAL, 
                                       aes(x = Município, 
                                           y = Notificados)) + 
  theme(axis.text.x = element_text(angle = 85, 
                                   vjust = .5,
                                   face = "bold")) +
  labs(caption = Fonte, 
       x = "Municípios",
       y = "Número de Casos",
       title = "CASOS NOTIFICADOS/MUNICÍPIO - 2022/23") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 19,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#046236") + 
  geom_label(aes(label = Notificados), 
             size = 3, 
             alpha = 0.5,
             nudge_y = 0.9)  +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

###########################################################################
#####      Casos Confirmados por município - Período sazonal atual     ####
###########################################################################

AUX_GRAF <- data.frame (Municípios = RS22_22_23_GERAL[, 2],
                        Confirmados = (RS22_22_23_GERAL[, 6] + 
                                         RS22_22_23_GERAL[, 7] + 
                                         RS22_22_23_GERAL[, 8]
                        )
)
RS22_22_23_GRAF_Confirmados <- ggplot (AUX_GRAF, 
                                       aes(x = Municípios, 
                                           y = Confirmados)) + 
  theme(axis.text.x = element_text(angle = 85, 
                                   vjust = .5,
                                   face = "bold")) +
  labs(caption = Fonte, 
       y = "Número de Casos",
       title = "CASOS CONFIRMADOS/MUNICÍPIO - 2022/23") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 19,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#8E1C21") + 
  geom_label(aes(label = Confirmados), 
             size = 3, 
             alpha = 0.5,
             nudge_y = 0.2)  +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

###########################################################################
#####      Casos autóctones por município - Período sazonal atual      ####
###########################################################################

RS22_22_23_GRAF_Autoctones <- ggplot (RS22_22_23_GERAL, 
                                      aes(x = Município, 
                                          y = Autoctones)) + 
  theme(axis.text.x = element_text(angle = 85, 
                                   vjust = .5,
                                   face = "bold")) +
  labs(caption = Fonte, 
       x = "Municípios",
       y = "Número de Casos",
       title = "CASOS AUTÓCTONES/MUNICÍPIO - 2022/23") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 19,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#BDB76B") + 
  geom_label(aes(label = Autoctones), 
             size = 3, 
             alpha = 0.5,
             nudge_y = 0.07) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

###########################################################################
#####  Casos em investigação por município - Período sazonal atual     ####
###########################################################################

RS22_22_23_GRAF_Investigacao <- ggplot (RS22_22_23_GERAL, 
                                        aes(x = Município, 
                                            y = Em_Investigacao)) + 
  theme(axis.text.x = element_text(angle = 85, 
                                   vjust = .5,
                                   face = "bold")) +
  labs(caption = Fonte, 
       x = "Municípios",
       y = "Número de Casos",
       title = "CASOS EM INVESTIGAÇÃO/MUNICÍPIO - 2022/23") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 19,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#EEE8AA") + 
  geom_label(aes(label = Em_Investigacao), 
             size = 3, 
             alpha = 0.5,
             nudge_y = 0.2) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

###########################################################################
#####      Incidência por município - Período sazonal atual            ####
###########################################################################


RS22_22_23_GRAF_Incidencia <- ggplot (RS22_22_23_GERAL, 
                                      aes(x = Município, 
                                          y = Incidencia)) + 
  theme(axis.text.x = element_text(angle = 85, 
                                   vjust = .5,
                                   face = "bold")) +
  labs(caption = Fonte, 
       x = "Municípios",
       y = "Número de Casos",
       title = "INCIDÊNCIA/MUNICÍPIO (CASOS AUTÓCTONES) - 2022/23") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 19,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#8FBC8F") + 
  geom_label(aes(label = Incidencia), 
             size = 3, 
             alpha = 0.5,
             nudge_y = 2) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

###########################################################################
#####      Casos descartados por município - Período sazonal atual     ####
###########################################################################

RS22_22_23_GRAF_Descartados <- ggplot (RS22_22_23_GERAL, 
                                       aes(x = Município, 
                                           y = Descartados)) + 
  theme(axis.text.x = element_text(angle = 85, 
                                   vjust = .5,
                                   face = "bold")) +
  labs(caption = Fonte, 
       x = "Municípios",
       y = "Número de Casos",
       title = "CASOS DESCARTADOS/MUNICÍPIO - 2022/23") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 19,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#8FBC8F") + 
  geom_label(aes(label = Descartados),
             alpha = 0.5,
             nudge_y = 0.6) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

###########################################################################
#####   Casos hospitalizados por município - Período sazonal atual     ####
###########################################################################

RS22_22_23_GRAF_Hospitalizados <- ggplot (RS22_22_23_GERAL, 
                                          aes(x = Município, 
                                              y = Hospitalizacao)) + 
  theme(axis.text.x = element_text(angle = 85, 
                                   vjust = .5,
                                   face = "bold")) +
  labs(caption = Fonte, 
       x = "Municípios",
       y = "Número de Casos",
       title = "CASOS HOSPITALIZADOS/MUNICÍPIO - 2022/23") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 19,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#D2B48C") + 
  geom_label(aes(label = Hospitalizacao),
             alpha = 0.5,
             nudge_y = 0.02) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

###########################################################################
#####      Casos Inconclusivos por município - Período sazonal atual   ####
###########################################################################

AUX_GRAF <- data.frame (Municípios = RS22_22_23_GERAL[, 2],
                        Inconclusivos = (RS22_22_23_GERAL[, 20]
                        )
)
RS22_22_23_GRAF_Inconclusivos <- ggplot (AUX_GRAF, 
                                         aes(x = Municípios, 
                                             y = Inconclusivos)) + 
  theme(axis.text.x = element_text(angle = 85, 
                                   vjust = .5,
                                   face = "bold")) +
  labs(caption = Fonte, 
       y = "Número de Casos",
       title = "CASOS INCONCLUSIVOS/MUNICÍPIO - 2022/23") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 19,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#8E1C21") + 
  geom_label(aes(label = Inconclusivos), 
             size = 3, 
             alpha = 0.5,
             nudge_y = 0.2)  +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))


####################################################################################################################
############      Trabalhando a tabela base do Canal Endêmico - IVAIPORÃ      ######################################
####################################################################################################################

######Canal Endêmico NOTIFICADOS#####

RS_CE_Notificados_SEDE_Base[(nrow(RS_CE_Notificados_SEDE_Base) +1), 1] <- "2022/23"
RS_CE_Notificados_SEDE_Base[nrow(RS_CE_Notificados_SEDE_Base), 2:54] <- as.integer(data.frame(RS22_22_23_SE_Notificados[6, 2:54]))

#####Utilizando objetos auxiliares porque se transpor o data frame direto ele transforma as variáveis em#############
#####caracter.            NÃO FOI DESCARTADO AINDA OS PERÍODOS EPIDÊMICOS                               #############
##### VERIFICAR SE PODE-SE UTILIZAR A MÉDIA COMO LIMITE INFERIOR.                                       #############

AUX <- RS_CE_Notificados_SEDE_Base[, -1]

AUX <- t(AUX)

AUX2 <- RS_CE_Notificados_SEDE_Base[, 1]

colnames(AUX) <- AUX2

RS_CE_Notificados_SEDE <- AUX

######Criando a coluna de média no data.frame#####################

AUX <- apply(RS_CE_Notificados_SEDE[,1: (ncol(RS_CE_Notificados_SEDE)-1)], 1 , mean)

RS_CE_Notificados_SEDE <- as.data.frame(RS_CE_Notificados_SEDE)

RS_CE_Notificados_SEDE$Media <- AUX

######Criando a coluna de Desvio Padrão no data frame###############

AUX <- apply(RS_CE_Notificados_SEDE[,1: (ncol(RS_CE_Notificados_SEDE) -2)], 1 , sd)

RS_CE_Notificados_SEDE$Desvio_Padrao <- AUX

###### Criando a coluna de Média + 2(DP)

AUX <- RS_CE_Notificados_SEDE[, (ncol(RS_CE_Notificados_SEDE)-1):ncol(RS_CE_Notificados_SEDE)]

AUX <- AUX %>% mutate(Lim_Superior = (Media + 1.96 * Desvio_Padrao))

RS_CE_Notificados_SEDE$Lim_Superior <- AUX$Lim_Superior

RS_CE_Notificados_SEDE[, (ncol(RS_CE_Notificados_SEDE)+1)] <- rownames(RS_CE_Notificados_SEDE)

RS_CE_Notificados_SEDE <- RS_CE_Notificados_SEDE[, c(ncol(RS_CE_Notificados_SEDE), 1:(ncol(RS_CE_Notificados_SEDE) -1))]

RS_CE_Notificados_SEDE[,1] <- c(31:53, 1:30)

colnames(RS_CE_Notificados_SEDE)[1] <- "Semana_Epidemiológica"

rownames(RS_CE_Notificados_SEDE) <- c(1:nrow(RS_CE_Notificados_SEDE))

rm(AUX, AUX2, RS_CE_Notificados_SEDE_Base)

write.csv (RS_CE_Notificados_SEDE, 
           paste0("Base_de_Dados/Tabulacoes_R/Arboviroses/RS", RS, "_CE_Notificados_SEDE.csv"), 
           row.names = FALSE)
#####################################################################################################################################################################################
#####################################################################################################################################################################################
###CANAL ENDÊMICO NOTIFICADOS - IVAIPORÃ####

###Puxando os dados da tabela RS22_CE_Notificados e excluindo os períodos epidêmicos: 2015/16, 2019/20 e 2021/22

AUX_GRAF <- RS_CE_Notificados_SEDE[, Periodos_Epidêmicos_SEDE]

###Usando apply para tirar a média por semana epidemiológica

AUX_GRAF$Media <- apply(AUX_GRAF[, -ncol(AUX_GRAF)], 1 , mean)

###Usando apply para tirar o desvio padrão por semana epidemiológica

AUX_GRAF$Desvio_Padrao <- apply(AUX_GRAF[, 1:(ncol(AUX_GRAF)-2)], 1 , sd)

###### Criando a coluna de Média + 2(DP)

AUX_GRAF <- AUX_GRAF[, c((ncol(AUX_GRAF) -1): ncol(AUX_GRAF))]

AUX_GRAF$Lim_Superior <- NA

AUX_GRAF <- AUX_GRAF %>% mutate(Lim_Superior = (Media + 1.96 * Desvio_Padrao))

###Criando uma coluna de ordem das se para o R não colocar em ordem numérica.

AUX_GRAF$Ordem <- c(1: nrow(RS_CE_Notificados_SEDE))

###Puxando o período sazonal atual para o gráfico de linhas

AUX_GRAF$`2022/23` <- RS_CE_Notificados_SEDE$`2022/23`

AUX_GRAF$Sem_Epidemiológica <- RS_CE_Notificados_SEDE$Semana_Epidemiológica

AUX_GRAF$Sem_EPI <-as.character(c("2022/31",  "2022/32", "2022/33",  "2022/34",  "2022/35",  "2022/36",  "2022/37", 
                                  "2022/38",  "2022/39",  "2022/40",  "2022/41",  "2022/42",  "2022/43",  "2022/44",  
                                  "2022/45",  "2022/46",  "2022/47",  "2022/48",  "2022/49",  "2022/50",  "2022/51",  
                                  "2022/52",  "2022/53",  "2023/01",  "2023/02",  "2023/03",  "2023/04",  "2023/05",  
                                  "2023/06",  "2023/07",  "2023/08",  "2023/09",  "2023/10", "2023/11",  "2023/12",  
                                  "2023/13",  "2023/14",  "2023/15",  "2023/16",  "2023/17",  "2023/18",  "2023/19",  
                                  "2023/20",  "2023/21",  "2023/22",  "2023/23",  "2023/24",  "2023/25",  "2023/26",  
                                  "2023/27",  "2023/28",  "2023/29",  "2023/30"))

RS_22_23_GRAF_CE_Notificados_SEDE <- ggplot(AUX_GRAF, aes(Ordem))  +
  theme(axis.text.x = element_text(angle = 85, 
                                   vjust = .5,
                                   face = "bold")) +
  labs(caption = Fonte,
       title = "Canal Endêmico Casos Notificados IVAIPORÃ - 2022/23") +
  theme(
    panel.grid.major = element_line(color = "#C0C0C0"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#DC143C"),
    plot.title = element_text(face = "bold",
                              size = 19)
  ) +
  geom_area(aes(,Lim_Superior), fill = "#F0E68C",alpha = 0.9) +
  geom_area(aes(,Media), fill = "#556B2F") +
  geom_line(aes(,`2022/23`), stat = "identity", color = "black", linewidth = 1.5) +
  xlab("Semana Epidemiológica") +
  ylab("Número de Casos") +
  scale_x_continuous(breaks = c(1:53), label = AUX_GRAF$Sem_EPI) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

####Canal Endêmico CONFIRMADOS####

RS_CE_Confirmados_SEDE_Base[(nrow(RS_CE_Confirmados_SEDE_Base) +1), 1] <- "2022/23"
RS_CE_Confirmados_SEDE_Base[nrow(RS_CE_Confirmados_SEDE_Base), 2:54] <- as.integer(data.frame(RS22_22_23_SE_Confirmados[6, 2:54]))

#####Utilizando objetos auxiliares porque se transpor o data frame direto ele transforma as variáveis em#############
#####caracter.            NÃO FOI DESCARTADO AINDA OS PERÍODOS EPIDÊMICOS                               #############
##### VERIFICAR SE PODE-SE UTILIZAR A MÉDIA COMO LIMITE INFERIOR.                                       #############

AUX <- RS_CE_Confirmados_SEDE_Base[,-1]

AUX <- t(AUX)

AUX2 <- RS_CE_Confirmados_SEDE_Base[,1]

colnames(AUX) <- AUX2

RS_CE_Confirmados_SEDE <- AUX

######Criando a coluna de média no data.frame#####################

AUX <- apply(RS_CE_Confirmados_SEDE[, 1: (ncol(RS_CE_Confirmados_SEDE)-1)], 1 , mean)

RS_CE_Confirmados_SEDE <- as.data.frame(RS_CE_Confirmados_SEDE)

RS_CE_Confirmados_SEDE$Media <- AUX

######Criando a coluna de Desvio Padrão no data frame###############

AUX <- apply(RS_CE_Confirmados_SEDE[, 1: (ncol(RS_CE_Confirmados_SEDE) -2)], 1 , sd)

RS_CE_Confirmados_SEDE$Desvio_Padrao <- AUX

###### Criando a coluna de Média + 2(DP)

AUX <- RS_CE_Confirmados_SEDE[, (ncol(RS_CE_Confirmados_SEDE)-1):ncol(RS_CE_Confirmados_SEDE)]

AUX <- AUX %>% mutate(Lim_Superior = (Media + 1.96 * Desvio_Padrao))

RS_CE_Confirmados_SEDE$Lim_Superior <- AUX$Lim_Superior

RS_CE_Confirmados_SEDE[, (ncol(RS_CE_Confirmados_SEDE)+1)] <- rownames(RS_CE_Confirmados_SEDE)

RS_CE_Confirmados_SEDE <- RS_CE_Confirmados_SEDE[, c(ncol(RS_CE_Confirmados_SEDE), 1:(ncol(RS_CE_Confirmados_SEDE) -1))]

RS_CE_Confirmados_SEDE[,1] <- c(31:53, 1:30)

colnames(RS_CE_Confirmados_SEDE)[1] <- "Semana_Epidemiológica"

rownames(RS_CE_Confirmados_SEDE) <- c(1:nrow(RS_CE_Confirmados_SEDE))

rm(AUX, AUX2, RS_CE_Confirmados_SEDE_Base)

write.csv (RS_CE_Confirmados_SEDE, 
           paste0("Base_de_Dados/Tabulacoes_R/Arboviroses/RS", RS, "_CE_Confirmados_SEDE.csv"), 
           row.names = FALSE)

#####################################################################################################################################################################################
#####################################################################################################################################################################################
###CANAL ENDÊMICO Confirmados - IVAIPORÃ####

###Puxando os dados da tabela RS22_CE_Confirmados e excluindo os períodos epidêmicos: 2015/16, 2019/20 e 2021/22

AUX_GRAF <- RS_CE_Confirmados_SEDE[, Periodos_Epidêmicos_SEDE]

###Usando apply para tirar a média por semana epidemiológica

AUX_GRAF$Media <- apply(AUX_GRAF[, -ncol(AUX_GRAF)], 1 , mean)

###Usando apply para tirar o desvio padrão por semana epidemiológica

AUX_GRAF$Desvio_Padrao <- apply(AUX_GRAF[,1:(ncol(AUX_GRAF)-2)], 1 , sd)

###### Criando a coluna de Média + 2(DP)

AUX_GRAF <- AUX_GRAF[, c((ncol(AUX_GRAF)-1):ncol(AUX_GRAF))]

AUX_GRAF$Lim_Superior <- NA

AUX_GRAF <- AUX_GRAF %>% mutate(Lim_Superior = (Media + 1.96 * Desvio_Padrao))

###Criando uma coluna de ordem das se para o R não colocar em ordem numérica.

AUX_GRAF$Ordem <- c(1: nrow(RS_CE_Confirmados_SEDE))

###Puxando o período sazonal atual para o gráfico de linhas

AUX_GRAF$`2022/23` <- RS_CE_Confirmados_SEDE$`2022/23`

AUX_GRAF$Sem_Epidemiológica <- RS_CE_Confirmados_SEDE$Semana_Epidemiológica

AUX_GRAF$Sem_EPI <-as.character(c("2022/31",  "2022/32", "2022/33",  "2022/34",  "2022/35",  "2022/36",  
                                  "2022/37",  "2022/38",  "2022/39",  "2022/40",  "2022/41",  "2022/42",  
                                  "2022/43",  "2022/44",  "2022/45",  "2022/46",  "2022/47",  "2022/48",  
                                  "2022/49",  "2022/50",  "2022/51",  "2022/52",  "2022/53",  "2023/01", 
                                  "2023/02",  "2023/03",  "2023/04",  "2023/05",  "2023/06",  "2023/07",  
                                  "2023/08",  "2023/09",  "2023/10", "2023/11",  "2023/12",  "2023/13",  
                                  "2023/14",  "2023/15",  "2023/16",  "2023/17",  "2023/18",  "2023/19", 
                                  "2023/20",  "2023/21",  "2023/22",  "2023/23",  "2023/24",  "2023/25", 
                                  "2023/26",  "2023/27",  "2023/28",  "2023/29",  "2023/30"))

RS_22_23_GRAF_CE_Confirmados_SEDE <- ggplot(AUX_GRAF, aes(Ordem))  +
  theme(axis.text.x = element_text(angle = 85, 
                                   vjust = .5,
                                   face = "bold")) +
  labs(caption = Fonte,
       title = "Canal Endêmico Casos CONFIRMADOS Ivaiporã- 2022/23") +
  theme(
    panel.grid.major = element_line(color = "#C0C0C0"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#DC143C"),
    plot.title = element_text(face = "bold",
                              size = 19)
  ) +
  geom_area(aes(,Lim_Superior), fill = "#F0E68C",alpha = 0.9) +
  geom_area(aes(,Media), fill = "#556B2F") +
  geom_line(aes(,`2022/23`), stat = "identity", color = "black", linewidth = 1.5) +
  xlab("Semana Epidemiológica") +
  ylab("Número de Casos") +
  scale_x_continuous(breaks = c(1:53), label = AUX_GRAF$Sem_EPI) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

####################################################################################################################################
####################################################################################################################################
###############Canal Endêmico Prováveis - IVAIPORÃ

####Casos prováveis por semana epidemiológica. Este objeto será apagado assim que for incluso no AUX_GRAF####

RS_22_23_Casos_Provaveis_SEDE <- (RS22_22_23_SE_Provaveis[6, 2: 54])

rownames(RS_22_23_Casos_Provaveis_SEDE)[1] <- "Provaveis"

RS_22_23_Casos_Provaveis_SEDE <- t(as.data.frame(RS_22_23_Casos_Provaveis_SEDE))

###CANAL ENDÊMICO Prováveis####

###Puxando os dados da tabela RS22_CE_Notificados e excluindo os períodos epidêmicos: 2015/16, 2019/20 e 2021/22

AUX_GRAF <- RS_CE_Confirmados_SEDE[, Periodos_Epidêmicos_SEDE]

###Usando apply para tirar a média por semana epidemiológica

AUX_GRAF$Media <- apply(AUX_GRAF[, -ncol(AUX_GRAF)], 1 , mean)

###Usando apply para tirar o desvio padrão por semana epidemiológica

AUX_GRAF$Desvio_Padrao <- apply(AUX_GRAF[, 1:(ncol(AUX_GRAF)-2)], 1 , sd)

###### Criando a coluna de Média + 2(DP)

AUX_GRAF <- AUX_GRAF[, c((ncol(AUX_GRAF)-1):ncol(AUX_GRAF))]

AUX_GRAF$Lim_Superior <- NA

AUX_GRAF <- AUX_GRAF %>% mutate(Lim_Superior = (Media + 1.96 * Desvio_Padrao))

###Criando uma coluna de ordem das se para o R não colocar em ordem numérica.

AUX_GRAF$Ordem <- c(1: nrow(RS_CE_Confirmados_SEDE))

###Puxando o período sazonal atual para o gráfico de linhas

AUX_GRAF$`2022/23` <- RS_CE_Confirmados_SEDE$`2022/23`

AUX_GRAF$Sem_Epidemiológica <- RS_CE_Confirmados_SEDE$Semana_Epidemiológica

AUX_GRAF$Sem_EPI <-as.character(c("2022/31",  "2022/32", "2022/33",  "2022/34",  "2022/35",  "2022/36",  
                                  "2022/37",  "2022/38",  "2022/39",  "2022/40",  "2022/41",  "2022/42",  
                                  "2022/43",  "2022/44",  "2022/45",  "2022/46",  "2022/47",  "2022/48",  
                                  "2022/49",  "2022/50",  "2022/51",  "2022/52",  "2022/53",  "2023/01",  
                                  "2023/02",  "2023/03",  "2023/04",  "2023/05",  "2023/06",  "2023/07",  
                                  "2023/08",  "2023/09",  "2023/10", "2023/11",  "2023/12",  "2023/13",  
                                  "2023/14",  "2023/15",  "2023/16",  "2023/17",  "2023/18",  "2023/19",  
                                  "2023/20",  "2023/21",  "2023/22",  "2023/23",  "2023/24",  "2023/25",  
                                  "2023/26",  "2023/27",  "2023/28",  "2023/29",  "2023/30"))

AUX_GRAF[, 8] <- RS_22_23_Casos_Provaveis_SEDE[,1]

colnames(AUX_GRAF)[8] <- "Provaveis"

RS_22_23_GRAF_CE_Provaveis_SEDE <- ggplot(AUX_GRAF, aes(Ordem))  +
  theme(axis.text.x = element_text(angle = 85, 
                                   vjust = .5,
                                   face = "bold")) +
  labs(caption = Fonte,
       title = "Canal Endêmico Casos PROVÁVEIS Ivaiporã - 2022/23") +
  theme(
    panel.grid.major = element_line(color = "#C0C0C0"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#DC143C"),
    plot.title = element_text(face = "bold",
                              size = 19)
  ) +
  geom_area(aes(,Lim_Superior), fill = "#F0E68C",alpha = 0.9) +
  geom_area(aes(,Media), fill = "#556B2F") +
  geom_line(aes(,Provaveis), stat = "identity", color = "black", linewidth = 1.5) +
  xlab("Semana Epidemiológica") +
  ylab("Número de Casos") +
  scale_x_continuous(breaks = c(1:53), label = AUX_GRAF$Sem_EPI) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

rm(RS_22_23_Casos_Provaveis_SEDE, AUX_GRAF)


##########################################################################################################################################
###################################   Vigilância Laboratorial  ###########################################################################


###########################################################################################################################################
###############################  Buscando CSV com dados do LACEN   ########################################################################
###########################################################################################################################################

RS22_22_23_LACEN_PESQ_ARBO <- read.csv("/home/gustavo/Área de Trabalho/Análise_de_Dados/Base_de_Dados/LACEN/Arboviroses/LACEN_PESQUISA_ARBOVIRUS_22_23.csv",
                                       header = TRUE,
                                       sep = ",")

#######################    Manipulando dados para criação da coluna SE   #########################################################

### stringr para separar data de hora da coluna Dt_Cadastro ####

AUX <- as.data.frame(str_split(RS22_22_23_LACEN_PESQ_ARBO$Dt_Cadastro, 
                               pattern = " ")
)

AUX <- t(AUX)

colnames(AUX)<- c("Data", "Hora")

AUX <- as.data.frame(AUX)

AUX$Data <- dmy(AUX$Data)

RS22_22_23_LACEN_PESQ_ARBO$Dt_Cadastro <- AUX$Data

###### Lubridate  para criar SE a partir de darta ######

RS22_22_23_LACEN_PESQ_ARBO$SE <- epiweek(AUX$Data)


####################  uSANDO STRINGR PARA ACENTUAR A TABELA DO lacen TORNANDO-A COMPATÍVEL COM A base_ibge   ###################################

RS22_22_23_LACEN_PESQ_ARBO$Municipio_Residencia <- str_replace(RS22_22_23_LACEN_PESQ_ARBO$Municipio_Residencia, "ARAPUA", "ARAPUÃ")

RS22_22_23_LACEN_PESQ_ARBO$Municipio_Residencia <- str_replace(RS22_22_23_LACEN_PESQ_ARBO$Municipio_Residencia, "ARIRANHA DO IVAI", "ARIRANHA DO IVAÍ")

RS22_22_23_LACEN_PESQ_ARBO$Municipio_Residencia <- str_replace(RS22_22_23_LACEN_PESQ_ARBO$Municipio_Residencia, "CANDIDO DE ABREU", "CÂNDIDO DE ABREU")

RS22_22_23_LACEN_PESQ_ARBO$Municipio_Residencia <- str_replace(RS22_22_23_LACEN_PESQ_ARBO$Municipio_Residencia, "IVAIPORA", "IVAIPORÃ")

RS22_22_23_LACEN_PESQ_ARBO$Municipio_Residencia <- str_replace(RS22_22_23_LACEN_PESQ_ARBO$Municipio_Residencia, "LIDIANOPOLIS", "LIDIANÓPOLIS")

RS22_22_23_LACEN_PESQ_ARBO$Municipio_Residencia <- str_replace(RS22_22_23_LACEN_PESQ_ARBO$Municipio_Residencia, "RIO BRANCO DO IVAÍ", "RIO BRANCO DO IVAÍ")

RS22_22_23_LACEN_PESQ_ARBO$Municipio_Residencia <- str_replace(RS22_22_23_LACEN_PESQ_ARBO$Municipio_Residencia, "ROSÁRIO DO IVAÍ", "ROSÁRIO DO IVAÍ")

RS22_22_23_LACEN_PESQ_ARBO$Municipio_Residencia <- str_replace(RS22_22_23_LACEN_PESQ_ARBO$Municipio_Residencia, "SAO JOAO DO IVAI", "SÃO JOÃO DO IVAÍ")


####################   Transformando colunas em fatores para o for loop funcionar   ##################################

RS22_22_23_LACEN_PESQ_ARBO[, 9] <- as.factor(RS22_22_23_LACEN_PESQ_ARBO[, 9])

RS22_22_23_LACEN_PESQ_ARBO[, 23] <- as.factor(RS22_22_23_LACEN_PESQ_ARBO[, 23])

RS22_22_23_LACEN_PESQ_ARBO[, 24] <- as.factor(RS22_22_23_LACEN_PESQ_ARBO[, 24])

###########################################################################################################################################
###############################  Buscando CSV com dados do LACEN   ########################################################################
###########################################################################################################################################

RS22_22_23_LACEN_SOROLOGIA <- read.csv("/home/gustavo/Área de Trabalho/Análise_de_Dados/Base_de_Dados/LACEN/Arboviroses/LACEN_SOROLOGIA_22_23.csv",
                                       header = TRUE,
                                       sep = ",")

#######################    Manipulando dados para criação da coluna SE   #########################################################

### stringr para separar data de hora da coluna Dt_Cadastro ####

AUX <- as.data.frame(str_split(RS22_22_23_LACEN_SOROLOGIA$Dt_Cadastro, 
                               pattern = " ")
)

AUX <- t(AUX)

colnames(AUX)<- c("Data", "Hora")

AUX <- as.data.frame(AUX)

AUX$Data <- dmy(AUX$Data)

RS22_22_23_LACEN_SOROLOGIA$Dt_Cadastro <- AUX$Data

###### Lubridate  para criar SE a partir de data ######

RS22_22_23_LACEN_SOROLOGIA$SE <- epiweek(AUX$Data)

####################  USANDO STRINGR PARA ACENTUAR A TABELA DO lacen TORNANDO-A COMPATÍVEL COM A base_ibge   ###################################

RS22_22_23_LACEN_SOROLOGIA$Municipio_Residencia <- str_replace(RS22_22_23_LACEN_SOROLOGIA$Municipio_Residencia, "ARAPUA", "ARAPUÃ")

RS22_22_23_LACEN_SOROLOGIA$Municipio_Residencia <- str_replace(RS22_22_23_LACEN_SOROLOGIA$Municipio_Residencia, "ARIRANHA DO IVAI", "ARIRANHA DO IVAÍ")

RS22_22_23_LACEN_SOROLOGIA$Municipio_Residencia <- str_replace(RS22_22_23_LACEN_SOROLOGIA$Municipio_Residencia, "CANDIDO DE ABREU", "CÂNDIDO DE ABREU")

RS22_22_23_LACEN_SOROLOGIA$Municipio_Residencia <- str_replace(RS22_22_23_LACEN_SOROLOGIA$Municipio_Residencia, "IVAIPORA", "IVAIPORÃ")

RS22_22_23_LACEN_SOROLOGIA$Municipio_Residencia <- str_replace(RS22_22_23_LACEN_SOROLOGIA$Municipio_Residencia, "LIDIANOPOLIS", "LIDIANÓPOLIS")

RS22_22_23_LACEN_SOROLOGIA$Municipio_Residencia <- str_replace(RS22_22_23_LACEN_SOROLOGIA$Municipio_Residencia, "RIO BRANCO DO IVAÍ", "RIO BRANCO DO IVAÍ")

RS22_22_23_LACEN_SOROLOGIA$Municipio_Residencia <- str_replace(RS22_22_23_LACEN_SOROLOGIA$Municipio_Residencia, "ROSÁRIO DO IVAÍ", "ROSÁRIO DO IVAÍ")

RS22_22_23_LACEN_SOROLOGIA$Municipio_Residencia <- str_replace(RS22_22_23_LACEN_SOROLOGIA$Municipio_Residencia, "SAO JOAO DO IVAI", "SÃO JOÃO DO IVAÍ")

####################   Transformando colunas em fatores para o for loop funcionar   ##################################

RS22_22_23_LACEN_SOROLOGIA[, 9] <- as.factor(RS22_22_23_LACEN_SOROLOGIA[, 9])

RS22_22_23_LACEN_SOROLOGIA[, 24] <- as.factor(RS22_22_23_LACEN_SOROLOGIA[, 24])

RS22_22_23_LACEN_SOROLOGIA[, 25] <- as.factor(RS22_22_23_LACEN_SOROLOGIA[, 25])

rm(AUX)

############################################################################################################################################
#####################  Realizando a contagem de exames por SE PESQ ARBO GERAL  #############################################################

RS22_22_23_SE_PESQ_ARB <- matrix(data = NA, 
                                 nrow = 16, 
                                 ncol = 54)

RS22_22_23_SE_PESQ_ARB <- as.data.frame(RS22_22_23_SE_PESQ_ARB)

colnames(RS22_22_23_SE_PESQ_ARB)[1] <- "Município" 

RS22_22_23_SE_PESQ_ARB[,1] <- BASE_IBGE[which(BASE_IBGE$RS == 22), 3]

colnames (RS22_22_23_SE_PESQ_ARB)[2:24] <- c(31:53)

colnames (RS22_22_23_SE_PESQ_ARB)[25:54] <- c(1:30)

for (i in BASE_IBGE[(which(BASE_IBGE$RS == 22)), 3]){
  
  RS22_22_23_SE_PESQ_ARB[which(RS22_22_23_SE_PESQ_ARB == i), 2] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                                filter(Municipio_Residencia == i,
                                                                                       SE == 31,
                                                                                       Status_Exame == "Resultado Liberado" |
                                                                                         Status_Exame == "Automação em Processo" |
                                                                                         Status_Exame == "Disponivel para Encaminhar") %>%
                                                                                count()
  )
  
  RS22_22_23_SE_PESQ_ARB[which(RS22_22_23_SE_PESQ_ARB == i), 3] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                                filter(Municipio_Residencia == i,
                                                                                       SE == 32,
                                                                                       Status_Exame == "Resultado Liberado" |
                                                                                         Status_Exame == "Automação em Processo" |
                                                                                         Status_Exame == "Disponivel para Encaminhar") %>% 
                                                                                count()
  )
  
  RS22_22_23_SE_PESQ_ARB[which(RS22_22_23_SE_PESQ_ARB == i), 4] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                                filter(Municipio_Residencia == i,
                                                                                       SE == 33,
                                                                                       Status_Exame == "Resultado Liberado" |
                                                                                         Status_Exame == "Automação em Processo" |
                                                                                         Status_Exame == "Disponivel para Encaminhar") %>% 
                                                                                count()
  )
  
  RS22_22_23_SE_PESQ_ARB[which(RS22_22_23_SE_PESQ_ARB == i),5] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                               filter(Municipio_Residencia == i,
                                                                                      SE == 34,
                                                                                      Status_Exame == "Resultado Liberado" |
                                                                                        Status_Exame == "Automação em Processo" |
                                                                                        Status_Exame == "Disponivel para Encaminhar") %>% 
                                                                               count()
  )
  
  RS22_22_23_SE_PESQ_ARB[which(RS22_22_23_SE_PESQ_ARB == i), 6] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                                filter(Municipio_Residencia == i,
                                                                                       SE == 35,
                                                                                       Status_Exame == "Resultado Liberado" |
                                                                                         Status_Exame == "Automação em Processo" |
                                                                                         Status_Exame == "Disponivel para Encaminhar") %>% 
                                                                                count() 
  )
  
  RS22_22_23_SE_PESQ_ARB[which(RS22_22_23_SE_PESQ_ARB == i), 7] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                                filter(Municipio_Residencia == i,
                                                                                       SE == 36,
                                                                                       Status_Exame == "Resultado Liberado" |
                                                                                         Status_Exame == "Automação em Processo" |
                                                                                         Status_Exame == "Disponivel para Encaminhar") %>%
                                                                                count()
  )
  
  RS22_22_23_SE_PESQ_ARB[which(RS22_22_23_SE_PESQ_ARB == i), 8] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                                filter(Municipio_Residencia == i,
                                                                                       SE == 37,
                                                                                       Status_Exame == "Resultado Liberado" |
                                                                                         Status_Exame == "Automação em Processo" |
                                                                                         Status_Exame == "Disponivel para Encaminhar") %>% 
                                                                                count() 
  )
  
  RS22_22_23_SE_PESQ_ARB[which(RS22_22_23_SE_PESQ_ARB == i), 9] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                                filter(Municipio_Residencia == i,
                                                                                       SE == 38,
                                                                                       Status_Exame == "Resultado Liberado" |
                                                                                         Status_Exame == "Automação em Processo" |
                                                                                         Status_Exame == "Disponivel para Encaminhar") %>% 
                                                                                count() 
  )
  
  RS22_22_23_SE_PESQ_ARB[which(RS22_22_23_SE_PESQ_ARB == i), 10] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 39,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automação em Processo" |
                                                                                          Status_Exame == "Disponivel para Encaminhar") %>%
                                                                                 count()
  )
  
  RS22_22_23_SE_PESQ_ARB[which(RS22_22_23_SE_PESQ_ARB == i), 11] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 40,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automação em Processo" |
                                                                                          Status_Exame == "Disponivel para Encaminhar") %>%
                                                                                 count() 
  )
  
  RS22_22_23_SE_PESQ_ARB[which(RS22_22_23_SE_PESQ_ARB == i), 12] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 41,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automação em Processo" |
                                                                                          Status_Exame == "Disponivel para Encaminhar") %>%
                                                                                 count() 
  )
  
  RS22_22_23_SE_PESQ_ARB[which(RS22_22_23_SE_PESQ_ARB == i), 13] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 42,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automação em Processo" |
                                                                                          Status_Exame == "Disponivel para Encaminhar") %>%
                                                                                 count() 
  )
  
  RS22_22_23_SE_PESQ_ARB[which(RS22_22_23_SE_PESQ_ARB == i), 14] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 43,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automação em Processo" |
                                                                                          Status_Exame == "Disponivel para Encaminhar") %>% 
                                                                                 count() 
  )
  
  RS22_22_23_SE_PESQ_ARB[which(RS22_22_23_SE_PESQ_ARB == i), 15] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 44,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automação em Processo" |
                                                                                          Status_Exame == "Disponivel para Encaminhar") %>%
                                                                                 count() 
  )
  
  RS22_22_23_SE_PESQ_ARB[which(RS22_22_23_SE_PESQ_ARB == i), 16] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 45,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automação em Processo" |
                                                                                          Status_Exame == "Disponivel para Encaminhar") %>%
                                                                                 count() 
  )
  
  RS22_22_23_SE_PESQ_ARB[which(RS22_22_23_SE_PESQ_ARB == i), 17] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 46,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automação em Processo" |
                                                                                          Status_Exame == "Disponivel para Encaminhar") %>%
                                                                                 count()
  )
  
  RS22_22_23_SE_PESQ_ARB[which(RS22_22_23_SE_PESQ_ARB == i), 18] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 47,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automação em Processo" |
                                                                                          Status_Exame == "Disponivel para Encaminhar") %>%       
                                                                                 count() 
  )
  
  RS22_22_23_SE_PESQ_ARB[which(RS22_22_23_SE_PESQ_ARB == i), 19] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 48,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automação em Processo" |
                                                                                          Status_Exame == "Disponivel para Encaminhar") %>%     
                                                                                 count() 
  )
  
  RS22_22_23_SE_PESQ_ARB[which(RS22_22_23_SE_PESQ_ARB == i), 20] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 49,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automação em Processo" |
                                                                                          Status_Exame == "Disponivel para Encaminhar") %>%
                                                                                 count() 
  )
  
  RS22_22_23_SE_PESQ_ARB[which(RS22_22_23_SE_PESQ_ARB == i),  21] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                                  filter(Municipio_Residencia == i,
                                                                                         SE == 50,
                                                                                         Status_Exame == "Resultado Liberado" |
                                                                                           Status_Exame == "Automação em Processo" |
                                                                                           Status_Exame == "Disponivel para Encaminhar") %>%
                                                                                  count() 
  )
  
  RS22_22_23_SE_PESQ_ARB[which(RS22_22_23_SE_PESQ_ARB == i), 22] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 51,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automação em Processo" |
                                                                                          Status_Exame == "Disponivel para Encaminhar") %>%
                                                                                 count() 
  )
  
  RS22_22_23_SE_PESQ_ARB[which(RS22_22_23_SE_PESQ_ARB == i), 23] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 52,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automação em Processo" |
                                                                                          Status_Exame == "Disponivel para Encaminhar") %>%
                                                                                 count() 
  )
  
  RS22_22_23_SE_PESQ_ARB[which(RS22_22_23_SE_PESQ_ARB == i), 24] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 53,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automação em Processo" |
                                                                                          Status_Exame == "Disponivel para Encaminhar") %>%
                                                                                 count()
  )
  
  RS22_22_23_SE_PESQ_ARB[which(RS22_22_23_SE_PESQ_ARB == i), 25] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 1,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automação em Processo" |
                                                                                          Status_Exame == "Disponivel para Encaminhar") %>%
                                                                                 count() 
  )
  
  RS22_22_23_SE_PESQ_ARB[which(RS22_22_23_SE_PESQ_ARB == i), 26] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 2,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automação em Processo" |
                                                                                          Status_Exame == "Disponivel para Encaminhar") %>%
                                                                                 count()
  )
  
  RS22_22_23_SE_PESQ_ARB[which(RS22_22_23_SE_PESQ_ARB == i), 27] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 3,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automação em Processo" |
                                                                                          Status_Exame == "Disponivel para Encaminhar") %>%
                                                                                 count() 
  )
  
  RS22_22_23_SE_PESQ_ARB[which(RS22_22_23_SE_PESQ_ARB == i), 28] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 4,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automação em Processo" |
                                                                                          Status_Exame == "Disponivel para Encaminhar") %>%
                                                                                 count() 
  )
  
  RS22_22_23_SE_PESQ_ARB[which(RS22_22_23_SE_PESQ_ARB == i), 29] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 5,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automação em Processo" |
                                                                                          Status_Exame == "Disponivel para Encaminhar") %>%
                                                                                 count() 
  )
  
  RS22_22_23_SE_PESQ_ARB[which(RS22_22_23_SE_PESQ_ARB == i), 30] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 6,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automação em Processo" |
                                                                                          Status_Exame == "Disponivel para Encaminhar") %>%
                                                                                 count() 
  )
  
  RS22_22_23_SE_PESQ_ARB[which(RS22_22_23_SE_PESQ_ARB == i), 31] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 7,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automação em Processo" |
                                                                                          Status_Exame == "Disponivel para Encaminhar") %>% 
                                                                                 count()
  )
  
  RS22_22_23_SE_PESQ_ARB[which(RS22_22_23_SE_PESQ_ARB == i), 32] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 8,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automação em Processo" |
                                                                                          Status_Exame == "Disponivel para Encaminhar") %>%
                                                                                 count()
  )
  
  RS22_22_23_SE_PESQ_ARB[which(RS22_22_23_SE_PESQ_ARB == i), 33] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 9,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automação em Processo" |
                                                                                          Status_Exame == "Disponivel para Encaminhar") %>%
                                                                                 count() 
  )
  
  RS22_22_23_SE_PESQ_ARB[which(RS22_22_23_SE_PESQ_ARB == i), 34] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 10,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automação em Processo" |
                                                                                          Status_Exame == "Disponivel para Encaminhar") %>% 
                                                                                 count() 
  )
  
  RS22_22_23_SE_PESQ_ARB[which(RS22_22_23_SE_PESQ_ARB == i), 35] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 11,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automação em Processo" |
                                                                                          Status_Exame == "Disponivel para Encaminhar") %>% 
                                                                                 count() 
  )
  
  RS22_22_23_SE_PESQ_ARB[which(RS22_22_23_SE_PESQ_ARB == i), 36] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 12,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automação em Processo" |
                                                                                          Status_Exame == "Disponivel para Encaminhar") %>%
                                                                                 count() 
  )
  
  RS22_22_23_SE_PESQ_ARB[which(RS22_22_23_SE_PESQ_ARB == i), 37] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 13,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automação em Processo" |
                                                                                          Status_Exame == "Disponivel para Encaminhar") %>%
                                                                                 count() 
  )
  
  RS22_22_23_SE_PESQ_ARB[which(RS22_22_23_SE_PESQ_ARB == i), 38] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 14,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automação em Processo" |
                                                                                          Status_Exame == "Disponivel para Encaminhar") %>% 
                                                                                 count() 
  )
  
  RS22_22_23_SE_PESQ_ARB[which(RS22_22_23_SE_PESQ_ARB == i), 39] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 15,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automação em Processo" |
                                                                                          Status_Exame == "Disponivel para Encaminhar") %>%
                                                                                 count()
  )
  
  RS22_22_23_SE_PESQ_ARB[which(RS22_22_23_SE_PESQ_ARB == i), 40] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 16,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automação em Processo" |
                                                                                          Status_Exame == "Disponivel para Encaminhar") %>%
                                                                                 count() 
  )
  
  RS22_22_23_SE_PESQ_ARB[which(RS22_22_23_SE_PESQ_ARB == i), 41] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 17,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automação em Processo" |
                                                                                          Status_Exame == "Disponivel para Encaminhar") %>%
                                                                                 count() 
  )
  
  RS22_22_23_SE_PESQ_ARB[which(RS22_22_23_SE_PESQ_ARB == i), 42] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 18,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automação em Processo" |
                                                                                          Status_Exame == "Disponivel para Encaminhar") %>%
                                                                                 count() 
  )
  
  RS22_22_23_SE_PESQ_ARB[which(RS22_22_23_SE_PESQ_ARB == i), 43] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 19,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automação em Processo" |
                                                                                          Status_Exame == "Disponivel para Encaminhar") %>%
                                                                                 count()
  )
  
  RS22_22_23_SE_PESQ_ARB[which(RS22_22_23_SE_PESQ_ARB == i), 44] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 20,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automação em Processo" |
                                                                                          Status_Exame == "Disponivel para Encaminhar") %>%
                                                                                 count()
  )
  
  RS22_22_23_SE_PESQ_ARB[which(RS22_22_23_SE_PESQ_ARB == i), 45] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 21,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automação em Processo" |
                                                                                          Status_Exame == "Disponivel para Encaminhar") %>%
                                                                                 count() 
  )
  
  RS22_22_23_SE_PESQ_ARB[which(RS22_22_23_SE_PESQ_ARB == i), 46] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 22,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automação em Processo" |
                                                                                          Status_Exame == "Disponivel para Encaminhar") %>%
                                                                                 count()
  )
  
  RS22_22_23_SE_PESQ_ARB[which(RS22_22_23_SE_PESQ_ARB == i), 47] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 23,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automação em Processo" |
                                                                                          Status_Exame == "Disponivel para Encaminhar") %>%
                                                                                 count() 
  )
  
  RS22_22_23_SE_PESQ_ARB[which(RS22_22_23_SE_PESQ_ARB == i), 48] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 24,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automação em Processo" |
                                                                                          Status_Exame == "Disponivel para Encaminhar") %>%
                                                                                 count() 
  )
  
  RS22_22_23_SE_PESQ_ARB[which(RS22_22_23_SE_PESQ_ARB == i), 49] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 25,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automação em Processo" |
                                                                                          Status_Exame == "Disponivel para Encaminhar") %>%
                                                                                 count() 
  )
  
  RS22_22_23_SE_PESQ_ARB[which(RS22_22_23_SE_PESQ_ARB == i), 50] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 26,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automação em Processo" |
                                                                                          Status_Exame == "Disponivel para Encaminhar") %>% 
                                                                                 count()
  )
  
  RS22_22_23_SE_PESQ_ARB[which(RS22_22_23_SE_PESQ_ARB == i), 51] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 27,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automação em Processo" |
                                                                                          Status_Exame == "Disponivel para Encaminhar") %>% 
                                                                                 count() 
  )
  
  RS22_22_23_SE_PESQ_ARB[which(RS22_22_23_SE_PESQ_ARB == i), 52] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 28,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automação em Processo" |
                                                                                          Status_Exame == "Disponivel para Encaminhar") %>%
                                                                                 count() 
  )
  
  RS22_22_23_SE_PESQ_ARB[which(RS22_22_23_SE_PESQ_ARB == i), 53] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 29,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automação em Processo" |
                                                                                          Status_Exame == "Disponivel para Encaminhar") %>%
                                                                                 count() 
  )
  
  RS22_22_23_SE_PESQ_ARB[which(RS22_22_23_SE_PESQ_ARB == i), 54] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 30,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automação em Processo" |
                                                                                          Status_Exame == "Disponivel para Encaminhar") %>%
                                                                                 count() 
  )
}

RS22_22_23_SE_PESQ_ARB[17,2:54] <- apply(RS22_22_23_SE_PESQ_ARB[,2:54], 2, sum)

RS22_22_23_SE_PESQ_ARB[17,1] <- "Total"

############################################################################################################################################
#####################  Realizando a contagem de exames por SE PESQ ARBO  DETECTÁVEIS GERAL  #############################################################

RS22_22_23_SE_PESQ_ARB_DETECTAVEL <- matrix(data = NA, 
                                            nrow = 16, 
                                            ncol = 54)

RS22_22_23_SE_PESQ_ARB_DETECTAVEL <- as.data.frame(RS22_22_23_SE_PESQ_ARB_DETECTAVEL)

colnames(RS22_22_23_SE_PESQ_ARB_DETECTAVEL)[1] <- "Município" 

RS22_22_23_SE_PESQ_ARB_DETECTAVEL[,1] <- BASE_IBGE[which(BASE_IBGE$RS == 22), 3]

colnames (RS22_22_23_SE_PESQ_ARB_DETECTAVEL)[2:24] <- c(31:53)

colnames (RS22_22_23_SE_PESQ_ARB_DETECTAVEL)[25:54] <- c(1:30)

for (i in BASE_IBGE[(which(BASE_IBGE$RS == 22)), 3]){
  
  RS22_22_23_SE_PESQ_ARB_DETECTAVEL[which(RS22_22_23_SE_PESQ_ARB_DETECTAVEL == i), 2] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                                                      filter(Municipio_Residencia == i,
                                                                                                             SE == 31,
                                                                                                             Resultado == "Detectável") %>%
                                                                                                      count()
  )
  
  RS22_22_23_SE_PESQ_ARB_DETECTAVEL[which(RS22_22_23_SE_PESQ_ARB_DETECTAVEL == i), 3] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                                                      filter(Municipio_Residencia == i,
                                                                                                             SE == 32,
                                                                                                             Resultado == "Detectável") %>% 
                                                                                                      count()
  )
  
  RS22_22_23_SE_PESQ_ARB_DETECTAVEL[which(RS22_22_23_SE_PESQ_ARB_DETECTAVEL == i), 4] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                                                      filter(Municipio_Residencia == i,
                                                                                                             SE == 33,
                                                                                                             Resultado == "Detectável") %>% 
                                                                                                      count()
  )
  
  RS22_22_23_SE_PESQ_ARB_DETECTAVEL[which(RS22_22_23_SE_PESQ_ARB_DETECTAVEL == i),5] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                                                     filter(Municipio_Residencia == i,
                                                                                                            SE == 34,
                                                                                                            Resultado == "Detectável") %>% 
                                                                                                     count()
  )
  
  RS22_22_23_SE_PESQ_ARB_DETECTAVEL[which(RS22_22_23_SE_PESQ_ARB_DETECTAVEL == i), 6] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                                                      filter(Municipio_Residencia == i,
                                                                                                             SE == 35,
                                                                                                             Resultado == "Detectável") %>% 
                                                                                                      count() 
  )
  
  RS22_22_23_SE_PESQ_ARB_DETECTAVEL[which(RS22_22_23_SE_PESQ_ARB_DETECTAVEL == i), 7] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                                                      filter(Municipio_Residencia == i,
                                                                                                             SE == 36,
                                                                                                             Resultado == "Detectável") %>%
                                                                                                      count()
  )
  
  RS22_22_23_SE_PESQ_ARB_DETECTAVEL[which(RS22_22_23_SE_PESQ_ARB_DETECTAVEL == i), 8] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                                                      filter(Municipio_Residencia == i,
                                                                                                             SE == 37,
                                                                                                             Resultado == "Detectável") %>% 
                                                                                                      count() 
  )
  
  RS22_22_23_SE_PESQ_ARB_DETECTAVEL[which(RS22_22_23_SE_PESQ_ARB_DETECTAVEL == i), 9] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                                                      filter(Municipio_Residencia == i,
                                                                                                             SE == 38,
                                                                                                             Resultado == "Detectável") %>% 
                                                                                                      count() 
  )
  
  RS22_22_23_SE_PESQ_ARB_DETECTAVEL[which(RS22_22_23_SE_PESQ_ARB_DETECTAVEL == i), 10] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                                                       filter(Municipio_Residencia == i,
                                                                                                              SE == 39,
                                                                                                              Resultado == "Detectável") %>%
                                                                                                       count()
  )
  
  RS22_22_23_SE_PESQ_ARB_DETECTAVEL[which(RS22_22_23_SE_PESQ_ARB_DETECTAVEL == i), 11] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                                                       filter(Municipio_Residencia == i,
                                                                                                              SE == 40,
                                                                                                              Resultado == "Detectável") %>%
                                                                                                       count() 
  )
  
  RS22_22_23_SE_PESQ_ARB_DETECTAVEL[which(RS22_22_23_SE_PESQ_ARB_DETECTAVEL == i), 12] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                                                       filter(Municipio_Residencia == i,
                                                                                                              SE == 41,
                                                                                                              Resultado == "Detectável") %>%
                                                                                                       count() 
  )
  
  RS22_22_23_SE_PESQ_ARB_DETECTAVEL[which(RS22_22_23_SE_PESQ_ARB_DETECTAVEL == i), 13] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                                                       filter(Municipio_Residencia == i,
                                                                                                              SE == 42,
                                                                                                              Resultado == "Detectável") %>%
                                                                                                       count() 
  )
  
  RS22_22_23_SE_PESQ_ARB_DETECTAVEL[which(RS22_22_23_SE_PESQ_ARB_DETECTAVEL == i), 14] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                                                       filter(Municipio_Residencia == i,
                                                                                                              SE == 43,
                                                                                                              Resultado == "Detectável") %>% 
                                                                                                       count() 
  )
  
  RS22_22_23_SE_PESQ_ARB_DETECTAVEL[which(RS22_22_23_SE_PESQ_ARB_DETECTAVEL == i), 15] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                                                       filter(Municipio_Residencia == i,
                                                                                                              SE == 44,
                                                                                                              Resultado == "Detectável") %>%
                                                                                                       count() 
  )
  
  RS22_22_23_SE_PESQ_ARB_DETECTAVEL[which(RS22_22_23_SE_PESQ_ARB_DETECTAVEL == i), 16] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                                                       filter(Municipio_Residencia == i,
                                                                                                              SE == 45,
                                                                                                              Resultado == "Detectável") %>%
                                                                                                       count() 
  )
  
  RS22_22_23_SE_PESQ_ARB_DETECTAVEL[which(RS22_22_23_SE_PESQ_ARB_DETECTAVEL == i), 17] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                                                       filter(Municipio_Residencia == i,
                                                                                                              SE == 46,
                                                                                                              Resultado == "Detectável") %>%
                                                                                                       count()
  )
  
  RS22_22_23_SE_PESQ_ARB_DETECTAVEL[which(RS22_22_23_SE_PESQ_ARB_DETECTAVEL == i), 18] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                                                       filter(Municipio_Residencia == i,
                                                                                                              SE == 47,
                                                                                                              Resultado == "Detectável") %>%       
                                                                                                       count() 
  )
  
  RS22_22_23_SE_PESQ_ARB_DETECTAVEL[which(RS22_22_23_SE_PESQ_ARB_DETECTAVEL == i), 19] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                                                       filter(Municipio_Residencia == i,
                                                                                                              SE == 48,
                                                                                                              Resultado == "Detectável") %>%     
                                                                                                       count() 
  )
  
  RS22_22_23_SE_PESQ_ARB_DETECTAVEL[which(RS22_22_23_SE_PESQ_ARB_DETECTAVEL == i), 20] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                                                       filter(Municipio_Residencia == i,
                                                                                                              SE == 49,
                                                                                                              Resultado == "Detectável") %>%
                                                                                                       count() 
  )
  
  RS22_22_23_SE_PESQ_ARB_DETECTAVEL[which(RS22_22_23_SE_PESQ_ARB_DETECTAVEL == i),  21] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                                                        filter(Municipio_Residencia == i,
                                                                                                               SE == 50,
                                                                                                               Resultado == "Detectável") %>%
                                                                                                        count() 
  )
  
  RS22_22_23_SE_PESQ_ARB_DETECTAVEL[which(RS22_22_23_SE_PESQ_ARB_DETECTAVEL == i), 22] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                                                       filter(Municipio_Residencia == i,
                                                                                                              SE == 51,
                                                                                                              Resultado == "Detectável") %>%
                                                                                                       count() 
  )
  
  RS22_22_23_SE_PESQ_ARB_DETECTAVEL[which(RS22_22_23_SE_PESQ_ARB_DETECTAVEL == i), 23] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                                                       filter(Municipio_Residencia == i,
                                                                                                              SE == 52,
                                                                                                              Resultado == "Detectável") %>%
                                                                                                       count() 
  )
  
  RS22_22_23_SE_PESQ_ARB_DETECTAVEL[which(RS22_22_23_SE_PESQ_ARB_DETECTAVEL == i), 24] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                                                       filter(Municipio_Residencia == i,
                                                                                                              SE == 53,
                                                                                                              Resultado == "Detectável") %>%
                                                                                                       count()
  )
  
  RS22_22_23_SE_PESQ_ARB_DETECTAVEL[which(RS22_22_23_SE_PESQ_ARB_DETECTAVEL == i), 25] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                                                       filter(Municipio_Residencia == i,
                                                                                                              SE == 1,
                                                                                                              Resultado == "Detectável") %>%
                                                                                                       count() 
  )
  
  RS22_22_23_SE_PESQ_ARB_DETECTAVEL[which(RS22_22_23_SE_PESQ_ARB_DETECTAVEL == i), 26] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                                                       filter(Municipio_Residencia == i,
                                                                                                              SE == 2,
                                                                                                              Resultado == "Detectável") %>%
                                                                                                       count()
  )
  
  RS22_22_23_SE_PESQ_ARB_DETECTAVEL[which(RS22_22_23_SE_PESQ_ARB_DETECTAVEL == i), 27] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                                                       filter(Municipio_Residencia == i,
                                                                                                              SE == 3,
                                                                                                              Resultado == "Detectável") %>%
                                                                                                       count() 
  )
  
  RS22_22_23_SE_PESQ_ARB_DETECTAVEL[which(RS22_22_23_SE_PESQ_ARB_DETECTAVEL == i), 28] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                                                       filter(Municipio_Residencia == i,
                                                                                                              SE == 4,
                                                                                                              Resultado == "Detectável") %>%
                                                                                                       count() 
  )
  
  RS22_22_23_SE_PESQ_ARB_DETECTAVEL[which(RS22_22_23_SE_PESQ_ARB_DETECTAVEL == i), 29] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                                                       filter(Municipio_Residencia == i,
                                                                                                              SE == 5,
                                                                                                              Resultado == "Detectável") %>%
                                                                                                       count() 
  )
  
  RS22_22_23_SE_PESQ_ARB_DETECTAVEL[which(RS22_22_23_SE_PESQ_ARB_DETECTAVEL == i), 30] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                                                       filter(Municipio_Residencia == i,
                                                                                                              SE == 6,
                                                                                                              Resultado == "Detectável") %>%
                                                                                                       count() 
  )
  
  RS22_22_23_SE_PESQ_ARB_DETECTAVEL[which(RS22_22_23_SE_PESQ_ARB_DETECTAVEL == i), 31] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                                                       filter(Municipio_Residencia == i,
                                                                                                              SE == 7,
                                                                                                              Resultado == "Detectável") %>% 
                                                                                                       count()
  )
  
  RS22_22_23_SE_PESQ_ARB_DETECTAVEL[which(RS22_22_23_SE_PESQ_ARB_DETECTAVEL == i), 32] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                                                       filter(Municipio_Residencia == i,
                                                                                                              SE == 8,
                                                                                                              Resultado == "Detectável") %>%
                                                                                                       count()
  )
  
  RS22_22_23_SE_PESQ_ARB_DETECTAVEL[which(RS22_22_23_SE_PESQ_ARB_DETECTAVEL == i), 33] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                                                       filter(Municipio_Residencia == i,
                                                                                                              SE == 9,
                                                                                                              Resultado == "Detectável") %>%
                                                                                                       count() 
  )
  
  RS22_22_23_SE_PESQ_ARB_DETECTAVEL[which(RS22_22_23_SE_PESQ_ARB_DETECTAVEL == i), 34] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                                                       filter(Municipio_Residencia == i,
                                                                                                              SE == 10,
                                                                                                              Resultado == "Detectável") %>% 
                                                                                                       count() 
  )
  
  RS22_22_23_SE_PESQ_ARB_DETECTAVEL[which(RS22_22_23_SE_PESQ_ARB_DETECTAVEL == i), 35] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                                                       filter(Municipio_Residencia == i,
                                                                                                              SE == 11,
                                                                                                              Resultado == "Detectável") %>% 
                                                                                                       count() 
  )
  
  RS22_22_23_SE_PESQ_ARB_DETECTAVEL[which(RS22_22_23_SE_PESQ_ARB_DETECTAVEL == i), 36] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                                                       filter(Municipio_Residencia == i,
                                                                                                              SE == 12,
                                                                                                              Resultado == "Detectável") %>%
                                                                                                       count() 
  )
  
  RS22_22_23_SE_PESQ_ARB_DETECTAVEL[which(RS22_22_23_SE_PESQ_ARB_DETECTAVEL == i), 37] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                                                       filter(Municipio_Residencia == i,
                                                                                                              SE == 13,
                                                                                                              Resultado == "Detectável") %>%
                                                                                                       count() 
  )
  
  RS22_22_23_SE_PESQ_ARB_DETECTAVEL[which(RS22_22_23_SE_PESQ_ARB_DETECTAVEL == i), 38] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                                                       filter(Municipio_Residencia == i,
                                                                                                              SE == 14,
                                                                                                              Resultado == "Detectável") %>% 
                                                                                                       count() 
  )
  
  RS22_22_23_SE_PESQ_ARB_DETECTAVEL[which(RS22_22_23_SE_PESQ_ARB_DETECTAVEL == i), 39] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                                                       filter(Municipio_Residencia == i,
                                                                                                              SE == 15,
                                                                                                              Resultado == "Detectável") %>%
                                                                                                       count()
  )
  
  RS22_22_23_SE_PESQ_ARB_DETECTAVEL[which(RS22_22_23_SE_PESQ_ARB_DETECTAVEL == i), 40] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                                                       filter(Municipio_Residencia == i,
                                                                                                              SE == 16,
                                                                                                              Resultado == "Detectável") %>%
                                                                                                       count() 
  )
  
  RS22_22_23_SE_PESQ_ARB_DETECTAVEL[which(RS22_22_23_SE_PESQ_ARB_DETECTAVEL == i), 41] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                                                       filter(Municipio_Residencia == i,
                                                                                                              SE == 17,
                                                                                                              Resultado == "Detectável") %>%
                                                                                                       count() 
  )
  
  RS22_22_23_SE_PESQ_ARB_DETECTAVEL[which(RS22_22_23_SE_PESQ_ARB_DETECTAVEL == i), 42] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                                                       filter(Municipio_Residencia == i,
                                                                                                              SE == 18,
                                                                                                              Resultado == "Detectável") %>%
                                                                                                       count() 
  )
  
  RS22_22_23_SE_PESQ_ARB_DETECTAVEL[which(RS22_22_23_SE_PESQ_ARB_DETECTAVEL == i), 43] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                                                       filter(Municipio_Residencia == i,
                                                                                                              SE == 19,
                                                                                                              Resultado == "Detectável") %>%
                                                                                                       count()
  )
  
  RS22_22_23_SE_PESQ_ARB_DETECTAVEL[which(RS22_22_23_SE_PESQ_ARB_DETECTAVEL == i), 44] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                                                       filter(Municipio_Residencia == i,
                                                                                                              SE == 20,
                                                                                                              Resultado == "Detectável") %>%
                                                                                                       count()
  )
  
  RS22_22_23_SE_PESQ_ARB_DETECTAVEL[which(RS22_22_23_SE_PESQ_ARB_DETECTAVEL == i), 45] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                                                       filter(Municipio_Residencia == i,
                                                                                                              SE == 21,
                                                                                                              Resultado == "Detectável") %>%
                                                                                                       count() 
  )
  
  RS22_22_23_SE_PESQ_ARB_DETECTAVEL[which(RS22_22_23_SE_PESQ_ARB_DETECTAVEL == i), 46] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                                                       filter(Municipio_Residencia == i,
                                                                                                              SE == 22,
                                                                                                              Resultado == "Detectável") %>%
                                                                                                       count()
  )
  
  RS22_22_23_SE_PESQ_ARB_DETECTAVEL[which(RS22_22_23_SE_PESQ_ARB_DETECTAVEL == i), 47] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                                                       filter(Municipio_Residencia == i,
                                                                                                              SE == 23,
                                                                                                              Resultado == "Detectável") %>%
                                                                                                       count() 
  )
  
  RS22_22_23_SE_PESQ_ARB_DETECTAVEL[which(RS22_22_23_SE_PESQ_ARB_DETECTAVEL == i), 48] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                                                       filter(Municipio_Residencia == i,
                                                                                                              SE == 24,
                                                                                                              Resultado == "Detectável") %>%
                                                                                                       count() 
  )
  
  RS22_22_23_SE_PESQ_ARB_DETECTAVEL[which(RS22_22_23_SE_PESQ_ARB_DETECTAVEL == i), 49] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                                                       filter(Municipio_Residencia == i,
                                                                                                              SE == 25,
                                                                                                              Resultado == "Detectável") %>%
                                                                                                       count() 
  )
  
  RS22_22_23_SE_PESQ_ARB_DETECTAVEL[which(RS22_22_23_SE_PESQ_ARB_DETECTAVEL == i), 50] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                                                       filter(Municipio_Residencia == i,
                                                                                                              SE == 26,
                                                                                                              Resultado == "Detectável") %>% 
                                                                                                       count()
  )
  
  RS22_22_23_SE_PESQ_ARB_DETECTAVEL[which(RS22_22_23_SE_PESQ_ARB_DETECTAVEL == i), 51] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                                                       filter(Municipio_Residencia == i,
                                                                                                              SE == 27,
                                                                                                              Resultado == "Detectável") %>% 
                                                                                                       count() 
  )
  
  RS22_22_23_SE_PESQ_ARB_DETECTAVEL[which(RS22_22_23_SE_PESQ_ARB_DETECTAVEL == i), 52] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                                                       filter(Municipio_Residencia == i,
                                                                                                              SE == 28,
                                                                                                              Resultado == "Detectável") %>%
                                                                                                       count() 
  )
  
  RS22_22_23_SE_PESQ_ARB_DETECTAVEL[which(RS22_22_23_SE_PESQ_ARB_DETECTAVEL == i), 53] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                                                       filter(Municipio_Residencia == i,
                                                                                                              SE == 29,
                                                                                                              Resultado == "Detectável") %>%
                                                                                                       count() 
  )
  
  RS22_22_23_SE_PESQ_ARB_DETECTAVEL[which(RS22_22_23_SE_PESQ_ARB_DETECTAVEL == i), 54] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                                                       filter(Municipio_Residencia == i,
                                                                                                              SE == 30,
                                                                                                              Resultado == "Detectável") %>%
                                                                                                       count() 
  )
}

RS22_22_23_SE_PESQ_ARB_DETECTAVEL[17,2:54] <- apply(RS22_22_23_SE_PESQ_ARB_DETECTAVEL[,2:54], 2, sum)

RS22_22_23_SE_PESQ_ARB_DETECTAVEL[17,1] <- "Total"

############################################################################################################################################
#####################  Realizando a contagem de exames por SE UNIDADE SENTINELA #############################################################

RS22_22_23_SE_US <- matrix(data = NA, 
                           nrow = 16, 
                           ncol = 54)

RS22_22_23_SE_US <- as.data.frame(RS22_22_23_SE_US)

colnames(RS22_22_23_SE_US)[1] <- "Município" 

RS22_22_23_SE_US[,1] <- BASE_IBGE[which(BASE_IBGE$RS == 22), 3]

colnames (RS22_22_23_SE_US)[2:24] <- c(31:53)

colnames (RS22_22_23_SE_US)[25:54] <- c(1:30)

for (i in BASE_IBGE[(which(BASE_IBGE$RS == 22)), 3]){
  
  RS22_22_23_SE_US[which(RS22_22_23_SE_US == i), 2] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                    filter(Municipio_Residencia == i,
                                                                           SE == 31,
                                                                           Status_Exame == "Resultado Liberado" |
                                                                             Status_Exame == "Automação em Processo" |
                                                                             Status_Exame == "Disponivel para Encaminhar",
                                                                           Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA") %>%
                                                                    count()
  )
  
  RS22_22_23_SE_US[which(RS22_22_23_SE_US == i), 3] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                    filter(Municipio_Residencia == i,
                                                                           SE == 32,
                                                                           Status_Exame == "Resultado Liberado" |
                                                                             Status_Exame == "Automação em Processo" |
                                                                             Status_Exame == "Disponivel para Encaminhar",
                                                                           Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA") %>% 
                                                                    count()
  )
  
  RS22_22_23_SE_US[which(RS22_22_23_SE_US == i), 4] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                    filter(Municipio_Residencia == i,
                                                                           SE == 33,
                                                                           Status_Exame == "Resultado Liberado" |
                                                                             Status_Exame == "Automação em Processo" |
                                                                             Status_Exame == "Disponivel para Encaminhar",
                                                                           Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA") %>% 
                                                                    count()
  )
  
  RS22_22_23_SE_US[which(RS22_22_23_SE_US == i),5] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                   filter(Municipio_Residencia == i,
                                                                          SE == 34,
                                                                          Status_Exame == "Resultado Liberado" |
                                                                            Status_Exame == "Automação em Processo" |
                                                                            Status_Exame == "Disponivel para Encaminhar",
                                                                          Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA") %>% 
                                                                   count()
  )
  
  RS22_22_23_SE_US[which(RS22_22_23_SE_US == i), 6] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                    filter(Municipio_Residencia == i,
                                                                           SE == 35,
                                                                           Status_Exame == "Resultado Liberado" |
                                                                             Status_Exame == "Automação em Processo" |
                                                                             Status_Exame == "Disponivel para Encaminhar",
                                                                           Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA") %>% 
                                                                    count() 
  )
  
  RS22_22_23_SE_US[which(RS22_22_23_SE_US == i), 7] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                    filter(Municipio_Residencia == i,
                                                                           SE == 36,
                                                                           Status_Exame == "Resultado Liberado" |
                                                                             Status_Exame == "Automação em Processo" |
                                                                             Status_Exame == "Disponivel para Encaminhar",
                                                                           Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA") %>%
                                                                    count()
  )
  
  RS22_22_23_SE_US[which(RS22_22_23_SE_US == i), 8] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                    filter(Municipio_Residencia == i,
                                                                           SE == 37,
                                                                           Status_Exame == "Resultado Liberado" |
                                                                             Status_Exame == "Automação em Processo" |
                                                                             Status_Exame == "Disponivel para Encaminhar",
                                                                           Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA") %>% 
                                                                    count() 
  )
  
  RS22_22_23_SE_US[which(RS22_22_23_SE_US == i), 9] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                    filter(Municipio_Residencia == i,
                                                                           SE == 38,
                                                                           Status_Exame == "Resultado Liberado" |
                                                                             Status_Exame == "Automação em Processo" |
                                                                             Status_Exame == "Disponivel para Encaminhar",
                                                                           Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA") %>% 
                                                                    count() 
  )
  
  RS22_22_23_SE_US[which(RS22_22_23_SE_US == i), 10] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                     filter(Municipio_Residencia == i,
                                                                            SE == 39,
                                                                            Status_Exame == "Resultado Liberado" |
                                                                              Status_Exame == "Automação em Processo" |
                                                                              Status_Exame == "Disponivel para Encaminhar",
                                                                            Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA") %>%
                                                                     count()
  )
  
  RS22_22_23_SE_US[which(RS22_22_23_SE_US == i), 11] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                     filter(Municipio_Residencia == i,
                                                                            SE == 40,
                                                                            Status_Exame == "Resultado Liberado" |
                                                                              Status_Exame == "Automação em Processo" |
                                                                              Status_Exame == "Disponivel para Encaminhar",
                                                                            Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA") %>%
                                                                     count() 
  )
  
  RS22_22_23_SE_US[which(RS22_22_23_SE_US == i), 12] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                     filter(Municipio_Residencia == i,
                                                                            SE == 41,
                                                                            Status_Exame == "Resultado Liberado" |
                                                                              Status_Exame == "Automação em Processo" |
                                                                              Status_Exame == "Disponivel para Encaminhar",
                                                                            Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA") %>%
                                                                     count() 
  )
  
  RS22_22_23_SE_US[which(RS22_22_23_SE_US == i), 13] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                     filter(Municipio_Residencia == i,
                                                                            SE == 42,
                                                                            Status_Exame == "Resultado Liberado" |
                                                                              Status_Exame == "Automação em Processo" |
                                                                              Status_Exame == "Disponivel para Encaminhar",
                                                                            Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA") %>%
                                                                     count() 
  )
  
  RS22_22_23_SE_US[which(RS22_22_23_SE_US == i), 14] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                     filter(Municipio_Residencia == i,
                                                                            SE == 43,
                                                                            Status_Exame == "Resultado Liberado" |
                                                                              Status_Exame == "Automação em Processo" |
                                                                              Status_Exame == "Disponivel para Encaminhar",
                                                                            Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA") %>% 
                                                                     count() 
  )
  
  RS22_22_23_SE_US[which(RS22_22_23_SE_US == i), 15] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                     filter(Municipio_Residencia == i,
                                                                            SE == 44,
                                                                            Status_Exame == "Resultado Liberado" |
                                                                              Status_Exame == "Automação em Processo" |
                                                                              Status_Exame == "Disponivel para Encaminhar",
                                                                            Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA") %>%
                                                                     count() 
  )
  
  RS22_22_23_SE_US[which(RS22_22_23_SE_US == i), 16] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                     filter(Municipio_Residencia == i,
                                                                            SE == 45,
                                                                            Status_Exame == "Resultado Liberado" |
                                                                              Status_Exame == "Automação em Processo" |
                                                                              Status_Exame == "Disponivel para Encaminhar",
                                                                            Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA") %>%
                                                                     count() 
  )
  
  RS22_22_23_SE_US[which(RS22_22_23_SE_US == i), 17] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                     filter(Municipio_Residencia == i,
                                                                            SE == 46,
                                                                            Status_Exame == "Resultado Liberado" |
                                                                              Status_Exame == "Automação em Processo" |
                                                                              Status_Exame == "Disponivel para Encaminhar",
                                                                            Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA") %>%
                                                                     count()
  )
  
  RS22_22_23_SE_US[which(RS22_22_23_SE_US == i), 18] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                     filter(Municipio_Residencia == i,
                                                                            SE == 47,
                                                                            Status_Exame == "Resultado Liberado" |
                                                                              Status_Exame == "Automação em Processo" |
                                                                              Status_Exame == "Disponivel para Encaminhar",
                                                                            Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA") %>%       
                                                                     count() 
  )
  
  RS22_22_23_SE_US[which(RS22_22_23_SE_US == i), 19] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                     filter(Municipio_Residencia == i,
                                                                            SE == 48,
                                                                            Status_Exame == "Resultado Liberado" |
                                                                              Status_Exame == "Automação em Processo" |
                                                                              Status_Exame == "Disponivel para Encaminhar",
                                                                            Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA") %>%     
                                                                     count() 
  )
  
  RS22_22_23_SE_US[which(RS22_22_23_SE_US == i), 20] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                     filter(Municipio_Residencia == i,
                                                                            SE == 49,
                                                                            Status_Exame == "Resultado Liberado" |
                                                                              Status_Exame == "Automação em Processo" |
                                                                              Status_Exame == "Disponivel para Encaminhar",
                                                                            Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA") %>%
                                                                     count() 
  )
  
  RS22_22_23_SE_US[which(RS22_22_23_SE_US == i),  21] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                      filter(Municipio_Residencia == i,
                                                                             SE == 50,
                                                                             Status_Exame == "Resultado Liberado" |
                                                                               Status_Exame == "Automação em Processo" |
                                                                               Status_Exame == "Disponivel para Encaminhar",
                                                                             Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA") %>%
                                                                      count() 
  )
  
  RS22_22_23_SE_US[which(RS22_22_23_SE_US == i), 22] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                     filter(Municipio_Residencia == i,
                                                                            SE == 51,
                                                                            Status_Exame == "Resultado Liberado" |
                                                                              Status_Exame == "Automação em Processo" |
                                                                              Status_Exame == "Disponivel para Encaminhar",
                                                                            Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA") %>%
                                                                     count() 
  )
  
  RS22_22_23_SE_US[which(RS22_22_23_SE_US == i), 23] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                     filter(Municipio_Residencia == i,
                                                                            SE == 52,
                                                                            Status_Exame == "Resultado Liberado" |
                                                                              Status_Exame == "Automação em Processo" |
                                                                              Status_Exame == "Disponivel para Encaminhar",
                                                                            Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA") %>%
                                                                     count() 
  )
  
  RS22_22_23_SE_US[which(RS22_22_23_SE_US == i), 24] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                     filter(Municipio_Residencia == i,
                                                                            SE == 53,
                                                                            Status_Exame == "Resultado Liberado" |
                                                                              Status_Exame == "Automação em Processo" |
                                                                              Status_Exame == "Disponivel para Encaminhar",
                                                                            Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA") %>%
                                                                     count()
  )
  
  RS22_22_23_SE_US[which(RS22_22_23_SE_US == i), 25] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                     filter(Municipio_Residencia == i,
                                                                            SE == 1,
                                                                            Status_Exame == "Resultado Liberado" |
                                                                              Status_Exame == "Automação em Processo" |
                                                                              Status_Exame == "Disponivel para Encaminhar",
                                                                            Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA") %>%
                                                                     count() 
  )
  
  RS22_22_23_SE_US[which(RS22_22_23_SE_US == i), 26] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                     filter(Municipio_Residencia == i,
                                                                            SE == 2,
                                                                            Status_Exame == "Resultado Liberado" |
                                                                              Status_Exame == "Automação em Processo" |
                                                                              Status_Exame == "Disponivel para Encaminhar",
                                                                            Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA") %>%
                                                                     count()
  )
  
  RS22_22_23_SE_US[which(RS22_22_23_SE_US == i), 27] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                     filter(Municipio_Residencia == i,
                                                                            SE == 3,
                                                                            Status_Exame == "Resultado Liberado" |
                                                                              Status_Exame == "Automação em Processo" |
                                                                              Status_Exame == "Disponivel para Encaminhar",
                                                                            Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA") %>%
                                                                     count() 
  )
  
  RS22_22_23_SE_US[which(RS22_22_23_SE_US == i), 28] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                     filter(Municipio_Residencia == i,
                                                                            SE == 4,
                                                                            Status_Exame == "Resultado Liberado" |
                                                                              Status_Exame == "Automação em Processo" |
                                                                              Status_Exame == "Disponivel para Encaminhar",
                                                                            Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA") %>%
                                                                     count() 
  )
  
  RS22_22_23_SE_US[which(RS22_22_23_SE_US == i), 29] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                     filter(Municipio_Residencia == i,
                                                                            SE == 5,
                                                                            Status_Exame == "Resultado Liberado" |
                                                                              Status_Exame == "Automação em Processo" |
                                                                              Status_Exame == "Disponivel para Encaminhar",
                                                                            Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA") %>%
                                                                     count() 
  )
  
  RS22_22_23_SE_US[which(RS22_22_23_SE_US == i), 30] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                     filter(Municipio_Residencia == i,
                                                                            SE == 6,
                                                                            Status_Exame == "Resultado Liberado" |
                                                                              Status_Exame == "Automação em Processo" |
                                                                              Status_Exame == "Disponivel para Encaminhar",
                                                                            Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA") %>%
                                                                     count() 
  )
  
  RS22_22_23_SE_US[which(RS22_22_23_SE_US == i), 31] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                     filter(Municipio_Residencia == i,
                                                                            SE == 7,
                                                                            Status_Exame == "Resultado Liberado" |
                                                                              Status_Exame == "Automação em Processo" |
                                                                              Status_Exame == "Disponivel para Encaminhar",
                                                                            Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA") %>% 
                                                                     count()
  )
  
  RS22_22_23_SE_US[which(RS22_22_23_SE_US == i), 32] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                     filter(Municipio_Residencia == i,
                                                                            SE == 8,
                                                                            Status_Exame == "Resultado Liberado" |
                                                                              Status_Exame == "Automação em Processo" |
                                                                              Status_Exame == "Disponivel para Encaminhar",
                                                                            Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA") %>%
                                                                     count()
  )
  
  RS22_22_23_SE_US[which(RS22_22_23_SE_US == i), 33] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                     filter(Municipio_Residencia == i,
                                                                            SE == 9,
                                                                            Status_Exame == "Resultado Liberado" |
                                                                              Status_Exame == "Automação em Processo" |
                                                                              Status_Exame == "Disponivel para Encaminhar",
                                                                            Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA") %>%
                                                                     count() 
  )
  
  RS22_22_23_SE_US[which(RS22_22_23_SE_US == i), 34] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                     filter(Municipio_Residencia == i,
                                                                            SE == 10,
                                                                            Status_Exame == "Resultado Liberado" |
                                                                              Status_Exame == "Automação em Processo" |
                                                                              Status_Exame == "Disponivel para Encaminhar",
                                                                            Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA") %>% 
                                                                     count() 
  )
  
  RS22_22_23_SE_US[which(RS22_22_23_SE_US == i), 35] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                     filter(Municipio_Residencia == i,
                                                                            SE == 11,
                                                                            Status_Exame == "Resultado Liberado" |
                                                                              Status_Exame == "Automação em Processo" |
                                                                              Status_Exame == "Disponivel para Encaminhar",
                                                                            Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA") %>% 
                                                                     count() 
  )
  
  RS22_22_23_SE_US[which(RS22_22_23_SE_US == i), 36] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                     filter(Municipio_Residencia == i,
                                                                            SE == 12,
                                                                            Status_Exame == "Resultado Liberado" |
                                                                              Status_Exame == "Automação em Processo" |
                                                                              Status_Exame == "Disponivel para Encaminhar",
                                                                            Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA") %>%
                                                                     count() 
  )
  
  RS22_22_23_SE_US[which(RS22_22_23_SE_US == i), 37] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                     filter(Municipio_Residencia == i,
                                                                            SE == 13,
                                                                            Status_Exame == "Resultado Liberado" |
                                                                              Status_Exame == "Automação em Processo" |
                                                                              Status_Exame == "Disponivel para Encaminhar",
                                                                            Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA") %>%
                                                                     count() 
  )
  
  RS22_22_23_SE_US[which(RS22_22_23_SE_US == i), 38] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                     filter(Municipio_Residencia == i,
                                                                            SE == 14,
                                                                            Status_Exame == "Resultado Liberado" |
                                                                              Status_Exame == "Automação em Processo" |
                                                                              Status_Exame == "Disponivel para Encaminhar",
                                                                            Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA") %>% 
                                                                     count() 
  )
  
  RS22_22_23_SE_US[which(RS22_22_23_SE_US == i), 39] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                     filter(Municipio_Residencia == i,
                                                                            SE == 15,
                                                                            Status_Exame == "Resultado Liberado" |
                                                                              Status_Exame == "Automação em Processo" |
                                                                              Status_Exame == "Disponivel para Encaminhar",
                                                                            Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA") %>%
                                                                     count()
  )
  
  RS22_22_23_SE_US[which(RS22_22_23_SE_US == i), 40] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                     filter(Municipio_Residencia == i,
                                                                            SE == 16,
                                                                            Status_Exame == "Resultado Liberado" |
                                                                              Status_Exame == "Automação em Processo" |
                                                                              Status_Exame == "Disponivel para Encaminhar",
                                                                            Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA") %>%
                                                                     count() 
  )
  
  RS22_22_23_SE_US[which(RS22_22_23_SE_US == i), 41] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                     filter(Municipio_Residencia == i,
                                                                            SE == 17,
                                                                            Status_Exame == "Resultado Liberado" |
                                                                              Status_Exame == "Automação em Processo" |
                                                                              Status_Exame == "Disponivel para Encaminhar",
                                                                            Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA") %>%
                                                                     count() 
  )
  
  RS22_22_23_SE_US[which(RS22_22_23_SE_US == i), 42] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                     filter(Municipio_Residencia == i,
                                                                            SE == 18,
                                                                            Status_Exame == "Resultado Liberado" |
                                                                              Status_Exame == "Automação em Processo" |
                                                                              Status_Exame == "Disponivel para Encaminhar",
                                                                            Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA") %>%
                                                                     count() 
  )
  
  RS22_22_23_SE_US[which(RS22_22_23_SE_US == i), 43] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                     filter(Municipio_Residencia == i,
                                                                            SE == 19,
                                                                            Status_Exame == "Resultado Liberado" |
                                                                              Status_Exame == "Automação em Processo" |
                                                                              Status_Exame == "Disponivel para Encaminhar",
                                                                            Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA") %>%
                                                                     count()
  )
  
  RS22_22_23_SE_US[which(RS22_22_23_SE_US == i), 44] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                     filter(Municipio_Residencia == i,
                                                                            SE == 20,
                                                                            Status_Exame == "Resultado Liberado" |
                                                                              Status_Exame == "Automação em Processo" |
                                                                              Status_Exame == "Disponivel para Encaminhar",
                                                                            Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA") %>%
                                                                     count()
  )
  
  RS22_22_23_SE_US[which(RS22_22_23_SE_US == i), 45] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                     filter(Municipio_Residencia == i,
                                                                            SE == 21,
                                                                            Status_Exame == "Resultado Liberado" |
                                                                              Status_Exame == "Automação em Processo" |
                                                                              Status_Exame == "Disponivel para Encaminhar",
                                                                            Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA") %>%
                                                                     count() 
  )
  
  RS22_22_23_SE_US[which(RS22_22_23_SE_US == i), 46] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                     filter(Municipio_Residencia == i,
                                                                            SE == 22,
                                                                            Status_Exame == "Resultado Liberado" |
                                                                              Status_Exame == "Automação em Processo" |
                                                                              Status_Exame == "Disponivel para Encaminhar",
                                                                            Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA") %>%
                                                                     count()
  )
  
  RS22_22_23_SE_US[which(RS22_22_23_SE_US == i), 47] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                     filter(Municipio_Residencia == i,
                                                                            SE == 23,
                                                                            Status_Exame == "Resultado Liberado" |
                                                                              Status_Exame == "Automação em Processo" |
                                                                              Status_Exame == "Disponivel para Encaminhar",
                                                                            Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA") %>%
                                                                     count() 
  )
  
  RS22_22_23_SE_US[which(RS22_22_23_SE_US == i), 48] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                     filter(Municipio_Residencia == i,
                                                                            SE == 24,
                                                                            Status_Exame == "Resultado Liberado" |
                                                                              Status_Exame == "Automação em Processo" |
                                                                              Status_Exame == "Disponivel para Encaminhar",
                                                                            Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA") %>%
                                                                     count() 
  )
  
  RS22_22_23_SE_US[which(RS22_22_23_SE_US == i), 49] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                     filter(Municipio_Residencia == i,
                                                                            SE == 25,
                                                                            Status_Exame == "Resultado Liberado" |
                                                                              Status_Exame == "Automação em Processo" |
                                                                              Status_Exame == "Disponivel para Encaminhar",
                                                                            Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA") %>%
                                                                     count() 
  )
  
  RS22_22_23_SE_US[which(RS22_22_23_SE_US == i), 50] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                     filter(Municipio_Residencia == i,
                                                                            SE == 26,
                                                                            Status_Exame == "Resultado Liberado" |
                                                                              Status_Exame == "Automação em Processo" |
                                                                              Status_Exame == "Disponivel para Encaminhar",
                                                                            Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA") %>% 
                                                                     count()
  )
  
  RS22_22_23_SE_US[which(RS22_22_23_SE_US == i), 51] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                     filter(Municipio_Residencia == i,
                                                                            SE == 27,
                                                                            Status_Exame == "Resultado Liberado" |
                                                                              Status_Exame == "Automação em Processo" |
                                                                              Status_Exame == "Disponivel para Encaminhar",
                                                                            Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA") %>% 
                                                                     count() 
  )
  
  RS22_22_23_SE_US[which(RS22_22_23_SE_US == i), 52] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                     filter(Municipio_Residencia == i,
                                                                            SE == 28,
                                                                            Status_Exame == "Resultado Liberado" |
                                                                              Status_Exame == "Automação em Processo" |
                                                                              Status_Exame == "Disponivel para Encaminhar",
                                                                            Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA") %>%
                                                                     count() 
  )
  
  RS22_22_23_SE_US[which(RS22_22_23_SE_US == i), 53] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                     filter(Municipio_Residencia == i,
                                                                            SE == 29,
                                                                            Status_Exame == "Resultado Liberado" |
                                                                              Status_Exame == "Automação em Processo" |
                                                                              Status_Exame == "Disponivel para Encaminhar",
                                                                            Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA") %>%
                                                                     count() 
  )
  
  RS22_22_23_SE_US[which(RS22_22_23_SE_US == i), 54] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                     filter(Municipio_Residencia == i,
                                                                            SE == 30,
                                                                            Status_Exame == "Resultado Liberado" |
                                                                              Status_Exame == "Automação em Processo" |
                                                                              Status_Exame == "Disponivel para Encaminhar",
                                                                            Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA") %>%
                                                                     count() 
  )
}

RS22_22_23_SE_US[17,2:54] <- apply(RS22_22_23_SE_US[,2:54], 2, sum)

RS22_22_23_SE_US[17,1] <- "Total"


############################################################################################################################################
#####################  Realizando a contagem de exames por SE UNIDADE SENTINELA DETECTÁVEL #############################################################

RS22_22_23_SE_US_DETECTAVEL <- matrix(data = NA, 
                                      nrow = 16, 
                                      ncol = 54)

RS22_22_23_SE_US_DETECTAVEL <- as.data.frame(RS22_22_23_SE_US_DETECTAVEL)

colnames(RS22_22_23_SE_US_DETECTAVEL)[1] <- "Município" 

RS22_22_23_SE_US_DETECTAVEL[,1] <- BASE_IBGE[which(BASE_IBGE$RS == 22), 3]

colnames (RS22_22_23_SE_US_DETECTAVEL)[2:24] <- c(31:53)

colnames (RS22_22_23_SE_US_DETECTAVEL)[25:54] <- c(1:30)

for (i in BASE_IBGE[(which(BASE_IBGE$RS == 22)), 3]){
  
  RS22_22_23_SE_US_DETECTAVEL[which(RS22_22_23_SE_US_DETECTAVEL == i), 2] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                                          filter(Municipio_Residencia == i,
                                                                                                 SE == 31,
                                                                                                 Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA",
                                                                                                 Resultado == "Detectável") %>%
                                                                                          count()
  )
  
  RS22_22_23_SE_US_DETECTAVEL[which(RS22_22_23_SE_US_DETECTAVEL == i), 3] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                                          filter(Municipio_Residencia == i,
                                                                                                 SE == 32,
                                                                                                 Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA",
                                                                                                 Resultado == "Detectável") %>% 
                                                                                          count()
  )
  
  RS22_22_23_SE_US_DETECTAVEL[which(RS22_22_23_SE_US_DETECTAVEL == i), 4] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                                          filter(Municipio_Residencia == i,
                                                                                                 SE == 33,
                                                                                                 Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA",
                                                                                                 Resultado == "Detectável") %>% 
                                                                                          count()
  )
  
  RS22_22_23_SE_US_DETECTAVEL[which(RS22_22_23_SE_US_DETECTAVEL == i),5] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                                         filter(Municipio_Residencia == i,
                                                                                                SE == 34,
                                                                                                Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA",
                                                                                                Resultado == "Detectável") %>% 
                                                                                         count()
  )
  
  RS22_22_23_SE_US_DETECTAVEL[which(RS22_22_23_SE_US_DETECTAVEL == i), 6] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                                          filter(Municipio_Residencia == i,
                                                                                                 SE == 35,
                                                                                                 Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA",
                                                                                                 Resultado == "Detectável") %>% 
                                                                                          count() 
  )
  
  RS22_22_23_SE_US_DETECTAVEL[which(RS22_22_23_SE_US_DETECTAVEL == i), 7] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                                          filter(Municipio_Residencia == i,
                                                                                                 SE == 36,
                                                                                                 Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA",
                                                                                                 Resultado == "Detectável") %>%
                                                                                          count()
  )
  
  RS22_22_23_SE_US_DETECTAVEL[which(RS22_22_23_SE_US_DETECTAVEL == i), 8] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                                          filter(Municipio_Residencia == i,
                                                                                                 SE == 37,
                                                                                                 Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA",
                                                                                                 Resultado == "Detectável") %>% 
                                                                                          count() 
  )
  
  RS22_22_23_SE_US_DETECTAVEL[which(RS22_22_23_SE_US_DETECTAVEL == i), 9] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                                          filter(Municipio_Residencia == i,
                                                                                                 SE == 38,
                                                                                                 Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA",
                                                                                                 Resultado == "Detectável") %>% 
                                                                                          count() 
  )
  
  RS22_22_23_SE_US_DETECTAVEL[which(RS22_22_23_SE_US_DETECTAVEL == i), 10] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                                           filter(Municipio_Residencia == i,
                                                                                                  SE == 39,
                                                                                                  Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA",
                                                                                                  Resultado == "Detectável") %>%
                                                                                           count()
  )
  
  RS22_22_23_SE_US_DETECTAVEL[which(RS22_22_23_SE_US_DETECTAVEL == i), 11] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                                           filter(Municipio_Residencia == i,
                                                                                                  SE == 40,
                                                                                                  Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA",
                                                                                                  Resultado == "Detectável") %>%
                                                                                           count() 
  )
  
  RS22_22_23_SE_US_DETECTAVEL[which(RS22_22_23_SE_US_DETECTAVEL == i), 12] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                                           filter(Municipio_Residencia == i,
                                                                                                  SE == 41,
                                                                                                  Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA",
                                                                                                  Resultado == "Detectável") %>%
                                                                                           count() 
  )
  
  RS22_22_23_SE_US_DETECTAVEL[which(RS22_22_23_SE_US_DETECTAVEL == i), 13] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                                           filter(Municipio_Residencia == i,
                                                                                                  SE == 42,
                                                                                                  Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA",
                                                                                                  Resultado == "Detectável") %>%
                                                                                           count() 
  )
  
  RS22_22_23_SE_US_DETECTAVEL[which(RS22_22_23_SE_US_DETECTAVEL == i), 14] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                                           filter(Municipio_Residencia == i,
                                                                                                  SE == 43,
                                                                                                  Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA",
                                                                                                  Resultado == "Detectável") %>% 
                                                                                           count() 
  )
  
  RS22_22_23_SE_US_DETECTAVEL[which(RS22_22_23_SE_US_DETECTAVEL == i), 15] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                                           filter(Municipio_Residencia == i,
                                                                                                  SE == 44,
                                                                                                  Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA",
                                                                                                  Resultado == "Detectável") %>%
                                                                                           count() 
  )
  
  RS22_22_23_SE_US_DETECTAVEL[which(RS22_22_23_SE_US_DETECTAVEL == i), 16] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                                           filter(Municipio_Residencia == i,
                                                                                                  SE == 45,
                                                                                                  Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA",
                                                                                                  Resultado == "Detectável") %>%
                                                                                           count() 
  )
  
  RS22_22_23_SE_US_DETECTAVEL[which(RS22_22_23_SE_US_DETECTAVEL == i), 17] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                                           filter(Municipio_Residencia == i,
                                                                                                  SE == 46,
                                                                                                  Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA",
                                                                                                  Resultado == "Detectável") %>%
                                                                                           count()
  )
  
  RS22_22_23_SE_US_DETECTAVEL[which(RS22_22_23_SE_US_DETECTAVEL == i), 18] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                                           filter(Municipio_Residencia == i,
                                                                                                  SE == 47,
                                                                                                  Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA",
                                                                                                  Resultado == "Detectável") %>%       
                                                                                           count() 
  )
  
  RS22_22_23_SE_US_DETECTAVEL[which(RS22_22_23_SE_US_DETECTAVEL == i), 19] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                                           filter(Municipio_Residencia == i,
                                                                                                  SE == 48,
                                                                                                  Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA",
                                                                                                  Resultado == "Detectável") %>%     
                                                                                           count() 
  )
  
  RS22_22_23_SE_US_DETECTAVEL[which(RS22_22_23_SE_US_DETECTAVEL == i), 20] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                                           filter(Municipio_Residencia == i,
                                                                                                  SE == 49,
                                                                                                  Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA",
                                                                                                  Resultado == "Detectável") %>%
                                                                                           count() 
  )
  
  RS22_22_23_SE_US_DETECTAVEL[which(RS22_22_23_SE_US_DETECTAVEL == i),  21] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                                            filter(Municipio_Residencia == i,
                                                                                                   SE == 50,
                                                                                                   Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA",
                                                                                                   Resultado == "Detectável") %>%
                                                                                            count() 
  )
  
  RS22_22_23_SE_US_DETECTAVEL[which(RS22_22_23_SE_US_DETECTAVEL == i), 22] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                                           filter(Municipio_Residencia == i,
                                                                                                  SE == 51,
                                                                                                  Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA",
                                                                                                  Resultado == "Detectável") %>%
                                                                                           count() 
  )
  
  RS22_22_23_SE_US_DETECTAVEL[which(RS22_22_23_SE_US_DETECTAVEL == i), 23] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                                           filter(Municipio_Residencia == i,
                                                                                                  SE == 52,
                                                                                                  Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA",
                                                                                                  Resultado == "Detectável") %>%
                                                                                           count() 
  )
  
  RS22_22_23_SE_US_DETECTAVEL[which(RS22_22_23_SE_US_DETECTAVEL == i), 24] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                                           filter(Municipio_Residencia == i,
                                                                                                  SE == 53,
                                                                                                  Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA",
                                                                                                  Resultado == "Detectável") %>%
                                                                                           count()
  )
  
  RS22_22_23_SE_US_DETECTAVEL[which(RS22_22_23_SE_US_DETECTAVEL == i), 25] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                                           filter(Municipio_Residencia == i,
                                                                                                  SE == 1,
                                                                                                  Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA",
                                                                                                  Resultado == "Detectável") %>%
                                                                                           count() 
  )
  
  RS22_22_23_SE_US_DETECTAVEL[which(RS22_22_23_SE_US_DETECTAVEL == i), 26] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                                           filter(Municipio_Residencia == i,
                                                                                                  SE == 2,
                                                                                                  Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA",
                                                                                                  Resultado == "Detectável") %>%
                                                                                           count()
  )
  
  RS22_22_23_SE_US_DETECTAVEL[which(RS22_22_23_SE_US_DETECTAVEL == i), 27] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                                           filter(Municipio_Residencia == i,
                                                                                                  SE == 3,
                                                                                                  Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA",
                                                                                                  Resultado == "Detectável") %>%
                                                                                           count() 
  )
  
  RS22_22_23_SE_US_DETECTAVEL[which(RS22_22_23_SE_US_DETECTAVEL == i), 28] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                                           filter(Municipio_Residencia == i,
                                                                                                  SE == 4,
                                                                                                  Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA",
                                                                                                  Resultado == "Detectável") %>%
                                                                                           count() 
  )
  
  RS22_22_23_SE_US_DETECTAVEL[which(RS22_22_23_SE_US_DETECTAVEL == i), 29] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                                           filter(Municipio_Residencia == i,
                                                                                                  SE == 5,
                                                                                                  Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA",
                                                                                                  Resultado == "Detectável") %>%
                                                                                           count() 
  )
  
  RS22_22_23_SE_US_DETECTAVEL[which(RS22_22_23_SE_US_DETECTAVEL == i), 30] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                                           filter(Municipio_Residencia == i,
                                                                                                  SE == 6,
                                                                                                  Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA",
                                                                                                  Resultado == "Detectável") %>%
                                                                                           count() 
  )
  
  RS22_22_23_SE_US_DETECTAVEL[which(RS22_22_23_SE_US_DETECTAVEL == i), 31] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                                           filter(Municipio_Residencia == i,
                                                                                                  SE == 7,
                                                                                                  Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA",
                                                                                                  Resultado == "Detectável") %>% 
                                                                                           count()
  )
  
  RS22_22_23_SE_US_DETECTAVEL[which(RS22_22_23_SE_US_DETECTAVEL == i), 32] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                                           filter(Municipio_Residencia == i,
                                                                                                  SE == 8,
                                                                                                  Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA",
                                                                                                  Resultado == "Detectável") %>%
                                                                                           count()
  )
  
  RS22_22_23_SE_US_DETECTAVEL[which(RS22_22_23_SE_US_DETECTAVEL == i), 33] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                                           filter(Municipio_Residencia == i,
                                                                                                  SE == 9,
                                                                                                  Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA",
                                                                                                  Resultado == "Detectável") %>%
                                                                                           count() 
  )
  
  RS22_22_23_SE_US_DETECTAVEL[which(RS22_22_23_SE_US_DETECTAVEL == i), 34] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                                           filter(Municipio_Residencia == i,
                                                                                                  SE == 10,
                                                                                                  Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA",
                                                                                                  Resultado == "Detectável") %>% 
                                                                                           count() 
  )
  
  RS22_22_23_SE_US_DETECTAVEL[which(RS22_22_23_SE_US_DETECTAVEL == i), 35] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                                           filter(Municipio_Residencia == i,
                                                                                                  SE == 11,
                                                                                                  Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA",
                                                                                                  Resultado == "Detectável") %>% 
                                                                                           count() 
  )
  
  RS22_22_23_SE_US_DETECTAVEL[which(RS22_22_23_SE_US_DETECTAVEL == i), 36] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                                           filter(Municipio_Residencia == i,
                                                                                                  SE == 12,
                                                                                                  Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA",
                                                                                                  Resultado == "Detectável") %>%
                                                                                           count() 
  )
  
  RS22_22_23_SE_US_DETECTAVEL[which(RS22_22_23_SE_US_DETECTAVEL == i), 37] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                                           filter(Municipio_Residencia == i,
                                                                                                  SE == 13,
                                                                                                  Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA",
                                                                                                  Resultado == "Detectável") %>%
                                                                                           count() 
  )
  
  RS22_22_23_SE_US_DETECTAVEL[which(RS22_22_23_SE_US_DETECTAVEL == i), 38] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                                           filter(Municipio_Residencia == i,
                                                                                                  SE == 14,
                                                                                                  Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA",
                                                                                                  Resultado == "Detectável") %>% 
                                                                                           count() 
  )
  
  RS22_22_23_SE_US_DETECTAVEL[which(RS22_22_23_SE_US_DETECTAVEL == i), 39] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                                           filter(Municipio_Residencia == i,
                                                                                                  SE == 15,
                                                                                                  Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA",
                                                                                                  Resultado == "Detectável") %>%
                                                                                           count()
  )
  
  RS22_22_23_SE_US_DETECTAVEL[which(RS22_22_23_SE_US_DETECTAVEL == i), 40] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                                           filter(Municipio_Residencia == i,
                                                                                                  SE == 16,
                                                                                                  Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA",
                                                                                                  Resultado == "Detectável") %>%
                                                                                           count() 
  )
  
  RS22_22_23_SE_US_DETECTAVEL[which(RS22_22_23_SE_US_DETECTAVEL == i), 41] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                                           filter(Municipio_Residencia == i,
                                                                                                  SE == 17,
                                                                                                  Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA",
                                                                                                  Resultado == "Detectável") %>%
                                                                                           count() 
  )
  
  RS22_22_23_SE_US_DETECTAVEL[which(RS22_22_23_SE_US_DETECTAVEL == i), 42] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                                           filter(Municipio_Residencia == i,
                                                                                                  SE == 18,
                                                                                                  Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA",
                                                                                                  Resultado == "Detectável") %>%
                                                                                           count() 
  )
  
  RS22_22_23_SE_US_DETECTAVEL[which(RS22_22_23_SE_US_DETECTAVEL == i), 43] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                                           filter(Municipio_Residencia == i,
                                                                                                  SE == 19,
                                                                                                  Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA",
                                                                                                  Resultado == "Detectável") %>%
                                                                                           count()
  )
  
  RS22_22_23_SE_US_DETECTAVEL[which(RS22_22_23_SE_US_DETECTAVEL == i), 44] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                                           filter(Municipio_Residencia == i,
                                                                                                  SE == 20,
                                                                                                  Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA",
                                                                                                  Resultado == "Detectável") %>%
                                                                                           count()
  )
  
  RS22_22_23_SE_US_DETECTAVEL[which(RS22_22_23_SE_US_DETECTAVEL == i), 45] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                                           filter(Municipio_Residencia == i,
                                                                                                  SE == 21,
                                                                                                  Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA",
                                                                                                  Resultado == "Detectável") %>%
                                                                                           count() 
  )
  
  RS22_22_23_SE_US_DETECTAVEL[which(RS22_22_23_SE_US_DETECTAVEL == i), 46] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                                           filter(Municipio_Residencia == i,
                                                                                                  SE == 22,
                                                                                                  Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA",
                                                                                                  Resultado == "Detectável") %>%
                                                                                           count()
  )
  
  RS22_22_23_SE_US_DETECTAVEL[which(RS22_22_23_SE_US_DETECTAVEL == i), 47] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                                           filter(Municipio_Residencia == i,
                                                                                                  SE == 23,
                                                                                                  Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA",
                                                                                                  Resultado == "Detectável") %>%
                                                                                           count() 
  )
  
  RS22_22_23_SE_US_DETECTAVEL[which(RS22_22_23_SE_US_DETECTAVEL == i), 48] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                                           filter(Municipio_Residencia == i,
                                                                                                  SE == 24,
                                                                                                  Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA",
                                                                                                  Resultado == "Detectável") %>%
                                                                                           count() 
  )
  
  RS22_22_23_SE_US_DETECTAVEL[which(RS22_22_23_SE_US_DETECTAVEL == i), 49] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                                           filter(Municipio_Residencia == i,
                                                                                                  SE == 25,
                                                                                                  Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA",
                                                                                                  Resultado == "Detectável") %>%
                                                                                           count() 
  )
  
  RS22_22_23_SE_US_DETECTAVEL[which(RS22_22_23_SE_US_DETECTAVEL == i), 50] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                                           filter(Municipio_Residencia == i,
                                                                                                  SE == 26,
                                                                                                  Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA",
                                                                                                  Resultado == "Detectável") %>% 
                                                                                           count()
  )
  
  RS22_22_23_SE_US_DETECTAVEL[which(RS22_22_23_SE_US_DETECTAVEL == i), 51] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                                           filter(Municipio_Residencia == i,
                                                                                                  SE == 27,
                                                                                                  Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA",
                                                                                                  Resultado == "Detectável") %>% 
                                                                                           count() 
  )
  
  RS22_22_23_SE_US_DETECTAVEL[which(RS22_22_23_SE_US_DETECTAVEL == i), 52] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                                           filter(Municipio_Residencia == i,
                                                                                                  SE == 28,
                                                                                                  Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA",
                                                                                                  Resultado == "Detectável") %>%
                                                                                           count() 
  )
  
  RS22_22_23_SE_US_DETECTAVEL[which(RS22_22_23_SE_US_DETECTAVEL == i), 53] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>%
                                                                                           filter(Municipio_Residencia == i,
                                                                                                  SE == 29,
                                                                                                  Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA",
                                                                                                  Resultado == "Detectável") %>%
                                                                                           count() 
  )
  
  RS22_22_23_SE_US_DETECTAVEL[which(RS22_22_23_SE_US_DETECTAVEL == i), 54] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO%>% 
                                                                                           filter(Municipio_Residencia == i,
                                                                                                  SE == 30,
                                                                                                  Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA",
                                                                                                  Resultado == "Detectável") %>%
                                                                                           count() 
  )
}

RS22_22_23_SE_US_DETECTAVEL[17,2:54] <- apply(RS22_22_23_SE_US_DETECTAVEL[,2:54], 2, sum)

RS22_22_23_SE_US_DETECTAVEL[17,1] <- "Total"

############################################################################################################################################
#####################  Realizando a contagem de exames por SE SOROLOGIA GERAL  #############################################################

RS22_22_23_SE_SOROLOGIA <- matrix(data = NA, 
                                  nrow = 16, 
                                  ncol = 54)

RS22_22_23_SE_SOROLOGIA <- as.data.frame(RS22_22_23_SE_SOROLOGIA)

colnames(RS22_22_23_SE_SOROLOGIA)[1] <- "Município" 

RS22_22_23_SE_SOROLOGIA[,1] <- BASE_IBGE[which(BASE_IBGE$RS == 22), 3]

colnames (RS22_22_23_SE_SOROLOGIA)[2:24] <- c(31:53)

colnames (RS22_22_23_SE_SOROLOGIA)[25:54] <- c(1:30)

for (i in BASE_IBGE[(which(BASE_IBGE$RS == 22)), 3]){
  
  RS22_22_23_SE_SOROLOGIA[which(RS22_22_23_SE_SOROLOGIA == i), 2] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>%
                                                                                  filter(Municipio_Residencia == i,
                                                                                         SE == 31,
                                                                                         Status_Exame == "Resultado Liberado" |
                                                                                           Status_Exame == "Automação em Processo" |
                                                                                           Status_Exame == "Exame em Análise" |
                                                                                           Status_Exame == "Disponivel para Encaminhar") %>%
                                                                                  count()
  )
  
  RS22_22_23_SE_SOROLOGIA[which(RS22_22_23_SE_SOROLOGIA == i), 3] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>% 
                                                                                  filter(Municipio_Residencia == i,
                                                                                         SE == 32,
                                                                                         Status_Exame == "Resultado Liberado" |
                                                                                           Status_Exame == "Automação em Processo" |
                                                                                           Status_Exame == "Exame em Análise" |
                                                                                           Status_Exame == "Disponivel para Encaminhar") %>% 
                                                                                  count()
  )
  
  RS22_22_23_SE_SOROLOGIA[which(RS22_22_23_SE_SOROLOGIA == i), 4] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>% 
                                                                                  filter(Municipio_Residencia == i,
                                                                                         SE == 33,
                                                                                         Status_Exame == "Resultado Liberado" |
                                                                                           Status_Exame == "Automação em Processo" |
                                                                                           Status_Exame == "Exame em Análise" |
                                                                                           Status_Exame == "Disponivel para Encaminhar") %>% 
                                                                                  count()
  )
  
  RS22_22_23_SE_SOROLOGIA[which(RS22_22_23_SE_SOROLOGIA == i),5] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>% 
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 34,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automação em Processo" |
                                                                                          Status_Exame == "Exame em Análise" |
                                                                                          Status_Exame == "Disponivel para Encaminhar") %>% 
                                                                                 count()
  )
  
  RS22_22_23_SE_SOROLOGIA[which(RS22_22_23_SE_SOROLOGIA == i), 6] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>% 
                                                                                  filter(Municipio_Residencia == i,
                                                                                         SE == 35,
                                                                                         Status_Exame == "Resultado Liberado" |
                                                                                           Status_Exame == "Automação em Processo" |
                                                                                           Status_Exame == "Exame em Análise" |
                                                                                           Status_Exame == "Disponivel para Encaminhar") %>% 
                                                                                  count() 
  )
  
  RS22_22_23_SE_SOROLOGIA[which(RS22_22_23_SE_SOROLOGIA == i), 7] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>%
                                                                                  filter(Municipio_Residencia == i,
                                                                                         SE == 36,
                                                                                         Status_Exame == "Resultado Liberado" |
                                                                                           Status_Exame == "Automação em Processo" |
                                                                                           Status_Exame == "Exame em Análise" |
                                                                                           Status_Exame == "Disponivel para Encaminhar") %>%
                                                                                  count()
  )
  
  RS22_22_23_SE_SOROLOGIA[which(RS22_22_23_SE_SOROLOGIA == i), 8] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>%
                                                                                  filter(Municipio_Residencia == i,
                                                                                         SE == 37,
                                                                                         Status_Exame == "Resultado Liberado" |
                                                                                           Status_Exame == "Automação em Processo" |
                                                                                           Status_Exame == "Exame em Análise" |
                                                                                           Status_Exame == "Disponivel para Encaminhar") %>% 
                                                                                  count() 
  )
  
  RS22_22_23_SE_SOROLOGIA[which(RS22_22_23_SE_SOROLOGIA == i), 9] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>%
                                                                                  filter(Municipio_Residencia == i,
                                                                                         SE == 38,
                                                                                         Status_Exame == "Resultado Liberado" |
                                                                                           Status_Exame == "Automação em Processo" |
                                                                                           Status_Exame == "Exame em Análise" |
                                                                                           Status_Exame == "Disponivel para Encaminhar") %>% 
                                                                                  count() 
  )
  
  RS22_22_23_SE_SOROLOGIA[which(RS22_22_23_SE_SOROLOGIA == i), 10] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>% 
                                                                                   filter(Municipio_Residencia == i,
                                                                                          SE == 39,
                                                                                          Status_Exame == "Resultado Liberado" |
                                                                                            Status_Exame == "Automação em Processo" |
                                                                                            Status_Exame == "Exame em Análise" |
                                                                                            Status_Exame == "Disponivel para Encaminhar") %>%
                                                                                   count()
  )
  
  RS22_22_23_SE_SOROLOGIA[which(RS22_22_23_SE_SOROLOGIA == i), 11] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>% 
                                                                                   filter(Municipio_Residencia == i,
                                                                                          SE == 40,
                                                                                          Status_Exame == "Resultado Liberado" |
                                                                                            Status_Exame == "Automação em Processo" |
                                                                                            Status_Exame == "Exame em Análise" |
                                                                                            Status_Exame == "Disponivel para Encaminhar") %>%
                                                                                   count() 
  )
  
  RS22_22_23_SE_SOROLOGIA[which(RS22_22_23_SE_SOROLOGIA == i), 12] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>%
                                                                                   filter(Municipio_Residencia == i,
                                                                                          SE == 41,
                                                                                          Status_Exame == "Resultado Liberado" |
                                                                                            Status_Exame == "Automação em Processo" |
                                                                                            Status_Exame == "Exame em Análise" |
                                                                                            Status_Exame == "Disponivel para Encaminhar") %>%
                                                                                   count() 
  )
  
  RS22_22_23_SE_SOROLOGIA[which(RS22_22_23_SE_SOROLOGIA == i), 13] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>% 
                                                                                   filter(Municipio_Residencia == i,
                                                                                          SE == 42,
                                                                                          Status_Exame == "Resultado Liberado" |
                                                                                            Status_Exame == "Automação em Processo" |
                                                                                            Status_Exame == "Exame em Análise" |
                                                                                            Status_Exame == "Disponivel para Encaminhar") %>%
                                                                                   count() 
  )
  
  RS22_22_23_SE_SOROLOGIA[which(RS22_22_23_SE_SOROLOGIA == i), 14] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>% 
                                                                                   filter(Municipio_Residencia == i,
                                                                                          SE == 43,
                                                                                          Status_Exame == "Resultado Liberado" |
                                                                                            Status_Exame == "Automação em Processo" |
                                                                                            Status_Exame == "Exame em Análise" |
                                                                                            Status_Exame == "Disponivel para Encaminhar") %>% 
                                                                                   count() 
  )
  
  RS22_22_23_SE_SOROLOGIA[which(RS22_22_23_SE_SOROLOGIA == i), 15] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>% 
                                                                                   filter(Municipio_Residencia == i,
                                                                                          SE == 44,
                                                                                          Status_Exame == "Resultado Liberado" |
                                                                                            Status_Exame == "Automação em Processo" |
                                                                                            Status_Exame == "Exame em Análise" |
                                                                                            Status_Exame == "Disponivel para Encaminhar") %>%
                                                                                   count() 
  )
  
  RS22_22_23_SE_SOROLOGIA[which(RS22_22_23_SE_SOROLOGIA == i), 16] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>% 
                                                                                   filter(Municipio_Residencia == i,
                                                                                          SE == 45,
                                                                                          Status_Exame == "Resultado Liberado" |
                                                                                            Status_Exame == "Automação em Processo" |
                                                                                            Status_Exame == "Exame em Análise" |
                                                                                            Status_Exame == "Disponivel para Encaminhar") %>%
                                                                                   count() 
  )
  
  RS22_22_23_SE_SOROLOGIA[which(RS22_22_23_SE_SOROLOGIA == i), 17] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>% 
                                                                                   filter(Municipio_Residencia == i,
                                                                                          SE == 46,
                                                                                          Status_Exame == "Resultado Liberado" |
                                                                                            Status_Exame == "Automação em Processo" |
                                                                                            Status_Exame == "Exame em Análise" |
                                                                                            Status_Exame == "Disponivel para Encaminhar") %>%
                                                                                   count()
  )
  
  RS22_22_23_SE_SOROLOGIA[which(RS22_22_23_SE_SOROLOGIA == i), 18] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>%
                                                                                   filter(Municipio_Residencia == i,
                                                                                          SE == 47,
                                                                                          Status_Exame == "Resultado Liberado" |
                                                                                            Status_Exame == "Automação em Processo" |
                                                                                            Status_Exame == "Exame em Análise" |
                                                                                            Status_Exame == "Disponivel para Encaminhar") %>%       
                                                                                   count() 
  )
  
  RS22_22_23_SE_SOROLOGIA[which(RS22_22_23_SE_SOROLOGIA == i), 19] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>% 
                                                                                   filter(Municipio_Residencia == i,
                                                                                          SE == 48,
                                                                                          Status_Exame == "Resultado Liberado" |
                                                                                            Status_Exame == "Automação em Processo" |
                                                                                            Status_Exame == "Exame em Análise" |
                                                                                            Status_Exame == "Disponivel para Encaminhar") %>%     
                                                                                   count() 
  )
  
  RS22_22_23_SE_SOROLOGIA[which(RS22_22_23_SE_SOROLOGIA == i), 20] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>% 
                                                                                   filter(Municipio_Residencia == i,
                                                                                          SE == 49,
                                                                                          Status_Exame == "Resultado Liberado" |
                                                                                            Status_Exame == "Automação em Processo" |
                                                                                            Status_Exame == "Exame em Análise" |
                                                                                            Status_Exame == "Disponivel para Encaminhar") %>%
                                                                                   count() 
  )
  
  RS22_22_23_SE_SOROLOGIA[which(RS22_22_23_SE_SOROLOGIA == i),  21] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>% 
                                                                                    filter(Municipio_Residencia == i,
                                                                                           SE == 50,
                                                                                           Status_Exame == "Resultado Liberado" |
                                                                                             Status_Exame == "Automação em Processo" |
                                                                                             Status_Exame == "Exame em Análise" |
                                                                                             Status_Exame == "Disponivel para Encaminhar") %>%
                                                                                    count() 
  )
  
  RS22_22_23_SE_SOROLOGIA[which(RS22_22_23_SE_SOROLOGIA == i), 22] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>% 
                                                                                   filter(Municipio_Residencia == i,
                                                                                          SE == 51,
                                                                                          Status_Exame == "Resultado Liberado" |
                                                                                            Status_Exame == "Automação em Processo" |
                                                                                            Status_Exame == "Exame em Análise" |
                                                                                            Status_Exame == "Disponivel para Encaminhar") %>%
                                                                                   count() 
  )
  
  RS22_22_23_SE_SOROLOGIA[which(RS22_22_23_SE_SOROLOGIA == i), 23] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>% 
                                                                                   filter(Municipio_Residencia == i,
                                                                                          SE == 52,
                                                                                          Status_Exame == "Resultado Liberado" |
                                                                                            Status_Exame == "Automação em Processo" |
                                                                                            Status_Exame == "Exame em Análise" |
                                                                                            Status_Exame == "Disponivel para Encaminhar") %>%
                                                                                   count() 
  )
  
  RS22_22_23_SE_SOROLOGIA[which(RS22_22_23_SE_SOROLOGIA == i), 24] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>% 
                                                                                   filter(Municipio_Residencia == i,
                                                                                          SE == 53,
                                                                                          Status_Exame == "Resultado Liberado" |
                                                                                            Status_Exame == "Automação em Processo" |
                                                                                            Status_Exame == "Exame em Análise" |
                                                                                            Status_Exame == "Disponivel para Encaminhar") %>%
                                                                                   count()
  )
  
  RS22_22_23_SE_SOROLOGIA[which(RS22_22_23_SE_SOROLOGIA == i), 25] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>%
                                                                                   filter(Municipio_Residencia == i,
                                                                                          SE == 1,
                                                                                          Status_Exame == "Resultado Liberado" |
                                                                                            Status_Exame == "Automação em Processo" |
                                                                                            Status_Exame == "Exame em Análise" |
                                                                                            Status_Exame == "Disponivel para Encaminhar") %>%
                                                                                   count() 
  )
  
  RS22_22_23_SE_SOROLOGIA[which(RS22_22_23_SE_SOROLOGIA == i), 26] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>% 
                                                                                   filter(Municipio_Residencia == i,
                                                                                          SE == 2,
                                                                                          Status_Exame == "Resultado Liberado" |
                                                                                            Status_Exame == "Automação em Processo" |
                                                                                            Status_Exame == "Exame em Análise" |
                                                                                            Status_Exame == "Disponivel para Encaminhar") %>%
                                                                                   count()
  )
  
  RS22_22_23_SE_SOROLOGIA[which(RS22_22_23_SE_SOROLOGIA == i), 27] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>%
                                                                                   filter(Municipio_Residencia == i,
                                                                                          SE == 3,
                                                                                          Status_Exame == "Resultado Liberado" |
                                                                                            Status_Exame == "Automação em Processo" |
                                                                                            Status_Exame == "Exame em Análise" |
                                                                                            Status_Exame == "Disponivel para Encaminhar") %>%
                                                                                   count() 
  )
  
  RS22_22_23_SE_SOROLOGIA[which(RS22_22_23_SE_SOROLOGIA == i), 28] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>% 
                                                                                   filter(Municipio_Residencia == i,
                                                                                          SE == 4,
                                                                                          Status_Exame == "Resultado Liberado" |
                                                                                            Status_Exame == "Automação em Processo" |
                                                                                            Status_Exame == "Exame em Análise" |
                                                                                            Status_Exame == "Disponivel para Encaminhar") %>%
                                                                                   count() 
  )
  
  RS22_22_23_SE_SOROLOGIA[which(RS22_22_23_SE_SOROLOGIA == i), 29] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>% 
                                                                                   filter(Municipio_Residencia == i,
                                                                                          SE == 5,
                                                                                          Status_Exame == "Resultado Liberado" |
                                                                                            Status_Exame == "Automação em Processo" |
                                                                                            Status_Exame == "Exame em Análise" |
                                                                                            Status_Exame == "Disponivel para Encaminhar") %>%
                                                                                   count() 
  )
  
  RS22_22_23_SE_SOROLOGIA[which(RS22_22_23_SE_SOROLOGIA == i), 30] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>%
                                                                                   filter(Municipio_Residencia == i,
                                                                                          SE == 6,
                                                                                          Status_Exame == "Resultado Liberado" |
                                                                                            Status_Exame == "Automação em Processo" |
                                                                                            Status_Exame == "Exame em Análise" |
                                                                                            Status_Exame == "Disponivel para Encaminhar") %>%
                                                                                   count() 
  )
  
  RS22_22_23_SE_SOROLOGIA[which(RS22_22_23_SE_SOROLOGIA == i), 31] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>%
                                                                                   filter(Municipio_Residencia == i,
                                                                                          SE == 7,
                                                                                          Status_Exame == "Resultado Liberado" |
                                                                                            Status_Exame == "Automação em Processo" |
                                                                                            Status_Exame == "Exame em Análise" |
                                                                                            Status_Exame == "Disponivel para Encaminhar") %>% 
                                                                                   count()
  )
  
  RS22_22_23_SE_SOROLOGIA[which(RS22_22_23_SE_SOROLOGIA == i), 32] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>% 
                                                                                   filter(Municipio_Residencia == i,
                                                                                          SE == 8,
                                                                                          Status_Exame == "Resultado Liberado" |
                                                                                            Status_Exame == "Automação em Processo" |
                                                                                            Status_Exame == "Exame em Análise" |
                                                                                            Status_Exame == "Disponivel para Encaminhar") %>%
                                                                                   count()
  )
  
  RS22_22_23_SE_SOROLOGIA[which(RS22_22_23_SE_SOROLOGIA == i), 33] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>%
                                                                                   filter(Municipio_Residencia == i,
                                                                                          SE == 9,
                                                                                          Status_Exame == "Resultado Liberado" |
                                                                                            Status_Exame == "Automação em Processo" |
                                                                                            Status_Exame == "Exame em Análise" |
                                                                                            Status_Exame == "Disponivel para Encaminhar") %>%
                                                                                   count() 
  )
  
  RS22_22_23_SE_SOROLOGIA[which(RS22_22_23_SE_SOROLOGIA == i), 34] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>%
                                                                                   filter(Municipio_Residencia == i,
                                                                                          SE == 10,
                                                                                          Status_Exame == "Resultado Liberado" |
                                                                                            Status_Exame == "Automação em Processo" |
                                                                                            Status_Exame == "Exame em Análise" |
                                                                                            Status_Exame == "Disponivel para Encaminhar") %>% 
                                                                                   count() 
  )
  
  RS22_22_23_SE_SOROLOGIA[which(RS22_22_23_SE_SOROLOGIA == i), 35] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>%
                                                                                   filter(Municipio_Residencia == i,
                                                                                          SE == 11,
                                                                                          Status_Exame == "Resultado Liberado" |
                                                                                            Status_Exame == "Automação em Processo" |
                                                                                            Status_Exame == "Exame em Análise" |
                                                                                            Status_Exame == "Disponivel para Encaminhar") %>% 
                                                                                   count() 
  )
  
  RS22_22_23_SE_SOROLOGIA[which(RS22_22_23_SE_SOROLOGIA == i), 36] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>% 
                                                                                   filter(Municipio_Residencia == i,
                                                                                          SE == 12,
                                                                                          Status_Exame == "Resultado Liberado" |
                                                                                            Status_Exame == "Automação em Processo" |
                                                                                            Status_Exame == "Exame em Análise" |
                                                                                            Status_Exame == "Disponivel para Encaminhar") %>%
                                                                                   count() 
  )
  
  RS22_22_23_SE_SOROLOGIA[which(RS22_22_23_SE_SOROLOGIA == i), 37] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>%
                                                                                   filter(Municipio_Residencia == i,
                                                                                          SE == 13,
                                                                                          Status_Exame == "Resultado Liberado" |
                                                                                            Status_Exame == "Automação em Processo" |
                                                                                            Status_Exame == "Exame em Análise" |
                                                                                            Status_Exame == "Disponivel para Encaminhar") %>%
                                                                                   count() 
  )
  
  RS22_22_23_SE_SOROLOGIA[which(RS22_22_23_SE_SOROLOGIA == i), 38] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>%
                                                                                   filter(Municipio_Residencia == i,
                                                                                          SE == 14,
                                                                                          Status_Exame == "Resultado Liberado" |
                                                                                            Status_Exame == "Automação em Processo" |
                                                                                            Status_Exame == "Exame em Análise" |
                                                                                            Status_Exame == "Disponivel para Encaminhar") %>% 
                                                                                   count() 
  )
  
  RS22_22_23_SE_SOROLOGIA[which(RS22_22_23_SE_SOROLOGIA == i), 39] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>% 
                                                                                   filter(Municipio_Residencia == i,
                                                                                          SE == 15,
                                                                                          Status_Exame == "Resultado Liberado" |
                                                                                            Status_Exame == "Automação em Processo" |
                                                                                            Status_Exame == "Exame em Análise" |
                                                                                            Status_Exame == "Disponivel para Encaminhar") %>%
                                                                                   count()
  )
  
  RS22_22_23_SE_SOROLOGIA[which(RS22_22_23_SE_SOROLOGIA == i), 40] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>%
                                                                                   filter(Municipio_Residencia == i,
                                                                                          SE == 16,
                                                                                          Status_Exame == "Resultado Liberado" |
                                                                                            Status_Exame == "Automação em Processo" |
                                                                                            Status_Exame == "Exame em Análise" |
                                                                                            Status_Exame == "Disponivel para Encaminhar") %>%
                                                                                   count() 
  )
  
  RS22_22_23_SE_SOROLOGIA[which(RS22_22_23_SE_SOROLOGIA == i), 41] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>%
                                                                                   filter(Municipio_Residencia == i,
                                                                                          SE == 17,
                                                                                          Status_Exame == "Resultado Liberado" |
                                                                                            Status_Exame == "Automação em Processo" |
                                                                                            Status_Exame == "Exame em Análise" |
                                                                                            Status_Exame == "Disponivel para Encaminhar") %>%
                                                                                   count() 
  )
  
  RS22_22_23_SE_SOROLOGIA[which(RS22_22_23_SE_SOROLOGIA == i), 42] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>% 
                                                                                   filter(Municipio_Residencia == i,
                                                                                          SE == 18,
                                                                                          Status_Exame == "Resultado Liberado" |
                                                                                            Status_Exame == "Automação em Processo" |
                                                                                            Status_Exame == "Exame em Análise" |
                                                                                            Status_Exame == "Disponivel para Encaminhar") %>%
                                                                                   count() 
  )
  
  RS22_22_23_SE_SOROLOGIA[which(RS22_22_23_SE_SOROLOGIA == i), 43] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>% 
                                                                                   filter(Municipio_Residencia == i,
                                                                                          SE == 19,
                                                                                          Status_Exame == "Resultado Liberado" |
                                                                                            Status_Exame == "Automação em Processo" |
                                                                                            Status_Exame == "Exame em Análise" |
                                                                                            Status_Exame == "Disponivel para Encaminhar") %>%
                                                                                   count()
  )
  
  RS22_22_23_SE_SOROLOGIA[which(RS22_22_23_SE_SOROLOGIA == i), 44] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>%
                                                                                   filter(Municipio_Residencia == i,
                                                                                          SE == 20,
                                                                                          Status_Exame == "Resultado Liberado" |
                                                                                            Status_Exame == "Automação em Processo" |
                                                                                            Status_Exame == "Exame em Análise" |
                                                                                            Status_Exame == "Disponivel para Encaminhar") %>%
                                                                                   count()
  )
  
  RS22_22_23_SE_SOROLOGIA[which(RS22_22_23_SE_SOROLOGIA == i), 45] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>%
                                                                                   filter(Municipio_Residencia == i,
                                                                                          SE == 21,
                                                                                          Status_Exame == "Resultado Liberado" |
                                                                                            Status_Exame == "Automação em Processo" |
                                                                                            Status_Exame == "Exame em Análise" |
                                                                                            Status_Exame == "Disponivel para Encaminhar") %>%
                                                                                   count() 
  )
  
  RS22_22_23_SE_SOROLOGIA[which(RS22_22_23_SE_SOROLOGIA == i), 46] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>%
                                                                                   filter(Municipio_Residencia == i,
                                                                                          SE == 22,
                                                                                          Status_Exame == "Resultado Liberado" |
                                                                                            Status_Exame == "Automação em Processo" |
                                                                                            Status_Exame == "Exame em Análise" |
                                                                                            Status_Exame == "Disponivel para Encaminhar") %>%
                                                                                   count()
  )
  
  RS22_22_23_SE_SOROLOGIA[which(RS22_22_23_SE_SOROLOGIA == i), 47] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>%
                                                                                   filter(Municipio_Residencia == i,
                                                                                          SE == 23,
                                                                                          Status_Exame == "Resultado Liberado" |
                                                                                            Status_Exame == "Automação em Processo" |
                                                                                            Status_Exame == "Exame em Análise" |
                                                                                            Status_Exame == "Disponivel para Encaminhar") %>%
                                                                                   count() 
  )
  
  RS22_22_23_SE_SOROLOGIA[which(RS22_22_23_SE_SOROLOGIA == i), 48] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>%
                                                                                   filter(Municipio_Residencia == i,
                                                                                          SE == 24,
                                                                                          Status_Exame == "Resultado Liberado" |
                                                                                            Status_Exame == "Automação em Processo" |
                                                                                            Status_Exame == "Exame em Análise" |
                                                                                            Status_Exame == "Disponivel para Encaminhar") %>%
                                                                                   count() 
  )
  
  RS22_22_23_SE_SOROLOGIA[which(RS22_22_23_SE_SOROLOGIA == i), 49] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>%
                                                                                   filter(Municipio_Residencia == i,
                                                                                          SE == 25,
                                                                                          Status_Exame == "Resultado Liberado" |
                                                                                            Status_Exame == "Automação em Processo" |
                                                                                            Status_Exame == "Exame em Análise" |
                                                                                            Status_Exame == "Disponivel para Encaminhar") %>%
                                                                                   count() 
  )
  
  RS22_22_23_SE_SOROLOGIA[which(RS22_22_23_SE_SOROLOGIA == i), 50] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>%
                                                                                   filter(Municipio_Residencia == i,
                                                                                          SE == 26,
                                                                                          Status_Exame == "Resultado Liberado" |
                                                                                            Status_Exame == "Automação em Processo" |
                                                                                            Status_Exame == "Exame em Análise" |
                                                                                            Status_Exame == "Disponivel para Encaminhar") %>% 
                                                                                   count()
  )
  
  RS22_22_23_SE_SOROLOGIA[which(RS22_22_23_SE_SOROLOGIA == i), 51] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>% 
                                                                                   filter(Municipio_Residencia == i,
                                                                                          SE == 27,
                                                                                          Status_Exame == "Resultado Liberado" |
                                                                                            Status_Exame == "Automação em Processo" |
                                                                                            Status_Exame == "Exame em Análise" |
                                                                                            Status_Exame == "Disponivel para Encaminhar") %>% 
                                                                                   count() 
  )
  
  RS22_22_23_SE_SOROLOGIA[which(RS22_22_23_SE_SOROLOGIA == i), 52] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>%
                                                                                   filter(Municipio_Residencia == i,
                                                                                          SE == 28,
                                                                                          Status_Exame == "Resultado Liberado" |
                                                                                            Status_Exame == "Automação em Processo" |
                                                                                            Status_Exame == "Exame em Análise" |
                                                                                            Status_Exame == "Disponivel para Encaminhar") %>%
                                                                                   count() 
  )
  
  RS22_22_23_SE_SOROLOGIA[which(RS22_22_23_SE_SOROLOGIA == i), 53] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>%
                                                                                   filter(Municipio_Residencia == i,
                                                                                          SE == 29,
                                                                                          Status_Exame == "Resultado Liberado" |
                                                                                            Status_Exame == "Automação em Processo" |
                                                                                            Status_Exame == "Exame em Análise" |
                                                                                            Status_Exame == "Disponivel para Encaminhar") %>%
                                                                                   count() 
  )
  
  RS22_22_23_SE_SOROLOGIA[which(RS22_22_23_SE_SOROLOGIA == i), 54] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>% 
                                                                                   filter(Municipio_Residencia == i,
                                                                                          SE == 30,
                                                                                          Status_Exame == "Resultado Liberado" |
                                                                                            Status_Exame == "Automação em Processo" |
                                                                                            Status_Exame == "Exame em Análise" |
                                                                                            Status_Exame == "Disponivel para Encaminhar") %>%
                                                                                   count() 
  )
}

RS22_22_23_SE_SOROLOGIA[17,2:54] <- apply(RS22_22_23_SE_SOROLOGIA[,2:54], 2, sum)

RS22_22_23_SE_SOROLOGIA[17,1] <- "Total"

############################################################################################################################################
#####################  Realizando a contagem de exames por SE SOROLOGIA REAGENTES GERAL  #############################################################

RS22_22_23_SE_SOROLOGIA_REAGENTE <- matrix(data = NA, 
                                           nrow = 16, 
                                           ncol = 54)

RS22_22_23_SE_SOROLOGIA_REAGENTE <- as.data.frame(RS22_22_23_SE_SOROLOGIA_REAGENTE)

colnames(RS22_22_23_SE_SOROLOGIA_REAGENTE)[1] <- "Município" 

RS22_22_23_SE_SOROLOGIA_REAGENTE[,1] <- BASE_IBGE[which(BASE_IBGE$RS == 22), 3]

colnames (RS22_22_23_SE_SOROLOGIA_REAGENTE)[2:24] <- c(31:53)

colnames (RS22_22_23_SE_SOROLOGIA_REAGENTE)[25:54] <- c(1:30)

for (i in BASE_IBGE[(which(BASE_IBGE$RS == 22)), 3]){
  
  RS22_22_23_SE_SOROLOGIA_REAGENTE[which(RS22_22_23_SE_SOROLOGIA_REAGENTE == i), 2] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>%
                                                                                                    filter(Municipio_Residencia == i,
                                                                                                           SE == 31,
                                                                                                           Resultado == "Reagente ") %>%
                                                                                                    count()
  )
  
  RS22_22_23_SE_SOROLOGIA_REAGENTE[which(RS22_22_23_SE_SOROLOGIA_REAGENTE == i), 3] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>% 
                                                                                                    filter(Municipio_Residencia == i,
                                                                                                           SE == 32,
                                                                                                           Resultado == "Reagente ") %>% 
                                                                                                    count()
  )
  
  RS22_22_23_SE_SOROLOGIA_REAGENTE[which(RS22_22_23_SE_SOROLOGIA_REAGENTE == i), 4] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>% 
                                                                                                    filter(Municipio_Residencia == i,
                                                                                                           SE == 33,
                                                                                                           Resultado == "Reagente ") %>% 
                                                                                                    count()
  )
  
  RS22_22_23_SE_SOROLOGIA_REAGENTE[which(RS22_22_23_SE_SOROLOGIA_REAGENTE == i),5] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>% 
                                                                                                   filter(Municipio_Residencia == i,
                                                                                                          SE == 34,
                                                                                                          Resultado == "Reagente ") %>% 
                                                                                                   count()
  )
  
  RS22_22_23_SE_SOROLOGIA_REAGENTE[which(RS22_22_23_SE_SOROLOGIA_REAGENTE == i), 6] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>% 
                                                                                                    filter(Municipio_Residencia == i,
                                                                                                           SE == 35,
                                                                                                           Resultado == "Reagente ") %>% 
                                                                                                    count() 
  )
  
  RS22_22_23_SE_SOROLOGIA_REAGENTE[which(RS22_22_23_SE_SOROLOGIA_REAGENTE == i), 7] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>%
                                                                                                    filter(Municipio_Residencia == i,
                                                                                                           SE == 36,
                                                                                                           Resultado == "Reagente ") %>%
                                                                                                    count()
  )
  
  RS22_22_23_SE_SOROLOGIA_REAGENTE[which(RS22_22_23_SE_SOROLOGIA_REAGENTE == i), 8] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>%
                                                                                                    filter(Municipio_Residencia == i,
                                                                                                           SE == 37,
                                                                                                           Resultado == "Reagente ") %>% 
                                                                                                    count() 
  )
  
  RS22_22_23_SE_SOROLOGIA_REAGENTE[which(RS22_22_23_SE_SOROLOGIA_REAGENTE == i), 9] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>%
                                                                                                    filter(Municipio_Residencia == i,
                                                                                                           SE == 38,
                                                                                                           Resultado == "Reagente ") %>% 
                                                                                                    count() 
  )
  
  RS22_22_23_SE_SOROLOGIA_REAGENTE[which(RS22_22_23_SE_SOROLOGIA_REAGENTE == i), 10] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>% 
                                                                                                     filter(Municipio_Residencia == i,
                                                                                                            SE == 39,
                                                                                                            Resultado == "Reagente ") %>%
                                                                                                     count()
  )
  
  RS22_22_23_SE_SOROLOGIA_REAGENTE[which(RS22_22_23_SE_SOROLOGIA_REAGENTE == i), 11] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>% 
                                                                                                     filter(Municipio_Residencia == i,
                                                                                                            SE == 40,
                                                                                                            Resultado == "Reagente ") %>%
                                                                                                     count() 
  )
  
  RS22_22_23_SE_SOROLOGIA_REAGENTE[which(RS22_22_23_SE_SOROLOGIA_REAGENTE == i), 12] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>%
                                                                                                     filter(Municipio_Residencia == i,
                                                                                                            SE == 41,
                                                                                                            Resultado == "Reagente ") %>%
                                                                                                     count() 
  )
  
  RS22_22_23_SE_SOROLOGIA_REAGENTE[which(RS22_22_23_SE_SOROLOGIA_REAGENTE == i), 13] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>% 
                                                                                                     filter(Municipio_Residencia == i,
                                                                                                            SE == 42,
                                                                                                            Resultado == "Reagente ") %>%
                                                                                                     count() 
  )
  
  RS22_22_23_SE_SOROLOGIA_REAGENTE[which(RS22_22_23_SE_SOROLOGIA_REAGENTE == i), 14] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>% 
                                                                                                     filter(Municipio_Residencia == i,
                                                                                                            SE == 43,
                                                                                                            Resultado == "Reagente ") %>% 
                                                                                                     count() 
  )
  
  RS22_22_23_SE_SOROLOGIA_REAGENTE[which(RS22_22_23_SE_SOROLOGIA_REAGENTE == i), 15] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>% 
                                                                                                     filter(Municipio_Residencia == i,
                                                                                                            SE == 44,
                                                                                                            Resultado == "Reagente ") %>%
                                                                                                     count() 
  )
  
  RS22_22_23_SE_SOROLOGIA_REAGENTE[which(RS22_22_23_SE_SOROLOGIA_REAGENTE == i), 16] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>% 
                                                                                                     filter(Municipio_Residencia == i,
                                                                                                            SE == 45,
                                                                                                            Resultado == "Reagente ") %>%
                                                                                                     count() 
  )
  
  RS22_22_23_SE_SOROLOGIA_REAGENTE[which(RS22_22_23_SE_SOROLOGIA_REAGENTE == i), 17] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>% 
                                                                                                     filter(Municipio_Residencia == i,
                                                                                                            SE == 46,
                                                                                                            Resultado == "Reagente ") %>%
                                                                                                     count()
  )
  
  RS22_22_23_SE_SOROLOGIA_REAGENTE[which(RS22_22_23_SE_SOROLOGIA_REAGENTE == i), 18] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>%
                                                                                                     filter(Municipio_Residencia == i,
                                                                                                            SE == 47,
                                                                                                            Resultado == "Reagente ") %>%       
                                                                                                     count() 
  )
  
  RS22_22_23_SE_SOROLOGIA_REAGENTE[which(RS22_22_23_SE_SOROLOGIA_REAGENTE == i), 19] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>% 
                                                                                                     filter(Municipio_Residencia == i,
                                                                                                            SE == 48,
                                                                                                            Resultado == "Reagente ") %>%     
                                                                                                     count() 
  )
  
  RS22_22_23_SE_SOROLOGIA_REAGENTE[which(RS22_22_23_SE_SOROLOGIA_REAGENTE == i), 20] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>% 
                                                                                                     filter(Municipio_Residencia == i,
                                                                                                            SE == 49,
                                                                                                            Resultado == "Reagente ") %>%
                                                                                                     count() 
  )
  
  RS22_22_23_SE_SOROLOGIA_REAGENTE[which(RS22_22_23_SE_SOROLOGIA_REAGENTE == i),  21] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>% 
                                                                                                      filter(Municipio_Residencia == i,
                                                                                                             SE == 50,
                                                                                                             Resultado == "Reagente ") %>%
                                                                                                      count() 
  )
  
  RS22_22_23_SE_SOROLOGIA_REAGENTE[which(RS22_22_23_SE_SOROLOGIA_REAGENTE == i), 22] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>% 
                                                                                                     filter(Municipio_Residencia == i,
                                                                                                            SE == 51,
                                                                                                            Resultado == "Reagente ") %>%
                                                                                                     count() 
  )
  
  RS22_22_23_SE_SOROLOGIA_REAGENTE[which(RS22_22_23_SE_SOROLOGIA_REAGENTE == i), 23] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>% 
                                                                                                     filter(Municipio_Residencia == i,
                                                                                                            SE == 52,
                                                                                                            Resultado == "Reagente ") %>%
                                                                                                     count() 
  )
  
  RS22_22_23_SE_SOROLOGIA_REAGENTE[which(RS22_22_23_SE_SOROLOGIA_REAGENTE == i), 24] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>% 
                                                                                                     filter(Municipio_Residencia == i,
                                                                                                            SE == 53,
                                                                                                            Resultado == "Reagente ") %>%
                                                                                                     count()
  )
  
  RS22_22_23_SE_SOROLOGIA_REAGENTE[which(RS22_22_23_SE_SOROLOGIA_REAGENTE == i), 25] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>%
                                                                                                     filter(Municipio_Residencia == i,
                                                                                                            SE == 1,
                                                                                                            Resultado == "Reagente ") %>%
                                                                                                     count() 
  )
  
  RS22_22_23_SE_SOROLOGIA_REAGENTE[which(RS22_22_23_SE_SOROLOGIA_REAGENTE == i), 26] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>% 
                                                                                                     filter(Municipio_Residencia == i,
                                                                                                            SE == 2,
                                                                                                            Resultado == "Reagente ") %>%
                                                                                                     count()
  )
  
  RS22_22_23_SE_SOROLOGIA_REAGENTE[which(RS22_22_23_SE_SOROLOGIA_REAGENTE == i), 27] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>%
                                                                                                     filter(Municipio_Residencia == i,
                                                                                                            SE == 3,
                                                                                                            Resultado == "Reagente ") %>%
                                                                                                     count() 
  )
  
  RS22_22_23_SE_SOROLOGIA_REAGENTE[which(RS22_22_23_SE_SOROLOGIA_REAGENTE == i), 28] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>% 
                                                                                                     filter(Municipio_Residencia == i,
                                                                                                            SE == 4,
                                                                                                            Resultado == "Reagente ") %>%
                                                                                                     count() 
  )
  
  RS22_22_23_SE_SOROLOGIA_REAGENTE[which(RS22_22_23_SE_SOROLOGIA_REAGENTE == i), 29] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>% 
                                                                                                     filter(Municipio_Residencia == i,
                                                                                                            SE == 5,
                                                                                                            Resultado == "Reagente ") %>%
                                                                                                     count() 
  )
  
  RS22_22_23_SE_SOROLOGIA_REAGENTE[which(RS22_22_23_SE_SOROLOGIA_REAGENTE == i), 30] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>%
                                                                                                     filter(Municipio_Residencia == i,
                                                                                                            SE == 6,
                                                                                                            Resultado == "Reagente ") %>%
                                                                                                     count() 
  )
  
  RS22_22_23_SE_SOROLOGIA_REAGENTE[which(RS22_22_23_SE_SOROLOGIA_REAGENTE == i), 31] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>%
                                                                                                     filter(Municipio_Residencia == i,
                                                                                                            SE == 7,
                                                                                                            Resultado == "Reagente ") %>% 
                                                                                                     count()
  )
  
  RS22_22_23_SE_SOROLOGIA_REAGENTE[which(RS22_22_23_SE_SOROLOGIA_REAGENTE == i), 32] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>% 
                                                                                                     filter(Municipio_Residencia == i,
                                                                                                            SE == 8,
                                                                                                            Resultado == "Reagente ") %>%
                                                                                                     count()
  )
  
  RS22_22_23_SE_SOROLOGIA_REAGENTE[which(RS22_22_23_SE_SOROLOGIA_REAGENTE == i), 33] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>%
                                                                                                     filter(Municipio_Residencia == i,
                                                                                                            SE == 9,
                                                                                                            Resultado == "Reagente ") %>%
                                                                                                     count() 
  )
  
  RS22_22_23_SE_SOROLOGIA_REAGENTE[which(RS22_22_23_SE_SOROLOGIA_REAGENTE == i), 34] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>%
                                                                                                     filter(Municipio_Residencia == i,
                                                                                                            SE == 10,
                                                                                                            Resultado == "Reagente ") %>% 
                                                                                                     count() 
  )
  
  RS22_22_23_SE_SOROLOGIA_REAGENTE[which(RS22_22_23_SE_SOROLOGIA_REAGENTE == i), 35] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>%
                                                                                                     filter(Municipio_Residencia == i,
                                                                                                            SE == 11,
                                                                                                            Resultado == "Reagente ") %>% 
                                                                                                     count() 
  )
  
  RS22_22_23_SE_SOROLOGIA_REAGENTE[which(RS22_22_23_SE_SOROLOGIA_REAGENTE == i), 36] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>% 
                                                                                                     filter(Municipio_Residencia == i,
                                                                                                            SE == 12,
                                                                                                            Resultado == "Reagente ") %>%
                                                                                                     count() 
  )
  
  RS22_22_23_SE_SOROLOGIA_REAGENTE[which(RS22_22_23_SE_SOROLOGIA_REAGENTE == i), 37] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>%
                                                                                                     filter(Municipio_Residencia == i,
                                                                                                            SE == 13,
                                                                                                            Resultado == "Reagente ") %>%
                                                                                                     count() 
  )
  
  RS22_22_23_SE_SOROLOGIA_REAGENTE[which(RS22_22_23_SE_SOROLOGIA_REAGENTE == i), 38] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>%
                                                                                                     filter(Municipio_Residencia == i,
                                                                                                            SE == 14,
                                                                                                            Resultado == "Reagente ") %>% 
                                                                                                     count() 
  )
  
  RS22_22_23_SE_SOROLOGIA_REAGENTE[which(RS22_22_23_SE_SOROLOGIA_REAGENTE == i), 39] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>% 
                                                                                                     filter(Municipio_Residencia == i,
                                                                                                            SE == 15,
                                                                                                            Resultado == "Reagente ") %>%
                                                                                                     count()
  )
  
  RS22_22_23_SE_SOROLOGIA_REAGENTE[which(RS22_22_23_SE_SOROLOGIA_REAGENTE == i), 40] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>%
                                                                                                     filter(Municipio_Residencia == i,
                                                                                                            SE == 16,
                                                                                                            Resultado == "Reagente ") %>%
                                                                                                     count() 
  )
  
  RS22_22_23_SE_SOROLOGIA_REAGENTE[which(RS22_22_23_SE_SOROLOGIA_REAGENTE == i), 41] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>%
                                                                                                     filter(Municipio_Residencia == i,
                                                                                                            SE == 17,
                                                                                                            Resultado == "Reagente ") %>%
                                                                                                     count() 
  )
  
  RS22_22_23_SE_SOROLOGIA_REAGENTE[which(RS22_22_23_SE_SOROLOGIA_REAGENTE == i), 42] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>% 
                                                                                                     filter(Municipio_Residencia == i,
                                                                                                            SE == 18,
                                                                                                            Resultado == "Reagente ") %>%
                                                                                                     count() 
  )
  
  RS22_22_23_SE_SOROLOGIA_REAGENTE[which(RS22_22_23_SE_SOROLOGIA_REAGENTE == i), 43] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>% 
                                                                                                     filter(Municipio_Residencia == i,
                                                                                                            SE == 19,
                                                                                                            Resultado == "Reagente ") %>%
                                                                                                     count()
  )
  
  RS22_22_23_SE_SOROLOGIA_REAGENTE[which(RS22_22_23_SE_SOROLOGIA_REAGENTE == i), 44] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>%
                                                                                                     filter(Municipio_Residencia == i,
                                                                                                            SE == 20,
                                                                                                            Resultado == "Reagente ") %>%
                                                                                                     count()
  )
  
  RS22_22_23_SE_SOROLOGIA_REAGENTE[which(RS22_22_23_SE_SOROLOGIA_REAGENTE == i), 45] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>%
                                                                                                     filter(Municipio_Residencia == i,
                                                                                                            SE == 21,
                                                                                                            Resultado == "Reagente ") %>%
                                                                                                     count() 
  )
  
  RS22_22_23_SE_SOROLOGIA_REAGENTE[which(RS22_22_23_SE_SOROLOGIA_REAGENTE == i), 46] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>%
                                                                                                     filter(Municipio_Residencia == i,
                                                                                                            SE == 22,
                                                                                                            Resultado == "Reagente ") %>%
                                                                                                     count()
  )
  
  RS22_22_23_SE_SOROLOGIA_REAGENTE[which(RS22_22_23_SE_SOROLOGIA_REAGENTE == i), 47] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>%
                                                                                                     filter(Municipio_Residencia == i,
                                                                                                            SE == 23,
                                                                                                            Resultado == "Reagente ") %>%
                                                                                                     count() 
  )
  
  RS22_22_23_SE_SOROLOGIA_REAGENTE[which(RS22_22_23_SE_SOROLOGIA_REAGENTE == i), 48] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>%
                                                                                                     filter(Municipio_Residencia == i,
                                                                                                            SE == 24,
                                                                                                            Resultado == "Reagente ") %>%
                                                                                                     count() 
  )
  
  RS22_22_23_SE_SOROLOGIA_REAGENTE[which(RS22_22_23_SE_SOROLOGIA_REAGENTE == i), 49] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>%
                                                                                                     filter(Municipio_Residencia == i,
                                                                                                            SE == 25,
                                                                                                            Resultado == "Reagente ") %>%
                                                                                                     count() 
  )
  
  RS22_22_23_SE_SOROLOGIA_REAGENTE[which(RS22_22_23_SE_SOROLOGIA_REAGENTE == i), 50] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>%
                                                                                                     filter(Municipio_Residencia == i,
                                                                                                            SE == 26,
                                                                                                            Resultado == "Reagente ") %>% 
                                                                                                     count()
  )
  
  RS22_22_23_SE_SOROLOGIA_REAGENTE[which(RS22_22_23_SE_SOROLOGIA_REAGENTE == i), 51] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>% 
                                                                                                     filter(Municipio_Residencia == i,
                                                                                                            SE == 27,
                                                                                                            Resultado == "Reagente ") %>% 
                                                                                                     count() 
  )
  
  RS22_22_23_SE_SOROLOGIA_REAGENTE[which(RS22_22_23_SE_SOROLOGIA_REAGENTE == i), 52] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>%
                                                                                                     filter(Municipio_Residencia == i,
                                                                                                            SE == 28,
                                                                                                            Resultado == "Reagente ") %>%
                                                                                                     count() 
  )
  
  RS22_22_23_SE_SOROLOGIA_REAGENTE[which(RS22_22_23_SE_SOROLOGIA_REAGENTE == i), 53] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>%
                                                                                                     filter(Municipio_Residencia == i,
                                                                                                            SE == 29,
                                                                                                            Resultado == "Reagente ") %>%
                                                                                                     count() 
  )
  
  RS22_22_23_SE_SOROLOGIA_REAGENTE[which(RS22_22_23_SE_SOROLOGIA_REAGENTE == i), 54] <- as.integer(RS22_22_23_LACEN_SOROLOGIA%>% 
                                                                                                     filter(Municipio_Residencia == i,
                                                                                                            SE == 30,
                                                                                                            Resultado == "Reagente ") %>%
                                                                                                     count() 
  )
}

RS22_22_23_SE_SOROLOGIA_REAGENTE[17,2:54] <- apply(RS22_22_23_SE_SOROLOGIA_REAGENTE[,2:54], 
                                                   2, 
                                                   sum)

RS22_22_23_SE_SOROLOGIA_REAGENTE[17,1] <- "Total"

#############################################################################################################################
#############################################################################################################################
#########################    Elaboração dos Gráficos para inserção no Informe  ##############################################
#############################################################################################################################


#################   Gráfico de amostras encaminhadas pela U.S.   ###########################################################

AUX <- RS22_22_23_SE_PESQ_ARB[6,]

AUX[2, ] <- colnames(RS22_22_23_SE_PESQ_ARB)

AUX <- AUX[c(2, 1),]

AUX <- as.data.frame(t(AUX))

colnames(AUX) <- AUX[1, ]

AUX <- AUX[-1, ]

AUX[, 2] <- as.numeric(AUX[, 2])

AUX$Sem_EPI <-as.character(c("2022/31",  "2022/32", "2022/33",  
                             "2022/34",  "2022/35",  "2022/36",  
                             "2022/37",  "2022/38",  "2022/39",  
                             "2022/40",  "2022/41",  "2022/42", 
                             "2022/43",  "2022/44",  "2022/45",  
                             "2022/46",  "2022/47",  "2022/48",  
                             "2022/49",  "2022/50",  "2022/51", 
                             "2022/52",  "2022/53",  "2023/01",  
                             "2023/02",  "2023/03",  "2023/04",  
                             "2023/05",  "2023/06",  "2023/07",  
                             "2023/08",  "2023/09",  "2023/10", 
                             "2023/11",  "2023/12",  "2023/13",  
                             "2023/14",  "2023/15",  "2023/16",  
                             "2023/17",  "2023/18",  "2023/19",  
                             "2023/20",  "2023/21",  "2023/22",  
                             "2023/23",  "2023/24",  "2023/25", 
                             "2023/26",  "2023/27",  "2023/28", 
                             "2023/29",  "2023/30")
)


RS22_22_23_GRAF_US_TOTAL <- ggplot(AUX, aes(x = Sem_EPI, y = IVAIPORÃ))  + 
  theme(axis.text.x = element_text(face = "bold",
                                   angle = 80,
                                   vjust = .5,
                                   size = 8)) +
  labs(caption = Fonte_1, 
       x = "Semana Epidemiológica",
       y = "Número de Amostras Encaminhadas",
       title = "Quantidade de Amostras Encaminhadas/SE - Unidade Sentinela") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#EEE8AA") + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.5)))

#######  Separando a quantidade de casos detectáveis da U.S.   ##########################

AUX_2 <- as.data.frame(RS22_22_23_SE_PESQ_ARB_DETECTAVEL[6, ])

AUX_2 <- AUX_2[, -1]

AUX_2 <- t(AUX_2)

AUX$US_DETEC <- AUX_2

colnames(AUX)[4] <- "US_DETEC"

#####  Criando coluna com porcentagem de casos detectáveis   #####

AUX$PORC_US_DETEC <- (AUX$US_DETEC/AUX$IVAIPORÃ) * 100

AUX$PORC_US_DETEC[which(is.nan(AUX$PORC_US_DETEC), 5)] <- 0

AUX$PORC_US_DETEC <- format(round(AUX$PORC_US_DETEC, 2))

AUX$PORC_US_DETEC <- as.numeric(AUX$PORC_US_DETEC)

#############  Criando gráfico com dados de detectáveis  ##############

RS22_22_23_GRAF_US_DETEC <-  ggplot(AUX, aes(x = Sem_EPI, y = PORC_US_DETEC))  + 
  theme(axis.text.x = element_text(face = "bold",
                                   angle = 80,
                                   vjust = .5,
                                   size = 8)) +
  labs(caption = Fonte_1, 
       x = "Semana Epidemiológica",
       y = "% de Amostras Encaminhadas Positivas",
       title = "Taxa de Amostras Positivas/SE - Unidade Sentinela") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#EEE8AA") + 
  scale_y_continuous(expand = expansion(mult = c(0, 
                                                 0.001)
  )
  )

################################################################################################################
######################  Amostras encaminhadas para o LACEN (sorologia e pesq. de arbovírus)  ###################

AUX <- RS22_22_23_SE_SOROLOGIA[17,]

AUX[2, ] <- colnames(RS22_22_23_SE_SOROLOGIA)

AUX <- AUX[c(2, 1),]

AUX <- as.data.frame(t(AUX))

colnames(AUX) <- AUX[1, ]

AUX <- AUX[-1, ]

AUX[, 2] <- as.numeric(AUX[, 2])

AUX$Sem_EPI <-as.character(c("2022/31",  "2022/32", "2022/33",  
                             "2022/34",  "2022/35",  "2022/36",  
                             "2022/37",  "2022/38",  "2022/39",  
                             "2022/40",  "2022/41",  "2022/42",  
                             "2022/43",  "2022/44",  "2022/45",  
                             "2022/46",  "2022/47",  "2022/48", 
                             "2022/49",  "2022/50",  "2022/51",  
                             "2022/52",  "2022/53",  "2023/01",  
                             "2023/02",  "2023/03",  "2023/04",  
                             "2023/05",  "2023/06",  "2023/07",  
                             "2023/08",  "2023/09",  "2023/10", 
                             "2023/11",  "2023/12",  "2023/13",  
                             "2023/14",  "2023/15",  "2023/16",  
                             "2023/17",  "2023/18",  "2023/19",  
                             "2023/20",  "2023/21",  "2023/22",  
                             "2023/23",  "2023/24",  "2023/25",  
                             "2023/26",  "2023/27",  "2023/28",  
                             "2023/29",  "2023/30")
)


RS22_22_23_GRAF_SORO_TOTAL <- ggplot(AUX, aes(x = Sem_EPI, y = Total))  + 
  theme(axis.text.x = element_text(face = "bold",
                                   angle = 80,
                                   vjust = .5,
                                   size = 8)) +
  labs(caption = Fonte_1, 
       x = "Semana Epidemiológica",
       y = "Número de Amostras",
       title = "Quantidade de Amostras (Sorologia) Encaminhadas/SE - 22ª RS") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#9ad2b0") + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))


#######  Amostras Encaminhadas ao LACEN que tiveram resultado REAGENTE ENZIMAIMUNOENSAIO   ##################
#############################################################################################################

AUX <- as.data.frame(RS22_22_23_SE_SOROLOGIA_REAGENTE[17, ])

AUX <- AUX[, -1]

AUX <- t(AUX)

AUX <- as.data.frame(AUX)

AUX$Sem_EPI <-as.character(c("2022/31",  "2022/32", "2022/33",  
                             "2022/34",  "2022/35",  "2022/36",  
                             "2022/37",  "2022/38",  "2022/39",  
                             "2022/40",  "2022/41",  "2022/42",  
                             "2022/43",  "2022/44",  "2022/45",  
                             "2022/46",  "2022/47",  "2022/48",  
                             "2022/49",  "2022/50",  "2022/51",  
                             "2022/52",  "2022/53",  "2023/01",  
                             "2023/02",  "2023/03",  "2023/04",  
                             "2023/05",  "2023/06",  "2023/07",  
                             "2023/08",  "2023/09",  "2023/10", 
                             "2023/11",  "2023/12",  "2023/13", 
                             "2023/14",  "2023/15",  "2023/16",  
                             "2023/17",  "2023/18",  "2023/19",  
                             "2023/20",  "2023/21",  "2023/22",  
                             "2023/23",  "2023/24",  "2023/25",  
                             "2023/26",  "2023/27",  "2023/28",  
                             "2023/29",  "2023/30")
)

colnames(AUX)[1] <- "Amostras"

AUX$Total <- t(RS22_22_23_SE_SOROLOGIA[17, 2:54])

colnames(AUX)[3] <- "Total"

#####  Criando coluna com porcentagem de casos detectáveis   #####

AUX$PORC_SORO_REAG <- (AUX$Amostras/AUX$Total) * 100

AUX$PORC_SORO_REAG[which(is.nan(AUX$PORC_SORO_REAG), 5)] <- 0

AUX$PORC_USORO_REAG <- format(round(AUX$PORC_SORO_REAG, 2))

AUX$PORC_SORO_REAG <- as.numeric(AUX$PORC_SORO_REAG)

#############  Criando gráfico com dados de detectáveis  ##############

RS22_22_23_GRAF_SORO_REAG <- ggplot(AUX, aes(x = Sem_EPI, y = PORC_SORO_REAG))  + 
  theme(axis.text.x = element_text(face = "bold",
                                   angle = 80,
                                   vjust = .5,
                                   size = 8)) +
  labs(caption = Fonte_1, 
       x = "Semana Epidemiológica",
       y = "% de Amostras Encaminhadas Reagentes",
       title = "Taxa de Amostras (Sorologia) Reagentes/SE - 22ª RS") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#9ad2b0") + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.001)))

#####################################################################################################################
####################  LACEN - MUNICÍPIOS   ##########################################################################
#####################################################################################################################

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == 22), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == 22), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == 22), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == 22), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Sorologia <- NA

AUX$Sorologia_Reag <- NA

AUX$Pesq_Arb <- NA

AUX$Pesq_Arb_Detec <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == 22)), 3]){
  
  ###Sorologia/Município###  
  AUX[which(AUX$Município == i), 5] <- as.integer(RS22_22_23_LACEN_SOROLOGIA %>% 
                                                    filter(Municipio_Residencia == i) %>%   
                                                    count()
  )   
  
  ###Sorologia reagente/Município##  
  AUX[which(AUX$Município == i), 6] <- as.integer(RS22_22_23_LACEN_SOROLOGIA %>% 
                                                    filter(Municipio_Residencia == i,
                                                           Resultado == "Reagente ") %>%   
                                                    count()
  )
  
  ###Pesquisa arbovírus/Município##  
  AUX[which(AUX$Município == i), 7] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO %>% 
                                                    filter(Municipio_Residencia == i) %>%   
                                                    count()
  )
  
  ###Pesquisa arbovírus detectável/Município##  
  AUX[which(AUX$Município == i), 8] <- as.integer(RS22_22_23_LACEN_PESQ_ARBO %>% 
                                                    filter(Municipio_Residencia == i,
                                                           Resultado == "Detectável") %>%   
                                                    count()
  )
}

AUX$ENCAMINHADAS <- (AUX$Sorologia + AUX$Pesq_Arb)

AUX$POSITIVAS <- (AUX$Sorologia_Reag + AUX$Pesq_Arb_Detec)

RS22_GRAF_LACEN_MUNIC <- ggplot (AUX, 
                                 aes(x = Município)
) + 
  theme(axis.text.x = element_text(angle = 85, 
                                   vjust = .5,
                                   face = "bold")) +
  labs(caption = Fonte, 
       x = NULL,
       y = "Número de Amostras",
       title = "AMOSTRAS ENCAMINHADAS/POSITIVAS - 22ªRS",
       subtitle = "Amostras de sorologia + pesquisa de arbovírus") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 20,
                                   colour = "#556B2F")) +
  geom_bar(
    aes( y = ENCAMINHADAS, fill = "ENCAMINHADAS"),
    stat = "identity",
    color = "black",
    width = .4,
    position = position_nudge(x = -.20)) + 
  geom_label(aes(y = ENCAMINHADAS,
                 label = ENCAMINHADAS),
             size = 3, 
             alpha = 0.5,
             nudge_x = -.20,
             nudge_y = 10) + 
  scale_fill_manual(name = "", values = c("ENCAMINHADAS" = "#556B2F", "POSITIVAS" = "#FF6347")) +
  theme(legend.position = "bottom") +
  geom_bar(
    aes( y = POSITIVAS, fill = "POSITIVAS"),
    stat = "identity",
    color = "black",
    width = .4,
    position = position_nudge(x = .20)) +
  geom_label(aes(y = POSITIVAS,
                 label = POSITIVAS),
             size = 3, 
             alpha = 0.5,
             nudge_x = .20,
             nudge_y = 10) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))

#####################################################################################################################
#####################  Dados Estaduais   ############################################################################
#####################################################################################################################
### Transformando os arquivos da base DBF em um único objeto referente ao período sazonal

PR_CHIK_22_23_AUX01 <- CHIKON2022 %>% 
  filter(SEM_PRI >= 202231)

PR_CHIK_22_23_AUX02 <- CHIKON2023 %>% 
  filter(SEM_PRI <=202330)

PR_CHIK_22_23_SINAN <- rbind(PR_CHIK_22_23_AUX01, PR_CHIK_22_23_AUX02)

rm(PR_CHIK_22_23_AUX01, PR_CHIK_22_23_AUX02)

####Elaborando for loop para criar tabela de dados gerais de notificação da 22ª RS###

PR_CHIK_22_23_GERAL <- BASE_IBGE[,-4]

PR_CHIK_22_23_GERAL$Notificados <- NA

PR_CHIK_22_23_GERAL$Confirmados <- NA

PR_CHIK_22_23_GERAL$Descartados <- NA

PR_CHIK_22_23_GERAL$Autoctones <- NA

PR_CHIK_22_23_GERAL$Importados <- NA

PR_CHIK_22_23_GERAL$Obitos <- NA

PR_CHIK_22_23_GERAL$Incidencia <- NA

for(i in BASE_IBGE[, 2]){
  
  ###Notiicações###  
  PR_CHIK_22_23_GERAL[which(PR_CHIK_22_23_GERAL$Código_IBGE == i), 5] <- as.integer(PR_CHIK_22_23_SINAN%>% 
                                                                                      filter(ID_MN_RESI == i) %>%   
                                                                                      count()
  )   
  
  ###Chikungunya###
  
  PR_CHIK_22_23_GERAL[which(PR_CHIK_22_23_GERAL$Código_IBGE == i), 6] <-as.integer(PR_CHIK_22_23_SINAN%>% 
                                                                                     filter(CLASSI_FIN == 13, 
                                                                                            ID_MN_RESI == i) %>%
                                                                                     count() 
  )
  
  ###Descartados###
  
  
  
  PR_CHIK_22_23_GERAL[which(PR_CHIK_22_23_GERAL$Código_IBGE == i), 7]<- as.integer(PR_CHIK_22_23_SINAN%>% 
                                                                                     filter(CLASSI_FIN == 5,
                                                                                            ID_MN_RESI == i) %>% 
                                                                                     count()
  )  
  
  ###Autóctones###
  
  
  PR_CHIK_22_23_GERAL[which(PR_CHIK_22_23_GERAL$Código_IBGE == i), 8]<- as.integer(PR_CHIK_22_23_SINAN%>% 
                                                                                     filter(ID_MN_RESI == i, 
                                                                                            TPAUTOCTO == 1,
                                                                                            CLASSI_FIN == 13) %>% 
                                                                                     count() 
  )
  
  ###Importados###
  
  
  PR_CHIK_22_23_GERAL[which(PR_CHIK_22_23_GERAL$Código_IBGE == i), 9]<- as.integer(PR_CHIK_22_23_SINAN%>% 
                                                                                     filter(ID_MN_RESI == i, 
                                                                                            TPAUTOCTO == 2,
                                                                                            CLASSI_FIN == 13) %>% 
                                                                                     count() 
  )
  
  ###Óbitos###
  
  PR_CHIK_22_23_GERAL[which(PR_CHIK_22_23_GERAL$Código_IBGE == i), 10] <- as.integer(PR_CHIK_22_23_SINAN%>% 
                                                                                       filter(ID_MN_RESI == i, 
                                                                                              EVOLUCAO == 2) %>% 
                                                                                       count() 
  )
}

###Incidência###FORA DO LOOP###

PR_CHIK_22_23_GERAL$Incidencia <- (PR_CHIK_22_23_GERAL$Autoctones/PR_CHIK_22_23_GERAL$População)*100000  
PR_CHIK_22_23_GERAL$Incidencia <- format(round(PR_CHIK_22_23_GERAL$Incidencia, 2))
PR_CHIK_22_23_GERAL$Incidencia <- as.numeric(PR_CHIK_22_23_GERAL$Incidencia)


PR_CHIK_22_23_GERAL$Em_Investigacao <- as.integer(PR_CHIK_22_23_GERAL$Notificados) - as.integer(PR_CHIK_22_23_GERAL$Confirmados + PR_CHIK_22_23_GERAL$Descartados)

PR_CHIK_22_23_GERAL <- PR_CHIK_22_23_GERAL[, c(1, 3, 4, 5, 6, 12, 7, 8, 9, 10, 11)]

PR_CHIK_22_23_GERAL[400, 3:10] <- apply(PR_CHIK_22_23_GERAL[, 3:10], 2, sum)

#######################################  TABELA SINAIS E SINTOMAS - Chikungunya/Paraná   ####################################################
#############################################################################################################################################

#PR_22_23_AUX01 <- CHIKON2022 %>% 
#  filter(SEM_PRI >= 202231)

#PR_22_23_AUX02 <- CHIKON2023 %>% 
#  filter(SEM_PRI <=202330)

#PR_22_23_SINAN_CHIK <- rbind(PR_22_23_AUX01, PR_22_23_AUX02)

#rm(PR_22_23_AUX01, PR_22_23_AUX02)

PR_22_23_SINAN_DECODIFICADO_CHIK <- PR_CHIK_22_23_SINAN

PR_22_23_SINAN_DECODIFICADO_CHIK$ID_AGRAVO <- factor(PR_22_23_SINAN_DECODIFICADO_CHIK$ID_AGRAVO,
                                                     label = c("Dengue", "Chikungunya"), 
                                                     levels = c("A90", "A92.0")
)

###Sintomas###
PR_22_23_SINAN_DECODIFICADO_CHIK$FEBRE <- factor(PR_22_23_SINAN_DECODIFICADO_CHIK$FEBRE,
                                                 label = c("SIM", "NÃO"), 
                                                 levels = c(1, 2)
)

PR_22_23_SINAN_DECODIFICADO_CHIK$MIALGIA <- factor(PR_22_23_SINAN_DECODIFICADO_CHIK$MIALGIA,
                                                   label = c("SIM", "NÃO"), 
                                                   levels = c(1, 2)
)

PR_22_23_SINAN_DECODIFICADO_CHIK$CEFALEIA <- factor(PR_22_23_SINAN_DECODIFICADO_CHIK$CEFALEIA,
                                                    label = c("SIM", "NÃO"), 
                                                    levels = c(1, 2)
)

PR_22_23_SINAN_DECODIFICADO_CHIK$EXANTEMA <- factor(PR_22_23_SINAN_DECODIFICADO_CHIK$EXANTEMA,
                                                    label = c("SIM", "NÃO"), 
                                                    levels = c(1, 2)
)

PR_22_23_SINAN_DECODIFICADO_CHIK$VOMITO <- factor(PR_22_23_SINAN_DECODIFICADO_CHIK$VOMITO,
                                                  label = c("SIM", "NÃO"), 
                                                  levels = c(1, 2)
)

PR_22_23_SINAN_DECODIFICADO_CHIK$NAUSEA <- factor(PR_22_23_SINAN_DECODIFICADO_CHIK$NAUSEA,
                                                  label = c("SIM", "NÃO"), 
                                                  levels = c(1, 2)
)

PR_22_23_SINAN_DECODIFICADO_CHIK$DOR_COSTAS <- factor(PR_22_23_SINAN_DECODIFICADO_CHIK$DOR_COSTAS,
                                                      label = c("SIM", "NÃO"), 
                                                      levels = c(1, 2)
)

PR_22_23_SINAN_DECODIFICADO_CHIK$DOR_RETRO <- factor(PR_22_23_SINAN_DECODIFICADO_CHIK$DOR_RETRO,
                                                     label = c("SIM", "NÃO"), 
                                                     levels = c(1, 2)
)

PR_22_23_SINAN_DECODIFICADO_CHIK$CONJUNTVIT <- factor(PR_22_23_SINAN_DECODIFICADO_CHIK$CONJUNTVIT,
                                                      label = c("SIM", "NÃO"), 
                                                      levels = c(1, 2)
)

PR_22_23_SINAN_DECODIFICADO_CHIK$ARTRALGIA <- factor(PR_22_23_SINAN_DECODIFICADO_CHIK$ARTRALGIA,
                                                     label = c("SIM", "NÃO"), 
                                                     levels = c(1, 2)
)

PR_22_23_SINAN_DECODIFICADO_CHIK$ARTRITE <- factor(PR_22_23_SINAN_DECODIFICADO_CHIK$ARTRITE,
                                                   label = c("SIM", "NÃO"), 
                                                   levels = c(1, 2)
)

PR_22_23_SINAN_DECODIFICADO_CHIK$PETEQUIA_N <- factor(PR_22_23_SINAN_DECODIFICADO_CHIK$PETEQUIA_N,
                                                      label = c("SIM", "NÃO"), 
                                                      levels = c(1, 2)
)

PR_22_23_SINAN_DECODIFICADO_CHIK$LEUCOPENIA <- factor(PR_22_23_SINAN_DECODIFICADO_CHIK$LEUCOPENIA,
                                                      label = c("SIM", "NÃO"), 
                                                      levels = c(1, 2)
)

PR_22_23_SINAN_DECODIFICADO_CHIK$LACO <- factor(PR_22_23_SINAN_DECODIFICADO_CHIK$LACO,
                                                label = c("SIM", "NÃO"), 
                                                levels = c(1, 2)
)

###Doenças Pré-existentes

PR_22_23_SINAN_DECODIFICADO_CHIK$DIABETES <- factor(PR_22_23_SINAN_DECODIFICADO_CHIK$DIABETES,
                                                    label = c("SIM", "NÃO"), 
                                                    levels = c(1, 2)
)

PR_22_23_SINAN_DECODIFICADO_CHIK$HEMATOLOG <- factor(PR_22_23_SINAN_DECODIFICADO_CHIK$HEMATOLOG,
                                                     label = c("SIM", "NÃO"), 
                                                     levels = c(1, 2)
)

PR_22_23_SINAN_DECODIFICADO_CHIK$HEPATOPAT <- factor(PR_22_23_SINAN_DECODIFICADO_CHIK$HEPATOPAT,
                                                     label = c("SIM", "NÃO"), 
                                                     levels = c(1, 2)
)

PR_22_23_SINAN_DECODIFICADO_CHIK$RENAL <- factor(PR_22_23_SINAN_DECODIFICADO_CHIK$RENAL,
                                                 label = c("SIM", "NÃO"), 
                                                 levels = c(1, 2)
)

PR_22_23_SINAN_DECODIFICADO_CHIK$HIPERTENSA <- factor(PR_22_23_SINAN_DECODIFICADO_CHIK$HIPERTENSA,
                                                      label = c("SIM", "NÃO"), 
                                                      levels = c(1, 2)
)

PR_22_23_SINAN_DECODIFICADO_CHIK$ACIDO_PEPT <- factor(PR_22_23_SINAN_DECODIFICADO_CHIK$ACIDO_PEPT,
                                                      label = c("SIM", "NÃO"), 
                                                      levels = c(1, 2)
)

PR_22_23_SINAN_DECODIFICADO_CHIK$AUTO_IMUNE <- factor(PR_22_23_SINAN_DECODIFICADO_CHIK$AUTO_IMUNE,
                                                      label = c("SIM", "NÃO"), 
                                                      levels = c(1, 2)
)

####Outros####

PR_22_23_SINAN_DECODIFICADO_CHIK$CS_GESTANT <- factor(PR_22_23_SINAN_DECODIFICADO_CHIK$CS_GESTANT,
                                                      label = c("1_TRI", "2_TRI", "3_TRI", "IDADE GESTACIONAL IGN", "NÃO", "NÃO SE APLICA", "IGNORADO"), 
                                                      levels = c(1, 2, 3, 4, 5, 6, 9)
)

PR_22_23_SINAN_DECODIFICADO_CHIK$CS_ESCOL_N <- factor(PR_22_23_SINAN_DECODIFICADO_CHIK$CS_ESCOL_N,
                                                      label = c("ANALFABETO", "1 a 4 SÉRIE DO FUNDAMENTAL INCOMPLETA", "4 SÉRIE DO FUNDAMENTAL COMPLETA", "5 a 8 SÉRIE DO FUNDAMENTAL INCOMPLETA", "FUNDAMENTAL COMPLETO", "ENSINO MÉDIO INCOMPLETO", "ENSINO MÉDIO COMPLETO", "SUPERIOR INCONPLETO", "SUPERIOR COMPLETO", "IGNORADO", "NÃO SE APLICA"), 
                                                      levels = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
)

PR_22_23_SINAN_DECODIFICADO_CHIK$RESUL_SORO <- factor(PR_22_23_SINAN_DECODIFICADO_CHIK$RESUL_SORO,
                                                      label = c("REAGENTE", "NÃO REAGENTE", "INCONCLUSIVO", "NÃO REALIZADO"), 
                                                      levels = c(1, 2, 3, 4)
)


PR_22_23_SINAN_DECODIFICADO_CHIK$RESUL_PCR_ <- factor(PR_22_23_SINAN_DECODIFICADO_CHIK$RESUL_PCR_,
                                                      label = c("DETECTÁVEL", "NÃO DETECTÀVEL", "INCONCLUSIVO", "NÃO REALIZADO"), 
                                                      levels = c(1, 2, 3, 4)
)

PR_22_23_SINAN_DECODIFICADO_CHIK$SOROTIPO <- factor(PR_22_23_SINAN_DECODIFICADO_CHIK$SOROTIPO,
                                                    label = c("I", "II", "III", "IV"), 
                                                    levels = c(1, 2, 3, 4)
)

PR_22_23_SINAN_DECODIFICADO_CHIK$CLASSI_FIN <- factor(PR_22_23_SINAN_DECODIFICADO_CHIK$CLASSI_FIN,
                                                      label = c("DESCARTADO", "DENGUE", "D.S.A.", "IDENGUE_GRAVE", "CHIKUNGUNYA"), 
                                                      levels = c(5, 10, 11, 12, 13)
)

PR_22_23_SINAN_DECODIFICADO_CHIK$CRITERIO <- factor(PR_22_23_SINAN_DECODIFICADO_CHIK$CRITERIO,
                                                    label = c("LABORATORIAL", "CLÍNICO-EPIDEMIOLÓGICO", "EM INVESTIGAÇÃO"), 
                                                    levels = c(1, 2, 3)
)

PR_22_23_SINAN_DECODIFICADO_CHIK$TPAUTOCTO <- factor(PR_22_23_SINAN_DECODIFICADO_CHIK$TPAUTOCTO,
                                                     label = c("SIM", "NÃO"), 
                                                     levels = c(1, 2)
)

PR_22_23_SINAN_DECODIFICADO_CHIK$HOSPITALIZ <- factor(PR_22_23_SINAN_DECODIFICADO_CHIK$HOSPITALIZ,
                                                      label = c("SIM", "NÃO"), 
                                                      levels = c(1, 2)
)

PR_22_23_SINAN_DECODIFICADO_CHIK$EVOLUCAO <- factor(PR_22_23_SINAN_DECODIFICADO_CHIK$EVOLUCAO,
                                                    label = c("CURA", "ÓBITO PELO AGRAVO", "ÓBITO POR OUTRAS CAUSAS","ÓBITO EM INVESTIGAÇÃO", "INDETERMINADO"), 
                                                    levels = c(1, 2, 3, 4, 9)
)

PR_22_23_SINAN_DECODIFICADO_CHIK$CS_ZONA <- factor(PR_22_23_SINAN_DECODIFICADO_CHIK$CS_ZONA,
                                                   label = c("URBANA", "RURAL", "PERIURBANA","INDETERMINADO"), 
                                                   levels = c(1, 2, 3, 9)
)

PR_22_23_SINAN_DECODIFICADO_CHIK$ALRM_LETAR <- factor(PR_22_23_SINAN_DECODIFICADO_CHIK$ALRM_LETAR,
                                                      label = c("SIM", "NÃO"), 
                                                      levels = c(1, 2)
)

PR_22_23_SINAN_DECODIFICADO_CHIK$ALRM_HEPAT <- factor(PR_22_23_SINAN_DECODIFICADO_CHIK$ALRM_HEPAT,
                                                      label = c("SIM", "NÃO"), 
                                                      levels = c(1, 2)
)

PR_22_23_SINAN_DECODIFICADO_CHIK$ALRM_LIQ <- factor(PR_22_23_SINAN_DECODIFICADO_CHIK$ALRM_LIQ,
                                                    label = c("SIM", "NÃO"), 
                                                    levels = c(1, 2)
)

PR_22_23_SINAN_DECODIFICADO_CHIK$ALRM_HIPOT <- factor(PR_22_23_SINAN_DECODIFICADO_CHIK$ALRM_HIPOT,
                                                      label = c("SIM", "NÃO"), 
                                                      levels = c(1, 2)
)

PR_22_23_SINAN_DECODIFICADO_CHIK$ALRM_PLAQ <- factor(PR_22_23_SINAN_DECODIFICADO_CHIK$ALRM_PLAQ,
                                                     label = c("SIM", "NÃO"), 
                                                     levels = c(1, 2)
)

PR_22_23_SINAN_DECODIFICADO_CHIK$ALRM_VOM <- factor(PR_22_23_SINAN_DECODIFICADO_CHIK$ALRM_VOM,
                                                    label = c("SIM", "NÃO"), 
                                                    levels = c(1, 2)
)

PR_22_23_SINAN_DECODIFICADO_CHIK$ALRM_SANG <- factor(PR_22_23_SINAN_DECODIFICADO_CHIK$ALRM_SANG,
                                                     label = c("SIM", "NÃO"), 
                                                     levels = c(1, 2)
)

PR_22_23_SINAN_DECODIFICADO_CHIK$ALRM_HEMAT <- factor(PR_22_23_SINAN_DECODIFICADO_CHIK$ALRM_LETAR,
                                                      label = c("SIM", "NÃO"), 
                                                      levels = c(1, 2)
)

PR_22_23_SINAN_DECODIFICADO_CHIK$ALRM_ABDOM <- factor(PR_22_23_SINAN_DECODIFICADO_CHIK$ALRM_ABDOM,
                                                      label = c("SIM", "NÃO"), 
                                                      levels = c(1, 2)
)
####PR_22_23_SINAN_DECODIFICADO_CHIK$Municipio 

PR_22_23_SINAN_DECODIFICADO_CHIK_AUX <- data.frame(COD = PR_22_23_SINAN_DECODIFICADO_CHIK[,12], 
                                                   Municipio = NA)

for (i in PR_22_23_SINAN_DECODIFICADO_CHIK[,12]){
  PR_22_23_SINAN_DECODIFICADO_CHIK_AUX[which(PR_22_23_SINAN_DECODIFICADO_CHIK_AUX$COD == i), 2] <- BASE_IBGE_BRASIL[which(BASE_IBGE_BRASIL$Código.Município.Completo == i),13]
  
}

PR_22_23_SINAN_DECODIFICADO_CHIK[,12] <- PR_22_23_SINAN_DECODIFICADO_CHIK_AUX[, 2]

####Município de Residência

PR_22_23_SINAN_DECODIFICADO_CHIK_AUX <- data.frame(COD = PR_22_23_SINAN_DECODIFICADO_CHIK[,20], 
                                                   Municipio = NA)

for (i in PR_22_23_SINAN_DECODIFICADO_CHIK[,20]){
  PR_22_23_SINAN_DECODIFICADO_CHIK_AUX[which(PR_22_23_SINAN_DECODIFICADO_CHIK_AUX$COD == i), 2] <- BASE_IBGE_BRASIL[which(BASE_IBGE_BRASIL$Código.Município.Completo == i),13]
  
}

PR_22_23_SINAN_DECODIFICADO_CHIK[,20] <- PR_22_23_SINAN_DECODIFICADO_CHIK_AUX[, 2]

rm (PR_22_23_SINAN_DECODIFICADO_CHIK_AUX)

colnames(PR_22_23_SINAN_DECODIFICADO_CHIK)<- c("RS", "SINAN", "Latitude", "Longitude", "Agravo", "Data_Notificacao", "ANO", "SE_Notificacao", "Data_Primeiros_Sintomas", "SE_Primeiros_Sintomas", "UF_Notificacao", "Municipio", "Nome", "Data_Nascimento", "Idade", "Sexo", "Gestante", "Escolaridade", "Nome_Mae", "Municipio_Residencia", "UF_Residencia", "RS_Residencia", "Logradouro", "Numero", "Bairro", "CEP", "Zona", "Data_Digitacao", "Data_Investigacao", "Febre", "Mialgia", "Cefaleia", "Exantema", "Vomito", "Nausea", "Dor_nas_Costas", "Conjuntivite", "Artrite", "Artralgia_Intensa", "Petequias", "Leucopenia", "Prova_do_Laco_Positiva", "Dor_retroorbital", "Diabetes", "Doenca_Hematologica", "Hepatopatia", "Doenca_Renal", "Hipertensao", "Doenca_Acido_Peptica", "Doenca_Auto_Imune", "Data_Sorologia", "Resultado_Sorologia", "Data_PCR", "Resultado_PCR", "Sorotipo", "Classificacao_Final", "Critério_Encerramento", "Autoctone", "UF_Infeccao", "Municipio_Infeccao", "Bairro_Infeccao", "Evolucao", "Hospitalizado", "Data_Internamento", "Data_Obito", "Data_Encerramento", "Data_SNA", "Letargia", "Hepatomegalia", "Acumulo_Liquidos", "Hipotensao_Lipotimia", "Queda_Abrupta_Plaquetas", "Vomitos_Persistentes", "Hemorragias", "Aumento_Hematocrito", "Dor_Abdominal", "Data_Dengue_Grave", "Pulso_Debil", "PA_Convergente", "TPC", "Acumulo_Liq_Insuficiencia_Resp", "Taquicardia", "Extremidades_Frias", "Hipotensao", "Hematemese", "Melena", "Metrorragia_", "Sangramento_SNC", "Aumento_AST_ALT", "Miocardite", "Alteracao_Consciencia", "Outros_Orgaos", "Manifestacao_Hemorragica", "Epistaxe", "Gengivorragia", "Metrorragia", "Observacoes" )

PR_22_23_CHIK_SINAIS_NOTIFICADOS <- tibble(Febre = as.integer(PR_22_23_SINAN_DECODIFICADO_CHIK %>% 
                                                                filter(Febre == "SIM" ) %>%
                                                                count()),
                                           Mialgia = as.integer(PR_22_23_SINAN_DECODIFICADO_CHIK %>%
                                                                  filter(Mialgia == "SIM" ) %>%
                                                                  count()),
                                           Cefaleia = as.integer(PR_22_23_SINAN_DECODIFICADO_CHIK %>%
                                                                   filter(Cefaleia == "SIM") %>%
                                                                   count()),
                                           Exantema = as.integer(PR_22_23_SINAN_DECODIFICADO_CHIK %>%
                                                                   filter(Exantema == "SIM") %>%
                                                                   count()),
                                           Vomito = as.integer(PR_22_23_SINAN_DECODIFICADO_CHIK %>%
                                                                 filter(Vomito == "SIM") %>%
                                                                 count()),
                                           Nausea = as.integer(PR_22_23_SINAN_DECODIFICADO_CHIK %>%
                                                                 filter(Nausea == "SIM") %>%
                                                                 count()),
                                           Dor_nas_Costas = as.integer(PR_22_23_SINAN_DECODIFICADO_CHIK %>%
                                                                         filter(Dor_nas_Costas == "SIM") %>%
                                                                         count()),
                                           Conjuntivite = as.integer(PR_22_23_SINAN_DECODIFICADO_CHIK %>%
                                                                       filter(Conjuntivite == "SIM") %>%
                                                                       count()),
                                           Artrite = as.integer(PR_22_23_SINAN_DECODIFICADO_CHIK %>%
                                                                  filter(Artrite == "SIM") %>%
                                                                  count()),
                                           Artralgia_Intensa = as.integer(PR_22_23_SINAN_DECODIFICADO_CHIK %>%
                                                                            filter(Artralgia_Intensa == "SIM") %>%
                                                                            count()),
                                           Petequias = as.integer(PR_22_23_SINAN_DECODIFICADO_CHIK %>%
                                                                    filter(Petequias == "SIM") %>%
                                                                    count()),
                                           Leucopenia = as.integer(PR_22_23_SINAN_DECODIFICADO_CHIK %>%
                                                                     filter(Leucopenia == "SIM") %>%
                                                                     count()),
                                           Prova_do_Laco_Positiva = as.integer(PR_22_23_SINAN_DECODIFICADO_CHIK %>%
                                                                                 filter(Prova_do_Laco_Positiva == "SIM") %>%
                                                                                 count()),
                                           Dor_retroorbital = as.integer(PR_22_23_SINAN_DECODIFICADO_CHIK %>%
                                                                           filter(Dor_retroorbital == "SIM") %>%
                                                                           count())
)
colnames(PR_22_23_CHIK_SINAIS_NOTIFICADOS) <- c("Febre", "Mialgia", "Cefaleia", "Exantema", "Vomito", "Nausea", "Dor_nas_Costas", "Conjuntivite", "Artrite", "Artralgia_Intensa", "Petequias", "Leucopenia", "Prova_do_Laco_Positiva", "Dor_Retroorbital")

PR_22_23_CHIK_SINAIS_Confirmados <- tibble(Febre = as.integer(PR_22_23_SINAN_DECODIFICADO_CHIK %>% 
                                                                filter(Febre == "SIM",
                                                                       Classificacao_Final == "CHIKUNGUNYA") %>%
                                                                count()),
                                           Mialgia = as.integer(PR_22_23_SINAN_DECODIFICADO_CHIK %>%
                                                                  filter(Mialgia == "SIM",
                                                                         Classificacao_Final == "CHIKUNGUNYA") %>%
                                                                  count()),
                                           Cefaleia = as.integer(PR_22_23_SINAN_DECODIFICADO_CHIK %>%
                                                                   filter(Cefaleia == "SIM",
                                                                          Classificacao_Final == "CHIKUNGUNYA") %>%
                                                                   count()),
                                           Exantema = as.integer(PR_22_23_SINAN_DECODIFICADO_CHIK %>%
                                                                   filter(Exantema == "SIM",
                                                                          Classificacao_Final == "CHIKUNGUNYA") %>%
                                                                   count()),
                                           Vomito = as.integer(PR_22_23_SINAN_DECODIFICADO_CHIK %>%
                                                                 filter(Vomito == "SIM",
                                                                        Classificacao_Final == "CHIKUNGUNYA") %>%
                                                                 count()),
                                           Nausea = as.integer(PR_22_23_SINAN_DECODIFICADO_CHIK %>%
                                                                 filter(Nausea == "SIM",
                                                                        Classificacao_Final == "CHIKUNGUNYA") %>%
                                                                 count()),
                                           Dor_nas_Costas = as.integer(PR_22_23_SINAN_DECODIFICADO_CHIK %>%
                                                                         filter(Dor_nas_Costas == "SIM",
                                                                                Classificacao_Final == "CHIKUNGUNYA") %>%
                                                                         count()),
                                           Conjuntivite = as.integer(PR_22_23_SINAN_DECODIFICADO_CHIK %>%
                                                                       filter(Conjuntivite == "SIM",
                                                                              Classificacao_Final == "CHIKUNGUNYA") %>%
                                                                       count()),
                                           Artrite = as.integer(PR_22_23_SINAN_DECODIFICADO_CHIK %>%
                                                                  filter(Artrite == "SIM",
                                                                         Classificacao_Final == "CHIKUNGUNYA") %>%
                                                                  count()),
                                           Artralgia_Intensa = as.integer(PR_22_23_SINAN_DECODIFICADO_CHIK %>%
                                                                            filter(Artralgia_Intensa == "SIM",
                                                                                   Classificacao_Final == "CHIKUNGUNYA") %>%
                                                                            count()),
                                           Petequias = as.integer(PR_22_23_SINAN_DECODIFICADO_CHIK %>%
                                                                    filter(Petequias == "SIM",
                                                                           Classificacao_Final == "CHIKUNGUNYA") %>%
                                                                    count()),
                                           Leucopenia = as.integer(PR_22_23_SINAN_DECODIFICADO_CHIK %>%
                                                                     filter(Leucopenia == "SIM",
                                                                            Classificacao_Final == "CHIKUNGUNYA") %>%
                                                                     count()),
                                           Prova_do_Laco_Positiva = as.integer(PR_22_23_SINAN_DECODIFICADO_CHIK %>%
                                                                                 filter(Prova_do_Laco_Positiva == "SIM",
                                                                                        Classificacao_Final == "CHIKUNGUNYA") %>%
                                                                                 count()),
                                           Dor_retroorbital = as.integer(PR_22_23_SINAN_DECODIFICADO_CHIK %>%
                                                                           filter(Dor_retroorbital == "SIM",
                                                                                  Classificacao_Final == "CHIKUNGUNYA") %>%
                                                                           count())
)
#colnames(PR_22_23_CHIK_SINAIS_NOTIFICADOS) <- c("Febre", "Mialgia", "Cefaleia", "Exantema", "Vomito", "Nausea", "Dor_nas_Costas", "Conjuntivite", "Artrite", "Artralgia_Intensa", "Petequias", "Leucopenia", "Prova_do_Laco_Positiva", "Dor_Retroorbital")


AUX_GRAF <- data.frame(Sintomas = c("Febre", "Mialgia", "Cefaleia", "Exantema", "Vomito", "Nausea", "Dor_nas_Costas", "Conjuntivite", "Artrite", "Artralgia_Intensa", "Petequias", "Leucopenia", "Prova_do_Laco_Positiva", "Dor_Retroorbital"),
                       Notificados = NA,
                       Confirmados = NA)

AUX_GRAF[1,2] <- PR_22_23_CHIK_SINAIS_NOTIFICADOS[, 1]
AUX_GRAF[2,2] <- PR_22_23_CHIK_SINAIS_NOTIFICADOS[, 2]
AUX_GRAF[3,2] <- PR_22_23_CHIK_SINAIS_NOTIFICADOS[, 3]
AUX_GRAF[4,2] <- PR_22_23_CHIK_SINAIS_NOTIFICADOS[, 4]
AUX_GRAF[5,2] <- PR_22_23_CHIK_SINAIS_NOTIFICADOS[, 5]
AUX_GRAF[6,2] <- PR_22_23_CHIK_SINAIS_NOTIFICADOS[, 6]
AUX_GRAF[7,2] <- PR_22_23_CHIK_SINAIS_NOTIFICADOS[, 7]
AUX_GRAF[8,2] <- PR_22_23_CHIK_SINAIS_NOTIFICADOS[, 8]
AUX_GRAF[9,2] <- PR_22_23_CHIK_SINAIS_NOTIFICADOS[, 9]
AUX_GRAF[10,2] <- PR_22_23_CHIK_SINAIS_NOTIFICADOS[, 10]
AUX_GRAF[11,2] <- PR_22_23_CHIK_SINAIS_NOTIFICADOS[, 11]
AUX_GRAF[12,2] <- PR_22_23_CHIK_SINAIS_NOTIFICADOS[, 12]
AUX_GRAF[13,2] <- PR_22_23_CHIK_SINAIS_NOTIFICADOS[, 13]
AUX_GRAF[14,2] <- PR_22_23_CHIK_SINAIS_NOTIFICADOS[, 14]

AUX_GRAF[1,3] <- PR_22_23_CHIK_SINAIS_Confirmados[, 1]
AUX_GRAF[2,3] <- PR_22_23_CHIK_SINAIS_Confirmados[, 2]
AUX_GRAF[3,3] <- PR_22_23_CHIK_SINAIS_Confirmados[, 3]
AUX_GRAF[4,3] <- PR_22_23_CHIK_SINAIS_Confirmados[, 4]
AUX_GRAF[5,3] <- PR_22_23_CHIK_SINAIS_Confirmados[, 5]
AUX_GRAF[6,3] <- PR_22_23_CHIK_SINAIS_Confirmados[, 6]
AUX_GRAF[7,3] <- PR_22_23_CHIK_SINAIS_Confirmados[, 7]
AUX_GRAF[8,3] <- PR_22_23_CHIK_SINAIS_Confirmados[, 8]
AUX_GRAF[9,3] <- PR_22_23_CHIK_SINAIS_Confirmados[, 9]
AUX_GRAF[10,3] <- PR_22_23_CHIK_SINAIS_Confirmados[, 10]
AUX_GRAF[11,3] <- PR_22_23_CHIK_SINAIS_Confirmados[, 11]
AUX_GRAF[12,3] <- PR_22_23_CHIK_SINAIS_Confirmados[, 12]
AUX_GRAF[13,3] <- PR_22_23_CHIK_SINAIS_Confirmados[, 13]
AUX_GRAF[14,3] <- PR_22_23_CHIK_SINAIS_Confirmados[, 14]

PR_22_23_GRAF_SINAIS_CHIK <- ggplot (AUX_GRAF, 
                                     aes(x = Sintomas)) + 
  theme(axis.text.x = element_text(angle = 80, 
                                   vjust = .5,
                                   face = "bold",
                                   size = 14)) +
  labs(caption = Fonte, 
       x = "Sintomas",
       y = "Número de Casos",
       title = "PREVALÊNCIA DE SINTOMAS EM CASOS NOTIFICADOS/CONFIRMADOS PARANÁ - 2022/23",
       subtitle = "Sinais Clínicos em Notificações de CHIKUNGUNYA Assinalados no Campo 33 da Ficha do SINAN") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 19,
                                   colour = "#556B2F")) +
  geom_bar(aes( y = Notificados, 
                fill = "Notificados"),
           stat = "identity",
           color = "black",
           width = .4,
           position = position_nudge(x = -.20)) + 
  geom_label(aes(y = Notificados,
                 label = Notificados),
             size = 3, 
             alpha = 0.5,
             nudge_x = -.20,
             nudge_y = .5) + 
  scale_fill_manual(name = "", 
                    values = c("Notificados" = "#4D5656", "Confirmados" = "#B03A2E")) +
  theme(legend.position = "bottom") +
  geom_bar(aes( y = Confirmados, 
                fill = "Confirmados"),
           stat = "identity",
           color = "black",
           width = .4,
           position = position_nudge(x = .20)) +
  geom_label(aes(y = Confirmados,
                 label = Confirmados),
             size = 3, 
             alpha = 0.5,
             nudge_x = .20,
             nudge_y = .2) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

###############################################################################################################
################   Dengue Paraná   ######################################
###############################################################################################################

####    Tabela de notificações SINAN    ###

PR_DENGUE_22_23_AUX01 <- DENGON2022 %>% 
  filter(SEM_PRI >= 202231)

PR_DENGUE_22_23_AUX02 <- DENGON2023 %>% 
  filter(SEM_PRI <=202330)

PR_DENGUE_22_23_SINAN <- rbind(PR_DENGUE_22_23_AUX01, PR_DENGUE_22_23_AUX02)

rm(PR_DENGUE_22_23_AUX01, PR_DENGUE_22_23_AUX02, DENGON2022, DENGON2023)


PR_DENGUE_22_23_GERAL <- BASE_IBGE[,-c(4,6)]

PR_DENGUE_22_23_GERAL$Notificados <- NA

PR_DENGUE_22_23_GERAL$Dengue <- NA

PR_DENGUE_22_23_GERAL$D_S_A <- NA

PR_DENGUE_22_23_GERAL$Dengue_Grave <- NA

PR_DENGUE_22_23_GERAL$Descartados <- NA

PR_DENGUE_22_23_GERAL$Autoctones <- NA

PR_DENGUE_22_23_GERAL$Incidencia <- NA

PR_DENGUE_22_23_GERAL$DENV_I <- NA

PR_DENGUE_22_23_GERAL$DENV_II <- NA

PR_DENGUE_22_23_GERAL$DENV_III <- NA

PR_DENGUE_22_23_GERAL$DENV_IV <- NA

PR_DENGUE_22_23_GERAL$Obitos <- NA

PR_DENGUE_22_23_GERAL$Inconclusivos <- NA

PR_DENGUE_22_23_GERAL$Importados <- NA

for(i in BASE_IBGE[, 2]){
  
  ###Notiicações###  
  PR_DENGUE_22_23_GERAL[which(PR_DENGUE_22_23_GERAL$Código_IBGE == i), 5] <- as.integer(PR_DENGUE_22_23_SINAN %>% 
                                                                                          filter(ID_MN_RESI == i) %>%   
                                                                                          count()
  )   
  
  ###Dengue###
  
  PR_DENGUE_22_23_GERAL[which(PR_DENGUE_22_23_GERAL$Código_IBGE == i), 6] <-as.integer(PR_DENGUE_22_23_SINAN %>% 
                                                                                         filter(CLASSI_FIN == 10, 
                                                                                                ID_MN_RESI == i) %>%
                                                                                         count() 
  )
  
  ###D.S.A.###
  
  PR_DENGUE_22_23_GERAL[which(PR_DENGUE_22_23_GERAL$Código_IBGE == i), 7] <- as.integer(PR_DENGUE_22_23_SINAN %>%  
                                                                                          filter(CLASSI_FIN == 11, 
                                                                                                 ID_MN_RESI == i) %>% 
                                                                                          count()
  )
  
  ###Dengue Grave###
  
  PR_DENGUE_22_23_GERAL[which(PR_DENGUE_22_23_GERAL$Código_IBGE == i), 8] <- as.integer(PR_DENGUE_22_23_SINAN %>%  
                                                                                          filter(CLASSI_FIN == 12, 
                                                                                                 ID_MN_RESI == i) %>% 
                                                                                          count()
  )
  
  ###Descartados###
  
  
  
  PR_DENGUE_22_23_GERAL[which(PR_DENGUE_22_23_GERAL$Código_IBGE == i), 9]<- as.integer(PR_DENGUE_22_23_SINAN %>% 
                                                                                         filter(CLASSI_FIN == 5,
                                                                                                ID_MN_RESI == i) %>% 
                                                                                         count()
  )  
  
  ###Autóctones###
  
  PR_DENGUE_22_23_GERAL[which(PR_DENGUE_22_23_GERAL$Código_IBGE == i), 10]<- as.integer(PR_DENGUE_22_23_SINAN %>% 
                                                                                          filter(ID_MN_RESI == i, 
                                                                                                 TPAUTOCTO == 1,
                                                                                                 CLASSI_FIN == 10) %>% 
                                                                                          count() 
  )
  
  ###DENV I###
  
  PR_DENGUE_22_23_GERAL[which(PR_DENGUE_22_23_GERAL$Código_IBGE == i), 12]<- as.integer(PR_DENGUE_22_23_SINAN %>% 
                                                                                          filter(ID_MN_RESI == i, 
                                                                                                 SOROTIPO == 1) %>% 
                                                                                          count() 
  )
  
  ###DENV II###
  
  
  PR_DENGUE_22_23_GERAL[which(PR_DENGUE_22_23_GERAL$Código_IBGE == i), 13] <- as.integer(PR_DENGUE_22_23_SINAN %>% 
                                                                                           filter(ID_MN_RESI == i, 
                                                                                                  SOROTIPO == 2) %>% 
                                                                                           count() 
  )
  
  ###DENV III###
  
  PR_DENGUE_22_23_GERAL[which(PR_DENGUE_22_23_GERAL$Código_IBGE == i), 14] <- as.integer(PR_DENGUE_22_23_SINAN %>% 
                                                                                           filter(ID_MN_RESI == i, 
                                                                                                  SOROTIPO == 3) %>% 
                                                                                           count() 
  )
  ###DENV IV###                                     
  
  PR_DENGUE_22_23_GERAL[which(PR_DENGUE_22_23_GERAL$Código_IBGE == i), 15]<- as.integer(PR_DENGUE_22_23_SINAN %>% 
                                                                                          filter(ID_MN_RESI == i, 
                                                                                                 SOROTIPO == 4) %>% 
                                                                                          count() 
  )
  
  ###Óbitos###
  
  PR_DENGUE_22_23_GERAL[which(PR_DENGUE_22_23_GERAL$Código_IBGE == i), 16] <- as.integer(PR_DENGUE_22_23_SINAN %>% 
                                                                                           filter(ID_MN_RESI == i, 
                                                                                                  EVOLUCAO == 2) %>% 
                                                                                           count() 
  )
  
  ###Inconclusivos###
  
  PR_DENGUE_22_23_GERAL[which(PR_DENGUE_22_23_GERAL$Código_IBGE == i), 17] <-as.integer(PR_DENGUE_22_23_SINAN %>% 
                                                                                          filter(CLASSI_FIN == 8, 
                                                                                                 ID_MN_RESI == i) %>%
                                                                                          count() 
  )
  
  ###Importados###
  
  PR_DENGUE_22_23_GERAL[which(PR_DENGUE_22_23_GERAL$Código_IBGE == i), 18]<- as.integer(PR_DENGUE_22_23_SINAN %>% 
                                                                                          filter(ID_MN_RESI == i, 
                                                                                                 TPAUTOCTO == 2,
                                                                                                 CLASSI_FIN == 10) %>% 
                                                                                          count() 
  )
}

###Incidência###FORA DO LOOP###

PR_DENGUE_22_23_GERAL$Incidencia <- (PR_DENGUE_22_23_GERAL$Autoctones/PR_DENGUE_22_23_GERAL$População)*100000  
PR_DENGUE_22_23_GERAL$Incidencia <- format(round(PR_DENGUE_22_23_GERAL$Incidencia, 2))
PR_DENGUE_22_23_GERAL$Incidencia <- as.numeric(PR_DENGUE_22_23_GERAL$Incidencia)

PR_DENGUE_22_23_GERAL$Provaveis <- as.integer(PR_DENGUE_22_23_GERAL$Notificados) - as.integer(PR_DENGUE_22_23_GERAL$Descartados)


PR_DENGUE_22_23_GERAL$Incidencia_Provaveis <- (PR_DENGUE_22_23_GERAL$Provaveis/PR_DENGUE_22_23_GERAL$População)*100000  
PR_DENGUE_22_23_GERAL$Incidencia_Provaveis <- format(round(PR_DENGUE_22_23_GERAL$Incidencia_Provaveis, 2))
PR_DENGUE_22_23_GERAL$Incidencia_Provaveis <- as.numeric(PR_DENGUE_22_23_GERAL$Incidencia_Provaveis)

PR_DENGUE_22_23_GERAL$Em_Investigacao <- as.integer(PR_DENGUE_22_23_GERAL$Notificados) - as.integer(PR_DENGUE_22_23_GERAL$Dengue + PR_DENGUE_22_23_GERAL$D_S_A + PR_DENGUE_22_23_GERAL$Dengue_Grave + PR_DENGUE_22_23_GERAL$Descartados)

PR_DENGUE_22_23_GERAL$Total <- as.integer(PR_DENGUE_22_23_GERAL$Dengue + PR_DENGUE_22_23_GERAL$D_S_A + PR_DENGUE_22_23_GERAL$Dengue_Grave) 

PR_DENGUE_22_23_GERAL <- PR_DENGUE_22_23_GERAL[, c(1, 3, 4, 5, 19, 6, 7, 8, 22, 9, 21, 17, 16, 10, 18, 11, 20, 12, 13,14, 15)]

################################################################################################
################################################################################################


#####################################################################################################################

#####     Planilhas Google Sheets. Realizando o download das planilhas do                  ######
#####     google sheets e fazendo o upload da planilha de notificações e Geral Resumida    ######

###              Upload de Notificações para posterior download da mesma planilha com 
###          as coordenadas. A planilha que irá subir para o google sheets é derivada 
###         da BASE DBF do SINAN e NÃO CONTÉM COORDENADAS. As coordenadas estão em planilha 
####             própria no google drive, preenchida pelos municípios, e é vinculada 
###                        no google sheets com esta planilha.####

RS22_22_23_SINAN_DECODIFICADO$SINAN <- as.numeric(as.character(RS22_22_23_SINAN_DECODIFICADO$SINAN))

sheet_write(RS22_22_23_SINAN_DECODIFICADO, ss = "https://docs.google.com/spreadsheets/d/1z-cXrCe0ZRMRBG2rqW2BmG09HEa4tMmplXhHMnLbRg4/edit#gid=668044240", 
            sheet = "Registros_SINAN")

sheet_write(PR_CHIK_22_23_GERAL, ss = "https://docs.google.com/spreadsheets/d/1DNR7tyVt2tZ6ItiwJN96Bb-I4nODOmcP0G_oG0Tf_28/edit?pli=1#gid=0", 
            sheet = "Chikungunya")

sheet_write(PR_DENGUE_22_23_GERAL, ss = "https://docs.google.com/spreadsheets/d/1DNR7tyVt2tZ6ItiwJN96Bb-I4nODOmcP0G_oG0Tf_28/edit?pli=1#gid=0", 
            sheet = "Dengue")

#PR_22_23_DENGUE_MUNICIPIOS <- read_sheet ("https://docs.google.com/spreadsheets/d/1DEidGb0GgH7jVWFQ3e4HS7-DPxu2969ydJAcoMpboUQ/edit#gid=0", 
#                                          sheet ="Dengue")

#PR_22_23_DENGUE_MUNICIPIOS <- PR_22_23_DENGUE_MUNICIPIOS[, -c(17:24)]

#PR_22_23_CHIKUNGUNYA_MUNICIPIOS <- read_sheet ("https://docs.google.com/spreadsheets/d/1DEidGb0GgH7jVWFQ3e4HS7-DPxu2969ydJAcoMpboUQ/edit#gid=0", 
#                                               sheet ="Chikungunya")

#colnames(PR_22_23_CHIKUNGUNYA_MUNICIPIOS)[1] <- "RS"

PR_ZIKA_22_23_GERAL <- read_sheet ("https://docs.google.com/spreadsheets/d/1DNR7tyVt2tZ6ItiwJN96Bb-I4nODOmcP0G_oG0Tf_28/edit?pli=1#gid=1817054379", 
                                   sheet ="Zika")


RS22_22_23_REDE_OVITRAMPAS <- read_sheet("https://docs.google.com/spreadsheets/d/1mAiDoUdGgVsTB1DK5LumL2oStZZvIkbCp5XsuafaEvE/edit#gid=863361484", 
                                         sheet = "Consolidado")

RS22_22_23_CICLOS_LOCALIDADES <- read_sheet("https://docs.google.com/spreadsheets/d/18hJMQnlFcRHeqNbtJObD0UCx8L-lOcJpOTYkn849Fcs/edit#gid=764914932")

###Substituindo NA por 200 na planilha. O QGIS não está reconhecendo NA e as variáveis ficam como String no SIG.###

RS22_22_23_CICLOS_LOCALIDADES[is.na(RS22_22_23_CICLOS_LOCALIDADES)] <- 200

RS22_22_23_CICLOS_MUNICIPIOS <- read_sheet("https://docs.google.com/spreadsheets/d/1P1fC2Z3R8yyaF2_P7wUcDEtwKqfq8MWcf2R5vmmw-kM/edit#gid=1734395963")

###Substituindo NA por 200 (QGIS não está reconhecendo NA. O SIG importa a planilha como character e não possibilita realizar análise dos dados de forma numérica)

RS22_22_23_CICLOS_MUNICIPIOS[is.na(RS22_22_23_CICLOS_MUNICIPIOS)] <- 200

RS22_22_23_RG_MUNICIPIOS <- read_sheet("https://docs.google.com/spreadsheets/d/1QILHWnVa1m2Lr4qqf02VejbCrPQaqkr1w51QNbxkaXk/edit#gid=1585473376")

###Por alguma razão a planilha veio como lista###

RS22_22_23_RG_MUNICIPIOS <- as.data.frame(lapply(RS22_22_23_RG_MUNICIPIOS, 
                                                 unlist))

RS22_22_23_RG_LOCALIDADES <- read_sheet("https://docs.google.com/spreadsheets/d/1js80T20EU2FvfqTLsjutPI7Zg93AaOhhbl8-wBD18FA/edit#gid=877642872")

RS22_22_23_PE <- read_sheet("https://docs.google.com/spreadsheets/d/1fW7nGY_h17JqeuV37rnk71sZvxyB1_D0rynci5m-dI8/edit#gid=863361484", 
                            sheet = "Consolidado")

####Por alguma razão a planilha veio como uma lista####

RS22_22_23_PE <- as.data.frame(lapply(RS22_22_23_PE, 
                                      unlist))

RS22_22_23_ASSISTENCIA <- read_sheet("https://docs.google.com/spreadsheets/d/1SCe_xImlW3cExZ2AzbfCQc51OPo9JETqAJkSvbGn8rk/edit#gid=863361484",
                                     sheet = "Consolidado")

###Upload tabelas a serem usadas nos Dashboards mmunicipais

sheet_write(RS22_22_23_SE_Notificados, ss = "https://docs.google.com/spreadsheets/d/1F8Yij8Opo5lcv9mlgVh-N37hEOrI9mnKw-mc2j4kQhk/edit#gid=0", 
            sheet = "Notificados")

sheet_write(RS22_22_23_SE_Confirmados, ss = "https://docs.google.com/spreadsheets/d/1F8Yij8Opo5lcv9mlgVh-N37hEOrI9mnKw-mc2j4kQhk/edit#gid=0", 
            sheet = "Confirmados")

sheet_write(RS22_22_23_EXTRA, ss = "https://docs.google.com/spreadsheets/d/1F8Yij8Opo5lcv9mlgVh-N37hEOrI9mnKw-mc2j4kQhk/edit#gid=0", 
            sheet = "EXTRA")

sheet_write(RS22_22_23_SINAIS_Notificados, ss = "https://docs.google.com/spreadsheets/d/1F8Yij8Opo5lcv9mlgVh-N37hEOrI9mnKw-mc2j4kQhk/edit#gid=0", 
            sheet = "Sinais_Notificados")

sheet_write(RS22_22_23_SINAIS_Confirmados, ss = "https://docs.google.com/spreadsheets/d/1F8Yij8Opo5lcv9mlgVh-N37hEOrI9mnKw-mc2j4kQhk/edit#gid=0", 
            sheet = "Sinais_Confirmados")

sheet_write(RS22_22_23_GERAL, ss = "https://docs.google.com/spreadsheets/d/1F8Yij8Opo5lcv9mlgVh-N37hEOrI9mnKw-mc2j4kQhk/edit#gid=0", 
            sheet = "GERAL")

sheet_write(RS22_22_23_DOENCAS_PRE_EXISTENTES, ss = "https://docs.google.com/spreadsheets/d/1F8Yij8Opo5lcv9mlgVh-N37hEOrI9mnKw-mc2j4kQhk/edit#gid=0", 
            sheet = "Doencas_Pre_Existentes")

###Criando tabela Geral Resumida para utilização no Informe###

RS22_22_23_Resumida <- tibble(Notificados = sum(RS22_22_23_GERAL$Notificados),
                              Dengue = sum(RS22_22_23_GERAL$Dengue),
                              DSA = sum(RS22_22_23_GERAL$D_S_A),
                              Dengue_Grave = sum(RS22_22_23_GERAL$Dengue_Grave),
                              Obitos = sum(RS22_22_23_GERAL$Obitos)
)

sheet_write(RS22_22_23_Resumida, ss = "https://docs.google.com/spreadsheets/d/1bAPfOaZfUOf7ZP8-sxNLXa91YRGJ9QtnBL5cFeLpJPc/edit#gid=0", 
            sheet = "Resumo")

#####Salvando as tabelas#####

####  Fazendo o Dowload da planilha estadual do google drive para manter controle de sorotipos igual Estado.  ###
#### foi feito upload prévio da planilha baseada no SINAN. 

PR_DENGUE_22_23_GERAL <- read_sheet("https://docs.google.com/spreadsheets/d/1DNR7tyVt2tZ6ItiwJN96Bb-I4nODOmcP0G_oG0Tf_28/edit?pli=1#gid=111480216", 
                                    sheet= "Dengue_Estado")

PR_DENGUE_22_23_GERAL <- PR_DENGUE_22_23_GERAL[, -c(22:29)]

###########Incluindo Sorotipos na Planilha RS22_22_23_GERAL. Essa etapa está sendo realizada somente agora pois depende da tabela 
###########PR_22_23_DENGUE_MUNICÍPIOS, a qual só foi realizado o download neste ponto do script###########

for (i in RS22_22_23_GERAL$Município){
  RS22_22_23_GERAL[which(RS22_22_23_GERAL$Município  == i), 22] <- as.character(PR_DENGUE_22_23_GERAL[which(PR_DENGUE_22_23_GERAL$Município_sem_Código  == i), 22])
}

write.csv(PR_DENGUE_22_23_GERAL, 
          "/home/gustavo/Área de Trabalho/Análise_de_Dados/Base_de_Dados/Tabulacoes_R/Arboviroses/PR_22_23_DENGUE_MUNICIPIOS.csv",
          row.names = FALSE)

write.csv(PR_CHIK_22_23_GERAL, 
          "/home/gustavo/Área de Trabalho/Análise_de_Dados/Base_de_Dados/Tabulacoes_R/Arboviroses/PR_22_23_CHIKUNGUNYA_MUNICIPIOS.csv",
          row.names = FALSE)

write.csv(PR_ZIKA_22_23_GERAL, 
          "/home/gustavo/Área de Trabalho/Análise_de_Dados/Base_de_Dados/Tabulacoes_R/Arboviroses/PR_22_23_ZIKA_MUNICIPIOS.csv",
          row.names = FALSE)

write.csv(RS22_22_23_REDE_OVITRAMPAS, 
          "/home/gustavo/Área de Trabalho/Análise_de_Dados/Base_de_Dados/Tabulacoes_R/Arboviroses/RS22_22_23_REDE_OVITRAMPAS.csv",
          row.names = FALSE)

write.csv(RS22_22_23_CICLOS_LOCALIDADES, 
          "/home/gustavo/Área de Trabalho/Análise_de_Dados/Base_de_Dados/Tabulacoes_R/Arboviroses/RS22_22_23_CICLOS_LOCALIDADES.csv",
          row.names = FALSE)

write.csv(RS22_22_23_CICLOS_MUNICIPIOS, 
          "/home/gustavo/Área de Trabalho/Análise_de_Dados/Base_de_Dados/Tabulacoes_R/Arboviroses/RS22_22_23_CICLOS_MUNICIPIOS.csv",
          row.names = FALSE)

write.csv(RS22_22_23_RG_MUNICIPIOS, 
          "/home/gustavo/Área de Trabalho/Análise_de_Dados/Base_de_Dados/Tabulacoes_R/Arboviroses/RS22_22_23_RG_MUNICIPIOS.csv",
          row.names = FALSE)

write.csv(RS22_22_23_RG_LOCALIDADES, 
          "/home/gustavo/Área de Trabalho/Análise_de_Dados/Base_de_Dados/Tabulacoes_R/Arboviroses/RS22_22_23_RG_LOCALIDADES.csv",
          row.names = FALSE)

write.csv(RS22_22_23_PE, 
          "/home/gustavo/Área de Trabalho/Análise_de_Dados/Base_de_Dados/Tabulacoes_R/Arboviroses/RS22_22_23_PE.csv",
          row.names = FALSE)

write.csv(RS22_22_23_ASSISTENCIA, 
          "/home/gustavo/Área de Trabalho/Análise_de_Dados/Base_de_Dados/Tabulacoes_R/Arboviroses/RS22_22_23_ASSISTENCIA.csv",
          row.names = FALSE)

write.csv(RS22_22_23_GERAL, 
          "/home/gustavo/Área de Trabalho/Análise_de_Dados/Base_de_Dados/Tabulacoes_R/Arboviroses/RS22_22_23_GERAL.csv",
          row.names = FALSE)

write.csv(RS22_22_23_EXTRA, 
          "/home/gustavo/Área de Trabalho/Análise_de_Dados/Base_de_Dados/Tabulacoes_R/Arboviroses/RS22_22_23_EXTRA.csv",
          row.names = FALSE)

write.csv(RS22_22_23_SINAN, 
          "/home/gustavo/Área de Trabalho/Análise_de_Dados/Base_de_Dados/Tabulacoes_R/Arboviroses/RS22_22_23_SINAN.csv",
          row.names = FALSE)

write.csv(RS22_22_23_SINAN_DECODIFICADO, 
          "/home/gustavo/Área de Trabalho/Análise_de_Dados/Base_de_Dados/Tabulacoes_R/Arboviroses/RS22_22_23_SINAN_DECODIFICADO.csv",
          row.names = FALSE)

write.csv(RS22_22_23_SINAIS_DE_ALARME, 
          "/home/gustavo/Área de Trabalho/Análise_de_Dados/Base_de_Dados/Tabulacoes_R/Arboviroses/RS22_22_23_SINAIS_DE_ALARME.csv",
          row.names = FALSE)

write.csv(RS22_22_23_SINAIS_Notificados, 
          "/home/gustavo/Área de Trabalho/Análise_de_Dados/Base_de_Dados/Tabulacoes_R/Arboviroses/RS22_22_23_SINAIS_Notificados.csv",
          row.names = FALSE)

write.csv(RS22_22_23_SINAIS_Confirmados, 
          "/home/gustavo/Área de Trabalho/Análise_de_Dados/Base_de_Dados/Tabulacoes_R/Arboviroses/RS22_22_23_SINAIS_Confirmados.csv",
          row.names = FALSE)

write.csv(RS22_22_23_SE_Confirmados, 
          "/home/gustavo/Área de Trabalho/Análise_de_Dados/Base_de_Dados/Tabulacoes_R/Arboviroses/RS22_22_23_SE_Confirmados.csv",
          row.names = FALSE)

write.csv(RS22_22_23_SE_Notificados, 
          "/home/gustavo/Área de Trabalho/Análise_de_Dados/Base_de_Dados/Tabulacoes_R/Arboviroses/RS22_22_23_SE_Notificados.csv",
          row.names = FALSE)

write.csv(RS22_22_23_DENGUE_GRAVE, 
          "/home/gustavo/Área de Trabalho/Análise_de_Dados/Base_de_Dados/Tabulacoes_R/Arboviroses/RS22_22_23_SE_DENGUE_GRAVE.csv",
          row.names = FALSE)

write.csv(RS22_22_23_DOENCAS_PRE_EXISTENTES, 
          "/home/gustavo/Área de Trabalho/Análise_de_Dados/Base_de_Dados/Tabulacoes_R/Arboviroses/RS22_22_23_DOENCAS_PRE_EXISTENTES.csv",
          row.names = FALSE)

write.csv (RS22_Serie_Historica, 
           "/home/gustavo/Área de Trabalho/Análise_de_Dados/Base_de_Dados/Tabulacoes_R/Arboviroses/RS22_Serie_Historica.csv", 
           row.names = FALSE)


####     Buscando a planilha RS22_22_23_SINAN_DECODIFICADO do google sheets com as coordenadas geográficas inseridas pelos municípios   ####

RS22_22_23_SINAN_DECODIFICADO <- read_sheet("https://docs.google.com/spreadsheets/d/16TyAkaCH-ZE6Xq7AfEGXGvX0jRDbxj8QUvzNQcw8zFQ/edit#gid=1437725143", 
                                            sheet= "SINAN")

####      Gravando a planilha RS22_22_23_SINAN_DECODIFICADO no diretório para ser utilizada pelo QGIS    ###

write.csv(RS22_22_23_SINAN_DECODIFICADO,  "/home/gustavo/Área de Trabalho/Análise_de_Dados/Base_de_Dados/Tabulacoes_R/Arboviroses/RS22_22_23_SINAN_DECODIFICADO.csv",
          row.names = FALSE)

###    MAPAS      ###
###########################################################################################################
##############   Abaixo, preparando o objeto PR_DENGUE_22_23 para entrar no left-Join  ####################
##############   com o read_municipality do geobr. Futuramente, padronizar o Base_IBGE ####################
###########################################################################################################

#####   Criando o Mapa Regional Dissolvida  e Mapa Base ####

RS22_Dissolvida <- read_health_region(year = 2013)

RS22_Dissolvida <- RS22_Dissolvida %>% filter(code_state == 41,
                                              code_health_region == 41022)

MAPA_BASE <- read_municipality(code_muni = "PR", year = 2020)

MAPA_BASE$name_muni <- toupper(MAPA_BASE$name_muni)

#####   Trabalhando os Mapas de dengue estaduais   ###

PR_DENGUE_22_23_GERAL[104, 2] <- "BELA VISTA DA CAROBA"
PR_DENGUE_22_23_GERAL[360, 2] <- "DIAMANTE D'OESTE"
PR_DENGUE_22_23_GERAL[93, 2] <- "ITAPEJARA D'OESTE"
PR_DENGUE_22_23_GERAL[265, 2] <- "MUNHOZ DE MELO"
PR_DENGUE_22_23_GERAL[117, 2] <- "PÉROLA D'OESTE"
PR_DENGUE_22_23_GERAL[184, 2] <- "RANCHO ALEGRE D'OESTE"
PR_DENGUE_22_23_GERAL[239, 2] <- "SANTA CRUZ DE MONTE CASTELO"
PR_DENGUE_22_23_GERAL[127, 2] <- "SÃO JORGE D'OESTE"

####  SOROTIPOS ESTADO   ##########

MAPA_BASE_PR <- left_join(MAPA_BASE, PR_DENGUE_22_23_GERAL, by = c("name_muni" = "Município_sem_Código"))

######  Código abaixo pode ser utilizado para o mesmo propósito no próximo  ##############
######  Período sazonal.                                                    ##############

AUX_I <- MAPA_BASE_PR %>% filter(DENV_I > 0 & DENV_II == 0 & DENV_III == 0 & DENV_IV == 0)
AUX_II <- MAPA_BASE_PR %>% filter(DENV_I == 0 & DENV_II > 0 & DENV_III == 0 & DENV_IV == 0)
AUX_III <- MAPA_BASE_PR %>% filter(DENV_I > 0 & DENV_II > 0 & DENV_III == 0 & DENV_IV == 0)
AUX_IV <- MAPA_BASE_PR %>% filter(DENV_I > 0 & DENV_II > 0 & DENV_III > 0 & DENV_IV == 0)
AUX_V <- MAPA_BASE_PR %>% filter(DENV_I > 0 & DENV_II == 0 & DENV_III > 0 & DENV_IV == 0)
AUX_VI <- MAPA_BASE_PR %>% filter(DENV_I == 0 & DENV_II > 0 & DENV_III > 0 & DENV_IV == 0)

PR_22_23_GRAF_SOROTIPO_PR <- ggplot() + 
  geom_sf(data = MAPA_BASE_PR, 
          color = "black", 
          fill = "white") + 
  geom_sf(data = AUX_I, 
          color ="black", 
          aes(fill = "I")) +  
  geom_sf(data = AUX_II, 
          color ="black", 
          aes(fill = "II")) + 
  geom_sf(data = AUX_III, 
          color ="black", 
          aes(fill = "I, II")) + 
  geom_sf(data = AUX_IV, 
          color ="black", 
          aes(fill = "I, II, III")) +
  geom_sf(data = AUX_V, 
          color ="black", 
          aes(fill = "I, III")) + 
  annotation_scale(location = "br") +
  annotation_north_arrow(location = "tr", 
                         which_north = "true") +
  theme_minimal() +
  scale_fill_manual (name = "Sorotipos",
                     values = c("I" = "green", 
                                "II" = "blue",
                                "I, II" = "#2F4F4F",
                                "I, III" ="#A0522D")) +
  theme(legend.position = "bottom") +
  coord_sf(expand = FALSE)+
  labs(caption = Fonte, 
       title = "Sorotipo Circulante - Paraná") + 
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold", 
                                   size = 14,
                                   colour = "#556B2F")
  ) +
  geom_sf(data = RS22_Dissolvida, 
          fill = "grey") +
  geom_sf_label(data = RS22_Dissolvida, 
                aes(label = "22 RS"),
                size = 4,
                position = "identity")

### Mapa Incidência Paraná  ####

MAPA_BASE_PR$Cat <- with(MAPA_BASE_PR, cut(x = Incidencia,
                                           breaks = c(-Inf, 0, 50, 100, 300, 500, Inf),
                                           labels = c("0 casos", "0,001 - 50", "50,001 - 100", 
                                                      "100,001 - 300", "300,001 - 500", ">500"))
)

PR_22_23_GRAF_INCIDENCIA_PR <- ggplot() + 
  geom_sf(data = MAPA_BASE_PR, 
          color = "black", 
          aes(fill = Cat)) +
  annotation_scale(location = "br") +
  annotation_north_arrow(location = "tr", 
                         which_north = "true") +
  theme_minimal() +
  scale_fill_manual (name = "Casos/100.000 hab",
                     values = c("0 casos" = "white", 
                                "0,001 - 50" = "#FDF5E6",
                                "50,001 - 100" = "#EEE8AA",
                                "100,001 - 300" ="#FFD700",
                                "300,001 - 500" = "#DAA520",
                                ">500" = "#DC143C")) +
  theme(legend.position = "bottom") +
  coord_sf(expand = FALSE)+
  labs(caption = Fonte, 
       title = "Incidência de Dengue - Paraná",
       subtitle = "Casos Autóctones por 100.000 Habitantes") + 
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold", 
                                   size = 14,
                                   colour = "#556B2F")
  ) +
  geom_sf(data = RS22_Dissolvida, 
          fill = "grey") +
  geom_sf_label(data = RS22_Dissolvida, 
                aes(label = "22 RS"),
                size = 4,
                position = "identity")

### Mapa Incidência Paraná  ####

MAPA_BASE_PR$Cat <- with(MAPA_BASE_PR, cut(x = Incidencia_Provaveis,
                                           breaks = c(-Inf, 0, 50, 100, 300, 500, Inf),
                                           labels = c("0 casos", "0,001 - 50", "50,001 - 100", 
                                                      "100,001 - 300", "300,001 - 500", ">500"))
)

PR_22_23_GRAF_INCIDENCIA_PROV_PR <- ggplot() + 
  geom_sf(data = MAPA_BASE_PR, 
          color = "black", 
          aes(fill = Cat)) +
  annotation_scale(location = "br") +
  annotation_north_arrow(location = "tr", 
                         which_north = "true") +
  theme_minimal() +
  scale_fill_manual (name = "Casos/100.000 hab",
                     values = c("0 casos" = "white", 
                                "0,001 - 50" = "#FDF5E6",
                                "50,001 - 100" = "#EEE8AA",
                                "100,001 - 300" ="#FFD700",
                                "300,001 - 500" = "#DAA520",
                                ">500" = "#DC143C")) +
  theme(legend.position = "bottom") +
  coord_sf(expand = FALSE) +
  labs(caption = Fonte, 
       title = "Incidência de Dengue - Paraná",
       subtitle = "Casos Prováveis por 100.000 Habitantes
Casos Prováveis = Casos Notificados - Casos Descartados") + 
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold", 
                                   size = 14,
                                   colour = "#556B2F")
  ) +
  geom_sf(data = RS22_Dissolvida, 
          fill = "grey") +
  geom_sf_label(data = RS22_Dissolvida, 
                aes(label = "22 RS"),
                size = 4,
                position = "identity")

#######   Chikungunya    #######

PR_CHIK_22_23_GERAL$Provaveis <- as.integer(PR_CHIK_22_23_GERAL$Notificados) - as.integer(PR_CHIK_22_23_GERAL$Descartados)

PR_CHIK_22_23_GERAL[104, 2] <- "BELA VISTA DA CAROBA"
PR_CHIK_22_23_GERAL[360, 2] <- "DIAMANTE D'OESTE"
PR_CHIK_22_23_GERAL[93, 2] <- "ITAPEJARA D'OESTE"
PR_CHIK_22_23_GERAL[265, 2] <- "MUNHOZ DE MELO"
PR_CHIK_22_23_GERAL[117, 2] <- "PÉROLA D'OESTE"
PR_CHIK_22_23_GERAL[184, 2] <- "RANCHO ALEGRE D'OESTE"
PR_CHIK_22_23_GERAL[239, 2] <- "SANTA CRUZ DE MONTE CASTELO"
PR_CHIK_22_23_GERAL[127, 2] <- "SÃO JORGE D'OESTE"

MAPA_BASE_PR <- left_join(MAPA_BASE, PR_CHIK_22_23_GERAL, by = c("name_muni" = "Município_sem_Código"))

###### Mapa Notificados   ############################

MAPA_BASE_PR$Cat <- with(MAPA_BASE_PR, cut(x = Notificados,
                                           breaks = c(-Inf, 0, 10, 50, 100, 500, Inf),
                                           labels = c("0 casos", "0 - 10", "11 - 50", 
                                                      "51 - 100", "101 - 500", ">500"))
)

PR_22_23_GRAF_CHIK_Notificados <- ggplot() + 
  geom_sf(data = MAPA_BASE_PR, 
          color = "black", 
          aes(fill = Cat))  +
  annotation_north_arrow(location = "tr", 
                         which_north = "true") +
  annotation_scale(location = "br") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold", 
                                   size = 14,
                                   colour = "#556B2F")) +
  scale_fill_manual (name = "Notificações", 
                     values = c("0 casos" = "white", 
                                "0 - 10" = "#FDF5E6",    
                                "11 - 50" = "#EEE8AA",
                                "51 - 100" = "#FFD700",
                                "101 - 500" = "#DAA520",
                                ">500" = "#DC143C")) +
  theme(legend.position = "bottom") +
  coord_sf(expand = FALSE) +
  labs(caption = Fonte, 
       title = "Chikungunya - Paraná",
       subtitle = "Casos Notificados") +
  geom_sf(data = RS22_Dissolvida, fill = "grey") +
  geom_sf_label(data = RS22_Dissolvida, 
                aes(label = "22 RS"),
                size = 4,
                position = "identity")

###### Mapa Incidência  ############################

MAPA_BASE_PR$Cat <- with(MAPA_BASE_PR, cut(x = Incidencia,
                                           breaks = c(-Inf, 0, 50, 100, 300, 500, Inf),
                                           labels = c("0 casos", "0,001 - 50", "50,001 - 100", 
                                                      "100,001 - 300", "300,001 - 500", ">500")
)
)

PR_22_23_GRAF_CHIK_Incidência <- ggplot() + 
  geom_sf(data = MAPA_BASE_PR, 
          color = "black", 
          aes(fill = Cat))  +
  annotation_north_arrow(location = "tr", 
                         which_north = "true") +
  annotation_scale(location = "br") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold", 
                                   size = 14,
                                   colour = "#556B2F")) +
  scale_fill_manual (name = "Casos/100.000 hab",
                     values = c("0 casos" = "white", 
                                "0,001 - 50" = "#FDF5E6",
                                "50,001 - 100" = "#EEE8AA",
                                "100,001 - 300" ="#FFD700",
                                "300,001 - 500" = "#DAA520",
                                ">500" = "#DC143C")) +
  theme(legend.position = "bottom") +
  coord_sf(expand = FALSE) +
  labs(caption = Fonte, 
       title = "Chikungunya - Paraná",
       subtitle = "Incidência - Casos por 100.000 hab") +
  geom_sf(data = RS22_Dissolvida, fill = "grey") +
  geom_sf_label(data = RS22_Dissolvida, 
                aes(label = "22 RS"),
                size = 4,
                position = "identity")

##############    ZIKA    ##########################################

PR_ZIKA_22_23_GERAL[104, 2] <- "BELA VISTA DA CAROBA"
PR_ZIKA_22_23_GERAL[360, 2] <- "DIAMANTE D'OESTE"
PR_ZIKA_22_23_GERAL[93, 2] <- "ITAPEJARA D'OESTE"
PR_ZIKA_22_23_GERAL[265, 2] <- "MUNHOZ DE MELO"
PR_ZIKA_22_23_GERAL[117, 2] <- "PÉROLA D'OESTE"
PR_ZIKA_22_23_GERAL[184, 2] <- "RANCHO ALEGRE D'OESTE"
PR_ZIKA_22_23_GERAL[239, 2] <- "SANTA CRUZ DE MONTE CASTELO"
PR_ZIKA_22_23_GERAL[127, 2] <- "SÃO JORGE D'OESTE"

MAPA_BASE_PR <- left_join(MAPA_BASE, PR_ZIKA_22_23_GERAL, by = c("name_muni" = "MUNICÍPIO"))

###### Mapa Notificados   ############################

MAPA_BASE_PR$Cat <- with(MAPA_BASE_PR, cut(x = NOTIFICADOS,
                                           breaks = c(-Inf, 0, 10, 50, 100, 500, Inf),
                                           labels = c("0 casos", "0 - 10", "11 - 50", 
                                                      "51 - 100", "101 - 500", ">500"))
)

PR_22_23_ZIKA_CHIK_Notificados <- ggplot() + 
  geom_sf(data = MAPA_BASE_PR, 
          color = "black", 
          aes(fill = Cat))  +
  annotation_north_arrow(location = "tr", 
                         which_north = "true") +
  annotation_scale(location = "br") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold", 
                                   size = 14,
                                   colour = "#556B2F")) +
  scale_fill_manual (name = "Notificações", 
                     values = c("0 casos" = "white", 
                                "0 - 10" = "#FDF5E6",    
                                "11 - 50" = "#EEE8AA",
                                "51 - 100" = "#FFD700",
                                "101 - 500" = "#DAA520",
                                ">500" = "#DC143C")) +
  theme(legend.position = "bottom") +
  coord_sf(expand = FALSE) +
  labs(caption = Fonte, 
       title = "Zika - Paraná",
       subtitle = "Casos Notificados") +
  geom_sf(data = RS22_Dissolvida, fill = "grey") +
  geom_sf_label(data = RS22_Dissolvida, 
                aes(label = "22 RS"),
                size = 4,
                position = "identity")

###### Mapa Incidência  ############################

MAPA_BASE_PR$Cat <- with(MAPA_BASE_PR, cut(x = INCIDENCIA,
                                           breaks = c(-Inf, 0, 50, 100, 300, 500, Inf),
                                           labels = c("0 casos", "0,001 - 50", "50,001 - 100", 
                                                      "100,001 - 300", "300,001 - 500", ">500")
)
)

PR_22_23_GRAF_ZIKA_Incidência <- ggplot() + 
  geom_sf(data = MAPA_BASE_PR, 
          color = "black", 
          aes(fill = Cat))  +
  annotation_north_arrow(location = "tr", 
                         which_north = "true") +
  annotation_scale(location = "br") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold", 
                                   size = 14,
                                   colour = "#556B2F")) +
  scale_fill_manual (name = "Casos/100.000 hab",
                     values = c("0 casos" = "white", 
                                "0,001 - 50" = "#FDF5E6",
                                "50,001 - 100" = "#EEE8AA",
                                "100,001 - 300" ="#FFD700",
                                "300,001 - 500" = "#DAA520",
                                ">500" = "#DC143C")) +
  theme(legend.position = "bottom") +
  coord_sf(expand = FALSE) +
  labs(caption = Fonte, 
       title = "Zika - Paraná",
       subtitle = "Incidência - Casos por 100.000 hab") +
  geom_sf(data = RS22_Dissolvida, fill = "grey") +
  geom_sf_label(data = RS22_Dissolvida, 
                aes(label = "22 RS"),
                size = 4,
                position = "identity")


#####  Mapas Regional   #####

MAPA_BASE_RS <- left_join(MAPA_BASE, RS22_22_23_GERAL, 
                          by = c("name_muni" = "Município")
)

MAPA_BASE_RS <- MAPA_BASE_RS %>% filter(RS == 22)

AUX_MAP <- MAPA_BASE_RS %>% filter(Sorotipos == "I,")
AUX_MAP_1 <- MAPA_BASE_RS %>% filter(Sorotipos == "II,")
AUX_MAP_2 <- MAPA_BASE_RS %>% filter(Sorotipos == "III,")
AUX_MAP_3 <- MAPA_BASE_RS %>% filter(Sorotipos == "I,II,")
AUX_MAP_4 <- MAPA_BASE_RS %>% filter(Sorotipos == "I,II,III,")
AUX_MAP_4 <- MAPA_BASE_RS %>% filter(Sorotipos == "II,III,")

RS22_22_23_GRAF_Sorotipo <- ggplot() + 
  geom_sf(data = MAPA_BASE_RS, 
          color = "black", 
          fill = "white") + 
  geom_sf(data = AUX_MAP, 
          color ="black", 
          aes(fill = "I")) +  
  geom_sf(data = AUX_MAP_1, 
          color ="black", 
          aes(fill = "II")) + 
  geom_sf(data = AUX_MAP_2, 
          color ="black", 
          aes(fill = "III")) + 
  geom_sf(data = AUX_MAP_3, 
          color ="black", 
          aes(fill = "I, II")) + 
  annotation_scale(location = "br") +
  annotation_north_arrow(location = "tr", 
                         which_north = "true") +
  coord_sf(expand = FALSE) +
  scale_fill_manual (name = "Sorotipos",
                     values = c("I" = "green", 
                                "II" = "blue",
                                "I, II" = "#228B22")) +
  theme(legend.position = "bottom") +
  labs(caption = Fonte, 
       title = "Sorotipo Circulante - 22ªRS") + 
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold", 
                                   size = 14,
                                   colour = "#556B2F")
  ) 

#########################################################################################################################
#########################   Mapa Chikungunya notificados REGIONAL   #####################################################

MAPA_BASE_RS <- left_join(MAPA_BASE, PR_CHIK_22_23_GERAL, 
                          by = c("name_muni" = "Município_sem_Código")
)

MAPA_BASE_RS <- MAPA_BASE_RS %>% filter(RS == 22)

MAPA_BASE_RS$Cat <- with(MAPA_BASE_RS, cut(x = Notificados,
                                           breaks = c(-Inf, 0, 5, 10, 50, 100, 500, Inf),
                                           labels = c("0 casos", "0 - 5", "6 - 10", "11 - 50", 
                                                      "51 - 100", "101 - 500", ">500"))
)

RS22_22_23_GRAF_CHK_Not <- ggplot() + 
  geom_sf(data = MAPA_BASE_RS, 
          color = "black", 
          aes(fill = Cat)) + 
  annotation_scale(location = "br") +
  annotation_north_arrow(location = "tr", 
                         which_north = "true") +
  coord_sf(expand = FALSE) +
  scale_fill_manual (name = "Notificações", 
                     values = c("0 casos" = "white", 
                                "0 - 5" = "#FDF5E6",
                                "6 - 10" = "#EEE8AA",    
                                "11 - 50" = "#FFD700",
                                "51 - 100" = "#DAA520",
                                "101 - 500" = "#FF8C00",
                                ">500" = "#DC143C")) +
  theme(legend.position = "bottom") +
  labs(caption = Fonte, 
       title = "Chikungunya Notificados - 22ªRS") + 
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold", 
                                   size = 14,
                                   colour = "#556B2F")
  ) 

#########################################################################################################################
#########################   Mapa Chikungunya Confirmados REGIONAL   #####################################################

MAPA_BASE_RS <- left_join(MAPA_BASE, PR_CHIK_22_23_GERAL, 
                          by = c("name_muni" = "Município_sem_Código")
)

MAPA_BASE_RS <- MAPA_BASE_RS %>% filter(RS == 22)

MAPA_BASE_RS$Cat <- with(MAPA_BASE_RS, cut(x = Incidencia,
                                           breaks = c(-Inf, 0, 50, 100, 300, 500, Inf),
                                           labels = c("0 casos", "0,001 - 50", "50,001 - 100", 
                                                      "100,001 - 300", "300,001 - 500", ">500")
)
)

RS22_22_23_GRAF_CHK_Conf <- ggplot() + 
  geom_sf(data = MAPA_BASE_RS, 
          color = "black", 
          aes(fill = Cat)) + 
  annotation_scale(location = "br") +
  annotation_north_arrow(location = "tl", 
                         which_north = "true") +
  theme_minimal() +
  scale_fill_manual (name = "Casos/100.000 hab",
                     values = c("0 casos" = "white", 
                                "0,001 - 50" = "#FDF5E6",
                                "50,001 - 100" = "#EEE8AA",
                                "100,001 - 300" ="#FFD700",
                                "300,001 - 500" = "#DAA520",
                                ">500" = "#DC143C")) +
  theme(legend.position = "bottom") +
  coord_sf(expand = FALSE) +
  labs(caption = Fonte, 
       title = "Chikungunya Incidência - 22ªRS",
       subtitle = "Casos AUTÓCTONES/100.000 habitantes")+ 
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold", 
                                   size = 14,
                                   colour = "#556B2F")
  ) 

#########################################################################################################################
##################################   Entomologia   ######################################################################
#########################   Mapa IIP REGIONAL  4º Ciclo/2022     ########################################################

MAPA_BASE_RS <- left_join(MAPA_BASE, RS22_22_23_CICLOS_MUNICIPIOS, 
                          by = c("name_muni" = "Município")
)

AUX_MAP <- MAPA_BASE_RS %>% filter(`4CICLO_2022_IIP` > 199)

AUX_MAP$Cat  <- "S/I"

AUX_MAP$`4CICLO_2022_IIP` <- "S/I"

MAPA_BASE_RS <- MAPA_BASE_RS %>% filter(`4CICLO_2022_IIP` >= 0 & `4CICLO_2022_IIP` < 200)

MAPA_BASE_RS$Cat <- with(MAPA_BASE_RS, cut(x = `4CICLO_2022_IIP`,
                                           breaks = c(-Inf, 1, 4, Inf),
                                           labels = c("0 - 0,9", "1 - 3,9", 
                                                      "> 4"))
)

MAPA_BASE_RS$`4CICLO_2022_IIP` <- format(round(MAPA_BASE_RS$`4CICLO_2022_IIP`, 2))

MAPA_BASE_RS <-rbind(MAPA_BASE_RS, AUX_MAP)

RS22_22_23_GRAF_IIP_Ciclo4 <- ggplot() + 
  geom_sf(data = MAPA_BASE_RS, 
          color = "black", 
          aes(fill = Cat)) + 
  coord_sf(expand = FALSE) +
  geom_sf_label(data = MAPA_BASE_RS, 
                aes(label = `4CICLO_2022_IIP`),
                label.padding = unit(0.5, "mm"),
                size = 3,
                position = "identity") +
  scale_fill_manual (name = "",
                     values = c("0 - 0,9" = "green",
                                "1 - 3,9" = "yellow",
                                "> 4" = "red",
                                "S/I" = "white")
  ) +
  annotation_scale(location = "br") +
  annotation_north_arrow(location = "tr", 
                         which_north = "true") +
  theme(legend.position = "right") +
  labs(x = NULL,
       y = NULL,
       caption = Fonte, 
       title = "Índice de Infestação Predial 4º Ciclo/2022 - 22ªRS") + 
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold", 
                                   size = 14,
                                   colour = "#556B2F")
  ) 

###########    Tratamento 4º Ciclo  ###################################

MAPA_BASE_RS$Cat <- with(MAPA_BASE_RS, cut(x = `4ºCICLO_2022_%_Visitas`,
                                           breaks = c(-Inf, 30, 60, 80, Inf),
                                           labels = c("< 30%", "30,01 a 60%", 
                                                      "60,01 a 80%", "> 80%"))
)

MAPA_BASE_RS$`4ºCICLO_2022_%_Visitas` <- format(round(MAPA_BASE_RS$`4ºCICLO_2022_%_Visitas`, 2))

RS22_22_23_GRAF_Tratamento_Ciclo4 <- ggplot() + 
  geom_sf(data = MAPA_BASE_RS, 
          color = "black", 
          aes(fill = Cat)) + 
  coord_sf(expand = FALSE) +
  geom_sf_label(data = MAPA_BASE_RS, 
                aes(label = `4ºCICLO_2022_%_Visitas`),
                label.padding = unit(0.5, "mm"),
                size = 3,
                position = "identity") +
  scale_fill_manual (name = "",
                     values = c("< 30%" = "red",
                                "30,01 a 60%" = "#FFA07A",
                                "60,01 a 80%" = "yellow",
                                "> 80%" = "green")
  ) +
  annotation_scale(location = "br") +
  annotation_north_arrow(location = "tr", 
                         which_north = "true") +
  theme(legend.position = "right") +
  labs(x = NULL,
       y = NULL,
       caption = Fonte, 
       title = "Porcentual de Imóveis Tratados 4º Ciclo/2022 - 22ªRS") + 
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold", 
                                   size = 14,
                                   colour = "#556B2F")
  )

#########################################################################################################################
#########################   Mapa IIP REGIONAL  5º Ciclo/2022     ########################################################

MAPA_BASE_RS <- left_join(MAPA_BASE, RS22_22_23_CICLOS_MUNICIPIOS, 
                          by = c("name_muni" = "Município")
)

AUX_MAP <- MAPA_BASE_RS %>% filter(`5ºCICLO_IIP` > 199)

AUX_MAP$Cat  <- "S/I"

AUX_MAP$`5ºCICLO_IIP` <- "S/I"

MAPA_BASE_RS <- MAPA_BASE_RS %>% filter(`5ºCICLO_IIP` >= 0 & `5ºCICLO_IIP` < 200)

MAPA_BASE_RS$Cat <- with(MAPA_BASE_RS, cut(x = `5ºCICLO_IIP`,
                                           breaks = c(-Inf, 1, 4, Inf),
                                           labels = c("0 - 0,9", "1 - 3,9", 
                                                      "> 4"))
)

MAPA_BASE_RS$`5ºCICLO_IIP` <- format(round(MAPA_BASE_RS$`5ºCICLO_IIP`, 2))

MAPA_BASE_RS <-rbind(MAPA_BASE_RS, AUX_MAP)

RS22_22_23_GRAF_IIP_Ciclo5 <- ggplot() + 
  geom_sf(data = MAPA_BASE_RS, 
          color = "black", 
          aes(fill = Cat)) + 
  coord_sf(expand = FALSE) +
  geom_sf_label(data = MAPA_BASE_RS, 
                aes(label = `5ºCICLO_IIP`),
                label.padding = unit(0.5, "mm"),
                size = 3,
                position = "identity") +
  scale_fill_manual (name = "",
                     values = c("0 - 0,9" = "green",
                                "1 - 3,9" = "yellow",
                                "> 4" = "red",
                                "S/I" = "white")
  ) +
  annotation_scale(location = "br") +
  annotation_north_arrow(location = "tr", 
                         which_north = "true") +
  theme(legend.position = "right") +
  labs(x = NULL,
       y = NULL,
       caption = Fonte, 
       title = "Índice de Infestação Predial 5º Ciclo/2022 - 22ªRS") + 
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold", 
                                   size = 14,
                                   colour = "#556B2F")
  ) 

###########    Tratamento 5º Ciclo  ###################################

MAPA_BASE_RS$Cat <- with(MAPA_BASE_RS, cut(x = `5ºCICLO_2022_%_Visitas`,
                                           breaks = c(-Inf, 30, 60, 80, Inf),
                                           labels = c("< 30%", "30,01 a 60%", 
                                                      "60,01 a 80%", "> 80%"))
)

MAPA_BASE_RS$`5ºCICLO_2022_%_Visitas` <- format(round(MAPA_BASE_RS$`5ºCICLO_2022_%_Visitas`, 2))

RS22_22_23_GRAF_Tratamento_Ciclo5 <- ggplot() + 
  geom_sf(data = MAPA_BASE_RS, 
          color = "black", 
          aes(fill = Cat)) + 
  coord_sf(expand = FALSE) +
  geom_sf_label(data = MAPA_BASE_RS, 
                aes(label = `5ºCICLO_2022_%_Visitas`),
                label.padding = unit(0.5, "mm"),
                size = 3,
                position = "identity") +
  scale_fill_manual (name = "",
                     values = c("< 30%" = "red",
                                "30,01 a 60%" = "#FFA07A",
                                "60,01 a 80%" = "yellow",
                                "> 80%" = "green")
  ) +
  annotation_scale(location = "br") +
  annotation_north_arrow(location = "tr", 
                         which_north = "true") +
  theme(legend.position = "right") +
  labs(x = NULL,
       y = NULL,
       caption = Fonte, 
       title = "Porcentual de Imóveis Tratados 5º Ciclo/2022 - 22ªRS") + 
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold", 
                                   size = 14,
                                   colour = "#556B2F")
  )

#########################################################################################################################
#########################   Mapa IIP REGIONAL  6º Ciclo/2022     ########################################################

MAPA_BASE_RS <- left_join(MAPA_BASE, RS22_22_23_CICLOS_MUNICIPIOS, 
                          by = c("name_muni" = "Município")
)

AUX_MAP <- MAPA_BASE_RS %>% filter(`6ºCICLO_IIP` > 199)

AUX_MAP$Cat  <- "S/I"

AUX_MAP$`6ºCICLO_IIP` <- "S/I"

MAPA_BASE_RS <- MAPA_BASE_RS %>% filter(`6ºCICLO_IIP` >= 0 & `6ºCICLO_IIP` < 200)

MAPA_BASE_RS$Cat <- with(MAPA_BASE_RS, cut(x = `6ºCICLO_IIP`,
                                           breaks = c(-Inf, 1, 4, Inf),
                                           labels = c("0 - 0,9", "1 - 3,9", 
                                                      "> 4"))
)

MAPA_BASE_RS$`6ºCICLO_IIP` <- format(round(MAPA_BASE_RS$`6ºCICLO_IIP`, 2))

MAPA_BASE_RS <-rbind(MAPA_BASE_RS, AUX_MAP)

RS22_22_23_GRAF_IIP_Ciclo6 <- ggplot() + 
  geom_sf(data = MAPA_BASE_RS, 
          color = "black", 
          aes(fill = Cat)) + 
  coord_sf(expand = FALSE) +
  geom_sf_label(data = MAPA_BASE_RS, 
                aes(label = `6ºCICLO_IIP`),
                label.padding = unit(0.5, "mm"),
                size = 3,
                position = "identity") +
  scale_fill_manual (name = "",
                     values = c("0 - 0,9" = "green",
                                "1 - 3,9" = "yellow",
                                "> 4" = "red",
                                "S/I" = "white")
  ) +
  annotation_scale(location = "br") +
  annotation_north_arrow(location = "tr", 
                         which_north = "true") +
  theme(legend.position = "right") +
  labs(x = NULL,
       y = NULL,
       caption = Fonte, 
       title = "Índice de Infestação Predial 6º Ciclo/2022 - 22ªRS") + 
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold", 
                                   size = 14,
                                   colour = "#556B2F")
  ) 

###########    Tratamento 6º Ciclo  ###################################

MAPA_BASE_RS$Cat <- with(MAPA_BASE_RS, cut(x = `6ºCICLO_2022_%_Visitas`,
                                           breaks = c(-Inf, 30, 60, 80, Inf),
                                           labels = c("< 30%", "30,01 a 60%", 
                                                      "60,01 a 80%", "> 80%"))
)

MAPA_BASE_RS$`6ºCICLO_2022_%_Visitas` <- format(round(MAPA_BASE_RS$`6ºCICLO_2022_%_Visitas`, 2))

RS22_22_23_GRAF_Tratamento_Ciclo6 <- ggplot() + 
  geom_sf(data = MAPA_BASE_RS, 
          color = "black", 
          aes(fill = Cat)) + 
  coord_sf(expand = FALSE) +
  geom_sf_label(data = MAPA_BASE_RS, 
                aes(label = `6ºCICLO_2022_%_Visitas`),
                label.padding = unit(0.5, "mm"),
                size = 3,
                position = "identity") +
  scale_fill_manual (name = "",
                     values = c("< 30%" = "red",
                                "30,01 a 60%" = "#FFA07A",
                                "60,01 a 80%" = "yellow",
                                "> 80%" = "green")
  ) +
  annotation_scale(location = "br") +
  annotation_north_arrow(location = "tr", 
                         which_north = "true") +
  theme(legend.position = "right") +
  labs(x = NULL,
       y = NULL,
       caption = Fonte, 
       title = "Porcentual de Imóveis Tratados 6º Ciclo/2022 - 22ªRS") + 
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold", 
                                   size = 14,
                                   colour = "#556B2F")
  )

#########################################################################################################################
#########################   Mapa IIP REGIONAL  1º Ciclo/2022     ########################################################

MAPA_BASE_RS <- left_join(MAPA_BASE, RS22_22_23_CICLOS_MUNICIPIOS, 
                          by = c("name_muni" = "Município")
)

AUX_MAP <- MAPA_BASE_RS %>% filter(`1ºCICLO_IIP` > 199)

AUX_MAP$Cat  <- "S/I"

AUX_MAP$`1ºCICLO_IIP` <- "S/I"

MAPA_BASE_RS <- MAPA_BASE_RS %>% filter(`1ºCICLO_IIP` >= 0 & `1ºCICLO_IIP` < 200)

MAPA_BASE_RS$Cat <- with(MAPA_BASE_RS, cut(x = `1ºCICLO_IIP`,
                                           breaks = c(-Inf, 1, 4, Inf),
                                           labels = c("0 - 0,9", "1 - 3,9", 
                                                      "> 4"))
)

MAPA_BASE_RS$`1ºCICLO_IIP` <- format(round(MAPA_BASE_RS$`1ºCICLO_IIP`, 2))

MAPA_BASE_RS <-rbind(MAPA_BASE_RS, AUX_MAP)

RS22_22_23_GRAF_IIP_Ciclo1 <- ggplot() + 
  geom_sf(data = MAPA_BASE_RS, 
          color = "black", 
          aes(fill = Cat)) + 
  coord_sf(expand = FALSE) +
  geom_sf_label(data = MAPA_BASE_RS, 
                aes(label = `1ºCICLO_IIP`),
                label.padding = unit(0.5, "mm"),
                size = 3,
                position = "identity") +
  scale_fill_manual (name = "",
                     values = c("0 - 0,9" = "green",
                                "1 - 3,9" = "yellow",
                                "> 4" = "red",
                                "S/I" = "white")
  ) +
  annotation_scale(location = "br") +
  annotation_north_arrow(location = "tr", 
                         which_north = "true") +
  theme(legend.position = "right") +
  labs(x = NULL,
       y = NULL,
       caption = Fonte, 
       title = "Índice de Infestação Predial 1º Ciclo/2023 - 22ªRS") + 
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold", 
                                   size = 14,
                                   colour = "#556B2F")
  ) 

###########    Tratamento 1º Ciclo  ###################################

MAPA_BASE_RS$Cat <- with(MAPA_BASE_RS, cut(x = `1ºCICLO_2023_%_Visitas`,
                                           breaks = c(-Inf, 30, 60, 80, Inf),
                                           labels = c("< 30%", "30,01 a 60%", 
                                                      "60,01 a 80%", "> 80%"))
)

MAPA_BASE_RS$`1ºCICLO_2023_%_Visitas` <- format(round(MAPA_BASE_RS$`1ºCICLO_2023_%_Visitas`, 2))

RS22_22_23_GRAF_Tratamento_Ciclo1 <- ggplot() + 
  geom_sf(data = MAPA_BASE_RS, 
          color = "black", 
          aes(fill = Cat)) + 
  coord_sf(expand = FALSE) +
  geom_sf_label(data = MAPA_BASE_RS, 
                aes(label = `1ºCICLO_2023_%_Visitas`),
                label.padding = unit(0.5, "mm"),
                size = 3,
                position = "identity") +
  scale_fill_manual (name = "",
                     values = c("< 30%" = "red",
                                "30,01 a 60%" = "#FFA07A",
                                "60,01 a 80%" = "yellow",
                                "> 80%" = "green")
  ) +
  annotation_scale(location = "br") +
  annotation_north_arrow(location = "tr", 
                         which_north = "true") +
  theme(legend.position = "right") +
  labs(x = NULL,
       y = NULL,
       caption = Fonte, 
       title = "Porcentual de Imóveis Tratados 1º Ciclo/2023 - 22ªRS") + 
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold", 
                                   size = 14,
                                   colour = "#556B2F")
  )

#########################################################################################################################
#########################   Mapa IIP REGIONAL  2º Ciclo/2022     ########################################################

MAPA_BASE_RS <- left_join(MAPA_BASE, RS22_22_23_CICLOS_MUNICIPIOS, 
                          by = c("name_muni" = "Município")
)

AUX_MAP <- MAPA_BASE_RS %>% filter(`2ºCICLO_IIP` > 199)

AUX_MAP$Cat  <- "S/I"

AUX_MAP$`2ºCICLO_IIP` <- "S/I"

MAPA_BASE_RS <- MAPA_BASE_RS %>% filter(`2ºCICLO_IIP` >= 0 & `2ºCICLO_IIP` < 200)

MAPA_BASE_RS$Cat <- with(MAPA_BASE_RS, cut(x = `2ºCICLO_IIP`,
                                           breaks = c(-Inf, 1, 4, Inf),
                                           labels = c("0 - 0,9", "1 - 3,9", 
                                                      "> 4"))
)

MAPA_BASE_RS$`2ºCICLO_IIP` <- format(round(MAPA_BASE_RS$`2ºCICLO_IIP`, 2))

MAPA_BASE_RS <-rbind(MAPA_BASE_RS, AUX_MAP)

RS22_22_23_GRAF_IIP_Ciclo2 <- ggplot() + 
  geom_sf(data = MAPA_BASE_RS, 
          color = "black", 
          aes(fill = Cat)) + 
  coord_sf(expand = FALSE) +
  geom_sf_label(data = MAPA_BASE_RS, 
                aes(label = `2ºCICLO_IIP`),
                label.padding = unit(0.5, "mm"),
                size = 3,
                position = "identity") +
  scale_fill_manual (name = "",
                     values = c("0 - 0,9" = "green",
                                "1 - 3,9" = "yellow",
                                "> 4" = "red",
                                "S/I" = "white")
  ) +
  annotation_scale(location = "br") +
  annotation_north_arrow(location = "tr", 
                         which_north = "true") +
  theme(legend.position = "right") +
  labs(x = NULL,
       y = NULL,
       caption = Fonte, 
       title = "Índice de Infestação Predial 2º Ciclo/2023 - 22ªRS") + 
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold", 
                                   size = 14,
                                   colour = "#556B2F")
  ) 

###########    Tratamento 2º Ciclo  ###################################

MAPA_BASE_RS$Cat <- with(MAPA_BASE_RS, cut(x = `2ºCICLO_2023_%_Visitas`,
                                           breaks = c(-Inf, 30, 60, 80, Inf),
                                           labels = c("< 30%", "30,01 a 60%", 
                                                      "60,01 a 80%", "> 80%"))
)

MAPA_BASE_RS$`2ºCICLO_2023_%_Visitas` <- format(round(MAPA_BASE_RS$`2ºCICLO_2023_%_Visitas`, 2))

RS22_22_23_GRAF_Tratamento_Ciclo2 <- ggplot() + 
  geom_sf(data = MAPA_BASE_RS, 
          color = "black", 
          aes(fill = Cat)) + 
  coord_sf(expand = FALSE) +
  geom_sf_label(data = MAPA_BASE_RS, 
                aes(label = `2ºCICLO_2023_%_Visitas`),
                label.padding = unit(0.5, "mm"),
                size = 3,
                position = "identity") +
  scale_fill_manual (name = "",
                     values = c("< 30%" = "red",
                                "30,01 a 60%" = "#FFA07A",
                                "60,01 a 80%" = "yellow",
                                "> 80%" = "green")
  ) +
  annotation_scale(location = "br") +
  annotation_north_arrow(location = "tr", 
                         which_north = "true") +
  theme(legend.position = "right") +
  labs(x = NULL,
       y = NULL,
       caption = Fonte, 
       title = "Porcentual de Imóveis Tratados 2º Ciclo/2023 - 22ªRS") + 
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold", 
                                   size = 14,
                                   colour = "#556B2F")
  )

#########################################################################################################################
#########################   Mapa IIP REGIONAL  3º Ciclo/2022     ########################################################

MAPA_BASE_RS <- left_join(MAPA_BASE, RS22_22_23_CICLOS_MUNICIPIOS, 
                          by = c("name_muni" = "Município")
)

AUX_MAP <- MAPA_BASE_RS %>% filter(`3ºCICLO_IIP` > 199)

AUX_MAP$Cat  <- "S/I"

AUX_MAP$`3ºCICLO_IIP` <- "S/I"

MAPA_BASE_RS <- MAPA_BASE_RS %>% filter(`3ºCICLO_IIP` >= 0 & `3ºCICLO_IIP` < 200)

MAPA_BASE_RS$Cat <- with(MAPA_BASE_RS, cut(x = `3ºCICLO_IIP`,
                                           breaks = c(-Inf, 1, 4, Inf),
                                           labels = c("0 - 0,9", "1 - 3,9", 
                                                      "> 4"))
)

MAPA_BASE_RS$`3ºCICLO_IIP` <- format(round(MAPA_BASE_RS$`3ºCICLO_IIP`, 2))

MAPA_BASE_RS <-rbind(MAPA_BASE_RS, AUX_MAP)

RS22_22_23_GRAF_IIP_Ciclo3 <- ggplot() + 
  geom_sf(data = MAPA_BASE_RS, 
          color = "black", 
          aes(fill = Cat)) + 
  coord_sf(expand = FALSE) +
  geom_sf_label(data = MAPA_BASE_RS, 
                aes(label = `3ºCICLO_IIP`),
                label.padding = unit(0.5, "mm"),
                size = 3,
                position = "identity") +
  scale_fill_manual (name = "",
                     values = c("0 - 0,9" = "green",
                                "1 - 3,9" = "yellow",
                                "> 4" = "red",
                                "S/I" = "white")
  ) +
  annotation_scale(location = "br") +
  annotation_north_arrow(location = "tr", 
                         which_north = "true") +
  theme(legend.position = "right") +
  labs(x = NULL,
       y = NULL,
       caption = Fonte, 
       title = "Índice de Infestação Predial 3º Ciclo/2023 - 22ªRS") + 
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold", 
                                   size = 14,
                                   colour = "#556B2F")
  ) 

###########    Tratamento 3º Ciclo  ###################################

MAPA_BASE_RS$Cat <- with(MAPA_BASE_RS, cut(x = `3ºCICLO_2023_%_Visitas`,
                                           breaks = c(-Inf, 30, 60, 80, Inf),
                                           labels = c("< 30%", "30,01 a 60%", 
                                                      "60,01 a 80%", "> 80%"))
)

MAPA_BASE_RS$`3ºCICLO_2023_%_Visitas` <- format(round(MAPA_BASE_RS$`3ºCICLO_2023_%_Visitas`, 2))

RS22_22_23_GRAF_Tratamento_Ciclo3 <- ggplot() + 
  geom_sf(data = MAPA_BASE_RS, 
          color = "black", 
          aes(fill = Cat)) + 
  coord_sf(expand = FALSE) +
  geom_sf_label(data = MAPA_BASE_RS, 
                aes(label = `3ºCICLO_2023_%_Visitas`),
                label.padding = unit(0.5, "mm"),
                size = 3,
                position = "identity") +
  scale_fill_manual (name = "",
                     values = c("< 30%" = "red",
                                "30,01 a 60%" = "#FFA07A",
                                "60,01 a 80%" = "yellow",
                                "> 80%" = "green")
  ) +
  annotation_scale(location = "br") +
  annotation_north_arrow(location = "tr", 
                         which_north = "true") +
  theme(legend.position = "right") +
  labs(x = NULL,
       y = NULL,
       caption = Fonte, 
       title = "Porcentual de Imóveis Tratados 3º Ciclo/2023 - 22ªRS") + 
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold", 
                                   size = 14,
                                   colour = "#556B2F")
  )


#####     Salvando os Gráficos e Mapas

###      Série Histórica - Pag_03

RS22_22_23_INFORME_Pag_03 <- (RS22_Serie_Historica_GRAF_Not_Conf / (RS22_Serie_Historica_GRAF_Sorotipo + RS22_Serie_Historica_GRAF_Hospitalizados))

png(filename = "/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/RS22_22_23_INFORME_Pag_03.png", 
    width = 33,
    height = 40,
    units = "cm", pointsize = 8, res = 300)

RS22_22_23_INFORME_Pag_03 

dev.off()

###          Canal Endêmicos Notificados/Confirmados - Pag 04

RS22_22_23_INFORME_Pag_04 <- (RS_22_23_GRAF_CE_Notificados / RS_22_23_GRAF_CE_Confirmados)

png(filename = "/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/RS22_22_23_INFORME_Pag_04.png", 
    width = 33,
    height = 40,
    units = "cm", pointsize = 8, res = 300)

RS22_22_23_INFORME_Pag_04 

dev.off()

####           Canal Endêmico Prováveis - Pag 05

RS22_22_23_INFORME_Pag_05 <- RS_22_23_GRAF_CE_Provaveis

png(filename = "/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/RS22_22_23_INFORME_Pag_05.png", 
    width = 33,
    height = 20,
    units = "cm", pointsize = 8, res = 300)

RS22_22_23_INFORME_Pag_05 

dev.off()


###            Notificados/Confirmados - Pag 06

RS22_22_23_INFORME_Pag_06 <- ((RS22_22_23_GRAF_Notificados + RS22_22_23_GRAF_Confirmados)/ (RS22_22_23_GRAF_Autoctones + RS22_22_23_GRAF_Investigacao))

png(filename = "/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/RS22_22_23_INFORME_Pag_06.png", 
    width = 36,
    height = 46,
    units = "cm", pointsize = 8, res = 300)

RS22_22_23_INFORME_Pag_06

dev.off()

###      Autóctones/Descartados

RS22_22_23_INFORME_Pag_07 <- ((RS22_22_23_GRAF_Descartados + RS22_22_23_GRAF_Hospitalizados)/ RS22_22_23_GRAF_Incidencia)

png(filename = "/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/RS22_22_23_INFORME_Pag_07.png", 
    width = 36,
    height = 46,
    units = "cm", pointsize = 8, res = 300)

RS22_22_23_INFORME_Pag_07

dev.off()


###    Em Investigação/Incidência

RS22_22_23_INFORME_Pag_08 <- (RS22_Serie_Historica_GRAF_Encerramento / RS22_22_23_GRAF_SINAIS)

png(filename = "/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/RS22_22_23_INFORME_Pag_08.png", 
    width = 33,
    height = 40,
    units = "cm", pointsize = 8, res = 300)

RS22_22_23_INFORME_Pag_08

dev.off()

#### LACEN Municípios

png(filename = "/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/RS22_22_23_INFORME_Pag_09A.png", 
    width = 33,
    height = 20,
    units = "cm", pointsize = 8, res = 300)

RS22_22_23_GRAF_Inconclusivos

dev.off()

####  Canais Endêmicos IVAIPORÃ

RS_22_23_CE_SEDE_I <- RS_22_23_GRAF_CE_Notificados_SEDE

RS_22_23_CE_SEDE_II <- (RS_22_23_GRAF_CE_Confirmados_SEDE / RS_22_23_GRAF_CE_Provaveis_SEDE)

png(filename = "/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/RS22_22_23_INFORME_Pag_09B.png", 
    width = 33,
    height = 20,
    units = "cm", pointsize = 8, res = 300)

RS_22_23_CE_SEDE_I

dev.off()

png(filename = "/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/RS22_22_23_INFORME_Pag_10.png", 
    width = 33,
    height = 40,
    units = "cm", pointsize = 8, res = 300)

RS_22_23_CE_SEDE_II

dev.off()


###      Histogramas Notificados - Pag 11

png(filename = "/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/RS22_22_23_INFORME_Pag_11.png", 
    width = 33,
    height = 40,
    units = "cm", pointsize = 8, res = 300)

RS22_22_23_GRAF_Histograma_Notificados_01

dev.off()

###        Histogramas Notificados - Pag 12

png(filename = "/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/RS22_22_23_INFORME_Pag_12.png", 
    width = 33,
    height = 40,
    units = "cm", pointsize = 8, res = 300)

RS22_22_23_GRAF_Histograma_Notificados_02

dev.off()

###      Histogramas Confirmados - ##PAG 13

png(filename = "/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/RS22_22_23_INFORME_Pag_13.png", 
    width = 33,
    height = 40,
    units = "cm", pointsize = 8, res = 300)

RS22_22_23_GRAF_Histograma_Confirmados_01

dev.off()

###      Histogramas Confirmados - ##PAG 14

png(filename = "/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/RS22_22_23_INFORME_Pag_14.png", 
    width = 33,
    height = 40,
    units = "cm", pointsize = 8, res = 300)

RS22_22_23_GRAF_Histograma_Confirmados_02

dev.off()

###     Histogramas Prováveis - ##PAG 15

png(filename = "/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/RS22_22_23_INFORME_Pag_15.png", 
    width = 33,
    height = 40,
    units = "cm", pointsize = 8, res = 300)

RS22_22_23_GRAF_Histograma_Provaveis_01

dev.off()

###     Histogramas Prováveis - ##PAG 16

png(filename = "/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/RS22_22_23_INFORME_Pag_16.png", 
    width = 33,
    height = 40,
    units = "cm", pointsize = 8, res = 300)

RS22_22_23_GRAF_Histograma_Provaveis_02

dev.off()

RS22_22_23_GRAF_1 <- (PR_22_23_GRAF_SOROTIPO_PR / RS22_22_23_GRAF_Sorotipo)
png(filename = "/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/RS22_22_23_INFORME_Pag_17.png", 
    width = 33,
    height = 40,
    units = "cm", pointsize = 8, res = 300)

RS22_22_23_GRAF_1

dev.off()

RS22_22_23_GRAF_1 <- (RS22_22_23_GRAF_US_TOTAL / RS22_22_23_GRAF_US_DETEC)
png(filename = "/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/RS22_22_23_INFORME_Pag_18A.png", 
    width = 33,
    height = 20,
    units = "cm", pointsize = 8, res = 300)

RS22_22_23_GRAF_1

dev.off()

RS22_22_23_GRAF_1 <- (RS22_22_23_GRAF_SORO_TOTAL / RS22_22_23_GRAF_SORO_REAG)
png(filename = "/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/RS22_22_23_INFORME_Pag_18B.png", 
    width = 33,
    height = 20,
    units = "cm", pointsize = 8, res = 300)

RS22_22_23_GRAF_1

dev.off()

##### Exames Municípios  

png(filename = "/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/RS22_22_23_INFORME_Pag_19.png", 
    width = 33,
    height = 20,
    units = "cm", pointsize = 8, res = 300)

RS22_GRAF_LACEN_MUNIC

dev.off()

RS22_22_23_GRAF_1 <- (PR_22_23_GRAF_INCIDENCIA_PR / PR_22_23_GRAF_INCIDENCIA_PROV_PR)
png(filename = "/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/RS22_22_23_INFORME_Pag_20.png", 
    width = 33,
    height = 40,
    units = "cm", pointsize = 8, res = 300)

RS22_22_23_GRAF_1

dev.off()

RS22_22_23_GRAF_1 <- (PR_22_23_GRAF_CHIK_Notificados / PR_22_23_GRAF_CHIK_Incidência)
png(filename = "/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/RS22_22_23_INFORME_Pag_21.png", 
    width = 33,
    height = 40,
    units = "cm", pointsize = 8, res = 300)

RS22_22_23_GRAF_1

dev.off()

RS22_22_23_GRAF_1 <- PR_22_23_GRAF_SINAIS_CHIK 
png(filename = "/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/RS22_22_23_INFORME_Pag_22A.png", 
    width = 33,
    height = 20,
    units = "cm", pointsize = 8, res = 300)

RS22_22_23_GRAF_1

dev.off()

RS22_22_23_GRAF_1 <- (RS22_22_23_GRAF_CHK_Not + RS22_22_23_GRAF_CHK_Conf) 
png(filename = "/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/RS22_22_23_INFORME_Pag_22B.png", 
    width = 33,
    height = 20,
    units = "cm", pointsize = 8, res = 300)

RS22_22_23_GRAF_1

dev.off()

RS22_22_23_GRAF_1 <- (PR_22_23_ZIKA_CHIK_Notificados / PR_22_23_GRAF_ZIKA_Incidência)
png(filename = "/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/RS22_22_23_INFORME_Pag_23.png", 
    width = 33,
    height = 40,
    units = "cm", pointsize = 8, res = 300)

RS22_22_23_GRAF_1

dev.off()

RS22_22_23_GRAF_1 <- (RS22_22_23_GRAF_IIP_Ciclo4 / RS22_22_23_GRAF_Tratamento_Ciclo4)
png(filename = "/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/RS22_22_23_INFORME_Pag_24.png", 
    width = 33,
    height = 40,
    units = "cm", pointsize = 8, res = 300)

RS22_22_23_GRAF_1

dev.off()

RS22_22_23_GRAF_1 <- (RS22_22_23_GRAF_IIP_Ciclo5 / RS22_22_23_GRAF_Tratamento_Ciclo5)
png(filename = "/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/RS22_22_23_INFORME_Pag_25.png", 
    width = 33,
    height = 40,
    units = "cm", pointsize = 8, res = 300)

RS22_22_23_GRAF_1

dev.off()

RS22_22_23_GRAF_1 <- (RS22_22_23_GRAF_IIP_Ciclo6 / RS22_22_23_GRAF_Tratamento_Ciclo6)
png(filename = "/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/RS22_22_23_INFORME_Pag_26.png", 
    width = 33,
    height = 40,
    units = "cm", pointsize = 8, res = 300)

RS22_22_23_GRAF_1

dev.off()

RS22_22_23_GRAF_1 <- (RS22_22_23_GRAF_IIP_Ciclo1 / RS22_22_23_GRAF_Tratamento_Ciclo1)
png(filename = "/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/RS22_22_23_INFORME_Pag_27.png", 
    width = 33,
    height = 40,
    units = "cm", pointsize = 8, res = 300)

RS22_22_23_GRAF_1

dev.off()

RS22_22_23_GRAF_1 <- (RS22_22_23_GRAF_IIP_Ciclo2 / RS22_22_23_GRAF_Tratamento_Ciclo2)
png(filename = "/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/RS22_22_23_INFORME_Pag_28.png", 
    width = 33,
    height = 40,
    units = "cm", pointsize = 8, res = 300)

RS22_22_23_GRAF_1

dev.off()

RS22_22_23_GRAF_1 <- (RS22_22_23_GRAF_IIP_Ciclo3 / RS22_22_23_GRAF_Tratamento_Ciclo3)
png(filename = "/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/RS22_22_23_INFORME_Pag_29.png", 
    width = 33,
    height = 40,
    units = "cm", pointsize = 8, res = 300)

RS22_22_23_GRAF_1

dev.off()

rm(RS22_22_23_GRAF_Autoctones, 
   SE_HIST_PROV_GODOY, 
   SE_HIST_NOT_ARAPUÃ, 
   SE_HIST_NOT_GODOY, 
   SE_HIST_CONF_MANOEL_RIBAS, 
   RS22_22_23_INFORME_Pag_05, 
   RS22_22_23_INFORME_Pag_06, 
   RS22_22_23_INFORME_Pag_08, 
   RS22_22_23_INFORME_Pag_07, 
   RS22_22_23_Resumida, 
   RS22_22_23_INFORME_Pag_03, 
   RS22_22_23_INFORME_Pag_04, 
   SE_HIST_CONF_MATO_RICO, 
   SE_HIST_CONF_RSI, 
   SE_HIST_CONF_RBI, 
   SE_HIST_CONF_NOVA_TEBAS, 
   SE_HIST_NOT_IVAIPORA, 
   SE_HIST_CONF_SMO, 
   SE_HIST_CONF_SJI, 
   SE_HIST_NOT_ARIRANHA, 
   SE_HIST_NOT_CANDIDO, 
   SE_HIST_NOT_CRUZMALTINA, 
   SE_HIST_NOT_RBI, 
   SE_HIST_NOT_NOVA_TEBAS, 
   SE_HIST_NOT_MATO_RICO,  
   SE_HIST_NOT_MANOEL_RIBAS, 
   SE_HIST_NOT_LUNARDELLI, 
   SE_HIST_NOT_LIDIANÓPOLIS, 
   SE_HIST_NOT_JARDIM, 
   SE_HIST_PROV_CANDIDO, 
   SE_HIST_NOT_SMO, 
   SE_HIST_NOT_SJI, 
   SE_HIST_NOT_RSI,  
   SE_HIST_PROV_CRUZMALTINA, 
   SE_HIST_PROV_ARIRANHA, 
   SE_HIST_PROV_ARAPUÃ,  
   SE_HIST_PROV_LUNARDELLI, 
   SE_HIST_PROV_LIDIANÓPOLIS, 
   SE_HIST_PROV_JARDIM, 
   SE_HIST_PROV_IVAIPORA,  
   RS22_22_23_GRAF_Incidencia, 
   RS22_Serie_Historica_GRAF_Hospitalizados, 
   RS22_Serie_Historica_GRAF_Not_Conf, 
   RS22_Serie_Historica_GRAF_Sorotipo, 
   SE_HIST_PROV_MATO_RICO, 
   SE_HIST_PROV_MANOEL_RIBAS, 
   SE_HIST_PROV_SMO, 
   RS22_22_23_GRAF_Histograma_Provaveis_01, 
   RS22_22_23_GRAF_Histograma_Provaveis_02, 
   RS22_22_23_GRAF_Hospitalizados, 
   SE_HIST_PROV_SJI, 
   SE_HIST_PROV_RSI, 
   SE_HIST_PROV_RBI, 
   SE_HIST_PROV_NOVA_TEBAS, 
   RS22_22_23_GRAF_SINAIS, 
   RS22_22_23_GRAF_Notificados, 
   RS22_22_23_GRAF_Investigacao, 
   RS22_22_23_GRAF_Descartados, 
   RS22_22_23_GRAF_Histograma_Notificados_01, 
   RS22_22_23_GRAF_Histograma_Notificados_02, 
   SE_HIST_CONF_ARAPUÃ, 
   SE_HIST_CONF_IVAIPORA, 
   SE_HIST_CONF_JARDIM, 
   SE_HIST_CONF_LIDIANÓPOLIS, 
   SE_HIST_CONF_LUNARDELLI, 
   SE_HIST_CONF_ARIRANHA, 
   SE_HIST_CONF_CANDIDO, 
   SE_HIST_CONF_CRUZMALTINA, 
   SE_HIST_CONF_GODOY, 
   RS22_22_23_GRAF_CE_Confirmados, 
   RS22_22_23_GRAF_CE_Notificados, 
   RS22_22_23_GRAF_CE_Provaveis, 
   RS22_22_23_GRAF_Confirmados, 
   RS22_22_23_GRAF_Histograma_Confirmados_01, 
   RS22_22_23_GRAF_Histograma_Confirmados_02,
   RS22_22_23_GRAF_CE_Confirmados_IVAIPORÃ, 
   RS22_22_23_GRAF_CE_Notificados_IVAIPORÃ, 
   RS22_22_23_GRAF_CE_Provaveis_IVAIPORÃ, 
   RS22_22_23_CE_IVAIPORÂ_I,
   RS22_22_23_CE_IVAIPORÃ_II,
   RS22_Serie_Historica_GRAF_Encerramento,
   AUX,
   AUX_2,
   BASE_IBGE,
   BASE_IBGE_BRASIL,
   RS22_22_23_GRAF_1,
   RS22_22_23_GRAF_SORO_REAG,
   RS22_22_23_GRAF_SORO_TOTAL,
   RS22_22_23_GRAF_US_DETEC,
   RS22_22_23_GRAF_US_TOTAL,
   Fonte,
   Fonte_1,
   Fonte_2,
   i,
   SE,
   AUX_GRAF,
   AUX_I,
   AUX_II,
   AUX_III,
   AUX_IV,
   AUX_MAP,
   AUX_MAP_1,
   AUX_MAP_2,
   AUX_MAP_3,
   AUX_MAP_4,
   AUX_V,
   AUX_VI,
   CHIKON2022,
   CHIKON2023,
   MAPA_BASE,
   MAPA_BASE_PR,
   MAPA_BASE_RS,
   RS22_Dissolvida,
   SINAN_DENGUE_RS,
   SINAN_CHIK_RS,
   RS22_GRAF_LACEN_MUNIC,
   AUX01,
   AUX02,
   PR_22_23_GRAF_CHIK_Incidência,
   PR_22_23_GRAF_CHIK_Notificados,
   PR_22_23_GRAF_INCIDENCIA_PR,
   PR_22_23_GRAF_INCIDENCIA_PROV_PR,
   PR_22_23_GRAF_SINAIS_CHIK,
   PR_22_23_GRAF_SOROTIPO_PR,
   PR_22_23_GRAF_ZIKA_Incidência,
   RS_22_23_GRAF_CE_Confirmados,
   RS_22_23_GRAF_CE_Confirmados_SEDE,
   RS_22_23_GRAF_CE_Notificados,
   RS_22_23_GRAF_CE_Notificados_SEDE,
   RS_22_23_GRAF_CE_Provaveis,
   RS_22_23_GRAF_CE_Provaveis_SEDE,
   RS22_22_23_GRAF_1,
   RS22_22_23_GRAF_SORO_REAG,
   RS22_22_23_GRAF_SORO_TOTAL,
   RS22_22_23_GRAF_US_DETEC,
   RS22_22_23_GRAF_US_TOTAL,
   ID_REG,
   nrow,
   Periodos_Epidêmicos_RS,
   Periodos_Epidêmicos_SEDE,
   RS)

warnings()

