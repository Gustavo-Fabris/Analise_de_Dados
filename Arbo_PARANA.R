####O script necessita da pasta Análise de Dados para funcionar. Esta pasta não está organizada
#### como deveria, mas como já há scripts utilizando sua estrutura e o qgis idem, eu ia alterar
#### a organização no próximo período sazonal.
################################################################################################

setwd("/home/gustavo/Área de Trabalho/Análise_de_Dados/Base_de_Dados/")

library(xlsx)
library(dplyr)
library(foreign)

#########################################################################################################################
####################### Criando Objeto Fonte para servir de auxiliar para os gráficos  ##################################
#################### O ggplot irá buscar esse objeto para atualizar a fonte dos gráficos  ###############################

Fonte <- "Fonte: SINAN. BASE DBF acessada em 21/07/2023"

################################# Assinalar a data em que a Base DBF foi baixada  #######################################
#########################################################################################################################
#########################################################################################################################

### Objetos abaixo são planilhas contendo tabela de municípios e seus respectivos cógidos do IBGE. É necessário como motor 
### para que o script vincule dados nos "for loops". Por enquanto as duas são necessárias para rodar.
##########################################################################################################################

BASE_IBGE<-read.table(file="/home/gustavo/Área de Trabalho/Análise_de_Dados/Base_de_Dados/Planilha_Base_IBGE.csv", 
                      header=TRUE, 
                      sep=",")

BASE_IBGE_BRASIL <- read.csv (file = "/home/gustavo/Área de Trabalho/Análise_de_Dados/Base_de_Dados/Planilha_Base_IBGE_BRASIL.csv",
                              header = TRUE,
                              sep = ",")

#####CHIKUNGUNYA######
######################

###Base DBF SINAN. Necessário realizar o download da base DBF de notificações 2022 e 2023. Renomear 
### os arquivos como CHIKON2022 e CHIKON2023 e salvá-los em pasta específica.
###########################################################################################################################

CHIKON2022 <- read.dbf("/home/gustavo/Área de Trabalho/Análise_de_Dados/Base_de_Dados/DBF/CHIKON2022.dbf",
                       as.is = FALSE) %>% select(ID_REGIONA, NU_NOTIFIC, ID_GEO1,ID_GEO2, ID_AGRAVO, ID_REGIONA, DT_NOTIFIC, NU_ANO, SEM_NOT, DT_SIN_PRI, SEM_PRI,  SG_UF_NOT, ID_MUNICIP, NM_PACIENT, DT_NASC, NU_IDADE_N, CS_SEXO, CS_GESTANT, CS_ESCOL_N, NM_MAE_PAC, ID_MN_RESI, SG_UF, ID_RG_RESI, NM_LOGRADO, NU_NUMERO, NM_BAIRRO, NU_CEP, CS_ZONA, DT_DIGITA, DT_INVEST, FEBRE, MIALGIA, CEFALEIA, EXANTEMA, VOMITO, NAUSEA, DOR_COSTAS, CONJUNTVIT, ARTRITE, ARTRALGIA, PETEQUIA_N, LEUCOPENIA, LACO, DOR_RETRO, DIABETES, HEMATOLOG, HEPATOPAT, RENAL, HIPERTENSA, ACIDO_PEPT, AUTO_IMUNE, DT_SORO, RESUL_SORO, DT_PCR, RESUL_PCR_, SOROTIPO, CLASSI_FIN, CRITERIO, TPAUTOCTO, COUFINF, COMUNINF, CO_BAINF, EVOLUCAO, HOSPITALIZ, DT_INTERNA, DT_OBITO, DT_ENCERRA, DT_ALRM, ALRM_LETAR, ALRM_HEPAT, ALRM_LIQ, ALRM_HIPOT, ALRM_PLAQ, ALRM_VOM, ALRM_SANG, ALRM_HEMAT, ALRM_ABDOM, DT_GRAV, GRAV_PULSO, GRAV_CONV, GRAV_ENCH, GRAV_INSUF, GRAV_TAQUI, GRAV_EXTRE, GRAV_HIPOT, GRAV_HEMAT, GRAV_MELEN, GRAV_METRO, GRAV_SANG, GRAV_AST, GRAV_MIOC, GRAV_CONSC, GRAV_ORGAO, MANI_HEMOR, EPISTAXE, GENGIVO, METRO, DS_OBS)

CHIKON2023 <- read.dbf("/home/gustavo/Área de Trabalho/Análise_de_Dados/Base_de_Dados/DBF/CHIKON2023.dbf",
                       as.is = FALSE) %>% select(ID_REGIONA, NU_NOTIFIC, ID_GEO1,ID_GEO2, ID_AGRAVO, ID_REGIONA, DT_NOTIFIC, NU_ANO, SEM_NOT, DT_SIN_PRI, SEM_PRI,  SG_UF_NOT, ID_MUNICIP, NM_PACIENT, DT_NASC, NU_IDADE_N, CS_SEXO, CS_GESTANT, CS_ESCOL_N, NM_MAE_PAC, ID_MN_RESI, SG_UF, ID_RG_RESI, NM_LOGRADO, NU_NUMERO, NM_BAIRRO, NU_CEP, CS_ZONA, DT_DIGITA, DT_INVEST, FEBRE, MIALGIA, CEFALEIA, EXANTEMA, VOMITO, NAUSEA, DOR_COSTAS, CONJUNTVIT, ARTRITE, ARTRALGIA, PETEQUIA_N, LEUCOPENIA, LACO, DOR_RETRO, DIABETES, HEMATOLOG, HEPATOPAT, RENAL, HIPERTENSA, ACIDO_PEPT, AUTO_IMUNE, DT_SORO, RESUL_SORO, DT_PCR, RESUL_PCR_, SOROTIPO, CLASSI_FIN, CRITERIO, TPAUTOCTO, COUFINF, COMUNINF, CO_BAINF, EVOLUCAO, HOSPITALIZ, DT_INTERNA, DT_OBITO, DT_ENCERRA, DT_ALRM, ALRM_LETAR, ALRM_HEPAT, ALRM_LIQ, ALRM_HIPOT, ALRM_PLAQ, ALRM_VOM, ALRM_SANG, ALRM_HEMAT, ALRM_ABDOM, DT_GRAV, GRAV_PULSO, GRAV_CONV, GRAV_ENCH, GRAV_INSUF, GRAV_TAQUI, GRAV_EXTRE, GRAV_HIPOT, GRAV_HEMAT, GRAV_MELEN, GRAV_METRO, GRAV_SANG, GRAV_AST, GRAV_MIOC, GRAV_CONSC, GRAV_ORGAO, MANI_HEMOR, EPISTAXE, GENGIVO, METRO, DS_OBS)

#####################################################################################################################
###########
###Transformando coluna de semana epidemiológica de fator para numérica (passando por texto, se for direto, ela transforma ###200905 em 06. 
###Seria possível realizar busca de SE passando direto de fator para numérica utilizando as.integer
###(DENGON2009 ###%>% filter(ID_MUNICIP == 410165, SEM_PRI == 6) -1, para buscar SE 05?
##Será usado para buscar semanas epidemiológicas
##

CHIKON2022$SEM_PRI <-as.numeric(as.character(CHIKON2022$SEM_PRI))

CHIKON2023$SEM_PRI <-as.numeric(as.character(CHIKON2023$SEM_PRI))

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

####DENGUE#####
###############

DENGON2022 <- read.dbf(file = "/home/gustavo/Área de Trabalho/Análise_de_Dados/Base_de_Dados/DBF/DENGON2022.dbf", 
                       as.is = FALSE) %>% select(ID_REGIONA, NU_NOTIFIC, ID_GEO1,ID_GEO2, ID_AGRAVO, ID_REGIONA, DT_NOTIFIC, NU_ANO, SEM_NOT, DT_SIN_PRI, SEM_PRI,  SG_UF_NOT, ID_MUNICIP, NM_PACIENT, DT_NASC, NU_IDADE_N, CS_SEXO, CS_GESTANT, CS_ESCOL_N, NM_MAE_PAC, ID_MN_RESI, SG_UF, ID_RG_RESI, NM_LOGRADO, NU_NUMERO, NM_BAIRRO, NU_CEP, CS_ZONA, DT_DIGITA, DT_INVEST, FEBRE, MIALGIA, CEFALEIA, EXANTEMA, VOMITO, NAUSEA, DOR_COSTAS, CONJUNTVIT, ARTRITE, ARTRALGIA, PETEQUIA_N, LEUCOPENIA, LACO, DOR_RETRO, DIABETES, HEMATOLOG, HEPATOPAT, RENAL, HIPERTENSA, ACIDO_PEPT, AUTO_IMUNE, DT_SORO, RESUL_SORO, DT_PCR, RESUL_PCR_, SOROTIPO, CLASSI_FIN, CRITERIO, TPAUTOCTO, COUFINF, COMUNINF, CO_BAINF, EVOLUCAO, HOSPITALIZ, DT_INTERNA, DT_OBITO, DT_ENCERRA, DT_ALRM, ALRM_LETAR, ALRM_HEPAT, ALRM_LIQ, ALRM_HIPOT, ALRM_PLAQ, ALRM_VOM, ALRM_SANG, ALRM_HEMAT, ALRM_ABDOM, DT_GRAV, GRAV_PULSO, GRAV_CONV, GRAV_ENCH, GRAV_INSUF, GRAV_TAQUI, GRAV_EXTRE, GRAV_HIPOT, GRAV_HEMAT, GRAV_MELEN, GRAV_METRO, GRAV_SANG, GRAV_AST, GRAV_MIOC, GRAV_CONSC, GRAV_ORGAO, MANI_HEMOR, EPISTAXE, GENGIVO, METRO, DS_OBS)

DENGON2023 <- read.dbf(file = "/home/gustavo/Área de Trabalho/Análise_de_Dados/Base_de_Dados/DBF/DENGON2023.dbf", 
                       as.is = FALSE) %>% select(ID_REGIONA, NU_NOTIFIC, ID_GEO1,ID_GEO2, ID_AGRAVO, ID_REGIONA, DT_NOTIFIC, NU_ANO, SEM_NOT, DT_SIN_PRI, SEM_PRI,  SG_UF_NOT, ID_MUNICIP, NM_PACIENT, DT_NASC, NU_IDADE_N, CS_SEXO, CS_GESTANT, CS_ESCOL_N, NM_MAE_PAC, ID_MN_RESI, SG_UF, ID_RG_RESI, NM_LOGRADO, NU_NUMERO, NM_BAIRRO, NU_CEP, CS_ZONA, DT_DIGITA, DT_INVEST, FEBRE, MIALGIA, CEFALEIA, EXANTEMA, VOMITO, NAUSEA, DOR_COSTAS, CONJUNTVIT, ARTRITE, ARTRALGIA, PETEQUIA_N, LEUCOPENIA, LACO, DOR_RETRO, DIABETES, HEMATOLOG, HEPATOPAT, RENAL, HIPERTENSA, ACIDO_PEPT, AUTO_IMUNE, DT_SORO, RESUL_SORO, DT_PCR, RESUL_PCR_, SOROTIPO, CLASSI_FIN, CRITERIO, TPAUTOCTO, COUFINF, COMUNINF, CO_BAINF, EVOLUCAO, HOSPITALIZ, DT_INTERNA, DT_OBITO, DT_ENCERRA, DT_ALRM, ALRM_LETAR, ALRM_HEPAT, ALRM_LIQ, ALRM_HIPOT, ALRM_PLAQ, ALRM_VOM, ALRM_SANG, ALRM_HEMAT, ALRM_ABDOM, DT_GRAV, GRAV_PULSO, GRAV_CONV, GRAV_ENCH, GRAV_INSUF, GRAV_TAQUI, GRAV_EXTRE, GRAV_HIPOT, GRAV_HEMAT, GRAV_MELEN, GRAV_METRO, GRAV_SANG, GRAV_AST, GRAV_MIOC, GRAV_CONSC, GRAV_ORGAO, MANI_HEMOR, EPISTAXE, GENGIVO, METRO, DS_OBS)

#####################################################################################################################
###########
###Transformando coluna de semana epidemiológica de fator para numérica (passando por texto, se for direto, ela transforma ###200905 em 06. 
###Seria possível realizar busca de SE passando direto de fator para numérica utilizando as.integer
###(DENGON2009 ###%>% filter(ID_MUNICIP == 410165, SEM_PRI == 6) -1, para buscar SE 05?
##Será usado para buscar semanas epidemiológicas
##

DENGON2022$SEM_PRI <-as.numeric(as.character(DENGON2022$SEM_PRI))

DENGON2023$SEM_PRI <-as.numeric(as.character(DENGON2023$SEM_PRI))

####Tabela de notificações SINAN###

PR_DENGUE_22_23_AUX01 <- DENGON2022 %>% 
  filter(SEM_PRI >= 202231)

PR_DENGUE_22_23_AUX02 <- DENGON2023 %>% 
  filter(SEM_PRI <=202330)

PR_DENGUE_22_23_SINAN <- rbind(PR_DENGUE_22_23_AUX01, PR_DENGUE_22_23_AUX02)

rm(PR_DENGUE_22_23_AUX01, PR_DENGUE_22_23_AUX02, DENGON2022, DENGON2023)


PR_DENGUE_22_23_GERAL <- BASE_IBGE[,-4]

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

#####Salvando planilhas em CSV na área de trabalho.
#write.xlsx(PR_CHIK_22_23_GERAL, file = "/home/gustavo/Área de Trabalho/PR_CHIK_22_23_GERAL.xlsx")

#write.xlsx(PR_DENGUE_22_23_GERAL, file = "/home/gustavo/Área de Trabalho/PR_DENGUE_22_23_GERAL.xlsx")


###########

library(geobr)
library(ggspatial)
library(ggplot2)

###    MAPAS      ###
###########################################################################################################
##############   Abaixo, preparando o objeto PR_DENGUE_22_23 para entrar no left-Join  ####################
##############   com o read_municipality do geobr. Futuramente, padronizar o Base_IBGE ####################
###########################################################################################################

PR_DENGUE_22_23_GERAL[104, 2] <- "BELA VISTA DA CAROBA"
PR_DENGUE_22_23_GERAL[360, 2] <- "DIAMANTE D'OESTE"
PR_DENGUE_22_23_GERAL[93, 2] <- "ITAPEJARA D'OESTE"
PR_DENGUE_22_23_GERAL[265, 2] <- "MUNHOZ DE MELO"
PR_DENGUE_22_23_GERAL[117, 2] <- "PÉROLA D'OESTE"
PR_DENGUE_22_23_GERAL[184, 2] <- "RANCHO ALEGRE D'OESTE"
PR_DENGUE_22_23_GERAL[239, 2] <- "SANTA CRUZ DE MONTE CASTELO"
PR_DENGUE_22_23_GERAL[127, 2] <- "SÃO JORGE D'OESTE"

####  SOROTIPOS ESTADO   ##########

MAPA_BASE_PR <- read_municipality(code_muni = "PR", year = 2020)

MAPA_BASE_PR$name_muni <- toupper(MAPA_BASE_PR$name_muni)

MAPA_BASE_PR <- left_join(MAPA_BASE_PR, PR_DENGUE_22_23_GERAL, by = c("name_muni" = "Município_sem_Código"))

AUX_I <- MAPA_BASE_PR %>% filter(DENV_I > 0 & DENV_II == 0 & DENV_III == 0 & DENV_IV == 0)
AUX_II <- MAPA_BASE_PR %>% filter(DENV_I == 0 & DENV_II > 0 & DENV_III == 0 & DENV_IV == 0)
AUX_III <- MAPA_BASE_PR %>% filter(DENV_I > 0 & DENV_II > 0 & DENV_III == 0 & DENV_IV == 0)
AUX_IV <- MAPA_BASE_PR %>% filter(DENV_I > 0 & DENV_II > 0 & DENV_III > 0 & DENV_IV == 0)
AUX_V <- MAPA_BASE_PR %>% filter(DENV_I > 0 & DENV_II == 0 & DENV_III > 0 & DENV_IV == 0)
AUX_VI <- MAPA_BASE_PR %>% filter(DENV_I == 0 & DENV_II > 0 & DENV_III > 0 & DENV_IV == 0)

ggplot() + 
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
  annotation_north_arrow(location = "tl", 
                         which_north = "true") +
  theme_minimal() +
  scale_fill_manual (name = "Sorotipos",
                     values = c("I" = "#F0E68C", 
                                "II" = "#FF0000",
                                "I, II" = "black",
                                "I, III" ="gray")) +
  theme(legend.position = "bottom") +
  labs(caption = "Fonte", 
       title = "Sorotipo Circulante - Paraná") + 
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold", 
                                   size = 14,
                                   colour = "#556B2F")
  ) 

### Mapa Incidência Paraná  ####

MAPA_BASE_PR$Cat <- with(MAPA_BASE_PR, cut(x = Incidencia,
                                                 breaks = c(-Inf, 0, 50, 100, 300, 500, Inf),
                                                 labels = c("0 casos", "0,001 - 50", "50,001 - 100", 
                                                            "100,001 - 300", "300,001 - 500", ">500"))
                               )

ggplot() + 
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
  labs(caption = "Fonte", 
       title = "Incidência de Dengue - Paraná",
       subtitle = "Casos Autóctones por 100.000 Habitantes") + 
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold", 
                                   size = 14,
                                   colour = "Black")
  ) 

### Mapa Incidência Paraná  ####

MAPA_BASE_PR$Cat <- with(MAPA_BASE_PR, cut(x = Incidencia_Provaveis,
                                           breaks = c(-Inf, 0, 50, 100, 300, 500, Inf),
                                           labels = c("0 casos", "0,001 - 50", "50,001 - 100", 
                                                      "100,001 - 300", "300,001 - 500", ">500"))
)

ggplot() + 
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
  labs(caption = "Fonte", 
       title = "Incidência de Dengue - Paraná",
       subtitle = "Casos Prováveis por 100.000 Habitantes
Casos Prováveis = Casos Notificados - Casos Descartados") + 
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold", 
                                   size = 14,
                                   colour = "Black")
  )

### Mapa chikungunya Estado    #####

PR_CHIK_22_23_GERAL$Provaveis <- as.integer(PR_CHIK_22_23_GERAL$Notificados) - as.integer(PR_CHIK_22_23_GERAL$Descartados)

PR_CHIK_22_23_MAPA <- read_municipality(code_muni = "PR", year = 2020)

PR_CHIK_22_23_MAPA$name_muni <- toupper(PR_CHIK_22_23_MAPA$name_muni)

PR_CHIK_22_23_GERAL[104, 2] <- "BELA VISTA DA CAROBA"
PR_CHIK_22_23_GERAL[360, 2] <- "DIAMANTE D'OESTE"
PR_CHIK_22_23_GERAL[93, 2] <- "ITAPEJARA D'OESTE"
PR_CHIK_22_23_GERAL[265, 2] <- "MUNHOZ DE MELO"
PR_CHIK_22_23_GERAL[117, 2] <- "PÉROLA D'OESTE"
PR_CHIK_22_23_GERAL[184, 2] <- "RANCHO ALEGRE D'OESTE"
PR_CHIK_22_23_GERAL[239, 2] <- "SANTA CRUZ DE MONTE CASTELO"
PR_CHIK_22_23_GERAL[127, 2] <- "SÃO JORGE D'OESTE"

PR_CHIK_22_23_MAPA <- left_join(PR_CHIK_22_23_MAPA, PR_CHIK_22_23_GERAL, by = c("name_muni" = "Município_sem_Código"))

PR_CHIK_22_23_MAPA_PROVAVEIS <- PR_CHIK_22_23_MAPA %>% filter(Provaveis > 0)

PR_CHIK_22_23_MAPA_AUTOCTONES <- PR_CHIK_22_23_MAPA %>% filter(Autoctones > 0)

ggplot() + 
  geom_sf(data = PR_CHIK_22_23_MAPA, 
          color = "black", 
          fill = "white") + 
  geom_sf(data = PR_CHIK_22_23_MAPA_PROVAVEIS, 
          color ="black", 
          aes(fill = "Prováveis")) +     
  geom_sf(data = PR_CHIK_22_23_MAPA_AUTOCTONES, 
          color ="black", 
          aes(fill = "Autóctones")) +
  annotation_north_arrow(location = "tl", 
                         which_north = "true") +
  annotation_scale(location = "br") +
  theme_minimal() +
  scale_fill_manual (name = "", 
                     values = c("Prováveis" = "#F0E68C", 
                                "Autóctones" = "#FF0000")) +
  theme(legend.position = "bottom") +
  coord_sf(expand = FALSE)


#############################################################################################################################################
#############################################################################################################################################
#######################################  TABELA SINAIS E SINTOMAS - PARANÁ    ###############################################################
#############################################################################################################################################

PR_22_23_AUX01 <- CHIKON2022 %>% 
  filter(SEM_PRI >= 202231)

PR_22_23_AUX02 <- CHIKON2023 %>% 
  filter(SEM_PRI <=202330)

PR_22_23_SINAN_CHIK <- rbind(PR_22_23_AUX01, PR_22_23_AUX02)

rm(PR_22_23_AUX01, PR_22_23_AUX02)

PR_22_23_SINAN_DECODIFICADO_CHIK <- PR_22_23_SINAN_CHIK

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

png(filename = "/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/PR_22_23_GRAF_SINAIS_CHIK.png", 
       width = 33,
       height = 20,
       units = "cm", pointsize = 8, res = 300)

PR_22_23_GRAF_SINAIS_CHIK

dev.off()

rm(CHIKON2022, 
   CHIKON2023,
   AUX_GRAF,
   BASE_IBGE,
   BASE_IBGE_BRASIL,
   Fonte,
   i,
   PR_22_23_GRAF_SINAIS_CHIK)

warnings()