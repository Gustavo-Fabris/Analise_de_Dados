##############   SCRIPT A SER TRABALHADO PARA UNIFICAR A BASE DO LACEN!!!!!!!!!!!!!!!!!##################
###teste <- RS22_22_23_LACEN_PESQ_ARBO %>% filter(Dt_Cadastro >= as.Date("2023-01-01") & Dt_Cadastro <= as.Date("2023-07-01), Exame == "Pesquisa de Arbovírus" | Exame == "Dengue")



library(stringr)
library(lubridate)
library(dplyr)
library(ggplot2)


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

###### Lubridate  para criar SE a partir de darta ######

RS22_22_23_LACEN_SOROLOGIA$SE <- epiweek(AUX$Data)

####################  uSANDO STRINGR PARA ACENTUAR A TABELA DO lacen TORNANDO-A COMPATÍVEL COM A base_ibge   ###################################

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

RS22_22_23_SE_SOROLOGIA_REAGENTE[17,2:54] <- apply(RS22_22_23_SE_SOROLOGIA_REAGENTE[,2:54], 2, sum)

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

AUX$Sem_EPI <-as.character(c("2022/31",  "2022/32", "2022/33",  "2022/34",  "2022/35",  "2022/36",  "2022/37",  "2022/38",  "2022/39",  "2022/40",  "2022/41",  "2022/42",  "2022/43",  "2022/44",  "2022/45",  "2022/46",  "2022/47",  "2022/48",  "2022/49",  "2022/50",  "2022/51",  "2022/52",  "2022/53",  "2023/01",  "2023/02",  "2023/03",  "2023/04",  "2023/05",  "2023/06",  "2023/07",  "2023/08",  "2023/09",  "2023/10", "2023/11",  "2023/12",  "2023/13",  "2023/14",  "2023/15",  "2023/16",  "2023/17",  "2023/18",  "2023/19",  "2023/20",  "2023/21",  "2023/22",  "2023/23",  "2023/24",  "2023/25",  "2023/26",  "2023/27",  "2023/28",  "2023/29",  "2023/30"))


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
       title = "Porcentual de Amostras Positivas/SE - Unidade Sentinela") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 15,
                                   colour = "#556B2F")) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#EEE8AA") + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.001)))

################################################################################################################
######################  Amostras encaminhadas para o LACEN (sorologia e pesq. de arbovírus)  ###################

AUX <- RS22_22_23_SE_SOROLOGIA[17,]

AUX[2, ] <- colnames(RS22_22_23_SE_SOROLOGIA)

AUX <- AUX[c(2, 1),]

AUX <- as.data.frame(t(AUX))

colnames(AUX) <- AUX[1, ]

AUX <- AUX[-1, ]

AUX[, 2] <- as.numeric(AUX[, 2])

AUX$Sem_EPI <-as.character(c("2022/31",  "2022/32", "2022/33",  "2022/34",  "2022/35",  "2022/36",  "2022/37",  "2022/38",  "2022/39",  "2022/40",  "2022/41",  "2022/42",  "2022/43",  "2022/44",  "2022/45",  "2022/46",  "2022/47",  "2022/48",  "2022/49",  "2022/50",  "2022/51",  "2022/52",  "2022/53",  "2023/01",  "2023/02",  "2023/03",  "2023/04",  "2023/05",  "2023/06",  "2023/07",  "2023/08",  "2023/09",  "2023/10", "2023/11",  "2023/12",  "2023/13",  "2023/14",  "2023/15",  "2023/16",  "2023/17",  "2023/18",  "2023/19",  "2023/20",  "2023/21",  "2023/22",  "2023/23",  "2023/24",  "2023/25",  "2023/26",  "2023/27",  "2023/28",  "2023/29",  "2023/30"))


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

AUX$Sem_EPI <-as.character(c("2022/31",  "2022/32", "2022/33",  "2022/34",  "2022/35",  "2022/36",  "2022/37",  "2022/38",  "2022/39",  "2022/40",  "2022/41",  "2022/42",  "2022/43",  "2022/44",  "2022/45",  "2022/46",  "2022/47",  "2022/48",  "2022/49",  "2022/50",  "2022/51",  "2022/52",  "2022/53",  "2023/01",  "2023/02",  "2023/03",  "2023/04",  "2023/05",  "2023/06",  "2023/07",  "2023/08",  "2023/09",  "2023/10", "2023/11",  "2023/12",  "2023/13",  "2023/14",  "2023/15",  "2023/16",  "2023/17",  "2023/18",  "2023/19",  "2023/20",  "2023/21",  "2023/22",  "2023/23",  "2023/24",  "2023/25",  "2023/26",  "2023/27",  "2023/28",  "2023/29",  "2023/30"))

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
       title = "Porcentual de Amostras (Sorologia) Reagentes/SE - 22ª RS") +
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

RS22_22_23_GRAF_1 <- (RS22_22_23_GRAF_US_TOTAL / RS22_22_23_GRAF_US_DETEC)
png(filename = "/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/RS22_22_23_INFORME_Pag_18A.png", 
    width = 40,
    height = 25,
    units = "cm", pointsize = 8, res = 300)

RS22_22_23_GRAF_1

dev.off()

RS22_22_23_GRAF_1 <- (RS22_22_23_GRAF_SORO_TOTAL / RS22_22_23_GRAF_SORO_REAG)
png(filename = "/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/RS22_22_23_INFORME_Pag_18B.png", 
    width = 40,
    height = 25,
    units = "cm", pointsize = 8, res = 300)

RS22_22_23_GRAF_1

dev.off()
