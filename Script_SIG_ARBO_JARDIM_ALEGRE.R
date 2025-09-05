rm(list =ls())

library (googlesheets4)

setwd("/home/gustavo/Área de trabalho/Análise_de_Dados/")

PR_2025_Chik <- read_sheet ("https://docs.google.com/spreadsheets/d/1ul_41n7CWg7YG0jAMKGoh4u_pmVXCmSeOnh6cOsXr9k/edit?gid=1019073941#gid=1019073941", 
                                           sheet ="Chikungunya_PR")

PR_2025_Dengue <- read_sheet ("https://docs.google.com/spreadsheets/d/1ul_41n7CWg7YG0jAMKGoh4u_pmVXCmSeOnh6cOsXr9k/edit?gid=1019073941#gid=1019073941", 
                            sheet ="Dengue_PR")

RS22_2025_Chik <- read_sheet ("https://docs.google.com/spreadsheets/d/1ul_41n7CWg7YG0jAMKGoh4u_pmVXCmSeOnh6cOsXr9k/edit?gid=1019073941#gid=1019073941", 
                            sheet ="Chikungunya")

RS22_2025_Dengue <- read_sheet ("https://docs.google.com/spreadsheets/d/1ul_41n7CWg7YG0jAMKGoh4u_pmVXCmSeOnh6cOsXr9k/edit?gid=1019073941#gid=1019073941", 
                              sheet ="Dengue")

RS_Jardim_Alegre_SINAN <- read_sheet ("https://docs.google.com/spreadsheets/d/13HgONdkmmWhhufINnwVzmZhxBBnOLDmZm75uGR5R7lU/edit?gid=799627228#gid=799627228", 
                                  sheet ="Notificações SINAN")

RS_Jardim_Alegre_Locais_Risco <- read_sheet ("https://docs.google.com/spreadsheets/d/13HgONdkmmWhhufINnwVzmZhxBBnOLDmZm75uGR5R7lU/edit?gid=1454301175#gid=1454301175", 
                                      sheet ="Locais de Risco")

RS_Jardim_Alegre_Locais_Risco <- as.data.frame(RS_Jardim_Alegre_Locais_Risco)

RS_Jardim_Alegre_PE <- read_sheet ("https://docs.google.com/spreadsheets/d/13HgONdkmmWhhufINnwVzmZhxBBnOLDmZm75uGR5R7lU/edit?gid=199328894#gid=199328894", 
                                      sheet ="PE")

RS_Jardim_Alegre_Assistencia <- read_sheet ("https://docs.google.com/spreadsheets/d/13HgONdkmmWhhufINnwVzmZhxBBnOLDmZm75uGR5R7lU/edit?gid=1019091010#gid=1019091010", 
                                      sheet ="Assistência")

RS_Jardim_Alegre_Ovitrampas <- read_sheet ("https://docs.google.com/spreadsheets/d/13HgONdkmmWhhufINnwVzmZhxBBnOLDmZm75uGR5R7lU/edit?gid=297610919#gid=297610919", 
                                      sheet ="SIG_OVITRAMPAS")

colnames(RS_Jardim_Alegre_Ovitrampas)[9] <- "Instalacao_1"
colnames(RS_Jardim_Alegre_Ovitrampas)[10] <- "Coleta_1"
colnames(RS_Jardim_Alegre_Ovitrampas)[11] <- "OBS_1"
colnames(RS_Jardim_Alegre_Ovitrampas)[12] <- "Resultado_1"

colnames(RS_Jardim_Alegre_Ovitrampas)[13] <- "Instalacao_2"
colnames(RS_Jardim_Alegre_Ovitrampas)[14] <- "Coleta_2"
colnames(RS_Jardim_Alegre_Ovitrampas)[15] <- "OBS_2"
colnames(RS_Jardim_Alegre_Ovitrampas)[16] <- "Resultado_2"

colnames(RS_Jardim_Alegre_Ovitrampas)[17] <- "Instalacao_3"
colnames(RS_Jardim_Alegre_Ovitrampas)[18] <- "Coleta_3"
colnames(RS_Jardim_Alegre_Ovitrampas)[19] <- "OBS_3"
colnames(RS_Jardim_Alegre_Ovitrampas)[20] <- "Resultado_3"

colnames(RS_Jardim_Alegre_Ovitrampas)[21] <- "Instalacao_4"
colnames(RS_Jardim_Alegre_Ovitrampas)[22] <- "Coleta_4"
colnames(RS_Jardim_Alegre_Ovitrampas)[23] <- "OBS_4"
colnames(RS_Jardim_Alegre_Ovitrampas)[24] <- "Resultado_4"

write.csv(PR_2025_Chik, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/Arboviroses/PR_2025_SIG_CHIK.csv",
          row.names = FALSE)

write.csv(PR_2025_Dengue, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/Arboviroses/PR_2025_SIG_DENGUE.csv",
          row.names = FALSE)

write.csv(RS22_2025_Chik, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/Arboviroses/RS22_2025_SIG_CHIK.csv",
          row.names = FALSE)

write.csv(RS22_2025_Dengue, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/Arboviroses/RS22_2025_SIG_DENGUE.csv",
          row.names = FALSE)

write.csv(RS_Jardim_Alegre_Locais_Risco, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/Arboviroses/RS22_LOCAIS_RISCO_Jardim_Alegre.csv",
          row.names = FALSE)

write.csv(RS_Jardim_Alegre_SINAN, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/Arboviroses/RS22_2025_SINAN_DECODIFICADO_Jardim_Alegre.csv",
          row.names = FALSE)

write.csv(RS_Jardim_Alegre_PE, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/Arboviroses/RS22_2025_PE_Jardim_Alegre.csv",
          row.names = FALSE)

write.csv(RS_Jardim_Alegre_Assistencia, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/Arboviroses/RS22_2025_Assistencia_Jardim_Alegre.csv",
          row.names = FALSE)

write.csv(RS_Jardim_Alegre_Ovitrampas, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/Arboviroses/RS22_2025_Ovitrampas_Jardim_Alegre.csv",
          row.names = FALSE)

