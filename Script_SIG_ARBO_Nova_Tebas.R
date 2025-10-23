rm(list =ls())

library (googlesheets4)

setwd("/home/gustavo/Área de trabalho/Análise_de_Dados/")

gs4_auth()

PR_2025_Chik <- read_sheet ("https://docs.google.com/spreadsheets/d/1ul_41n7CWg7YG0jAMKGoh4u_pmVXCmSeOnh6cOsXr9k/edit?gid=1019073941#gid=1019073941", 
                                           sheet ="Chikungunya_PR")

PR_2025_Dengue <- read_sheet ("https://docs.google.com/spreadsheets/d/1ul_41n7CWg7YG0jAMKGoh4u_pmVXCmSeOnh6cOsXr9k/edit?gid=1019073941#gid=1019073941", 
                            sheet ="Dengue_PR")

RS22_2025_Chik <- read_sheet ("https://docs.google.com/spreadsheets/d/1ul_41n7CWg7YG0jAMKGoh4u_pmVXCmSeOnh6cOsXr9k/edit?gid=1019073941#gid=1019073941", 
                            sheet ="Chikungunya")

RS22_2025_Dengue <- read_sheet ("https://docs.google.com/spreadsheets/d/1ul_41n7CWg7YG0jAMKGoh4u_pmVXCmSeOnh6cOsXr9k/edit?gid=1019073941#gid=1019073941", 
                              sheet ="Dengue")

RS_Nova_Tebas_SINAN <- read_sheet ("https://docs.google.com/spreadsheets/d/1etqgUoPBa5nPOeJiR9QfenXhPQPzWGubb2WRjGjtJds/edit?gid=1649656412#gid=1649656412", 
                                  sheet ="Notificações_SINAN")

RS_Nova_Tebas_Locais_Risco <- read_sheet ("https://docs.google.com/spreadsheets/d/1etqgUoPBa5nPOeJiR9QfenXhPQPzWGubb2WRjGjtJds/edit?gid=1649656412#gid=1649656412", 
                                      sheet ="Locais de Risco")

RS_Nova_Tebas_Locais_Risco <- as.data.frame(RS_Nova_Tebas_Locais_Risco)

RS_Nova_Tebas_PE <- read_sheet ("https://docs.google.com/spreadsheets/d/1etqgUoPBa5nPOeJiR9QfenXhPQPzWGubb2WRjGjtJds/edit?gid=1649656412#gid=1649656412", 
                                      sheet ="PE")

RS_Nova_Tebas_Assistencia <- read_sheet ("https://docs.google.com/spreadsheets/d/1etqgUoPBa5nPOeJiR9QfenXhPQPzWGubb2WRjGjtJds/edit?gid=1649656412#gid=1649656412", 
                                      sheet ="Assistência")

RS_Nova_Tebas_Ovitrampas <- read_sheet ("https://docs.google.com/spreadsheets/d/1etqgUoPBa5nPOeJiR9QfenXhPQPzWGubb2WRjGjtJds/edit?gid=1649656412#gid=1649656412", 
                                      sheet ="SIG_OVITRAMPAS")

colnames(RS_Nova_Tebas_Ovitrampas)[9] <- "Instalacao_1"
colnames(RS_Nova_Tebas_Ovitrampas)[10] <- "Coleta_1"
colnames(RS_Nova_Tebas_Ovitrampas)[11] <- "OBS_1"
colnames(RS_Nova_Tebas_Ovitrampas)[12] <- "Resultado_1"

colnames(RS_Nova_Tebas_Ovitrampas)[13] <- "Instalacao_2"
colnames(RS_Nova_Tebas_Ovitrampas)[14] <- "Coleta_2"
colnames(RS_Nova_Tebas_Ovitrampas)[15] <- "OBS_2"
colnames(RS_Nova_Tebas_Ovitrampas)[16] <- "Resultado_2"

colnames(RS_Nova_Tebas_Ovitrampas)[17] <- "Instalacao_3"
colnames(RS_Nova_Tebas_Ovitrampas)[18] <- "Coleta_3"
colnames(RS_Nova_Tebas_Ovitrampas)[19] <- "OBS_3"
colnames(RS_Nova_Tebas_Ovitrampas)[20] <- "Resultado_3"

colnames(RS_Nova_Tebas_Ovitrampas)[21] <- "Instalacao_4"
colnames(RS_Nova_Tebas_Ovitrampas)[22] <- "Coleta_4"
colnames(RS_Nova_Tebas_Ovitrampas)[23] <- "OBS_4"
colnames(RS_Nova_Tebas_Ovitrampas)[24] <- "Resultado_4"

RS_Nova_Tebas_SINAN_10S <- read_sheet ("https://docs.google.com/spreadsheets/d/1etqgUoPBa5nPOeJiR9QfenXhPQPzWGubb2WRjGjtJds/edit?gid=1649656412#gid=1649656412", 
                                     sheet ="Notificações_10S")

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

write.csv(RS_Nova_Tebas_Locais_Risco, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/Arboviroses/RS22_LOCAIS_RISCO_Nova_Tebas.csv",
          row.names = FALSE)

write.csv(RS_Nova_Tebas_SINAN, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/Arboviroses/RS22_2025_SINAN_DECODIFICADO_Nova_Tebas.csv",
          row.names = FALSE)

write.csv(RS_Nova_Tebas_PE, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/Arboviroses/RS22_2025_PE_Nova_Tebas.csv",
          row.names = FALSE)

write.csv(RS_Nova_Tebas_Assistencia, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/Arboviroses/RS22_2025_Assistencia_Nova_Tebas.csv",
          row.names = FALSE)

write.csv(RS_Nova_Tebas_Ovitrampas, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/Arboviroses/RS22_2025_Ovitrampas_Nova_Tebas.csv",
          row.names = FALSE)

write.csv(RS_Nova_Tebas_SINAN_10S, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/Arboviroses/RS22_2025_SINAN_DECODIFICADO_10S_Nova_Tebas.csv",
          row.names = FALSE)

