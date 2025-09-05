rm(list =ls())

library (googlesheets4)

setwd("/home/gustavo/Área de trabalho/Análise_de_Dados/")

gs4_auth()

########    Buscando os dados na planilha geral

PR_2025_Chik <- read_sheet ("https://docs.google.com/spreadsheets/d/12Mdi2aAFohbGWi6Tfmju_nctvPCxiuLkdgG_8tzj5RM/edit?gid=0#gid=0", 
                            sheet ="Chikungunya_PR")

PR_2025_Dengue <- read_sheet ("https://docs.google.com/spreadsheets/d/12Mdi2aAFohbGWi6Tfmju_nctvPCxiuLkdgG_8tzj5RM/edit?gid=0#gid=0", 
                              sheet ="Dengue_PR")

RS22_2025_Chik <- read_sheet ("https://docs.google.com/spreadsheets/d/12Mdi2aAFohbGWi6Tfmju_nctvPCxiuLkdgG_8tzj5RM/edit?gid=0#gid=0", 
                              sheet ="Chikungunya")

RS22_2025_Dengue <- read_sheet ("https://docs.google.com/spreadsheets/d/12Mdi2aAFohbGWi6Tfmju_nctvPCxiuLkdgG_8tzj5RM/edit?gid=0#gid=0", 
                                sheet ="Dengue")

RS_Ivaipora_SINAN <- read_sheet ("https://docs.google.com/spreadsheets/d/12Mdi2aAFohbGWi6Tfmju_nctvPCxiuLkdgG_8tzj5RM/edit?gid=0#gid=0", 
                                      sheet ="SINAN")

RS_Ivaipora_Locais_Risco <- read_sheet ("https://docs.google.com/spreadsheets/d/12Mdi2aAFohbGWi6Tfmju_nctvPCxiuLkdgG_8tzj5RM/edit?gid=0#gid=0", 
                                             sheet ="Locais de Risco")

RS_Ivaipora_Locais_Risco <- as.data.frame(RS_Ivaipora_Locais_Risco)

RS_Ivaipora_PE <- read_sheet ("https://docs.google.com/spreadsheets/d/12Mdi2aAFohbGWi6Tfmju_nctvPCxiuLkdgG_8tzj5RM/edit?gid=0#gid=0", 
                                   sheet ="PE")

RS_Ivaipora_Assistencia <- read_sheet ("https://docs.google.com/spreadsheets/d/12Mdi2aAFohbGWi6Tfmju_nctvPCxiuLkdgG_8tzj5RM/edit?gid=0#gid=0", 
                                            sheet ="Assistência")

RS_Ivaipora_Ovitrampas <- read_sheet ("https://docs.google.com/spreadsheets/d/12Mdi2aAFohbGWi6Tfmju_nctvPCxiuLkdgG_8tzj5RM/edit?gid=0#gid=0", 
                                           sheet ="Ovitrampas")

colnames(RS_Ivaipora_Ovitrampas)[9] <- "Instalacao_1"
colnames(RS_Ivaipora_Ovitrampas)[10] <- "Coleta_1"
colnames(RS_Ivaipora_Ovitrampas)[11] <- "OBS_1"
colnames(RS_Ivaipora_Ovitrampas)[12] <- "Resultado_1"

colnames(RS_Ivaipora_Ovitrampas)[13] <- "Instalacao_2"
colnames(RS_Ivaipora_Ovitrampas)[14] <- "Coleta_2"
colnames(RS_Ivaipora_Ovitrampas)[15] <- "OBS_2"
colnames(RS_Ivaipora_Ovitrampas)[16] <- "Resultado_2"

colnames(RS_Ivaipora_Ovitrampas)[17] <- "Instalacao_3"
colnames(RS_Ivaipora_Ovitrampas)[18] <- "Coleta_3"
colnames(RS_Ivaipora_Ovitrampas)[19] <- "OBS_3"
colnames(RS_Ivaipora_Ovitrampas)[20] <- "Resultado_3"

colnames(RS_Ivaipora_Ovitrampas)[21] <- "Instalacao_4"
colnames(RS_Ivaipora_Ovitrampas)[22] <- "Coleta_4"
colnames(RS_Ivaipora_Ovitrampas)[23] <- "OBS_4"
colnames(RS_Ivaipora_Ovitrampas)[24] <- "Resultado_4"

RS_Ivaipora_SINAN_10S <- read_sheet ("https://docs.google.com/spreadsheets/d/12Mdi2aAFohbGWi6Tfmju_nctvPCxiuLkdgG_8tzj5RM/edit?gid=0#gid=0", 
                                      sheet ="SINAN_10S")

###############   Salvando no computador local para uso pelo SIG

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

write.csv(RS_Ivaipora_Locais_Risco, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/Arboviroses/RS22_LOCAIS_RISCO_Ivaipora.csv",
          row.names = FALSE)

write.csv(RS_Ivaipora_SINAN, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/Arboviroses/RS22_2025_SINAN_DECODIFICADO_Ivaipora.csv",
          row.names = FALSE)

write.csv(RS_Ivaipora_PE, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/Arboviroses/RS22_2025_PE_Ivaipora.csv",
          row.names = FALSE)

write.csv(RS_Ivaipora_Assistencia, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/Arboviroses/RS22_2025_Assistencia_Ivaipora.csv",
          row.names = FALSE)

RS_Ivaipora_Ovitrampas <- as.data.frame(RS_Ivaipora_Ovitrampas)

RS_Ivaipora_Ovitrampas[, 3] <- as.numeric(RS_Ivaipora_Ovitrampas[, 3])

write.csv(RS_Ivaipora_Ovitrampas, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/Arboviroses/RS22_2025_Ovitrampas_Ivaipora.csv",
          row.names = FALSE)

write.csv(RS_Ivaipora_SINAN_10S, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/Arboviroses/RS22_2025_SINAN_DECODIFICADO_10S_Ivaipora.csv",
          row.names = FALSE)

