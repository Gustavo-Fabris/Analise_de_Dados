
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
           paste0("Base_de_Dados/Tabulacoes_R/Raiva/RS22", RS, "_ANTRAB_2012_EXPOSICAO.csv"), 
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
           paste0("Base_de_Dados/Tabulacoes_R/Raiva/RS22", RS, "_ANTRAB_2012_LOCALIZACAO.csv"), 
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
           paste0("Base_de_Dados/Tabulacoes_R/Raiva/RS22", RS, "_ANTRAB_2012_FERIMENTO.csv"), 
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

for(i in BASE_IBGE[(which(BASE_IBGE$RS == 22)), 2]){
  
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
           paste0("Base_de_Dados/Tabulacoes_R/Raiva/RS22", RS, "_ANTRAB_2012_TIPO_FERIMENTO.csv"), 
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
           paste0("Base_de_Dados/Tabulacoes_R/Raiva/RS22", RS, "_ANTRAB_2012_ANTECEDENTE_ANTR.csv"), 
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

AUX$Herbivaro_Domestico <- NA

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
           paste0("Base_de_Dados/Tabulacoes_R/Raiva/RS22", RS, "_ANTRAB_2012_AGRESSOR.csv"), 
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
           paste0("Base_de_Dados/Tabulacoes_R/Raiva/RS22", RS, "_ANTRAB_2012_COND_ANIMAL_ACID.csv"), 
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
           paste0("Base_de_Dados/Tabulacoes_R/Raiva/RS22", RS, "_ANTRAB_2012_OBSERVAVEL.csv"), 
           row.names = FALSE)

###Tabela Tratamento (campo 43 do SINAN)####

SINAN_ANTRAB_2012_TRATAMENTO <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == 22), 3])

SINAN_ANTRAB_2012_TRATAMENTO$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == 22), 2]

SINAN_ANTRAB_2012_TRATAMENTO$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == 22), 5]

SINAN_ANTRAB_2012_TRATAMENTO$RS <- BASE_IBGE[which(BASE_IBGE$RS == 22), 1]

SINAN_ANTRAB_2012_TRATAMENTO <- SINAN_ANTRAB_2012_TRATAMENTO[,c(4, 1, 2, 3)]

SINAN_ANTRAB_2012_TRATAMENTO$Dispensa_Tratamento <- NA

SINAN_ANTRAB_2012_TRATAMENTO$Observação_Animal <- NA

SINAN_ANTRAB_2012_TRATAMENTO$Observação_Vacina <- NA

SINAN_ANTRAB_2012_TRATAMENTO$Vacina <- NA

SINAN_ANTRAB_2012_TRATAMENTO$Soro_Vacina <- NA

SINAN_ANTRAB_2012_TRATAMENTO$Esquema_Reexposicao <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == 22)), 2]){
  
  SINAN_ANTRAB_2012_TRATAMENTO[which(SINAN_ANTRAB_2012_TRATAMENTO$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                                                                   filter(ID_MN_RESI == i,
                                                                                                          TRAT_ATUAL == "2") %>%   
                                                                                                   count()
  )    
  
  SINAN_ANTRAB_2012_TRATAMENTO[which(SINAN_ANTRAB_2012_TRATAMENTO$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                                                                   filter(ID_MN_RESI == i,
                                                                                                          TRAT_ATUAL == "3") %>%   
                                                                                                   count()
  )
  
  SINAN_ANTRAB_2012_TRATAMENTO[which(SINAN_ANTRAB_2012_TRATAMENTO$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                                                                   filter(ID_MN_RESI == i,
                                                                                                          TRAT_ATUAL == "4") %>%   
                                                                                                   count()
  )
  
  SINAN_ANTRAB_2012_TRATAMENTO[which(SINAN_ANTRAB_2012_TRATAMENTO$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                                                                   filter(ID_MN_RESI == i,
                                                                                                          TRAT_ATUAL == "5") %>%   
                                                                                                   count()
  )
  
  SINAN_ANTRAB_2012_TRATAMENTO[which(SINAN_ANTRAB_2012_TRATAMENTO$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                                                                   filter(ID_MN_RESI == i,
                                                                                                          TRAT_ATUAL == "6") %>%   
                                                                                                   count()
  )
  
  SINAN_ANTRAB_2012_TRATAMENTO[which(SINAN_ANTRAB_2012_TRATAMENTO$COD_IBGE == i), 10] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                                                                    filter(ID_MN_RESI == i,
                                                                                                           TRAT_ATUAL == "7") %>%   
                                                                                                    count()
  )
}

SINAN_ANTRAB_2012_TRATAMENTO[17, 4:10] <- apply(SINAN_ANTRAB_2012_TRATAMENTO[, 4:10], 2, sum)
SINAN_ANTRAB_2012_TRATAMENTO[17, 1] <- "Total"

###Tabela Condição final do Animal (campo 48 do SINAN)###

SINAN_ANTRAB_2012_COND_FINAL_ANIMAL <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == 22), 3])

SINAN_ANTRAB_2012_COND_FINAL_ANIMAL$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == 22), 2]

SINAN_ANTRAB_2012_COND_FINAL_ANIMAL$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == 22), 5]

SINAN_ANTRAB_2012_COND_FINAL_ANIMAL$RS <- BASE_IBGE[which(BASE_IBGE$RS == 22), 1]

SINAN_ANTRAB_2012_COND_FINAL_ANIMAL<- SINAN_ANTRAB_2012_COND_FINAL_ANIMAL[,c(4, 1, 2, 3)]

SINAN_ANTRAB_2012_COND_FINAL_ANIMAL$Negativo_Clinico <- NA

SINAN_ANTRAB_2012_COND_FINAL_ANIMAL$Negativo_Lab <- NA

SINAN_ANTRAB_2012_COND_FINAL_ANIMAL$Positivo_Clinico <- NA

SINAN_ANTRAB_2012_COND_FINAL_ANIMAL$Positivo_Lab <- NA

SINAN_ANTRAB_2012_COND_FINAL_ANIMAL$Morto_SemDiagnostico <- NA

SINAN_ANTRAB_2012_COND_FINAL_ANIMAL$Ignorado <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == 22)), 2]){
  
  SINAN_ANTRAB_2012_COND_FINAL_ANIMAL[which(SINAN_ANTRAB_2012_COND_FINAL_ANIMAL$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                                                                                 filter(ID_MN_RESI == i,
                                                                                                                        FIM_ANIMAL == "1") %>%   
                                                                                                                 count()
  )    
  
  SINAN_ANTRAB_2012_COND_FINAL_ANIMAL[which(SINAN_ANTRAB_2012_COND_FINAL_ANIMAL$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                                                                                 filter(ID_MN_RESI == i,
                                                                                                                        FIM_ANIMAL == "2") %>%   
                                                                                                                 count()
  )
  
  SINAN_ANTRAB_2012_COND_FINAL_ANIMAL[which(SINAN_ANTRAB_2012_COND_FINAL_ANIMAL$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                                                                                 filter(ID_MN_RESI == i,
                                                                                                                        FIM_ANIMAL == "3") %>%   
                                                                                                                 count()
  )
  
  SINAN_ANTRAB_2012_COND_FINAL_ANIMAL[which(SINAN_ANTRAB_2012_COND_FINAL_ANIMAL$COD_IBGE == i), 8] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                                                                                 filter(ID_MN_RESI == i,
                                                                                                                        FIM_ANIMAL == "4") %>%   
                                                                                                                 count()
  )
  
  SINAN_ANTRAB_2012_COND_FINAL_ANIMAL[which(SINAN_ANTRAB_2012_COND_FINAL_ANIMAL$COD_IBGE == i), 9] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                                                                                 filter(ID_MN_RESI == i,
                                                                                                                        FIM_ANIMAL == "5") %>%   
                                                                                                                 count()
  )
  
  SINAN_ANTRAB_2012_COND_FINAL_ANIMAL[which(SINAN_ANTRAB_2012_COND_FINAL_ANIMAL$COD_IBGE == i), 10] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                                                                                  filter(ID_MN_RESI == i,
                                                                                                                         FIM_ANIMAL == "9") %>%   
                                                                                                                  count()
  )
}

SINAN_ANTRAB_2012_COND_FINAL_ANIMAL[17, 4:10] <- apply(SINAN_ANTRAB_2012_COND_FINAL_ANIMAL[, 4:10], 2, sum)
SINAN_ANTRAB_2012_COND_FINAL_ANIMAL[17, 1] <- "Total"

###Tabela Interrupção de tratamento (campo 49 do SINAN)####

SINAN_ANTRAB_2012_INTERRUPCAO_TRAT <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == 22), 3])

SINAN_ANTRAB_2012_INTERRUPCAO_TRAT$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == 22), 2]

SINAN_ANTRAB_2012_INTERRUPCAO_TRAT$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == 22), 5]

SINAN_ANTRAB_2012_INTERRUPCAO_TRAT$RS <- BASE_IBGE[which(BASE_IBGE$RS == 22), 1]

SINAN_ANTRAB_2012_INTERRUPCAO_TRAT <- SINAN_ANTRAB_2012_INTERRUPCAO_TRAT[,c(4, 1, 2, 3)]

SINAN_ANTRAB_2012_INTERRUPCAO_TRAT$Sim <- NA

SINAN_ANTRAB_2012_INTERRUPCAO_TRAT$Nao <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == 22)), 2]){
  
  SINAN_ANTRAB_2012_INTERRUPCAO_TRAT[which(SINAN_ANTRAB_2012_INTERRUPCAO_TRAT$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                                                                               filter(ID_MN_RESI == i,
                                                                                                                      TRA_INTERR == "1") %>%   
                                                                                                               count()
  )    
  
  SINAN_ANTRAB_2012_INTERRUPCAO_TRAT[which(SINAN_ANTRAB_2012_INTERRUPCAO_TRAT$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                                                                               filter(ID_MN_RESI == i,
                                                                                                                      TRA_INTERR == "2") %>%   
                                                                                                               count()
  )
}

SINAN_ANTRAB_2012_INTERRUPCAO_TRAT[17, 4:6] <- apply(SINAN_ANTRAB_2012_INTERRUPCAO_TRAT[, 4:6], 2, sum)
SINAN_ANTRAB_2012_INTERRUPCAO_TRAT[17, 1] <- "Total"

####Tabela Soroterapia (campo 53 do SINAN)###

SINAN_ANTRAB_2012_SOROTERAPIA <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == 22), 3])

SINAN_ANTRAB_2012_SOROTERAPIA$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == 22), 2]

SINAN_ANTRAB_2012_SOROTERAPIA$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == 22), 5]

SINAN_ANTRAB_2012_SOROTERAPIA$RS <- BASE_IBGE[which(BASE_IBGE$RS == 22), 1]

SINAN_ANTRAB_2012_SOROTERAPIA <- SINAN_ANTRAB_2012_SOROTERAPIA[,c(4, 1, 2, 3)]

SINAN_ANTRAB_2012_SOROTERAPIA$Sim <- NA

SINAN_ANTRAB_2012_SOROTERAPIA$Nao <- NA

SINAN_ANTRAB_2012_SOROTERAPIA$Ignorado <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == 22)), 2]){
  
  SINAN_ANTRAB_2012_SOROTERAPIA[which(SINAN_ANTRAB_2012_SOROTERAPIA$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                                                                     filter(ID_MN_RESI == i,
                                                                                                            TRA_INDI_N == "1") %>%   
                                                                                                     count()
  )    
  
  SINAN_ANTRAB_2012_SOROTERAPIA[which(SINAN_ANTRAB_2012_SOROTERAPIA$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                                                                     filter(ID_MN_RESI == i,
                                                                                                            TRA_INDI_N == "2") %>%   
                                                                                                     count()
  )
  
  SINAN_ANTRAB_2012_SOROTERAPIA[which(SINAN_ANTRAB_2012_SOROTERAPIA$COD_IBGE == i), 7] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                                                                     filter(ID_MN_RESI == i,
                                                                                                            TRA_INDI_N == "9") %>%   
                                                                                                     count()
  )
}

SINAN_ANTRAB_2012_SOROTERAPIA[17, 4:7] <- apply(SINAN_ANTRAB_2012_SOROTERAPIA[, 4:7], 2, sum)
SINAN_ANTRAB_2012_SOROTERAPIA[17, 1] <- "Total"

###Tabela tipo de imunobiologico (campo 55 SINAN)####

SINAN_ANTRAB_2012_TIPO_IMUNOBIOLOGICO <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == 22), 3])

SINAN_ANTRAB_2012_TIPO_IMUNOBIOLOGICO$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == 22), 2]

SINAN_ANTRAB_2012_TIPO_IMUNOBIOLOGICO$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == 22), 5]

SINAN_ANTRAB_2012_TIPO_IMUNOBIOLOGICO$RS <- BASE_IBGE[which(BASE_IBGE$RS == 22), 1]

SINAN_ANTRAB_2012_TIPO_IMUNOBIOLOGICO<- SINAN_ANTRAB_2012_TIPO_IMUNOBIOLOGICO[,c(4, 1, 2, 3)]

SINAN_ANTRAB_2012_TIPO_IMUNOBIOLOGICO$SAR <- NA

SINAN_ANTRAB_2012_TIPO_IMUNOBIOLOGICO$IGHAR <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == 22)), 2]){
  
  SINAN_ANTRAB_2012_TIPO_IMUNOBIOLOGICO[which(SINAN_ANTRAB_2012_TIPO_IMUNOBIOLOGICO$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                                                                                     filter(ID_MN_RESI == i,
                                                                                                                            TIP_SORO == "1") %>%   
                                                                                                                     count()
  )    
  
  SINAN_ANTRAB_2012_TIPO_IMUNOBIOLOGICO[which(SINAN_ANTRAB_2012_TIPO_IMUNOBIOLOGICO$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                                                                                     filter(ID_MN_RESI == i,
                                                                                                                            TIP_SORO== "2") %>%   
                                                                                                                     count()
  )
  
}

SINAN_ANTRAB_2012_TIPO_IMUNOBIOLOGICO[17, 4:6] <- apply(SINAN_ANTRAB_2012_TIPO_IMUNOBIOLOGICO[, 4:6], 2, sum)
SINAN_ANTRAB_2012_TIPO_IMUNOBIOLOGICO[17, 1] <- "Total"

###Tabela Infiltração (campo 56 do SINAN)###

SINAN_ANTRAB_2012_SORO_INFILTRACAO <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == 22), 3])

SINAN_ANTRAB_2012_SORO_INFILTRACAO$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == 22), 2]

SINAN_ANTRAB_2012_SORO_INFILTRACAO$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == 22), 5]

SINAN_ANTRAB_2012_SORO_INFILTRACAO$RS <- BASE_IBGE[which(BASE_IBGE$RS == 22), 1]

SINAN_ANTRAB_2012_SORO_INFILTRACAO  <- SINAN_ANTRAB_2012_SORO_INFILTRACAO [,c(4, 1, 2, 3)]

SINAN_ANTRAB_2012_SORO_INFILTRACAO$TOTAL <- NA

SINAN_ANTRAB_2012_SORO_INFILTRACAO$PARCIAL <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  SINAN_ANTRAB_2012_SORO_INFILTRACAO[which(SINAN_ANTRAB_2012_SORO_INFILTRACAO$COD_IBGE == i), 5] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                                                                               filter(ID_MN_RESI == i,
                                                                                                                      TRA_INFILT == "1") %>%   
                                                                                                               count()
  )    
  
  SINAN_ANTRAB_2012_SORO_INFILTRACAO[which(SINAN_ANTRAB_2012_SORO_INFILTRACAO$COD_IBGE == i), 6] <- as.integer(SINAN_ANTRAB_2012 %>% 
                                                                                                               filter(ID_MN_RESI == i,
                                                                                                                      TRA_INFI_1 == "1") %>%   
                                                                                                               count()
  )
  
}

SINAN_ANTRAB_2012_SORO_INFILTRACAO[17, 4:6] <- apply(SINAN_ANTRAB_2012_SORO_INFILTRACAO[, 4:6], 2, sum)
SINAN_ANTRAB_2012_SORO_INFILTRACAO[17, 1] <- "Total"


