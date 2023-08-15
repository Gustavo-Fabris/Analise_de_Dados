
#####    Contagem de casos por Semana epidemiológica visando o canal endêmico   #####

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
           paste0("Base_de_Dados/Tabulacoes_R/Raiva/RS22", RS, "_ANTRAB_2012_SE_Notificados.csv"), 
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
           paste0("Base_de_Dados/Tabulacoes_R/Raiva/RS22", RS, "_ANTRAB_2012_GERAL.csv"), 
           row.names = FALSE)




