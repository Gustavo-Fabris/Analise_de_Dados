
AUX <- matrix(data = NA, 
              nrow = nrow, 
              ncol = 54)

AUX <- as.data.frame(AUX)

colnames(AUX)[1] <- "Município" 

AUX[,1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

colnames (AUX)[2:54] <- c(1:53)

for (i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX == i), 2] <- as.integer(SINAN_CHIK_RS %>%
                                          filter(ID_MN_RESI == i,
                                                 SEM_PRI ==202501)%>%
                                          count()
                                        
  )
  
  AUX[which(AUX == i), 3] <- as.integer(SINAN_CHIK_RS %>% 
                                          filter(ID_MN_RESI == i, 
                                                 SEM_PRI ==202502) %>% 
                                          count()
  )
  
  AUX[which(AUX == i), 4] <- as.integer(SINAN_CHIK_RS %>% 
                                          filter(ID_MN_RESI == i,
                                                 SEM_PRI ==202503) %>% 
                                          count()
  )
  
  AUX[which(AUX == i),5] <- as.integer(SINAN_CHIK_RS %>% 
                                         filter(ID_MN_RESI == i,
                                                SEM_PRI ==202504) %>% 
                                         count()
  )
  
  AUX[which(AUX == i), 6] <- as.integer(SINAN_CHIK_RS %>% 
                                          filter(ID_MN_RESI == i,
                                                 SEM_PRI ==202505) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 7] <- as.integer(SINAN_CHIK_RS %>%
                                          filter(ID_MN_RESI == i, 
                                                 SEM_PRI ==202506) %>%
                                          count()
  )
  
  AUX[which(AUX == i), 8] <- as.integer(SINAN_CHIK_RS %>%
                                          filter(ID_MN_RESI == i, 
                                                 SEM_PRI ==202507) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 9] <- as.integer(SINAN_CHIK_RS %>%
                                          filter(ID_MN_RESI == i, 
                                                 SEM_PRI ==202508) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 10] <- as.integer(SINAN_CHIK_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202509) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 11] <- as.integer(SINAN_CHIK_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202510) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 12] <- as.integer(SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202511) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 13] <- as.integer(SINAN_CHIK_RS %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202512) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 14] <- as.integer(SINAN_CHIK_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202513) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 15] <- as.integer(SINAN_CHIK_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202514) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 16] <- as.integer(SINAN_CHIK_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202515) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 17] <- as.integer(SINAN_CHIK_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202516) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 18] <- as.integer(SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202517) %>%  
                                           count() 
  )
  
  AUX[which(AUX == i), 19] <- as.integer(SINAN_CHIK_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202518) %>%      
                                           count() 
  )
  
  AUX[which(AUX == i), 20] <- as.integer(SINAN_CHIK_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202519) %>%
                                           count() 
  )
  
  AUX[which(AUX == i),  21] <- as.integer(SINAN_CHIK_RS %>% 
                                            filter(ID_MN_RESI == i, 
                                                   SEM_PRI ==202520) %>%
                                            count() 
  )
  
  AUX[which(AUX == i), 22] <- as.integer(SINAN_CHIK_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202521) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 23] <- as.integer(SINAN_CHIK_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202522) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 24] <- as.integer(SINAN_CHIK_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202523) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 25] <- as.integer(SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202524) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 26] <- as.integer(SINAN_CHIK_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202525) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 27] <- as.integer(SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202526) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 28] <- as.integer(SINAN_CHIK_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202527) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 29] <- as.integer(SINAN_CHIK_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202528) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 30] <- as.integer(SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202529) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 31] <- as.integer(SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202530) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 32] <- as.integer(SINAN_CHIK_RS %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202531) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 33] <- as.integer(SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202532) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 34] <- as.integer(SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202533) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 35] <- as.integer(SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202534) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 36] <- as.integer(SINAN_CHIK_RS %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202535) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 37] <- as.integer(SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202536) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 38] <- as.integer(SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202537) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 39] <- as.integer(SINAN_CHIK_RS %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202538) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 40] <- as.integer(SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202539) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 41] <- as.integer(SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202540) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 42] <- as.integer(SINAN_CHIK_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202541) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 43] <- as.integer(SINAN_CHIK_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202542) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 44] <- as.integer(SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202543) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 45] <- as.integer(SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202544) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 46] <- as.integer(SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202545) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 47] <- as.integer(SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202546) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 48] <- as.integer(SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202547) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 49] <- as.integer(SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202548) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 50] <- as.integer(SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202549) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 51] <- as.integer(SINAN_CHIK_RS %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202550) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 52] <- as.integer(SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202551) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 53] <- as.integer(SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202552) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 54] <- as.integer(SINAN_CHIK_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202553) %>%
                                           count() 
  )
}

AUX[,1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX[(nrow(AUX)+ 1),2:54] <- apply(AUX[,2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_2025_SE_Notificados_CHIK"), AUX)

assign("RS_2025_SE_Notificados_CHIK", AUX)

########################################################################################################
###     Construindo um for loop para realizar a tabela de Confirmados por semana epidemiológica      ###
########################################################################################################

AUX <- matrix(data = NA, 
              nrow = nrow, 
              ncol = 54)

AUX <- as.data.frame(AUX)

colnames(AUX)[1] <- "Município" 

AUX[,1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

colnames (AUX)[2:54] <- c(1:53)

for (i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX == i), 2] <- as.integer(SINAN_CHIK_RS %>%
                                          filter(ID_MN_RESI == i,
                                                 CLASSI_FIN == 13,
                                                 SEM_PRI ==202501)%>%
                                          count()
  )
  
  AUX[which(AUX == i), 3] <- as.integer(SINAN_CHIK_RS %>% 
                                          filter(ID_MN_RESI == i, 
                                                 CLASSI_FIN == 13,
                                                 SEM_PRI ==202502) %>% 
                                          count()
  )
  
  AUX[which(AUX == i), 4] <- as.integer(SINAN_CHIK_RS %>% 
                                          filter(ID_MN_RESI == i,
                                                 CLASSI_FIN == 13,
                                                 SEM_PRI ==202503) %>% 
                                          count()
  )
  
  AUX[which(AUX == i),5] <- as.integer(SINAN_CHIK_RS %>% 
                                         filter(ID_MN_RESI == i,
                                                CLASSI_FIN == 13,
                                                SEM_PRI ==202504) %>% 
                                         count()
  )
  
  AUX[which(AUX == i), 6] <- as.integer(SINAN_CHIK_RS %>% 
                                          filter(ID_MN_RESI == i,
                                                 CLASSI_FIN == 13,
                                                 SEM_PRI ==202505) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 7] <- as.integer(SINAN_CHIK_RS %>%
                                          filter(ID_MN_RESI == i, 
                                                 CLASSI_FIN == 13,
                                                 SEM_PRI ==202506) %>%
                                          count()
  )
  
  AUX[which(AUX == i), 8] <- as.integer(SINAN_CHIK_RS %>%
                                          filter(ID_MN_RESI == i, 
                                                 CLASSI_FIN == 13,
                                                 SEM_PRI ==202507) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 9] <- as.integer(SINAN_CHIK_RS %>%
                                          filter(ID_MN_RESI == i, 
                                                 CLASSI_FIN == 13,
                                                 SEM_PRI ==202508) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 10] <- as.integer(SINAN_CHIK_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  CLASSI_FIN == 13,
                                                  SEM_PRI ==202509) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 11] <- as.integer(SINAN_CHIK_RS %>% 
                                           filter(ID_MN_RESI == i,
                                                  CLASSI_FIN == 13,
                                                  SEM_PRI ==202510) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 12] <- as.integer(SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  CLASSI_FIN == 13,
                                                  SEM_PRI ==202511) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 13] <- as.integer(SINAN_CHIK_RS %>% 
                                           filter(ID_MN_RESI == i,
                                                  CLASSI_FIN == 13,
                                                  SEM_PRI ==202512) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 14] <- as.integer(SINAN_CHIK_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  CLASSI_FIN == 13,
                                                  SEM_PRI ==202513) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 15] <- as.integer(SINAN_CHIK_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  CLASSI_FIN == 13,
                                                  SEM_PRI ==202514) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 16] <- as.integer(SINAN_CHIK_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  CLASSI_FIN == 13,
                                                  SEM_PRI ==202515) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 17] <- as.integer(SINAN_CHIK_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  CLASSI_FIN == 13,
                                                  SEM_PRI ==202516) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 18] <- as.integer(SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i, 
                                                  CLASSI_FIN == 13,
                                                  SEM_PRI ==202517) %>%    
                                           count() 
  )
  
  AUX[which(AUX == i), 19] <- as.integer(SINAN_CHIK_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  CLASSI_FIN == 13,
                                                  SEM_PRI ==202518) %>%  
                                           count() 
  )
  
  AUX[which(AUX == i), 20] <- as.integer(SINAN_CHIK_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  CLASSI_FIN == 13,
                                                  SEM_PRI ==202519) %>%
                                           count() 
  )
  
  AUX[which(AUX == i),  21] <- as.integer(SINAN_CHIK_RS %>% 
                                            filter(ID_MN_RESI == i,
                                                   CLASSI_FIN == 13,
                                                   SEM_PRI ==202520) %>%
                                            count() 
  )
  
  AUX[which(AUX == i), 22] <- as.integer(SINAN_CHIK_RS %>% 
                                           filter(ID_MN_RESI == i,
                                                  CLASSI_FIN == 13,
                                                  SEM_PRI ==202521) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 23] <- as.integer(SINAN_CHIK_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  CLASSI_FIN == 13,
                                                  SEM_PRI ==202522) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 24] <- as.integer(SINAN_CHIK_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  CLASSI_FIN == 13,
                                                  SEM_PRI ==202523) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 25] <- as.integer(SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i, 
                                                  CLASSI_FIN == 13,
                                                  SEM_PRI ==202524) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 26] <- as.integer(SINAN_CHIK_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  CLASSI_FIN == 13,
                                                  SEM_PRI ==202525) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 27] <- as.integer(SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i, 
                                                  CLASSI_FIN == 13,
                                                  SEM_PRI ==202526) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 28] <- as.integer(SINAN_CHIK_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  CLASSI_FIN == 13,
                                                  SEM_PRI ==202527) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 29] <- as.integer(SINAN_CHIK_RS %>% 
                                           filter(ID_MN_RESI == i,
                                                  CLASSI_FIN == 13,
                                                  SEM_PRI ==202528) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 30] <- as.integer(SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  CLASSI_FIN == 13,
                                                  SEM_PRI ==202529) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 31] <- as.integer(SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  CLASSI_FIN == 13,
                                                  SEM_PRI ==202530) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 32] <- as.integer(SINAN_CHIK_RS %>% 
                                           filter(ID_MN_RESI == i,
                                                  CLASSI_FIN == 13,
                                                  SEM_PRI ==202531) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 33] <- as.integer(SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  CLASSI_FIN == 13,
                                                  SEM_PRI ==202532) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 34] <- as.integer(SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  CLASSI_FIN == 13,
                                                  SEM_PRI ==202533) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 35] <- as.integer(SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  CLASSI_FIN == 13,
                                                  SEM_PRI ==202534) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 36] <- as.integer(SINAN_CHIK_RS %>% 
                                           filter(ID_MN_RESI == i,
                                                  CLASSI_FIN == 13,
                                                  SEM_PRI ==202535) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 37] <- as.integer(SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  CLASSI_FIN == 13,
                                                  SEM_PRI ==202536) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 38] <- as.integer(SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  CLASSI_FIN == 13,
                                                  SEM_PRI ==202537) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 39] <- as.integer(SINAN_CHIK_RS %>% 
                                           filter(ID_MN_RESI == i,
                                                  CLASSI_FIN == 13,
                                                  SEM_PRI ==202538) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 40] <- as.integer(SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  CLASSI_FIN == 13,
                                                  SEM_PRI ==202539) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 41] <- as.integer(SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  CLASSI_FIN == 13,
                                                  SEM_PRI ==202540) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 42] <- as.integer(SINAN_CHIK_RS %>% 
                                           filter(ID_MN_RESI == i,
                                                  CLASSI_FIN == 13,
                                                  SEM_PRI ==202541) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 43] <- as.integer(SINAN_CHIK_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  CLASSI_FIN == 13,
                                                  SEM_PRI ==202542) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 44] <- as.integer(SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  CLASSI_FIN == 13,
                                                  SEM_PRI ==202543) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 45] <- as.integer(SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  CLASSI_FIN == 13,
                                                  SEM_PRI ==202544) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 46] <- as.integer(SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  CLASSI_FIN == 13,
                                                  SEM_PRI ==202545) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 47] <- as.integer(SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  CLASSI_FIN == 13,
                                                  SEM_PRI ==202546) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 48] <- as.integer(SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  CLASSI_FIN == 13,
                                                  SEM_PRI ==202547) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 49] <- as.integer(SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  CLASSI_FIN == 13,
                                                  SEM_PRI ==202548) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 50] <- as.integer(SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i, 
                                                  CLASSI_FIN == 13,
                                                  SEM_PRI ==202549) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 51] <- as.integer(SINAN_CHIK_RS %>% 
                                           filter(ID_MN_RESI == i,
                                                  CLASSI_FIN == 13,
                                                  SEM_PRI ==202550) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 52] <- as.integer(SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  CLASSI_FIN == 13,
                                                  SEM_PRI ==202551) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 53] <- as.integer(SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  CLASSI_FIN == 13,
                                                  SEM_PRI ==202552) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 54] <- as.integer(SINAN_CHIK_RS %>% 
                                           filter(ID_MN_RESI == i,
                                                  CLASSI_FIN == 13,
                                                  SEM_PRI ==202553) %>%
                                           count() 
  )
}

AUX[,1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX[(nrow(AUX)+ 1),2:54] <- apply(AUX[,2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_2025_SE_Confirmados_CHIK"), AUX)

assign("RS_2025_SE_Confirmados_CHIK", AUX)

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

colnames (AUX)[2:54] <- c(1:53)

for (i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX == i), 2] <- as.integer(SINAN_CHIK_RS %>%
                                          filter(ID_MN_RESI == i,
                                                 SEM_PRI ==202501)%>%
                                          count()
                                        -
                                          SINAN_CHIK_RS %>%
                                          filter(ID_MN_RESI == i,
                                                 SEM_PRI == 202501,
                                                 CLASSI_FIN == 5) %>%
                                          count()
  )                                                                                       
  
  AUX[which(AUX == i), 3] <- as.integer(SINAN_CHIK_RS %>%
                                          filter(ID_MN_RESI == i,
                                                 SEM_PRI ==202502)%>%
                                          count()
                                        -
                                          SINAN_CHIK_RS %>%
                                          filter(ID_MN_RESI == i,
                                                 SEM_PRI == 202502,
                                                 CLASSI_FIN == 5) %>%
                                          count()
  )  
  
  AUX[which(AUX == i), 4] <- as.integer(SINAN_CHIK_RS %>% 
                                          filter(ID_MN_RESI == i,
                                                 SEM_PRI ==202503) %>% 
                                          count()
                                        -
                                          SINAN_CHIK_RS %>%
                                          filter(ID_MN_RESI == i,
                                                 SEM_PRI == 202503,
                                                 CLASSI_FIN == 5) %>%
                                          count()
  )   
  
  AUX[which(AUX == i),5] <- as.integer(SINAN_CHIK_RS %>% 
                                         filter(ID_MN_RESI == i,
                                                SEM_PRI ==202504) %>% 
                                         count()
                                       -
                                         SINAN_CHIK_RS %>%
                                         filter(ID_MN_RESI == i,
                                                SEM_PRI == 202504,
                                                CLASSI_FIN == 5) %>%
                                         count()
  )   
  
  AUX[which(AUX == i), 6] <- as.integer(SINAN_CHIK_RS %>% 
                                          filter(ID_MN_RESI == i,
                                                 SEM_PRI ==202505) %>% 
                                          count()
                                        -
                                          SINAN_CHIK_RS %>%
                                          filter(ID_MN_RESI == i,
                                                 SEM_PRI == 202505,
                                                 CLASSI_FIN == 5) %>%
                                          count()
  )   
  
  AUX[which(AUX == i), 7] <- as.integer(SINAN_CHIK_RS %>%
                                          filter(ID_MN_RESI == i, 
                                                 SEM_PRI ==202506) %>%
                                          count()
                                        -
                                          SINAN_CHIK_RS %>%
                                          filter(ID_MN_RESI == i,
                                                 SEM_PRI == 202506,
                                                 CLASSI_FIN == 5) %>%
                                          count()
  ) 
  
  AUX[which(AUX == i), 8] <- as.integer(SINAN_CHIK_RS %>%
                                          filter(ID_MN_RESI == i, 
                                                 SEM_PRI ==202507) %>% 
                                          count() 
                                        -
                                          SINAN_CHIK_RS %>%
                                          filter(ID_MN_RESI == i,
                                                 SEM_PRI == 202507,
                                                 CLASSI_FIN == 5) %>%
                                          count()
  )   
  
  AUX[which(AUX == i), 9] <- as.integer(SINAN_CHIK_RS %>%
                                          filter(ID_MN_RESI == i, 
                                                 SEM_PRI ==202508) %>% 
                                          count() 
                                        -
                                          SINAN_CHIK_RS %>%
                                          filter(ID_MN_RESI == i,
                                                 SEM_PRI == 202508,
                                                 CLASSI_FIN == 5) %>%
                                          count()
  )   
  
  AUX[which(AUX == i), 10] <- as.integer(SINAN_CHIK_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202509) %>%
                                           count()
                                         -
                                           SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202509,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  )   
  
  AUX[which(AUX == i), 11] <- as.integer(SINAN_CHIK_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202510) %>%
                                           count()
                                         -
                                           SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202510,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  )   
  
  AUX[which(AUX == i), 12] <- as.integer(SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202511) %>%
                                           count()
                                         -
                                           SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202511,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  )   
  
  AUX[which(AUX == i), 13] <- as.integer(SINAN_CHIK_RS %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202512) %>%
                                           count() 
                                         -
                                           SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202512,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  )  
  
  AUX[which(AUX == i), 14] <- as.integer(SINAN_CHIK_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202513) %>% 
                                           count() 
                                         -
                                           SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202513,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  )   
  
  
  AUX[which(AUX == i), 15] <- as.integer(SINAN_CHIK_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202514) %>%
                                           count() 
                                         -
                                           SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202514,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  )   
  
  AUX[which(AUX == i), 16] <- as.integer(SINAN_CHIK_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202515) %>%
                                           count() 
                                         -
                                           SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202515,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  )  
  
  AUX[which(AUX == i), 17] <- as.integer(SINAN_CHIK_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202516) %>%
                                           count()
                                         -
                                           SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202516,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  )   
  
  AUX[which(AUX == i), 18] <- as.integer(SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202517) %>%  
                                           count() 
                                         -
                                           SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202517,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  ) 
  
  AUX[which(AUX == i), 19] <- as.integer(SINAN_CHIK_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202518) %>%   
                                           count() 
                                         -
                                           SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202518,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  )   
  
  
  AUX[which(AUX == i), 20] <- as.integer(SINAN_CHIK_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202519) %>%
                                           count() 
                                         -
                                           SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202519,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  )   
  
  AUX[which(AUX == i),  21] <- as.integer(SINAN_CHIK_RS %>% 
                                            filter(ID_MN_RESI == i, 
                                                   SEM_PRI ==202520) %>%
                                            count() 
                                          -
                                            SINAN_CHIK_RS %>%
                                            filter(ID_MN_RESI == i,
                                                   SEM_PRI == 202520,
                                                   CLASSI_FIN == 5) %>%
                                            count()
  )   
  
  AUX[which(AUX == i), 22] <- as.integer(SINAN_CHIK_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202521) %>%
                                           count() 
                                         -
                                           SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202521,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  )   
  
  AUX[which(AUX == i), 23] <- as.integer(SINAN_CHIK_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202522) %>%
                                           count() 
                                         -
                                           SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202522,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  )   
  
  AUX[which(AUX == i), 24] <- as.integer(SINAN_CHIK_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202523) %>%
                                           count()
                                         -
                                           SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202523,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  )   
  
  AUX[which(AUX == i), 25] <- as.integer(SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202524) %>%
                                           count() 
                                         -
                                           SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202524,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  )   
  
  AUX[which(AUX == i), 26] <- as.integer(SINAN_CHIK_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202525) %>%
                                           count()
                                         -
                                           SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202525,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  )   
  
  AUX[which(AUX == i), 27] <- as.integer(SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202526) %>%
                                           count() 
                                         -
                                           SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202526,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  )   
  
  AUX[which(AUX == i), 28] <- as.integer(SINAN_CHIK_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202527) %>%
                                           count() 
                                         -
                                           SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202527,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  )   
  
  AUX[which(AUX == i), 29] <- as.integer(SINAN_CHIK_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202528) %>%
                                           count() 
                                         -
                                           SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202528,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  )   
  
  AUX[which(AUX == i), 30] <- as.integer(SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202529) %>%
                                           count() 
                                         -
                                           SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202529,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  )   
  
  AUX[which(AUX == i), 31] <- as.integer(SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202530) %>% 
                                           count()
                                         -
                                           SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202530,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  ) 
  
  AUX[which(AUX == i), 32] <- as.integer(SINAN_CHIK_RS %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202531) %>%
                                           count()
                                         -
                                           SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202531,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  ) 
  
  AUX[which(AUX == i), 33] <- as.integer(SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202532) %>%
                                           count() 
                                         -
                                           SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202532,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  ) 
  
  AUX[which(AUX == i), 34] <- as.integer(SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202533) %>% 
                                           count() 
                                         -
                                           SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202533,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  ) 
  
  AUX[which(AUX == i), 35] <- as.integer(SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202534) %>% 
                                           count() 
                                         -
                                           SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202534,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  ) 
  
  AUX[which(AUX == i), 36] <- as.integer(SINAN_CHIK_RS %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202535) %>%
                                           count() 
                                         -
                                           SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202535,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  ) 
  
  AUX[which(AUX == i), 37] <- as.integer(SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202536) %>%
                                           count() 
                                         -
                                           SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202536,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 38] <- as.integer(SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202537) %>% 
                                           count() 
                                         -
                                           SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202537,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 39] <- as.integer(SINAN_CHIK_RS %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202538) %>%
                                           count()
                                         -
                                           SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202538,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 40] <- as.integer(SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202539) %>%
                                           count() 
                                         -
                                           SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202539,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 41] <- as.integer(SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202540) %>%
                                           count() 
                                         -
                                           SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202540,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 42] <- as.integer(SINAN_CHIK_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202541) %>%
                                           count() 
                                         -
                                           SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202541,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  ) 
  
  AUX[which(AUX == i), 43] <- as.integer(SINAN_CHIK_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202542) %>%
                                           count()
                                         -
                                           SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202542,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 44] <- as.integer(SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202543) %>%
                                           count()
                                         -
                                           SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202543,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 45] <- as.integer(SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202544) %>%
                                           count() 
                                         -
                                           SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202544,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 46] <- as.integer(SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202545) %>%
                                           count()
                                         -
                                           SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202545,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 47] <- as.integer(SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202546) %>%
                                           count() 
                                         -
                                           SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202546,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 48] <- as.integer(SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202547) %>%
                                           count() 
                                         -
                                           SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202547,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 49] <- as.integer(SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202548) %>%
                                           count() 
                                         -
                                           SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202548,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 50] <- as.integer(SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202549) %>% 
                                           count()
                                         -
                                           SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202549,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  ) 
  
  AUX[which(AUX == i), 51] <- as.integer(SINAN_CHIK_RS %>% 
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202550) %>% 
                                           count() 
                                         -
                                           SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202550,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 52] <- as.integer(SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202551) %>%
                                           count() 
                                         -
                                           SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202551,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 53] <- as.integer(SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI ==202552) %>%
                                           count() 
                                         -
                                           SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202552,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  ) 
  
  AUX[which(AUX == i), 54] <- as.integer(SINAN_CHIK_RS %>% 
                                           filter(ID_MN_RESI == i, 
                                                  SEM_PRI ==202553) %>%
                                           count() 
                                         -
                                           SINAN_CHIK_RS %>%
                                           filter(ID_MN_RESI == i,
                                                  SEM_PRI == 202553,
                                                  CLASSI_FIN == 5) %>%
                                           count()
  ) 
}

AUX[, 1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX[(nrow(AUX)+ 1), 2:54] <- apply(AUX[, 2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

####Casos prováveis por semana epidemiológica. Este objeto será apagado assim que for incluso no AUX_GRAF####

assign(paste0("RS", "_2025_SE_Provaveis_CHIK"), AUX)

assign(paste0("RS", RS, "_2025_SE_Provaveis_CHIK"), AUX)

write.csv (assign(paste0("RS", RS, "_2025_SE_Provaveis_CHIK"), AUX), 
           paste0("Tabulacoes_R/Arboviroses/RS", RS, "_2025_SE_Provaveis_CHIK.csv"), 
           row.names = FALSE)


#######################################################
######     Histogramas Municipais   ###################
#######################################################

###       NOTIFICADOS     ########
RS_2025_SE_Notificados_CHIK <- RS_2025_SE_Notificados_CHIK[, -54]

RS_2025_SE_Notificados_CHIK[nrow(RS_2025_SE_Notificados_CHIK) +1, 2:ncol(RS_2025_SE_Notificados_CHIK)] <- c("1",  "2", "3", 
                                                                                             "4",  "5",  "6",  
                                                                                             "7",  "8",  "9",  
                                                                                             "10",  "11",  "12",  
                                                                                             "13",  "14",  "15",  
                                                                                             "16",  "17",  "18",  
                                                                                             "19",  "20",  "21",  
                                                                                             "22",  "23",  
                                                                                             "24",  "25",  "26",  
                                                                                             "27",  "28",  "29",  
                                                                                             "30",  "31",  "32", 
                                                                                             "33",  "34",  "35",  
                                                                                             "36",  "37",  "38",  
                                                                                             "39",  "40",  "41",  
                                                                                             "42",  "43",  "244",  
                                                                                             "45",  "46",  "47",  
                                                                                             "48",  "49",  "50",  
                                                                                             "51",  "52")

AUX_GRAF <- as.data.frame(RS_2025_SE_Notificados_CHIK$Município)

AUX_GRAF[, 2] <- as.data.frame(RS_2025_SE_Notificados_CHIK[, which(colnames(RS_2025_SE_Notificados_CHIK) == SE) - 9])
AUX_GRAF[, 3] <- as.data.frame(RS_2025_SE_Notificados_CHIK[, which(colnames(RS_2025_SE_Notificados_CHIK) == SE) - 8])
AUX_GRAF[, 4] <- as.data.frame(RS_2025_SE_Notificados_CHIK[, which(colnames(RS_2025_SE_Notificados_CHIK) == SE) - 7])
AUX_GRAF[, 5] <- as.data.frame(RS_2025_SE_Notificados_CHIK[, which(colnames(RS_2025_SE_Notificados_CHIK) == SE) - 6])
AUX_GRAF[, 6] <- as.data.frame(RS_2025_SE_Notificados_CHIK[, which(colnames(RS_2025_SE_Notificados_CHIK) == SE) - 5])
AUX_GRAF[, 7] <- as.data.frame(RS_2025_SE_Notificados_CHIK[, which(colnames(RS_2025_SE_Notificados_CHIK) == SE) - 4])
AUX_GRAF[, 8] <- as.data.frame(RS_2025_SE_Notificados_CHIK[, which(colnames(RS_2025_SE_Notificados_CHIK) == SE) - 3])
AUX_GRAF[, 9] <- as.data.frame(RS_2025_SE_Notificados_CHIK[, which(colnames(RS_2025_SE_Notificados_CHIK) == SE) - 2])
AUX_GRAF[, 10] <- as.data.frame(RS_2025_SE_Notificados_CHIK[, which(colnames(RS_2025_SE_Notificados_CHIK) == SE) - 1])
AUX_GRAF[, 11] <- as.data.frame(RS_2025_SE_Notificados_CHIK[, which(colnames(RS_2025_SE_Notificados_CHIK) == SE)])

colnames(AUX_GRAF)[1] <- "Municipios"
colnames(AUX_GRAF)[2] <- colnames(RS_2025_SE_Notificados_CHIK)[which(colnames(RS_2025_SE_Notificados_CHIK) == SE) - 9]
colnames(AUX_GRAF)[3] <- colnames(RS_2025_SE_Notificados_CHIK)[which(colnames(RS_2025_SE_Notificados_CHIK) == SE) - 8]
colnames(AUX_GRAF)[4] <- colnames(RS_2025_SE_Notificados_CHIK)[which(colnames(RS_2025_SE_Notificados_CHIK) == SE) - 7]
colnames(AUX_GRAF)[5] <- colnames(RS_2025_SE_Notificados_CHIK)[which(colnames(RS_2025_SE_Notificados_CHIK) == SE) - 6]
colnames(AUX_GRAF)[6] <- colnames(RS_2025_SE_Notificados_CHIK)[which(colnames(RS_2025_SE_Notificados_CHIK) == SE) - 5]
colnames(AUX_GRAF)[7] <- colnames(RS_2025_SE_Notificados_CHIK)[which(colnames(RS_2025_SE_Notificados_CHIK) == SE) - 4]
colnames(AUX_GRAF)[8] <- colnames(RS_2025_SE_Notificados_CHIK)[which(colnames(RS_2025_SE_Notificados_CHIK) == SE) - 3]
colnames(AUX_GRAF)[9] <- colnames(RS_2025_SE_Notificados_CHIK)[which(colnames(RS_2025_SE_Notificados_CHIK) == SE) - 2]
colnames(AUX_GRAF)[10] <- colnames(RS_2025_SE_Notificados_CHIK)[which(colnames(RS_2025_SE_Notificados_CHIK) == SE) - 1]
colnames(AUX_GRAF)[11] <- colnames(RS_2025_SE_Notificados_CHIK)[which(colnames(RS_2025_SE_Notificados_CHIK) == SE)]

AUX_GRAF[nrow(AUX_GRAF), 1] <- "Municipios"

AUX_GRAF <- AUX_GRAF[-(nrow(AUX_GRAF)-1),]

AUX_GRAF <- AUX_GRAF[c(nrow(AUX_GRAF), 1:(nrow(AUX_GRAF) -1)), ]

AUX_GRAF <- t(AUX_GRAF)

colnames(AUX_GRAF) <- AUX_GRAF[1,]

AUX_GRAF <- AUX_GRAF[-1,]

colnames(AUX_GRAF)[1] <- "SE"

AUX_GRAF <- as.data.frame(AUX_GRAF)

AUX_GRAF[, 2: ncol(AUX_GRAF)] <- apply(AUX_GRAF[, 2: ncol(AUX_GRAF)], 2, as.numeric)

rownames(AUX_GRAF) <- 1: nrow(AUX_GRAF)

colnames(AUX_GRAF) <- str_replace(colnames(AUX_GRAF), " ", "_")
colnames(AUX_GRAF) <- str_replace(colnames(AUX_GRAF), " ", "_")
colnames(AUX_GRAF) <- str_replace(colnames(AUX_GRAF), " ", "_")
colnames(AUX_GRAF) <- str_replace(colnames(AUX_GRAF), " ", "_")

###############  Criando uma função para o tema do gráfico   ##################

Theme_Hist <- function(){ 
  theme_minimal(base_size = 10) %+replace%  
    theme(
      axis.text.x = element_text(face = "bold"),
      panel.grid.major = element_line(color = "#C0C0C0"),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "#F5F5F5"),
      plot.title = element_text(face = "bold", 
                                size = 15, 
                                colour = "#556B2F")
    )
}

AUX_GRAF$SE <- factor(as.numeric(AUX_GRAF$SE), 
                      levels = sort(unique(as.numeric(AUX_GRAF$SE))))

######   Criando o conjunto de gráficos dos histogramas via Lapply.  ######

AUX_HIST_CHIK_NOT_LIST <- AUX_GRAF %>%
  pivot_longer(-SE, names_to = "Municipios") %>% 
  mutate(
    SE = SE,
    Municipios = gsub("_", " ", Municipios)
  ) %>%
  group_split(Municipios) %>% 
  lapply(\(dados) {
    titulo <- dados$Municipios %>% 
      unique() %>% 
      paste0(" - Notificados")
    ggplot(dados, aes(x = SE, 
                      y = value)
    ) + 
      geom_col(color = "black", 
               fill = "#8FBC8F") + 
      geom_label(aes(label = value), 
                 alpha = 0.5, 
                 vjust = 0.1) +
      labs(
        caption = Fonte, 
        x = "Semana Epidemiológica",
        y = "Número de Casos",
        title = titulo
      ) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
      Theme_Hist()
  })

RS_2025_GRAF_CHIK_Histograma_Notificados_01 <- (AUX_HIST_CHIK_NOT_LIST[[1]] + AUX_HIST_CHIK_NOT_LIST[[2]]) / 
  (AUX_HIST_CHIK_NOT_LIST[[3]] + AUX_HIST_CHIK_NOT_LIST[[4]]) / 
  (AUX_HIST_CHIK_NOT_LIST[[5]] + AUX_HIST_CHIK_NOT_LIST[[6]]) / 
  (AUX_HIST_CHIK_NOT_LIST[[7]] + AUX_HIST_CHIK_NOT_LIST[[8]]) 

RS_2025_GRAF_CHIK_Histograma_Notificados_02 <- (AUX_HIST_CHIK_NOT_LIST[[9]] + AUX_HIST_CHIK_NOT_LIST[[10]]) / 
  (AUX_HIST_CHIK_NOT_LIST[[11]] + AUX_HIST_CHIK_NOT_LIST[[12]]) / 
  (AUX_HIST_CHIK_NOT_LIST[[13]] + AUX_HIST_CHIK_NOT_LIST[[14]]) / 
  (AUX_HIST_CHIK_NOT_LIST[[15]] + AUX_HIST_CHIK_NOT_LIST[[16]]) 

###     Confirmados    #####

RS_2025_SE_Confirmados_CHIK <- RS_2025_SE_Confirmados_CHIK[, -54]

RS_2025_SE_Confirmados_CHIK[nrow(RS_2025_SE_Confirmados_CHIK) +1, 2:ncol(RS_2025_SE_Confirmados_CHIK)] <- c("1",  "2", "3", 
                                                                                             "4",  "5",  "6",  
                                                                                             "7",  "8",  "9",  
                                                                                             "10",  "11",  "12",  
                                                                                             "13",  "14",  "15",  
                                                                                             "16",  "17",  "18",  
                                                                                             "19",  "20",  "21",  
                                                                                             "22",  "23",  
                                                                                             "24",  "25",  "26",  
                                                                                             "27",  "28",  "29",  
                                                                                             "30",  "31",  "32", 
                                                                                             "33",  "34",  "35",  
                                                                                             "36",  "37",  "38",  
                                                                                             "39",  "40",  "41",  
                                                                                             "42",  "43",  "244",  
                                                                                             "45",  "46",  "47",  
                                                                                             "48",  "49",  "50",  
                                                                                             "51",  "52")

AUX_GRAF <- as.data.frame(RS_2025_SE_Confirmados_CHIK$Município)

AUX_GRAF[, 2] <- as.data.frame(RS_2025_SE_Confirmados_CHIK[, which(colnames(RS_2025_SE_Confirmados_CHIK) == SE) - 9])
AUX_GRAF[, 3] <- as.data.frame(RS_2025_SE_Confirmados_CHIK[, which(colnames(RS_2025_SE_Confirmados_CHIK) == SE) - 8])
AUX_GRAF[, 4] <- as.data.frame(RS_2025_SE_Confirmados_CHIK[, which(colnames(RS_2025_SE_Confirmados_CHIK) == SE) - 7])
AUX_GRAF[, 5] <- as.data.frame(RS_2025_SE_Confirmados_CHIK[, which(colnames(RS_2025_SE_Confirmados_CHIK) == SE) - 6])
AUX_GRAF[, 6] <- as.data.frame(RS_2025_SE_Confirmados_CHIK[, which(colnames(RS_2025_SE_Confirmados_CHIK) == SE) - 5])
AUX_GRAF[, 7] <- as.data.frame(RS_2025_SE_Confirmados_CHIK[, which(colnames(RS_2025_SE_Confirmados_CHIK) == SE) - 4])
AUX_GRAF[, 8] <- as.data.frame(RS_2025_SE_Confirmados_CHIK[, which(colnames(RS_2025_SE_Confirmados_CHIK) == SE) - 3])
AUX_GRAF[, 9] <- as.data.frame(RS_2025_SE_Confirmados_CHIK[, which(colnames(RS_2025_SE_Confirmados_CHIK) == SE) - 2])
AUX_GRAF[, 10] <- as.data.frame(RS_2025_SE_Confirmados_CHIK[, which(colnames(RS_2025_SE_Confirmados_CHIK) == SE) - 1])
AUX_GRAF[, 11] <- as.data.frame(RS_2025_SE_Confirmados_CHIK[, which(colnames(RS_2025_SE_Confirmados_CHIK) == SE)])

colnames(AUX_GRAF)[1] <- "Municipios"
colnames(AUX_GRAF)[2] <- colnames(RS_2025_SE_Confirmados_CHIK)[which(colnames(RS_2025_SE_Confirmados_CHIK) == SE) - 9]
colnames(AUX_GRAF)[3] <- colnames(RS_2025_SE_Confirmados_CHIK)[which(colnames(RS_2025_SE_Confirmados_CHIK) == SE) - 8]
colnames(AUX_GRAF)[4] <- colnames(RS_2025_SE_Confirmados_CHIK)[which(colnames(RS_2025_SE_Confirmados_CHIK) == SE) - 7]
colnames(AUX_GRAF)[5] <- colnames(RS_2025_SE_Confirmados_CHIK)[which(colnames(RS_2025_SE_Confirmados_CHIK) == SE) - 6]
colnames(AUX_GRAF)[6] <- colnames(RS_2025_SE_Confirmados_CHIK)[which(colnames(RS_2025_SE_Confirmados_CHIK) == SE) - 5]
colnames(AUX_GRAF)[7] <- colnames(RS_2025_SE_Confirmados_CHIK)[which(colnames(RS_2025_SE_Confirmados_CHIK) == SE) - 4]
colnames(AUX_GRAF)[8] <- colnames(RS_2025_SE_Confirmados_CHIK)[which(colnames(RS_2025_SE_Confirmados_CHIK) == SE) - 3]
colnames(AUX_GRAF)[9] <- colnames(RS_2025_SE_Confirmados_CHIK)[which(colnames(RS_2025_SE_Confirmados_CHIK) == SE) - 2]
colnames(AUX_GRAF)[10] <- colnames(RS_2025_SE_Confirmados_CHIK)[which(colnames(RS_2025_SE_Confirmados_CHIK) == SE) - 1]
colnames(AUX_GRAF)[11] <- colnames(RS_2025_SE_Confirmados_CHIK)[which(colnames(RS_2025_SE_Confirmados_CHIK) == SE)]

AUX_GRAF[nrow(AUX_GRAF), 1] <- "Municipios"

AUX_GRAF <- AUX_GRAF[-(nrow(AUX_GRAF)-1),]

AUX_GRAF <- AUX_GRAF[c(nrow(AUX_GRAF), 1:(nrow(AUX_GRAF) -1)), ]

AUX_GRAF <- t(AUX_GRAF)

colnames(AUX_GRAF) <- AUX_GRAF[1,]

AUX_GRAF <- AUX_GRAF[-1,]

colnames(AUX_GRAF)[1] <- "SE"

AUX_GRAF <- as.data.frame(AUX_GRAF)

AUX_GRAF[, 2: ncol(AUX_GRAF)] <- apply(AUX_GRAF[, 2: ncol(AUX_GRAF)], 2, as.numeric)

rownames(AUX_GRAF) <- 1: nrow(AUX_GRAF)

colnames(AUX_GRAF) <- str_replace(colnames(AUX_GRAF), " ", "_")
colnames(AUX_GRAF) <- str_replace(colnames(AUX_GRAF), " ", "_")
colnames(AUX_GRAF) <- str_replace(colnames(AUX_GRAF), " ", "_")
colnames(AUX_GRAF) <- str_replace(colnames(AUX_GRAF), " ", "_")

AUX_GRAF$SE <- factor(as.numeric(AUX_GRAF$SE), 
                      levels = sort(unique(as.numeric(AUX_GRAF$SE))))

######   Criando o conjunto de gráficos dos histogramas via Lapply.  ######

AUX_HIST_CHIK_CONF_LIST <- AUX_GRAF %>%
  pivot_longer(-SE, names_to = "Municipios") %>% 
  mutate(
    SE = SE,
    Municipios = gsub("_", " ", Municipios)
  ) %>%
  group_split(Municipios) %>% 
  lapply(\(dados) {
    titulo <- dados$Municipios %>% 
      unique() %>% 
      paste0(" - Confirmados")
    ggplot(dados, aes(x = SE, 
                      y = value)
    ) + 
      geom_col(color = "black", 
               fill = "#DB7093") + 
      geom_label(aes(label = value), 
                 alpha = 0.5, 
                 vjust = 0.1) +
      labs(
        caption = Fonte, 
        x = "Semana Epidemiológica",
        y = "Número de Casos",
        title = titulo
      ) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
      Theme_Hist()
  })

RS_2025_GRAF_CHIK_Histograma_Confirmados_01 <- (AUX_HIST_CHIK_CONF_LIST[[1]] + AUX_HIST_CHIK_CONF_LIST[[2]]) / 
  (AUX_HIST_CHIK_CONF_LIST[[3]] + AUX_HIST_CHIK_CONF_LIST[[4]]) / 
  (AUX_HIST_CHIK_CONF_LIST[[5]] + AUX_HIST_CHIK_CONF_LIST[[6]]) / 
  (AUX_HIST_CHIK_CONF_LIST[[7]] + AUX_HIST_CHIK_CONF_LIST[[8]]) 

RS_2025_GRAF_CHIK_Histograma_Confirmados_02 <- (AUX_HIST_CHIK_CONF_LIST[[9]] + AUX_HIST_CHIK_CONF_LIST[[10]]) / 
  (AUX_HIST_CHIK_CONF_LIST[[11]] + AUX_HIST_CHIK_CONF_LIST[[12]]) / 
  (AUX_HIST_CHIK_CONF_LIST[[13]] + AUX_HIST_CHIK_CONF_LIST[[14]]) / 
  (AUX_HIST_CHIK_CONF_LIST[[15]] + AUX_HIST_CHIK_CONF_LIST[[16]]) 

######Histogramas

###Provaveis

RS_2025_SE_Provaveis_CHIK <- RS_2025_SE_Provaveis_CHIK[, -54]
RS_2025_SE_Provaveis_CHIK[nrow(RS_2025_SE_Provaveis_CHIK) +1, 2:ncol(RS_2025_SE_Provaveis_CHIK)] <- c("1",  "2", "3", 
                                                                                       "4",  "5",  "6",  
                                                                                       "7",  "8",  "9",  
                                                                                       "10",  "11",  "12",  
                                                                                       "13",  "14",  "15",  
                                                                                       "16",  "17",  "18",  
                                                                                       "19",  "20",  "21",  
                                                                                       "22",  "23",  
                                                                                       "24",  "25",  "26",  
                                                                                       "27",  "28",  "29",  
                                                                                       "30",  "31",  "32", 
                                                                                       "33",  "34",  "35",  
                                                                                       "36",  "37",  "38",  
                                                                                       "39",  "40",  "41",  
                                                                                       "42",  "43",  "244",  
                                                                                       "45",  "46",  "47",  
                                                                                       "48",  "49",  "50",  
                                                                                       "51",  "52")


AUX_GRAF <- as.data.frame(RS_2025_SE_Provaveis_CHIK$Município)

AUX_GRAF[, 2] <- as.data.frame(RS_2025_SE_Provaveis_CHIK[, which(colnames(RS_2025_SE_Provaveis_CHIK) == SE) - 9])
AUX_GRAF[, 3] <- as.data.frame(RS_2025_SE_Provaveis_CHIK[, which(colnames(RS_2025_SE_Provaveis_CHIK) == SE) - 8])
AUX_GRAF[, 4] <- as.data.frame(RS_2025_SE_Provaveis_CHIK[, which(colnames(RS_2025_SE_Provaveis_CHIK) == SE) - 7])
AUX_GRAF[, 5] <- as.data.frame(RS_2025_SE_Provaveis_CHIK[, which(colnames(RS_2025_SE_Provaveis_CHIK) == SE) - 6])
AUX_GRAF[, 6] <- as.data.frame(RS_2025_SE_Provaveis_CHIK[, which(colnames(RS_2025_SE_Provaveis_CHIK) == SE) - 5])
AUX_GRAF[, 7] <- as.data.frame(RS_2025_SE_Provaveis_CHIK[, which(colnames(RS_2025_SE_Provaveis_CHIK) == SE) - 4])
AUX_GRAF[, 8] <- as.data.frame(RS_2025_SE_Provaveis_CHIK[, which(colnames(RS_2025_SE_Provaveis_CHIK) == SE) - 3])
AUX_GRAF[, 9] <- as.data.frame(RS_2025_SE_Provaveis_CHIK[, which(colnames(RS_2025_SE_Provaveis_CHIK) == SE) - 2])
AUX_GRAF[, 10] <- as.data.frame(RS_2025_SE_Provaveis_CHIK[, which(colnames(RS_2025_SE_Provaveis_CHIK) == SE) - 1])
AUX_GRAF[, 11] <- as.data.frame(RS_2025_SE_Provaveis_CHIK[, which(colnames(RS_2025_SE_Provaveis_CHIK) == SE)])

colnames(AUX_GRAF)[1] <- "Municipios"
colnames(AUX_GRAF)[2] <- colnames(RS_2025_SE_Provaveis_CHIK)[which(colnames(RS_2025_SE_Provaveis_CHIK) == SE) - 9]
colnames(AUX_GRAF)[3] <- colnames(RS_2025_SE_Provaveis_CHIK)[which(colnames(RS_2025_SE_Provaveis_CHIK) == SE) - 8]
colnames(AUX_GRAF)[4] <- colnames(RS_2025_SE_Provaveis_CHIK)[which(colnames(RS_2025_SE_Provaveis_CHIK) == SE) - 7]
colnames(AUX_GRAF)[5] <- colnames(RS_2025_SE_Provaveis_CHIK)[which(colnames(RS_2025_SE_Provaveis_CHIK) == SE) - 6]
colnames(AUX_GRAF)[6] <- colnames(RS_2025_SE_Provaveis_CHIK)[which(colnames(RS_2025_SE_Provaveis_CHIK) == SE) - 5]
colnames(AUX_GRAF)[7] <- colnames(RS_2025_SE_Provaveis_CHIK)[which(colnames(RS_2025_SE_Provaveis_CHIK) == SE) - 4]
colnames(AUX_GRAF)[8] <- colnames(RS_2025_SE_Provaveis_CHIK)[which(colnames(RS_2025_SE_Provaveis_CHIK) == SE) - 3]
colnames(AUX_GRAF)[9] <- colnames(RS_2025_SE_Provaveis_CHIK)[which(colnames(RS_2025_SE_Provaveis_CHIK) == SE) - 2]
colnames(AUX_GRAF)[10] <- colnames(RS_2025_SE_Provaveis_CHIK)[which(colnames(RS_2025_SE_Provaveis_CHIK) == SE) - 1]
colnames(AUX_GRAF)[11] <- colnames(RS_2025_SE_Provaveis_CHIK)[which(colnames(RS_2025_SE_Provaveis_CHIK) == SE)]


AUX_GRAF[nrow(AUX_GRAF), 1] <- "Municipios"

AUX_GRAF <- AUX_GRAF[-(nrow(AUX_GRAF)-1),]

AUX_GRAF <- AUX_GRAF[c(nrow(AUX_GRAF), 1:(nrow(AUX_GRAF) -1)), ]

AUX_GRAF <- t(AUX_GRAF)

colnames(AUX_GRAF) <- AUX_GRAF[1,]

AUX_GRAF <- AUX_GRAF[-1,]

colnames(AUX_GRAF)[1] <- "SE"

AUX_GRAF <- as.data.frame(AUX_GRAF)

AUX_GRAF[, 2: ncol(AUX_GRAF)] <- apply(AUX_GRAF[, 2: ncol(AUX_GRAF)], 2, as.numeric)

rownames(AUX_GRAF) <- 1: nrow(AUX_GRAF)

colnames(AUX_GRAF) <- str_replace(colnames(AUX_GRAF), " ", "_")
colnames(AUX_GRAF) <- str_replace(colnames(AUX_GRAF), " ", "_")
colnames(AUX_GRAF) <- str_replace(colnames(AUX_GRAF), " ", "_")
colnames(AUX_GRAF) <- str_replace(colnames(AUX_GRAF), " ", "_")

AUX_GRAF$SE <- factor(as.numeric(AUX_GRAF$SE), 
                      levels = sort(unique(as.numeric(AUX_GRAF$SE))))

AUX_HIST_CHIK_PROV_LIST <- AUX_GRAF %>%
  pivot_longer(-SE, names_to = "Municipios") %>% 
  mutate(
    SE = SE,
    Municipios = gsub("_", " ", Municipios)
  ) %>%
  group_split(Municipios) %>% 
  lapply(\(dados) {
    titulo <- dados$Municipios %>% 
      unique() %>% 
      paste0(" - Prováveis")
    ggplot(dados, aes(x = SE, 
                      y = value)
    ) + 
      geom_col(color = "black", 
               fill = "#F0E68C") + 
      geom_label(aes(label = value), 
                 alpha = 0.5, 
                 vjust = 0.1) +
      labs(
        caption = Fonte, 
        x = "Semana Epidemiológica",
        y = "Número de Casos",
        title = titulo
      ) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
      Theme_Hist()
  })

RS_2025_GRAF_CHIK_Histograma_Provaveis_01 <- (AUX_HIST_CHIK_PROV_LIST[[1]] + AUX_HIST_CHIK_PROV_LIST[[2]]) / 
  (AUX_HIST_CHIK_PROV_LIST[[3]] + AUX_HIST_CHIK_PROV_LIST[[4]]) / 
  (AUX_HIST_CHIK_PROV_LIST[[5]] + AUX_HIST_CHIK_PROV_LIST[[6]]) / 
  (AUX_HIST_CHIK_PROV_LIST[[7]] + AUX_HIST_CHIK_PROV_LIST[[8]]) 

RS_2025_GRAF_CHIK_Histograma_Provaveis_02 <- (AUX_HIST_CHIK_PROV_LIST[[9]] + AUX_HIST_CHIK_PROV_LIST[[10]]) / 
  (AUX_HIST_CHIK_PROV_LIST[[11]] + AUX_HIST_CHIK_PROV_LIST[[12]]) / 
  (AUX_HIST_CHIK_PROV_LIST[[13]] + AUX_HIST_CHIK_PROV_LIST[[14]]) / 
  (AUX_HIST_CHIK_PROV_LIST[[15]] + AUX_HIST_CHIK_PROV_LIST[[16]]) 


##########################LACEN CHIKUNGUNYA   ############################################################
############################################################################################################################################
#####################  Realizando a contagem de exames por SE SOROLOGIA GERAL  #############################################################

RS22_2025_SE_SOROLOGIA_CHIK <- matrix(data = NA, 
                                 nrow = nrow, 
                                 ncol = 54)

RS22_2025_SE_SOROLOGIA_CHIK <- as.data.frame(RS22_2025_SE_SOROLOGIA_CHIK)

colnames(RS22_2025_SE_SOROLOGIA_CHIK)[1] <- "Município" 

RS22_2025_SE_SOROLOGIA_CHIK[,1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

colnames (RS22_2025_SE_SOROLOGIA_CHIK)[2:54] <- c(1:53)

for (i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 3]){
  
  RS22_2025_SE_SOROLOGIA_CHIK[which(RS22_2025_SE_SOROLOGIA_CHIK == i), 2] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>%
                                                                                filter(Municipio_Residencia == i,
                                                                                       SE == 1,
                                                                                       Status_Exame == "Resultado Liberado" |
                                                                                         Status_Exame == "Automaçăo em Processamento" |
                                                                                         Status_Exame == "Exame em Análise" |
                                                                                         Status_Exame == "Disponível para Encaminhar" |
                                                                                         Status_Exame == "Aguardando Triagem") %>% 
                                                                                count()
  )
  
  RS22_2025_SE_SOROLOGIA_CHIK[which(RS22_2025_SE_SOROLOGIA_CHIK == i), 3] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>% 
                                                                                filter(Municipio_Residencia == i,
                                                                                       SE == 2,
                                                                                       Status_Exame == "Resultado Liberado" |
                                                                                         Status_Exame == "Automaçăo em Processamento" |
                                                                                         Status_Exame == "Exame em Análise" |
                                                                                         Status_Exame == "Disponível para Encaminhar" |
                                                                                         Status_Exame == "Aguardando Triagem") %>% 
                                                                                count()
  )
  
  RS22_2025_SE_SOROLOGIA_CHIK[which(RS22_2025_SE_SOROLOGIA_CHIK == i), 4] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>% 
                                                                                filter(Municipio_Residencia == i,
                                                                                       SE == 3,
                                                                                       Status_Exame == "Resultado Liberado" |
                                                                                         Status_Exame == "Automaçăo em Processamento" |
                                                                                         Status_Exame == "Exame em Análise" |
                                                                                         Status_Exame == "Disponível para Encaminhar" |
                                                                                         Status_Exame == "Aguardando Triagem") %>%  
                                                                                count()
  )
  
  RS22_2025_SE_SOROLOGIA_CHIK[which(RS22_2025_SE_SOROLOGIA_CHIK == i),5] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>% 
                                                                               filter(Municipio_Residencia == i,
                                                                                      SE == 4,
                                                                                      Status_Exame == "Resultado Liberado" |
                                                                                        Status_Exame == "Automaçăo em Processamento" |
                                                                                        Status_Exame == "Exame em Análise" |
                                                                                        Status_Exame == "Disponível para Encaminhar" |
                                                                                        Status_Exame == "Aguardando Triagem") %>% 
                                                                               count()
  )
  
  RS22_2025_SE_SOROLOGIA_CHIK[which(RS22_2025_SE_SOROLOGIA_CHIK == i), 6] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>% 
                                                                                filter(Municipio_Residencia == i,
                                                                                       SE == 5,
                                                                                       Status_Exame == "Resultado Liberado" |
                                                                                         Status_Exame == "Automaçăo em Processamento" |
                                                                                         Status_Exame == "Exame em Análise" |
                                                                                         Status_Exame == "Disponível para Encaminhar" |
                                                                                         Status_Exame == "Aguardando Triagem") %>% 
                                                                                count() 
  )
  
  RS22_2025_SE_SOROLOGIA_CHIK[which(RS22_2025_SE_SOROLOGIA_CHIK == i), 7] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>%
                                                                                filter(Municipio_Residencia == i,
                                                                                       SE == 6,
                                                                                       Status_Exame == "Resultado Liberado" |
                                                                                         Status_Exame == "Automaçăo em Processamento" |
                                                                                         Status_Exame == "Exame em Análise" |
                                                                                         Status_Exame == "Disponível para Encaminhar" |
                                                                                         Status_Exame == "Aguardando Triagem") %>% 
                                                                                count()
  )
  
  RS22_2025_SE_SOROLOGIA_CHIK[which(RS22_2025_SE_SOROLOGIA_CHIK == i), 8] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>%
                                                                                filter(Municipio_Residencia == i,
                                                                                       SE == 7,
                                                                                       Status_Exame == "Resultado Liberado" |
                                                                                         Status_Exame == "Automaçăo em Processamento" |
                                                                                         Status_Exame == "Exame em Análise" |
                                                                                         Status_Exame == "Disponível para Encaminhar" |
                                                                                         Status_Exame == "Aguardando Triagem") %>% 
                                                                                count() 
  )
  
  RS22_2025_SE_SOROLOGIA_CHIK[which(RS22_2025_SE_SOROLOGIA_CHIK == i), 9] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>%
                                                                                filter(Municipio_Residencia == i,
                                                                                       SE == 8,
                                                                                       Status_Exame == "Resultado Liberado" |
                                                                                         Status_Exame == "Automaçăo em Processamento" |
                                                                                         Status_Exame == "Exame em Análise" |
                                                                                         Status_Exame == "Disponível para Encaminhar" |
                                                                                         Status_Exame == "Aguardando Triagem") %>% 
                                                                                count() 
  )
  
  RS22_2025_SE_SOROLOGIA_CHIK[which(RS22_2025_SE_SOROLOGIA_CHIK == i), 10] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>% 
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 9,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automaçăo em Processamento" |
                                                                                          Status_Exame == "Exame em Análise" |
                                                                                          Status_Exame == "Disponível para Encaminhar" |
                                                                                          Status_Exame == "Aguardando Triagem") %>% 
                                                                                 count()
  )
  
  RS22_2025_SE_SOROLOGIA_CHIK[which(RS22_2025_SE_SOROLOGIA_CHIK == i), 11] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>% 
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 10,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automaçăo em Processamento" |
                                                                                          Status_Exame == "Exame em Análise" |
                                                                                          Status_Exame == "Disponível para Encaminhar" |
                                                                                          Status_Exame == "Aguardando Triagem") %>% 
                                                                                 count() 
  )
  
  RS22_2025_SE_SOROLOGIA_CHIK[which(RS22_2025_SE_SOROLOGIA_CHIK == i), 12] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>%
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 11,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automaçăo em Processamento" |
                                                                                          Status_Exame == "Exame em Análise" |
                                                                                          Status_Exame == "Disponível para Encaminhar" |
                                                                                          Status_Exame == "Aguardando Triagem") %>% 
                                                                                 count() 
  )
  
  RS22_2025_SE_SOROLOGIA_CHIK[which(RS22_2025_SE_SOROLOGIA_CHIK == i), 13] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>% 
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 12,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automaçăo em Processamento" |
                                                                                          Status_Exame == "Exame em Análise" |
                                                                                          Status_Exame == "Disponível para Encaminhar" |
                                                                                          Status_Exame == "Aguardando Triagem") %>% 
                                                                                 count() 
  )
  
  RS22_2025_SE_SOROLOGIA_CHIK[which(RS22_2025_SE_SOROLOGIA_CHIK == i), 14] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>% 
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 13,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automaçăo em Processamento" |
                                                                                          Status_Exame == "Exame em Análise" |
                                                                                          Status_Exame == "Disponível para Encaminhar" |
                                                                                          Status_Exame == "Aguardando Triagem") %>% 
                                                                                 count() 
  )
  
  RS22_2025_SE_SOROLOGIA_CHIK[which(RS22_2025_SE_SOROLOGIA_CHIK == i), 15] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>% 
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 14,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automaçăo em Processamento" |
                                                                                          Status_Exame == "Exame em Análise" |
                                                                                          Status_Exame == "Disponível para Encaminhar" |
                                                                                          Status_Exame == "Aguardando Triagem") %>% 
                                                                                 count() 
  )
  
  RS22_2025_SE_SOROLOGIA_CHIK[which(RS22_2025_SE_SOROLOGIA_CHIK == i), 16] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>% 
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 15,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automaçăo em Processamento" |
                                                                                          Status_Exame == "Exame em Análise" |
                                                                                          Status_Exame == "Disponível para Encaminhar" |
                                                                                          Status_Exame == "Aguardando Triagem") %>% 
                                                                                 count() 
  )
  
  RS22_2025_SE_SOROLOGIA_CHIK[which(RS22_2025_SE_SOROLOGIA_CHIK == i), 17] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>% 
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 16,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automaçăo em Processamento" |
                                                                                          Status_Exame == "Exame em Análise" |
                                                                                          Status_Exame == "Disponível para Encaminhar" |
                                                                                          Status_Exame == "Aguardando Triagem") %>% 
                                                                                 count()
  )
  
  RS22_2025_SE_SOROLOGIA_CHIK[which(RS22_2025_SE_SOROLOGIA_CHIK == i), 18] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>%
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 17,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automaçăo em Processamento" |
                                                                                          Status_Exame == "Exame em Análise" |
                                                                                          Status_Exame == "Disponível para Encaminhar" |
                                                                                          Status_Exame == "Aguardando Triagem") %>%       
                                                                                 count() 
  )
  
  RS22_2025_SE_SOROLOGIA_CHIK[which(RS22_2025_SE_SOROLOGIA_CHIK == i), 19] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>% 
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 18,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automaçăo em Processamento" |
                                                                                          Status_Exame == "Exame em Análise" |
                                                                                          Status_Exame == "Disponível para Encaminhar" |
                                                                                          Status_Exame == "Aguardando Triagem") %>%    
                                                                                 count() 
  )
  
  RS22_2025_SE_SOROLOGIA_CHIK[which(RS22_2025_SE_SOROLOGIA_CHIK == i), 20] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>% 
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 19,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automaçăo em Processamento" |
                                                                                          Status_Exame == "Exame em Análise" |
                                                                                          Status_Exame == "Disponível para Encaminhar" |
                                                                                          Status_Exame == "Aguardando Triagem") %>% 
                                                                                 count() 
  )
  
  RS22_2025_SE_SOROLOGIA_CHIK[which(RS22_2025_SE_SOROLOGIA_CHIK == i),  21] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>% 
                                                                                  filter(Municipio_Residencia == i,
                                                                                         SE == 20,
                                                                                         Status_Exame == "Resultado Liberado" |
                                                                                           Status_Exame == "Automaçăo em Processamento" |
                                                                                           Status_Exame == "Exame em Análise" |
                                                                                           Status_Exame == "Disponível para Encaminhar" |
                                                                                           Status_Exame == "Aguardando Triagem") %>% 
                                                                                  count() 
  )
  
  RS22_2025_SE_SOROLOGIA_CHIK[which(RS22_2025_SE_SOROLOGIA_CHIK == i), 22] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>% 
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 21,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automaçăo em Processamento" |
                                                                                          Status_Exame == "Exame em Análise" |
                                                                                          Status_Exame == "Disponível para Encaminhar" |
                                                                                          Status_Exame == "Aguardando Triagem") %>% 
                                                                                 count() 
  )
  
  RS22_2025_SE_SOROLOGIA_CHIK[which(RS22_2025_SE_SOROLOGIA_CHIK == i), 23] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>% 
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 22,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automaçăo em Processamento" |
                                                                                          Status_Exame == "Exame em Análise" |
                                                                                          Status_Exame == "Disponível para Encaminhar" |
                                                                                          Status_Exame == "Aguardando Triagem") %>% 
                                                                                 count() 
  )
  
  RS22_2025_SE_SOROLOGIA_CHIK[which(RS22_2025_SE_SOROLOGIA_CHIK == i), 24] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>% 
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 23,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automaçăo em Processamento" |
                                                                                          Status_Exame == "Exame em Análise" |
                                                                                          Status_Exame == "Disponível para Encaminhar" |
                                                                                          Status_Exame == "Aguardando Triagem") %>% 
                                                                                 count()
  )
  
  RS22_2025_SE_SOROLOGIA_CHIK[which(RS22_2025_SE_SOROLOGIA_CHIK == i), 25] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>%
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 24,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automaçăo em Processamento" |
                                                                                          Status_Exame == "Exame em Análise" |
                                                                                          Status_Exame == "Disponível para Encaminhar" |
                                                                                          Status_Exame == "Aguardando Triagem") %>% 
                                                                                 count() 
  )
  
  RS22_2025_SE_SOROLOGIA_CHIK[which(RS22_2025_SE_SOROLOGIA_CHIK == i), 26] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>% 
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 25,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automaçăo em Processamento" |
                                                                                          Status_Exame == "Exame em Análise" |
                                                                                          Status_Exame == "Disponível para Encaminhar" |
                                                                                          Status_Exame == "Aguardando Triagem") %>% 
                                                                                 count()
  )
  
  RS22_2025_SE_SOROLOGIA_CHIK[which(RS22_2025_SE_SOROLOGIA_CHIK == i), 27] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>%
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 26,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automaçăo em Processamento" |
                                                                                          Status_Exame == "Exame em Análise" |
                                                                                          Status_Exame == "Disponível para Encaminhar" |
                                                                                          Status_Exame == "Aguardando Triagem") %>% 
                                                                                 count() 
  )
  
  RS22_2025_SE_SOROLOGIA_CHIK[which(RS22_2025_SE_SOROLOGIA_CHIK == i), 28] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>% 
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 27,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automaçăo em Processamento" |
                                                                                          Status_Exame == "Exame em Análise" |
                                                                                          Status_Exame == "Disponível para Encaminhar" |
                                                                                          Status_Exame == "Aguardando Triagem") %>% 
                                                                                 count() 
  )
  
  RS22_2025_SE_SOROLOGIA_CHIK[which(RS22_2025_SE_SOROLOGIA_CHIK == i), 29] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>% 
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 28,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automaçăo em Processamento" |
                                                                                          Status_Exame == "Exame em Análise" |
                                                                                          Status_Exame == "Disponível para Encaminhar" |
                                                                                          Status_Exame == "Aguardando Triagem") %>% 
                                                                                 count() 
  )
  
  RS22_2025_SE_SOROLOGIA_CHIK[which(RS22_2025_SE_SOROLOGIA_CHIK == i), 30] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>%
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 29,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automaçăo em Processamento" |
                                                                                          Status_Exame == "Exame em Análise" |
                                                                                          Status_Exame == "Disponível para Encaminhar" |
                                                                                          Status_Exame == "Aguardando Triagem") %>% 
                                                                                 count() 
  )
  
  RS22_2025_SE_SOROLOGIA_CHIK[which(RS22_2025_SE_SOROLOGIA_CHIK == i), 31] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>%
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 30,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automaçăo em Processamento" |
                                                                                          Status_Exame == "Exame em Análise" |
                                                                                          Status_Exame == "Disponível para Encaminhar" |
                                                                                          Status_Exame == "Aguardando Triagem") %>% 
                                                                                 count()
  )
  
  RS22_2025_SE_SOROLOGIA_CHIK[which(RS22_2025_SE_SOROLOGIA_CHIK == i), 32] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>% 
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 31,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automaçăo em Processamento" |
                                                                                          Status_Exame == "Exame em Análise" |
                                                                                          Status_Exame == "Disponível para Encaminhar" |
                                                                                          Status_Exame == "Aguardando Triagem") %>% 
                                                                                 count()
  )
  
  RS22_2025_SE_SOROLOGIA_CHIK[which(RS22_2025_SE_SOROLOGIA_CHIK == i), 33] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>%
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 32,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automaçăo em Processamento" |
                                                                                          Status_Exame == "Exame em Análise" |
                                                                                          Status_Exame == "Disponível para Encaminhar" |
                                                                                          Status_Exame == "Aguardando Triagem") %>% 
                                                                                 count() 
  )
  
  RS22_2025_SE_SOROLOGIA_CHIK[which(RS22_2025_SE_SOROLOGIA_CHIK == i), 34] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>%
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 33,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automaçăo em Processamento" |
                                                                                          Status_Exame == "Exame em Análise" |
                                                                                          Status_Exame == "Disponível para Encaminhar" |
                                                                                          Status_Exame == "Aguardando Triagem") %>% 
                                                                                 count() 
  )
  
  RS22_2025_SE_SOROLOGIA_CHIK[which(RS22_2025_SE_SOROLOGIA_CHIK == i), 35] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>%
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 34,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automaçăo em Processamento" |
                                                                                          Status_Exame == "Exame em Análise" |
                                                                                          Status_Exame == "Disponível para Encaminhar" |
                                                                                          Status_Exame == "Aguardando Triagem") %>% 
                                                                                 count() 
  )
  
  RS22_2025_SE_SOROLOGIA_CHIK[which(RS22_2025_SE_SOROLOGIA_CHIK == i), 36] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>% 
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 35,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automaçăo em Processamento" |
                                                                                          Status_Exame == "Exame em Análise" |
                                                                                          Status_Exame == "Disponível para Encaminhar" |
                                                                                          Status_Exame == "Aguardando Triagem") %>% 
                                                                                 count() 
  )
  
  RS22_2025_SE_SOROLOGIA_CHIK[which(RS22_2025_SE_SOROLOGIA_CHIK == i), 37] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>%
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 36,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automaçăo em Processamento" |
                                                                                          Status_Exame == "Exame em Análise" |
                                                                                          Status_Exame == "Disponível para Encaminhar" |
                                                                                          Status_Exame == "Aguardando Triagem") %>% 
                                                                                 count() 
  )
  
  RS22_2025_SE_SOROLOGIA_CHIK[which(RS22_2025_SE_SOROLOGIA_CHIK == i), 38] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>%
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 37,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automaçăo em Processamento" |
                                                                                          Status_Exame == "Exame em Análise" |
                                                                                          Status_Exame == "Disponível para Encaminhar" |
                                                                                          Status_Exame == "Aguardando Triagem") %>% 
                                                                                 count() 
  )
  
  RS22_2025_SE_SOROLOGIA_CHIK[which(RS22_2025_SE_SOROLOGIA_CHIK == i), 39] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>% 
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 38,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automaçăo em Processamento" |
                                                                                          Status_Exame == "Exame em Análise" |
                                                                                          Status_Exame == "Disponível para Encaminhar" |
                                                                                          Status_Exame == "Aguardando Triagem") %>% 
                                                                                 count()
  )
  
  RS22_2025_SE_SOROLOGIA_CHIK[which(RS22_2025_SE_SOROLOGIA_CHIK == i), 40] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>%
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 39,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automaçăo em Processamento" |
                                                                                          Status_Exame == "Exame em Análise" |
                                                                                          Status_Exame == "Disponível para Encaminhar" |
                                                                                          Status_Exame == "Aguardando Triagem") %>% 
                                                                                 count() 
  )
  
  RS22_2025_SE_SOROLOGIA_CHIK[which(RS22_2025_SE_SOROLOGIA_CHIK == i), 41] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>%
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 40,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automaçăo em Processamento" |
                                                                                          Status_Exame == "Exame em Análise" |
                                                                                          Status_Exame == "Disponível para Encaminhar" |
                                                                                          Status_Exame == "Aguardando Triagem") %>% 
                                                                                 count() 
  )
  
  RS22_2025_SE_SOROLOGIA_CHIK[which(RS22_2025_SE_SOROLOGIA_CHIK == i), 42] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>% 
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 41,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automaçăo em Processamento" |
                                                                                          Status_Exame == "Exame em Análise" |
                                                                                          Status_Exame == "Disponível para Encaminhar" |
                                                                                          Status_Exame == "Aguardando Triagem") %>% 
                                                                                 count() 
  )
  
  RS22_2025_SE_SOROLOGIA_CHIK[which(RS22_2025_SE_SOROLOGIA_CHIK == i), 43] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>% 
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 42,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automaçăo em Processamento" |
                                                                                          Status_Exame == "Exame em Análise" |
                                                                                          Status_Exame == "Disponível para Encaminhar" |
                                                                                          Status_Exame == "Aguardando Triagem") %>% 
                                                                                 count()
  )
  
  RS22_2025_SE_SOROLOGIA_CHIK[which(RS22_2025_SE_SOROLOGIA_CHIK == i), 44] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>%
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 43,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automaçăo em Processamento" |
                                                                                          Status_Exame == "Exame em Análise" |
                                                                                          Status_Exame == "Disponível para Encaminhar" |
                                                                                          Status_Exame == "Aguardando Triagem") %>% 
                                                                                 count()
  )
  
  RS22_2025_SE_SOROLOGIA_CHIK[which(RS22_2025_SE_SOROLOGIA_CHIK == i), 45] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>%
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 44,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automaçăo em Processamento" |
                                                                                          Status_Exame == "Exame em Análise" |
                                                                                          Status_Exame == "Disponível para Encaminhar" |
                                                                                          Status_Exame == "Aguardando Triagem") %>% 
                                                                                 count() 
  )
  
  RS22_2025_SE_SOROLOGIA_CHIK[which(RS22_2025_SE_SOROLOGIA_CHIK == i), 46] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>%
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 45,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automaçăo em Processamento" |
                                                                                          Status_Exame == "Exame em Análise" |
                                                                                          Status_Exame == "Disponível para Encaminhar" |
                                                                                          Status_Exame == "Aguardando Triagem") %>% 
                                                                                 count()
  )
  
  RS22_2025_SE_SOROLOGIA_CHIK[which(RS22_2025_SE_SOROLOGIA_CHIK == i), 47] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>%
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 46,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automaçăo em Processamento" |
                                                                                          Status_Exame == "Exame em Análise" |
                                                                                          Status_Exame == "Disponível para Encaminhar" |
                                                                                          Status_Exame == "Aguardando Triagem") %>% 
                                                                                 count() 
  )
  
  RS22_2025_SE_SOROLOGIA_CHIK[which(RS22_2025_SE_SOROLOGIA_CHIK == i), 48] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>%
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 47,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automaçăo em Processamento" |
                                                                                          Status_Exame == "Exame em Análise" |
                                                                                          Status_Exame == "Disponível para Encaminhar" |
                                                                                          Status_Exame == "Aguardando Triagem") %>% 
                                                                                 count() 
  )
  
  RS22_2025_SE_SOROLOGIA_CHIK[which(RS22_2025_SE_SOROLOGIA_CHIK == i), 49] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>%
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 48,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automaçăo em Processamento" |
                                                                                          Status_Exame == "Exame em Análise" |
                                                                                          Status_Exame == "Disponível para Encaminhar" |
                                                                                          Status_Exame == "Aguardando Triagem") %>% 
                                                                                 count() 
  )
  
  RS22_2025_SE_SOROLOGIA_CHIK[which(RS22_2025_SE_SOROLOGIA_CHIK == i), 50] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>%
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 49,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automaçăo em Processamento" |
                                                                                          Status_Exame == "Exame em Análise" |
                                                                                          Status_Exame == "Disponível para Encaminhar" |
                                                                                          Status_Exame == "Aguardando Triagem") %>% 
                                                                                 count()
  )
  
  RS22_2025_SE_SOROLOGIA_CHIK[which(RS22_2025_SE_SOROLOGIA_CHIK == i), 51] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>% 
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 50,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automaçăo em Processamento" |
                                                                                          Status_Exame == "Exame em Análise" |
                                                                                          Status_Exame == "Disponível para Encaminhar" |
                                                                                          Status_Exame == "Aguardando Triagem") %>% 
                                                                                 count() 
  )
  
  RS22_2025_SE_SOROLOGIA_CHIK[which(RS22_2025_SE_SOROLOGIA_CHIK == i), 52] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>%
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 51,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automaçăo em Processamento" |
                                                                                          Status_Exame == "Exame em Análise" |
                                                                                          Status_Exame == "Disponível para Encaminhar" |
                                                                                          Status_Exame == "Aguardando Triagem") %>% 
                                                                                 count() 
  )
  
  RS22_2025_SE_SOROLOGIA_CHIK[which(RS22_2025_SE_SOROLOGIA_CHIK == i), 53] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>%
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 52,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automaçăo em Processamento" |
                                                                                          Status_Exame == "Exame em Análise" |
                                                                                          Status_Exame == "Disponível para Encaminhar" |
                                                                                          Status_Exame == "Aguardando Triagem") %>% 
                                                                                 count() 
  )
  
  RS22_2025_SE_SOROLOGIA_CHIK[which(RS22_2025_SE_SOROLOGIA_CHIK == i), 54] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>% 
                                                                                 filter(Municipio_Residencia == i,
                                                                                        SE == 53,
                                                                                        Status_Exame == "Resultado Liberado" |
                                                                                          Status_Exame == "Automaçăo em Processamento" |
                                                                                          Status_Exame == "Exame em Análise" |
                                                                                          Status_Exame == "Disponível para Encaminhar" |
                                                                                          Status_Exame == "Aguardando Triagem") %>% 
                                                                                 count() 
  )
}

RS22_2025_SE_SOROLOGIA_CHIK[(nrow(RS22_2025_SE_SOROLOGIA_CHIK) +1), 2:54] <- apply(RS22_2025_SE_SOROLOGIA_CHIK[, 2:54], 
                                                                         2, 
                                                                         sum)

RS22_2025_SE_SOROLOGIA_CHIK[nrow(RS22_2025_SE_SOROLOGIA_CHIK),1] <- "Total"

############################################################################################################################################
#####################  Realizando a contagem de exames por SE SOROLOGIA REAGENTES GERAL  #############################################################

RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK <- matrix(data = NA, 
                                          nrow = nrow, 
                                          ncol = 54)

RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK <- as.data.frame(RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK)

colnames(RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK)[1] <- "Município" 

RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK[,1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

colnames (RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK)[2:54] <- c(1:53)

for (i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 3]){
  
  RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK[which(RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK == i), 2] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>%
                                                                                                  filter(Municipio_Residencia == i,
                                                                                                         SE == 1,
                                                                                                         Resultado == "Reagente " |
                                                                                                           Resultado == "Reagente") %>%
                                                                                                  count()
  )
  
  RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK[which(RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK == i), 3] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>% 
                                                                                                  filter(Municipio_Residencia == i,
                                                                                                         SE == 2,
                                                                                                         Resultado == "Reagente " |
                                                                                                           Resultado == "Reagente") %>% 
                                                                                                  count()
  )
  
  RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK[which(RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK == i), 4] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>% 
                                                                                                  filter(Municipio_Residencia == i,
                                                                                                         SE == 3,
                                                                                                         Resultado == "Reagente " |
                                                                                                           Resultado == "Reagente") %>% 
                                                                                                  count()
  )
  
  RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK[which(RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK == i),5] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>% 
                                                                                                 filter(Municipio_Residencia == i,
                                                                                                        SE == 4,
                                                                                                        Resultado == "Reagente " |
                                                                                                          Resultado == "Reagente") %>% 
                                                                                                 count()
  )
  
  RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK[which(RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK == i), 6] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>% 
                                                                                                  filter(Municipio_Residencia == i,
                                                                                                         SE == 5,
                                                                                                         Resultado == "Reagente " |
                                                                                                           Resultado == "Reagente") %>% 
                                                                                                  count() 
  )
  
  RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK[which(RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK == i), 7] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>%
                                                                                                  filter(Municipio_Residencia == i,
                                                                                                         SE == 6,
                                                                                                         Resultado == "Reagente " |
                                                                                                           Resultado == "Reagente") %>%
                                                                                                  count()
  )
  
  RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK[which(RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK == i), 8] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>%
                                                                                                  filter(Municipio_Residencia == i,
                                                                                                         SE == 7,
                                                                                                         Resultado == "Reagente " |
                                                                                                           Resultado == "Reagente") %>% 
                                                                                                  count() 
  )
  
  RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK[which(RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK == i), 9] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>%
                                                                                                  filter(Municipio_Residencia == i,
                                                                                                         SE == 8,
                                                                                                         Resultado == "Reagente " |
                                                                                                           Resultado == "Reagente") %>% 
                                                                                                  count() 
  )
  
  RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK[which(RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK == i), 10] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>% 
                                                                                                   filter(Municipio_Residencia == i,
                                                                                                          SE == 9,
                                                                                                          Resultado == "Reagente " |
                                                                                                            Resultado == "Reagente") %>%
                                                                                                   count()
  )
  
  RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK[which(RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK == i), 11] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>% 
                                                                                                   filter(Municipio_Residencia == i,
                                                                                                          SE == 10,
                                                                                                          Resultado == "Reagente " |
                                                                                                            Resultado == "Reagente") %>%
                                                                                                   count() 
  )
  
  RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK[which(RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK == i), 12] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>%
                                                                                                   filter(Municipio_Residencia == i,
                                                                                                          SE == 11,
                                                                                                          Resultado == "Reagente " |
                                                                                                            Resultado == "Reagente") %>%
                                                                                                   count() 
  )
  
  RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK[which(RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK == i), 13] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>% 
                                                                                                   filter(Municipio_Residencia == i,
                                                                                                          SE == 12,
                                                                                                          Resultado == "Reagente " |
                                                                                                            Resultado == "Reagente") %>%
                                                                                                   count() 
  )
  
  RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK[which(RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK == i), 14] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>% 
                                                                                                   filter(Municipio_Residencia == i,
                                                                                                          SE == 13,
                                                                                                          Resultado == "Reagente " |
                                                                                                            Resultado == "Reagente") %>% 
                                                                                                   count() 
  )
  
  RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK[which(RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK == i), 15] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>% 
                                                                                                   filter(Municipio_Residencia == i,
                                                                                                          SE == 14,
                                                                                                          Resultado == "Reagente " |
                                                                                                            Resultado == "Reagente") %>%
                                                                                                   count() 
  )
  
  RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK[which(RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK == i), 16] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>% 
                                                                                                   filter(Municipio_Residencia == i,
                                                                                                          SE == 15,
                                                                                                          Resultado == "Reagente " |
                                                                                                            Resultado == "Reagente") %>%
                                                                                                   count() 
  )
  
  RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK[which(RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK == i), 17] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>% 
                                                                                                   filter(Municipio_Residencia == i,
                                                                                                          SE == 16,
                                                                                                          Resultado == "Reagente " |
                                                                                                            Resultado == "Reagente") %>%
                                                                                                   count()
  )
  
  RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK[which(RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK == i), 18] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>%
                                                                                                   filter(Municipio_Residencia == i,
                                                                                                          SE == 17,
                                                                                                          Resultado == "Reagente " |
                                                                                                            Resultado == "Reagente") %>%       
                                                                                                   count() 
  )
  
  RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK[which(RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK == i), 19] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>% 
                                                                                                   filter(Municipio_Residencia == i,
                                                                                                          SE == 18,
                                                                                                          Resultado == "Reagente " |
                                                                                                            Resultado == "Reagente") %>%     
                                                                                                   count() 
  )
  
  RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK[which(RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK == i), 20] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>% 
                                                                                                   filter(Municipio_Residencia == i,
                                                                                                          SE == 19,
                                                                                                          Resultado == "Reagente " |
                                                                                                            Resultado == "Reagente") %>%
                                                                                                   count() 
  )
  
  RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK[which(RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK == i),  21] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>% 
                                                                                                    filter(Municipio_Residencia == i,
                                                                                                           SE == 20,
                                                                                                           Resultado == "Reagente " |
                                                                                                             Resultado == "Reagente") %>%
                                                                                                    count() 
  )
  
  RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK[which(RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK == i), 22] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>% 
                                                                                                   filter(Municipio_Residencia == i,
                                                                                                          SE == 21,
                                                                                                          Resultado == "Reagente " |
                                                                                                            Resultado == "Reagente") %>%
                                                                                                   count() 
  )
  
  RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK[which(RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK == i), 23] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>% 
                                                                                                   filter(Municipio_Residencia == i,
                                                                                                          SE == 22,
                                                                                                          Resultado == "Reagente " |
                                                                                                            Resultado == "Reagente") %>%
                                                                                                   count() 
  )
  
  RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK[which(RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK == i), 24] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>% 
                                                                                                   filter(Municipio_Residencia == i,
                                                                                                          SE == 23,
                                                                                                          Resultado == "Reagente " |
                                                                                                            Resultado == "Reagente") %>%
                                                                                                   count()
  )
  
  RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK[which(RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK == i), 25] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>%
                                                                                                   filter(Municipio_Residencia == i,
                                                                                                          SE == 24,
                                                                                                          Resultado == "Reagente " |
                                                                                                            Resultado == "Reagente") %>%
                                                                                                   count() 
  )
  
  RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK[which(RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK == i), 26] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>% 
                                                                                                   filter(Municipio_Residencia == i,
                                                                                                          SE == 25,
                                                                                                          Resultado == "Reagente " |
                                                                                                            Resultado == "Reagente") %>%
                                                                                                   count()
  )
  
  RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK[which(RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK == i), 27] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>%
                                                                                                   filter(Municipio_Residencia == i,
                                                                                                          SE == 26,
                                                                                                          Resultado == "Reagente " |
                                                                                                            Resultado == "Reagente") %>%
                                                                                                   count() 
  )
  
  RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK[which(RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK == i), 28] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>% 
                                                                                                   filter(Municipio_Residencia == i,
                                                                                                          SE == 27,
                                                                                                          Resultado == "Reagente " |
                                                                                                            Resultado == "Reagente") %>%
                                                                                                   count() 
  )
  
  RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK[which(RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK == i), 29] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>% 
                                                                                                   filter(Municipio_Residencia == i,
                                                                                                          SE == 28,
                                                                                                          Resultado == "Reagente " |
                                                                                                            Resultado == "Reagente") %>%
                                                                                                   count() 
  )
  
  RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK[which(RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK == i), 30] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>%
                                                                                                   filter(Municipio_Residencia == i,
                                                                                                          SE == 29,
                                                                                                          Resultado == "Reagente " |
                                                                                                            Resultado == "Reagente") %>%
                                                                                                   count() 
  )
  
  RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK[which(RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK == i), 31] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>%
                                                                                                   filter(Municipio_Residencia == i,
                                                                                                          SE == 30,
                                                                                                          Resultado == "Reagente " |
                                                                                                            Resultado == "Reagente") %>% 
                                                                                                   count()
  )
  
  RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK[which(RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK == i), 32] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>% 
                                                                                                   filter(Municipio_Residencia == i,
                                                                                                          SE == 31,
                                                                                                          Resultado == "Reagente " |
                                                                                                            Resultado == "Reagente") %>%
                                                                                                   count()
  )
  
  RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK[which(RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK == i), 33] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>%
                                                                                                   filter(Municipio_Residencia == i,
                                                                                                          SE == 32,
                                                                                                          Resultado == "Reagente " |
                                                                                                            Resultado == "Reagente") %>%
                                                                                                   count() 
  )
  
  RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK[which(RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK == i), 34] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>%
                                                                                                   filter(Municipio_Residencia == i,
                                                                                                          SE == 33,
                                                                                                          Resultado == "Reagente " |
                                                                                                            Resultado == "Reagente") %>% 
                                                                                                   count() 
  )
  
  RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK[which(RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK == i), 35] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>%
                                                                                                   filter(Municipio_Residencia == i,
                                                                                                          SE == 34,
                                                                                                          Resultado == "Reagente " |
                                                                                                            Resultado == "Reagente") %>% 
                                                                                                   count() 
  )
  
  RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK[which(RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK == i), 36] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>% 
                                                                                                   filter(Municipio_Residencia == i,
                                                                                                          SE == 35,
                                                                                                          Resultado == "Reagente " |
                                                                                                            Resultado == "Reagente") %>%
                                                                                                   count() 
  )
  
  RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK[which(RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK == i), 37] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>%
                                                                                                   filter(Municipio_Residencia == i,
                                                                                                          SE == 36,
                                                                                                          Resultado == "Reagente " |
                                                                                                            Resultado == "Reagente") %>%
                                                                                                   count() 
  )
  
  RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK[which(RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK == i), 38] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>%
                                                                                                   filter(Municipio_Residencia == i,
                                                                                                          SE == 37,
                                                                                                          Resultado == "Reagente " |
                                                                                                            Resultado == "Reagente") %>% 
                                                                                                   count() 
  )
  
  RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK[which(RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK == i), 39] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>% 
                                                                                                   filter(Municipio_Residencia == i,
                                                                                                          SE == 38,
                                                                                                          Resultado == "Reagente " |
                                                                                                            Resultado == "Reagente") %>%
                                                                                                   count()
  )
  
  RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK[which(RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK == i), 40] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>%
                                                                                                   filter(Municipio_Residencia == i,
                                                                                                          SE == 39,
                                                                                                          Resultado == "Reagente " |
                                                                                                            Resultado == "Reagente") %>%
                                                                                                   count() 
  )
  
  RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK[which(RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK == i), 41] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>%
                                                                                                   filter(Municipio_Residencia == i,
                                                                                                          SE == 40,
                                                                                                          Resultado == "Reagente "|
                                                                                                            Resultado == "Reagente") %>%
                                                                                                   count() 
  )
  
  RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK[which(RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK == i), 42] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>% 
                                                                                                   filter(Municipio_Residencia == i,
                                                                                                          SE == 41,
                                                                                                          Resultado == "Reagente " |
                                                                                                            Resultado == "Reagente") %>%
                                                                                                   count() 
  )
  
  RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK[which(RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK == i), 43] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>% 
                                                                                                   filter(Municipio_Residencia == i,
                                                                                                          SE == 42,
                                                                                                          Resultado == "Reagente " |
                                                                                                            Resultado == "Reagente") %>%
                                                                                                   count()
  )
  
  RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK[which(RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK == i), 44] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>%
                                                                                                   filter(Municipio_Residencia == i,
                                                                                                          SE == 43,
                                                                                                          Resultado == "Reagente " |
                                                                                                            Resultado == "Reagente") %>%
                                                                                                   count()
  )
  
  RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK[which(RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK == i), 45] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>%
                                                                                                   filter(Municipio_Residencia == i,
                                                                                                          SE == 44,
                                                                                                          Resultado == "Reagente " |
                                                                                                            Resultado == "Reagente") %>%
                                                                                                   count() 
  )
  
  RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK[which(RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK == i), 46] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>%
                                                                                                   filter(Municipio_Residencia == i,
                                                                                                          SE == 45,
                                                                                                          Resultado == "Reagente " |
                                                                                                            Resultado == "Reagente") %>%
                                                                                                   count()
  )
  
  RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK[which(RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK == i), 47] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>%
                                                                                                   filter(Municipio_Residencia == i,
                                                                                                          SE == 46,
                                                                                                          Resultado == "Reagente " |
                                                                                                            Resultado == "Reagente") %>%
                                                                                                   count() 
  )
  
  RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK[which(RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK == i), 48] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>%
                                                                                                   filter(Municipio_Residencia == i,
                                                                                                          SE == 47,
                                                                                                          Resultado == "Reagente " |
                                                                                                            Resultado == "Reagente") %>%
                                                                                                   count() 
  )
  
  RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK[which(RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK == i), 49] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>%
                                                                                                   filter(Municipio_Residencia == i,
                                                                                                          SE == 48,
                                                                                                          Resultado == "Reagente " |
                                                                                                            Resultado == "Reagente") %>%
                                                                                                   count() 
  )
  
  RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK[which(RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK == i), 50] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>%
                                                                                                   filter(Municipio_Residencia == i,
                                                                                                          SE == 49,
                                                                                                          Resultado == "Reagente " |
                                                                                                            Resultado == "Reagente") %>% 
                                                                                                   count()
  )
  
  RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK[which(RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK == i), 51] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>% 
                                                                                                   filter(Municipio_Residencia == i,
                                                                                                          SE == 50,
                                                                                                          Resultado == "Reagente " |
                                                                                                            Resultado == "Reagente") %>% 
                                                                                                   count() 
  )
  
  RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK[which(RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK == i), 52] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>%
                                                                                                   filter(Municipio_Residencia == i,
                                                                                                          SE == 51,
                                                                                                          Resultado == "Reagente " |
                                                                                                            Resultado == "Reagente") %>%
                                                                                                   count() 
  )
  
  RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK[which(RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK == i), 53] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>%
                                                                                                   filter(Municipio_Residencia == i,
                                                                                                          SE == 52,
                                                                                                          Resultado == "Reagente " |
                                                                                                            Resultado == "Reagente") %>%
                                                                                                   count() 
  )
  
  RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK[which(RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK == i), 54] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK%>% 
                                                                                                   filter(Municipio_Residencia == i,
                                                                                                          SE == 53,
                                                                                                          Resultado == "Reagente " |
                                                                                                            Resultado == "Reagente") %>%
                                                                                                   count() 
  )
}

RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK[, 13] <- as.numeric(RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK[, 13])

####CORREÇÔES####

## RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK[13, 16] <- 1

## RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK[, 13] <- as.numeric(RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK[, 13])

####CORREÇÔES####

RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK[(nrow(RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK) +1), 2:54] <- apply(RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK[, 2:54], 
                                                                                           2, 
                                                                                           sum)

RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK[nrow(RS22_2025_SE_SOROLOGIA_REAGENTE_CHIK),1] <- "Total"

#####################################################################################################################
####################  LACEN - MUNICÍPIOS   ##########################################################################
#####################################################################################################################

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Sorologia <- NA

AUX$Sorologia_Reag <- NA

AUX$Pesq_Arb <- NA

AUX$Pesq_Arb_Detec <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 3]){
  
  ###Sorologia/Município###  
  AUX[which(AUX$Município == i), 5] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK %>% 
                                                    filter(Municipio_Residencia == i) %>%   
                                                    count()
  )   
  
  ###Sorologia reagente/Município##  
  AUX[which(AUX$Município == i), 6] <- as.integer(RS22_2025_LACEN_SOROLOGIA_CHIK %>% 
                                                    filter(Municipio_Residencia == i,
                                                           Resultado == "Reagente ") %>%   
                                                    count()
  )
  
  ###Pesquisa arbovírus/Município##  
  AUX[which(AUX$Município == i), 7] <- as.integer(RS22_2025_LACEN_PESQ_ARBO_CHIK %>% 
                                                    filter(Municipio_Residencia == i) %>%   
                                                    count()
  )
  
  ###Pesquisa arbovírus detectável/Município##  
  AUX[which(AUX$Município == i), 8] <- as.integer(RS22_2025_LACEN_PESQ_ARBO_CHIK %>% 
                                                    filter(Municipio_Residencia == i,
                                                           Resultado == "Detectável") %>%   
                                                    count()
  )
}

AUX$ENCAMINHADAS <- (AUX$Sorologia)

AUX$POSITIVAS <- (AUX$Sorologia_Reag)

####Correções ####
## AUX[13, 9] <- 1
## AUX[13, 10] <- 1
####Correções###

RS22_GRAF_LACEN_MUNIC <- ggplot (AUX, 
                                 aes(x = Município)) + 
  labs(caption = Fonte, 
       x = NULL,
       y = "Número de Amostras",
       title = "AMOSTRAS ENCAMINHADAS/POSITIVAS - 22ªRS",
       subtitle = "Amostras de sorologia para Chikungunya") +
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
             vjust = 0.1) + 
  scale_fill_manual(name = "", values = c("ENCAMINHADAS" = "#556B2F", "POSITIVAS" = "#FF6347")) +
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
             vjust = 0.1) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  Theme() +
  theme(axis.text.x = element_text(angle = 75))


##########################################################################################################

write.csv(RS22_2025_SE_Confirmados_CHIK, 
          "/home/gustavo/Área de Trabalho/Análise_de_Dados/Tabulacoes_R/Arboviroses/RS22_2025_SE_Confirmados_CHIK.csv",
          row.names = FALSE)

write.csv(RS22_2025_SE_Notificados_CHIK, 
          "/home/gustavo/Área de Trabalho/Análise_de_Dados/Tabulacoes_R/Arboviroses/RS22_2025_SE_Notificados_CHIK.csv",
          row.names = FALSE)

png(filename = "/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2025_INFORME_CHIK_1.png", 
    width = 36,
    height = 46,
    units = "cm", pointsize = 8, res = 300)

RS_2025_GRAF_CHIK_Histograma_Notificados_01

dev.off()

png(filename = "/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2025_INFORME_CHIK_2.png", 
    width = 36,
    height = 46,
    units = "cm", pointsize = 8, res = 300)

RS_2025_GRAF_CHIK_Histograma_Notificados_02

dev.off()

png(filename = "/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2025_INFORME_CHIK_3.png", 
    width = 36,
    height = 46,
    units = "cm", pointsize = 8, res = 300)

RS_2025_GRAF_CHIK_Histograma_Confirmados_01

dev.off()

png(filename = "/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2025_INFORME_CHIK_4.png", 
    width = 36,
    height = 46,
    units = "cm", pointsize = 8, res = 300)

RS_2025_GRAF_CHIK_Histograma_Confirmados_02

dev.off()

png(filename = "/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2025_INFORME_CHIK_5.png", 
    width = 36,
    height = 46,
    units = "cm", pointsize = 8, res = 300)

RS_2025_GRAF_CHIK_Histograma_Provaveis_01

dev.off()

png(filename = "/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2025_INFORME_CHIK_6.png", 
    width = 36,
    height = 46,
    units = "cm", pointsize = 8, res = 300)

RS_2025_GRAF_CHIK_Histograma_Provaveis_02

dev.off()

RS22_2025_GRAF_1 <- (RS22_2025_GRAF_SORO_CHIK_TOTAL / RS22_2025_GRAF_SORO_CHIK_REAG)
png(filename = "/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2025_INFORME_Pag_19B.png", 
    width = 36,
    height = 23,
    units = "cm", pointsize = 8, res = 300)

RS22_2025_GRAF_1

dev.off()

png(filename = "/home/gustavo/Área de Trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2025_INFORME_Pag_20.png", 
    width = 36,
    height = 23,
    units = "cm", pointsize = 8, res = 300)

RS22_GRAF_LACEN_MUNIC_CHIK

dev.off()