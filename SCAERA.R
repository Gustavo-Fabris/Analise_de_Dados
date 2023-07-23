library (dplyr)
library (read.dbc)

BASE_IBGE<-read.table(file="/home/gustavo/Área de Trabalho/Análise_de_Dados/Base_de_Dados/Planilha_Base_IBGE.csv", 
                      header=TRUE, 
                      sep=",")

BASE_IBGE_BRASIL <- read.csv (file = "/home/gustavo/Área de Trabalho/Análise_de_Dados/Base_de_Dados/Planilha_Base_IBGE_BRASIL.csv",
                              header = TRUE,
                              sep = ",")

SCAERA <- read.dbc("/home/gustavo/Área de Trabalho/RDPR2303.dbc")

#write.csv(SCAERA, 
#          "/home/gustavo/Área de Trabalho/SCAERA.csv",
#          row.names = FALSE)

SCAERA22RS_2023 <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == 22), 3])

SCAERA22RS_2023$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == 22), 2]

SCAERA22RS_2023$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == 22), 5]

SCAERA22RS_2023$RS <- BASE_IBGE[which(BASE_IBGE$RS == 22), 1]

SCAERA22RS_2023 <- SCAERA22RS_2023[,c(4, 1, 2, 3)]

SCAERA22RS_2023$Notificados <- NA

SCAERA22RS_2023$Dengue <- NA

SCAERA22RS_2023$D_S_A <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == 22)), 2]){
  
  ###Notiicações###  
  SCAERA22RS_2023[which(SCAERA22RS_2023$COD_IBGE == i), 5] <- as.integer(RS22_22_23_SINAN %>% 
                                                                     filter(ID_MN_RESI == i) %>%   
                                                                     count()
  )    
  
  ###Dengue###
  
  SCAERA22RS_2023[which(SCAERA22RS_2023$COD_IBGE == i), 6] <-as.integer(RS22_22_23_SINAN %>% 
                                                                          filter(CLASSI_FIN == 10, 
                                                                                 ID_MN_RESI == i) %>%
                                                                          count() 
  )
  ###D.S.A.###
  
  SCAERA22RS_2023[which(SCAERA22RS_2023$COD_IBGE == i), 7] <- as.integer(RS22_22_23_SINAN %>%  
                                                                           filter(CLASSI_FIN == 11, 
                                                                                  ID_MN_RESI == i) %>% 
                                                                           count()
  )
}
  
