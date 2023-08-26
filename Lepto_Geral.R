rm(list =ls())

####Indicando Diretório de Trabalho.#####

setwd("/home/gustavo/Área de Trabalho/Análise_de_Dados/")

###Libraries###

library(foreign)
library (dplyr)
library(ggplot2)

####Planilha com os dados dos municípios e com os códigos do IBGE. Será utilizada nos for loops para buscar dados######## 
####dos municípios e vinculá-los com os dados da base DBF do SINAN#######################################################

BASE_IBGE<-read.csv(file="Base_de_Dados/Planilha_Base_IBGE.csv", 
                    header=TRUE, 
                    sep=",")

############################################################################################
####   Definindo o objeto RS para servir de apoio para    ##################################
####    buscar dados de todas as RS. Usar 1, 2, 3..., 21, 22    ############################
############################################################################################

RS <- 22   #####  Deve-se colocar AQUI a Regional

######   Criando objeto ID_REG. Será utilizado para selecionar
######   RS no DBF do SINAN ONLINE.

ID_REG <- as.data.frame(BASE_IBGE[which(BASE_IBGE$RS == RS), 6])

ID_REG <- as.numeric(ID_REG[1,1])

####   Estabelecendo o número de municípios em cada RS

nrow <- NROW(BASE_IBGE[which(BASE_IBGE$RS == RS), 1])

####Criando um objeto com a base DBF do SINAN#################

LEPTONET<- read.dbf(file = "Base_de_Dados/DBF/LEPTONET.DBF",
                       as.is = FALSE)

AUX <- LEPTONET %>% filter(NU_ANO != "2002" & 
                             NU_ANO != "2006" & 
                             NU_ANO != "2007" &
                             NU_ANO != "2008") %>% 
  count(NU_ANO)

AUX01 <- LEPTONET %>% filter(NU_ANO != "2002" & 
                             NU_ANO != "2006" & 
                             NU_ANO != "2007" &
                             NU_ANO != "2008",
                               CLASSI_FIN == 1) %>% 
  count(NU_ANO)

AUX$c <- AUX01$n

ggplot(AUX, aes(x= NU_ANO))+
  theme(axis.text.x = element_text(angle = 0, 
                                   vjust = 1,
                                   face = "bold"),
        axis.text.y = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 0.5,
                                   face = "bold"),
        panel.grid.major = element_line(color = "#C0C0C0"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#F5F5F5"),
        plot.title = element_text(face = "bold",
                                  size = 19,
                                  colour = "#556B2F")) +
  geom_bar(aes(y = n, fill = "Notificados"),
           stat = "identity",
           color = "black",
    #       fill = "green",
           width = .4,
           position = position_nudge(x = -.20)) +
  geom_bar(aes(y = c, fill = "Confirmados"),
    stat = "identity",
    color = "black",
    #fill = "green",
    width = .4,
    position = position_nudge(x = .20)) +
  labs(caption = "Fonte", 
       x = NULL,
       y = "Número de Casos",
       title = "Série Histórica - Paraná",
       subtitle = "Casos Notificados/2009 - 2023") +
  geom_label(aes(y = AUX$n,
                 label = AUX$n), 
             size = 3, 
             alpha = 0.5,
             nudge_x = -.20,
             vjust = 0.2) +
geom_label(aes(y = AUX$c, 
               label = AUX$c), 
           size = 3, 
           alpha = 0.5,
           nudge_x = .20,
           vjust = 0.2) +
  scale_fill_manual(name = "", values = c("Notificados" = "#556B2F", "Confirmados" = "#FF6347")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))
  
##################################################################################
#################   2009    ######################################################
##################################################################################

SINAN_LEPTO_2009 <- LEPTONET %>% filter(ID_REGIONA == ID_REG | ID_RG_RESI == ID_REG,
                                        NU_ANO == 2009)

assign(paste0("RS", RS, "_LEPTO_2009_SINAN"), SINAN_LEPTO_2009)

write.csv (assign(paste0("RS", RS, "_LEPTO_2009_SINAN"), SINAN_LEPTO_2009), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2009_SINAN.csv"), 
           row.names = FALSE)

################################################################################################################
############      Filtrando os dados por SE para elaborar o Canal Endêmico   ###################################

AUX <- matrix(data = NA, 
              nrow = nrow, 
              ncol = 54)

AUX <- as.data.frame(AUX)

colnames(AUX)[1] <- "Município" 

AUX[,1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

colnames (AUX)[2:54] <- c(1:53)

for (i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX == i), 2] <- as.integer(SINAN_LEPTO_2009 %>%
                                          filter(ID_MN_RESI ==i &
                                                 SEM_PRI ==200901)%>%
                                          count()
  )
  
  AUX[which(AUX == i), 3] <- as.integer(SINAN_LEPTO_2009 %>% 
                                          filter(ID_MN_RESI ==i & 
                                                 SEM_PRI ==200902) %>% 
                                          count()
  )
  
  AUX[which(AUX == i), 4] <- as.integer(SINAN_LEPTO_2009 %>% 
                                          filter(ID_MN_RESI ==i &
                                                 SEM_PRI ==200903) %>% 
                                          count()
  )
  
  AUX[which(AUX == i),5] <- as.integer(SINAN_LEPTO_2009 %>% 
                                         filter(ID_MN_RESI ==i &
                                                SEM_PRI ==200904) %>% 
                                         count()
  )
  
  AUX[which(AUX == i), 6] <- as.integer(SINAN_LEPTO_2009 %>% 
                                          filter(ID_MN_RESI ==i &
                                                 SEM_PRI ==200905) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 7] <- as.integer(SINAN_LEPTO_2009 %>%
                                          filter(ID_MN_RESI ==i & 
                                                 SEM_PRI ==200906) %>%
                                          count()
  )
  
  AUX[which(AUX == i), 8] <- as.integer(SINAN_LEPTO_2009 %>%
                                          filter(ID_MN_RESI ==i & 
                                                 SEM_PRI ==200907) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 9] <- as.integer(SINAN_LEPTO_2009 %>%
                                          filter(ID_MN_RESI ==i & 
                                                 SEM_PRI ==200908) %>% 
                                          count() 
  )
  
  AUX[which(AUX == i), 10] <- as.integer(SINAN_LEPTO_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                  SEM_PRI ==200909) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 11] <- as.integer(SINAN_LEPTO_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                  SEM_PRI ==200910) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 12] <- as.integer(SINAN_LEPTO_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==200911) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 13] <- as.integer(SINAN_LEPTO_2009 %>% 
                                           filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==200912) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 14] <- as.integer(SINAN_LEPTO_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                  SEM_PRI ==200913) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 15] <- as.integer(SINAN_LEPTO_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                  SEM_PRI ==200914) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 16] <- as.integer(SINAN_LEPTO_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                  SEM_PRI ==200915) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 17] <- as.integer(SINAN_LEPTO_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                  SEM_PRI ==200916) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 18] <- as.integer(SINAN_LEPTO_2009 %>%
                                           filter(ID_MN_RESI ==i & 
                                                  SEM_PRI ==200917) %>%  
                                           count() 
  )
  
  AUX[which(AUX == i), 19] <- as.integer(SINAN_LEPTO_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                  SEM_PRI ==200918) %>%      
                                           count() 
  )
  
  AUX[which(AUX == i), 20] <- as.integer(SINAN_LEPTO_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                  SEM_PRI ==200919) %>%
                                           count() 
  )
  
  AUX[which(AUX == i),  21] <- as.integer(SINAN_LEPTO_2009 %>% 
                                            filter(ID_MN_RESI ==i & 
                                                   SEM_PRI ==200920) %>%
                                            count() 
  )
  
  AUX[which(AUX == i), 22] <- as.integer(SINAN_LEPTO_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                  SEM_PRI ==200921) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 23] <- as.integer(SINAN_LEPTO_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                  SEM_PRI ==200922) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 24] <- as.integer(SINAN_LEPTO_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                  SEM_PRI ==200923) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 25] <- as.integer(SINAN_LEPTO_2009 %>%
                                           filter(ID_MN_RESI ==i & 
                                                  SEM_PRI ==200924) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 26] <- as.integer(SINAN_LEPTO_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                  SEM_PRI ==200925) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 27] <- as.integer(SINAN_LEPTO_2009 %>%
                                           filter(ID_MN_RESI ==i & 
                                                  SEM_PRI ==200926) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 28] <- as.integer(SINAN_LEPTO_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                  SEM_PRI ==200927) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 29] <- as.integer(SINAN_LEPTO_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                  SEM_PRI ==200928) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 30] <- as.integer(SINAN_LEPTO_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==200929) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 31] <- as.integer(SINAN_LEPTO_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==200930) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 32] <- as.integer(SINAN_LEPTO_2009 %>% 
                                           filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==200931) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 33] <- as.integer(SINAN_LEPTO_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==200932) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 34] <- as.integer(SINAN_LEPTO_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==200933) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 35] <- as.integer(SINAN_LEPTO_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==200934) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 36] <- as.integer(SINAN_LEPTO_2009 %>% 
                                           filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==200935) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 37] <- as.integer(SINAN_LEPTO_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==200936) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 38] <- as.integer(SINAN_LEPTO_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==200937) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 39] <- as.integer(SINAN_LEPTO_2009 %>% 
                                           filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==200938) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 40] <- as.integer(SINAN_LEPTO_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==200939) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 41] <- as.integer(SINAN_LEPTO_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==200940) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 42] <- as.integer(SINAN_LEPTO_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                  SEM_PRI ==200941) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 43] <- as.integer(SINAN_LEPTO_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                  SEM_PRI ==200942) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 44] <- as.integer(SINAN_LEPTO_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==200943) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 45] <- as.integer(SINAN_LEPTO_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==200944) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 46] <- as.integer(SINAN_LEPTO_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==200945) %>%
                                           count()
  )
  
  AUX[which(AUX == i), 47] <- as.integer(SINAN_LEPTO_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==200946) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 48] <- as.integer(SINAN_LEPTO_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==200947) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 49] <- as.integer(SINAN_LEPTO_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==200948) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 50] <- as.integer(SINAN_LEPTO_2009 %>%
                                           filter(ID_MN_RESI ==i & 
                                                  SEM_PRI ==200949) %>% 
                                           count()
  )
  
  AUX[which(AUX == i), 51] <- as.integer(SINAN_LEPTO_2009 %>% 
                                           filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==200950) %>% 
                                           count() 
  )
  
  AUX[which(AUX == i), 52] <- as.integer(SINAN_LEPTO_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==200951) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 53] <- as.integer(SINAN_LEPTO_2009 %>%
                                           filter(ID_MN_RESI ==i &
                                                  SEM_PRI ==200952) %>%
                                           count() 
  )
  
  AUX[which(AUX == i), 54] <- as.integer(SINAN_LEPTO_2009 %>% 
                                           filter(ID_MN_RESI ==i & 
                                                  SEM_PRI ==200953) %>%
                                           count() 
  )
}

AUX[, 1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX[(nrow(AUX)+ 1), 2:54] <- apply(AUX[, 2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2009_SE_Notificados"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2009_SE_Notificados"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2009_SE_Notificados.csv"), 
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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                   filter(ID_MN_RESI == i) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                          CS_ZONA == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                          CS_ZONA == 2) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                          CS_ZONA == 3) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                   filter(ID_MN_RESI ==i &
                                                          CS_ZONA == 9 
                                                          |
                                                            is.na(CS_ZONA)) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                           CS_SEXO == "F") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                           CS_SEXO == "M") %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                           NU_IDADE_N <=3012) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                    filter(ID_MN_RESI ==i &  
                                                           NU_IDADE_N > 4000 
                                                           & 
                                                             NU_IDADE_N <=4005) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                           NU_IDADE_N > 4005 
                                                           & 
                                                             NU_IDADE_N <=4012) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                           NU_IDADE_N > 4012 
                                                           & 
                                                             NU_IDADE_N <=4018) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 16] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                           NU_IDADE_N > 4018 
                                                           & 
                                                             NU_IDADE_N <=4059) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 17] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                           NU_IDADE_N > 4059) %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 18] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                           CS_ESCOL_N == "00") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 19] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                           CS_ESCOL_N == "01" 
                                                           | 
                                                             CS_ESCOL_N == "02" 
                                                           | 
                                                             CS_ESCOL_N == "03") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 20] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                           CS_ESCOL_N == "04") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 21] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                           CS_ESCOL_N == "05") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 22] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                           CS_ESCOL_N == "06") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 23] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                           CS_ESCOL_N == "07") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 24] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                           CS_ESCOL_N == "08") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 25] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                           CS_ESCOL_N == "10") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 26] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                           CS_ESCOL_N == "09") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 27] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                           CS_GESTANT == "1" 
                                                           |
                                                             CS_GESTANT== "2" 
                                                           | 
                                                             CS_GESTANT == "3" 
                                                           | 
                                                             CS_GESTANT == "4") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 28] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                           CS_GESTANT == "5") %>% 
                                                    count()
  )
  AUX[which(AUX$COD_IBGE == i), 29] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                           CS_GESTANT == "6") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 30] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                           CS_GESTANT == "9") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 31] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                           CS_RACA == "01") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 32] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                           CS_RACA == "2") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 33] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                           CS_RACA == "3") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 34] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                           CS_RACA == "4") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 35] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                           CS_RACA == "5") %>% 
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 36] <- as.integer(SINAN_LEPTO_2009 %>% 
                                                    filter(ID_MN_RESI ==i &
                                                           CS_RACA == "9") %>% 
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:36] <- apply(AUX[, 4:36], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_LEPTO_2009_GERAL"), AUX)

write.csv (assign(paste0("RS", RS, "_LEPTO_2009_GERAL"), AUX), 
           paste0("Base_de_Dados/Tabulacoes_R/Leptospirose/RS", RS, "_LEPTO_2009_GERAL.csv"), 
           row.names = FALSE)

