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

####Criando um objeto com a base DBF do SINAN#################

LEPTONET<- read.dbf(file = "Base_de_Dados/DBF/LEPTONET.DBF",
                       as.is = FALSE)
AUX <- LEPTONET %>% count(NU_ANO)

ggplot(AUX, aes(x= NU_ANO, y = n))+
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
  geom_bar(
           stat = "identity",
           color = "black",
           fill = "green") +
  labs(caption = "Fonte", 
       x = NULL,
       y = "Número de Casos",
       title = "Série Histórica - Paraná",
       subtitle = "Casos Notificados/2012 - 2023") +
  geom_label(aes(label = AUX$n), 
             size = 3, 
             alpha = 0.5,
             vjust = 0.2) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))
  
