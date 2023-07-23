###########################   Elaboração dos Mapas baseado nas tabelas do google drive  ################################
########################################################################################################################
#rm(list = ls())
####################   Elaborando um objeto MAPA_BASE para ser utilizado na criação dos mapas   ########################

MAPA_BASE <- read_municipality(code_muni = "PR", year = 2020)

MAPA_BASE$name_muni <- toupper(MAPA_BASE$name_muni)

########################################################################################################################
############################   Mapa regional de sorotipo circulante     ################################################
############################   Realizar left_join com a tabela que há interesse em criar a informação  #################

MAPA_BASE_22RS <- left_join(MAPA_BASE, RS22_22_23_GERAL, 
                                by = c("name_muni" = "Município")
                                )

MAPA_BASE_22RS <- MAPA_BASE_22RS %>% filter(RS == 22)

AUX_MAP <- MAPA_BASE_22RS %>% filter(Sorotipos == "I,")
AUX_MAP_1 <- MAPA_BASE_22RS %>% filter(Sorotipos == "II,")
AUX_MAP_2 <- MAPA_BASE_22RS %>% filter(Sorotipos == "III,")
AUX_MAP_3 <- MAPA_BASE_22RS %>% filter(Sorotipos == "I,II,")
AUX_MAP_4 <- MAPA_BASE_22RS %>% filter(Sorotipos == "I,II,III,")
AUX_MAP_4 <- MAPA_BASE_22RS %>% filter(Sorotipos == "II,III,")

ggplot() + 
  geom_sf(data = MAPA_BASE_22RS, 
          color = "black", 
          fill = "white") + 
  geom_sf(data = AUX_MAP, 
          color ="black", 
          aes(fill = "I")) +  
  geom_sf(data = AUX_MAP_1, 
          color ="black", 
          aes(fill = "II")) + 
  geom_sf(data = AUX_MAP_2, 
          color ="black", 
          aes(fill = "III")) + 
  geom_sf(data = AUX_MAP_3, 
          color ="black", 
          aes(fill = "I, II")) + 
  annotation_scale(location = "br") +
  annotation_north_arrow(location = "tl", 
                         which_north = "true") +
  theme_minimal() +
  scale_fill_manual (name = "Sorotipos",
                     values = c("I" = "#F0E68C", 
                                "II" = "#FF0000",
                                "I, II" = "black")) +
  theme(legend.position = "bottom") +
  labs(caption = "Fonte", 
       title = "Sorotipo Circulante - 22ªRS") + 
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold", 
                                   size = 14,
                                   colour = "#556B2F")
         ) 

#########################################################################################################################
#########################   Mapa Chikungunya notificados REGIONAL   #####################################################

MAPA_BASE_22RS <- left_join(MAPA_BASE, PR_22_23_CHIKUNGUNYA_MUNICIPIOS, 
                            by = c("name_muni" = "MUNICÍPIO")
)

MAPA_BASE_22RS <- MAPA_BASE_22RS %>% filter(RS == 22)

AUX_MAP <- MAPA_BASE_22RS %>% filter(NOTIFICADOS != 0)

ggplot() + 
  geom_sf(data = MAPA_BASE_22RS, 
          color = "black", 
          fill = "white") + 
  geom_sf(data = AUX_MAP, 
          color ="black", 
          aes(fill = NOTIFICADOS)) + 
  annotation_scale(location = "br") +
  annotation_north_arrow(location = "tl", 
                         which_north = "true") +
  theme_minimal() +
  scale_fill_manual (name = "Notificados",
                     values = c(NOTIFICADOS = "#F0E68C")
                     ) +
  theme(legend.position = "bottom") +
  labs(caption = "Fonte", 
       title = "Chikungunya Notificados - 22ªRS") + 
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold", 
                                   size = 14,
                                   colour = "#556B2F")
  ) 

#########################################################################################################################
#########################   Mapa Chikungunya Confirmados REGIONAL   #####################################################

MAPA_BASE_22RS <- left_join(MAPA_BASE, PR_22_23_CHIKUNGUNYA_MUNICIPIOS, 
                            by = c("name_muni" = "MUNICÍPIO")
)

MAPA_BASE_22RS <- MAPA_BASE_22RS %>% filter(RS == 22)

AUX_MAP <- MAPA_BASE_22RS %>% filter(CONFIRMADOS != 0)

ggplot() + 
  geom_sf(data = MAPA_BASE_22RS, 
          color = "black", 
          fill = "white") + 
  geom_sf(data = AUX_MAP, 
          color ="black", 
          aes(fill = "Notificados")) + 
  annotation_scale(location = "br") +
  annotation_north_arrow(location = "tl", 
                         which_north = "true") +
  theme_minimal() +
  scale_fill_manual (name = "Notificados",
                     values = c("Notificados" = "#F0E68C")
  ) +
  theme(legend.position = "bottom") +
  labs(caption = "Fonte", 
       title = "Chikungunya Notificados - 22ªRS") + 
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold", 
                                   size = 14,
                                   colour = "#556B2F")
  ) 

#########################################################################################################################
#########################   Mapa IIP REGIONAL  4º Ciclo/2022     ########################################################

MAPA_BASE_22RS <- left_join(MAPA_BASE, RS22_22_23_CICLOS_MUNICIPIOS, 
                            by = c("name_muni" = "Município")
)

AUX_MAP <- MAPA_BASE_22RS %>% filter(`4CICLO_2022_IIP` > 199)

AUX_MAP$Cat  <- "S/I"

MAPA_BASE_22RS <- MAPA_BASE_22RS %>% filter(`4CICLO_2022_IIP` >= 0 & `4CICLO_2022_IIP` < 200)

MAPA_BASE_22RS$Cat <- with(MAPA_BASE_22RS, cut(x = `4CICLO_2022_IIP`,
                                           breaks = c(-Inf, 1, 4, Inf),
                                           labels = c("0 - 0,9", "1 - 3,9", 
                                                      "> 4"))
)

MAPA_BASE_22RS <-rbind(MAPA_BASE_22RS, AUX_MAP)

ggplot() + 
  geom_sf(data = MAPA_BASE_22RS, 
          color = "black", 
          aes(fill = Cat)) + 
  theme_minimal() +
  scale_fill_manual (name = "",
                     values = c("0 - 0,9" = "green",
                                "1 - 3,9" = "yellow",
                                "> 4" = "red",
                                "S/I" = "white")
  ) +
   annotation_scale(location = "br") +
  annotation_north_arrow(location = "tr", 
                         which_north = "true") +
   theme(legend.position = "right") +
  labs(caption = "Fonte", 
       title = "Índice de Infestação Predial 4º Ciclo/2022 - 22ªRS") + 
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold", 
                                   size = 14,
                                   colour = "#556B2F")
  ) 

#########################################################################################################################
#########################   Mapa IIP REGIONAL  5º Ciclo/2022     ########################################################

MAPA_BASE_22RS <- left_join(MAPA_BASE, RS22_22_23_CICLOS_MUNICIPIOS, 
                            by = c("name_muni" = "Município")
)

AUX_MAP <- MAPA_BASE_22RS %>% filter(`5ºCICLO_IIP` > 199)

AUX_MAP$Cat  <- "S/I"

MAPA_BASE_22RS <- MAPA_BASE_22RS %>% filter(`5ºCICLO_IIP` >= 0 & `5ºCICLO_IIP` < 200)

MAPA_BASE_22RS$Cat <- with(MAPA_BASE_22RS, cut(x = `5ºCICLO_IIP`,
                                               breaks = c(-Inf, 1, 4, Inf),
                                               labels = c("0 - 0,9", "1 - 3,9", 
                                                          "> 4"))
)

MAPA_BASE_22RS <-rbind(MAPA_BASE_22RS, AUX_MAP)

ggplot() + 
  geom_sf(data = MAPA_BASE_22RS, 
          color = "black", 
          aes(fill = Cat)) + 
  theme_minimal() +
  scale_fill_manual (name = "",
                     values = c("0 - 0,9" = "green",
                                "1 - 3,9" = "yellow",
                                "> 4" = "red",
                                "S/I" = "white")
  ) +
  annotation_scale(location = "br") +
  annotation_north_arrow(location = "tr", 
                         which_north = "true") +
  theme(legend.position = "right") +
  labs(caption = "Fonte", 
       title = "Índice de Infestação Predial 4º Ciclo/2022 - 22ªRS") + 
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold", 
                                   size = 14,
                                   colour = "#556B2F")
  ) 

#########################################################################################################################
#########################   Mapa IIP REGIONAL  6º Ciclo/2022     ########################################################

MAPA_BASE_22RS <- left_join(MAPA_BASE, RS22_22_23_CICLOS_MUNICIPIOS, 
                            by = c("name_muni" = "Município")
)

AUX_MAP <- MAPA_BASE_22RS %>% filter(`6ºCICLO_IIP` > 199)

AUX_MAP$Cat  <- "S/I"

MAPA_BASE_22RS <- MAPA_BASE_22RS %>% filter(`6ºCICLO_IIP` >= 0 & `6ºCICLO_IIP` < 200)

MAPA_BASE_22RS$Cat <- with(MAPA_BASE_22RS, cut(x = `6ºCICLO_IIP`,
                                               breaks = c(-Inf, 1, 4, Inf),
                                               labels = c("0 - 0,9", "1 - 3,9", 
                                                          "> 4"))
)

MAPA_BASE_22RS <-rbind(MAPA_BASE_22RS, AUX_MAP)

ggplot() + 
  geom_sf(data = MAPA_BASE_22RS, 
          color = "black", 
          aes(fill = Cat)) + 
  theme_minimal() +
  scale_fill_manual (name = "",
                     values = c("0 - 0,9" = "green",
                                "1 - 3,9" = "yellow",
                                "> 4" = "red",
                                "S/I" = "white")
  ) +
  annotation_scale(location = "br") +
  annotation_north_arrow(location = "tr", 
                         which_north = "true") +
  theme(legend.position = "right") +
  labs(caption = "Fonte", 
       title = "Índice de Infestação Predial 4º Ciclo/2022 - 22ªRS") + 
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold", 
                                   size = 14,
                                   colour = "#556B2F")
  ) 

