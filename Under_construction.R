RS_22_23_SE_Notificados <- RS22_22_23_SE_Notificados

AUX_GRAF <- as.data.frame(RS_22_23_SE_Notificados$Município)

AUX_GRAF[, 2] <- as.data.frame(RS_22_23_SE_Notificados[, which(colnames(RS_22_23_SE_Notificados) == SE) - 9])
AUX_GRAF[, 3] <- as.data.frame(RS_22_23_SE_Notificados[, which(colnames(RS_22_23_SE_Notificados) == SE) - 8])
AUX_GRAF[, 4] <- as.data.frame(RS_22_23_SE_Notificados[, which(colnames(RS_22_23_SE_Notificados) == SE) - 7])
AUX_GRAF[, 5] <- as.data.frame(RS_22_23_SE_Notificados[, which(colnames(RS_22_23_SE_Notificados) == SE) - 6])
AUX_GRAF[, 6] <- as.data.frame(RS_22_23_SE_Notificados[, which(colnames(RS_22_23_SE_Notificados) == SE) - 5])
AUX_GRAF[, 7] <- as.data.frame(RS_22_23_SE_Notificados[, which(colnames(RS_22_23_SE_Notificados) == SE) - 4])
AUX_GRAF[, 8] <- as.data.frame(RS_22_23_SE_Notificados[, which(colnames(RS_22_23_SE_Notificados) == SE) - 3])
AUX_GRAF[, 9] <- as.data.frame(RS_22_23_SE_Notificados[, which(colnames(RS_22_23_SE_Notificados) == SE) - 2])
AUX_GRAF[, 10] <- as.data.frame(RS_22_23_SE_Notificados[, which(colnames(RS_22_23_SE_Notificados) == SE) - 1])
AUX_GRAF[, 11] <- as.data.frame(RS_22_23_SE_Notificados[, which(colnames(RS_22_23_SE_Notificados) == SE)])

colnames(AUX_GRAF)[1] <- "Municipios"
colnames(AUX_GRAF)[2] <- colnames(RS_22_23_SE_Notificados)[which(colnames(RS_22_23_SE_Notificados) == SE) - 9]
colnames(AUX_GRAF)[3] <- colnames(RS_22_23_SE_Notificados)[which(colnames(RS_22_23_SE_Notificados) == SE) - 8]
colnames(AUX_GRAF)[4] <- colnames(RS_22_23_SE_Notificados)[which(colnames(RS_22_23_SE_Notificados) == SE) - 7]
colnames(AUX_GRAF)[5] <- colnames(RS_22_23_SE_Notificados)[which(colnames(RS_22_23_SE_Notificados) == SE) - 6]
colnames(AUX_GRAF)[6] <- colnames(RS_22_23_SE_Notificados)[which(colnames(RS_22_23_SE_Notificados) == SE) - 5]
colnames(AUX_GRAF)[7] <- colnames(RS_22_23_SE_Notificados)[which(colnames(RS_22_23_SE_Notificados) == SE) - 4]
colnames(AUX_GRAF)[8] <- colnames(RS_22_23_SE_Notificados)[which(colnames(RS_22_23_SE_Notificados) == SE) - 3]
colnames(AUX_GRAF)[9] <- colnames(RS_22_23_SE_Notificados)[which(colnames(RS_22_23_SE_Notificados) == SE) - 2]
colnames(AUX_GRAF)[10] <- colnames(RS_22_23_SE_Notificados)[which(colnames(RS_22_23_SE_Notificados) == SE) - 1]
colnames(AUX_GRAF)[11] <- colnames(RS_22_23_SE_Notificados)[which(colnames(RS_22_23_SE_Notificados) == SE)]

AUX_GRAF[nrow(AUX_GRAF),] <- colnames(AUX_GRAF)

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

Theme_Hist <- function(){ 
  theme_minimal(base_size = 14) %+replace%    #
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

AUX_GG_LIST <- AUX_GRAF %>%
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
        caption = "Fonte", 
        x = "Semana Epidemiológica",
        y = "Número de Casos",
        title = titulo
      ) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
      Theme_Hist()
  })

# needed because that's the result AUX_GG_LIST order
# (given by dplyr::group_split)
old_loc <- Sys.getlocale("LC_COLLATE")
Sys.setlocale("LC_COLLATE", "C")
AUX_GG_LIST <- setNames(AUX_GG_LIST, names(AUX_GRAF)[-1] |> sort())
Sys.setlocale("LC_COLLATE", old_loc)

# the plots are all different, for instance the 1st and the 3rd
AUX_GG_LIST[[9]]

while()