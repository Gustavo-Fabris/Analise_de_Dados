CHIKON2022 <- read.dbf("/home/gustavo/Área de Trabalho/Análise_de_Dados/Base_de_Dados/Arboviroses/DBF/CHIKON2022.dbf",
                       as.is = FALSE) %>% select(ID_REGIONA, NU_NOTIFIC, ID_GEO1,ID_GEO2, ID_AGRAVO, ID_REGIONA, DT_NOTIFIC, NU_ANO, SEM_NOT, DT_SIN_PRI, SEM_PRI,  SG_UF_NOT, ID_MUNICIP, NM_PACIENT, DT_NASC, NU_IDADE_N, CS_SEXO, CS_GESTANT, CS_ESCOL_N, NM_MAE_PAC, ID_MN_RESI, SG_UF, ID_RG_RESI, NM_LOGRADO, NU_NUMERO, NM_BAIRRO, NU_CEP, CS_ZONA, DT_DIGITA, DT_INVEST, FEBRE, MIALGIA, CEFALEIA, EXANTEMA, VOMITO, NAUSEA, DOR_COSTAS, CONJUNTVIT, ARTRITE, ARTRALGIA, PETEQUIA_N, LEUCOPENIA, LACO, DOR_RETRO, DIABETES, HEMATOLOG, HEPATOPAT, RENAL, HIPERTENSA, ACIDO_PEPT, AUTO_IMUNE, DT_SORO, RESUL_SORO, DT_PCR, RESUL_PCR_, SOROTIPO, CLASSI_FIN, CRITERIO, TPAUTOCTO, COUFINF, COMUNINF, CO_BAINF, EVOLUCAO, HOSPITALIZ, DT_INTERNA, DT_OBITO, DT_ENCERRA, DT_ALRM, ALRM_LETAR, ALRM_HEPAT, ALRM_LIQ, ALRM_HIPOT, ALRM_PLAQ, ALRM_VOM, ALRM_SANG, ALRM_HEMAT, ALRM_ABDOM, DT_GRAV, GRAV_PULSO, GRAV_CONV, GRAV_ENCH, GRAV_INSUF, GRAV_TAQUI, GRAV_EXTRE, GRAV_HIPOT, GRAV_HEMAT, GRAV_MELEN, GRAV_METRO, GRAV_SANG, GRAV_AST, GRAV_MIOC, GRAV_CONSC, GRAV_ORGAO, MANI_HEMOR, EPISTAXE, GENGIVO, METRO, DS_OBS)

CHIKON2023 <- read.dbf("/home/gustavo/Área de Trabalho/Análise_de_Dados/Base_de_Dados/Arboviroses/DBF/CHIKON2023.dbf",
                       as.is = FALSE) %>% select(ID_REGIONA, NU_NOTIFIC, ID_GEO1,ID_GEO2, ID_AGRAVO, ID_REGIONA, DT_NOTIFIC, NU_ANO, SEM_NOT, DT_SIN_PRI, SEM_PRI,  SG_UF_NOT, ID_MUNICIP, NM_PACIENT, DT_NASC, NU_IDADE_N, CS_SEXO, CS_GESTANT, CS_ESCOL_N, NM_MAE_PAC, ID_MN_RESI, SG_UF, ID_RG_RESI, NM_LOGRADO, NU_NUMERO, NM_BAIRRO, NU_CEP, CS_ZONA, DT_DIGITA, DT_INVEST, FEBRE, MIALGIA, CEFALEIA, EXANTEMA, VOMITO, NAUSEA, DOR_COSTAS, CONJUNTVIT, ARTRITE, ARTRALGIA, PETEQUIA_N, LEUCOPENIA, LACO, DOR_RETRO, DIABETES, HEMATOLOG, HEPATOPAT, RENAL, HIPERTENSA, ACIDO_PEPT, AUTO_IMUNE, DT_SORO, RESUL_SORO, DT_PCR, RESUL_PCR_, SOROTIPO, CLASSI_FIN, CRITERIO, TPAUTOCTO, COUFINF, COMUNINF, CO_BAINF, EVOLUCAO, HOSPITALIZ, DT_INTERNA, DT_OBITO, DT_ENCERRA, DT_ALRM, ALRM_LETAR, ALRM_HEPAT, ALRM_LIQ, ALRM_HIPOT, ALRM_PLAQ, ALRM_VOM, ALRM_SANG, ALRM_HEMAT, ALRM_ABDOM, DT_GRAV, GRAV_PULSO, GRAV_CONV, GRAV_ENCH, GRAV_INSUF, GRAV_TAQUI, GRAV_EXTRE, GRAV_HIPOT, GRAV_HEMAT, GRAV_MELEN, GRAV_METRO, GRAV_SANG, GRAV_AST, GRAV_MIOC, GRAV_CONSC, GRAV_ORGAO, MANI_HEMOR, EPISTAXE, GENGIVO, METRO, DS_OBS)


RS22_22_23_AUX01 <- CHIKON2022 %>% 
  filter(SEM_PRI >= 202231)

RS22_22_23_AUX02 <- CHIKON2023 %>% 
  filter(SEM_PRI <=202330)

RS22_22_23_SINAN_CHIK <- rbind(RS22_22_23_AUX01, RS22_22_23_AUX02)

rm(RS22_22_23_AUX01, RS22_22_23_AUX02)

RS22_22_23_SINAN_DECODIFICADO_CHIK <- RS22_22_23_SINAN_CHIK
                        
RS22_22_23_SINAN_DECODIFICADO_CHIK$ID_AGRAVO <- factor(RS22_22_23_SINAN_DECODIFICADO_CHIK$ID_AGRAVO,
                                                  label = c("Dengue", "Chikungunya"), 
                                                  levels = c("A90", "A92.0")
)

###Sintomas###
RS22_22_23_SINAN_DECODIFICADO_CHIK$FEBRE <- factor(RS22_22_23_SINAN_DECODIFICADO_CHIK$FEBRE,
                                              label = c("SIM", "NÃO"), 
                                              levels = c(1, 2)
)

RS22_22_23_SINAN_DECODIFICADO_CHIK$MIALGIA <- factor(RS22_22_23_SINAN_DECODIFICADO_CHIK$MIALGIA,
                                                label = c("SIM", "NÃO"), 
                                                levels = c(1, 2)
)

RS22_22_23_SINAN_DECODIFICADO_CHIK$CEFALEIA <- factor(RS22_22_23_SINAN_DECODIFICADO_CHIK$CEFALEIA,
                                                 label = c("SIM", "NÃO"), 
                                                 levels = c(1, 2)
)

RS22_22_23_SINAN_DECODIFICADO_CHIK$EXANTEMA <- factor(RS22_22_23_SINAN_DECODIFICADO_CHIK$EXANTEMA,
                                                 label = c("SIM", "NÃO"), 
                                                 levels = c(1, 2)
)

RS22_22_23_SINAN_DECODIFICADO_CHIK$VOMITO <- factor(RS22_22_23_SINAN_DECODIFICADO_CHIK$VOMITO,
                                               label = c("SIM", "NÃO"), 
                                               levels = c(1, 2)
)

RS22_22_23_SINAN_DECODIFICADO_CHIK$NAUSEA <- factor(RS22_22_23_SINAN_DECODIFICADO_CHIK$NAUSEA,
                                               label = c("SIM", "NÃO"), 
                                               levels = c(1, 2)
)

RS22_22_23_SINAN_DECODIFICADO_CHIK$DOR_COSTAS <- factor(RS22_22_23_SINAN_DECODIFICADO_CHIK$DOR_COSTAS,
                                                   label = c("SIM", "NÃO"), 
                                                   levels = c(1, 2)
)

RS22_22_23_SINAN_DECODIFICADO_CHIK$DOR_RETRO <- factor(RS22_22_23_SINAN_DECODIFICADO_CHIK$DOR_RETRO,
                                                  label = c("SIM", "NÃO"), 
                                                  levels = c(1, 2)
)

RS22_22_23_SINAN_DECODIFICADO_CHIK$CONJUNTVIT <- factor(RS22_22_23_SINAN_DECODIFICADO_CHIK$CONJUNTVIT,
                                                   label = c("SIM", "NÃO"), 
                                                   levels = c(1, 2)
)

RS22_22_23_SINAN_DECODIFICADO_CHIK$ARTRALGIA <- factor(RS22_22_23_SINAN_DECODIFICADO_CHIK$ARTRALGIA,
                                                  label = c("SIM", "NÃO"), 
                                                  levels = c(1, 2)
)

RS22_22_23_SINAN_DECODIFICADO_CHIK$ARTRITE <- factor(RS22_22_23_SINAN_DECODIFICADO_CHIK$ARTRITE,
                                                label = c("SIM", "NÃO"), 
                                                levels = c(1, 2)
)

RS22_22_23_SINAN_DECODIFICADO_CHIK$PETEQUIA_N <- factor(RS22_22_23_SINAN_DECODIFICADO_CHIK$PETEQUIA_N,
                                                   label = c("SIM", "NÃO"), 
                                                   levels = c(1, 2)
)

RS22_22_23_SINAN_DECODIFICADO_CHIK$LEUCOPENIA <- factor(RS22_22_23_SINAN_DECODIFICADO_CHIK$LEUCOPENIA,
                                                   label = c("SIM", "NÃO"), 
                                                   levels = c(1, 2)
)

RS22_22_23_SINAN_DECODIFICADO_CHIK$LACO <- factor(RS22_22_23_SINAN_DECODIFICADO_CHIK$LACO,
                                             label = c("SIM", "NÃO"), 
                                             levels = c(1, 2)
)

###Doenças Pré-existentes

RS22_22_23_SINAN_DECODIFICADO_CHIK$DIABETES <- factor(RS22_22_23_SINAN_DECODIFICADO_CHIK$DIABETES,
                                                 label = c("SIM", "NÃO"), 
                                                 levels = c(1, 2)
)

RS22_22_23_SINAN_DECODIFICADO_CHIK$HEMATOLOG <- factor(RS22_22_23_SINAN_DECODIFICADO_CHIK$HEMATOLOG,
                                                  label = c("SIM", "NÃO"), 
                                                  levels = c(1, 2)
)

RS22_22_23_SINAN_DECODIFICADO_CHIK$HEPATOPAT <- factor(RS22_22_23_SINAN_DECODIFICADO_CHIK$HEPATOPAT,
                                                  label = c("SIM", "NÃO"), 
                                                  levels = c(1, 2)
)

RS22_22_23_SINAN_DECODIFICADO_CHIK$RENAL <- factor(RS22_22_23_SINAN_DECODIFICADO_CHIK$RENAL,
                                              label = c("SIM", "NÃO"), 
                                              levels = c(1, 2)
)

RS22_22_23_SINAN_DECODIFICADO_CHIK$HIPERTENSA <- factor(RS22_22_23_SINAN_DECODIFICADO_CHIK$HIPERTENSA,
                                                   label = c("SIM", "NÃO"), 
                                                   levels = c(1, 2)
)

RS22_22_23_SINAN_DECODIFICADO_CHIK$ACIDO_PEPT <- factor(RS22_22_23_SINAN_DECODIFICADO_CHIK$ACIDO_PEPT,
                                                   label = c("SIM", "NÃO"), 
                                                   levels = c(1, 2)
)

RS22_22_23_SINAN_DECODIFICADO_CHIK$AUTO_IMUNE <- factor(RS22_22_23_SINAN_DECODIFICADO_CHIK$AUTO_IMUNE,
                                                   label = c("SIM", "NÃO"), 
                                                   levels = c(1, 2)
)

####Outros####

RS22_22_23_SINAN_DECODIFICADO_CHIK$CS_GESTANT <- factor(RS22_22_23_SINAN_DECODIFICADO_CHIK$CS_GESTANT,
                                                   label = c("1_TRI", "2_TRI", "3_TRI", "IDADE GESTACIONAL IGN", "NÃO", "NÃO SE APLICA", "IGNORADO"), 
                                                   levels = c(1, 2, 3, 4, 5, 6, 9)
)

RS22_22_23_SINAN_DECODIFICADO_CHIK$CS_ESCOL_N <- factor(RS22_22_23_SINAN_DECODIFICADO_CHIK$CS_ESCOL_N,
                                                   label = c("ANALFABETO", "1 a 4 SÉRIE DO FUNDAMENTAL INCOMPLETA", "4 SÉRIE DO FUNDAMENTAL COMPLETA", "5 a 8 SÉRIE DO FUNDAMENTAL INCOMPLETA", "FUNDAMENTAL COMPLETO", "ENSINO MÉDIO INCOMPLETO", "ENSINO MÉDIO COMPLETO", "SUPERIOR INCONPLETO", "SUPERIOR COMPLETO", "IGNORADO", "NÃO SE APLICA"), 
                                                   levels = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
)

RS22_22_23_SINAN_DECODIFICADO_CHIK$RESUL_SORO <- factor(RS22_22_23_SINAN_DECODIFICADO_CHIK$RESUL_SORO,
                                                   label = c("REAGENTE", "NÃO REAGENTE", "INCONCLUSIVO", "NÃO REALIZADO"), 
                                                   levels = c(1, 2, 3, 4)
)


RS22_22_23_SINAN_DECODIFICADO_CHIK$RESUL_PCR_ <- factor(RS22_22_23_SINAN_DECODIFICADO_CHIK$RESUL_PCR_,
                                                   label = c("DETECTÁVEL", "NÃO DETECTÀVEL", "INCONCLUSIVO", "NÃO REALIZADO"), 
                                                   levels = c(1, 2, 3, 4)
)

RS22_22_23_SINAN_DECODIFICADO_CHIK$SOROTIPO <- factor(RS22_22_23_SINAN_DECODIFICADO_CHIK$SOROTIPO,
                                                 label = c("I", "II", "III", "IV"), 
                                                 levels = c(1, 2, 3, 4)
)

RS22_22_23_SINAN_DECODIFICADO_CHIK$CLASSI_FIN <- factor(RS22_22_23_SINAN_DECODIFICADO_CHIK$CLASSI_FIN,
                                                   label = c("DESCARTADO", "DENGUE", "D.S.A.", "IDENGUE_GRAVE", "CHIKUNGUNYA"), 
                                                   levels = c(5, 10, 11, 12, 13)
)

RS22_22_23_SINAN_DECODIFICADO_CHIK$CRITERIO <- factor(RS22_22_23_SINAN_DECODIFICADO_CHIK$CRITERIO,
                                                 label = c("LABORATORIAL", "CLÍNICO-EPIDEMIOLÓGICO", "EM INVESTIGAÇÃO"), 
                                                 levels = c(1, 2, 3)
)

RS22_22_23_SINAN_DECODIFICADO_CHIK$TPAUTOCTO <- factor(RS22_22_23_SINAN_DECODIFICADO_CHIK$TPAUTOCTO,
                                                  label = c("SIM", "NÃO"), 
                                                  levels = c(1, 2)
)

RS22_22_23_SINAN_DECODIFICADO_CHIK$HOSPITALIZ <- factor(RS22_22_23_SINAN_DECODIFICADO_CHIK$HOSPITALIZ,
                                                   label = c("SIM", "NÃO"), 
                                                   levels = c(1, 2)
)

RS22_22_23_SINAN_DECODIFICADO_CHIK$EVOLUCAO <- factor(RS22_22_23_SINAN_DECODIFICADO_CHIK$EVOLUCAO,
                                                 label = c("CURA", "ÓBITO PELO AGRAVO", "ÓBITO POR OUTRAS CAUSAS","ÓBITO EM INVESTIGAÇÃO", "INDETERMINADO"), 
                                                 levels = c(1, 2, 3, 4, 9)
)

RS22_22_23_SINAN_DECODIFICADO_CHIK$CS_ZONA <- factor(RS22_22_23_SINAN_DECODIFICADO_CHIK$CS_ZONA,
                                                label = c("URBANA", "RURAL", "PERIURBANA","INDETERMINADO"), 
                                                levels = c(1, 2, 3, 9)
)

RS22_22_23_SINAN_DECODIFICADO_CHIK$ALRM_LETAR <- factor(RS22_22_23_SINAN_DECODIFICADO_CHIK$ALRM_LETAR,
                                                   label = c("SIM", "NÃO"), 
                                                   levels = c(1, 2)
)

RS22_22_23_SINAN_DECODIFICADO_CHIK$ALRM_HEPAT <- factor(RS22_22_23_SINAN_DECODIFICADO_CHIK$ALRM_HEPAT,
                                                   label = c("SIM", "NÃO"), 
                                                   levels = c(1, 2)
)

RS22_22_23_SINAN_DECODIFICADO_CHIK$ALRM_LIQ <- factor(RS22_22_23_SINAN_DECODIFICADO_CHIK$ALRM_LIQ,
                                                 label = c("SIM", "NÃO"), 
                                                 levels = c(1, 2)
)

RS22_22_23_SINAN_DECODIFICADO_CHIK$ALRM_HIPOT <- factor(RS22_22_23_SINAN_DECODIFICADO_CHIK$ALRM_HIPOT,
                                                   label = c("SIM", "NÃO"), 
                                                   levels = c(1, 2)
)

RS22_22_23_SINAN_DECODIFICADO_CHIK$ALRM_PLAQ <- factor(RS22_22_23_SINAN_DECODIFICADO_CHIK$ALRM_PLAQ,
                                                  label = c("SIM", "NÃO"), 
                                                  levels = c(1, 2)
)

RS22_22_23_SINAN_DECODIFICADO_CHIK$ALRM_VOM <- factor(RS22_22_23_SINAN_DECODIFICADO_CHIK$ALRM_VOM,
                                                 label = c("SIM", "NÃO"), 
                                                 levels = c(1, 2)
)

RS22_22_23_SINAN_DECODIFICADO_CHIK$ALRM_SANG <- factor(RS22_22_23_SINAN_DECODIFICADO_CHIK$ALRM_SANG,
                                                  label = c("SIM", "NÃO"), 
                                                  levels = c(1, 2)
)

RS22_22_23_SINAN_DECODIFICADO_CHIK$ALRM_HEMAT <- factor(RS22_22_23_SINAN_DECODIFICADO_CHIK$ALRM_LETAR,
                                                   label = c("SIM", "NÃO"), 
                                                   levels = c(1, 2)
)

RS22_22_23_SINAN_DECODIFICADO_CHIK$ALRM_ABDOM <- factor(RS22_22_23_SINAN_DECODIFICADO_CHIK$ALRM_ABDOM,
                                                   label = c("SIM", "NÃO"), 
                                                   levels = c(1, 2)
)
####RS22_22_23_SINAN_DECODIFICADO_CHIK$Municipio 

RS22_22_23_SINAN_DECODIFICADO_CHIK_AUX <- data.frame(COD = RS22_22_23_SINAN_DECODIFICADO_CHIK[,12], 
                                                Municipio = NA)

for (i in RS22_22_23_SINAN_DECODIFICADO_CHIK[,12]){
  RS22_22_23_SINAN_DECODIFICADO_CHIK_AUX[which(RS22_22_23_SINAN_DECODIFICADO_CHIK_AUX$COD == i), 2] <- BASE_IBGE_BRASIL[which(BASE_IBGE_BRASIL$Código.Município.Completo == i),13]
  
}

RS22_22_23_SINAN_DECODIFICADO_CHIK[,12] <- RS22_22_23_SINAN_DECODIFICADO_CHIK_AUX[, 2]

####Município de Residência

RS22_22_23_SINAN_DECODIFICADO_CHIK_AUX <- data.frame(COD = RS22_22_23_SINAN_DECODIFICADO_CHIK[,20], 
                                                Municipio = NA)

for (i in RS22_22_23_SINAN_DECODIFICADO_CHIK[,20]){
  RS22_22_23_SINAN_DECODIFICADO_CHIK_AUX[which(RS22_22_23_SINAN_DECODIFICADO_CHIK_AUX$COD == i), 2] <- BASE_IBGE_BRASIL[which(BASE_IBGE_BRASIL$Código.Município.Completo == i),13]
  
}

RS22_22_23_SINAN_DECODIFICADO_CHIK[,20] <- RS22_22_23_SINAN_DECODIFICADO_CHIK_AUX[, 2]

rm (RS22_22_23_SINAN_DECODIFICADO_CHIK_AUX)

colnames(RS22_22_23_SINAN_DECODIFICADO_CHIK)<- c("RS", "SINAN", "Latitude", "Longitude", "Agravo", "Data_Notificacao", "ANO", "SE_Notificacao", "Data_Primeiros_Sintomas", "SE_Primeiros_Sintomas", "UF_Notificacao", "Municipio", "Nome", "Data_Nascimento", "Idade", "Sexo", "Gestante", "Escolaridade", "Nome_Mae", "Municipio_Residencia", "UF_Residencia", "RS_Residencia", "Logradouro", "Numero", "Bairro", "CEP", "Zona", "Data_Digitacao", "Data_Investigacao", "Febre", "Mialgia", "Cefaleia", "Exantema", "Vomito", "Nausea", "Dor_nas_Costas", "Conjuntivite", "Artrite", "Artralgia_Intensa", "Petequias", "Leucopenia", "Prova_do_Laco_Positiva", "Dor_retroorbital", "Diabetes", "Doenca_Hematologica", "Hepatopatia", "Doenca_Renal", "Hipertensao", "Doenca_Acido_Peptica", "Doenca_Auto_Imune", "Data_Sorologia", "Resultado_Sorologia", "Data_PCR", "Resultado_PCR", "Sorotipo", "Classificacao_Final", "Critério_Encerramento", "Autoctone", "UF_Infeccao", "Municipio_Infeccao", "Bairro_Infeccao", "Evolucao", "Hospitalizado", "Data_Internamento", "Data_Obito", "Data_Encerramento", "Data_SNA", "Letargia", "Hepatomegalia", "Acumulo_Liquidos", "Hipotensao_Lipotimia", "Queda_Abrupta_Plaquetas", "Vomitos_Persistentes", "Hemorragias", "Aumento_Hematocrito", "Dor_Abdominal", "Data_Dengue_Grave", "Pulso_Debil", "PA_Convergente", "TPC", "Acumulo_Liq_Insuficiencia_Resp", "Taquicardia", "Extremidades_Frias", "Hipotensao", "Hematemese", "Melena", "Metrorragia_", "Sangramento_SNC", "Aumento_AST_ALT", "Miocardite", "Alteracao_Consciencia", "Outros_Orgaos", "Manifestacao_Hemorragica", "Epistaxe", "Gengivorragia", "Metrorragia", "Observacoes" )

PR_22_23_CHIK_SINAIS_NOTIFICADOS <- tibble(Febre = as.integer(RS22_22_23_SINAN_DECODIFICADO_CHIK %>% 
                                                 filter(Febre == "SIM" ) %>%
                                                 count()),
                                               Mialgia = as.integer(RS22_22_23_SINAN_DECODIFICADO_CHIK %>%
                                                 filter(Mialgia == "SIM" ) %>%
                                                 count()),
                                               Cefaleia = as.integer(RS22_22_23_SINAN_DECODIFICADO_CHIK %>%
                                                 filter(Cefaleia == "SIM") %>%
                                                 count()),
                                               Exantema = as.integer(RS22_22_23_SINAN_DECODIFICADO_CHIK %>%
                                                 filter(Exantema == "SIM") %>%
                                                 count()),
                                               Vomito = as.integer(RS22_22_23_SINAN_DECODIFICADO_CHIK %>%
                                                 filter(Vomito == "SIM") %>%
                                                 count()),
                                               Nausea = as.integer(RS22_22_23_SINAN_DECODIFICADO_CHIK %>%
                                                 filter(Nausea == "SIM") %>%
                                                 count()),
                                               Dor_nas_Costas = as.integer(RS22_22_23_SINAN_DECODIFICADO_CHIK %>%
                                                 filter(Dor_nas_Costas == "SIM") %>%
                                                 count()),
                                               Conjuntivite = as.integer(RS22_22_23_SINAN_DECODIFICADO_CHIK %>%
                                                 filter(Conjuntivite == "SIM") %>%
                                                 count()),
                                               Artrite = as.integer(RS22_22_23_SINAN_DECODIFICADO_CHIK %>%
                                                 filter(Artrite == "SIM") %>%
                                                 count()),
                                               Artralgia_Intensa = as.integer(RS22_22_23_SINAN_DECODIFICADO_CHIK %>%
                                                 filter(Artralgia_Intensa == "SIM") %>%
                                                 count()),
                                               Petequias = as.integer(RS22_22_23_SINAN_DECODIFICADO_CHIK %>%
                                                 filter(Petequias == "SIM") %>%
                                                 count()),
                                               Leucopenia = as.integer(RS22_22_23_SINAN_DECODIFICADO_CHIK %>%
                                                 filter(Leucopenia == "SIM") %>%
                                                 count()),
                                               Prova_do_Laco_Positiva = as.integer(RS22_22_23_SINAN_DECODIFICADO_CHIK %>%
                                                 filter(Prova_do_Laco_Positiva == "SIM") %>%
                                                 count()),
                                               Dor_retroorbital = as.integer(RS22_22_23_SINAN_DECODIFICADO_CHIK %>%
                                                 filter(Dor_retroorbital == "SIM") %>%
                                                 count())
                                               )
colnames(PR_22_23_CHIK_SINAIS_NOTIFICADOS) <- c("Febre", "Mialgia", "Cefaleia", "Exantema", "Vomito", "Nausea", "Dor_nas_Costas", "Conjuntivite", "Artrite", "Artralgia_Intensa", "Petequias", "Leucopenia", "Prova_do_Laco_Positiva", "Dor_Retroorbital")

PR_22_23_CHIK_SINAIS_Confirmados <- tibble(Febre = as.integer(RS22_22_23_SINAN_DECODIFICADO_CHIK %>% 
                                             filter(Febre == "SIM",
                                                    Classificacao_Final == "CHIKUNGUNYA") %>%
                                             count()),
                                           Mialgia = as.integer(RS22_22_23_SINAN_DECODIFICADO_CHIK %>%
                                             filter(Mialgia == "SIM",
                                                    Classificacao_Final == "CHIKUNGUNYA") %>%
                                             count()),
                                           Cefaleia = as.integer(RS22_22_23_SINAN_DECODIFICADO_CHIK %>%
                                             filter(Cefaleia == "SIM",
                                                    Classificacao_Final == "CHIKUNGUNYA") %>%
                                             count()),
                                           Exantema = as.integer(RS22_22_23_SINAN_DECODIFICADO_CHIK %>%
                                             filter(Exantema == "SIM",
                                                    Classificacao_Final == "CHIKUNGUNYA") %>%
                                             count()),
                                           Vomito = as.integer(RS22_22_23_SINAN_DECODIFICADO_CHIK %>%
                                             filter(Vomito == "SIM",
                                                    Classificacao_Final == "CHIKUNGUNYA") %>%
                                             count()),
                                           Nausea = as.integer(RS22_22_23_SINAN_DECODIFICADO_CHIK %>%
                                             filter(Nausea == "SIM",
                                                    Classificacao_Final == "CHIKUNGUNYA") %>%
                                             count()),
                                           Dor_nas_Costas = as.integer(RS22_22_23_SINAN_DECODIFICADO_CHIK %>%
                                             filter(Dor_nas_Costas == "SIM",
                                                    Classificacao_Final == "CHIKUNGUNYA") %>%
                                             count()),
                                           Conjuntivite = as.integer(RS22_22_23_SINAN_DECODIFICADO_CHIK %>%
                                             filter(Conjuntivite == "SIM",
                                                    Classificacao_Final == "CHIKUNGUNYA") %>%
                                             count()),
                                           Artrite = as.integer(RS22_22_23_SINAN_DECODIFICADO_CHIK %>%
                                             filter(Artrite == "SIM",
                                                    Classificacao_Final == "CHIKUNGUNYA") %>%
                                             count()),
                                           Artralgia_Intensa = as.integer(RS22_22_23_SINAN_DECODIFICADO_CHIK %>%
                                             filter(Artralgia_Intensa == "SIM",
                                                    Classificacao_Final == "CHIKUNGUNYA") %>%
                                             count()),
                                           Petequias = as.integer(RS22_22_23_SINAN_DECODIFICADO_CHIK %>%
                                             filter(Petequias == "SIM",
                                                    Classificacao_Final == "CHIKUNGUNYA") %>%
                                             count()),
                                           Leucopenia = as.integer(RS22_22_23_SINAN_DECODIFICADO_CHIK %>%
                                             filter(Leucopenia == "SIM",
                                                    Classificacao_Final == "CHIKUNGUNYA") %>%
                                             count()),
                                           Prova_do_Laco_Positiva = as.integer(RS22_22_23_SINAN_DECODIFICADO_CHIK %>%
                                             filter(Prova_do_Laco_Positiva == "SIM",
                                                    Classificacao_Final == "CHIKUNGUNYA") %>%
                                             count()),
                                           Dor_retroorbital = as.integer(RS22_22_23_SINAN_DECODIFICADO_CHIK %>%
                                             filter(Dor_retroorbital == "SIM",
                                                    Classificacao_Final == "CHIKUNGUNYA") %>%
                                             count())
)
#colnames(PR_22_23_CHIK_SINAIS_NOTIFICADOS) <- c("Febre", "Mialgia", "Cefaleia", "Exantema", "Vomito", "Nausea", "Dor_nas_Costas", "Conjuntivite", "Artrite", "Artralgia_Intensa", "Petequias", "Leucopenia", "Prova_do_Laco_Positiva", "Dor_Retroorbital")


AUX_GRAF <- data.frame(Sintomas = c("Febre", "Mialgia", "Cefaleia", "Exantema", "Vomito", "Nausea", "Dor_nas_Costas", "Conjuntivite", "Artrite", "Artralgia_Intensa", "Petequias", "Leucopenia", "Prova_do_Laco_Positiva", "Dor_Retroorbital"),
                       Notificados = NA,
                       Confirmados = NA)

AUX_GRAF[1,2] <- PR_22_23_CHIK_SINAIS_NOTIFICADOS[, 1]
AUX_GRAF[2,2] <- PR_22_23_CHIK_SINAIS_NOTIFICADOS[, 2]
AUX_GRAF[3,2] <- PR_22_23_CHIK_SINAIS_NOTIFICADOS[, 3]
AUX_GRAF[4,2] <- PR_22_23_CHIK_SINAIS_NOTIFICADOS[, 4]
AUX_GRAF[5,2] <- PR_22_23_CHIK_SINAIS_NOTIFICADOS[, 5]
AUX_GRAF[6,2] <- PR_22_23_CHIK_SINAIS_NOTIFICADOS[, 6]
AUX_GRAF[7,2] <- PR_22_23_CHIK_SINAIS_NOTIFICADOS[, 7]
AUX_GRAF[8,2] <- PR_22_23_CHIK_SINAIS_NOTIFICADOS[, 8]
AUX_GRAF[9,2] <- PR_22_23_CHIK_SINAIS_NOTIFICADOS[, 9]
AUX_GRAF[10,2] <- PR_22_23_CHIK_SINAIS_NOTIFICADOS[, 10]
AUX_GRAF[11,2] <- PR_22_23_CHIK_SINAIS_NOTIFICADOS[, 11]
AUX_GRAF[12,2] <- PR_22_23_CHIK_SINAIS_NOTIFICADOS[, 12]
AUX_GRAF[13,2] <- PR_22_23_CHIK_SINAIS_NOTIFICADOS[, 13]
AUX_GRAF[14,2] <- PR_22_23_CHIK_SINAIS_NOTIFICADOS[, 14]

AUX_GRAF[1,3] <- PR_22_23_CHIK_SINAIS_Confirmados[, 1]
AUX_GRAF[2,3] <- PR_22_23_CHIK_SINAIS_Confirmados[, 2]
AUX_GRAF[3,3] <- PR_22_23_CHIK_SINAIS_Confirmados[, 3]
AUX_GRAF[4,3] <- PR_22_23_CHIK_SINAIS_Confirmados[, 4]
AUX_GRAF[5,3] <- PR_22_23_CHIK_SINAIS_Confirmados[, 5]
AUX_GRAF[6,3] <- PR_22_23_CHIK_SINAIS_Confirmados[, 6]
AUX_GRAF[7,3] <- PR_22_23_CHIK_SINAIS_Confirmados[, 7]
AUX_GRAF[8,3] <- PR_22_23_CHIK_SINAIS_Confirmados[, 8]
AUX_GRAF[9,3] <- PR_22_23_CHIK_SINAIS_Confirmados[, 9]
AUX_GRAF[10,3] <- PR_22_23_CHIK_SINAIS_Confirmados[, 10]
AUX_GRAF[11,3] <- PR_22_23_CHIK_SINAIS_Confirmados[, 11]
AUX_GRAF[12,3] <- PR_22_23_CHIK_SINAIS_Confirmados[, 12]
AUX_GRAF[13,3] <- PR_22_23_CHIK_SINAIS_Confirmados[, 13]
AUX_GRAF[14,3] <- PR_22_23_CHIK_SINAIS_Confirmados[, 14]

RS22_22_23_GRAF_SINAIS_CHIK <- ggplot (AUX_GRAF, 
                                  aes(x = Sintomas)) + 
  theme(axis.text.x = element_text(angle = 80, 
                                   vjust = .5,
                                   face = "bold",
                                   size = 14)) +
  labs(caption = Fonte, 
       x = "Sintomas",
       y = "Número de Casos",
       title = "PREVALÊNCIA DE SINTOMAS EM CASOS NOTIFICADOS/CONFIRMADOS PARANÁ - 2022/23",
       subtitle = "Sinais Clínicos em Notificações de CHIKUNGUNYA Assinalados no Campo 33 da Ficha do SINAN") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 19,
                                   colour = "#556B2F")) +
  geom_bar(aes( y = Notificados, 
                fill = "Notificados"),
    stat = "identity",
    color = "black",
    width = .4,
    position = position_nudge(x = -.20)) + 
  geom_label(aes(y = Notificados,
                 label = Notificados),
             size = 3, 
             alpha = 0.5,
             nudge_x = -.20,
             nudge_y = .5) + 
  scale_fill_manual(name = "", 
                    values = c("Notificados" = "#4D5656", "Confirmados" = "#B03A2E")) +
  theme(legend.position = "bottom") +
  geom_bar(aes( y = Confirmados, 
         fill = "Confirmados"),
    stat = "identity",
    color = "black",
    width = .4,
    position = position_nudge(x = .20)) +
  geom_label(aes(y = Confirmados,
                 label = Confirmados),
             size = 3, 
             alpha = 0.5,
             nudge_x = .20,
             nudge_y = .2) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

RS22_22_23_INFORME_CHIK <- RS22_22_23_GRAF_SINAIS_CHIK

ggsave(filename = "/home/gustavo/Área de Trabalho/Análise_de_Dados/Graficos_Mapas/RS22_22_23_GRAF_SINAIS_CHIK.png", 
       plot = RS22_22_23_GRAF_SINAIS_CHIK,
       width = 15.51,
       height = 9.51)