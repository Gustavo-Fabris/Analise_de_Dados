rm(list =ls())

library(foreign)
library (dplyr)
library(tidyr)


setwd("/home/gustavo/Área de trabalho/Análise_de_Dados/")

BASE_IBGE_BRASIL <- read.csv (file = "Base_de_Dados/Auxiliares/Planilha_Base_IBGE_BRASIL.csv",
                              header = TRUE,
                              sep = ",")

DENGON2022 <- read.dbf(file = "Base_de_Dados/DBF/DENGON2022.dbf", 
                       as.is = FALSE) %>% select(ID_REGIONA, NU_NOTIFIC, ID_GEO1,ID_GEO2, ID_AGRAVO, ID_REGIONA, DT_NOTIFIC, 
                                                 NU_ANO, SEM_NOT, DT_SIN_PRI, SEM_PRI,  SG_UF_NOT, ID_MUNICIP, NM_PACIENT, DT_NASC,
                                                 NU_IDADE_N, CS_SEXO, CS_GESTANT, CS_ESCOL_N, NM_MAE_PAC, ID_MN_RESI, SG_UF, ID_RG_RESI, 
                                                 NM_LOGRADO, NU_NUMERO, NM_BAIRRO, NU_CEP, CS_ZONA, DT_DIGITA, DT_INVEST, FEBRE, MIALGIA, 
                                                 CEFALEIA, EXANTEMA, VOMITO, NAUSEA, DOR_COSTAS, CONJUNTVIT, ARTRITE, ARTRALGIA, PETEQUIA_N,
                                                 LEUCOPENIA, LACO, DOR_RETRO, DIABETES, HEMATOLOG, HEPATOPAT, RENAL, HIPERTENSA, ACIDO_PEPT, 
                                                 AUTO_IMUNE, DT_SORO, RESUL_SORO, DT_PCR, RESUL_PCR_, SOROTIPO, CLASSI_FIN, CRITERIO, 
                                                 TPAUTOCTO, COUFINF, COMUNINF, CO_BAINF, EVOLUCAO, HOSPITALIZ, DT_INTERNA, DT_OBITO, 
                                                 DT_ENCERRA, DT_ALRM, ALRM_LETAR, ALRM_HEPAT, ALRM_LIQ, ALRM_HIPOT, ALRM_PLAQ, ALRM_VOM, 
                                                 ALRM_SANG, ALRM_HEMAT, ALRM_ABDOM, DT_GRAV, GRAV_PULSO, GRAV_CONV, GRAV_ENCH, GRAV_INSUF, 
                                                 GRAV_TAQUI, GRAV_EXTRE, GRAV_HIPOT, GRAV_HEMAT, GRAV_MELEN, GRAV_METRO, GRAV_SANG, GRAV_AST,
                                                 GRAV_MIOC, GRAV_CONSC, GRAV_ORGAO, MANI_HEMOR, EPISTAXE, GENGIVO, METRO, DS_OBS)

CHIKON2022 <- read.dbf("Base_de_Dados/DBF/CHIKON2022.dbf",
                       as.is = FALSE) %>% select(ID_REGIONA, NU_NOTIFIC, ID_GEO1,ID_GEO2, ID_AGRAVO, ID_REGIONA, DT_NOTIFIC, NU_ANO, SEM_NOT, 
                                                 DT_SIN_PRI, SEM_PRI,  SG_UF_NOT, ID_MUNICIP, NM_PACIENT, DT_NASC, NU_IDADE_N, CS_SEXO, CS_GESTANT, 
                                                 CS_ESCOL_N, NM_MAE_PAC, ID_MN_RESI, SG_UF, ID_RG_RESI, NM_LOGRADO, NU_NUMERO, NM_BAIRRO, NU_CEP, 
                                                 CS_ZONA, DT_DIGITA, DT_INVEST, FEBRE, MIALGIA, CEFALEIA, EXANTEMA, VOMITO, NAUSEA, DOR_COSTAS, 
                                                 CONJUNTVIT, ARTRITE, ARTRALGIA, PETEQUIA_N, LEUCOPENIA, LACO, DOR_RETRO, DIABETES, HEMATOLOG, 
                                                 HEPATOPAT, RENAL, HIPERTENSA, ACIDO_PEPT, AUTO_IMUNE, DT_SORO, RESUL_SORO, DT_PCR, RESUL_PCR_, 
                                                 SOROTIPO, CLASSI_FIN, CRITERIO, TPAUTOCTO, COUFINF, COMUNINF, CO_BAINF, EVOLUCAO, HOSPITALIZ, 
                                                 DT_INTERNA, DT_OBITO, DT_ENCERRA, DT_ALRM, ALRM_LETAR, ALRM_HEPAT, ALRM_LIQ, ALRM_HIPOT, ALRM_PLAQ, 
                                                 ALRM_VOM, ALRM_SANG, ALRM_HEMAT, ALRM_ABDOM, DT_GRAV, GRAV_PULSO, GRAV_CONV, GRAV_ENCH, GRAV_INSUF, 
                                                 GRAV_TAQUI, GRAV_EXTRE, GRAV_HIPOT, GRAV_HEMAT, GRAV_MELEN, GRAV_METRO, GRAV_SANG, GRAV_AST, 
                                                 GRAV_MIOC, GRAV_CONSC, GRAV_ORGAO, MANI_HEMOR, EPISTAXE, GENGIVO, METRO, DS_OBS)

SINAN_DENGUE_RS <- DENGON2022 %>% 
  filter(ID_MN_RESI == 411150,
         CLASSI_FIN == 10 |
           CLASSI_FIN == 11 |
           CLASSI_FIN == 12 |
           CLASSI_FIN == 8)

SINAN_CHIK_RS <- CHIKON2022 %>% 
  filter(ID_MN_RESI == 411150,
         CLASSI_FIN == 13 )

TEST <- read.csv(file = "/home/gustavo/Área de trabalho/TEST.csv",
                 header = TRUE,
                 sep =",")

TEST$SINAN <- as.factor(TEST$SINAN)

AUX <- rbind(SINAN_DENGUE_RS,
             SINAN_CHIK_RS)

AUX$ID_AGRAVO <- factor(AUX$ID_AGRAVO,
                        label = c("Dengue", "Chikungunya"), 
                        levels = c("A90", "A92.0")
)

###Sintomas###
AUX$FEBRE <- factor(AUX$FEBRE,
                    label = c("SIM", "NÃO"), 
                    levels = c(1, 2)
)

AUX$MIALGIA <- factor(AUX$MIALGIA,
                      label = c("SIM", "NÃO"), 
                      levels = c(1, 2)
)

AUX$CEFALEIA <- factor(AUX$CEFALEIA,
                       label = c("SIM", "NÃO"), 
                       levels = c(1, 2)
)

AUX$EXANTEMA <- factor(AUX$EXANTEMA,
                       label = c("SIM", "NÃO"), 
                       levels = c(1, 2)
)

AUX$VOMITO <- factor(AUX$VOMITO,
                     label = c("SIM", "NÃO"), 
                     levels = c(1, 2)
)

AUX$NAUSEA <- factor(AUX$NAUSEA,
                     label = c("SIM", "NÃO"), 
                     levels = c(1, 2)
)

AUX$DOR_COSTAS <- factor(AUX$DOR_COSTAS,
                         label = c("SIM", "NÃO"), 
                         levels = c(1, 2)
)

AUX$DOR_RETRO <- factor(AUX$DOR_RETRO,
                        label = c("SIM", "NÃO"), 
                        levels = c(1, 2)
)

AUX$CONJUNTVIT <- factor(AUX$CONJUNTVIT,
                         label = c("SIM", "NÃO"), 
                         levels = c(1, 2)
)

AUX$ARTRALGIA <- factor(AUX$ARTRALGIA,
                        label = c("SIM", "NÃO"), 
                        levels = c(1, 2)
)

AUX$ARTRITE <- factor(AUX$ARTRITE,
                      label = c("SIM", "NÃO"), 
                      levels = c(1, 2)
)

AUX$PETEQUIA_N <- factor(AUX$PETEQUIA_N,
                         label = c("SIM", "NÃO"), 
                         levels = c(1, 2)
)

AUX$LEUCOPENIA <- factor(AUX$LEUCOPENIA,
                         label = c("SIM", "NÃO"), 
                         levels = c(1, 2)
)

AUX$LACO <- factor(AUX$LACO,
                   label = c("SIM", "NÃO"), 
                   levels = c(1, 2)
)

###Doenças Pré-existentes

AUX$DIABETES <- factor(AUX$DIABETES,
                       label = c("SIM", "NÃO"), 
                       levels = c(1, 2)
)

AUX$HEMATOLOG <- factor(AUX$HEMATOLOG,
                        label = c("SIM", "NÃO"), 
                        levels = c(1, 2)
)

AUX$HEPATOPAT <- factor(AUX$HEPATOPAT,
                        label = c("SIM", "NÃO"), 
                        levels = c(1, 2)
)

AUX$RENAL <- factor(AUX$RENAL,
                    label = c("SIM", "NÃO"), 
                    levels = c(1, 2)
)

AUX$HIPERTENSA <- factor(AUX$HIPERTENSA,
                         label = c("SIM", "NÃO"), 
                         levels = c(1, 2)
)

AUX$ACIDO_PEPT <- factor(AUX$ACIDO_PEPT,
                         label = c("SIM", "NÃO"), 
                         levels = c(1, 2)
)

AUX$AUTO_IMUNE <- factor(AUX$AUTO_IMUNE,
                         label = c("SIM", "NÃO"), 
                         levels = c(1, 2)
)

####Outros####

AUX$CS_GESTANT <- factor(AUX$CS_GESTANT,
                         label = c("1_TRI", "2_TRI", "3_TRI", "IDADE GESTACIONAL IGN", "NÃO", "NÃO SE APLICA", "IGNORADO"), 
                         levels = c(1, 2, 3, 4, 5, 6, 9)
)

AUX$CS_ESCOL_N <- factor(AUX$CS_ESCOL_N,
                         label = c("ANALFABETO", "1 a 4 SÉRIE DO FUNDAMENTAL INCOMPLETA", "4 SÉRIE DO FUNDAMENTAL COMPLETA", "5 a 8 SÉRIE DO FUNDAMENTAL INCOMPLETA", "FUNDAMENTAL COMPLETO", "ENSINO MÉDIO INCOMPLETO", "ENSINO MÉDIO COMPLETO", "SUPERIOR INCONPLETO", "SUPERIOR COMPLETO", "IGNORADO", "NÃO SE APLICA"), 
                         levels = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
)

AUX$RESUL_SORO <- factor(AUX$RESUL_SORO,
                         label = c("REAGENTE", "NÃO REAGENTE", "INCONCLUSIVO", "NÃO REALIZADO"), 
                         levels = c(1, 2, 3, 4)
)


AUX$RESUL_PCR_ <- factor(AUX$RESUL_PCR_,
                         label = c("DETECTÁVEL", "NÃO DETECTÀVEL", "INCONCLUSIVO", "NÃO REALIZADO"), 
                         levels = c(1, 2, 3, 4)
)

AUX$SOROTIPO <- factor(AUX$SOROTIPO,
                       label = c("I", "II", "III", "IV"), 
                       levels = c(1, 2, 3, 4)
)

AUX$CLASSI_FIN <- factor(AUX$CLASSI_FIN,
                         label = c("DESCARTADO", "INCONCLUSIVO", "DENGUE", "D.S.A.", "IDENGUE_GRAVE", "CHIKUNGUNYA"), 
                         levels = c(5, 8, 10, 11, 12, 13)
)

AUX$CRITERIO <- factor(AUX$CRITERIO,
                       label = c("LABORATORIAL", "CLÍNICO-EPIDEMIOLÓGICO", "EM INVESTIGAÇÃO"), 
                       levels = c(1, 2, 3)
)

AUX$TPAUTOCTO <- factor(AUX$TPAUTOCTO,
                        label = c("SIM", "NÃO"), 
                        levels = c(1, 2)
)

AUX$HOSPITALIZ <- factor(AUX$HOSPITALIZ,
                         label = c("SIM", "NÃO"), 
                         levels = c(1, 2)
)

AUX$EVOLUCAO <- factor(AUX$EVOLUCAO,
                       label = c("CURA", "ÓBITO PELO AGRAVO", "ÓBITO POR OUTRAS CAUSAS","ÓBITO EM INVESTIGAÇÃO", "INDETERMINADO"), 
                       levels = c(1, 2, 3, 4, 9)
)

AUX$CS_ZONA <- factor(AUX$CS_ZONA,
                      label = c("URBANA", "RURAL", "PERIURBANA","INDETERMINADO"), 
                      levels = c(1, 2, 3, 9)
)

AUX$ALRM_LETAR <- factor(AUX$ALRM_LETAR,
                         label = c("SIM", "NÃO"), 
                         levels = c(1, 2)
)

AUX$ALRM_HEPAT <- factor(AUX$ALRM_HEPAT,
                         label = c("SIM", "NÃO"), 
                         levels = c(1, 2)
)

AUX$ALRM_LIQ <- factor(AUX$ALRM_LIQ,
                       label = c("SIM", "NÃO"), 
                       levels = c(1, 2)
)

AUX$ALRM_HIPOT <- factor(AUX$ALRM_HIPOT,
                         label = c("SIM", "NÃO"), 
                         levels = c(1, 2)
)

AUX$ALRM_PLAQ <- factor(AUX$ALRM_PLAQ,
                        label = c("SIM", "NÃO"), 
                        levels = c(1, 2)
)

AUX$ALRM_VOM <- factor(AUX$ALRM_VOM,
                       label = c("SIM", "NÃO"), 
                       levels = c(1, 2)
)

AUX$ALRM_SANG <- factor(AUX$ALRM_SANG,
                        label = c("SIM", "NÃO"), 
                        levels = c(1, 2)
)

AUX$ALRM_HEMAT <- factor(AUX$ALRM_LETAR,
                         label = c("SIM", "NÃO"), 
                         levels = c(1, 2)
)

AUX$ALRM_ABDOM <- factor(AUX$ALRM_ABDOM,
                         label = c("SIM", "NÃO"), 
                         levels = c(1, 2)
)
####AUX$Municipio 

AUX01 <- data.frame(COD = AUX[,12], 
                    Municipio = NA)

for (i in AUX[,12]){
  AUX01[which(AUX01$COD == i), 2] <- BASE_IBGE_BRASIL[which(BASE_IBGE_BRASIL$Código.Município.Completo == i),13]
  
}

AUX[,12] <- AUX01[, 2]

####Município de Residência

AUX02 <- data.frame(COD = AUX[,20], 
                    Municipio = NA)

for (i in AUX[,20]){
  AUX02[which(AUX02$COD == i), 2] <- BASE_IBGE_BRASIL[which(BASE_IBGE_BRASIL$Código.Município.Completo == i),13]
  
}

AUX[,20] <- AUX02[, 2]

colnames(AUX)<- c("RS", "SINAN", "Latitude", "Longitude", 
                  "Agravo", "Data_Notificacao", "ANO", "SE_Notificacao", 
                  "Data_Primeiros_Sintomas", "SE_Primeiros_Sintomas", "UF_Notificacao", 
                  "Municipio", "Nome", "Data_Nascimento", "Idade", "Sexo", "Gestante", 
                  "Escolaridade", "Nome_Mae", "Municipio_Residencia", "UF_Residencia", 
                  "RS_Residencia", "Logradouro", "Numero", "Bairro", "CEP", "Zona", 
                  "Data_Digitacao", "Data_Investigacao", "Febre", "Mialgia", "Cefaleia", 
                  "Exantema", "Vomito", "Nausea", "Dor_nas_Costas", "Conjuntivite", "Artrite", 
                  "Artralgia_Intensa", "Petequias", "Leucopenia", "Prova_do_Laco_Positiva", 
                  "Dor_retroorbital", "Diabetes", "Doenca_Hematologica", "Hepatopatia", 
                  "Doenca_Renal", "Hipertensao", "Doenca_Acido_Peptica", "Doenca_Auto_Imune", 
                  "Data_Sorologia", "Resultado_Sorologia", "Data_PCR", "Resultado_PCR", "Sorotipo", 
                  "Classificacao_Final", "Critério_Encerramento", "Autoctone", "UF_Infeccao", 
                  "Municipio_Infeccao", "Bairro_Infeccao", "Evolucao", "Hospitalizado", 
                  "Data_Internamento", "Data_Obito", "Data_Encerramento", "Data_SNA", "Letargia", 
                  "Hepatomegalia", "Acumulo_Liquidos", "Hipotensao_Lipotimia", "Queda_Abrupta_Plaquetas", 
                  "Vomitos_Persistentes", "Hemorragias", "Aumento_Hematocrito", "Dor_Abdominal", 
                  "Data_Dengue_Grave", "Pulso_Debil", "PA_Convergente", "TPC", "Acumulo_Liq_Insuficiencia_Resp", 
                  "Taquicardia", "Extremidades_Frias", "Hipotensao", "Hematemese", "Melena", "Metrorragia_", 
                  "Sangramento_SNC", "Aumento_AST_ALT", "Miocardite", "Alteracao_Consciencia", "Outros_Orgaos", 
                  "Manifestacao_Hemorragica", "Epistaxe", "Gengivorragia", "Metrorragia", "Observacoes" )

TEST[, 4] <- as.character(TEST[, 4])

TEST[, 5] <- as.character(TEST[, 5])

AUX <- left_join(AUX, TEST, by = c("SINAN" = "SINAN"))

AUX[, c(3,4)] <- AUX[, c(100,101)]

assign(paste0("RS22_2022_SINAN_DECODIFICADO"), AUX) 

write.csv(AUX, 
          "/home/gustavo/Área de trabalho/RS22_2022_SINAN_PROVAVEIS_DECODIFICADO_Ivaipora.csv",
          row.names = FALSE)

