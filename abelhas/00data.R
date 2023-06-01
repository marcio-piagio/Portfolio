require(dplyr)

Dados_abelhas <- readxl::read_excel("data/Dados abelhas.xlsx")

Dados_abelhas$Meses <- dplyr::case_when(
  Dados_abelhas$Meses == 1 ~ "Jan",
  Dados_abelhas$Meses == 2 ~ "Fer",
  Dados_abelhas$Meses == 3 ~ "Mar",
  Dados_abelhas$Meses == 4 ~ "Abr",
  Dados_abelhas$Meses == 5 ~ "Mai",
  Dados_abelhas$Meses == 6 ~ "Jun",
  Dados_abelhas$Meses == 7 ~ "Jul",
  Dados_abelhas$Meses == 8 ~ "Ago",
  Dados_abelhas$Meses == 9 ~ "Set",
  Dados_abelhas$Meses == 10 ~ "Out",
  Dados_abelhas$Meses == 11 ~ "Nov",
  TRUE ~ "Dez"
)


Dados_abelhas$Meses <- factor(Dados_abelhas$Meses,
                              levels = c("Jan", "Fer", "Mar", "Abr", "Mai", "Jun",
                                         "Jul", "Ago", "Set", "Out", "Nov", "Dez" ))

mediana <- tapply(Dados_abelhas$`N° potes Mel`, Dados_abelhas$Colônias, median)
Dados_abelhas$Colônias <- factor(Dados_abelhas$Colônias, levels = names(mediana)[order(mediana)])

bar <- Dados_abelhas %>% group_by(Colônias) %>% summarise('N° potes Mel'= sum(`N° potes Mel`))

bar2 <- Dados_abelhas %>% group_by(Meses) %>% summarise('N° potes Mel'= sum(`N° potes Mel`))

bar3 <- Dados_abelhas %>% group_by(Colônias,Meses) %>% summarise("N° potes Mel"=sum(`N° potes Mel`))

bar4 <- Dados_abelhas %>% group_by(Colônias,Meses) %>% summarise("Est. Pop." = sum(`Est. Pop.`))

numerica <- Dados_abelhas[3:9]

sd <- tapply(Dados_abelhas$`N° potes Mel`, Dados_abelhas$Colônias, sd)
Dados_abelhas$Colônias <- factor(Dados_abelhas$Colônias, levels = names(sd)[order(sd)])

mediana <- tapply(Dados_abelhas$`N° potes Mel`, Dados_abelhas$Colônias, median)
Dados_abelhas$Colônias <- factor(Dados_abelhas$Colônias, levels = names(mediana)[order(mediana)])

################################################################################

dadosModelo <- Dados_abelhas[-c(1,9)]

modelo.1 <- glm(`N° potes Mel` ~ .,data = dadosModelo, family = poisson(link="log"))

modelo.2 <- MASS::glm.nb(`N° potes Mel` ~ `N° discos` + Colônias, data = dadosModelo)

# summary(modelo.2)

sd <- tapply(Dados_abelhas$`N° potes Mel`, Dados_abelhas$Colônias, sd)
Dados_abelhas$Colônias <- factor(Dados_abelhas$Colônias, levels = names(sd)[order(sd)])

dadosModelo <- Dados_abelhas[-c(1,9)]

modelo.3 <- pscl::zeroinfl(`N° potes Mel` ~ .|1,
                           dist = "poisson", data = dadosModelo)

modelo.3.1 <- pscl::zeroinfl(`N° potes Mel` ~ .|`Comp. Canudo (cm)` + `Diâm. Canudo (cm)`+
                            `Tam. Discos (cm)`+ `Peso (Kg)`,
                           dist = "poisson", data = dadosModelo)

modelo.4 <- pscl::zeroinfl(`N° potes Mel` ~ .| 1,
                     dist = "negbin", data = dadosModelo)

modelo.4.2 <- pscl::zeroinfl(`N° potes Mel` ~ `Comp. Canudo (cm)`+`N° discos` + Colônias|
                             `Comp. Canudo (cm)`+ `N° discos` + `Tam. Discos (cm)` + `Peso (Kg)`,
                           dist = "negbin", data = dadosModelo)

# library(INLA)
# 
# d = dadosModelo[order(dadosModelo$Colônias),]
# 
# d$id <- rep(1:12,16)
# 
# nyc.inla <- inla(dadosModelo$`N° potes Mel` ~ dadosModelo$Colônias + dadosModelo$`Comp. Canudo (cm)`, data = dadosModelo,
#                  family = "zeroinflatednbinomial0")
# 
# 
# summary(nyc.inla)
# 
# library(hnp)
# hnp(modelo.4.2)

modelo.4.3 <- pscl::zeroinfl(`N° potes Mel` ~ .| `Comp. Canudo (cm)` + `Diâm. Canudo (cm)`+
                               `Tam. Discos (cm)`+ `Peso (Kg)`,
                             dist = "geometric", data = dadosModelo,)

# ## load pscl
library(pscl)
## fit hurdle model
modelo.5 <- hurdle(`N° potes Mel` ~ . | 1,data = dadosModelo,
                    dist = "poisson")

modelo.6 <- hurdle(`N° potes Mel` ~ . | 1,data = dadosModelo,
                   dist = "negbin")

modelo.6.1 <- hurdle(`N° potes Mel` ~ `Comp. Canudo (cm)`+`N° discos` + Colônias|
                       `Comp. Canudo (cm)`+ `N° discos` + `Tam. Discos (cm)` + `Peso (Kg)`,
                   dist = "negbin",data = dadosModelo)

# summary(modelo.6.1)
# hnp(modelo.6.1)

# logLik(modelo.1)
# logLik(modelo.2)
# logLik(modelo.3)
# logLik(modelo.3.1)
# logLik(modelo.4)
# logLik(modelo.4.2)
# logLik(modelo.4.3)
# logLik(modelo.5)
# logLik(modelo.6)
# logLik(modelo.6.1)

# AIC(modelo.1)
# AIC(modelo.2)
# AIC(modelo.3)
# AIC(modelo.3.1)
# AIC(modelo.4)
# AIC(modelo.4.2)
# AIC(modelo.4.3)
# AIC(modelo.5)
# AIC(modelo.6)
# AIC(modelo.6.1)

tab1 <- data.frame("AIC" =c(AIC(modelo.1),AIC(modelo.2),AIC(modelo.3),AIC(modelo.4),AIC(modelo.5),AIC(modelo.6) ),
           "logLik" = c(logLik(modelo.1)[1],logLik(modelo.2)[1],logLik(modelo.3)[1],logLik(modelo.4)[1],logLik(modelo.5)[1],logLik(modelo.6)[1]),
           "df" = c(21,22,22,23,22,23),
           "Dispersion"=c(disGlm(modelo.1,dadosModelo),disGlm(modelo.2,dadosModelo),disGlm(modelo.3,dadosModelo),disGlm(modelo.4,dadosModelo),
             disGlm(modelo.5,dadosModelo),disGlm(modelo.6,dadosModelo)),
             "dif"= abs(1-c(disGlm(modelo.1,dadosModelo),disGlm(modelo.2,dadosModelo),disGlm(modelo.3,dadosModelo),disGlm(modelo.4,dadosModelo),
                            disGlm(modelo.5,dadosModelo),disGlm(modelo.6,dadosModelo)))
           )

row.names(tab1) <- paste0("modelo.",1:6)

res1 <- res(modelo.2)



