source("rdocs/source/packages.R")

# ---------------------------------------------------------------------------- #

#        ______   _____  ________      ________ 
#      |  ____| / ____| |__   __| /\  |__   __|
#     | |__    | (___     | |   /  \    | |   
#    |  __|    \___ \    | |  / /\ \   | |   
#   | |____   ____) |   | |  /____ \  | |   
#  |______   |_____/   |_| /_/    \_\|_|   
#  
#         Consultoria estatística 
#

# ---------------------------------------------------------------------------- #
# ############################## README ###################################### #
# Consultor, favor utilizar este arquivo .R para realizar TODAS as análises
# alocadas a você neste projeto pelo gerente responsável, salvo instrução 
# explícita do gerente para mudança.
#
# Escreva seu código da forma mais clara e legível possível, eliminando códigos
# de teste depreciados, ou ao menos deixando como comentário. Dê preferência
# as funções dos pacotes contidos no Tidyverse para realizar suas análises.
# ---------------------------------------------------------------------------- #
dados <- read.xlsx("banco/planilha.xlsx", sheetIndex = 1)


#Testes pareados com separação entre controle e experimento (testou-se as diferenças entre depois e antes)

#TDR----
dados$TDRdif <- dados$TDR.2-dados$TDR.1
shapiro.test(dados$TDRdif)
shapiro.test(dados$TDRdif[dados$GRUPO=="Experimental"])
shapiro.test(dados$TDRdif[dados$GRUPO=="Controle"])
#Não normal. Teste de medianas
t.test(dados$TDRdif[dados$GRUPO=="Experimental"],
       dados$TDRdif[dados$GRUPO=="Controle"])
wilcox.test(dados$TDRdif[dados$GRUPO=="Experimental"],
            dados$TDRdif[dados$GRUPO=="Controle"])

#boxplot
#Uni
ggplot(dados) +
  aes(
    x = factor(""),
    y = TDRdif
  ) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  guides(fill = FALSE) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "", y = "Aumento em TDR") +
  theme_estat()
ggsave("resultados/Stefan/box_uni_TDR.pdf", width = 158, height = 93, units = "mm")

#Bi
ggplot(dados) +
  aes(
    x = GRUPO,
    y = TDRdif
  ) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Grupo", y = "Aumento em TDR") +
  theme_estat()
ggsave("resultados/Stefan/box_bi_TDR.pdf", width = 158, height = 93, units = "mm")


#GAI----
dados$GAIdif <- dados$GAI.2-dados$GAI.1
shapiro.test(dados$GAIdif)
shapiro.test(dados$GAIdif[dados$GRUPO=="Experimental"])
shapiro.test(dados$GAIdif[dados$GRUPO=="Controle"])
#Normal. Teste de médias
t.test(dados$GAIdif[dados$GRUPO=="Experimental"],
        dados$GAIdif[dados$GRUPO=="Controle"])
wilcox.test(dados$GAIdif[dados$GRUPO=="Experimental"],
            dados$GAIdif[dados$GRUPO=="Controle"])

#boxplot
#Uni
ggplot(dados) +
  aes(
    x = factor(""),
    y = GAIdif
  ) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  guides(fill = FALSE) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "", y = "Aumento em GAI") +
  theme_estat()
ggsave("resultados/Stefan/box_uni_GAI.pdf", width = 158, height = 93, units = "mm")

#Bi
ggplot(dados) +
  aes(
    x = GRUPO,
    y = GAIdif
  ) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Grupo", y = "Aumento em GAI") +
  theme_estat()
ggsave("resultados/Stefan/box_bi_GAI.pdf", width = 158, height = 93, units = "mm")



#CESD-D----
dados$CESD.Ddif <- dados$CESD.D.2-dados$CESD.D.1
shapiro.test(dados$CESD.Ddif)
shapiro.test(dados$CESD.Ddif[dados$GRUPO=="Experimental"])
shapiro.test(dados$CESD.Ddif[dados$GRUPO=="Controle"])
#Normal. Teste de médias
t.test(dados$CESD.Ddif[dados$GRUPO=="Experimental"],
       dados$CESD.Ddif[dados$GRUPO=="Controle"])
wilcox.test(dados$CESD.Ddif[dados$GRUPO=="Experimental"],
            dados$CESD.Ddif[dados$GRUPO=="Controle"])

#boxplot
#Uni
ggplot(dados) +
  aes(
    x = factor(""),
    y = CESD.Ddif
  ) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  guides(fill = FALSE) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "", y = "Aumento em CESD.D") +
  theme_estat()
ggsave("resultados/Stefan/box_uni_CESD_D.pdf", width = 158, height = 93, units = "mm")

#Bi
ggplot(dados) +
  aes(
    x = GRUPO,
    y = CESD.Ddif
  ) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Grupo", y = "Aumento em CESD.D") +
  theme_estat()
ggsave("resultados/Stefan/box_bi_CESD_D.pdf", width = 158, height = 93, units = "mm")


#ANIMAIS----
dados$ANIMAISdif <- dados$ANIMAIS.2-dados$ANIMAIS.1
shapiro.test(dados$ANIMAISdif)
shapiro.test(dados$ANIMAISdif[dados$GRUPO=="Experimental"])
shapiro.test(dados$ANIMAISdif[dados$GRUPO=="Controle"])
#Normal. Teste de médias
t.test(dados$ANIMAISdif[dados$GRUPO=="Experimental"],
       dados$ANIMAISdif[dados$GRUPO=="Controle"])
wilcox.test(dados$ANIMAISdif[dados$GRUPO=="Experimental"],
            dados$ANIMAISdif[dados$GRUPO=="Controle"])

#boxplot
#Uni
ggplot(dados) +
  aes(
    x = factor(""),
    y = ANIMAISdif
  ) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  guides(fill = FALSE) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "", y = "Aumento em ANIMAIS") +
  theme_estat()
ggsave("resultados/Stefan/box_uni_ANIMAIS.pdf", width = 158, height = 93, units = "mm")

#Bi
ggplot(dados) +
  aes(
    x = GRUPO,
    y = ANIMAISdif
  ) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Grupo", y = "Aumento em ANIMAIS") +
  theme_estat()
ggsave("resultados/Stefan/box_bi_ANIMAIS.pdf", width = 158, height = 93, units = "mm")



#Memória imediata----
dados$M.IMEDIATAdif <- dados$M.IMEDIATA.2-dados$M.IMEDIATA.1
shapiro.test(dados$M.IMEDIATAdif)
shapiro.test(dados$M.IMEDIATAdif[dados$GRUPO=="Experimental"])
shapiro.test(dados$M.IMEDIATAdif[dados$GRUPO=="Controle"])
#Normal. Teste de médias
t.test(dados$M.IMEDIATAdif[dados$GRUPO=="Experimental"],
       dados$M.IMEDIATAdif[dados$GRUPO=="Controle"])
wilcox.test(dados$M.IMEDIATAdif[dados$GRUPO=="Experimental"],
            dados$M.IMEDIATAdif[dados$GRUPO=="Controle"])

#boxplot
#Uni
ggplot(dados) +
  aes(
    x = factor(""),
    y = M.IMEDIATAdif
  ) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  guides(fill = FALSE) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "", y = "Aumento em Memória Imediata") +
  theme_estat()
ggsave("resultados/Stefan/box_uni_M.IMEDIATA.pdf", width = 158, height = 93, units = "mm")

#Bi
ggplot(dados) +
  aes(
    x = GRUPO,
    y = M.IMEDIATAdif
  ) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Grupo", y = "Aumento em Memória Imediata") +
  theme_estat()
ggsave("resultados/Stefan/box_bi_M.IMEDIATA.pdf", width = 158, height = 93, units = "mm")


#Memória tardia----
dados$M.TARDIAdif <- dados$M.TARDIA.2-dados$M.TARDIA.1
shapiro.test(dados$M.TARDIAdif)
shapiro.test(dados$M.TARDIAdif[dados$GRUPO=="Experimental"])
shapiro.test(dados$M.TARDIAdif[dados$GRUPO=="Controle"])
#Não normal. Teste de medianas
t.test(dados$M.TARDIAdif[dados$GRUPO=="Experimental"],
       dados$M.TARDIAdif[dados$GRUPO=="Controle"])
wilcox.test(dados$M.TARDIAdif[dados$GRUPO=="Experimental"],
            dados$M.TARDIAdif[dados$GRUPO=="Controle"])

#boxplot
#Uni
ggplot(dados) +
  aes(
    x = factor(""),
    y = M.TARDIAdif
  ) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  guides(fill = FALSE) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "", y = "Aumento em Memória Tardia") +
  theme_estat()
ggsave("resultados/Stefan/box_uni_M.TARDIA.pdf", width = 158, height = 93, units = "mm")

#Bi
ggplot(dados) +
  aes(
    x = GRUPO,
    y = M.TARDIAdif
  ) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Grupo", y = "Aumento em Memória Tardia") +
  theme_estat()
ggsave("resultados/Stefan/box_bi_M.TARDIA.pdf", width = 158, height = 93, units = "mm")


#Reconhecimento----
dados$RECONHECIMENTOdif <- dados$RECONHECIMENTO.2-dados$RECONHECIMENTO.1
shapiro.test(dados$RECONHECIMENTOdif)
shapiro.test(dados$RECONHECIMENTOdif[dados$GRUPO=="Experimental"])
shapiro.test(dados$RECONHECIMENTOdif[dados$GRUPO=="Controle"])
#Não normal. Teste de medianas
t.test(dados$RECONHECIMENTOdif[dados$GRUPO=="Experimental"],
       dados$RECONHECIMENTOdif[dados$GRUPO=="Controle"])
wilcox.test(dados$RECONHECIMENTOdif[dados$GRUPO=="Experimental"],
            dados$RECONHECIMENTOdif[dados$GRUPO=="Controle"])

#boxplot
#Uni
ggplot(dados) +
  aes(
    x = factor(""),
    y = RECONHECIMENTOdif
  ) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  guides(fill = FALSE) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "", y = "Aumento em Reconhecimento") +
  theme_estat()
ggsave("resultados/Stefan/box_uni_RECONHECIMENTO.pdf", width = 158, height = 93, units = "mm")

#Bi
ggplot(dados) +
  aes(
    x = GRUPO,
    y = RECONHECIMENTOdif
  ) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Grupo", y = "Aumento em Reconhecimento") +
  theme_estat()
ggsave("resultados/Stefan/box_bi_RECONHECIMENTO.pdf", width = 158, height = 93, units = "mm")


#Testes pareados sem separação entre controle e experimento (testou-se as diferenças entre depois e antes)----

#TDR----
shapiro.test(dados$TDRdif)
#Não normal. Teste de medianas
t.test(dados$TDRdif)
wilcox.test(dados$TDRdif)


#GAI----
shapiro.test(dados$GAIdif)
#Normal. Teste de médias
t.test(dados$GAIdif)
wilcox.test(dados$GAIdif)


#CESD-D----
shapiro.test(dados$CESD.Ddif)
#Normal. Teste de médias
t.test(dados$CESD.Ddif)
wilcox.test(dados$CESD.Ddif)


#ANIMAIS----
shapiro.test(dados$ANIMAISdif)
#Normal. Teste de médias
t.test(dados$ANIMAISdif)
wilcox.test(dados$ANIMAISdif)


#Memória imediata----
shapiro.test(dados$M.IMEDIATAdif)
#Normal. Teste de médias
t.test(dados$M.IMEDIATAdif)
wilcox.test(dados$M.IMEDIATAdif)


#Memória  tardia----
shapiro.test(dados$M.TARDIAdif)
#Não normal. Teste de medianas
t.test(dados$M.TARDIAdif)
wilcox.test(dados$M.TARDIAdif)


#Reconhecimento----
shapiro.test(dados$RECONHECIMENTOdif)
#Não normal. Teste de medianas
t.test(dados$RECONHECIMENTOdif)
wilcox.test(dados$RECONHECIMENTOdif)
