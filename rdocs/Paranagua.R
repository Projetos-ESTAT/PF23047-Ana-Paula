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
# ----------------------------------------------------------------------------

library(readxl)
library(dplyr)
library(ggplot2)
library(DescTools)
library(asbio)
library(gridExtra)
library(lmtest)
library(agricolae)
library(readxl)
library(psych)
library(plyr)
library(lmtest)
library(tidyverse)
library(stats)
library(Metrics)
library(corrplot)
library(olsrr)
library(caret)
library(MASS)
library(ggpubr)
library(rstatix)


banco <- read_excel("planilha.xlsx")


cores_estat <- c('A11D21','#003366', '#CC9900', '#663333','#FF6600','#CC9966','#999966','#006606','#008091', '#041835','#666666')

theme_estat <- function (...) {
  theme <- ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title.y = ggplot2:: element_text(colour = "black",
                                            size = 12),
      axis.title.x = ggplot2:: element_text(colour = "black",
                                            size = 12),
      axis.text = ggplot2:: element_text(colour = "black", size
                                         = 9.5),
      panel.border = ggplot2:: element_blank() ,
      axis.line = ggplot2:: element_line(colour = "black"),
      legend.position = "top",
      ...
    )
  return (
    list(
      theme,
      scale_fill_manual(values = cores_estat ),
      scale_colour_manual(values = cores_estat )
    )
  )
}

##Gráficos


bx1 <- ggplot(banco1) + aes(x = `Teve COVID?`, y = `TDR 2`) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  labs(title = "Teste do relógio") +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "", y = "Queixas cognitivas") +
  theme_estat() +
  scale_x_discrete(labels = function(x) str_wrap(x,width = 20))


bx2 <- ggplot(banco1) + aes(x = `Teve COVID?`, y = `GAI 2`) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  labs(title = "Ansiedade geriátrica") +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Teve Covid?", y = "") +
  theme_estat() +
  scale_x_discrete(labels = function(x) str_wrap(x,width = 20))


bx3 <- ggplot(banco1) + aes(x = `Teve COVID?`, y = `CESD-D 2`) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  labs(title = "Escala de depressão") +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "", y = "") +
  theme_estat() +
  scale_x_discrete(labels = function(x) str_wrap(x,width = 20))


bx4 <- ggplot(banco1) + aes(x = `Teve COVID?`, y = `ANIMAIS 2`) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  labs(title = "Teste de fluência verbal") +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "", y = "Queixas cognitivas") +
  theme_estat() +
  scale_x_discrete(labels = function(x) str_wrap(x,width = 20))


bx5 <- ggplot(banco1) + aes(x = `Teve COVID?`, y = `M.IMEDIATA 2`) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  labs(title = "Memória imediata") +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Teve Covid?", y = "") +
  theme_estat() +
  scale_x_discrete(labels = function(x) str_wrap(x,width = 20))


bx6 <- ggplot(banco1) + aes(x = `Teve COVID?`, y = `M.TARDIA 2`) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  labs(title = "Memória tardia") +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  theme_estat() +
  labs(x = "", y = "") +
  scale_x_discrete(labels = function(x) str_wrap(x,width = 20))


bx7 <- ggplot(banco1) + aes(x = `Teve COVID?`, y = `RECONHECIMENTO 2`) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  labs(title = "Reconhecimento") +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Teve Covid?", y = "Queixas cognitivas") +
  theme_estat() +
  scale_x_discrete(labels = function(x) str_wrap(x,width = 20))



grid.arrange(bx1, bx2, bx3, bx4, bx5, bx6, bx7)
ggsave("BoxplotTC.pdf", width = 158, height = 93, units = "mm")


#### Pressupostos

banco1 <- banco %>%
  filter(GRUPO == "Experimental") %>%
  filter(`Teve COVID?` == "sim")

# Normalidade


shapiro.test(banco1$`TDR 2`)
shapiro.test(banco1$`GAI 2`) #
shapiro.test(banco1$`CESD-D 2`) #
shapiro.test(banco1$`ANIMAIS 2`) #
shapiro.test(banco1$`M.IMEDIATA 2`) #
shapiro.test(banco1$`M.TARDIA 2`) #
shapiro.test(banco1$`RECONHECIMENTO 2`)


#### Pressupostos

banco1 <- banco %>%
  filter(GRUPO == "Experimental") %>%
  filter(`Teve COVID?` == "não")

# Normalidade


shapiro.test(banco1$`TDR 2`)
shapiro.test(banco1$`GAI 2`) #
shapiro.test(banco1$`CESD-D 2`) #
shapiro.test(banco1$`ANIMAIS 2`) #
shapiro.test(banco1$`M.IMEDIATA 2`) #
shapiro.test(banco1$`M.TARDIA 2`) #
shapiro.test(banco1$`RECONHECIMENTO 2`)


#### Pressupostos

banco1 <- banco %>%
  filter(GRUPO == "Experimental")

# Normalidade


shapiro.test(banco1$`TDR 2`)
shapiro.test(banco1$`GAI 2`) #
shapiro.test(banco1$`CESD-D 2`) #
shapiro.test(banco1$`ANIMAIS 2`) #
shapiro.test(banco1$`M.IMEDIATA 2`)
shapiro.test(banco1$`M.TARDIA 2`)
shapiro.test(banco1$`RECONHECIMENTO 2`)


## Testes

wilcox.test(`TDR 2` ~ `Teve COVID?`, data = banco1)
t.test(`GAI 2` ~ `Teve COVID?`, data = banco1)
t.test(`CESD-D 2` ~ `Teve COVID?`, data = banco1)
t.test(`ANIMAIS 2` ~ `Teve COVID?`, data = banco1)
wilcox.test(`M.IMEDIATA 2` ~ `Teve COVID?`, data = banco1)
wilcox.test(`M.TARDIA 2` ~ `Teve COVID?`, data = banco1)
wilcox.test(`RECONHECIMENTO 2` ~ `Teve COVID?`, data = banco1)



## Análise 2

banco2 <- banco %>%
  filter(GRUPO == "Experimental")

##Gráficos

dp1 <- ggplot(banco2) +
  aes(x = `TDR 1` , y = `TDR 2`) +
  geom_point( colour = "#A11D21", size = 3) +
  labs(
    x = "Teste do relógio (antes)",
    y = "Teste do relógio (depois)") +
  geom_smooth() +
  theme_estat()


dp2 <- ggplot(banco2) +
  aes(x = `GAI 1` , y = `GAI 2`) +
  geom_point( colour = "#A11D21", size = 3) +
  labs(
    x = "Ansiedade (antes)",
    y = "Ansiedade (depois)") +
  geom_smooth() +
  theme_estat()


dp3 <- ggplot(banco2) +
  aes(x = `CESD-D 1` , y = `CESD-D 2`) +
  geom_point( colour = "#A11D21", size = 3) +
  labs(
    x = "E. de depressão (antes)",
    y = "E. de depressão (depois)") +
  geom_smooth() +
  theme_estat()


dp4 <- ggplot(banco2) +
  aes(x = `ANIMAIS 1` , y = `ANIMAIS 2`) +
  geom_point( colour = "#A11D21", size = 3) +
  labs(
    x = "Fluência verbal (antes)",
    y = "Fluência verbal (depois)") +
  geom_smooth() +
  theme_estat()


dp5 <- ggplot(banco2) +
  aes(x = `M.IMEDIATA 1` , y = `M.IMEDIATA 2`) +
  geom_point( colour = "#A11D21", size = 3) +
  labs(
    x = "M. imediata (antes)",
    y = "M. imediata (depois)") +
  geom_smooth() +
  theme_estat()


dp6 <- ggplot(banco2) +
  aes(x = `M.TARDIA 1` , y = `M.TARDIA 2`) +
  geom_point( colour = "#A11D21", size = 3) +
  labs(
    x = "M. tardia (antes)",
    y = "M. tardia (depois)") +
  geom_smooth() +
  theme_estat()


dp7 <- ggplot(banco2) +
  aes(x = `RECONHECIMENTO 1` , y = `RECONHECIMENTO 2`) +
  geom_point( colour = "#A11D21", size = 3) +
  labs(
    x = "Reconhecimento (antes)",
    y = "Reconhecimento (depois)") +
  geom_smooth() +
  theme_estat()


grid.arrange(dp1, dp2, dp3, dp4, dp5, dp6, dp7)
ggsave("DispersãoAD.pdf", width = 158, height = 93, units = "mm")


# Correlação


cor(banco2$`TDR 1`, banco2$`TDR 2`, method = "spearman")
cor(banco2$`GAI 1`, banco2$`GAI 2`, method = "spearman")
cor(banco2$`CESD-D 1`, banco2$`CESD-D 2`, method = "spearman")
cor(banco2$`ANIMAIS 1`, banco2$`ANIMAIS 2`, method = "pearson")
cor(banco2$`M.IMEDIATA 1`, banco2$`M.IMEDIATA 2`, method = "pearson")
cor(banco2$`M.TARDIA 1`, banco2$`M.TARDIA 2`, method = "pearson")
cor(banco2$`RECONHECIMENTO 1`, banco2$`RECONHECIMENTO 2`, method = "pearson")



## Análise 3

banco <- read_excel("planilha.xlsx")

banco2 <- banco %>%
  filter(GRUPO == "Controle")


##Gráficos

dp1 <- ggplot(banco2) +
  aes(x = `TDR 1` , y = `TDR 2`) +
  geom_point( colour = "#A11D21", size = 3) +
  labs(
    x = "Teste do relógio (antes)",
    y = "Teste do relógio (depois)") +
  geom_smooth() +
  theme_estat()


dp2 <- ggplot(banco2) +
  aes(x = `GAI 1` , y = `GAI 2`) +
  geom_point( colour = "#A11D21", size = 3) +
  labs(
    x = "Ansiedade (antes)",
    y = "Ansiedade (depois)") +
  geom_smooth() +
  theme_estat()


dp3 <- ggplot(banco2) +
  aes(x = `CESD-D 1` , y = `CESD-D 2`) +
  geom_point( colour = "#A11D21", size = 3) +
  labs(
    x = "E. de depressão (antes)",
    y = "E. de depressão (depois)") +
  geom_smooth() +
  theme_estat()


dp4 <- ggplot(banco2) +
  aes(x = `ANIMAIS 1` , y = `ANIMAIS 2`) +
  geom_point( colour = "#A11D21", size = 3) +
  labs(
    x = "Fluência verbal (antes)",
    y = "Fluência verbal (depois)") +
  geom_smooth() +
  theme_estat()


dp5 <- ggplot(banco2) +
  aes(x = `M.IMEDIATA 1` , y = `M.IMEDIATA 2`) +
  geom_point( colour = "#A11D21", size = 3) +
  labs(
    x = "M. imediata (antes)",
    y = "M. imediata (depois)") +
  geom_smooth() +
  theme_estat()


dp6 <- ggplot(banco2) +
  aes(x = `M.TARDIA 1` , y = `M.TARDIA 2`) +
  geom_point( colour = "#A11D21", size = 3) +
  labs(
    x = "M. tardia (antes)",
    y = "M. tardia (depois)") +
  geom_smooth() +
  theme_estat()


dp7 <- ggplot(banco2) +
  aes(x = `RECONHECIMENTO 1` , y = `RECONHECIMENTO 2`) +
  geom_point( colour = "#A11D21", size = 3) +
  labs(
    x = "Reconhecimento (antes)",
    y = "Reconhecimento (depois)") +
  geom_jitter() +
  geom_smooth() +
  theme_estat()


grid.arrange(dp1, dp2, dp3, dp4, dp5, dp6, dp7)
ggsave("DispersãoAD.pdf", width = 158, height = 93, units = "mm")


# Correlação


cor(banco2$`TDR 1`, banco2$`TDR 2`, method = "pearson")
cor(banco2$`GAI 1`, banco2$`GAI 2`, method = "spearman")
cor(banco2$`CESD-D 1`, banco2$`CESD-D 2`, method = "pearson")
cor(banco2$`ANIMAIS 1`, banco2$`ANIMAIS 2`, method = "pearson")
cor(banco2$`M.IMEDIATA 1`, banco2$`M.IMEDIATA 2`, method = "pearson")
cor(banco2$`M.TARDIA 1`, banco2$`M.TARDIA 2`, method = "pearson")
cor(banco2$`RECONHECIMENTO 1`, banco2$`RECONHECIMENTO 2`, method = "spearman")
