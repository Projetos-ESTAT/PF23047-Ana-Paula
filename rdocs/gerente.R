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
library(readxl)
planilha <- read_excel("banco/planilha.xlsx")
View(planilha)

#TDR2 
tabela <- planilha[c(1:18),]


tabela <- tabela %>%
  select(ESCOLARIDADE, `TDR 2`)%>%
  na.omit()

tabela_cont <- table(tabela$ESCOLARIDADE,tabela$`TDR 2`)

(tabela_cont) # frequência seja de pelo menos 5 elementos em cada célula da tabela

unique(tabela$ESCOLARIDADE)

teste <- tabela %>% 
  mutate(
    ensino = case_when(
      ESCOLARIDADE == "Ensino Fundamental incompleto (antigo ginásio ou 1º grau)" ~ 1,
      ESCOLARIDADE == "Ensino Médio incompleto (antigo colegial ou 2º grau)" ~ 2,
      ESCOLARIDADE == "Ensino Médio completo (antigo colegial ou 2º grau)" ~ 3,
      ESCOLARIDADE == "Ensino Superior completo" ~ 4,
      ESCOLARIDADE == "Pós-Graduação Lato Sensu ou Stricto Sensu" ~ 5
    )
  )

class(teste$ensino)


install.packages("coin")

library(coin)

resultado_teste <- spearman_test(ensino ~ `TDR 2`, data = teste)

print(resultado_teste)

ggplot(teste) +
  aes(
    x = as.character(`ensino`),
    y = `TDR 2`
  ) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Transmissão", y = "Consumo em Cidade (milhas/galão)") +
  theme_estat()


#GAI
tabela <- planilha[c(1:18),]


tabela <- tabela %>%
  select(ESCOLARIDADE, "GAI 2")%>%
  na.omit()

tabela_cont <- table(tabela$ESCOLARIDADE,tabela$`GAI 2`)
(tabela_cont) # frequência seja de pelo menos 5 elementos em cada célula da tabela
chisq.test(tabela_cont, correct = TRUE)


unique(tabela$`GAI 2`)

teste <- tabela %>% 
  mutate(
    ensino = case_when(
      ESCOLARIDADE == "Ensino Fundamental incompleto (antigo ginásio ou 1º grau)" ~ 1,
      ESCOLARIDADE == "Ensino Médio incompleto (antigo colegial ou 2º grau)" ~ 2,
      ESCOLARIDADE == "Ensino Médio completo (antigo colegial ou 2º grau)" ~ 3,
      ESCOLARIDADE == "Ensino Superior completo" ~ 4,
      ESCOLARIDADE == "Pós-Graduação Lato Sensu ou Stricto Sensu" ~ 5
    )
  )

class(teste$ensino)


install.packages("coin")

library(coin)


resultado_teste <- spearman_test(ensino ~ `GAI 2`, data = teste)

print(resultado_teste)

#CESD-D 3

tabela <- planilha[c(1:18),]

tabela <- tabela %>%
  select(ESCOLARIDADE, `CESD-D 2`)%>%
  na.omit()

tabela_cont <- table(tabela$ESCOLARIDADE,tabela$`CESD-D 2`)
(tabela_cont) # frequência seja de pelo menos 5 elementos em cada célula da tabela


unique(tabela$`CESD-D 2`)

teste <- tabela %>% 
  mutate(
    ensino = case_when(
      ESCOLARIDADE == "Ensino Fundamental incompleto (antigo ginásio ou 1º grau)" ~ 1,
      ESCOLARIDADE == "Ensino Médio incompleto (antigo colegial ou 2º grau)" ~ 2,
      ESCOLARIDADE == "Ensino Médio completo (antigo colegial ou 2º grau)" ~ 3,
      ESCOLARIDADE == "Ensino Superior completo" ~ 4,
      ESCOLARIDADE == "Pós-Graduação Lato Sensu ou Stricto Sensu" ~ 5
    )
  )

class(teste$ensino)

resultado_teste <- spearman_test(ensino ~ `CESD-D 2`, data = teste)

print(resultado_teste)


#ANIMAIS 

tabela <- planilha[c(1:18),]

tabela <- tabela %>%
  select(ESCOLARIDADE, "ANIMAIS 2")%>%
  na.omit()

teste <- tabela %>% 
  mutate(
    ensino = case_when(
      ESCOLARIDADE == "Ensino Fundamental incompleto (antigo ginásio ou 1º grau)" ~ 1,
      ESCOLARIDADE == "Ensino Médio incompleto (antigo colegial ou 2º grau)" ~ 2,
      ESCOLARIDADE == "Ensino Médio completo (antigo colegial ou 2º grau)" ~ 3,
      ESCOLARIDADE == "Ensino Superior completo" ~ 4,
      ESCOLARIDADE == "Pós-Graduação Lato Sensu ou Stricto Sensu" ~ 5
    )
  )

shapiro.test(teste$`ANIMAIS 2`)

boxplot(`ANIMAIS 2` ~ `ESCOLARIDADE`, data = tabela)

install.packages("car")
library(car)

leveneTest(`ANIMAIS 2` ~ `ESCOLARIDADE`, data = tabela)

plot(tabela$`ANIMAIS 2`)

anova <- aov(`ANIMAIS 2` ~ `ESCOLARIDADE`, data = tabela)
summary(anova)

tuk <- TukeyHSD(anova)

tuk

lab <- c("EF incompleto","EM incompleto","EM completo","ES completo","Pós-\ngraduação")
ggplot(teste) +
  aes(
    x = as.character(`ensino`),
    y = `ANIMAIS 2`
  ) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  scale_x_discrete(labels = c(
    "1" = "EF incompleto",
    "2" = "EM incompleto",
    "3" = "EM completo",
    "4" = "ES completo",
    "5" = "Pós-\ngraduação"
  ))+
  labs(x = "Escolaridade" , y = "Aumento da fluência verbal") +
  theme_estat()

ggsave("box_bi.pdf", width = 158, height = 93, units = "mm")

#M.IMEDIATA

tabela <- planilha[c(1:18),]

tabela <- tabela %>%
  select(ESCOLARIDADE, `M.IMEDIATA 2`)%>%
  na.omit()

tabela_cont <- table(tabela$ESCOLARIDADE,tabela$`M.IMEDIATA 2`)
(tabela_cont) 


unique(tabela$`M.IMEDIATA 2`)

teste <- tabela %>% 
  mutate(
    ensino = case_when(
      ESCOLARIDADE == "Ensino Fundamental incompleto (antigo ginásio ou 1º grau)" ~ 1,
      ESCOLARIDADE == "Ensino Médio incompleto (antigo colegial ou 2º grau)" ~ 2,
      ESCOLARIDADE == "Ensino Médio completo (antigo colegial ou 2º grau)" ~ 3,
      ESCOLARIDADE == "Ensino Superior completo" ~ 4,
      ESCOLARIDADE == "Pós-Graduação Lato Sensu ou Stricto Sensu" ~ 5
    )
  )

class(teste$ensino)

resultado_teste <- spearman_test(ensino ~ `M.IMEDIATA 2`, data = teste)

print(resultado_teste)


#M.TARDIA 2 
tabela <- planilha[c(1:18),]

tabela <- tabela %>%
  select(ESCOLARIDADE, `M.TARDIA 2`)%>%
  na.omit()

tabela_cont <- table(tabela$ESCOLARIDADE,tabela$`M.TARDIA 2`)
(tabela_cont) 


unique(tabela$`M.TARDIA 2`)

teste <- tabela %>% 
  mutate(
    ensino = case_when(
      ESCOLARIDADE == "Ensino Fundamental incompleto (antigo ginásio ou 1º grau)" ~ 1,
      ESCOLARIDADE == "Ensino Médio incompleto (antigo colegial ou 2º grau)" ~ 2,
      ESCOLARIDADE == "Ensino Médio completo (antigo colegial ou 2º grau)" ~ 3,
      ESCOLARIDADE == "Ensino Superior completo" ~ 4,
      ESCOLARIDADE == "Pós-Graduação Lato Sensu ou Stricto Sensu" ~ 5
    )
  )

class(teste$ensino)

resultado_teste <- spearman_test(ensino ~ `M.TARDIA 2`, data = teste)

print(resultado_teste)


#RECONHECIMENTO 2

tabela <- planilha[c(1:18),]

tabela <- tabela %>%
  select(ESCOLARIDADE, `RECONHECIMENTO 2`)%>%
  na.omit()

tabela_cont <- table(tabela$ESCOLARIDADE,tabela$`RECONHECIMENTO 2`)
(tabela_cont) 


unique(tabela$`RECONHECIMENTO 2`)

teste <- tabela %>% 
  mutate(
    ensino = case_when(
      ESCOLARIDADE == "Ensino Fundamental incompleto (antigo ginásio ou 1º grau)" ~ 1,
      ESCOLARIDADE == "Ensino Médio incompleto (antigo colegial ou 2º grau)" ~ 2,
      ESCOLARIDADE == "Ensino Médio completo (antigo colegial ou 2º grau)" ~ 3,
      ESCOLARIDADE == "Ensino Superior completo" ~ 4,
      ESCOLARIDADE == "Pós-Graduação Lato Sensu ou Stricto Sensu" ~ 5
    )
  )

class(teste$ensino)

resultado_teste <- spearman_test(ensino ~ `RECONHECIMENTO 2`, data = teste)

print(resultado_teste)
