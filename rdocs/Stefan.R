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
dados <- read.xlsx("banco/ESTAT.xlsx", sheetIndex = 1)


#Testes pareados com separação entre controle e experimento (testou-se as diferenças entre depois e antes)

#TDR----
dados$TDRdif <- dados$TDR.2-dados$TDR.1
shapiro.test(dados$TDRdif)
shapiro.test(dados$TDRdif[dados$TIPO.DO.GRUPO=="Grupo Experimental"])
shapiro.test(dados$TDRdif[dados$TIPO.DO.GRUPO=="Grupo Controle"])
#Não normal. Teste de medianas
t.test(dados$TDRdif[dados$TIPO.DO.GRUPO=="Grupo Experimental"],
       dados$TDRdif[dados$TIPO.DO.GRUPO=="Grupo Controle"])
wilcox.test(dados$TDRdif[dados$TIPO.DO.GRUPO=="Grupo Experimental"],
            dados$TDRdif[dados$TIPO.DO.GRUPO=="Grupo Controle"])


#GAI----
dados$GAIdif <- dados$GAI.2-dados$GAI.1
shapiro.test(dados$GAIdif)
shapiro.test(dados$GAIdif[dados$TIPO.DO.GRUPO=="Grupo Experimental"])
shapiro.test(dados$GAIdif[dados$TIPO.DO.GRUPO=="Grupo Controle"])
#Normal. Teste de médias
t.test(dados$GAIdif[dados$TIPO.DO.GRUPO=="Grupo Experimental"],
        dados$GAIdif[dados$TIPO.DO.GRUPO=="Grupo Controle"])
wilcox.test(dados$GAIdif[dados$TIPO.DO.GRUPO=="Grupo Experimental"],
            dados$GAIdif[dados$TIPO.DO.GRUPO=="Grupo Controle"])


#CESD-D----
dados$CESD.Ddif <- dados$CESD.D.2-dados$CESD.D.1
shapiro.test(dados$CESD.Ddif)
shapiro.test(dados$CESD.Ddif[dados$TIPO.DO.GRUPO=="Grupo Experimental"])
shapiro.test(dados$CESD.Ddif[dados$TIPO.DO.GRUPO=="Grupo Controle"])
#Normal. Teste de médias
t.test(dados$CESD.Ddif[dados$TIPO.DO.GRUPO=="Grupo Experimental"],
       dados$CESD.Ddif[dados$TIPO.DO.GRUPO=="Grupo Controle"])
wilcox.test(dados$CESD.Ddif[dados$TIPO.DO.GRUPO=="Grupo Experimental"],
            dados$CESD.Ddif[dados$TIPO.DO.GRUPO=="Grupo Controle"])


#ANIMAIS----
dados$ANIMAISdif <- dados$ANIMAIS.2-dados$ANIMAIS.1
shapiro.test(dados$ANIMAISdif)
shapiro.test(dados$ANIMAISdif[dados$TIPO.DO.GRUPO=="Grupo Experimental"])
shapiro.test(dados$ANIMAISdif[dados$TIPO.DO.GRUPO=="Grupo Controle"])
#Normal. Teste de médias
t.test(dados$ANIMAISdif[dados$TIPO.DO.GRUPO=="Grupo Experimental"],
       dados$ANIMAISdif[dados$TIPO.DO.GRUPO=="Grupo Controle"])
wilcox.test(dados$ANIMAISdif[dados$TIPO.DO.GRUPO=="Grupo Experimental"],
            dados$ANIMAISdif[dados$TIPO.DO.GRUPO=="Grupo Controle"])


#Memória imediata----
dados$M.IMEDIATAdif <- dados$M.IMEDIATA.2-dados$M.IMEDIATA.1
shapiro.test(dados$M.IMEDIATAdif)
shapiro.test(dados$M.IMEDIATAdif[dados$TIPO.DO.GRUPO=="Grupo Experimental"])
shapiro.test(dados$M.IMEDIATAdif[dados$TIPO.DO.GRUPO=="Grupo Controle"])
#Normal. Teste de médias
t.test(dados$M.IMEDIATAdif[dados$TIPO.DO.GRUPO=="Grupo Experimental"],
       dados$M.IMEDIATAdif[dados$TIPO.DO.GRUPO=="Grupo Controle"])
wilcox.test(dados$M.IMEDIATAdif[dados$TIPO.DO.GRUPO=="Grupo Experimental"],
            dados$M.IMEDIATAdif[dados$TIPO.DO.GRUPO=="Grupo Controle"])


#Memória  tardia----
dados$M.TARDIAdif <- dados$M.TARDIA.2-dados$M.TARDIA.1
shapiro.test(dados$M.TARDIAdif)
shapiro.test(dados$M.TARDIAdif[dados$TIPO.DO.GRUPO=="Grupo Experimental"])
shapiro.test(dados$M.TARDIAdif[dados$TIPO.DO.GRUPO=="Grupo Controle"])
#Não normal. Teste de medianas
t.test(dados$M.TARDIAdif[dados$TIPO.DO.GRUPO=="Grupo Experimental"],
       dados$M.TARDIAdif[dados$TIPO.DO.GRUPO=="Grupo Controle"])
wilcox.test(dados$M.TARDIAdif[dados$TIPO.DO.GRUPO=="Grupo Experimental"],
            dados$M.TARDIAdif[dados$TIPO.DO.GRUPO=="Grupo Controle"])


#Reconhecimento----
dados$RECONHECIMENTOdif <- dados$RECONHECIMENTO.2-dados$RECONHECIMENTO.1
shapiro.test(dados$RECONHECIMENTOdif)
shapiro.test(dados$RECONHECIMENTOdif[dados$TIPO.DO.GRUPO=="Grupo Experimental"])
shapiro.test(dados$RECONHECIMENTOdif[dados$TIPO.DO.GRUPO=="Grupo Controle"])
#Não normal. Teste de medianas
t.test(dados$RECONHECIMENTOdif[dados$TIPO.DO.GRUPO=="Grupo Experimental"],
       dados$RECONHECIMENTOdif[dados$TIPO.DO.GRUPO=="Grupo Controle"])
wilcox.test(dados$RECONHECIMENTOdif[dados$TIPO.DO.GRUPO=="Grupo Experimental"],
            dados$RECONHECIMENTOdif[dados$TIPO.DO.GRUPO=="Grupo Controle"])



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
