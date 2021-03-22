library(tidyverse)
glimpse(mes0619)
str(mes0619)
 
dados <- mes0619 %>% filter(!is.na(Tipo_Regiao), !is.na(Desc_Renda_Bruta))
write.csv2(dados, "dados.csv")

indices <- sample(1:nrow(dados),floor(nrow(dados)*.8))
indices

treino <- dados[indices,]
teste <- dados[-indices,]

GLM.1 <- glm(status ~ Qtde_dependentes + SEXO + Tipo_Regiao + Desc_Renda_Bruta + id_ini_serv_pub, 
             family=binomial(logit), data= treino)
summary(GLM.1)
exp(coef(GLM.1))  # Exponentiated coefficients ("odds ratios")


modfinal <- step(GLM.1,scope = list(lower=~1))
summary(modfinal)
modfinal$fitted.values

tabela <- table(modfinal$fitted.values>.5,treino$status)
tabela

sum(diag(tabela))/sum(tabela)

install.packages("ROCR")




previsao_teste <- predict(modfinal,teste,"response")
tabelateste <- table(previsao_teste>.35,teste$status)
sum(diag(tabelateste))/sum(tabelateste)



length(modfinal$fitted.values)
length(treino$status)

any(is.na(dados$SEXO))
any(is.na(dados$Tipo_Regiao))
any(is.na(dados$Qtde_dependentes))
any(is.na(dados$Desc_Renda_Bruta))
any(is.na(dados$id_ini_serv_pub))


dados1 <- mes0619[!is.na(mes0619$Tipo_Regiao), !is.na(mes0619$Desc_Renda_Bruta),]



