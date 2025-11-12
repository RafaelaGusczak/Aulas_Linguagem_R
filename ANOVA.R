# Nome: Rafaela Gusczak. Turma: 2BN.

# Exercício 1
# Uma companhia deseja testar quatro tipos diferentes de válvulas: A, B, C e D. As vidas
# médias, em horas, constam na tabela abaixo. Cada tipo foi testado, aleatoriamente, em seis
# aparelhos idênticos

# a) Crie o objeto “vida_media” e “válvula” (este como fator).
A <- c(53, 58, 56, 60, 51, 55)
B <- c(52, 60, 52, 58, 50, 54)
C <- c(51, 57, 55, 53, 54, 50)
D <- c(49, 54, 52, 50, 53, 51)

vida_media <- c(A, B, C, D)
vida_media
valvula <- factor(rep(c("A", "B", "C", "D"), each = 6))
valvula

# b) Teste se há diferença significativa entre as válvulas, ao nível de 5%. Interprete o resultado
anova_valv <- aov(vida_media ~ valvula)
summary(anova_valv)

# c) Crie um data.frame com “válvula” e “vida_media”
dados_valv <- data.frame(valvula, vida_media)

# d) Ordene o data.frame de acordo com o nome da válvula.
dados_valv_ordenado <- dados_valv[order(dados_valv$valvula), ]
dados_valv_ordenado

# Exercício 2
# São feitas cinco misturas da mesma liga metálica e para cada mistura foram efetuadas seis
medidas de densidade.

#Há evidência de que certas misturas tenham densidade média maior do que de outras? Adote α =
#5%.

A <- c(3.6, 3.5, 3.7, 3.1, 3.1, 3.2)
B <- c(3.3, 3.5, 3.4, 3.2, 3.4, 3.4)
C <- c(3.5, 3.3, 3.4, 3.4, 3.3, 3.2)
D <- c(3.5, 3.4, 3.0, 3.3, 3.3, 3.8)
E <- c(3.7, 3.4, 3.6, 3.5, 3.6, 3.4)

densidade <- c(A, B, C, D, E)
mistura <- factor(rep(c("A", "B", "C", "D", "E"), each = 6))

anova_liga <- aov(densidade ~ mistura)
summary(anova_liga)

# Exercício 3
#  Os dados a seguir, representam, em segundos, o tempo gasto por cinco operários para
# realizar certa tarefa, usando três máquinas diferentes. Considerando α = 5%, verifique se
# há diferenças entre as máquinas e entre os operários.

tempo <- c(40,59,42,
           39,55,51,
           47,55,45,
           45,50,40,
           52,52,41)

operario <- factor(rep(1:5, each = 3))
maquina <- factor(rep(c("A","B","C"), times = 5))

dados <- data.frame(operario, maquina, tempo)

anova_op <- aov(tempo ~ maquina + operario)
summary(anova_op)

# Exercício 4
#  Plantam-se quatro tipos diferentes de sementes de café em cinco tipos de solo. Cada solo
# é dividido em quatro lotes, pelos quais se distribuem, aleatoriamente, os quatro tipos de
# sementes. Ao nível de 5%, teste se a produção varia devido ao solo e/ou devido à variedade
# do café.

producao <- matrix(c(
  15,12,10,14,
  19,15,12,11,
  18,14,15,12,
  16,11,12,16,
  17,16,11,14
), ncol = 4, byrow = TRUE)

solo <- factor(rep(c("A","B","C","D","E"), each = 4))
cafe <- factor(rep(c("I","II","III","IV"), times = 5))
prod <- as.vector(producao)

dados_cafe <- data.frame(solo, cafe, prod)

anova_cafe <- aov(prod ~ solo + cafe)
summary(anova_cafe)

anova_cafe2 <- aov(prod ~ solo * cafe)
summary(anova_cafe2)



