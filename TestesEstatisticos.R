# Nome: Rafaela Gusczak
# Turma: 2BN

Carga_no_ponto_de_falha <- read.csv("/cloud/project/Carga_no_ponto_de_falha.txt", sep="")

# 1) Um artigo do periódico Materials Engineering descreve os resultados de teste de tensão
quanto à adesão em 22 corpos de prova de liga U-700. A carga no ponto de falha do corpo
de prova é dada pelo arquivo “carga no ponto de falha.txt”. Verifique se os dados sugerem
que a carga média na falha excede 10 MPa. Considere o nível de significância de 5%.
Interprete o resultado.

t.test(Carga_no_ponto_de_falha$Carga, 
       mu = 10, conf.level = 0.95, 
       alternative = "greater")

# 2) Dois catalisadores estão sendo analisados para determinar como eles afetam o rendimento
médio de um processo químico. Especificamente, o catalisador 1 está corretamente em
uso, mas o catalisador 2 é aceitável. Uma vez que o catalisador 2 é mais barato, ele deve
ser adotado, desde que ele não mude o rendimento do processo. Um teste é feito em uma
planta piloto, resultando nos dados do arquivo “catalisadores”. Há alguma diferença entre
os rendimentos médios? Use α = 0,05 e considere variâncias iguais. Formule antes as
hipóteses.

library(readxl)
catalisadores <- read_excel("catalisadores.xlsx", col_types = c("numeric", "numeric"))

t.test(catalisadores$catalisador_1, catalisadores$catalisador_2,
       var.equal = T, conf.level = 0.95,
       alternative = "two.sided")

# 3)
Uma companhia fabrica propulsores para uso em motores de turbinas de avião. Uma das
operações envolve esmerilhar o acabamento de uma superfície particular para um
componente de liga de titânio. Dois processos diferentes para esmerilhar podem ser
usados, podendo produzir peças com iguais rugosidades médias na superfície. Uma
amostra aleatória de n1 = 11 peças, proveniente do primeiro processo, resulta em um
desvio-padrão de s1 = 5,1 micropolegadas. Uma amostra aleatória de n2 = 16 peças,
proveniente do segundo processo, resulta em um desvio-padrão de s2 = 4,7
micropolegadas. Verifique se a razão entre as duas variâncias é diferente de 1 com um
nível de confiança de 90%. Considere que os dois processos sejam diferentes e a
rugosidade na superfície seja normalmente distribuída.

X<-rnorm(11, sd = 5.1)
X

Y<-rnorm(16, sd = 4.7)
Y

var.test(X, Y, ratio = 1, alternative = "t", conf.level = 0.9)

# 4) Verifique se os dados do arquivo “carga no ponto de falha” segue distribuição normal.

shapiro.test(Carga_no_ponto_de_falha$Carga)

qqnorm(Carga_no_ponto_de_falha$Carga)
qqline(Carga_no_ponto_de_falha$Carga)

# 5) Verifique se os dados do arquivo “catalisadores” seguem distribuição normal.

# Catalisador 1
shapiro.test(catalisadores$catalisador_1)

qqnorm(catalisadores$catalisador_1)
qqline(catalisadores$catalisador_1)

# Catalisador 2
shapiro.test(catalisadores$catalisador_2)

qqnorm(catalisadores$catalisador_2)
qqline(catalisadores$catalisador_2)
