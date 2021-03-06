######ANALISE DE DADOS######
#####LISTA 5
#####ANTONIO FERNANDES

###QUESTAO 5 - LISTA I

Emp <- data.frame("EMP" = 1:15, "MES" = c(8,9,4,5,3,6,8,6,6,8,5,5,6,4,4), 
                  "SET" = c('C','C','I','I','I','C','C','I','I','C','C','I','C','I','I'), 
                  "TAM" = c('G','M','G','M','M','P','G','M','P','M','P','P','M','M','G')) ##Criando Banco

  ##Divindo o banco entre comercio e industria

C <- Emp[ which(Emp$SET == "C"),]  ##Comercio
I <- Emp[ which(Emp$SET == "I"),]  ##Industria

  #Media e mediana de cada grupo

mean(C$MES) ##Média = 7.14
mean(I$MES) ##Média = 4.62

median(C$MES) ##Mediana = 8
median(I$MES) ##Mediana = 4.5

  #Desvio Padrão de cada grupo

sd(C$MES) ##Sd = 1.46
sd(I$MES) ##Sd = 1.06

    ##Identificando 25 decil

fivenum(Emp$MES) ## 25 decil igual a 4.5 (n maximo de meses)

    ##Calculando media, mediana e desvio padrao de acordo com o tamanho da empresa

G <- Emp[ which(Emp$TAM == "G"),]  ##Tamanho grande
M <- Emp[ which(Emp$TAM == "M"),]  ##Tamanho médio
P <- Emp[ which(Emp$TAM == "P"),]  ##Tamanho pequeno

median(G$MES) ##Mediana empresa grande = 6
median(M$MES) ##Mediana empresa media = 6
median(P$MES) ##Mediana empresa pequena = 5.5

mean(G$MES) ##Media empresa grande = 6
mean(M$MES) ##Media empresa media = 5.86
mean(P$MES) ##Media empresa pequena = 5.5

sd(G$MES) ##sd empresa grande = 2.30
sd(M$MES) ##sd empresa media = 2.12
sd(P$MES) ##sd empresa pequena = 0.58


###QUESTAO 6 - LISTA I

Inv <- data.frame("CID" = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J"),
                  "INV" = c(26,16, 14,10, 19, 15, 19, 16, 19, 18)) ##Criando banco

mean(Inv$INV) ##Investimento médio = 17.2

s <- sd(Inv$INV) ##Desvio padrao = 4.18

mean(Inv$INV) - (2*s) ##Identificando valor para programa especial == 8.83

mean(Inv$INV) - (2*s) ##Valor minimo investimento básico
mean(Inv$INV) + (2*s) ##Valor maximo investimento basico

invba <- Inv[ which(Inv$INV < 25.56 
                         & Inv$INV > 8.83), ] ##Selecionando casos com investimento básico

mean(invba$INV) #media investimento basico = 16.22


###QUESTAO 7 - LISTA I

Est <- data.frame("IND" = 1:20, "A" = c(55,2,13,11,23,2,15,12,14,28,12,45,19,30,16,12,7,13,1,7),
                  "B" =  c(20,7,6,5,3,25,5,3,3,10,8,5,1,35,9,8,12,2,26,NA)) ##Criando Banco

mean(Est$A) ##Media estimulo A = 16.85
mean(Est$B, na.rm = TRUE) ##Media estimulo B = 10.16

median(Est$A) ##Mediana estimulo A = 13
median(Est$B, na.rm = TRUE) ##Mediana estimulo B  = 7

sd(Est$A) ##Desvio padrao estimulo A = 13.80
sd(Est$B, na.rm = TRUE) ##Desvio padrao estimulo B = 9.45

boxplot(Est$A, Est$B) ##Boxplot com os dois estimulos



##QUESTAO 8 - LISTA I

fam <- data.frame("FAM" = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J"), "REN" = 
                    c(12,16,18,20,28,30,40,48,50,54), "SAU" = c(7.2,7.4,7,6.5,6.6,6.7,6,5.6,6,5.5))

mean(fam$REN) #Media renda familia = 31.6
mean(fam$SAU) #Media percentual gasto com saude = 6.45

sd(fam$REN) #sd renda familia = 15.42
sd(fam$SAU) #sd percentual gasto com saude = 0.66

cov(fam$REN, fam$SAU) #cov entre as duas variaveis = -9.53

cor(fam$REN, fam$SAU) #correlacao entre as duas variaveis -0.94

##QUESTAO 9 - LISTA I 

not <- data.frame("ALU" = c("A", "B", "C", "D", "E", "F", "G", "H", "I"), "PI" = 
                    c(7.5,8.2,8.5,8.7,8.8,9.1,9.2,9.3,10), "P2" = c(8.2, 8,8.3,
                                                                    8.5,9.4,9.6,9,9.3,9.7)) ##Criando banco

cor(not$PI, not$P2) ##correlacao entre as duas variaveis = 0.83


##QUESTÃO 5 - LISTA II

Banco <- data.frame("RES" = 1:1000) ##Criando o banco

Banco$valor <- ifelse(Banco$RES <= 620, 1, 0) ##Até 620 atribuir valor 1 e depois valor 0

mean(Banco$valor) ## média

sd(Banco$valor) ##desvio padrão

error <- qnorm(0.975) ##identificando valor Z para 95%

0.62 + (error*0.486/sqrt(1000)) ##Valor máximo dentro do IC de 95
0.62 - (error*0.486/sqrt(1000)) ##Valor minimo dentro do IC de 95

##QUESTÃO 6 - LISTA II

###Letra a

Zs <- 1.96^2 ##Colocando Z ao quadrado
E <- 0.05^2  ##Calculando erro ao quadrado

0.5*(1-0.5) ##Executando parte da fórmula para obter valor final com p

n <- Zs* 0.25/E ##Obtendo o valor de N com erro de 0.05


##Letra b

E <- 0.02^2 ##Caclulando erro ao quadro de 0.02

n <- Zs* 0.25/E ##Obtendo o valor de N com erro de 0.02


##Letra c

0.25*(1-0.25) ##Executando parte da fórmula para obter valor final com p

n <- Zs* 0.1875/E ##Obtendo o valor de N com erro de 0.02 e informação de 25% dos eleitores

##Letra d

Banco <- data.frame("Elet" = 1:2401) ##Criando o banco

Banco$valor <- ifelse(Banco$Elet <= 564, 1, 0) ##Até 564 atribuir valor 1 e depois valor 0

mean(Banco$valor) ##media
sd(Banco$valor) ##Desvio padrao

error <- qnorm(0.975) ##identificando valor Z para 95%

0.235 + (error*0.424/sqrt(2401)) ##Valor máximo dentro do IC de 95
0.235 - (error*0.424/sqrt(2401)) ##Valor minimo dentro do IC de 95

###QUESTÃO 11 - LISTA II

droga <- matrix(cbind(c(450,100),c(150,300)), nrow = 2, 
                dimnames = list(c("FAV","CONT"), c("ESQ","DIR"))) ##Criando matriz com as informações

options(scipen=999)

chisq.test(droga) ##executando o teste qui-quadrado

##Nesse caso, podemos rejeitar a hipótese nula de que não há associação entre ideologia e o voto




###QUESTÃO 12 - LISTA II

election <- data.frame("YEAR" = seq(from = 1964, to = 2006, by = 2 ), "HOUSE" = 
                    c(87,88,97,85,94,88,96,94,91,90,95,98,98,96,88,90,94,98,98,96,98,94), 
                    "SEN" = c(85,88,71,77,74,85,64,60,55,93,90,75,85,96,83,92,91,90,79,86,96,79)) ##Criando banco

election$SCANDAL <- ifelse(election$YEAR <= 1972, 0,1) ##Criano variável do escândalo

t.test(election$HOUSE ~ election$SCANDAL) ##teste t comparando antes e depois do escândalo para a câmara

t.test(election$SEN ~ election$SCANDAL) ##teste t comparando antes e depois do escândalo para o senado

##Nesse caso, não é possível rejeitar a hipóse nula de que não há diferença entre o período 
##antes e depois do escândalo. 


###QUESTÃO 13 - LISTA II

vote <- data.frame("YEAR" = seq(from = 1876, to = 1932, by = 4 ), "GROWTH" = 
                         c(5.11,3.879,1.589,-5.553, 2.763,-10.024,-1.425,-2.421,-6.281,4.164,2.229,-11.463,-3.872,4.623, -14.586), 
                       "VOTES" = c(48.516,50.22,49.846,50.414,48.268,47.76,53.171,60.006,54.483,54.708,51.682,36.148,58.263,58.756,40.851)) ##Criando banco

cor.test(vote$GROWTH, vote$VOTES) ##correlação entre votos e crescimento


