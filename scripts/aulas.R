# Aula 2 ------------

## Objeto para guardar vetores ------------

idade <- c(25, 28, 30, 45, 68, 100) 

class(idade)

raca <-  c("negro", "negro", "branco", "indigena", "amarelo/asiatico", "branco")

class(raca)

## Objeto para guardar bancos de dados (data.frame)

idades_banco_dados <- data.frame(idade = idade,
                                 raca = raca)

class(idades_banco_dados)


# Aula 4 - 11-04-2023 ----------

## factor

escolaridade_character <- c("ensino médio", "ensino superior", "ensino fundamental")

escolaridade <- factor(x = escolaridade_character,
                      levels = c("ensino fundamental", "ensino médio", "ensino superior"))

banco_dados <- data.frame(pessoa = c("Andressa", "Heloisa", "William", "Daniel", "Anelize"),
                          escolaridade = c("Ensino Superior", "Ensino Médio", "Ensino Médio", "Ensino Fundamental", "Ensino Fundamental"))


as.factor(banco_dados$escolaridade)
as.numeric()
as.character()
as.data.frame()

banco_dados$escolaridade <- factor(x = banco_dados$escolaridade, 
                                   levels = c("Ensino Fundamental", "Ensino Médio", "Ensino Superior"))


class(banco_dados$escolaridade)

as.numeric(banco_dados$escolaridade)

mean(as.numeric(banco_dados$escolaridade))



##logic


vetor_logico <- c(2 == 2, 3 == 1)

vetor_numeros <- c(2, 3)

vetor_numeros_nome <- ifelse(vetor_numeros == 2, "dois", "tres")

class(vetor_logico)

##list

lista <- list(dados_percepcao_inicial, c(1, 2, 3))

## Operacoes com variaveis quantitativas

### Soma

sum(idade)

idade + idade

idade - 2

### Subtracao

idade - idade

### Multiplicacao

idade*idade

idade*2

### Divisao

idade/idade

idade/2

### Quadrado

idade^2

### Raiz quadrada

sqrt(sum(idade))

### Media - medida de tendencia central

mean(idade)
mean(idade + 15)


### Mediana - medida de tendencia central

median(idade)

### Desvio-padrao - medida de dispersao

sd(idade)

sqrt(var(idade))

### Variancia - medida de dispersao

var(idade)

## Carregando banco de dados 

#install.packages("rio")

library(rio)

#import()

#read.csv()

#read.csv2()

library(readxl)

dados_percepcao_inicial <- read_xlsx("bases/percepcao_dados_2023.1.inicio.xlsx")

View(dados_percepcao_inicial)

head(dados_percepcao_inicial)


# Aula 5 - 25-04-2023 ----------

## Subsetting de vetores e data.frames

View(idades_banco_dados)
class(idades_banco_dados)
class(idades_banco_dados$idade)

### Uma posicao

idade[1]

### Mais de uma posicao

idade[c(5,1)]

### Tamanho de vetor

length(idade)

### Remocao de posicao

idade[-1]

### No banco de dados

idades_banco_dados$idade[2]

idades_banco_dados$idade[idades_banco_dados$raca == "branco" |
                         idades_banco_dados$raca == "amarelo/asiatico"]


idades_banco_dados[ ,1]


idades_banco_dados[ ,1] == idades_banco_dados$idade


# Pacote tidyverse

#install.packages("tidyverse")
#install.packages("dplyr")
library(tidyverse)
library(dplyr)

banco_filtrado <- idades_banco_dados %>%
  filter(raca == "negro") %>%
  mutate(RACA = raca) %>%
  rename("idade das pessoas" = idade)



## Renomear variaveis

glimpse(dados_percepcao_inicial) # ve o banco de dados
names(dados_percepcao_inicial) # ve os nomes das variaveis

dados_percepcao_inicial <- dados_percepcao_inicial %>%
  rename("primeiro_nome" = `First name`)

## Criacao de variaveis

dados_percepcao_inicial <- dados_percepcao_inicial %>%
  mutate(primeiro_nome = `First name`)

## Recodificar variaveis

### Recodificando uma variavel por vez

dados_percepcao_inicial <- dados_percepcao_inicial %>%
  mutate(
    atuacao_analise = case_when(
      `Você já atuou com análise quantitativa antes do curso?` == "A" ~ 1, #"sim"
      `Você já atuou com análise quantitativa antes do curso?` == "B" ~ 0, #"nao"
      T ~ as.numeric(NA)
    ),
    compreensao_dados = case_when(
      `De 1 a 4, o quanto você sente que compreende sobre dados quantitativos?` == "A" ~ 1,
      `De 1 a 4, o quanto você sente que compreende sobre dados quantitativos?` == "B" ~ 2,
      `De 1 a 4, o quanto você sente que compreende sobre dados quantitativos?` == "C" ~ 3,
      `De 1 a 4, o quanto você sente que compreende sobre dados quantitativos?` == "D" ~ 4,
      T ~ as.numeric(NA)
    )
  )



### Recodificando varias variaveis de uma so vez


dados_percepcao_inicial <- dados_percepcao_inicial %>%
  mutate(across(.cols = c(`Você já atuou com análise quantitativa antes do curso?`,
                          `De 1 a 4, o quanto você sente que compreende sobre dados quantitativos?`,
                          `O quanto você precisa atuar com dados quantitativos, hoje?`,
                          `De 1 a 4, o quanto avalia a sua base prévia em matemática?`,
                          `De 1 a 4, o quanto avalia a sua base prévia em programação (R ou outra linguagem)?`),
                .fns = ~case_when(.x == "A" ~ 1,
                                  .x == "B" ~ 2,
                                  .x == "C" ~ 3,
                                  .x == "D" ~ 4,
                                  T ~ as.numeric(NA))))

## Selecao de variaveis

dados_percepcao_inicial %>%
  select(`De 1 a 4, o quanto você sente que compreende sobre dados quantitativos?`,
        compreensao_dados) %>%
        View()


## Visualizacao de variaveis quantitativas

### Variaveis quantitativas discretas

### Tabela descritiva das variaveis discretas - table(), prop.table() e janitor::tabyl()

prop.table(table(dados_percepcao_inicial$`Você já atuou com análise quantitativa antes do curso?`))

library(janitor)

dados_percepcao_inicial %>% tabyl(`Você já atuou com análise quantitativa antes do curso?`)  %>% flextable::flextable()

### Grafico descritivo

library(ggplot2)

#### - Usando geom_bar

dados_percepcao_inicial %>%
  ggplot(aes(x = `Você já atuou com análise quantitativa antes do curso?`)) +
  geom_bar(fill = "blue", col = "black", size = 0.7) +
  theme_classic()

#### - Usando geom_bar com troca de ordem dos valores de x 

dados_percepcao_inicial %>%
  mutate(`Você já atuou com análise quantitativa antes do curso?` = factor(`Você já atuou com análise quantitativa antes do curso?`, levels = c(2, 1))) %>%
  ggplot(aes(x = `Você já atuou com análise quantitativa antes do curso?`)) +
  geom_bar(fill = "blue", col = "black", size = 0.7) +
  theme_classic()

#### Usando tabyl()
