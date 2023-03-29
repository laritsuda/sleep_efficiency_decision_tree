# Instalação e carregamento dos pacotes utilizados
pacotes <- c("tidyverse",
             "rpart",
             "reshape",
             "rpart.plot",
             "readxl",
             "caTools",
             "fastDummies",
             "caret",
             "pROC",
             "plotly")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

set.seed(42)

# Carregando a base de dados
sleep <- read.csv("~/Rprojects/Sleep_Efficiency/Sleep_Efficiency_2.csv", header=TRUE, stringsAsFactors=FALSE)

# analise das linhas e colunas
glimpse(sleep)

# Vamos identificar as variáveis categóricas como "factors"
sleep$Gender <- as.factor(sleep$Gender)
sleep$Smoking.status <- as.factor(sleep$Smoking.status)
sleep$Is_Bedtime_after_0am <- as.factor(sleep$Is_Bedtime_after_0am)
sleep$Wakeup_time_before_6am <- as.factor(sleep$Wakeup_time_before_6am)

# Vamos analisar algumas estatísticas descritivas da base de dados
summary(sleep)

# REMOVE TIMESTAMP AND ID COLUMNS 
sleep = sleep[,-5:-6]
sleep = sleep[,-1:-2]


#removendo registros com valores nulos
sleep<-na.omit(sleep)

# Verificando a distribuição e a densidade da nossa variável dependente

dens <- density(sleep$Sleep.efficiency)
plot(dens, main="Distribution of the sleep efficiency of observations")

#Criando uma coluna id 
sleep$id <- 1:nrow(sleep)

# Separação da base de dados em bases de treino e teste

treino <- sleep %>% sample_frac(0.80)
teste  <- anti_join(sleep, treino, by = 'id')

# Retirando a coluna id que criamos na etapa anterior

treino$id <- NULL
teste$id <- NULL


# VALORES DEFAULT:
# minsplit = 20
# cp = 0.01
# maxdepth = 30

arvore <- rpart(formula = Sleep.efficiency ~ .,
                data = treino,
                method = "anova",
                control = rpart.control(maxdepth = 3,
                                        cp = 0) # a argumento de complexidade precisa estar zerado para controlar 
                # a profundidade da árvore
)

# method = "anova" indica que trata-se de uma árvore de regressão

# Plotando a árvore
rpart.plot(arvore, type = 4, clip.right.labs = F, extra = 101, nn = T)

# Alguns parâmetros dos resultados
summary(arvore)
plotcp(arvore)
printcp(arvore)

# Importância das variáveis
importancia_variaveis <- data.frame(imp = arvore$variable.importance)

importancia_variaveis <- importancia_variaveis %>% 
  tibble::rownames_to_column() %>% 
  dplyr::rename("variable" = rowname) %>% 
  dplyr::arrange(imp) %>%
  dplyr::mutate(variable = forcats::fct_inorder(variable))

ggplot(importancia_variaveis) +
  geom_segment(aes(x = variable, y = 0, xend = variable, yend = imp), 
               size = 1.5, alpha = 0.7) +
  geom_point(aes(x = variable, y = imp, col = variable), 
             size = 4, show.legend = F) +
  coord_flip() +
  theme_bw()


# Valores preditos pela árvore (base de treino)
pred_treino <- predict(arvore, treino)

# Verificando os valores únicos
unique(pred_treino)

# Identificando os erros em cada predição
erros_treino <- treino$Sleep.efficiency - pred_treino

# Criando informação dos valores observados x preditos
obs_treino <- treino$Sleep.efficiency
data_resultados_treino <- data.frame(obs=obs_treino, pred=pred_treino)

# Identificando o R² na base de treino
round(defaultSummary(data_resultados_treino), 3)

### VERIFICANDO A CAPACIDADE DE GENERALIZACAO DA ARVORE

# Valores preditos pela árvore (base de teste)
pred <- predict(arvore, teste)

# Identificando os erros em cada predição
erros_teste <- teste$Sleep.efficiency - pred

# Criando informação dos valores observados x preditos
obs <- teste$Sleep.efficiency
data_resultados_teste <- data.frame(obs, pred)

# Identificando o R² na base de teste
round(defaultSummary(data_resultados_teste), 3)

# Visualizando melhor esse resultado
ggplot(data_resultados_teste, aes(x=data_resultados_teste$obs, y=data_resultados_teste$pred)) +
  geom_point(color="red", size=3) +
  geom_abline()

# E possivel notar claramente os pontos finais da árvore que visualizamos no gráfico do plot da árvore

# Verificando a distribuicao dos residuos
d <- density(erros_teste)
plot(d, main="Densidade da distribuicao")

# Validação cruzada

# Divide o dataset em 5 partes iguais sendo 1 para teste e 4 para treino em 
# 5 divisoes diferentes o resultado da validação vai ser a média de cada 
# uma das árvores geradas
# porém a gente não precisa se preocupar com a validação cruzada nesse caso, 
# porque a própria biblioteca já faz isso com k=10

# Grid Search

# Parametrizando o grid
grid <- expand.grid(cp = c(0.2, 0.15, 0.1, 0.05, 0),
                    maxdepth = seq(from = 1, to = 10, by = 1))

# Criando uma lista para armazenar os resultados
modelos <- list()
erros <- list()

# Gerando um processo iterativo
for (i in 1:nrow(grid)) {
  
  # Coletando os parâmtros do grid
  cp <- grid$cp[i]
  maxdepth <- grid$maxdepth[i]
  
  # Estimando os modelos e armazenando os resultados
  modelos[[i]] <- rpart(
    formula = Sleep.efficiency ~ .,
    data    = treino,
    method  = "anova",
    control = list(cp = cp, maxdepth = maxdepth))
}

# Função para coletar parâmetro cp dos modelos
coleta_cp <- function(x) {
  min <- which.min(x$cptable[, "xerror"])
  cp <- x$cptable[min, "CP"] 
}

# Função para coletar o erro mínimo dos modelos
coleta_erro <- function(x) {
  min <- which.min(x$cptable[, "xerror"])
  xerror <- x$cptable[min, "xerror"] 
}

# Analisando os melhores resultados

grid %>%
  mutate(
    cp = purrr::map_dbl(modelos, coleta_cp),
    erro = purrr::map_dbl(modelos, coleta_erro)
  ) %>%
  arrange(erro) %>%
  slice_head(n = 5)

arvore_grid <- rpart(formula = Sleep.efficiency ~ .,
                     data = treino,
                     method = 'anova',
                     control = rpart.control(cp = 0.002864400, 
                                             maxdepth = 7))


# Plotando a árvore
rpart.plot(arvore_grid, type = 4, clip.right.labs = F, extra = 101, nn = T)

# Visualizando o plot do cp, esse treinamento poderia ser custoso em grandes volumes de dados
# ou gerar um overfitting

plotcp(arvore_grid)

# Visualizando resultados utilizando a base de treino

# Criando informação dos valores observados x preditos para a base de treino

obs <- treino$Sleep.efficiency
pred <- predict(arvore_grid, treino)
resultados_grid_treino <- data.frame(obs, pred)

# Identificando as informações de erros na base de treino

round(defaultSummary(resultados_grid_treino), 3)

# Porém para identificar a resultado do modelo e sua capacidade de generalização, precisamos 
# observar os resultados da base de teste

# Criando informação dos valores observados x preditos para a base de teste
obs <- teste$Sleep.efficiency
pred <- predict(arvore_grid, teste)
data_grid <- data.frame(obs, pred)

# Identificando o R²
round(defaultSummary(data_grid), 3)

# Identificando os erros em cada predição
erros_melhor_modelo <- teste$Sleep.efficiency - pred

# Comparando a distribuicao dos residuos dos dois modelos
w <- data.frame(erros_primeira_arvore=erros_teste,
                erros_melhor_modelo=erros_melhor_modelo)
w.plot <- melt(w) 
p <- ggplot(aes(x=value, colour=variable), data=w.plot)
p + geom_density()

# Comparando os modelos observados versus predict
dat <- data.frame(x = teste$Sleep.efficiency, 
                  arvore_inicial = data_resultados_teste$pred, 
                  arvore_grid = data_grid$pred)

dat.m <- melt(dat, id.vars = "x")

ggplot(dat.m, aes(x, value, colour = variable, alpha=0.5)) +
  geom_point(size=3) +
  scale_colour_manual(values = c("red", "blue")) +
  geom_abline()


# Utilizando o modelo final para fazer um previsão de resultado
nova_observacao <- data.frame(Deep.sleep.percentage = 45, 
                              Light.sleep.percentage=16, 
                              REM.sleep.percentage=11, 
                              Awakenings=5, 
                              Alcohol.consumption=4, 
                              Smoking.status=0, 
                              Age=26,
                              Gender="Female",
                              Sleep.duration=5,
                              Caffeine.consumption=0,
                              Exercise.frequency=2,
                              Is_Bedtime_after_0am=1,
                              Wakeup_time_before_6am=0
                              )

predict(arvore_grid, nova_observacao)

# Salvar o modelo para consumo posterior
saveRDS(arvore_grid, file = "modelo_salvo.rds")

modelo_salvo <- readRDS("modelo_salvo.rds")
