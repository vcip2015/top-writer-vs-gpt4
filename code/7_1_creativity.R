setwd("C:\\Users\\gmarc\\OneDrive - UNED\\workspace-current\\patricio_sinopsis\\data")
df <- read.csv('resultados_en_bruto.csv')

df$username <- as.factor(df$username)
df$X3_relevancia <- as.factor(df$X3_relevancia)
df$X5_autoria_titulo <- as.factor(df$X5_autoria_titulo)
df$X5_autoria_sinopsis <- as.factor(df$X5_autoria_sinopsis)
df$title <- as.factor(df$title)
df$title_id <- as.factor(df$title_id)
df$title_writer <- as.factor(df$title_writer)
df$sinopsis_writer <- as.factor(df$sinopsis_writer)
df$experiment <- as.factor(df$experiment)
df$title_id <- as.factor(df$title_id)

library(lmerTest)


mixed0 <- lmer(X4_creatividad_sinopsis ~ X1_atractivo_estilo + X1_atractivo_tema + X2_originalidad_estilo + X2_originalidad_tema + (1|title) + (1|username), data = df)
summary(mixed0)

mixed1 <- lmer(X4_creatividad_sinopsis ~ sinopsis_writer + X1_atractivo_estilo*X1_atractivo_tema*X2_originalidad_tema*X2_originalidad_estilo + (1|title) + (sinopsis_writer|username), data = df)
summary(mixed1)

mixed2 <- lmer(X4_creatividad_sinopsis ~ sinopsis_writer + X2_originalidad_titulo*X1_atractivo_titulo*X1_atractivo_estilo*X1_atractivo_tema*X2_originalidad_tema*X2_originalidad_estilo + (1|title) + (sinopsis_writer|username), data = df)
anova(mixed2, mixed1)