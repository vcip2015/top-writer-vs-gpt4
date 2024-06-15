setwd("C:\\Users\\gmarc\\OneDrive - UNED\\workspace-current\\patricio_sinopsis\\data")
df <- read.csv('lmer_answers.csv')

df$question <- as.factor(df$question)
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


#df <- subset(df,df['question']=='4_creatividad_sinopsis')

library(lmerTest)


mixed0 <- lmer(answer ~ sinopsis_writer + (1|title) + (1|username), data = df)
mixed4 <- lmer(answer ~ sinopsis_writer + question + (1|title) + (1|username), data = df)
anova(mixed0,mixed4)

mixed5 <- lmer(answer ~ sinopsis_writer + question + (1|title) + (sinopsis_writer|username), data = df)
anova(mixed4,mixed5)

mixed6 <- lmer(answer ~ sinopsis_writer*question + (1|title) + (sinopsis_writer|username), data = df)
anova(mixed5,mixed6)

summary(mixed6)

mixed_final <- lmer(answer ~ sinopsis_writer*question + experiment + (1|title) + (sinopsis_writer|username), data = df)
summary(mixed_final)
