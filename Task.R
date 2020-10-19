library(ggplot2) ## Визуализация
library(dplyr,warn.conflicts = FALSE) ## Манимуляции даннными
library(GGally)
library(mice) ## Обучение 
library(rpart)  ## Визуализация
library(rpart.plot)  ## Визуализация

# Загрузка данных 
df <- read.csv(file = "Dataset_for_student-1.csv", header = TRUE)

# Задание 1
# Поверка пустых значений 
colSums(is.na(df))
colSums(df=="")

# Задание 2
ggplot(df, aes(x = factor(Survived), fill = factor(Survived))) + 
geom_bar() + 
geom_text(aes(label = stat(count)), stat='count', vjust = -0.5, 
          color = "black", size = 3) + 
xlab("Survived") + theme_classic() +  theme(legend.position="none")

# Задание 3
ggplot(df, aes(x = Sex, fill = factor(Sex))) + 
  geom_bar() + 
  geom_text(aes(label = stat(count)),stat='count', vjust = -0.5, 
            color = "black", size = 3) +
  theme_classic() +  theme(legend.position="none")

# Задание 4 
table(factor(df$Survived), factor(df$Sex)) # Таблица сопряженности 

# Задание 5
ggplot(df, aes(x = factor(Survived), group = Pclass, fill = factor(..x..))) + 
  geom_bar(aes(y = ..prop..), stat="count") + labs(title  = "Pclass") +
  geom_text(aes( label = scales::percent(..prop..), y= ..prop.. ), 
            stat= "count", vjust = -0.5) +
  scale_y_continuous(labels=scales::percent) +
  ylab("percent of people") +
  xlab("Survived") +
  facet_grid(~Pclass) +
  theme_classic() +  theme(legend.position="none")

# Задание 6
# Формирование нового столбца и преведение к типу Factor
df$age_group <- df$Age %/% 10 + 1
df[,"age_group"] <- as.factor(df[,"age_group"])

# Построение столбчатой диаграммы
ggplot(data = subset(df, !is.na(df$age_group)), aes(x = factor(age_group), 
                                                    fill = factor(Survived))) +
  geom_bar(position = position_dodge()) +
  geom_text(aes(label=stat(count)), stat='count', vjust = -0.5, color = "black", 
            size = 3, position=position_dodge(width=1)) + 
  xlab("Group of age") +
  theme_classic() +  theme(legend.position="none")

# Задание 7
# Заполнение пропусков в данных с использованием RandomForest
miceMod <- mice(df[, !names(df) %in% 
                     c("Ticket", "Cabin", "Survived", "Pclass", "Sex")],
                method = "rf", seed = 256)
mice_output <- complete(miceMod)

#Визуальное сравнение распределения 
par(mfrow = c(1, 2))
hist(df$Age, breaks = "Sturges",
     freq = NULL, xlab = "Age", main =  "Histogram of Age before input")
hist(mice_output$Age, breaks = "Sturges",
     freq = NULL, xlab = "Age", main =  "Histogram of Age after input" )

# Замена столбцов 
df$Age -> mice_output$Age
df$age_group -> mice_output$age_group