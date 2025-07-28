install.packages("dplyr")
install.packages("ggplot2")
install.packages("corrplot")
install.packages("tidyr")
install.packages("naniar")
install.packages("GGally")

library(dplyr)      # Для манипуляции с данными
library(ggplot2)    # Для визуализаций
library(corrplot)   # Для корреляционной матрицы
library(tidyr)      # Для работы с пропусками
library(naniar)     # Для визуализации пропусков
library(GGally)

getwd()
setwd("C:/Users/79179/Desktop/ADA master/3 семестр/ком оф резалтс")

df <- read.csv('loan_data.csv')

?str
str(df) #смотрим структуру (инфо дескрайб)

?summary
summary(df)    # Статистики по числовым переменным

?is.na
colSums(is.na(df))   # Подсчитаем пропущенные значения
gg_miss_var(df)      # Визуализация пропусков в данных

sum(duplicated(df))  # Проверка на дублирующиеся строки

cor_matrix <- cor(df %>% select(where(is.numeric)))  # Расчет корреляции для числовых переменных
cor_matrix
corrplot(cor_matrix, method="circle")  # Визуализация матрицы корреляций

ggpairs(df)
# Визуализация парных графиков для всех переменных
ggpairs(df[, c('person_age', 'person_income', 'loan_amnt', 'loan_status')]) 

pairs(df)

ggplot(df, aes(x = loan_amnt)) + 
  geom_histogram(binwidth = 5000, fill = "blue", color = "black", alpha = 0.7) + 
  theme_minimal() +
  labs(title = "Loan amount distr", x = "Loan amount", y = "Freq")

ggplot(df, aes(x = person_age)) + 
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) + 
  theme_minimal() +
  labs(title = "Age distr", x = "Age", y = "Freq")

ggplot(df, aes(x = person_income)) + 
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) + 
  theme_minimal() +
  labs(title = "Income distr", x = "Income", y = "Freq")

ggplot(df, aes(x = factor(loan_status), fill = factor(person_gender))) + 
  geom_bar(position = "dodge") + 
  theme_minimal() + 
  labs(title = "Распределение статуса кредита по полу", x = "Статус кредита", y = "Частота")


