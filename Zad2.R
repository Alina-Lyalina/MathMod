# Создайте модель множественной линейной регрессии потоков паров воды за летний период 2013 года 
# по данным измерений методом турбулентной пульсации.
# Для выбора нужных суток используйте переменную DOY - день года (1 января - DOY = 1) 
#Начало - указываем рабочую директорию 

setwd ("D:/Group_125/Lyalina Alina/MathMod2") 
#проверим директорию 
getwd()
#подключаем tidyverse 
library(tidyverse) 
library(lubridate) 
#читаем данные из файла, пропускаем первую строку, заменяем текстовые 'NA', пустые и сгенерированные пороговые значения на NA, игнорируем строки с "[" 


data = read_csv("eddypro.csv", skip = 1, na =c("","NA","-9999","-9999.0"), comment=c("[")) 
#ещё одну строку убираем 
data = data[-1,] 
#убираем ненужные колонки 
data = data[, c(-1, -3, -9, -12, -15, -18, -21, -30, -35, -63 , -70, -88:-99) ] 
#преобразуем строковые значения в факторные 
data = data %>% mutate_if(is.character, factor) 
#заменяем конфликтующие знаки колонок 
names(data) = names(data) %>% str_replace_all("[!]","_emph_") %>% 
  str_replace_all("[?]","_quest_") %>% 
  str_replace_all("[*]","_star_") %>% 
  str_replace_all("[+]","_plus_") %>% 
  str_replace_all("[-]","_minus_") %>% 
  str_replace_all("[@]","_at_") %>% 
  str_replace_all("[$]","_dollar_") %>% 
  str_replace_all("[#]","_hash_") %>% 
  str_replace_all("[/]","_div_") %>% 
  str_replace_all("[%]","_perc_") %>% 
  str_replace_all("[&]","_amp_") %>% 
  str_replace_all("[\\^]","_power_") %>% 
  str_replace_all("[()]","_") 
#Посмотрим, что получилось 
glimpse(data) 
#оставим данные только по летнему периоду 2013 года: 
data = data[data$DOY>=152 & data$DOY<=243 & year(data$date) == 2013, c(1:ncol(data))] 
#выберем все переменные типа numeric 
data_numeric = data[,sapply(data,is.numeric) ] 
#все остальные переменные: 
data_non_numeric = data[,!sapply(data,is.numeric) ] 
# создадим матрицу для корелляционного анализа и преобразуем ее в таблицу, выбрав нужный столбец (потоки паров воды) 
cor_td = cor(drop_na(data_numeric)) %>% as.data.frame %>% select(h2o_flux) 
#выберем имена переменных (строк) с коэффициентом детерминации больше 0.2 
vars = row.names(cor_td)[cor_td$h2o_flux^2 > .2] %>% na.exclude; vars 
#соберем переменные из вектора в одну формулу: 
formula = as.formula(paste("h2o_flux~", paste(vars,collapse = "+"), sep="")); formula 
#создадим обучающую и тестирующую выборки: 
row_numbers = 1:length(data$date) 
teach = sample(row_numbers, floor(length(data$date)*.7)) 
test = row_numbers[-teach] 
#непересекающиеся подвыборки: 
teaching_tbl_unq = data[teach,] 
testing_tbl_unq = data[test,] 
# МОДЕЛЬ 1 
#создаем модель линейной регрессии 
model = lm(formula, data = data);model 
#коэффициенты 
coef(model) 
#остатки 
resid(model) 
#доверительный интервал 
confint(model) 
#P-значения по модели 
summary(model) 
#дисперсионный анализ 
anova(model) 

# МОДЕЛЬ 2 
formula2 = as.formula(paste("h2o_flux~", "(", paste(vars,collapse = "+"), ")^2", sep="", collapse = NULL));formula2 
#создаем модель линейной регрессии 
model2 = lm(formula2, data = data);model2 
#коэффициенты 
coef(model2) 
#остатки 
resid(model2) 
#доверительный интервал 
confint(model2) 
#P-значения по модели 
summary(model2) 
#дисперсионный анализ 
anova(model2) 

#МОДЕЛЬ 3
formula3 = h2o_flux~(Tau + rand_err_Tau + H + rand_err_H + LE + rand_err_LE+co2_flux + rand_err_h2o_flux +h2o_time_lag+ sonic_temperature + air_temperature + air_density +air_molar_volume+es+ RH+VPD+max_speed+u_star_ + TKE + T_star_ + un_Tau + un_H+un_LE+un_co2_flux+un_h2o_flux + w_div_co2_cov+w_div_h2o_cov)^2; formula3
#создаем модель линейной регрессии 
model3 = lm(formula3, data = data);model 
#коэффициенты 
coef(model3) 
#остатки 
resid(model3) 
#доверительный интервал 
confint(model3) 
#P-значения по модели 
summary(model3) 
#дисперсионный анализ 
anova(model3) 

#МОДЕЛЬ 4
formula4 = h2o_flux~(Tau + rand_err_Tau + H + rand_err_H + LE);formula4
#создаем модель линейной регрессии 
model4 = lm(formula4, data = data);model4 
#коэффициенты 
coef(model4) 
#остатки 
resid(model4) 
#доверительный интервал 
confint(model4) 
#P-значения по модели 
summary(model4) 
#дисперсионный анализ 
anova(model4) 

#Создадим таблицу, в котором оставим только значимые переменные
data2 = select(data, Tau , rand_err_Tau , H , rand_err_H , LE)
#Посчитаем коэффициенты корреляции для всей таблицы данных
cor(drop_na(data2)) 

#МОДЕЛЬ 5

formula5 = h2o_flux~(Tau + H + LE);formula5
#создаем модель линейной регрессии 
model5 = lm(formula5, data = data);model5 
#коэффициенты 
coef(model5) 
#остатки 
resid(model5) 
#доверительный интервал 
confint(model5) 
#P-значения по модели 
summary(model5) 
#дисперсионный анализ 
anova(model5)

#МОДЕЛЬ 6

formula6 = h2o_flux~(Tau + H + LE)^2;formula6
#создаем модель линейной регрессии 
model6 = lm(formula6, data = data);model6
#коэффициенты 
coef(model6) 
#остатки 
resid(model6) 
#доверительный интервал 
confint(model6) 
#P-значения по модели 
summary(model6) 
#дисперсионный анализ 
anova(model6)


#МОДЕЛЬ 7

formula7 = h2o_flux~(Tau + H + LE + Tau:H + H:LE);formula7
#создаем модель линейной регрессии 
model7 = lm(formula7, data = data);model7
#коэффициенты 
coef(model7) 
#остатки 
resid(model7) 
#доверительный интервал 
confint(model7) 
#P-значения по модели 
summary(model7) 
#дисперсионный анализ 
anova(model7)