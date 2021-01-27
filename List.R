library("tidyverse") # коллекция пакетов от Hadley Wickham

library("lmtest") # тесты в линейных моделях
library("sandwich") # оценки ковариационной матрицы робастные к гетероскедастичности
library("erer") # подборка пакетов для эмпирических исследований
library("AUC") # подсчёт показателя AUC

library("estimatr") # модели с робастными стандартными ошибками
library("e1071") # SVM

library("quantmod")

library("GGally") # матрица диаграмм рассеяния
library("lattice") # конкурент ggplot2
library("ggplot2") # базовый графический пакет
library("vcd") # мозаичный график
library("hexbin") # график из шестиугольников
library("sjPlot") # визуализация результатов МНК
library("factoextra") # визуализация для метода главных компонент и не только
library("sm") # + графики
library("vioplot") # + графики
library("beeswarm")
library("lattice")

library("reshape2") # длинные <-> широкие таблицы
library("psych") # описательные статистики
library("skimr") # описательные статистики

library("glmnet") # LASSO
library("HSAUR")
library("sgof")
library("car") # для тестирования линейных гипотез, подсчёта vif

library("spikeslab") # байесовская регрессия пик-плато
library("quantreg") # квантильная регрессия
library("MCMCpack") # набор моделей с байесовским подходом

library("devtools") # разработка пакетов

library("caret") # подбор параметров с помощью кросс-валидации
library("AER")
library("ivpack") # инструментальные переменные


# Если результирующая переменная - бинарная, политомическая (хороший, плохой, злой)
# счетная (число ДТП за день) и если их средняя и дисперсия связаны

# Логистическая регрессия
library("AER")
library("forcats")  # работа с факторными переменными

library(readxl)
Base <- read_excel("D:/Наука/Статьи и работы начатые/С А.Н. и Надей/Tablitsa_dlya_nauchnoy_stati.xlsx")
Base <- Base[,1:10]
glimpse(Base)
Base <- mutate(Base, H = as.integer(H), degree = as.factor(degree), Status = as.factor(Status), institute = as.factor(institute), Science = as.factor(Science))
glimpse(Base)

qplot(data=Base, H, geom="density",xlim="H-index")
qplot(data=Base, M, geom="density",xlim="M-index")

plot(Base$Impact, Base$H, pch=19)
abline(lm(Base$H~Base$Impact), col="red", lwd=2, lty=1)
lines(lowess(Base$Impact, Base$H), col="blue", lwd=2, lty=2) 

scatterplot(Base$H ~ Base$Number | Base$degree, lwd=1,
            legend.plot=TRUE,
            id.method="identify",
            boxplots="xy",smooth=TRUE,regLine=FALSE)

library(qcc)
qcc.overdispersion.test(Base$H, type="poisson")
# Пуассоновская регрессия
fit_all <- glm(H ~ log(Number) + Share +Impact+ Impact:degree, data=Base, family=quasipoisson())
summary(fit_all)
exp(fit_all$coefficients)


library(MASS)
full.model <- glm(H ~ Experience + log(Number) + Share + Impact + Experience:degree*institute*Status + degree*institute*Status + log(Number):degree*institute*Status + Share:degree*institute*Status + Impact:degree*institute*Status  , data=Base, family=poisson())
step.model_2 <- stepAIC(full.model, direction = "both",  trace = FALSE)
summary(step.model_2)




quantile(Base$M)
library("quantreg")  # квантильная регрессия
full.model_1 <- lm(M ~ Experience + log(Number) + Share + Impact + Experience:degree*institute*Status + degree*institute*Status + log(Number):degree*institute*Status + Share:degree*institute*Status + Impact:degree*institute*Status  , data=Base)
step.model_1 <- stepAIC(full.model_1, direction = "both",  trace = FALSE)
summary(step.model_1)
model_q01 <- rq(data = Base, M ~ 0+Experience + log(Number) + Share + Status , tau = c(0.25))
summary(model_q01)
model_q02 <- rq(data = Base, M ~ 0+Experience + log(Number) + Share + Status , tau = c(0.5))
summary(model_q02)
model_q03 <- rq(data = Base, M ~ 0+Experience + log(Number) + Share, tau = c(0.75))
summary(model_q03)
