А/В тест 3 (максимально наближений до реального)

Примітка: дані не є результатом реального експерименту, тому ефект змодельовано
штучно

0. Пакети

library(dplyr)
library(ggplot2)
library(effectsize)
library(pwr)

1. Завантаження даних та створення метрики

ab_3 <- read.csv("a_b_testing_3.csv")

str(ab_3)
head(ab_3)
nrow(ab_3)
summary(ab_3$avg_freight)\

ab_3$avg_freight <- as.numeric(ab_3$avg_freight)

ab_3 <- ab_3 %>% filter(!is.na(avg_freight), avg_freight > 0)

Одиницею аналізу є замовлення (order_id), що забезпечує незалежність спостережень.

2. Випадковий спліт на А/В

set.seed(513565)

ab_3$group <- sample(c("A","B"), size = nrow(ab_3), replace = T)

Рандомізація виконана на рівні замовлень (order_id), що відповідає одиниці аналізу.

3. Перевірка балансу груп

3.1 Простий баланс по кількості

table(ab_3$group)
prop.table(table(ab_3$group))

3.2 SRM-перевірка

chisq.test(table(ab_3$group), p = c(0.5, 0.5))

SRM не виявлено (р = 0,7024), розподіл груп коректний

3.3 Базовий баланс по метриці до treatment

ab_3 %>% group_by(group) %>% summarise(n = n(), mean_avg_freight = mean(avg_freight),
                                       median_avg_freight = median(avg_freight),
                                       sd_avg_freight = sd(avg_freight))

4. А/А тест

aa_test <- t.test(avg_freight~group, data = ab_3)
aa_test

А/А тест не виявив відмінностей між групами (р = 0,5315), що підтверджує корректність
рандомізації.

5. Швидка перевірка розподілу 

hist(ab_3$avg_freight)

boxplot(avg_freight~group, data = ab_3)

Опційно: percentiles

ab_3 %>% group_by(group) %>% summarise(p50 = quantile(avg_freight, 0.5),
                                       p90 = quantile(avg_freight, 0.9),
                                       p95 = quantile(avg_freight, 0.95),
                                       p99 = quantile(avg_freight, 0.99))

6. Задаємо treatment ефект

effect <- 0.1

ab_3$avg_freight_ab <- ifelse(ab_3$group == "B", ab_3$avg_freight * (1+effect),
                              ab_3$avg_freight)

Перевірка середніх

ab_3 %>% group_by(group) %>% summarise(mean_avg_freight_ab = mean(avg_freight_ab),
                                       median_avg_freight_ab = median(avg_freight_ab),
                                       sd_avg_freight_ab = sd(avg_freight_ab))

У даному прикладі treatment effect задано як фіксоване відсоткове збільшення, що
є спрощенням. У реальних умовах ефект зазвичай є неоднорідним.

7. Основний А/В тест 

ab_test <- t.test(avg_freight_ab~group, data = ab_3)
ab_test

Показник р становить 0,7699, що значить, що немає суттєвої статистично значущої
різниці між групами. 

ab_test$conf.int

Довірчий інтервал включає 0, що означає відсутність статистично значущої різниці
між групами на обраному рівні значущості.

8. Effect size

cohens_d(avg_freight_ab~group, data = ab_3)

Розмір ефекту є малим - -0,02. Довірчий інтервал [-0.16, 0.12] включає нуль, що
свідчить про відсутність статистично значущого ефекту. Різниця між групами є 
незначною як з статистичної, так і з практичної точки зору.

9. Оцінка power/MDE

9.1 Оцінимо pooled_sd

sd_pool <- ab_3 %>% summarise(sd_pool = sd(avg_freight)) %>% pull(sd_pool)

Стандартне відхилення метрики становить 116.8, що свідчить про високий рівень 
варіативності в даних.

9.2 Розмір груп

n_A <- sum(ab_3$group == "A")
n_B <- sum(ab_3$group == "B")

n_A
n_B

9.3 Приблизний MDE для power 80%

n_min <- min(n_A, n_B)

mde_d <- pwr.t.test(n = n_min, sig.level = 0.05, power = 0.8, type = "two.sample",
                    alternative = "two.sided")$d

Отже, мінімально виявлений ефект MDE становить 0,196, тобто тест здатен виявити
різницю між групами лише у випадку, якщо вона перевищує приблизно 19,6% від 
стандартного відхилення

Переведемо це в гроші/абсолютну різницю

mde_abs <- mde_d * sd_pool

У грощовому вираженні мінімально виявлена різниця MDE ствановить приблизно 22,92,
тобто тест здатен зафіксувати ефект лише якщо різниця між групами перевищує 23
одиниці. 

І в процентах від середнього чеку

baseline_mean <- mean(ab_3$avg_freight)

mde_pct <- mde_abs / baseline_mean

ОТже, мінімально виявлений ефект становить приблизно 29,2% від середнього значення
метрики, тобто тест зафіксує різницю лише якщо вона перевищує приблизно 30%.

10. Готовий короткий бізнес висновок

mean_A <- mean(ab_3$avg_freight_ab[ab_3$group == "A"])
mean_B <- mean(ab_3$avg_freight_ab[ab_3$group == "B"])
uplift <- (mean_B - mean_A)/ mean_A

mean_A
mean_B
uplift

Спостережуваний ефект становить приблизно 3,09%, тобто середнє значення метрики
в групі В є на 3,09% вищим порівняно з групою А.

Висновок

Було проведено Welch t-test для порівняння середньої вартості доставки між групами
A та B. У групі B спостерігається uplift на 3.09%, однак статистично значущої 
різниці не виявлено (p = 0.7699). За поточної варіативності та розміру вибірки 
тест має MDE близько 29.2% від середнього значення метрики, тому не має достатньої
потужності для надійного виявлення настільки малого ефекту. Отже, результати не
підтверджують ефективність змін, але й не дозволяють впевнено виключити наявність
невеликого позитивного ефекту.

Далі проведемо аналіз guardrail-метрики для перевірки відсутності негативних 
побічних ефектів

11. Аналіз guardrail-метрики

11.1 Створення метрики 

ab_3$delivery_day <- as.numeric(ab_3$delivery_day)

ab_3 <- ab_3 %>% filter(!is.na(delivery_day), delivery_day > 0)

# використовуємо ті самі групи, що й в основному тесті.

11.2 Перевірка балансу груп. 

table(ab_3$group)
prop.table(table(ab_3$group))

chisq.test(table(ab_3$group), p = c(0.5, 0.5))

Показник р становить 0,6729, що значить, що критичного перекосу між групами немає.

11.3 Описова статистика

ab_3 %>% group_by(group) %>% summarise(n = n(), mean_del_day = mean(delivery_day),
                                       median_del_day = median(delivery_day),
                                       sd_del_day = sd(delivery_day))

11.4 Візцалізація

hist(ab_3$delivery_day)

boxplot(delivery_day~group, data = ab_3)

11.5 Статистичний тест

t.test(delivery_day~group, data = ab_3)

Статистично значущих відмінностей між групами не виявлено (р = 0,5823).

Висновок. Було проведено Велч т-тест для порівняння середнього часу доставки товарів
між групами А та В. Статистично значущих відмінностей не виявлено (р = 0,5823), 
що свідчить про відсутність негативного впливу на швидкість доставки. Отже,
guardrail-метрика не сигналізує нам про негативні побічні ефекти.