A/B тест 4 (максимально наближений до реального)

Примітка: дані не є результатом реального експерименту, тому ефект змодельовано
штучно

0. Пакети

library(dplyr)
library(ggplot2)
library(effectsize)
library(pwr)

1. Завантаження даних та створення метрики

ab_4 <- read.csv("a_b_testing_4.csv")

str(ab_4)
head(ab_4)
nrow(ab_4)
summary(ab_4$avg_price)

ab_4$avg_price <- as.numeric(ab_4$avg_price)

ab_4 <- ab_4 %>% filter(!is.na(avg_price), avg_price > 0)

Одиницею аналізу є замовлення (order_id), що забезпечує незалежність спостереженнь.

2. Випадковий спліт на А/В

set.seed(112516585)

ab_4$group <- sample(c("A", "B"), size = nrow(ab_4), replace = TRUE, prob = c(0.5, 0.5))

Рандомізація виконана на рівні замовленнь (order_id), що відповідає одиниці аналізу.

3. Перевірка балансу груп

3.1 Простий баланс по кількості

table(ab_4$group)
prop.table(table(ab_4$group))

3.2 SRM - перевірка

chisq.test(table(ab_4$group), p =c(0.5,0.5))

SRM не виявлено (р = 0,4658), розподіл груп коректний

3.3 Базовий баланс по метриці до treatment

ab_4 %>% group_by(group) %>% summarise(n = n(), mean_avg_price  = mean(avg_price),
                                       median_avg_price = median(avg_price),
                                       sd_avg_price = sd(avg_price))

4. А/А тест

aa_test <- t.test(avg_price~group, data = ab_4)
aa_test

А/А тест не виявив відмінностей між групами (р = 0,9943), що підтверджує корректність
рандомізації.

5. Швидка перевірка розподілу

hist(ab_4$avg_price)

boxplot(avg_price~group, data = ab_4)

опційно: percentiles

ab_4 %>% group_by(group) %>% summarise(p50 = quantile(avg_price, 0.5),
                                       p90 = quantile(avg_price, 0.9),
                                       p95 = quantile(avg_price, 0.95),
                                       p99 = quantile(avg_price, 0.99))

Задаємо treatment ефект

ab_4$price_multiplier <- 1

# у групі підвищення ціни неоднорідне

ab_4$price_multiplier[ab_4$group == "B"] <- runif(sum(ab_4$group == "B"), 1.05, 1.25)

ab_4$avg_price_new <- ab_4$avg_price * ab_4$price_multiplier

# Ймовірність втрати ордера

ab_4$keep_order <- 1

ab_4$keep_order[ab_4$group == "B"] <- rbinom(sum(ab_4$group == "B"), 1, 0.9)

ab_4_sim <- ab_4 %>% filter(group == "A" | keep_order == 1)

ab_4_sim$behavior_factor <- 1

ab_4_sim$behavior_factor[ab_4_sim$group == "B"] <- runif(sum(ab_4_sim$group == "B"),
                                                         0.75, 1.00)

ab_4_sim$sum_chek_new <- ifelse(ab_4_sim$group == "B",
   ab_4_sim$sum_chek * (ab_4_sim$avg_price_new / ab_4_sim$avg_price) * ab_4_sim$behavior_factor,
   ab_4_sim$sum_chek)

Перевірка середніх

ab_4_sim %>% group_by(group) %>% summarise(mean_sum_chek_new = mean(sum_chek_new),
                                        median_sum_chek_new = median(sum_chek_new),
                                        sd_sum_chek_new = sd(sum_chek_new))

У даному прикладі treatment-ефект змодельовано як неоднорідний: у групі B ціна 
змінюється в різному діапазоні, частина ордерів втрачається, а поведінка 
користувачів додатково модифікується через behavior_factor. Це краще наближує 
симуляцію до реальної реакції користувачів на зміну умов.

7. Основний А/В тест

ab_test <- t.test(sum_chek_new~group, data = ab_4_sim)
ab_test

Показник р становить 0,5959, що значить, що немає суттєвої статистично значущої
різниці між групами

ab_test$conf.int

Довірчий інтервал включає в себе 0, що означає - немає статистично значущої різниці
між групами на обраному рівні значущості

8. Effect size

cohens_d(sum_chek_new~group, data = ab_4_sim)
 
Розмір ефекту є малим - -0.04. Довірчий інтервал включає в себе 0. Отже, різниця
є статистично не значущою; практичну значущість результату є низькою

9. Оцінимо power/mde 

9.1 Оцінимо pooled_sd

sd_pool <- ab_4_sim %>% summarise(sd_pool = sd(sum_chek_new)) %>% pull(sd_pool)

СТандартне відхилення метрики становить 1845.825, що свідчить про високий рівень
варіативності даних

9.2 Розмір груп

n_A <- sum(ab_4_sim$group == "A")
n_B <- sum(ab_4_sim$group == "B")

n_A
n_B

9.3 Приблизний МDE для power 80%

n_min <- min(n_A, n_B)

mde_d <- pwr.t.test(n = n_min, sig.level = 0.05, power = 0.8, type = "two.sample",
                    alternative = "two.sided")$d

Отже, мінімально виявлений ефект MDE становить 0,205, тобто тест здатен виявити 
різницю між групами лише у випадку, якщо вона перевищує приблизно 20.5% від 
стандартного відхилення

переведемо це в гроші/абсолютну різницю

mde_abs <- mde_d * sd_pool

В грошовому вираженні мінімально виявлена різниця MDE становить приблизно 378.65,
тобто тест здатен зафіксувати ефект лише якщо різниця між групами перевищує 378.65 
одиниць

І в процентах від середнього чеку

baseline_mean <- mean(ab_4_sim$sum_chek_new)

mde_pct <- mde_abs / baseline_mean

Отже, мінімально виявлений ефект становить приблизно 25,16% від середнього значення
метрики , тобто тест зафіксує різницю лише якщо вона перевищує приблизно 25%.

10. Готовий короткий бізнес висновок

mean_A <- mean(ab_4_sim$sum_chek_new[ab_4_sim$group=="A"])
mean_B <- mean(ab_4_sim$sum_chek_new[ab_4_sim$group=="B"])
uplift <- (mean_B - mean_A) / mean_A

mean_A
mean_B
uplift

Спостережуваний ефект становить -4,46, тобто середнє значення метрики в групі В
є на 4.46% нижчим порівняно з групою А.

Висновок

Було проведено A/B тест для оцінки впливу змін на середній чек замовлення. 
Статистично значущої різниці між групами не виявлено (p = 0.5959), а спостережуваний
ефект становить близько -4.46%, тобто середнє значення метрики в групі B є нижчим,
ніж у групі A.
Розрахунок MDE показав, що тест здатен надійно виявляти ефекти від приблизно 25% 
і вище. Оскільки фактичний ефект є значно меншим за цей поріг, поточний експеримент
не має достатньої чутливості для виявлення такої різниці.
Отже, за результатами тесту немає підстав стверджувати, що зміна позитивно вплинула
на середній чек замовлення.

Далі проведемо аналіз guardrail-метрики для перевірки відсутності негативних 
побічних ефектів

11. Аналіз guardrail-метрики

11.1 Метрика

ab_4$lost_order <- ifelse(ab_4$group == "B" & ab_4$keep_order == 0,1,0)

ab_4$lost_order[ab_4$group == "A"] <- 0

# використовуємо ті самі групи, що й в основному тесті

11.2 Опис

ab_4 %>% group_by(group) %>% summarise(n = n(), loss_rate = mean(lost_order))

loss_rate у групі B = 12%, у групі A = 0%

11.3 Cтатистичний тест

tab_loss <- table(ab_4$group, ab_4$lost_order)
chisq.test(tab_loss)

Виявлено статистично значущі відмінності між групами (p = 1.885e-12), що свідчить
про підвищення частки втрачених ордерів у групі B.

Висновок

Було проведено аналіз guardrail-метрики lost_order, яка відображає частку втрачених
ордерів. За результатами χ²-тесту виявлено статистично значущі відмінності між 
групами (p = 1.885e-12). Це означає, що в групі B частка втрачених ордерів є вищою,
тобто зміна супроводжується негативним побічним ефектом. Отже, guardrail-метрика
сигналізує про потенційне погіршення користувацької поведінки.

