А/В тест 2(максимально наближений до реального)

Примітка:
Дані не є результатом реального експерименту, тому ефект змодельовано штучно.

0. Пакети

library(dplyr)
library(ggplot2)
library(effectsize)
library(pwr)

1. Завантаження даних та створення змінної

ab_2 <- read.csv("a_b_testing_2.csv")

str(ab_2)
head(ab_2)
nrow(ab_2)
summary(ab_2$unit_price)

ab_2$unit_price <- as.numeric(ab_2$unit_price)

ab_2 <- ab_2 %>% filter(!is.na(unit_price), unit_price > 0)

Одиницею аналізу є товарна позиція (order line), що може порушувати незалежність
спостережень.

2. Випадковий спліт на А/В

set.seed(12345)

ab_2$group <- sample(c("A","B"), size = nrow(ab_2), replace = T)

Рандомізація виконана на рівні записів (order line), що є спрощенням. 
У реальних умовах використовується user-level split.

3. Перевірка балансу груп

3.1. Простий баланс по кількості

table(ab_2$group)
prop.table(table(ab_2$group))

3.2. SRM-перевірка

chisq.test(table(ab_2$group), p = c(0.5, 0.5))

SRM не виявлено (p = 0.3321), розподіл груп коректний.

3.3. Базовий баланс по метриці до treatment

ab_2 %>% group_by(group) %>% summarise(n = n(), mean_price = mean(unit_price),
                                       median_price = median(unit_price),
                                       sd_price = sd(unit_price))

4. А/А тест

aa_test <- t.test(unit_price~group, data = ab_2)
aa_test

A/A тест не виявив відмінностей між групами (p = 0.7002), що підтверджує коректність
рандомізації.

5. Швидка перевірка розподілу

hist(ab_2$unit_price)

boxplot(unit_price~group, data = ab_2)

Опційно: percentiles

ab_2 %>% group_by(group) %>% summarise(p50 = quantile(unit_price, 0.5),
                                       p90 = quantile(unit_price, 0.9),
                                       p95 = quantile(unit_price, 0.95),
                                       p99 = quantile(unit_price, 0.99))

6. Задаємо treatment effect

effect_pct <- 0.05

ab_2$price_ab <- ifelse(ab_2$group == "B", ab_2$unit_price * (1+ effect_pct),
                        ab_2$unit_price)

Перевірка середніх

ab_2 %>% group_by(group) %>% summarise(mean_price_ab = mean(price_ab),
                                       median_price_ab = median(price_ab),
                                       sd_price_ab = sd(price_ab))

У даному прикладі treatment effect задано як фіксоване відсоткове збільшення, що
є спрощенням. У реальних умовах ефект зазвичай є неоднорідним і варіюється між 
користувачами.

7. А/В тест 

Основним тестом обрано Welch t-test як стандартний підхід для порівняння середніх.

7.1 Основний А/В тест (т-тест)

ab_test <- t.test(price_ab~group, data = ab_2)
ab_test

Показник р становить 0,1692, що значить, що немає суттєвої статистично значущої
різниці між обома групами

7.2 Robustness check: Mann–Whitney test

wilcox.test(price_ab~group, data = ab_2)

Не виявлено статистично значущу різницю у розташуванні розподілів (p = 0.1087).

7.3 Robustness check: Log-transform + t-test

ab_2$log_price_ab <- log(ab_2$price_ab + 1)

t.test(log_price_ab~group, data = ab_2)

Не виявлено статистично значущу різницю між середніми логарифмованих значень 
(p = 0.1508).

Додаткові перевірки (Mann–Whitney та t-test після логарифмування) показали 
не статистично значущий результат. Тобто, підтвердили результат основгного тесту.

8. Effect size

cohens_d(price_ab~group, data = ab_2)

Розмір ефекту є малим (Cohen’s d = -0,06). Довірчий інтервал [-0,14; 0,03] включає 
нуль, що свідчить про відсутність статистично значущого ефекту. Різниця між групами
є незначною як з статистичної, так і з практичної точки зору.

9. Оцінка power / MDE

9.1. Оцінимо pooled SD

sd_pool <- ab_2 %>% summarise(sd_pool = sd(unit_price)) %>% pull(sd_pool)

sd_pool

9.2. Розмір груп

n_A <- sum(ab_2$group == "A")
n_B <- sum(ab_2$group == "B")

n_A
n_B

9.3. Приблизний MDE для power = 80%

n_min <- min(n_A, n_B)

mde_d <- pwr.t.test(n = n_min, sig.level = 0.05, power = 0.8,
                    type = "two.sample", alternative = "two.sided")$d

mde_d

Переведемо це в гроші / абсолютну різницю:
  
mde_abs <- mde_d * sd_pool
mde_abs

І в процентах від середнього чеку

baseline_mean <- mean(ab_2$unit_price)

mde_pct <- mde_abs / baseline_mean

mde_pct

10. Готовий короткий бізнес-висновок

mean_A <- mean(ab_2$price_ab[ab_2$group == "A"])
mean_B <- mean(ab_2$price_ab[ab_2$group == "B"])
uplift_pct <- (mean_B - mean_A) / mean_A * 100

mean_A
mean_B
uplift_pct

Спостережуваний ефект становить +7.0%.

Було проведено Welch t-test для порівняння середньої ціни між групами A та B.
Середня ціна у групі A становить 25.98, у групі B — 27.80, що відповідає зростанню
на +7.0%.
Однак отримане значення p-value (p = 0.1692) не дозволяє вважати цю різницю 
статистично значущою на рівні α = 0.05.
Додаткові перевірки (Mann–Whitney та t-test після логарифмування) також не виявили
статистично значущих відмінностей, що підтверджує результат основного тесту.
Оцінка статистичної потужності показала, що мінімально детектований ефект (MDE)
становить близько 13.9%, що суттєво перевищує спостережуваний uplift (+7.0%).
Це означає, що за поточного обсягу даних тест не має достатньої чутливості, щоб 
надійно виявити ефект такого масштабу.

Далі проведемо аналіз guardrail-метрики для перевірки відсутності негативних 
побічних ефектів.

11. Аналіз guardrail-метрики

1. Завантаження даних і створення метрики

# використовуємо вже підготовлений датасет з group з основного тесту

ab_2$quantity <- as.numeric(ab_2$quantity)

ab_2 <- ab_2 %>% filter(!is.na(quantity), quantity > 0)

# використовуємо той самий group, що і в основному A/B тесті

2. Перевірка балансу груп

table(ab_2$group)
prop.table(table(ab_2$group))

chisq.test(table(ab_2$group), p = c(0.5, 0.5))

Показник р становить 0,3321, що значить, що критичного перекосу між групами немає.

3. Описова статистика

ab_2 %>% group_by(group) %>% summarise(n = n(), mean_quantity = mean(quantity),
                                       median_quantity = median(quantity),
                                       sd_quantity = sd(quantity))


5.Візуалізація

hist(ab_2$quantity)

boxplot(quantity ~ group, data = ab_2)

6. Статистичний тест 

t.test(quantity ~ group, data = ab_2)

Статистично значущих відмінностей між групами не виявлено (p = 0.9445), 
що свідчить про відсутність негативного впливу на швидкість доставки.

wilcox.test(quantity ~ group, data = ab_2)

Статистично значущих відмінностей між групами не виявлено (p = 0.6291), 
що свідчить про відсутність негативного впливу на швидкість доставки.

Було проведено Welch t-test для порівняння середнього обсягу куплених товарів між
групами A та B. Статистично значущих відмінностей не виявлено (p = 0.9445), що 
свідчить про відсутність впливу експерименту на обсяг проданих товарів. 
Додатковий Mann–Whitney тест підтверджує цей результат, що підвищує надійність 
висновку. Отже, guardrail-метрика не сигналізує про негативні побічні ефекти.