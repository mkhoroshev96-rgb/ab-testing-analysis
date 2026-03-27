А/В тест 1 (максимально наближений до реального)

Примітка:
Дані не є результатом реального експерименту, тому ефект змодельовано штучно.

0. Пакети

library(dplyr)
library(ggplot2)
library(effectsize)
library(pwr)

1. Завантаження даних і створення метрики

ab_1 <- read.csv("a_b_testing_1.csv")

str(ab_1)
head(ab_1)
nrow(ab_1)
summary(ab_1$chek)

ab_1$chek <- as.numeric(ab_1$chek)

ab_1 <- ab_1 %>% filter(!is.na(chek), chek > 0)

Одиницею аналізу є товарна позиція (order line), що може порушувати незалежність
спостережень.

2. Випадковий split на A/B

set.seed(42)

ab_1$group <- sample(c("A", "B"), size = nrow(ab_1), replace = TRUE)

Рандомізація виконана на рівні записів (order line), що є спрощенням. 
У реальних умовах використовується user-level split.

3. Перевірка балансу груп

3.1. Простий баланс по кількості

table(ab_1$group)
prop.table(table(ab_1$group))

3.2. SRM-перевірка

chisq.test(table(ab_1$group), p = c(0.5, 0.5))

SRM не виявлено (p = 0.477), розподіл груп коректний.

3.3. Базовий баланс по метриці до treatment

ab_1 %>% group_by(group) %>% summarise(n = n(), mean_chek = mean(chek), 
                             median_chek = median(chek), sd_chek = sd(chek))

4. А/А-тест

aa_test <- t.test(chek~group, data = ab_1)

A/A тест не виявив відмінностей між групами (p = 0.194), що підтверджує коректність
рандомізації.

5. Швидка перевірка розподілу

hist(ab_1$chek)

boxplot(chek~group, data = ab_1)

Опційно: percentiles

ab_1 %>% group_by(group) %>% summarise(p50 = quantile(chek, 0.5),
                                       p90 = quantile(chek, 0.9),
                                       p95 = quantile(chek, 0.95),
                                       p99 = quantile(chek, 0.99))

6. Задаємо treatment effect

effect_pct <- 0.12

ab_1$chek_ab <- ifelse(ab_1$group == "B", ab_1$chek * (1 + effect_pct), ab_1$chek)

Перевірка середніх

ab_1 %>% group_by(group) %>% summarise(mean_chek_ab = mean(chek_ab),
                                       median_chek_ab = median(chek_ab),
                                       sd_chek_ab = sd(chek_ab))

У даному прикладі treatment effect задано як фіксоване відсоткове збільшення, що
є спрощенням. У реальних умовах ефект зазвичай є неоднорідним і варіюється між 
користувачами.

7. А/В тест 

Основним тестом обрано Welch t-test як стандартний підхід для порівняння середніх.

7.1 Основний А/В тест (т-тест)

ab_test <- t.test(chek_ab ~ group, data = ab_1)
ab_test

Показник р становить 0,1943, що значить, що немає суттєвої статистично значущої
різниці між обома групами

ab_test$conf.int

Довірчий інтервал дозволяє оцінити можливий діапазон ефекту та є більш інформативним
для бізнесу, ніж лише p-value.

7.2 Robustness check: Mann–Whitney test

wilcox.test(chek_ab ~ group, data = ab_1)

Виявлено статистично значущу різницю у розташуванні розподілів (p = 0.022).

7.3 Robustness check: Log-transform + t-test

ab_1$log_chek_ab <- log(ab_1$chek_ab + 1)

t.test(log_chek_ab ~ group, data = ab_1)

Виявлено статистично значущу різницю між середніми логарифмованих значень (p = 0.010).

Додаткові перевірки (Mann–Whitney та t-test після логарифмування) показали 
статистично значущий результат.
Це свідчить про можливу асиметрію розподілу та вплив викидів, однак ці тести 
розглядаються як допоміжні (robustness checks), а не як основа для прийняття рішення.

8. Effect size

cohens_d(chek_ab~group, data = ab_1)

Розмір ефекту є малим (Cohen’s d = -0,06). Довірчий інтервал [-0,14; 0,03] включає 
нуль, що свідчить про відсутність статистично значущого ефекту. Різниця між групами
є незначною як з статистичної, так і з практичної точки зору.

9. Оцінка power / MDE

9.1. Оцінимо pooled SD

sd_pool <- ab_1 %>% summarise(sd_pool = sd(chek)) %>% pull(sd_pool)

sd_pool

9.2. Розмір груп

n_A <- sum(ab_1$group == "A")
n_B <- sum(ab_1$group == "B")

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

baseline_mean <- mean(ab_1$chek)

mde_pct <- mde_abs / baseline_mean

mde_pct

10. Готовий короткий бізнес-висновок

mean_A <- mean(ab_1$chek_ab[ab_1$group == "A"])
mean_B <- mean(ab_1$chek_ab[ab_1$group == "B"])
uplift_pct <- (mean_B - mean_A) / mean_A * 100

mean_A
mean_B
uplift_pct

Спостережуваний ефект становить +9.7%.

Було проведено Welch t-test для порівняння середнього чеку між групами A та B.
Середній чек у групі B вищий (+9.7%), однак статистично значущої різниці 
не виявлено (p = 0.194).
Додаткові перевірки показали можливу наявність ефекту, проте вони можуть 
відображати вплив асиметрії розподілу та розглядаються як допоміжні.
MDE (~20%) перевищує спостережуваний uplift (~9.7%), що вказує на недостатню 
статистичну потужність для виявлення такого ефекту.
Отже, результати не дають достатніх підстав для підтвердження ефективності змін, 
хоча спостерігається потенційний позитивний сигнал.

Далі проведемо аналіз guardrail-метрики для перевірки відсутності негативних 
побічних ефектів.

11. Аналіз guardrail-метрики

1. Завантаження даних і створення метрики

# використовуємо вже підготовлений датасет з group з основного тесту

ab_1$delivery_day <- as.numeric(ab_1$delivery_day)

ab_1 <- ab_1 %>% filter(!is.na(delivery_day), delivery_day > 0)

# використовуємо той самий group, що і в основному A/B тесті

2. Перевірка балансу груп

table(ab_1$group)
prop.table(table(ab_1$group))

chisq.test(table(ab_1$group), p = c(0.5, 0.5))

Показник р становить 0,456, що значить, що критичного перекосу між групами немає.

3. Описова статистика

ab_1 %>% group_by(group) %>% summarise(n = n(), mean_del_day = mean(delivery_day),
                                       median_del_day = median(delivery_day),
                                       sd_del_day = sd(delivery_day))


5.Візуалізація

hist(ab_1$delivery_day)

boxplot(delivery_day ~ group, data = ab_1)

6. Статистичний тест 

t.test(delivery_day ~ group, data = ab_1)

Статистично значущих відмінностей між групами не виявлено (p = 0.4178), 
що свідчить про відсутність негативного впливу на швидкість доставки.

wilcox.test(delivery_day ~ group, data = ab_1)

Статистично значущих відмінностей між групами не виявлено (p = 0.6622), 
що свідчить про відсутність негативного впливу на швидкість доставки.

Було проведено Welch t-test для порівняння середнього часу доставки між групами
A та B. Статистично значущих відмінностей не виявлено (p = 0.4178), що свідчить 
про відсутність впливу експерименту на швидкість доставки. 
Додатковий Mann–Whitney тест підтверджує цей результат, що підвищує надійність 
висновку. Отже, guardrail-метрика не сигналізує про негативні побічні ефекти.
