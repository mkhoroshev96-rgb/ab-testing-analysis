Контекст

Тестуємо фічу: персоналізовані знижки для high-value коричтувачів(10-20%)

Метрики 

Primary: ARPU(дохід на користувача) - чи зростає загальний дохід
Guardrail: Discount/Revenue ratio - чи не "переплачуємо" за рахунок знижок

Гіпотеза

Н0:фіча не впливає на ARPU
Н1:ARPU зростає.
 
Логіка

користувачі (high segment) будуть: отримувати персональну знижку → ↑ ймовірність 
покупки → ↑ ARPU але: знижка → ↓ маржа з кожного замовлення

Потенційний ризик ARPU може зрости, але: бізнес фактично втрачає гроші через 
агресивні знижки

А/В тест 8

library(dplyr)
library(boot)
library(pwr)

set.seed(321) # щоб результати були однакові кожен запуск

#1. Sample size

effect_d <- (1000 * 0.05)/350 # ефект становить 0.1428571

n <- ceiling(pwr.t.test(d = effect_d, sig.level = 0.05, power = 0.8,
                        type = "two.sample")$n * 2) # скільки юзерів треба

# 2. Дані 

df <- data.frame(group = sample(rep(c("A","B"), length.out = n )),
                 segment = sample(c("low", "mid", "high"), n, replace = T,
                 prob = c(0.5, 0.3, 0.2)), day = sample(1:14, n, replace = T))

# 2.1 SRM check (чи рівні групи)

table(df$group)
srm <- chisq.test(table(df$group), p = c(0.5, 0.5))

Показник р становить 0,9797, що свідчить про те, що немає істотного перекосу між 
обома групами

# 2.2 Balance check (чи рівномірні сегменти)

seg_balace <- chisq.test(table(df$group, df$segment))

Показник р становить 0,6537, що значить про те, що сегменти розподілені між групами
рівномірно

# 3. Purchase simulation (чи зробив користувач покупку)

df$converted <- rbinom(n, 1, ifelse(df$group == "A",
ifelse(df$segment == "high", 0.25, ifelse(df$segment == "mid", 0.18, 0.15)),# A
ifelse(df$segment == "high", 0.32, ifelse(df$segment == "mid", 0.22, 0.17)))) # B трохи краще

# 4. Revenue (тільки для тих, хто купив)

df$revenue <- 0
idx <- df$converted == 1  # тільки покупці

df$revenue_gross[idx] <- rlnorm(sum(idx), meanlog = ifelse(df$segment[idx] == "high", log(2200),
                                   ifelse(df$segment[idx] == "mid", log(1350), log(800))),
                                   sdlog = 0.45)

df$revenue_gross[idx & df$group == "B"] <- 
  df$revenue_gross[idx & df$group == "B"] *
  ifelse(df$segment[idx & df$group == "B"] == "high", runif(sum(idx & df$group == "B" & df$segment == "high"), 1.05, 1.15),
         ifelse(df$segment[idx & df$group == "B"] == "mid", runif(sum(idx & df$group == "B" & df$segment == "mid"), 1.03, 1.10),
                runif(sum(idx & df$group == "B" & df$segment == "low"), 1.00, 1.05)))

# 4.1 Discount (% знижки)

df$discount <- 0

# control: майже без знижок
df$discount[idx & df$group == "A"] <- runif(sum(idx & df$group == "A"), 0.00, 0.05) 

# treatment: персоналізовані знижки для B
df$discount[idx & df$group == "B" & df$segment == "high"] <- 
runif(sum(idx & df$group == "B" & df$segment == "high", 0.07, 0.12))

df$discount[idx & df$group == "B" & df$segment == "mid"] <- 
runif(sum(idx & df$group == "B" & df$segment == "mid", 0.05, 0.10))

df$discount[idx & df$group == "B" & df$segment == "low"] <- 
runif(sum(idx & df$group == "B" & df$segment == "low", 0.01, 0.04))

# 4.2 Revenue after discount (чек ПІСЛЯ знижки)

df$revenue <- df$revenue_gross * (1 - df$discount)

# сезонність

df$revenue[idx] <- df$revenue[idx] * ifelse(df$day[idx] %in% c(1,5,10,11), 1.05, 1)

# outliers

out <- sample(which(idx), size = round(0.03 * sum(idx)))

df$revenue[out] <- df$revenue[out] * runif(length(out), 1.5, 3)

# KPI

df$arpu <- df$revenue # середній дохід на юзера (включає нулі)

# 5. Основні тести

# Правило прийняття рішення перед аналізом:
# 1. p-value для SRM > 0.05
# 2. Основна метрика ARPU: p-value < 0.05
# 3. 95% bootstrap довірчий інтервал для uplift не включає 0
# 4. Конверсія не є статистично значуще гіршою, ніж у контрольній групі

# discount_ratio (guardrail — щоб не зламали метрику)

df$disc_ratio[idx] <- (df$revenue_gross[idx] - df$revenue[idx]) / df$revenue_gross[idx]
df$disc_ratio[is.na(df$disc_ratio)] <- 0
df$disc_ratio[is.infinite(df$disc_ratio)] <- 0

t.test(disc_ratio~group, data = df)

Показник р становить p-value < 2.2e-16, що свідчить про присутність статистично значущої
різниці між групами. Отже, фіча призвела до суттєвого зростання частки знижок на 
чеки.

# ARPU (основний тест)

test <- t.test(revenue~group, data = df)

Показник р становить 1.461e-06, що свідчить про те, що існує суттєва статистично
значуща різниця між обома групами.

# uplift (на скільки % змінилось)

uplift <- with(df, (mean(revenue[group == "B"], na.rm = T) - mean(revenue[group == "A"], na.rm = T))/
                 mean(revenue[group == "A"], na.rm = T))

Величина спостережуваного ефектк становить - 43,12%. Тобто, спостережуваний ефект
в групі В є на вказану величину нижчим, ніж в групі А. 

# 6. Bootstrap CI (наскільки стабільний ефект)

boot_fn <- function(d,i) {
  x <- d[i, ] # bootstrap вибірка
  (mean(x$revenue[x$group == "B"],na.rm = T) - mean(x$revenue[x$group == "A"], na.rm = T)) /
    mean(x$revenue[x$group == "A"], na.rm = T) # рахуємо uplift
}

boot_res <- boot(df, boot_fn, R = 1000)

ci <- quantile(boot_res$t, c(0.025, 0.975), na.rm = T) # 95% інтервал

Отже, довірчий інтервал не включає в себе 0, що свідчить про те, що ефект є стійким
та статистично значущим.

# 7. Висновок

У ході А/В тесту було проведено зміни в групі В на знижки та дохід

SRM та баланс сегментів не виявили проблем (р > 0.05), отже групи є корректно сформованими

Знижки: Було виявлено статистично значуще зростання (р = 2.2e-16),  що, ймовірно,
є ключовою причиною зниження ARPU

ARPU (основна метрика): статистично значуща різниця існує (p-value = 1.461e-06), 
що свідчить про те, що зміна статистично значуще ЗМЕНШУЄ дохід.

Uplift: спостерігається сильний та стабільний обернений Uplift(- 43,12%), який 
підтверджений статистично. Це означає, що негативний ефект від знижок перевищує 
позитивний ефект від зростання конверсії та чека.

Довірчий інтервал: не включає 0 → ефект не є випадковим.

Згідно з попередньо визначеним правилом прийняття рішення, основна метрика 
досягла статистичної значущості.

ФІНАЛЬНЕ РІШЕННЯ: Не запускати (NO ROLLOUT), оскільки фіча статистично значуще 
зменшує ARPU через надмірне зростання знижок