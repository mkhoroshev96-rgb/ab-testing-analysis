Контекст

Тестуємо фічу:“безкоштовна доставка при замовленні від $100”

Метрики
Primary ARPU (дохід на користувача) чи зростає загальний дохід
Guardrail Freight / Revenue ratio чи не з’їдає доставка весь прибуток
Гіпотеза

H0: фіча не впливає на ARPU
H1: ARPU зростає

Логіка
користувачі будуть: добирати товари до $100 → ↑ чек → ↑ ARPU але:
доставка стає безкоштовною → ↑ витрати

Потенційний ризик ARPU може зрости, але бізнес стане менш прибутковим

А/В тест 7 (спрощена версія)

library(dplyr)
library(pwr)
library(boot)

set.seed(123) # щоб результати були однакові кожен запуск

# 1. Sample size

effect_d <- (1000 * 0.05)/350

n <- ceiling(pwr.t.test(d = effect_d, sig.level = 0.05, power = 0.8,
                        type = "two.sample")$n * 2) # скільки юзерів треба

# 2. Дані 

df <- data.frame(group = sample(rep(c("A","B"), length.out = n)), # ~50/50 розподіл
                 segment = sample(c("low", "mid","high"), n, replace = T,
                                  prob = c(0.5, 0.3, 0.2)), # тип юзера
                 day = sample(1:14, n, replace = T)) # день (імітація часу)

# 2.1 SRM check (чи рівні групи)

table(df$group)

srm <- chisq.test(table(df$group), p =c(0.5, 0.5))

Показник р становить 0,9797, що свідчить про те, що немає істотного перекосу між
обома групами

# 2.2 Balance check (чи рівномірні сегменти)

seg_balance <- chisq.test(table(df$group, df$segment))

Показник р становить 0,103, що свідчить про те, що сегменти розподілені між
групами рівномірно.

# 3. Purchase simulation (чи зробив користувач покупку)

df$converted <- rbinom(n, 1, ifelse(df$group == "A",
ifelse(df$segment == "high", 0.18, ifelse(df$segment == "mid", 0.25,0.15)),# A
ifelse(df$segment == "high", 0.20, ifelse(df$segment == "mid", 0.28,0.18)))) # B трохи краще

# 4. Revenue (тільки для тих, хто купив)

df$revenue <- 0
idx <- df$converted == 1 # тільки покупці

df$revenue[idx] <- rlnorm(sum(idx), meanlog = ifelse(df$segment[idx] == "high",log(1850),
                  ifelse(df$segment[idx] == "mid", log(1230), log(340))),
                  sdlog = 0.85) # lognormal → реалістичний розподіл доходу (heavy tail)

# 4.1 Freight 

df$freight <- 0

df$freight[idx] <- rlnorm(sum(idx),
                          meanlog = ifelse(df$segment[idx] == "high", log(535),
                          ifelse(df$segment[idx] == "mid", log(483), log(313))),
                          sdlog = 0.35)

# сезонність (вихідні трохи сильніші)

df$revenue[idx] <- df$revenue[idx] * ifelse(df$day[idx] %in% c(1,5,10), 1.08, 1)
df$freight[idx] <- df$freight[idx] * ifelse(df$day[idx] %in% c(2,8,12,14), 1.01, 1)

# treatment (ефект фічі)

df$revenue[idx & df$group == "B"] <- df$revenue[idx & df$group == "B"] * 
  runif(sum(idx & df$group == "B"), 1.05, 1.12) # трохи збільшуємо чек

df$freight[idx & df$group == "B"] <- df$freight[idx & df$group == "B"] *
  runif(sum(idx & df$group == "B"), 1.02, 1.08) 

# outliers (4% дуже великих чеків)

out <- sample(which(idx), size = round(0.04 * sum(idx))) # 2% юзерів
df$revenue[out] <- df$revenue[out] * runif(length(out),5,12) # дуже великі покупки
df$freight[out] <- df$freight[out] * runif(length(out),5,12)

# KPI

df$arpu <- df$revenue # середній дохід на юзера (включає нулі)

# 5. Основні тести

# Правило прийняття рішення перед аналізом:
# 1. p-value для SRM > 0.05
# 2. Основна метрика ARPU: p-value < 0.05
# 3. 95% bootstrap довірчий інтервал для uplift не включає 0
# 4. Конверсія не є статистично значуще гіршою, ніж у контрольній групі

# freight (guardrail — щоб не зламали метрику)

df$fr_ratio <- df$freight / df$revenue
df$fr_ratio[is.na(df$fr_ratio)] <- 0
df$fr_ratio[is.infinite(df$fr_ratio)] <- 0

t.test(fr_ratio~group, data = df)

Показник p становить 0.1973, що свідчить про відсутність статистично значущої
різниці між групами. Отже, фіча не призвела до зростання частки витрат на доставку.

# ARPU (основний тест)

test <- t.test(revenue~group, data = df)  # p-value по ARPU

Показник р становить 0,01926, що свідчить про те, що існує суттєва статистично
значуща різниці по цій метриці між групами. 

# uplift (на скільки % змінилось)

uplift <- with(df, (mean(revenue[group == "B"]) - mean(revenue[group == "A"]))/ 
                 mean(revenue[group == "A"]))

Величина спостережуваного ефекту становить 81,21%. Тобто, спостережуваний ефект
в групі В вищий на вище зазначену величину порівняно з групою А.

# 6. Bootstrap CI (наскільки стабільний ефект)

boot_fn <- function(d,i) {
  x <- d[i, ] # bootstrap вибірка
  (mean(x$revenue[x$group == "B"]) - mean(x$revenue[x$group == "A"])) /
    mean(x$revenue[x$group == "A"]) # рахуємо uplift
}

boot_res <- boot(df, boot_fn, R = 1000)

ci <- quantile(boot_res$t, c(0.025, 0.975), na.rm = TRUE) # 95% інтервал

Отже, як ми бачимо, довірчі інтервали не включають в себе 0, що свідчить про те,
що ефект є стійким та статистично значущим. 

# 7. Висновок

У ході A/B тесту було перевірено вплив змін у групі B на відношення вартості
доставки (Freight / Revenue ratio)  та дохід (ARPU).

SRM та баланс сегментів не виявили проблем (p > 0.05), отже групи є коректно 
сформованими.

Freight / Revenue ratio: статистично значущих змін не виявлено (p = 0.1973),
тобто частка витрат на доставку не змінилася.

revenue (основна метрика): Статистично значуща різниця виявлена (p = 0.01926), що
означає те, що існує надійний ефект на дохід.

Uplift: Спостерігається значний uplift (+81%), який є стабільним та
підтверджений статистично.

Довірчий інтервал: не включає 0 → ефект є стабільним.

Згідно з попередньо визначеним правилом прийняття рішення, основна метрика досягла
статистично значущого покращення.

ФІНАЛЬНЕ РІШЕННЯ: запускати, оскільки є достатні докази реального впливу на дохід.