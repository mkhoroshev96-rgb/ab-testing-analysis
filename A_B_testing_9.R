Контекст

Тестуємо фічу: “показ рекомендацій популярних товарів у кошику перед оплатою”

Метрики

Primary:AOV (average order value) — чи збільшився середній чек

Guardrail:conversion_rate — чи не впала конверсія через “перевантаження” користувача

Гіпотеза
H0: фіча не впливає на середній чек
H1: середній чек зростає

Логіка
користувач бачить “докупи ще щось” перед оплатою
частина користувачів додає ще товари → чек ↑
але частина може задушитись вибором → конверсія ↓

Потенційний ризик чек росте, але конверсія падає → загальний дохід може навіть просісти

А/В тест 9

library(dplyr)
library(boot)
library(pwr)

set.seed(1) # щоб результати були однакові кожен запуск

#1. Sample size

effect_d <- (800 * 0.05)/300 # розмір ефекту

n <- ceiling(pwr.t.test(d = effect_d, sig.level = 0.05, power = 0.8,
                        type = "two.sample")$n * 2) # скільки юзерів треба

# 2. Дані 

df <- data.frame(
  group = sample(c("A", "B"), n, replace = TRUE),   
  segment = sample(c("low","mid","high"), n, replace = TRUE,
                   prob = c(0.5,0.3,0.2)),
  day = sample(1:21, n, replace = TRUE)
)
                 
# 2.1 SRM check (чи рівні групи)

chisq.test(table(df$group), p = c(0.5,0.5))

показник р становить 0.6343, що свідчить про те, що групи є абсолютно рівними.

# 2.2 Balance check (чи рівномірні сегменти)

chisq.test(table(df$group, df$segment))

Показник р становить 0,7669, що свідчить про те, що сегменти розподілені між 
даними групами рівномірно.

# 3. Conversion (0/1 — купив чи ні)

df$prob <- 0

# група A

df$prob[df$group == "A" & df$segment == "high"] <- 0.18
df$prob[df$group == "A" & df$segment == "mid"]  <- 0.12
df$prob[df$group == "A" & df$segment == "low"]  <- 0.08

# група B
df$prob[df$group == "B" & df$segment == "high"] <- 0.22
df$prob[df$group == "B" & df$segment == "mid"]  <- 0.15
df$prob[df$group == "B" & df$segment == "low"]  <- 0.10

# генерація
df$converted <- rbinom(n, 1, df$prob)

# 4. AOV(тільки для тих, хто купив)

df$aov <- 0
idx <- df$converted == 1

meanlog_vec <- ifelse(df$segment == "high", log(950),
               ifelse(df$segment == "mid", log(650), log(300)))

df$aov[idx] <- rlnorm(sum(idx), meanlog = meanlog_vec[idx], sdlog = 0.3)

# сезонність (вихідні трохи сильніші)

df$aov[idx] <- df$aov[idx] * ifelse(df$day[idx] %in% c(6,7,13,14,20,21), 1.10,1) 

# treatment (ефект фічі)

treat_idx <- idx & df$group == "B"

df$aov[treat_idx] <- df$aov[treat_idx] * runif(sum(treat_idx), 1.04, 1.12) 

# outliers(деякий відсоток дуже великих чеків)

out_idx <- sample(which(idx), size = round(0.04 * sum(idx))) # 4% юзерів.
df$aov[out_idx] <- df$aov[out_idx] * runif(length(out_idx),3,8)

# 5. Основні тести

# Правило прийняття рішення перед аналізом:
# 1. p-value для SRM > 0.05
# 2. Основна метрика AOV: p-value < 0.05
# 3. 95% bootstrap довірчий інтервал для uplift не включає 0
# 4. Конверсія не є статистично значуще гіршою, ніж у контрольній групі

conv <- prop.test(table(df$group, df$converted)[,2],
                  rowSums(table(df$group, df$converted)))

Показник р становить 0,1152, що свідчить про те, що немає суттєвої статистично
значущої різниці між групами. Тобто, фіча ніяк не вплинула на конверсію.

#AOV (основний тест)

t.test(aov~group, data = df[df$converted == 1, ])

Показник р становить 0,3307, що значить, що немає суттєвої статистично значущої
різниці між групами.

# uplift (на скільки % змінилось)

df_buy <- df[df$converted == 1, ]

uplift <- with(df_buy, (mean(aov[group == "B"], na.rm = T) - mean(aov[group == "A"], na.rm = T))/
                 mean(aov[group == "A"], na.rm = T))

Величина спостережуваного ефекту становить 13,95%, тобто спостережуваний ефект 
в групі В вищий на зазначену величину, ніж в групі А.

# 6. Bootstrap CI (наскільки стабільний ефект)

boot_fn <- function(d,i) {
  x <- d[i, ] # bootstrap вибірка
  x <- x[x$converted == 1, ]
  (mean(x$aov[x$group == "B"],na.rm = T) - mean(x$aov[x$group == "A"], na.rm = T)) /
    mean(x$aov[x$group == "A"], na.rm = T) # рахуємо uplift
}

boot_res <- boot(df, boot_fn, R = 1000)

ci <- quantile(boot_res$t, c(0.025, 0.975), na.rm = T)

Отже, довірчий інтервал включає в себе 0, що значить, що ефект не є ствйким та 
не є статистично значущим.

# 7. Висновок

У ході А/В тесту було проведено зміни в групі В на конверсію та середній чек

SRM та баланс сегментів не виявили проблем (р > 0.05), отже групи є корректно 
сформованими

Конверсія: Статистично значущої різниці виявлено не було (р = 0,1152),  що 
значить, що зміни ніяк не вплинули на неї.

AOV (основна метрика): статистично значуща різниця не виявлено (p-value = 0,3307), 
що свідчить про те, що зміни ніяк не вплинули на дану метрику.

Uplift: Спостерігається великий uplift (13,95%), але він не є статистично значущим
і може бути випадковим.

Довірчий інтервал: включає 0 → ефект може бути випадковим.

Згідно з попередньо визначеним правилом прийняття рішення, основна метрика 
не досягла статистичної значущості.

ФІНАЛЬНЕ РІШЕННЯ: Не запускати (NO ROLLOUT), оскільки фіча не вплинула позитивно 
на основну метрику, тобто середні чеки не виросли.