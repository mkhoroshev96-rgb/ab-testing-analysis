А/В тест 6(складна версія)

library(dplyr)
library(pwr)
library(effectsize)
library(boot)

# Ми тестуємо збільшення знижки для частини користувачів (група B), що має підвищити 
# конверсію, але потенційно зменшити середній чек.

set.seed(123)

# 0. Stopping rule - тест триватиме до досягнення потрібного n_total або 30 днів
# що раніше

#1. Параметри для тесту(задаємо baseline + очікуваний ефект)

alpha <- 0.05

power_target <- 0.8
baseline_mean <- 1000
baseline_sd <- 350
uplift_pct <- 0.05  # очікуємо +5% в групі B

effects_abs <- baseline_mean * uplift_pct
effect_d <- effects_abs/baseline_sd

#2. Розрахунок потрібного розміру вибірки до тесту

sample_size <- pwr.t.test(d = effect_d, sig.level = alpha, power = power_target,
                          type = "two.sample", alternative = "two.sided")$n

n_total <- ceiling(sample_size * 2)

#3. Неідеальний спліт(імітація реального трафіку)

ab_data <- data.frame(user_id = 1:n_total,
                      group = sample(c("A","B"),n_total, replace = T,
                                     prob = c(0.51, 0.49)))

#3.1 Додаємо часовий вимір (імітація трафіку по дням)

ab_data$date <- sample(seq.Date(as.Date("2024-01-01"),
                                as.Date("2024-01-30"),by = "day"),
                       n_total, replace = T)

# 3.2 SRM check

table(ab_data$group)

srm_test <- chisq.test(table(ab_data$group), p = c(0.5, 0.5))

показник р становить 0,4297, що свідчить про те, що розподіл груп є корректний.

# 3.3 Додаємо сегменти користувачів

ab_data$segment <- sample(c("low", "mid", "high"), n_total, replace = T,
                          prob = c(0.5, 0.3, 0.2))

# 3.4 Sanity check: баланс сегментів між групами

segment_balance <- table(ab_data$group, ab_data$segment)
segment_balance

chisq.test(segment_balance)

Показник р сстановить 0,4039, що значить, що немає різниць між сегментами

# 3.5 Sanity check: баланс трафіку по днях

daily_balance <- table(ab_data$date, ab_data$group)
head(daily_balance)

#4. КОнверсія залежить від сегменту

ab_data$converted <- rbinom(n_total, 1, ifelse(ab_data$group == "A",
                                               ifelse(ab_data$segment == "high", 0.18,
                                               ifelse(ab_data$segment == "mid", 0.12,0.08)),
                                               
                                               ifelse(ab_data$segment == "high", 0.2,
                                               ifelse(ab_data$segment == "mid", 0.14, 0.1))))

#5. REVENUE(тільки для тих, хто купив)

ab_data$revenue <- 0

# Базовий lognormal revenue залежить від сегменту

ab_data$revenue[ab_data$converted == 1] <- 
  rlnorm(sum(ab_data$converted == 1),
         meanlog = ifelse(ab_data$segment[ab_data$converted == 1] == "high", log(1200),
         ifelse(ab_data$segment[ab_data$converted == 1] == "mid", log(900),log(600)))
         ,sdlog = 0.6)

# 5.1 Treatment effect — правильно:
# не перегенеровуємо revenue для B, а множимо базове значення

treatment_multiplier <- runif(sum(ab_data$converted == 1 & ab_data$group == "B"),
                               1.02, 1.08)

ab_data$revenue[ab_data$converted == 1 & ab_data$group == "B"] <-
  ab_data$revenue[ab_data$converted == 1 & ab_data$group == "B"] * treatment_multiplier

# 5.2 Аутлайєри (імітація великих чеків)

outlier_idx <- sample(
  which(ab_data$converted == 1),
  size = round(0.02 * sum(ab_data$converted)),
  replace = FALSE
)

ab_data$revenue[outlier_idx] <-
  ab_data$revenue[outlier_idx] * runif(length(outlier_idx), 3, 8)

# 5.3 Primary KPI — ARPU
# revenue per user, тобто revenue з нулями теж входить

ab_data$arpu <- ab_data$revenue

# 5.4 Pseudo pre-period metric для CUPED
# має бути корельована з target metric``

ab_data$pre_revenue <- pmax(
  0,
  ab_data$arpu * runif(n_total, 0.7, 1.3) + rnorm(n_total, 0, 80)
)

theta <- cov(ab_data$arpu, ab_data$pre_revenue) / var(ab_data$pre_revenue)

ab_data$arpu_cuped <- ab_data$arpu - theta * (ab_data$pre_revenue - mean(ab_data$pre_revenue))

# 6. Описові зрізи

ab_data %>%
  group_by(group, segment) %>%
  summarise(
    mean_rev = mean(revenue),
    conversion = mean(converted),
    .groups = "drop"
  )

ab_data %>%
  group_by(date, group) %>%
  summarise(
    mean_rev = mean(revenue),
    conversion = mean(converted),
    .groups = "drop"
  )


#7. Візуалізація

hist(ab_data$revenue)

Отже, як ми бачимо на гістограмі, наші дані мають дуже сильне скошення та чіткий
правий хвіст.

#8. Тест (тільки серед покупців)

buyers <- ab_data %>% filter(converted == 1)

buyers_test <- t.test(revenue ~ group, data = buyers)

Показник р становить 0,2978, що вказує нам на те, що немає суттєвої статистично
значущої різниці між групами.

#9. Розмір ефекту

cohens_d(revenue~group, data = buyers)

Розмір ефекту є малим, він становить 0,15. При цьому довірчі інтервали включають 
в себе 0, що свідчить про те, що ефект не є стійким та статистично значущим.
Тобто, розмір ефекту є не статистично значущим і практичний сенс його відсутній.

Перейдемо до перевірки Guardrail-метрики на предмет того чи не постраждала вона
через нововведення

# 10. Guardrail-метрика: conversion

table_conv <- table(ab_data$group, ab_data$converted)
table_conv

conv_test <- prop.test(table_conv[, 2], rowSums(table_conv))
conv_test

11. Тест на всіх, включаючи нулі

ab_test_all <- t.test(revenue~group, data = ab_data)
ab_test_all

В данному випадку показник р становить 0,8749, що свідчить про те, що конверсія
не постраждала після нововведення на всіх користувачах. Тобто в обох випадках 
все добре.

# 12. CUPED-аналіз по primary KPI

cuped_test <- t.test(arpu_cuped ~ group, data = ab_data)
cuped_test

#13. Підсумкові середні

all_effect <- cohens_d(arpu ~ group, data = ab_data)
all_effect

cuped_effect <- cohens_d(arpu_cuped ~ group, data = ab_data)
cuped_effect

# 14. Підсумкові середні

summary_table <- ab_data %>% group_by(group) %>% summarise(n = n(), conversion = mean(converted),
    mean_arpu = mean(arpu), sd_arpu = sd(arpu), mean_arpu_cuped = mean(arpu_cuped),
    sd_arpu_cuped = sd(arpu_cuped), .groups = "drop")

summary_table

# 15. Спостережуваний uplift по ARPU

mean_A <- mean(ab_data$arpu[ab_data$group == "A"])
mean_B <- mean(ab_data$arpu[ab_data$group == "B"])

uplift_observed <- (mean_B - mean_A) / mean_A
uplift_observed

# 16. Bootstrap CI для uplift по ARPU

set.seed(123)

boot_fn <- function(data, idx) {
  d <- data[idx, ]
  
  mean_A <- mean(d$arpu[d$group == "A"])
  mean_B <- mean(d$arpu[d$group == "B"])
  
  if (mean_A == 0) return(NA_real_)
  
  (mean_B - mean_A) / mean_A
}

boot_res <- boot(ab_data, statistic = boot_fn, R = 2000)

boot_res$t <- boot_res$t[is.finite(boot_res$t)]

boot_ci <- quantile(boot_res$t, probs = c(0.025, 0.975), na.rm = TRUE)
boot_ci

# 17. Сегментний аналіз ефекту

segment_uplift <- ab_data %>%
  group_by(segment) %>% summarise( mean_A = mean(arpu[group == "A"]), 
  mean_B = mean(arpu[group == "B"]), uplift = (mean_B - mean_A) / mean_A, .groups = "drop")

segment_uplift

# 18. Multiple testing correction

pvals <- c(
  buyers_test$p.value,
  conv_test$p.value,
  ab_test_all$p.value,
  cuped_test$p.value
)

p.adjust(pvals, method = "bonferroni")
p.adjust(pvals, method = "BH")

# 19. Decision block

cat("SRM p-value:", round(srm_test$p.value, 4), "\n")
cat("Conversion p-value:", round(conv_test$p.value, 4), "\n")
cat("ARPU p-value:", round(ab_test_all$p.value, 4), "\n")
cat("CUPED ARPU p-value:", round(cuped_test$p.value, 4), "\n")
cat("Observed uplift:", round(uplift_observed * 100, 2), "%\n")
cat("95% bootstrap CI for uplift:", 
    paste0(round(boot_ci[1] * 100, 2), "% ; ", round(boot_ci[2] * 100, 2), "%"), "\n")

if (ab_test_all$p.value < 0.05 && conv_test$p.value >= 0.05) {
  cat("Висновок: primary KPI статистично значущо змінився, guardrail не постраждала.\n")
} else if (ab_test_all$p.value < 0.05 && conv_test$p.value < 0.05) {
  cat("Висновок: primary KPI змінився, але є ризик по guardrail-метриці.\n")
} else {
  cat("Висновок: статистично значущих підстав для rollout немає.\n")
}