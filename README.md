# A/B Testing Analysis (R)

This repository contains multiple A/B testing scenarios that simulate real-world analytical workflows.

The goal of these projects is to demonstrate not only statistical testing, but also proper experiment design, validation, and business-oriented interpretation of results.

---

## Overview

The repository includes a set of A/B testing cases with different metrics, assumptions, and experimental setups.

All datasets are simulated, but the analytical approach follows real-world practices used in product and marketing analytics.

Each case represents a full A/B testing pipeline:

- data simulation and preprocessing  
- user-level randomization (A/B split)  
- validation of experiment setup (SRM, A/A tests)  
- statistical testing  
- confidence interval estimation (including bootstrap)  
- effect size evaluation  
- power analysis (MDE)  
- business interpretation of results  
- guardrail metrics analysis  

---

## What is covered

The cases explore different types of metrics and scenarios, including:

- revenue-based metrics (ARPU, average check, unit price)  
- behavioral metrics (conversion, quantity, engagement)  
- cost-related metrics (freight, delivery impact)  
- guardrail metrics (to validate experiment safety)  

Special attention is given to:

- handling skewed distributions  
- impact of outliers  
- experiment sensitivity and statistical power  
- difference between statistical and practical significance  

---

## Key insights

- Lack of statistical significance does not necessarily mean absence of effect  
- Power (MDE) plays a critical role in experiment interpretation  
- Data variability directly impacts detectability of effects  
- Bootstrap helps better estimate uncertainty in skewed data  
- Guardrail metrics are essential for safe decision-making  

---

## 🧠 Methods used

- Welch t-test  
- Mann–Whitney test  
- Bootstrap confidence intervals  
- Log-transformation  
- Effect size estimation (Cohen’s d)  
- Power analysis (MDE)  
- SRM check (chi-square test)  

---

## 🛠 Tools

- R  
- dplyr  
- pwr  
- boot  

---

## Conclusion

These projects demonstrate a practical, product-oriented approach to A/B testing, focusing on decision-making and real-world analytical reasoning rather than purely statistical outputs.

## Conclusion

These cases demonstrate a practical approach to A/B testing, focusing on decision-making rather than purely statistical results.
