# A/B Testing Analysis (R)

This repository contains several A/B testing cases that simulate real-world analytical workflows.

The focus of these projects is not only on statistical testing, but also on correct experiment setup, validation and business interpretation of results.

## 📊 Overview

The repository includes multiple A/B testing scenarios with different metrics and assumptions.  
All datasets are simulated, but the analysis follows real-world best practices.

Each case demonstrates a full pipeline:

- data preprocessing
- randomization (A/B split)
- validation of experiment setup (SRM, A/A test)
- statistical testing
- effect size estimation
- power analysis (MDE)
- business interpretation
- guardrail metrics analysis

## 🧪 Cases included

### Case 1 — Average check analysis
File: :contentReference[oaicite:0]{index=0}  

- Metric: average check  
- Includes robustness checks (Mann–Whitney, log-transform)  
- Demonstrates impact of skewed distributions and outliers  
- Result: no statistically significant effect, potential positive signal  

---

### Case 2 — Unit price analysis
File: :contentReference[oaicite:1]{index=1}  

- Metric: unit price  
- Full validation pipeline (SRM, A/A test)  
- Power analysis shows insufficient sensitivity (MDE > observed effect)  
- Result: effect exists but cannot be reliably detected  

---

### Case 3 — Freight cost analysis
File: :contentReference[oaicite:2]{index=2}  

- Metric: average freight cost  
- Analysis at order level (correct unit of analysis)  
- Includes guardrail metric (delivery time)  
- Result: small uplift (~3%), but statistically insignificant due to low power  

---

## 📈 Key insights

- Lack of statistical significance does not necessarily mean absence of effect  
- MDE is critical for interpreting experiment results  
- Variability of data strongly impacts detectability of effects  
- Guardrail metrics are essential to validate safety of changes  

## 🧠 Methods used

- Welch t-test  
- Mann–Whitney test  
- Log-transformation  
- Effect size (Cohen’s d)  
- Power analysis (MDE)  
- SRM check (chi-square test)  

## 🛠 Tools

- R  
- dplyr  
- ggplot2  
- effectsize  
- pwr  

## 💡 Conclusion

These cases demonstrate a practical approach to A/B testing, focusing on decision-making rather than purely statistical results.
