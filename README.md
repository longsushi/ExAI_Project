# Capstone Projekt – Bank Marketing (XAI mit R)

## ✅ TODOs (nach Phase)

### 1. Exploratory Data Analysis (EDA)
- [X] Daten laden & inspizieren (`data/raw`)
- [ ] Bereinigung und Transformation (`scripts/data_cleaning.R`)
- [ ] Visualisierungen zur Mustererkennung (`ggplot2`, `DataExplorer`, etc.)
- [ ] Ergebnisse dokumentieren in `01_EDA.Rmd`

### 2. Feature Engineering
- [ ] Neue Merkmale entwickeln (z. B. binär, kategorisch, aggregiert)
- [ ] Skalierung und Encoding
- [ ] Dokumentation in `02_Feature_Engineering.Rmd`

### 3. Machine Learning Workflow
- [ ] Modelle trainieren und tunen (z. B. Logistic Regression, Random Forest)
- [ ] Vergleich intrinsisch interpretierbarer Modelle vs. Black-Box-Modelle
- [ ] Evaluierung: Precision, Recall, F1, ROC AUC
- [ ] Dokumentation in `03_Modeling.Rmd`

### 4. Explainable AI (XAI)
#### Local Methods
- [ ] SHAP-Werte berechnen (`DALEX`, `iml`)
- [ ] ICE-Plots, Kontrafaktische Erklärungen (`counterfactuals`)
- [ ] Dokumentation in `04_XAI_Local.Rmd`

#### Global Methods
- [ ] Permutation Feature Importance
- [ ] Partial Dependence Plots
- [ ] Global Surrogate Model trainieren
- [ ] Dokumentation in `05_XAI_Global.Rmd`

### 5. Dokumentation und Abgabe
- [ ] Abschlussbericht erstellen (`reports/final_report.pdf`)
- [ ] Contribution Sheet ausfüllen (`contribution_sheet.md`)
- [ ] Reproduzierbare, kommentierte Notebooks/Skripte bereitstellen
- [ ] Abgabe bis **09. Juli 2025, 23:59 CEST**

## 🔗 Ressourcen
- [Bank Marketing Dataset](https://archive.ics.uci.edu/dataset/222/bank+marketing)
- [DALEX Docs](https://dalex.drwhy.ai/)
- [DALEX Source](https://github.com/ModelOriented/DrWhy)
- [Counterfactuals](https://github.com/dandls/counterfactuals)
- [Tidy Modeling mit R](https://www.tmwr.org/)
- [XAI für Business Analytics Slides](https://gamma.app/docs/XAI-for-Business-Analytics-qhfob17f0774ai5)
