# Capstone Projekt – Bank Marketing (XAI mit R)

## 📄 Dataset Introduction

This project uses the Bank Marketing Dataset from a Portuguese banking institution. The data contains detailed information on clients contacted during a direct marketing campaign (phone calls). The goal of the campaign was to encourage clients to subscribe to a bank term deposit.

## ✅ TODOs (nach Phase)

### 1. Exploratory Data Analysis (EDA)
- [X] Daten laden & inspizieren
- [X] Bereinigung und Transformation
- [X] Visualisierungen zur Mustererkennung

### 2. Feature Engineering
- [X] Neue Merkmale entwickeln (z. B. binär, kategorisch, aggregiert)
- [X] Skalierung und Encoding

### 3. Machine Learning Workflow
- [X] Modelle trainieren und tunen (z. B. Logistic Regression, Random Forest)
- [X] Vergleich intrinsisch interpretierbarer Modelle vs. Black-Box-Modelle
- [X] Evaluierung: Precision, Recall, F1, ROC AUC

### 4. Explainable AI (XAI)
#### Local Methods
- [X] SHAP-Werte berechnen (`DALEX`, `iml`)
- [X] ICE-Plots, Kontrafaktische Erklärungen (`counterfactuals`)

#### Global Methods
- [X] Permutation Feature Importance
- [X] Partial Dependence Plots
- [X] Global Surrogate Model trainieren

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
