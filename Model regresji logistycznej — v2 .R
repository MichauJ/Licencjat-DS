# ====== Biblioteki ======

library(tidyr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(mgcv)
options(scipen = 999, digits = 2)
# ====== Wczytanie danych ======

file_path <- "I:/Mój dysk/Licencjat/Dane/"
df <- read.csv(paste0(file_path, "df.csv"), stringsAsFactors = TRUE)
df_extremes <- read.csv(paste0(file_path, "df_extreme.csv"), stringsAsFactors = TRUE) #1 = wartości o skrajnym nasileniu, 0 pozostałe

df_extremes <- df_extremes

# ====== Sprawdzenie braków danych ======

colSums(is.na(df))
colSums(is.na(df_extremes))


library(dplyr)

# Usunięcie kolumn o dużej ilości braków danych
df_extremes <- df_extremes %>%
  select(-netustm, -nwspol, -ext_high_edu_partner, -ext_religious, -ext_worried_climate)
table(df_extremes$rlgblg)
# Usunięcie brakujących wartości
df_extremes <- df_extremes %>% drop_na() %>% filter(rlgblg %in% c(0, 1))


# Zapisanie kolumny 'anweight' do nowej zmiennej

anweight_extreme <- df_extremes$anweight
# Usunięcie kolumny 'anweight' z obu zbiorów
df_extremes <- df_extremes[ , !names(df_extremes) %in% "anweight"]

# Ustawienie 'PL' jako kategorii bazowej dla zmiennej country
df_extremes$cntry <-  relevel(factor(df_extremes$cntry), ref = "PL")

# Lista zmiennych, które mają być skonwertowane
integer_columns <- c("happy_binary_median", "professional_activity", "chldhhe")

df_extremes <- df_extremes %>%
  mutate(cntry = recode(cntry,
                          "AT" = "Austria",
                          "BE" = "Belgia",
                          "CH" = "Szwajcaria",
                          "CY" = "Cypr",
                          "DE" = "Niemcy",
                          "ES" = "Hiszpania",
                          "FI" = "Finlandia",
                          "FR" = "Francja",
                          "GB" = "Wielka Brytania",
                          "GR" = "Grecja",
                          "HR" = "Chorwacja",
                          "HU" = "Węgry",
                          "IE" = "Irlandia",
                          "IS" = "Islandia",
                          "IT" = "Włochy",
                          "LT" = "Litwa",
                          "NL" = "Niderlandy",
                          "NO" = "Norwegia",
                          "PT" = "Portugalia",
                          "RS" = "Serbia",
                          "SE" = "Szwecja",
                          "SI" = "Słowenia",
                          "SK" = "Słowacja"
  ))

df_extremes <- df_extremes %>%
  rename(
    `Kraj badania` = cntry,
    `Czuje się zdrowy/a` = ext_good_health,
    `Ma bliską osobę` = ext_someone_close,
    `Przynależy do większości etnicznej/rasowej` = feethngr,
    `Aktywny/a społecznie` = ext_socially_active,
    `Przynależność religijna` = rlgblg,
    `Ojciec urodzony w kraju badania` = facntr,
    `Matka urodzona w kraju badania` = mocntr,
    `Płeć` = gndr,
    `Wyższe wykształcenie` = ext_high_edu,
    `Poglądy prawicowe` = ext_right_wing,
    `Poglądy lewicowe` = ext_left_wing,
    `Obywatel danego kraju` = ctzcntr,
    `Dzieci w gospodarstwie domowym` = chldhhe,
    `Tygodniowa liczba godzin pracy` = wkhtot,
    `Wyższe wykształcenie matki` = ext_high_edu_mother,
    `Wyższe wykształcenie ojca` = ext_high_edu_father,
    `Aktywny/a zawodowo` = professional_activity,
    `Poglądy centrystyczne` = ext_centrist,
    `Doświadczył/a rozwodu` = dvrcdeva,
    `Urodzony/a w danym kraju` = brncntr,
    `Liczba lat wykształcenia`= eduyrs,
    'Bardzo szczęśliwy/a' = ext_happy_or_not
  )


# ====== df_extremes ===============

# Rozmiar zbioru
dim(df_extremes)
# Nazwy kolumn
colnames(df_extremes)
# Podgląd pierwszych wierszy
head(df_extremes)
# Struktura danych (typy zmiennych)
str(df_extremes)

# ====== Wizualizacja zmiennych ===============

  # ====== extremes ===============

df_extremes %>%
  select(where(is.numeric)) %>%
  gather() %>%
  ggplot(aes(value)) +
  geom_histogram(bins = 30, fill = "red", alpha = 0.6) +
  facet_wrap(~ key, scales = "free") +
  theme_minimal()

df_extremes %>%
  select(eduyrs, wkhtot) %>%
  gather(key = "Variable", value = "Value") %>%
  ggplot(aes(x = Value, fill = Variable)) +  
  geom_density(alpha = 0.6) +  # Wykres gęstości z przezroczystością
  facet_wrap(~Variable, scales = "free") +  # Osobne wykresy dla każdej zmiennej
  theme_minimal() +
  labs(x = "Value", y = "Density", title = "Wykresy gęstości dla wybranych zmiennych") +
  theme(legend.position = "none")  # Ukrycie legendy, bo mamy osobne wykresy


# ====== Stworzenie modeli regresji logistycznej =======
logistic_model_extreme <- glm(`Bardzo szczęśliwy/a` ~ .,
                             data = df_extremes,
                             family = 'binomial',weights = anweight_extreme)
summary(logistic_model_extreme)

library(openxlsx)

# --- tabela współczynników ---
coef_df <- as.data.frame(summary(logistic_model_extreme)$coefficients)
coef_df <- cbind(Zmienna = rownames(coef_df), coef_df, row.names = NULL)

# --- zapis do Excela ---
wb <- createWorkbook()
addWorksheet(wb, "Model")
writeData(wb, "Model", coef_df)

saveWorkbook(wb, "wspolczynniki_modelu_pierwotnego.xlsx", overwrite = TRUE)


# ====== Zastosowanie regresji krokowej (stepwise regression) =======

ext_logistic_model_stepwise <- step(
  logistic_model_extreme,
  direction = "both",
  trace = 1,
  keep = function(model, aic) list(model = model, aic = aic)
)

# Przekształcenie współczynników na ilorazy szans (odds ratios)
coeffs_ext
odds_ratios_ext <- exp(coeffs_ext)
#odds_ratios_med <- exp(coeffs_med)

summary(ext_logistic_model_stepwise)
odds_ratios_ext


# ====== Ocena dystansu Cooka ========
cooks_d <- cooks.distance(ext_logistic_model_stepwise)

# Wydrukowanie obserwacji przekraczających próg
influential_points <- which(cooks_d > (4 / length(cooks_d)))
#wydrukowanie odrzuconych
df_extremes_removed <- df_extremes[influential_points, ]

#-----Dopasowanie modelu po usunięciu obserwacji wg Cooka dystansu -------


# Usunięcie obserwacji
df_extremes_clean <- df_extremes[-influential_points, ]
# Rozmiar zbioru
dim(df_extremes_clean)
# Nazwy kolumn
colnames(df_extremes_clean)

# Struktura danych (typy zmiennych)
str(df_extremes_clean)
df_extremes_clean
anweight_extreme_clean <- anweight_extreme[-influential_points]


# Obliczanie Cook's distance
cooks <- cooks.distance(ext_logistic_model_stepwise)
# Opcjonalnie: identyfikacja najbardziej wpływowych punktów
which(cooks > (4/length(cooks)))
  library(ggplot2)

cooks_df <- data.frame(
  Observation = 1:length(cooks),
  CooksD = cooks
)

ggplot(cooks_df, aes(x = Observation, y = CooksD)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 4/length(cooks), color = "red", linetype = "dashed") +
  labs(title = "Dystans Cook'a",
       x = "Indeks obserwacji",
       y = "Wartość Dystansu") +
  theme_minimal()

# ====== Wizualizacja zmiennych ===============

# ====== extremes, po usunięciu obserwacji wg. dystansu Cooka ===============

df_extremes_clean %>%
  select(where(is.numeric)) %>%
  gather() %>%
  ggplot(aes(value)) +
  geom_histogram(bins = 30, fill = "red", alpha = 0.6) +
  facet_wrap(~ key, scales = "free") +
  theme_minimal()

df_extremes_clean %>%
  select(eduyrs, wkhtot) %>%
  gather(key = "Variable", value = "Value") %>%
  ggplot(aes(x = Value, fill = Variable)) +  
  geom_density(alpha = 0.6) +  # Wykres gęstości z przezroczystością
  facet_wrap(~Variable, scales = "free") +  # Osobne wykresy dla każdej zmiennej
  theme_minimal() +
  labs(x = "Value", y = "Density", title = "Wykresy gęstości dla wybranych zmiennych") +
  theme(legend.position = "none")  # Ukrycie legendy, bo mamy osobne wykresy

# ====== Ponowne dopasowanie modelu po usunięciu wpływowych punktów ======

# Model pełny
logistic_model_extreme_clean <- glm(`Bardzo szczęśliwy/a` ~ .,
                                    data = df_extremes_clean,
                                    family = 'binomial',
                                    weights = anweight_extreme_clean)

# Regresja krokowa
ext_logistic_model_stepwise_clean <- step(logistic_model_extreme_clean, direction = "both", trace = 0)

# ====== Podsumowanie nowego modelu ======
summary(ext_logistic_model_stepwise_clean)

# ====== Nowe ilorazy szans ======
coeffs_ext_clean <- summary(ext_logistic_model_stepwise_clean)$coefficients[, 1]
odds_ratios_ext_clean <- exp(coeffs_ext_clean)

odds_ratios_ext_clean

# R^2 dla modelu opartego na ekstremach
1 - (ext_logistic_model_stepwise$deviance / ext_logistic_model_stepwise$null.deviance)
# R^2 dla modelu opartego na ekstremach po ocenie dystansu Cooka
1 - (ext_logistic_model_stepwise_clean$deviance / ext_logistic_model_stepwise_clean$null.deviance)
# log-likelihood
log_likelihood <- logLik(ext_logistic_model_stepwise_clean)

#======= Test Hosmera-Lemeshowa
library(ResourceSelection)

# Predykcja prawdopodobieństwa z modelu krokowego
pred_ext <- predict(ext_logistic_model_stepwise_clean, type = "response")

y <- ext_logistic_model_stepwise_clean$y

# Test HL (np. 10 grup)
hl_gof  <- hoslem.test(y, pred_ext, g = 10)
hl_gof
# ====== R^2 Nagelkerke
#install.packages("DescTools")
library(DescTools)

PseudoR2(ext_logistic_model_stepwise_clean, which = "Nagelkerke")
# Coxa-Snella
n <- nrow(df_extremes_clean)
L0 <- logLik(glm(df_extremes_clean$`Bardzo szczęśliwy/a` ~ 1, data = df_extremes_clean, family = binomial))
LM <- logLik(ext_logistic_model_stepwise_clean)

R2_CS <- 1 - exp((2/n)*(as.numeric(L0) - as.numeric(LM)))
R2_CS

# ====== Macierz pomyłek dla df_extremes ======



# Przekształcenie predykcji do 0/1 według progu 0.5
threshold <- 0.5
predicted_ext <- ifelse(pred_ext > threshold, 1, 0)

# Macierz pomyłek
conf_matrix_ext <- table(Prawdziwa = df_extremes_clean$`Bardzo szczęśliwy/a`, Predykcja = predicted_ext)
print(conf_matrix_ext)

# Dokładność
true_labels <- ext_logistic_model_stepwise_clean$y
pred_classes <- ifelse(predicted_ext >= 0.5, 1, 0)
accuracy <- mean(pred_classes == true_labels)

# Czułość (True Positive Rate)
TP <- conf_matrix_ext[2,2]
FN <- conf_matrix_ext[2,1]
sensitivity_ext <- TP / (TP + FN)

# Swoistość (True Negative Rate)
TN <- conf_matrix_ext[1,1]
FP <- conf_matrix_ext[1,2]
specificity_ext <- TN / (TN + FP)
print(c(specificity_ext,sensitivity_ext,accuracy))

# --- przedziały ufności


# Dla modelu ekstremów (współczynniki)
confint_coeffs <- round(confint(ext_logistic_model_stepwise_clean),2)

# Przedziały dla ilorazów szans
confint_chances <- round(
  exp(confint(ext_logistic_model_stepwise_clean)),
      2)

# 1. Konwersja do ramki danych (jeśli to jeszcze nie jest data frame)
confint_df <- as.data.frame(confint_chances)
# 2. Dodanie kolumny z nazwami zmiennych
confint_df$Variable <- rownames(confint_df)
# 3. Przeniesienie kolumny 'Variable' na początek
confint_df <- confint_df[, c("Variable", setdiff(names(confint_df), "Variable"))]
# 4. Zapis do pliku Excel
library(writexl)
write_xlsx(confint_df, "przedzialy_ilorazow_szans.xlsx")


#install.packages("stargazer")
library(stargazer)
stargazer(ext_logistic_model_stepwise_clean, type = "text", digits = 2,out = "result.html")


# ======
#install.packages(c("parameters","performance"))
library(parameters); library(performance)
model_parameters(ext_logistic_model_stepwise_clean, digits = 2, p_digits = 3)
model_performance(ext_logistic_model_stepwise_clean) %>%
  dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 2)))
#=====

summary(ext_logistic_model_stepwise_clean)
print(summary(ext_logistic_model_stepwise_clean), digits = 2)

# ====== Obliczanie VIF dla modelu ekstremalnego ======
library(writexl)
library(car)
vif_values <- vif(ext_logistic_model_stepwise_clean)
vif_table <- data.frame(
  Zmienna = rownames(vif_values),
  VIF = vif_values[, 1]
)
# Zapisz do pliku Excel
write_xlsx(vif_table, "vif_tabela.xlsx")

# --- klasyfikator konkurencyjny dla porównania

library(e1071)

# Konwersja zmiennej celu na faktor
df_extremes_clean$`Bardzo szczęśliwy/a` <- as.factor(df_extremes_clean$`Bardzo szczęśliwy/a`)

# Trening modelu Naive Bayes
nb_model <- naiveBayes(`Bardzo szczęśliwy/a` ~ ., data = df_extremes_clean)

# Predykcja
nb_pred <- predict(nb_model, df_extremes_clean)

# Macierz pomyłek
nb_conf_matrix <- table(Prawdziwa = df_extremes_clean$`Bardzo szczęśliwy/a`, Predykcja = nb_pred)

# Oblicz czułość i swoistość
TP_nb <- nb_conf_matrix[2,2]
FN_nb <- nb_conf_matrix[2,1]
TN_nb <- nb_conf_matrix[1,1]
FP_nb <- nb_conf_matrix[1,2]

sensitivity_nb <- TP_nb / (TP_nb + FN_nb)
specificity_nb <- TN_nb / (TN_nb + FP_nb)

cat("Naive Bayes — Czułość:", round(sensitivity_nb, 4), "\n")
cat("Naive Bayes — Swoistość:", round(specificity_nb, 4), "\n")
nb_conf_matrix


# --- klasyfikator na bazie średniej

# Dominująca klasa
dominant_class <- as.numeric(names(sort(table(df_extremes$ext_happy_or_not), decreasing = TRUE))[1])

# Utworzenie przewidywań — każdy przypadek dostaje dominującą klasę
mean_based_pred <- rep(dominant_class, nrow(df_extremes_clean))

# Ustawienie poziomów 0 i 1 — nawet jeśli model ich nie przewidział
mean_based_pred <- factor(mean_based_pred, levels = c(0, 1))
true_labels <- factor(df_extremes_clean$`Bardzo szczęśliwy/a`, levels = c(0, 1))

# Macierz pomyłek
mean_based_conf <- table(Prawdziwa = df_extremes_clean$`Bardzo szczęśliwy/a`, Predykcja = mean_based_pred)

# Czułość i swoistość
TP_base <- mean_based_conf[2,2]
FN_base <- mean_based_conf[2,1]
TN_base <- mean_based_conf[1,1]
FP_base <- mean_based_conf[1,2]

sensitivity_base <- TP_base / (TP_base + FN_base)
specificity_base <- TN_base / (TN_base + FP_base)

cat("Średni klasyfikator — Czułość:", round(sensitivity_base, 4), "\n")
cat("Średni klasyfikator — Swoistość:", round(specificity_base, 4), "\n")
mean_based_conf

# ================ Test normalności reszt ==============

#install.packages("nortest")  # tylko jeśli nie masz zainstalowanego
library(nortest)
residuals_ext <- residuals(ext_logistic_model_stepwise_clean, type = "deviance")
ad.test(residuals_ext)

residuals_ext <- residuals(ext_logistic_model_stepwise_clean, type = "deviance")

hist(residuals_ext,
     breaks = 50,
     col = "lightblue",
     main = "Histogram reszt deviance",
     xlab = "Reszty deviance")
qqnorm(residuals_ext, main = "Q-Q plot reszt deviance")
qqline(residuals_ext, col = "red", lwd = 2)

plot(fitted(ext_logistic_model_stepwise_clean),
     residuals_ext,
     xlab = "Wartości dopasowane",
     ylab = "Reszty deviance",
     main = "Reszty vs wartości dopasowane",
     pch = 20, col = "gray")
abline(h = 0, col = "red", lty = 2)

# 

summary(ext_logistic_model_stepwise_clean)

# ================ Wykres precyzji w funkcji pełności =================
# Pakiet do ROC
library(pROC)

# Predykcja prawdopodobieństw
predicted_prob <- predict(ext_logistic_model_stepwise_clean, type = "response")

# Obiekt ROC
roc_obj <- roc(df_extremes_clean$`Bardzo szczęśliwy/a`,
               predicted_prob,
               weights = anweight_extreme_clean)

# AUC
auc_value <- auc(roc_obj)
print(paste("AUC:", round(auc_value, 4)))

# Wykres ROC z wymuszoną skalą osi X i Y od 0 do 1
plot(roc_obj,
     col = "darkgreen",
     main = "",
     xlab = "1 - Swoistość",
     ylab = "Czułość",
     legacy.axes = TRUE,
     print.auc = TRUE,
     font.main = 1,  # pogrubiony tytuł
     font.lab = 3,   # kursywa dla osi
     font.axis = 1,  # normalne liczby
     family = "sans")  # rodzaj czcionki)

#==== Test Boxa- Tidwella === 

# Licza lat wykształcenia
df_extremes_clean$Licza_lat_wyksztalcenia_log <- df_extremes_clean$`Licza lat wykształcenia` *
  log(df_extremes_clean$`Licza lat wykształcenia` + 1e-5)

# Tygodniowa liczba godzin pracy
df_extremes_clean$Tygodniowa_liczba_godzin_pracy_log <- df_extremes_clean$`Tygodniowa liczba godzin pracy` *
  log(df_extremes_clean$`Tygodniowa liczba godzin pracy` + 1e-5)

bt_formula <- `Bardzo szczęśliwy/a` ~ 
  `Licza lat wykształcenia` +
  `Tygodniowa liczba godzin pracy` +
  `Czuje się zdrowy/a` +
  `Przynależność religijna` +
  `Obywatel danego kraju` +
  `Urodzony/a w danym kraju` +
  `Przynależy do większości etnicznej/rasowej` +
  `Ma bliską osobę` +
  `Aktywny/a społecznie` +
  `Wyższe wykształcenie` +
  `Wyższe wykształcenie matki` +
  `Dzieci w gospodarstwie domowym` +
  `Aktywny/a zawodowo` +
  `Doświadczył/a rozwodu` +
  `Poglądy prawicowe` +
  `Poglądy centrystyczne` +
  Licza_lat_wyksztalcenia_log +
  Tygodniowa_liczba_godzin_pracy_log

bt_model <- glm(bt_formula, data = df_extremes_clean, family = binomial)
summary(bt_model)

#=== Test separacji perfekcyjnej

install.packages("brglm2")  # jeśli nie masz pakietu
library(brglm2)

# Zbudowanie macierzy modelowej i wektora y
mf <- model.frame(ext_logistic_model_stepwise_clean)
X <- model.matrix(ext_logistic_model_stepwise_clean)
y <- model.response(mf)

# Wywołanie detect_separation
sep_res <- detect_separation(x = X,
                             y = y,
                             family = binomial("logit"))

print(sep_res)




