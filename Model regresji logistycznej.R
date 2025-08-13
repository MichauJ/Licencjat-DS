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
df_median <- read.csv(paste0(file_path, "df_median.csv"), stringsAsFactors = TRUE)  #0 = równe lub mniejsze od mediany, 1 = powyżej
df_extremes <- read.csv(paste0(file_path, "df_extreme.csv"), stringsAsFactors = TRUE) #1 = wartości o skrajnym nasileniu, 0 pozostałe



# ====== Sprawdzenie braków danych ======

colSums(is.na(df))
colSums(is.na(df_extremes))
colSums(is.na(df_median))

# Usunięcie brakujących wartości

df_median <- drop_na(df_median)
df_extremes <- drop_na(df_extremes)

# Zapisanie kolumny 'anweight' do nowej zmiennej
anweight_median <- df_median$anweight
anweight_extreme <- df_extremes$anweight
# Usunięcie kolumny 'anweight' z obu zbiorów
df_median <- df_median[ , !names(df_median) %in% "anweight"]
df_extremes <- df_extremes[ , !names(df_extremes) %in% "anweight"]

# Ustawienie 'PL' jako kategorii bazowej dla zmiennej country
df_median$cntry <- relevel(factor(df_median$cntry), ref = "PL")
df_extremes$cntry <-  relevel(factor(df_extremes$cntry), ref = "PL")

# Lista zmiennych, które mają być skonwertowane
integer_columns <- c("happy_binary_median", "professional_activity", "chldhhe")

# Konwersja tych zmiennych na integer
df_median[integer_columns] <- lapply(df_median[integer_columns], as.integer)

# ====== df_extremes ===============

# Rozmiar zbioru
dim(df_extremes)
# Nazwy kolumn
colnames(df_extremes)
# Podgląd pierwszych wierszy
head(df_extremes)
# Struktura danych (typy zmiennych)
str(df_extremes)

# ====== df_median ===============

# Rozmiar zbioru
dim(df_median)
# Nazwy kolumn
colnames(df_median)
# Podgląd pierwszych wierszy
head(df_median)
# Struktura danych (typy zmiennych)
str(df_median)

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
  select(nwspol, netustm, eduyrs, wkhtot) %>%
  gather(key = "Variable", value = "Value") %>%
  ggplot(aes(x = Value, fill = Variable)) +  
  geom_density(alpha = 0.6) +  # Wykres gęstości z przezroczystością
  facet_wrap(~Variable, scales = "free") +  # Osobne wykresy dla każdej zmiennej
  theme_minimal() +
  labs(x = "Value", y = "Density", title = "Wykresy gęstości dla wybranych zmiennych") +
  theme(legend.position = "none")  # Ukrycie legendy, bo mamy osobne wykresy

  # ====== median_based ===============

# Histogram dla zmiennych numerycznych w df_median
df_median %>%
  select(where(is.numeric)) %>%
  gather() %>%
  ggplot(aes(value)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.6) +
  facet_wrap(~ key, scales = "free") +
  theme_minimal()

# Wykresy gęstości dla wybranych zmiennych w df_median
df_median %>%
  select(nwspol, netustm, eduyrs, wkhtot) %>%
  gather(key = "Variable", value = "Value") %>%
  ggplot(aes(x = Value, fill = Variable)) +  
  geom_density(alpha = 0.6) + 
  facet_wrap(~Variable, scales = "free") +
  theme_minimal() +
  labs(x = "Value", y = "Density", title = "Wykresy gęstości") +
  theme(legend.position = "none")

# ====== Stworzenie modeli regresji logistycznej =======

#logistic_model_median <- glm(happy_binary_median ~ .,
#                      data = df_median,
#                      family = 'binomial',weights = anweight_median)
logistic_model_extreme <- glm(ext_happy_or_not ~ .,
                             data = df_extremes,
                             family = 'binomial',weights = anweight_extreme)

# ====== Zastosowanie regresji krokowej (stepwise regression) =======

#med_logistic_model_stepwise <- step(logistic_model_median,direction = "both",trace = 0)

ext_logistic_model_stepwise <- step(logistic_model_extreme,direction = "both",trace = 0)

coeffs_ext <- summary(ext_logistic_model_stepwise)$coefficients[, 1]
#coeffs_med <- summary(med_logistic_model_stepwise)$coefficients[, 1]
# Przekształcenie współczynników na ilorazy szans (odds ratios)
coeffs_ext
odds_ratios_ext <- exp(coeffs_ext)
#odds_ratios_med <- exp(coeffs_med)

summary(ext_logistic_model_stepwise)
odds_ratios_ext

#summary(med_logistic_model_stepwise)
#odds_ratios_med


# ====== Ocena dystansu Cooka ========
cooks_d <- cooks.distance(ext_logistic_model_stepwise)

# Sprawdzenie największych wartości
plot(cooks_d, type = "h", main = "Cook’s Distance", ylab = "Cook's D")
abline(h = 4/length(cooks_d), col = "red", lty = 2)  # typowy próg ostrzegawczy

# Wydrukowanie obserwacji przekraczających próg
influential_points <- which(cooks_d > (4 / length(cooks_d)))
influential_points
#wydrukowanie odrzuconych
df_extremes_removed <- df_extremes[influential_points, ]
print(df_extremes_removed)
#-----Dopasowanie modelu po usunięciu obserwacji wg Cooka dystansu -------


# Usunięcie obserwacji
df_extremes_clean <- df_extremes[-influential_points, ]
# Rozmiar zbioru
dim(df_extremes_clean)
# Nazwy kolumn
colnames(df_extremes_clean)
# Podgląd pierwszych wierszy
head(df_extremes_clean)
# Struktura danych (typy zmiennych)
str(df_extremes_clean)
df_extremes_clean
anweight_extreme_clean <- anweight_extreme[-influential_points]

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
  select(nwspol, netustm, eduyrs, wkhtot) %>%
  gather(key = "Variable", value = "Value") %>%
  ggplot(aes(x = Value, fill = Variable)) +  
  geom_density(alpha = 0.6) +  # Wykres gęstości z przezroczystością
  facet_wrap(~Variable, scales = "free") +  # Osobne wykresy dla każdej zmiennej
  theme_minimal() +
  labs(x = "Value", y = "Density", title = "Wykresy gęstości dla wybranych zmiennych") +
  theme(legend.position = "none")  # Ukrycie legendy, bo mamy osobne wykresy

# ====== Ponowne dopasowanie modelu po usunięciu wpływowych punktów ======

# Model pełny
logistic_model_extreme_clean <- glm(ext_happy_or_not ~ .,
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
  
#----

# R^2 dla modelu opartego na medianie
#1 - (med_logistic_model_stepwise$deviance / med_logistic_model_stepwise$null.deviance)

# R^2 dla modelu opartego na ekstremach
1 - (ext_logistic_model_stepwise$deviance / ext_logistic_model_stepwise$null.deviance)
# R^2 dla modelu opartego na ekstremach po ocenie dystansu Cooka
1 - (ext_logistic_model_stepwise_clean$deviance / ext_logistic_model_stepwise_clean$null.deviance)

# Dalej kontynuuje tylko z modelem opartym na wartościch skrajnych

#======= Test Hosmera-Lemeshowa
library(ResourceSelection)

# Predykcja prawdopodobieństwa z modelu krokowego
pred_ext <- predict(ext_logistic_model_stepwise_clean, type = "response")

y <- ext_logistic_model_stepwise_clean$y

# Test HL (np. 10 grup)
hl_gof  <- hoslem.test(y, pred_ext, g = 10)
hl_gof
# ====== R^2 Nagelkerke
install.packages("DescTools")
library(DescTools)

PseudoR2(ext_logistic_model_stepwise_clean, which = "Nagelkerke")

# ====== Macierz pomyłek dla df_extremes ======




# Przekształcenie predykcji do 0/1 według progu 0.5
threshold <- 0.5
predicted_ext <- ifelse(pred_ext > threshold, 1, 0)

# Macierz pomyłek
conf_matrix_ext <- table(Prawdziwa = df_extremes_clean$ext_happy_or_not, Predykcja = predicted_ext)
print(conf_matrix_ext)

# Dokładność
true_labels <- ext_logistic_model_stepwise_clean$y
pred_classes <- ifelse(predicted_ext >= 0.5, 1, 0)
accuracy <- mean(pred_classes == true_labels)
accuracy

# Czułość (True Positive Rate)
TP <- conf_matrix_ext[2,2]
FN <- conf_matrix_ext[2,1]
sensitivity_ext <- TP / (TP + FN)

# Swoistość (True Negative Rate)
TN <- conf_matrix_ext[1,1]
FP <- conf_matrix_ext[1,2]
specificity_ext <- TN / (TN + FP)

# Wyświetlenie wyników
cat("Czułość:", round(sensitivity_ext, 4), "\n")
cat("Swoistość:", round(specificity_ext, 4), "\n")

# --- przedziały ufności


# Dla modelu ekstremów (współczynniki)
round(confint(ext_logistic_model_stepwise_clean),2)

# Przedziały dla ilorazów szans
round(
  exp(confint(ext_logistic_model_stepwise_clean)),
      2)

#summary(med_logistic_model_stepwise)
summary(ext_logistic_model_stepwise_clean)

# ====== Obliczanie VIF dla modelu ekstremalnego ======
library(car)
vif(ext_logistic_model_stepwise)

# --- klasyfikator konkurencyjny dla porównania

library(e1071)

# Konwersja zmiennej celu na faktor
df_extremes_clean$ext_happy_or_not <- as.factor(df_extremes_clean$ext_happy_or_not)

# Trening modelu Naive Bayes
nb_model <- naiveBayes(ext_happy_or_not ~ ., data = df_extremes_clean)

# Predykcja
nb_pred <- predict(nb_model, df_extremes_clean)

# Macierz pomyłek
nb_conf_matrix <- table(Prawdziwa = df_extremes_clean$ext_happy_or_not, Predykcja = nb_pred)

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
true_labels <- factor(df_extremes_clean$ext_happy_or_not, levels = c(0, 1))

# Macierz pomyłek
mean_based_conf <- table(Prawdziwa = df_extremes_clean$ext_happy_or_not, Predykcja = mean_based_pred)

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
predicted_prob <- predict(logistic_model_extreme_clean, type = "response")

# Obiekt ROC
roc_obj <- roc(df_extremes_clean$ext_happy_or_not,
               predicted_prob,
               weights = anweight_extreme_clean)

# AUC
auc_value <- auc(roc_obj)
print(paste("AUC:", round(auc_value, 4)))

# Wykres ROC z wymuszoną skalą osi X i Y od 0 do 1
plot(roc_obj,
     col = "darkgreen",
     main = "Wykres krzywej ROC",
     xlab = "1 - Swoistość",
     ylab = "Czułość",
     legacy.axes = TRUE,
     print.auc = TRUE,
     font.main = 1,  # pogrubiony tytuł
     font.lab = 3,   # kursywa dla osi
     font.axis = 1,  # normalne liczby
     family = "sans")  # rodzaj czcionki)





