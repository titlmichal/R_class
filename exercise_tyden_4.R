library(psych)
library(tidyverse)


# Importujte dataset personality_all.csv a nezapoměňte správně definovat 
# kódy missing values
data = read.csv2("data/personality_all.csv", na.strings = c("-99"))
glimpse(data)

# Převeďte sloupec gender na factor tak, aby chlapci byli první úrovní
# a děvčata druhou
data <- data %>% 
  mutate(
    gender = factor(gender,
                    levels = c("M", "F"),
                    labels = c("Boi", "Girl"))
  )

table(data$gender)

# Vytvořte textový vektor s názvy všech reverzních položek
# Pro škálu Need for cognition se jedná o položky 10 až 16
# Pro IPIP se jedná o položky:
# 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 29, 30, 32, 34, 
# 36, 38, 39, 44, 46, 49
# Můžete k tomu využít tuto funkci

item_range <- function(prefix, suffix) {
  str_c(prefix, 
        str_pad(suffix, pad = 0, width = 2))
}

nfc_reversed <- item_range("nfc_", 10:16)
typeof(nfc_reversed)

ipip_reversed <- c(item_range("ipip_", seq(2, 38, by = 2)), 
  item_range("ipip_", seq(29, 49, by = 10)),
  item_range("ipip_", c(44, 46)))
typeof(ipip_reversed)

nfc_reversed
ipip_reversed <- sort(ipip_reversed)
ipip_reversed

# Rekódujte všechny reverzní položky
# Můžete a měli byste využít vektor s reverzními položkami z předchozího kroku
data <- data %>% 
  mutate(
    across(all_of(c(nfc_reversed, ipip_reversed)),
                  ~5 - .x)
  )

data

# Vytvořte list items, jehož prvky budou textové vektory s názvy položek
# jednotlivých škál. Samotné vektory by měly být pojmenovány podle zkratek škál
# První dva prvky by vypadaly takto:
items <- list(
  nfc = item_range("nfc_", 1:16),
  ext = item_range("ipip_", seq(1, 50, by = 5)),
  agg = item_range("ipip_", seq(2, 50, by = 5)),
  con = item_range("ipip_", seq(3, 50, by = 5)),
  emo = item_range("ipip_", seq(4, 50, by = 5)),
  int = item_range("ipip_", seq(5, 50, by = 5))
)
# Čísla položek škál dotazníku IPIP:
# Extraversion: 1 6 11 16 21 26 31 36 41 46 
# Agreeableness: 2 7 12 17 22 27 32 37 42 47
# Conscientiousness: 3 8 13 18 23 28 33 38 43 48
# Emotional stability: 4 9 14 19 24 29 34 39 44 49
# Intellect: 5 10 15 20 25 30 35 40 45 50
items


# Vypočtěte celkové skóry pro všechny škály
# Můžete k tomu použít následující funkci v kombinaci s listem z předchozího
# kroku.
# Umístěte nově vzniklé sloupce někde na začátek datasetu, třeba za demografické
# proměnné a při použití funkce row_mean() nastavte, ať se vždy tolerují
# dvě nevyplněné položky
row_mean <- function(..., max.na = 0) {
  data <- pick(...)
  n_miss <- data %>% 
    is.na() %>% 
    rowSums()
  output <- data %>% 
    rowMeans(na.rm = TRUE)
  output[n_miss > max.na] <- NA_real_
  return(output)
}

data

data <- data %>% 
  mutate(
    nfc = row_mean(all_of(items$nfc), max.na = 2),
     ext = row_mean(all_of(items$ext), max.na = 2),
     agg = row_mean(all_of(items$agg), max.na = 2),
     con = row_mean(all_of(items$con), max.na = 2),
     emo = row_mean(all_of(items$emo), max.na = 2),
     int = row_mean(all_of(items$int), max.na = 2),
     , .after = class)

data

# Pomocí funkce group_by() rozdělte dataset do skupin podle ročníku a pohlaví
# (alternativně to jde i pomocí arugmentu .by až v rámci funkce summarise())
# a pak vypočtěte deskriptivní statistiky (průměry a směrodatné odchylky)
# pro všechny škály (NFC a dimenze Big Five)
data %>% 
  group_by(grade, gender) %>% 
  summarise(M_nfc = mean(nfc, na.rm = TRUE),
            SD_nfc = sd(nfc, na.rm = TRUE),
            M_ext = mean(ext, na.rm = TRUE),
            SD_ext = sd(ext, na.rm = TRUE),
            M_agg = mean(agg, na.rm = TRUE),
            SD_agg = sd(agg, na.rm = TRUE),
            M_con = mean(con, na.rm = TRUE),
            SD_con = sd(con, na.rm = TRUE),
            M_emo = mean(emo, na.rm = TRUE),
            SD_emo = sd(emo, na.rm = TRUE),
            M_int = mean(int, na.rm = TRUE),
            SD_int = sd(int, na.rm = TRUE)
            )


# Doplňující úkoly (NEPOVINNÉ)
# Zopakujte předchozí krok, ale doplňte i údaje o počtu validních a chybějících
# hodnot
data %>% 
  group_by(grade, gender) %>% 
  summarise(M_nfc = mean(nfc, na.rm = TRUE),
            SD_nfc = sd(nfc, na.rm = TRUE),
            valid_nfc = sum(!is.na(nfc)),
            missing_nfc = sum(is.na(nfc)),
            M_ext = mean(ext, na.rm = TRUE),
            SD_ext = sd(ext, na.rm = TRUE),
            valid_ext = sum(!is.na(ext)),
            missing_ext = sum(is.na(ext)),
            M_agg = mean(agg, na.rm = TRUE),
            SD_agg = sd(agg, na.rm = TRUE),
            valid_agg = sum(!is.na(agg)),
            missing_agg = sum(is.na(agg)),
            M_con = mean(con, na.rm = TRUE),
            SD_con = sd(con, na.rm = TRUE),
            valid_con = sum(!is.na(con)),
            missing_con = sum(is.na(con)),
            M_emo = mean(emo, na.rm = TRUE),
            SD_emo = sd(emo, na.rm = TRUE),
            valid_emo = sum(!is.na(emo)),
            missing_emo = sum(is.na(emo)),
            M_int = mean(int, na.rm = TRUE),
            SD_int = sd(int, na.rm = TRUE),
            valid_int = sum(!is.na(int)),
            missing_int = sum(is.na(int))
  )
#lze zjednodušit pomocí across v summarise fci
#(c proměnných, list agregací)


# Rozdělte dataset opět podle ročníku a pohlaví, ale místo průměrů a SD
# vypočtěte hodnoty kvartilů
data %>% 
  group_by(grade, gender) %>% 
  summarise(
    as_tibble_row(quantile(nfc, na.rm = TRUE)
    ))

data %>% 
  group_by(grade, gender) %>% 
  summarise(
    across(
      c(nfc, ext, agg, con, emo, int),
      ~ as_tibble_row(quantile(.x, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)),
      .names = "{.col}_Q{.fn}"
    ))
