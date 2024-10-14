library(psych)
library(tidyverse)


# Importujte dataset personality_all.csv a nezapoměňte správně definovat 
# kódy missing values


# Převeďte sloupec gender na factor tak, aby chlapci byli první úrovní
# a děvčata druhou


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

item_range("nfc_", c(1, 2, 3))


# Rekódujte všechny reverzní položky
# Můžete a měli byste využít vektor s reverzními položkami z předchozího kroku


# Vytvořte list items, jehož prvky budou textové vektory s názvy položek
# jednotlivých škál. Samotné vektory by měly být pojmenovány podle zkratek škál
# První dva prvky by vypadaly takto:
items <- list(
  nfc = item_range("nfc_", 1:16),
  ext = item_range("ipip_", seq(1, 50, by = 5)),
)
# Čísla položek škál dotazníku IPIP:
# Extraversion: 1 6 11 16 21 26 31 36 41 46 
# Agreeableness: 2 7 12 17 22 27 32 37 42 47
# Conscientiousness: 3 8 13 18 23 28 33 38 43 48
# Emotional stability: 4 9 14 19 24 29 34 39 44 49
# Intellect: 5 10 15 20 25 30 35 40 45 50



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

df %>% 
  mutate(nfc = row_mean(all_of(items$nfc))) # Takto by to vypadalo pro škálu NFC

# Pomocí funkce group_by() rozdělte dataset do skupin podle ročníku a pohlaví
# (alternativně to jde i pomocí arugmentu .by až v rámci funkce summarise())
# a pak vypočtěte deskriptivní statistiky (průměry a směrodatné odchylky)
# pro všechny škály (NFC a dimenze Big Five)



# Doplňující úkoly (NEPOVINNÉ)
# Zopakujte předchozí krok, ale doplňte i údaje o počtu validních a chybějících
# hodnot




# Rozdělte dataset opět podle ročníku a pohlaví, ale místo průměrů a SD
# vypočtěte hodnoty kvartilů















