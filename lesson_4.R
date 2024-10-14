install.packages("psych")

library(psych)
library(tidyverse)

# Načtení dat 
df <- read_csv2("https://is.muni.cz/el/fss/podzim2024/PSYb2320/um/datasets/personality_all.csv")
#read_csv2 protože oddělovače jsou středníky

df
glimpse(df)
#alternativně lze kliknout v environment
#id, gender, třída, a/b/c
#a hlavně položky dotazníků
# need for cognitiion a ipip (B5)

# Načtení dat i se správnou definíc missing 
df <- read_csv2("https://is.muni.cz/el/fss/podzim2024/PSYb2320/um/datasets/personality_all.csv"
                , na = c("", "-99"))
df


# Funkce distinct pro sloupec gender
distinct(df, gender)

# Převod gender na factor - když bychom chtěli použít třeba v regresi
# ať je to kategorická proměnná a ne jen stringy

df <- df %>% 
  mutate(
    gender = factor(gender,
                    levels = c("M", "F"),
                    labels = c("Boy", "Girl"))
  )

df

# Kombinace grade a class
df <- df %>% 
  mutate(
    grade_class = str_c(grade, class),
    .after = class
  )

df

# Výběr položek začínajících na "nfc
names(df)  #vypíše jména všech sloupců datasetu

nfc_items <- df %>% 
  select(starts_with("nfc"))

# Ověření reliability škál NFC
nfc_items %>% 
  psych::alpha()

#něco je asi špatně ... nízká reliabilita a negativní korelace položek
# --> reverzní položky jsou v datech 

# Použití funkce summary() na dataset

nfc_items %>% 
  summary()

# Položky NFC 10 až 16 jsou reverzní
# Rekódování hodnot položku po položce ... by bylo dost pracné
...

# Podíváme se na ně nejprve pomocí funkce select()


# Pomocí funkce across() v rámci mutate()

  

# Vektor nfc_reversed s reverzními položkami NFC (10 až 16)
df %>% 
  select(nfc_10:nfc_16)

#někdy ale položky nejdou tak hezky za sebou
#takže si udělám vektor těch položek
reverse_items <- str_c("nfc_", 10:16)
reverse_items

#...a pomocí nich vyberu
# Použití funkce all_of()
df %>% 
  select(all_of(reverse_items))

nfc_reversed <- reverse_items
nfc_reversed

# A co reverzní položky IPIP?
# Jedná se o typo položky
nums <- c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 29, 30, 32, 34, 
          36, 38, 39, 44, 46, 49)

nums

# Vytvoření vektoru s ipip_reversed s reverzními položkami IPIP
# Využití funkce str_c()
# a také funkce str_pad()
ipip_reversed <- str_c("ipip_", nums)
ipip_reversed

str_pad(1:9, width = 2, pad = 0)

ipip_reversed <- str_c("ipip_", 
                       str_pad(nums, width = 2, pad = 0))
                      #argument width nastavuje finální délku
                      #tj. u 2ciferných nic nepřidá
ipip_reversed

# Kombinace všech reverzních položek do jednoho vektoru reversed_items
reverse_items <- c(nfc_reversed, ipip_reversed)

# Uložení nerekódovaného datasetu do nového objektu not_recoded
not_recoded <- df


# Finální rekódování všech položek
df %>% 
  mutate(
    across(all_of(reverse_items),  #across a all_of na volbu a uplatnění
           ~5 - .x)     #tady vlnka (btw altgr+;+1) je tam pro custom fci
                        # a to .x je tam jako placeholder hodnot
  ) %>% 
  select(all_of(reverse_items))

df <- df %>% 
  mutate(
    across(all_of(reverse_items),
           ~5 - .x)
  )

df %>% 
  select(all_of(reverse_items))

# Cronbachovo alfa pro škálu NFC


str_c("nfc_", 
      str_pad(seq(1, 16), pad = 0, width = 2))

# Co kdybychom si vytvořili vlastní funkci item_range pro vytváření
# jmen položek, která:
# - doplní nulu k 1ciferným jménům
item_range <- function(prefix, suffix) {
  str_c(prefix, 
        str_pad(suffix, pad = 0, width = 2))
}

# Příklad použití této funkce
item_range("nfc_", 1:16)

df %>% 
  select(nfc_01:nfc_16) %>% 
  psych::alpha()

# Co kdybychom si vytvořili list "items" s názvy položek jednotlivých škál
# NFC: 1 až 16
# Extraversion: 1  6 11 16 21 26 31 36 41 46 
# Agreeableness: 2  7 12 17 22 27 32 37 42 47
# Conscientiousness: 3  8 13 18 23 28 33 38 43 48
# Emotional stability: 4  9 14 19 24 29 34 39 44 49
# Intellect: 5 10 15 20 25 30 35 40 45 
nfc <- item_range("nfc_", 1:16)
ext <- item_range("ipip_", c(1, 6, 11, 16, 21, 26, 31, 36, 41, 46))
agr <- item_range("ipip_", c(2, 7, 12, 17, 22, 27, 32, 37, 42, 47))
con <- item_range("ipip_", c(3, 8, 13, 18, 23, 28, 33, 38, 43, 48))
#emo <- item_range("ipip_", c(
#int <- item_range("ipip_", c(

items <- list(
  nfc = nfc,
  ext = ext,
  agr = agr,
  con = con
)

# Odkázat se na jednotlivé prvky listu items jde pomocí $
items$nfc
items$con


# Funkce row_mean() pro výpočet celkových skórů jako průměr položek
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

df

# Upltnění funkce row_mean() pro výpočet celkových skórů
# nfc, ext, con
# Jejich umístění někde na začátek
df <- df  %>% 
  mutate(
    nfc = row_mean(all_of(items$nfc), max.na = 6),
    ext = row_mean(all_of(items$ext), max.na = 6),
    agr = row_mean(all_of(items$agr), max.na = 6),
    .after = grade_class
  )
#průměr pro všechny nfc proměnné s tolerancí NA až 6
df


# Výpočet deskriptivních statistik v závislosti na ročníku a pohlaví
# M, SD, skew
df %>%
  summarise(
    M_nfc = mean(nfc, na.rm = TRUE),
    SD_nfc = sd(nfc, na.rm = TRUE),
    valid = sum(!is.na(nfc)), #n() by vrátila řádky, zde NOT na suma
    .by = c(grade, gender) #když bych to předtím do group_by(), tak je to stejný
  )

is.na(c(1:9, NA, NA))
!is.na(c(1:9, NA, NA))
sum(!is.na(c(1:9, NA, NA)))

df %>% 
  group_by(grade, gender) %>% #...groupnu
  summarise(                  #...shrnu
    across(nfc:agr,           #...napříč kterými sloupci to chci
           list(M = ~mean(.x, na.rm = TRUE),  #...list jakých metrik chci
                SD = ~sd(.x, na.rm = TRUE), #(víc fcí --> proto ten list)
                skew = ~psych::skew(.x)))  #btw skew má na.rm default TRUE
  )

# A co kvantily s pomocí funkce quantile()
# Se summarise() by to šlo, ale máme varování
df %>% 
  summarise(
    q = quantile(nfc, probs = c(0, .25, .5, .75, 1), #po 25% --> kvartily
                 na.rm = TRUE)
  )
#VARUJE, že summarise pro víc hodnot k vrácení není vhodná
#proto použít REFRAME

df %>% 
  group_by(grade, gender) %>% 
  reframe(
    probs = c(0, .25, .5, .75, 1),
    q = quantile(nfc, probs = c(0, .25, .5, .75, 1),
                 na.rm = TRUE)
  )
#btw probs tam jsou pro to, aby v outputu to bylo jasnější


# Pokud se nám výstup nelíbí, můžeme použít navíc pivot_wider()




# Co když chceme vypočíst statistiky pro více než jednu proměnnou?



# Vybereme s datasetu id, gender, grade, nfc, ext a con
# a zkusíme to nejdříve s využitím pivot_longer()




# Výsledek si uložíme do nové matice df_longer()





# Pak můžeme opět použít group_by() a summarise()
# Vypočteme si M, SD, ale i počet chybějících a validních hodnot




# Pokud chceme, můžeme také nejprve vyřadit řádky s chybějícími hodnotami
# pomocí funkce drop_na()




# Šlo by to i pomocí funkce across
# Nejprve si vypočteme jenom průměry




# Do listu přidáme více funkcí včetně sd()




# Musíme použít tzv. lambda funkce, abychom mohli měnit další argumenty,
# jako např. na.rm





# Ještě si můžeme vytvoři graf s četnostmi pro ročník a pohlaví
# Můžeme nejprve vypočíst četnosti pomocí count()




# Pak přidáme relativní četnosti




# Výsledek pošleme do ggplotu a použijeme funkci geom_col()





# Ještě změníme pozici na position = "dodge"





# A změníme barvy
# třeba #0D92F4 pro kluky a #C62E2E
# pomocí scale_fill_manual




