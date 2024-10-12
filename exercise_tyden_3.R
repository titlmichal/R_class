# Transformace dat ------------------------------------------------------
library(tidyverse)
library(nycflights13)
library(janitor)

# Pracujte s datasetem flights
flights
glimpse(flights)

# Zjistěte, který přepravce (carrier) mívá největší průměrné zpoždění při 
# příletu (arr_delay)
# Tj. rozdělte dataset podle přepravce, vypočtěte průměrné zpoždění pro 
# každého přepravce zvlášť a průměry seřaďte 
flights %>%
  select(carrier, arr_delay) %>%
  summarise(
    avg_arr_delay = mean(arr_delay, na.rm = TRUE)
    , .by = carrier
    ) %>% 
  arrange(desc(avg_arr_delay))


# Pomocí vhodné funkce slice_ vyberte z každého letiště odletu (origin)
# let z největším zpožděním (dep_delay)
flights %>% 
  distinct(origin)

flights %>% 
  group_by(origin) %>% 
  slice_max(dep_delay) %>% 
  select(year:day, dep_delay, carrier, origin)

# Proměnná hour udává plánovanou denní hodinu odletu
# Rozdělte dataset podle této proměnné
# a vypočtěte průměrné zpoždění při odletu (dep_delay) pro každou denní
# hodinu
flights %>% 
  select(hour, dep_delay) %>% 
  summarise(
    avg_delay = mean(dep_delay, na.rm = TRUE),
    .by = hour
  ) %>% 
  arrange(hour)

flights %>% 
  select(year:carrier, hour) %>% 
  filter(hour == 1) 
#  %>% distinct(hour, dep_delay)

# Dobrovolná, obtížnější úloha:
# Zkuste výsledek předchozího kroku poslat do funkce ggplot() a 
# Znázornit průměry pomocí sloupcového 

data = flights %>% 
  select(hour, dep_delay) %>% 
  summarise(
    avg_delay = mean(dep_delay, na.rm = TRUE),
    .by = hour
  ) %>% 
  arrange(hour)

ggplot(
  data = data,
  aes(x = hour, y = avg_delay)
) + geom_col()

# Pomocí funkce count() zjistěte, který přepravce měl nejvíce letů
# (argumentem sort nastavte, ať jsou četnosti seřazeny sestupně).
# Výsledek pak pošlete do funkce mutate a přidejte nový sloupec p 
# s relativními četnostmi

flights %>% 
  count(carrier, sort = TRUE) %>% 
  mutate(rel_freq = n / sum(n))

# Import dat ------------------------------------------------------------

# Importujte soubor hotdogs_1.txt do R jako nový objekt (hotdogs)
# Jako oddělovače jsou v něm použity tabulátory.
# Chtěli bychom zároveň, ať se první sloupce importuje jako proměnný
# typu faktor a zbylé dva sloupce jako proměnné typu integer

hotdogs <- read_tsv("data/hotdogs_1.txt") %>% 
  mutate(
    Type = factor(Type),
    Calories = as.integer(Calories),
    Sodium = as.integer(Sodium)
  )

hotdogs

# Importujte soubor swimming_pools.csv do R jako nový objekt (pools)
# Oddělovačem jsou zde čárky.
# Pomocí funkce clean_names() z balíčku janitor nastavte vhodnější názvy 
# sloupců
# Sloupec postcode převeďte na proměnnou typu integer
# a sloupec state na proměnnou typu faktor (můžete to udělat už při importu
# nebo dodatečně pomocí funkce mutate)


pools <- read_csv("data/swimming_pools.csv") %>% 
  clean_names() %>% 
  mutate(
    postcode = as.integer(postcode),
    state = factor(state)
  )

pools