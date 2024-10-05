# Vizualizace dat -------------------------------------------------------------

library(tidyverse)
library(palmerpenguins)
glimpse(penguins)
penguins
data = penguins

# Pracujte s datasetem penguins
# Pomocí sloupcového grafu zobrazte rozdělení proměnné island
ggplot(
  data = data, 
  mapping = aes(x = island)
  ) + geom_bar()


# Pomocí histogramu zobrazte rozdělení proměnné flipper_length_mm
# a zkuste nastavit jinou šířku intervalů a graf rozdělit na fazety 
# podle proměnné species
ggplot(
  data = data,
  mapping = aes(x = flipper_length_mm)
) + geom_histogram(binwidth = 5) + facet_wrap(~species, ncol = 1)

# Pomocí grafu hustoty pravděpodobnosti zobrazte rozdělení proměnné 
# flipper_length_mm a barevně rozlište jednotlivé druhy (species), 
# a to jak barvou obrysů (color), tak výplně (fill)
# Argumentem linewidth zvyšte tloušťku křivek na jeden bod
# a argumentem alpha učiňte výplň poloprůhlednou
ggplot(
  data = data,
  mapping = aes(x = flipper_length_mm, group = species, fill = species
                , color = species)
) + geom_density(alpha = 0.5, linewidth = 1)


# Pomocí skládaného sloupcového grafu ověřte, zda je relativní rozdělení pohlaví
# u každého druhu tučňáka přibližně 50:50
ggplot(
  data = data,
  mapping = aes(x = species, fill = sex)
) + geom_bar(
  position = "fill"
  )
#přibližně ano


# Pomocí bodového grafu ukažte vztah mezi délkou (bill_length_mm) a 
# tloušťkou zobáku (bill_depth_mm)
# barvou i tvarem odlište jednotlivé druhy tučňáků
# a komentářem popište, čím se jednotlivé druhy vyznačují 
# (v grafu by to mělo být jasně vidět)
ggplot(
  data = data,
  mapping = aes(x = bill_length_mm, y = bill_depth_mm, color = species
                , shape = species)
) + geom_point(size = 2) + labs(caption = 
"Obecně se zdá, že druh Gentoo má méně tlusté, spíše delší zobáky.
Adelie mívajá zobáky tlusté, ale kratší. Chinstrap vyhrává na poli
délky i tloušťky.")


# Pomocí boxplotu zobrazte vztah mezi druhem tučňáka a jeho tělesnou 
# hmotností. Pomocí barvy (color) rozlište pohlaví tučňáka.
ggplot(
  data = data,
  mapping = aes(x = species, y = body_mass_g, color = sex)
) + geom_boxplot()


# Transformace dat --------------------------------------------------------
# Pracujte s datasetem flights

library(nycflights13)
flights
glimpse(flights)

# Pomocí funkce filter() vyberte lety, které

# - letěly do Houstonu (destinace IAH nebo HOU)
flights %>%
  filter(dest %in% c("IAH", "HOU"))

# - letěly v letních měsících (červenec až září)
flights %>%
  filter(between(month, 7, 9))

# - dorazily se zpožděním větším než dvě hodiny, i když odletěly bez zpoždění
flights %>%
  filter(dep_delay == 0 , arr_delay > 120)

# Seřaďte dataset flights podle zpoždění při odletu (sestupně) a zároveň 
# podle zpoždění při příletu (vzestupně)
flights %>%
  arrange(desc(dep_delay), arr_delay)

# Pomocí funkce distinct() zjistěte, zda každý den v roce 
# proběhl aspoň jeden let
flights %>%
  distinct(month, day)
#ano, zobrazuje 10+355 řádků

# Pomocí funkce mutate() doplňte do datasetu novou proměnnou speed,
# která bude udávat rychlost letadla v kilometrech za hodinu.
# Udělejte to tak, aby nově vytvořená proměnná byla zařazena na začátek datasetu
# a aby se změny uložily.
flights <- flights %>%
  mutate(speed = (distance/(hour + (minute/60))) * 1.609, .before = 1)

# Pomocí funkce rename() přejmenujte tuto proměnnou speed na speed_km_h
# aby byly jasné jednotky měření
flights <- flights %>%
  rename(speed_km_h = speed)

# Pomocí funkce relocate() přesuňte proměnnou speed_km_h za sloupce day
flights <- flights %>%
  relocate(speed_km_h, .after = day)

# Pomocí funkce select() z datasetu vyberte čtyři proměnné: 
# dep_time, dep_delay, arr_time a arr_delay
# Zkuste vymyslet více než jeden způsob, jak toho dosáhnout.
flights %>%
  select(dep_time, dep_delay, arr_time, arr_delay)

flights %>%
  select(5, 7, 8, 10)

flights %>%
  select(starts_with("dep") | starts_with("arr"))