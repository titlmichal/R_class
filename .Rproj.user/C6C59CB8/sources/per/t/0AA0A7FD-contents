#   ____________________________________________________________________________
#   Základy vizualizace dat                                                 ####
library(tidyverse)
library(palmerpenguins)

# Zatím jsme se odkazovali na hlavní argumenty funkce ggplot() explicitně
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point()

# Abychom si ušetřili psaní, jde to i implicitně, protože když neuvedeme 
# názvy argumentů, bude funkce očekávat na první pozici data a na druhé
# pozici mapování proměnných
ggplot(
  penguins,
  aes(x = flipper_length_mm, 
      y = body_mass_g)
) +
  geom_point()

# Můžeme se už začít seznamovat s operátorem pipe %>% 
penguins %>% 
  ggplot(aes(x = flipper_length_mm, 
             y = body_mass_g)) +
  geom_point()


# * Vizualizace rozdělení (distribuce) jedné proměnné ------------------------

# Když chceme znázornit rozdělení jedné kategorické proměnné, obvykle použijeme
# sloupcový graf
ggplot(penguins, aes(x = species)) +
  geom_bar()

# Často chceme kategorie seřadit podle zastoupení, toho můžeme docílit pomocí 
# funkce fct_infreq()
?fct_inorder 

ggplot(penguins, aes(x = fct_infreq(species))) +
  geom_bar()

# Ke znázornění distribuce numerické proměnné často používáme histogramy
ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram()
#u histogramu není třeba specifikovat y, protože jen počítáme a balíček to ví

#TADY NĚKDE JSEM PŘIŠEL DO (ONLINE) HODINY

# Šířku intervalů můžeme specifikovat argumentem binwidth a vybrat nějakou 
# rozumnou hodnotu
ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(binwidth = 200)

ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(binwidth = 2000)

ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(binwidth = 20)

# Anebo můžeme specifikovat počet sloupců argumentem bins
#Rko tím prostě rozdělí proměnnou na daný počet kategorií
ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(bins = 30)

ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(bins = 300)

ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(bins = 10)

# Alternativně můžeme použít také graf hustoty pravděpodobnosti
ggplot(penguins, aes(x = body_mass_g)) +
  geom_density()
#něco jako histogramu, ale neskládáme kostičky
#ale takové kinda gaussovky
#btw plocha pod křivkou = 1

# * Vizualizace vztahů mezi proměnnými ----------------------------------

# Vztah mezi kategorickou a numerickou proměnnou můžeme znázornit pomocí 
# krabicového grafu (boxplotu)
ggplot(penguins, aes(x = species, y = body_mass_g)) +
  geom_boxplot()
#zápis je zase stejný - ggplot, dataset, osy a plus geom_boxplot
ggplot(penguins, aes(y = species, x = body_mass_g)) +
  geom_boxplot()
#osy můžeme klidně převrátit
#jinak boxplot žejo:
#minimum - 3. kvartil - Md - 1. kvartil - maximum

# Nebo můžeme použít graf hustoty pravděpodobnosti a barevně odlišit úrovně
# kategorické proměnné
ggplot(penguins, aes(x = body_mass_g, color = species)) +
  geom_density(linewidth = 0.75)

# Funkce geom_density() umožňuje specifikovat jak barvu křivek, tak výplně
ggplot(penguins, aes(x = body_mass_g, 
                     color = species,
                     fill = species)) +
  geom_density()
#tj. barevně odlišit nejen barvou křivky, ale výplní
#nehodí se to, ale kdžy se příkrávají - viz de

#proto vhodné zvolit míru ne/průhlednosti
#stačí hodit alpha = ... do geom_density
ggplot(penguins, aes(x = body_mass_g, 
                     color = species,
                     fill = species)) +
  geom_density(alpha = .250)

# Vztah mezi dvěma kategorickými proměnnými můžeme znázornit pomocí sloupcového
# grafu

# Defaultně zobrazuje absolutní četnosti a sloupce jsou naskládány na sebe
# zde je druh tučňáka dle ostrovů
# defaultně je graf stacked a jde o absolutní četnosti
ggplot(penguins, aes(x = island, fill = species)) +
  geom_bar()

# Když nastavíme v v rámci geom_bar() argument position na hodnotu "fill"
# zobrazí se nám relativní četnosti v rámci každé kategorie na ose X
ggplot(penguins, aes(x = island, fill = species)) +
  geom_bar(position = "fill")

# Když máme dvě numerické proměnné, můžeme jejich vztah znázornit pomocí 
# bodového grafu (scatterplotu) --> geom_point()
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point()

# Informace o dalších proměnných pak můžeme mapovat na další vlastnosti bodů
# např. barvu nebo tvar
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(color = species, shape = island),
             size = 3)
#tady oproti geom_density příkladu specifujeme barvu a tvary
#až v geom_point fci, ALE zde je to jedno,
#protože když se něco specifikuje v ggplotu,
#tak to zdědí geomy v rámci něj

# Ale pak může být graf informačně přehlcený

# Namísto toho můžeme graf rozdělit na několik samostatných panelů podle úrovní
# některé z proměnných, a to pomocí funkce facet_wrap()
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(color = species, shape = species)) +
  facet_wrap(~island, ncol = 1)
#ncol argumentem říkáme number of columns

# * Uložení grafu ---------------------------------------------------
# Vytvořené grafy můžeme uložit na disk pomocí funkce ggsave()
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point()

# Defaultně se uloží poslední vytvořený graf v aktuálních rozměrech
?ggsave
ggsave(filename = "plots/penguin-plot-1.png")
#stačí specifikovat relativní cestu
#defaultní velikost je daná oknem v R studio
#formát souboru volím v té cestě
#NECHCE specifikovat graf k uložení --> ukládá poslední zobrazený
#alternativně lze objekt někam uložit a pak specifikovat
p <- ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point()

ggsave(filename = "plots/penguin-plot-2.png",
       plot = p)

# Funkce ggsave() má různé argumenty, jimiž můžeme měnit např. rozměry nebo
# rozlišení grafu
ggsave(filename = "plots/penguin-plot-3.png",
       units = "cm",
       width = 20,
       height = 20)

#   ____________________________________________________________________________
#   Transformace dat                                                        ####

install.packages("nycflights13")
library(nycflights13)
flights
?flights
glimpse(flights)
#za názvy jsou datové typy
summary(flights)
#vyjede deskriptivy s proměnným

# Všechny funkce balíčku dplyr fungují na podobných principech.
# Prvním argumentem je vždy dataset, který chceme použít
# Dalšími argumenty specifikuje, které sloupce chceme využít nebo co s nimi 
# chceme dělat.
# A výstupem je vždy nový dataset.

# Často chceme provést více operací zároveň.
# K jejich zřetězení se hodí pipe operátor %>% 
# z mého chápání ten pipe umožní zřetězit operace
flights  %>%    #vyberu dataset
  filter(dest == "IAH") %>%   #vyfiltruju dataset
  group_by(year, month, day) %>%  #groupnu dle proměnných
  summarize(
    arr_delay = mean(arr_delay, na.rm = TRUE)
  )   #a shrnu
#prý není třeba znát specifika tohoto konkrétního kousku kódu
#je to jen ukázka funkcí z balíčku dplyr (ten je součásti tidyverse)

# * Výběr a uspořádání řádků datasetu --------------------------------

# Funkce filter() slouží k výběru řádků, které chceme zachovat
flights %>% 
  filter(dep_delay > 120)
#zpoždění letadla při odletu v minutách (větší jak 120)

# Flights that departed on January 1
flights %>%  
  filter(month == 1 & day == 1)
#tyhle 2 statementy jsou stejné
flights %>%  
  filter(month == 1, day == 1)
#nemusíme specifikovat rok, protože vše je z roku 2013
#btw rovná se jsou 2 == podobně jak v Pythonu
#jedním = bych přiradil hodnotu

# Flights that departed in January OR February
flights %>%  
  filter(month == 1 | month == 2)

# A shorter way to select flights that departed in January or February
flights %>%  
  filter(month %in% c(2, 3))

#když chci rozmezí:
flights %>%
  filter(between(month, 6, 8))

# Funkce dplyr NIKDY NEmodifikují PŮVODNÍ dataset
flights %>%  
  filter(month == 3 & day == 5)
flights

# Když chceme změny uložit, obvykle do nového objektu, musíme použít 
# assignment operátor
jan1 <- flights %>%  
  filter(month == 1 & day == 1)
jan1

# Nejčastější chyby
flights %>%  
  filter(month = 1)
flights %>%  
  filter(month == 1 | 2)

# Funkce arrange slouží k seřazení řádků podle hodnot zvolených proměnných.
# Na rozdíl od funkce filter() nevyřazuje řádky.
flights %>%  
  arrange(year, month, day, dep_time)

# Pokud chceme sestupné řazení, můžeme použít pomocnou funkci desc()
flights %>%  
  arrange(desc(dep_delay))

# Funkce distinct() slouží k výběru všech jedinečných řádků.
# To znamená těch řádků, pro které se hodnoty v daném sloupci nebo kombinaci
# sloupců neopakují.

# Vymaž jakékoli duplicitní řádky
flights %>%  
  distinct()

# Vyber všechny jedinečné řádky pro sloupce origin a dest
flights %>%  
  distinct(origin, dest) %>%
  arrange(origin, dest)

# Funkce count() pak slouží ke zjištění četností kombinací
flights %>% 
  count(origin, dest, sort = TRUE)
#sort = TRUE ... seřaď sestupně

#takže fce filter arrange, distinct, count apod.

# * Tvorba, výběr, přejmenování a přesunutí sloupců datasetu -----------------

#další z dplyr, tentokrát pro sloupce

# Funkce mutate() slouží k tvorbě nových sloupců
#zase první argumentem je dataset
#(skrze použití pipeoperátoru to nevypadá
#protože pipe operátor bere výsledek předchozí operace
#a dává jej jako první argumentem do navazující fce)
flights %>%  
  mutate(
    air_delay = arr_delay - dep_delay,
    speed = (distance*1.609344) / (air_time / 60)
  )

# Argumentem .before nebo .after můžeme specifikovat, kde se nové sloupce
#buď pořadí sloupce nebo název
# mají vložit (defaultně budou až na konci)
#tyhle fce taky neupravují původní dataset --> pro uložení nový objekt
flights %>%  
  mutate(
    air_delay = arr_delay - dep_delay,
    speed = (distance*1.609344) / (air_time / 60),
    .before = 1
  )

flights %>%  
  mutate(
    air_delay = arr_delay - dep_delay,
    speed = (distance*1.609344) / (air_time / 60),
    .after = day
  )

# Pomocí argumentu .keep můžeme specifikovat, 
# které z původních sloupců chceme zachovat
?mutate
flights %>%  
  mutate(
    air_delay = arr_delay - dep_delay,
    speed = (distance*1.609344) / (air_time / 60),
    .keep = "none"
  )
#none se hodí, když vytvářím nové sloupce a původní už nepotřebuju

flights %>%  
  mutate(
    air_delay = arr_delay - dep_delay,
    speed = (distance*1.609344) / (air_time / 60),
    .keep = "used"
  )

flights %>%  
  mutate(
    air_delay = arr_delay - dep_delay,
    speed = (distance*1.609344) / (air_time / 60),
    .keep = "unused"
  )

# Funkce select() slouží k výběru sloupců, které chceme zachovat
#opět z dplyr, nemodifikuje původní dataset
# Pomocí jména:
flights %>%  
  select(year, month, day, dep_delay)

# Všechny sloupce v rozmezí:
flights %>%  
  select(year:dep_delay)

# Všechny sloupce MIMO rozmezí:
flights %>%  
  select(!month:day)

# Výběr sloupců podle typu dat:
flights %>%  
  select(where(is.character))

flights %>%  
  select(where(is.numeric))

flights %>%  
  select(where(is.double))

# Pomocné funkce starts_with(), ends_with() a contains()
#je to pro názvy sloupců, ne obsah/řádky
#filter vybírá řádky, select sloupce
flights

flights %>%  
  select(starts_with("dep"))

flights %>%  
  select(ends_with("time"))

flights %>%  
  select(contains("arr"))

# Funkce select() můžeme sloužit i rovnou k přejmenování sloupce
flights %>%  
  select(tail_num = tailnum)

# Funkce rename() na rozdíl od funkce select() pouze přejmenuje vybrané sloupce,
# ale ponechá všechny ostatní
flights %>%  
  rename(tail_num = tailnum)


# A konečně funkce relocate() slouží k přesunu sloupců
#ýběr sloupců podobně jako u selectu
#místo podobně z mutate
glimpse(flights)
flights %>%  
  relocate(distance:time_hour, .after = day)

flights  %>%  
  relocate(starts_with("arr"), .before = dep_time)
