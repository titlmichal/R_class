#   ____________________________________________________________________________
#   Transformace dat - pokračování                                          ####

library(tidyverse)
library(nycflights13)

filter()  #filtrování řádků
arrange() #seřazení sloupců
count()   #sečtení sloupců
select()  #výběr sloupců
rename()  #přejmenování sloupců
relocate()#přemístění sloupce


# * Použití pipe operátoru -----------------------------------
#ctrl + shift + m = %>% 
# Přehledné zřetězení více operací dohromady
flights |> 
  filter(dest == "IAH") |> 
  mutate(speed = distance / air_time * 60) |> 
  select(year:day, dep_time, carrier, flight, speed) |> 
  arrange(desc(speed))

# Bez něj by kód vypadat nějak takto
arrange(
  select(
    mutate(
      filter(
        flights, 
        dest == "IAH"
      ),
      speed = distance / air_time * 60
    ),
    year:day, dep_time, carrier, flight, speed
  ),
  desc(speed)
)

# Nebo bychom museli vytvářet mnoho nových objektů
flights1 <- filter(flights, dest == "IAH")
flights2 <- mutate(flights1, speed = distance / air_time * 60)
flights3 <- select(flights2, year:day, dep_time, carrier, flight, speed)
arrange(flights3, desc(speed))


# * Rozdělení datasetu do skupin --------------------------------------------

# Funkce group_by() rozdělí dataset do skupin pro další analýzu
# Všimněte si výstupu groups --> výstup popíše seskupení viz:
## Groups:   month [12]
flights |> 
  group_by(month)

#btw |> je to stejné cca jako %>% 

# V kombinaci s funkcí summarise() můžeme vypočíst různé statistiky
# Třeba průměrné zpoždění při odletu
# Chybějící hodnoty jsou "nakažlivé
#by default totiž jedna null znamená, že výsledek je null

flights |> 
  group_by(month) |> 
  summarise(
    avg_delay = mean(dep_delay)
  )

# Arugmentem na.rm je budeme ignorovat
# tedy remove na = TRUE
flights |> 
  group_by(month) |> 
  summarise(
    avg_delay = mean(dep_delay, na.rm = TRUE)
  )

# Funkce n() podá informace o počtu řádků
flights |> 
  group_by(month) |> 
  summarise(
    avg_delay = mean(dep_delay, na.rm = TRUE), 
    n = n()
  )

# Funkce slice_
grouped <- flights %>% 
  group_by(month)

# slice_head() Vybere PRVNÍ řádek/řádky z každé skupiny
grouped %>% 
  slice_head(n = 1)
# vybírá z KAŽDÉ SKUPINY, zde tedy z měsíce --> 12 řádků

# slice_tail() Vybere POSLEDNÍ řádek/řádky z každé skupiny
grouped %>% 
  slice_tail(n = 1)

# slice_min() vybere řádek/řádky s nejmenšími hodnotami zvoleného sloupce
grouped %>% 
  slice_min(dep_delay, n = 1)
#zde musím říct, s nejmíň čeho (jaké proměnné)

# slice_max() Vybere řádek/řádky s největšími hodnotami zvoleného sloupce
grouped %>% 
  slice_max(arr_delay, n = 1)

# slice_sample() vybere náhodné řádky
grouped %>% 
  slice_sample(n = 1)
# zase vybírá z KAŽDÉ SKUPINY random n řádků

# Do funkce group_by můžeme dát více proměnných
daily <- flights |>  
  group_by(year, month, day)
#takže výsledkem je tabulka seskupená dle roku, měsíce a dne

daily

summarise(daily, n=n())

daily %>% 
  summarise(
    avg_daily_delay = mean(dep_delay, na.rm = TRUE)
  )
# Všimněte si, že funkce summarise() defaultně "odloupne" dělení podle
# poslední proměnné (ve výsledném datasetu)
# Toto chování lze změnit pomocí argumentu .groups
?summarise

# Zrušit rozdělení do skupin jde pomocí funkce ungroup
daily %>% 
  ungroup() %>% 
  summarise(
    avg_total_delay = mean(dep_delay, na.rm = TRUE)
  )

# Nověji jde pracovat i bez funkce group_by() a specifikovat rozdělení
# datasetu do skupin až v samotné funkci summarise()
# pomocí argumentu .by
flights |> 
  summarise(
    delay = mean(dep_delay, na.rm = TRUE), 
    n = n(),
    .by = c(origin, dest)
  )
#výsledek totozný, ale není to pak děleno dál do skupin
#zůstává to v tomto call funkce
#tedy buď prvně groupnu samotnou funkcí a specifikují čím
# NEBO
#groupnu pak až v summarise() fci pomocí argumentu .by = c(sloupec1,sloupce2)


#   ____________________________________________________________________________
#   Import dat                                                              ####

# * Flat files -------------------------------------------------------------
#flat file = dvourozměrné soubory, otevřetelné třeba v notepadu
#viz třeba csvéčka

# Funkce balíčku readr (spadá pod tidyverse)
# funkce read_csv() čte data, kde je oddělovačem hodnot čárka
# funkce read_csv2() čte data, kde je oddělovačem hodnot středník
# funkce read_tsv() čte data, kde je oddělovačem hodnot tabulátor
# funkce read_delim() je obecná funkce, která umožňuje specifikovat jakýkoli
# oddělovat hodnot

students <- read_csv("https://pos.it/r4ds-students-csv")
#tohle je html adresa na github
students1 <- read_csv("data/students.csv")
#když to mám lokálně, stačí relativní cesta

# Dataset se nám importoval, ale nevypadá úplně OK
students
students1
#missing values červeným, některé NA je jako N/A, kterou ale Rko už nepoznalo
#plus jeden věk je tam jako five, ne 5, takže i ostatní uložil jako string

# Nejdříve můžeme redefinovat chybějící hodnoty
students <- read_csv("https://pos.it/r4ds-students-csv",
                     na = c("", "N/A"))
students   #teď už svítí i původní N/A
#ale pořád tam jsou problémy:
#zejména jména sloupců --> fce rename()
#ALE lze použít balíček janitor - viz níže

# Můžeme také změnit názvy sloupců, ať jsou konzistentní a lépe se na ně
# odkazuje
students |> 
  rename(
    student_id = `Student ID`,
    full_name = `Full Name`
  )

# Automatizovaně s využitím balíčku janitor a funkce clean_names()
#janitor použiuje snakeCase
#ty :: jsou pro odkázání se na knihovnu/balíček, ale není to nutné
students <- students |> 
  janitor::clean_names()
students

# Dále můžeme opravit sloupec age
#funkce if_else...dost podobné jak v excelu
#pokud je age "five", pak vráti "5", jinak klasický age
#a změní se tím sloupce
students %>% 
  mutate(age = if_else(age == "five", true = "5", false = age))

students %>% 
  mutate(age = if_else(age == "five", true = "5", false = age) %>% 
           as.integer())
          #plus tady z toho uděláme integer

students <- students %>% 
  mutate(age = if_else(age == "five", true = "5", false = age) %>% 
           as.integer())
students

# Nakonec můžeme meal_plan změnit na kategorickou proměnnou
#pomocí fce mutate (na změnu sloupce) a factor
#když to převedeme na kategorickou, tak vrátí počty (místo jen délky a Mo)
summary(students)

students <- students %>% 
  mutate(
    meal_plan = factor(meal_plan)
  )

summary(students)

# Balíček readr většinou dovede správně identifikovat typ sloupců

# Mohli bychom například mít tento .csv soubor
example <- "
  logical,numeric,date,string
  TRUE,1,2021-01-15,abc
  false,4.5,2021-02-15,def
  T,Inf,2021-02-16,ghi
"

# Načteno ok
read_csv(example)

# Někdy je nutné ručně definovat chybějící hodnoty
simple_csv <- "
  x
  10
  .
  20
  30"

read_csv(simple_csv)

read_csv(simple_csv, na = ".")

# Nebo ručně specifikovat, jaký typ proměnné chceme, pomocí argumentu col_types
df <- read_csv(
  simple_csv, 
  col_types = list(x = col_double())
)

# A pak se pomocí funkce problems() podívat, jestli při importu nastal problém
problems(df)
#na řádku 3 sloupce 1 čekal číslo, ale byla tam tečka
df
#takže to pak není tam v tom df jako tečka, ale NA

# Různé typy sloupců k nastavení - ty pak hážu DO TÉ FCE col_type()
# col_logical(): logické hodnoty 0/1, TRUE/FALSE
# col_integer(): celá čísla
# col_double(): reálná čísla
# col_character(): text
# col_factor(): kategorické proměnné
# col_date(): datum
# col_datetime(): datum a čas
# col_number(): z "textu" importuje pouze numerické hodnoty
# col_skip(): sloupec není importován, ale "přeskočen"


# Další cvičný csv soubor
another_csv <- "
x,y,z
1,2,3
4,5,6"

read_csv(another_csv)
#tady to dobře rozpozná jako doubles

# Taky můžeme změnit defaultní typ sloupců, např. kdybychom pro jistotu
# chtěli nejprve vše nejprve importovat jako text
read_csv(
  another_csv, 
  col_types = cols(.default = col_character())
)
#tady jsem mu řekl, že všechny jsou stringy/characters

# Anebo můžeme ručně definovat, že se mají importovat pouze určité sloupce
# Takto bychom specifikovali, že se sloupec x má importovat jako textový
# (ostatní sloupce automaticky)
read_csv(
  another_csv, 
  col_types = list(x = col_character(),
                   y = col_integer())
)

# Takto bychom specifikovali, že se sloupec x má importovat jako textový
# a že jiné se importovat nemají
read_csv(
  another_csv,
  col_types = cols_only(x = col_character())
)
#cols_only importuje jen definované, ostatní nepustí
# NEBO
#list pustí vše, nastaví vše, co je definované, a zbytek automaticky

#DŮLEŽITÉ Z TÉTO SEKCE
#col_types argument k definování typu hodnot
  #list (definované i ostatní) X cols_only (jen definované)
  #cols(.default = col_dany_typ_co_chcu()) pro default všech
#na argument k rozpoznání missing values


# * SPSS -----------------------------------------------------------------
# Balíček haven
library(haven)

# Načtení dat z SPSS obvykle není problematické
international <- read_spss("data/international.sav")
#zase stačí (relativní) cesta
international
#defaultně bere data z SPSS jakou doubles, pokud tam jsou čísla
#tak nějak to (ne)počítá, že SPSS používají psychologové
#a u faktorů bychom nespočítali naše oblíbené statistiky

#btw na zjištění, kde zrovna jsem
getwd()

# Jediné, co nám nemusí vyhovovat, je to, že se proměnné definované
# v SPSS jako nominální a ordinální importují jako numerické
# ale labely jsou zachovány

# Ale balíček haven proto má pomocnou funkci as_factor
?as_factor

international %>% 
  mutate(
    contint = as_factor(contint)
  )

# * Excel --------------------------------------------------------------
# Spíše případová studie
library(readxl)
install.packages('janitor')
library(janitor)

df <- read_excel("data/four_countries_data.xlsx")

# Takhle bychom to nechtěli
df

# Nejprve opravíme názvy proměnných a někam si je uložíme
var_names <- read_excel("data/four_countries_data.xlsx",
                        n_max = 0) %>% 
  clean_names() %>% 
  colnames()
#načtu data (n_max říká kolik řádků, rozpoznané headers nemají řádek), 
#očistím názvy a uložím názvy sloupců do var_names

var_names
#teď mám vektor s 53 prvky názvů sloupců

# Na druhém řádku jsou popisnější labely proměnných, ty si také uložíme
var_labels <- read_excel("data/four_countries_data.xlsx",
           skip = 1,
           n_max = 0) %>% 
  colnames()
var_labels
#teď to je to stejný, akorát pro labely sloupců
#takže asi vektor o 53 prvcích

metadata <- tibble(
  names = var_names,
  labels = var_labels
)
metadata
#tohle vytvoří "tabulku" pro uložené labely a descriptions

# Pak konečně importujeme dataset bez tří prvních řádků
df <- read_excel("data/four_countries_data.xlsx",
                 skip = 3,
                 col_names = FALSE)
df
#takže teď načíst zase stejně ten excel, ale skipnou se 3 řádky
#a col_names = FALSE znamená, že se nenačtou a použijou se čísla

# Pomocí set_names() přepíšeme názvy sloupců
df <- df %>% 
  set_names(var_names)
df

# Dále můžeme odstranit některé nepotřebné sloupce 
# např. progress
# s využitím funkcí balíčku janitor
# použiju fci remove_constant, která odstraní sloupce s konstantami
# a remove_empty, která zahodí ty sloupce, které nemají žádnou hodnotu
df <- df %>% 
  remove_constant() %>% 
  remove_empty(which = "cols")
df

# Někdy se z Excelu importuje chybně datum jako numerická proměnná
# (počet dní od data 1899-12-30) 
# I pro to má janitor užitečnou funkci
# fce excel_numeric_to_date
df <- df %>% 
  mutate(start_date = excel_numeric_to_date(start_date),
         end_date = excel_numeric_to_date(end_date),
         recorded_date = excel_numeric_to_date(recorded_date))
df
#hurá, jsou z toho data (obsahem i formátem)
#kdyby ale ty labely tam nepřekážely, tak by stačil jen:
  #df <- read_csv("relativni_cesta_k_souboru.csv")

#fci save pak můžu použít na uložení
save(df, students, grouped, international,
     file = "data/my_data.Rdata")
#když si pak cleannu workspace...

load("data/my_data.Rdata")
#...tak si ty data můžu tady zase zpátky načíst

#btw pokud bych nechtěl používat původní název, tak můžu použít write_rds
write_rds(df,
          file = "data/df.rds")

random_nazev <- read_rds("data/df.rds")
random_nazev

#...a pak to můžu klidně hodit jako csv zase
write_csv(df,
          file = "data/df.csv")