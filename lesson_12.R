##%######################################################%##
#                                                          #
####                 Writing functions                  ####
#                                                          #
##%######################################################%##

library(tidyverse)

# Funkce jsou prvník krokem k tomu vyhdnout se zbytečnému copy-pastování

# Vector functions ----------------------------------------

# Jsou funkce pro práci s atomickými vektory, takže obvykle je vstupním 
# argumentem nějaký atomický vektor a výstupem také atomický vektor
# v nějaké přetvořené podobě

# Kdy si vytvořit vlastní funkci?
# Nejlépe kdykoli, když nějaký kód kopírujeme více než dvakrát

# Cvičný dataframe
df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10),
  e = rnorm(10)
)

df

# Zkuste říct, co dělá tento kód
#přeškálovává -> převádí sloupec na hodnoty 0 až 1

df$a <- (df$a - min(df$a, na.rm = TRUE)) / 
  (max(df$a, na.rm = TRUE) - min(df$a, na.rm = TRUE))
df$b <- (df$b - min(df$b, na.rm = TRUE)) / 
  (max(df$b, na.rm = TRUE) - min(df$a, na.rm = TRUE))
df$c <- (df$c - min(df$c, na.rm = TRUE)) / 
  (max(df$c, na.rm = TRUE) - min(df$c, na.rm = TRUE))
df$d <- (df$d - min(df$d, na.rm = TRUE)) / 
  (max(df$d, na.rm = TRUE) - min(df$d, na.rm = TRUE))
df

# Abychom mohli vytvořit funkci, nejdříve musíme zjistit,
# KOLIK bude mít vstupních ARGUMENTŮ (inputs)

(df$e - min(df$e, na.rm = TRUE)) /
  (max(df$e, na.rm = TRUE) - min(df$e, na.rm = TRUE))

# Tento kód má jen jeden hlavní argument: vstupní atomický vektor df$a
# Pro zpřehlednění mu můžeme dát nějaký obecný název, např. x

x <- df$e
(x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))

# Duplikaci kódu můžeme dále omezit použitím range() namísto min() a max()
range(x)
  #vrací minimum a maximum
rng <- range(x)

# Protože funkce range vrací minimum i maximum
#stále bez použití fce, protože tam sypeme to definované x výše
(x - rng[1]) / (rng[2] - rng[1])


# Když máme vytvořen základní kód a víme že funguje, můžeme jej přetvořit 
# ve funkci
#1) musíme ji nějak nazvat
#2) pak assignment operator
#3) pak funkce k tvorbě fcí function(argument1, ..., argument n)
#4) pak složené závorky {}, do kterých řekneme, co se bude dít ve fci
#5) retunr nemusíme explicitně definovat, by default bere poslední řádek

rescale_01 <- function(x) {
  rng <- range(x, na.rm = TRUE)     #vyrobí range
  (x - rng[1]) / (rng[2] - rng[1])  #a pak spočítá to přeškálování
}

rescale_01(c(0, 5, 10))

# Tvorba funkce zahrnuje tři kroky:
# JMÉNO, VSTUPNÍ HODNOTY/ARGUMENTY (INPUTS) A TĚLO FUNKCE (BODY)
# 1. musíme vybrat příhodné jméno, podle toho, k čemu daná funkce slouží
# 2. pak musíme vypsat názvy jejích argumentů čili vstupních hodnot (inputs)
#    v rámci function()
# 3. nakonec následuje samotný kód funkce, který přetváří vstupní hodnoty
#    ve výstupní (outputs) a obvykle jej obklopujeme {...}

# function_name <- function(input_arguments) {
#    function_body
#}

# Když funkci vytvoříme, měli bychom ji vyzkoušet s různými vstupními
# hodnotami, jestli funguje tak, jak má

rescale_01(c(-10, 0, 10))
rescale_01(c(1, 2, 3, NA, 5))

# Další výhodou funkcí je to, že když narazíme na něco, s čím jsme nepočítali,
# stačí učinit jen malou změnu dané funkce.

# Např. když zjistíme, že některé proměnné zahrnují nekonečno a funkce
# rescale_01 nepracuje tak, jak potřebujeme
x <- c(1:10, NA, Inf)
rescale_01(x)
  #převede hodnoty na 0, protože tam má ty NA a nekonečno, co dělá nepořádek

# Protože k tranformaci dat jsme použili funkci, stačí změnit jen její chování
# např. doplnit finite = TRUE, aby funkce ponechala -Inf a +Inf (ale i NA) beze
# změny a nezahrnovala je do výpočtu rng

rescale_01 <- function(x) {
  rng <- range(x, finite = TRUE) # Ignorovat nekonečné hodnoty 
  (x - rng[1]) / (rng[2] - rng[1])
}

rescale_01(x)

rescale_01(c(-Inf, 1:10, Inf))

rescale_01 <- function(x) {
  rng <- range(x, finite = TRUE) # Ignorovat nekonečné hodnoty 
  output <- (x - rng[1]) / (rng[2] - rng[1])
  output[output == -Inf] <- 0
  output[output == Inf] <- 1
  return(output)
}

rescale_01(c(-Inf, 1:10, Inf))

#argumenty lze přiřadit explicitně nebo pořadím
mean(x = 1:10, trim = 0, na.rm = TRUE)
mean(1:10, 0, TRUE)

#úzus je datové nespecifikovat jménem,
#ale ostatní upravující fci funkce je to vhodné
mean(1:10, trim = 0, na.rm = TRUE)


# * Funkce pro použití v rámci mutate() ------------------------
# Tyto funkce obvykle vracejí VÍCE než jednu hodnotu
# Slouží pro transformaci proměnných

# Cvičný dataframe
df <- tibble(
  a = runif(10, min = 0, max = 100),
  b = runif(10, min = 0, max = 100),
  c = runif(10, min = 0, max = 100),
  d = runif(10, min = 0, max = 100),
  e = runif(10, min = 0, max = 100),
)

# Použití funkce rescale_01, kterou jsme vytvořili
df %>% 
  mutate(a = rescale_01(a),
         b = rescale_01(b))

#btw lze to dál iterovat pomocí across()
df %>% 
  mutate(
    across(a:e, rescale_01)  #když bych to chtěl psát ze závorkou
  )                          #tak bych tam musel dát vlnku před a za to (.)

# Další příklady
# Často chceme proměnné standardizovat na z-skóry
z_score <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

df %>% 
  mutate(a = z_score(a))

# Nebo omezit shora a zdola hodnoty nějaké proměnné
#clamp převádí nižší hodnoty na minimum a vyšší na maximum
clamp <- function(x, min, max) {
  case_when(
    x < min ~ min,
    x > max ~ max,
    .default = x
  )
}

clamp(1:10, min = 3, max = 7)

# Můžeme pracovat s textem a chtít kapitalizovat první písmeno
first_upper <- function(x) {
  str_sub(x, 1, 1) <- str_to_upper(str_sub(x, 1, 1))
  x
}

first_upper("hello")

# Nebo z čísel speciální znaky odstranit znaky jako % nebo $
clean_number <- function(x) {
  is_pct <- str_detect(x, "%")
  num <- x %>% 
    str_remove_all("%") %>% 
    str_remove_all(",") %>% 
    str_remove_all(fixed("$")) %>% 
    as.numeric()
  if_else(is_pct, num / 100, num)
}

clean_number("$12,300")
clean_number("45%")

# Případně změnit některé hodnoty na missing values 
fix_na <- function(x) {
  if_else(x %in% c(997, 998, 999), NA, x)
}

fix_na(c(12, 997, 33, 998,  45, 999))

#můžeme si i dospecifikovat argument na dané missing
fix_na <- function(x, missing) {
  if_else(x %in% missing, NA, x)
}

fix_na(c(12, 997, 33, 998,  45, 999), c(997, 998, 999))

#btw na IQR je fce() .... jmenuje se IQR()
# (:

# * Funkce pro použití v rámci summary ----------------------------------
# Tyto funkce obvykle vrací jen jednu hodnotu, protože nějakým způsobem
# sumarizují data

# Například tato funkce spojít všechny prvky dohromady definovaným
# oddělovačem
commas <- function(x) {
  str_flatten(x, collapse = ", ", last = " and ")
}

commas(c("cat", "dog", "pigeon"))

# Tato funkce by vypočetla variační koeficient
#poměr mezi SD a průměrem
var_coef <- function(x, na.rm = FALSE) {
  sd(x, na.rm = na.rm) / mean(x, na.rm = na.rm)
}

var_coef(runif(100, min = 0, max = 50))
var_coef(runif(100, min = 0, max = 500))

# Tato funkce by vypočetla počet chybějících hodnot
n_missing <- function(x) {
  sum(is.na(x))
} 

n_missing(c(NA, NA, NA, 1:7))

n_missin_ratio <- function(x) {
  mean(is.na(x))
} 

n_missin_ratio(c(NA, NA, NA, 1:7))

df %>% 
  summarise(n_missing(a))
  #takže si můžu dosypat do summarise, co potřebuju

# Dataframe functions ----------------------------------------------

# To jsou funkce, které pracují s dataframy, to znamená, že jako vstupní 
# argument používají dataframe/tibble a vracejí ho v nějaké přetvořené 
# podobě
#Většinou to jsou takové nejnáročnější fce

# Když chceme používat funkce balíčku dplyr pro psaní vlastních funkcí
# není to tak jednoduché, jak se na první pohled zdá
grouped_mean <- function(df, group_var, mean_var) {
  df %>%  
    group_by(group_var) %>% 
    summarize(mean(mean_var))
}

diamonds
# Tento kód háže chybu
diamonds %>% 
  grouped_mean(cut, carat)

# Protože do dplyr chápe doslovně jako
#takže se dívá po proměnné, co se jmenuje group_var
#a nedívá se do těch vstupních argumentů
diamonds %>% 
  group_by(group_var) %>% 
  summarize(mean(mean_var))

# Místo toho, aby dosadil hodnoty vstupních argumentů
#--> musíme to říct
diamonds %>% 
  group_by(cut) %>% 
  summarize(mean(carat))


# Aby dplyr pochopil, co po něm chceme, musíme použít tzv. embracing {{  }}
# Tím mu řekneme, že např. group_var nemá chápat doslova jako jméno proměnné
# z dataframu, ale má místo toho použít hodnotu vstupního argumentu group_var
#takže {{group_var}} bude už chápat, jako vstupní argument, ne název proměnné
grouped_mean <- function(df, group_var, mean_var) {
  df %>% 
    group_by({{ group_var }}) %>% 
    summarize(mean({{ mean_var }}))
}
 
# Teď už funkce pracuje tak, jak má
diamonds %>% 
  grouped_mean(cut, carat)

# Je to proto, že funkce jako arrange(), count(), filter(), group_by(), 
# mutate(), a  summarise() používají tzv. data masking a primárně hledají 
# proměnné, na které se odkazujeme, v rámci samotného dataframu, ne v globálním
# prostředí, z něhož pocházejí i vstupní argumenty

#takže je to vlastně fajn, když pracujeme s built-in věcmi a nemusíme tak řešit
#odkazování na datové zdroj/df/..., ale pro vlastní definici fcí to může být náročné

# Další příklady

# Funkce pro výpočet deskriptivních statistik stanovené proměnné
summary6 <- function(data, var) {
  data %>% summarize(
    min = min({{ var }}, na.rm = TRUE),
    mean = mean({{ var }}, na.rm = TRUE),
    median = median({{ var }}, na.rm = TRUE),
    max = max({{ var }}, na.rm = TRUE),
    n = n(),
    n_miss = sum(is.na({{ var }})),
    .groups = "drop"
  )
}

diamonds %>% 
  summary6(carat)

diamonds %>% 
  group_by(cut) %>% 
  summary6(carat)

# Funkce pro výpočet relativních četností určité proměnné
count_prop <- function(df, var, sort = FALSE) {
  df %>%
    count({{ var }}, sort = sort) %>%
    mutate(prop = n / sum(n))
}

diamonds %>% 
  count_prop(clarity)


# Někdy chceme vybrat více proměnných v rámci funkce, která používá 
# data-masking, jako je např group_by()
count_missing <- function(df, group_vars, x_var) {
  df %>%  
    group_by({{ group_vars }}) %>%
    summarize(
      n_miss = sum(is.na({{ x_var }})),
      .groups = "drop"
    )
}

nycflights13::flights %>% 
  count_missing(group_vars = c(year, month, day), 
                x_var = dep_time)

# To ale nejde, protože funkce používající data-masking očekávají jen
# jednu hodnotu vstupního argumentu, ne tři jako v příkladu nahoře

# Lze to obejít pomocí funkce pick(), která umožňuje tzv. tidy-selection 
# i ve funkcích používajících data-masking 

# tidy-selection  je označení zjednodušený způsob výběrů více proměnných
# z datasetu, který používají funkce jako across(), 
# relocate(), rename(), select(), and pull(),
# kdy stačí uvést např. jen c(názvy, sloupců, oddělené, čárkou)

#takže kód je úplně stejný, ale pomocí pick() tam může být víc sloupců

count_missing <- function(df, group_vars, x_var) {
  df %>% 
    group_by(pick({{ group_vars }})) %>% 
    summarize(
      n_miss = sum(is.na({{ x_var }})),
      .groups = "drop"
    )
}

nycflights13::flights %>% 
  count_missing(group_vars = c(year, month, day), 
                x_var = dep_time)

# Ukázka pokročilejší funkce
count_wide <- function(data, rows, cols) {
  data %>%  
    count(pick(c({{ rows }}, {{ cols }}))) %>%  
    pivot_wider(
      names_from = {{ cols }}, 
      values_from = n,
      names_sort = TRUE,
      values_fill = 0
    )
}
  #vybere řádky a spočítá počty
  #a pivotne

diamonds %>% 
  count_wide(rows = c(clarity, color),
             cols = cut)

# Po krocích to, co funkce výše dělá
diamonds %>% 
  count(pick(clarity, color, cut))

diamonds %>% 
  count(pick(clarity, color, cut)) %>% 
  pivot_wider(
    names_from = cut,
    values_from = n,
    names_sort = TRUE,
    values_fill = 0
  )

#TAKE HOME MESSAGE
#Pokud fce používají data masking
#musíme použít pomocnou funkci pick
#pokud chceme použít kwargs
#jinak to hodí chybu

# Funkce pro tvorbu grafů ---------------------------------------------

# Namísto přetvořeného dataframu také můžeme chtít generovat nějaký graf.
# Naštější můžeme použít předchozí znalosti, protože ggplot2 také používá
# data-masking

# Kdybychom např. chtěli napsat funkci generující histogram
# Jako vstupní argumenty bychom určitě použili dataframe a sloupec,
# a možná i binwidth

diamonds %>% 
  ggplot(aes(x = carat)) +
  geom_histogram(binwidth = 0.1)

diamonds %>% 
  ggplot(aes(x = carat)) +
  geom_histogram(binwidth = 0.05)
  
histogram <- function(df, var, binwidth = NULL) {
  df %>% 
    ggplot(aes(x = {{ var }})) + 
    geom_histogram(binwidth = binwidth)
}  #binwidth je tedy defaultně nespecifikovaný

diamonds %>% 
  histogram(carat, binwidth = 0.1)

# Protože funkce histogram vrací ggplot graf, můžeme libovolně přidávat další
# komponenty, např. popisky os
diamonds %>% 
  histogram(carat, 0.1) +
  labs(x = "Size (in carats)", y = "Number of diamonds")

linearity_check <- function(df, x, y) {
  df %>% 
    ggplot(aes(x = {{ x }}, y = {{ y }})) +
    geom_point() +
    geom_smooth(method = "loess", formula = y ~ x, color = "red",
                fill = "red", alpha = .2) +
    geom_smooth(method = "lm", formula = y ~ x, color = "blue", se = FALSE) 
}
  #3 argumenty: dataset, proměnná na ose X, proměnná na ose Y

starwars %>%  
  filter(mass < 1000) %>% 
  linearity_check(mass, height)

starwars
  #jojo, for real má ggplot builtin dataset Starwars

starwars %>% 
  colnames()

# Samozřejmě jde při tvorbě vlastní funkce využít funkce více balíčků 
# spadajících pod tidyverse

# Zde např. funkce fct_infreq() pro seřazení úrovní faktoru podle zastoupení a 
# fct_rev pro reverzi jejich pořadí, aby početnější úrovně byly v grafu 
# od shora
sorted_bars <- function(df, var) {
  df %>%  
    mutate({{ var }} := fct_rev(fct_infreq({{ var }})))  %>%
    ggplot(aes(y = {{ var }})) +
    geom_bar()
}   #fce používá walrus operátor, kde upravuje a přiřazuje zároveň

diamonds %>% 
  sorted_bars(clarity)

# Vraťme se ještě k funkci histogram()
histogram <- function(df, var, binwidth = NULL) {
  df %>% 
    ggplot(aes(x = {{ var }})) + 
    geom_histogram(binwidth = binwidth)
}

histogram(diamonds, var = carat, binwidth = 0.1)

# Bylo by pěkné, aby generavala graf i s vhodným nadpisem, např. 
# Histogram proměnné carat s šířkou sloupce = 0.1
# Ale carat jako vstupní argument není textový vektor obklopený ""
# Takže by to NEšlo jednoduše s pomocí 
str_c("Histogram proměnné", carat, "s šířkou sloupce =", 0.1)

# Jde to ale s využitím rlang::englue(), která rozumí {{}} i {}
# {{var}}  znamená vlož zde hodnotu z argumentu var,
# ovšem nechápej ji "doslova" jako existující objekt, 
# ale nejprve převeď na textový vektor
# {binwidth} znamená vlož zde hodnotu z argumentu binwidth

histogram <- function(df, var, binwidth) {
  label <- rlang::englue("A histogram of {{var}} with binwidth {binwidth}")
  
  df %>% 
    ggplot(aes(x = {{ var }})) + 
    geom_histogram(binwidth = binwidth) + 
    labs(title = label)
}

histogram(diamonds, var = carat, binwidth = 0.1)