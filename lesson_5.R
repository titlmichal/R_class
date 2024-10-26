library(tidyverse)

# Vektory ----------------------------
# vektor = datový objekt
# * Vlastnosti vektorů ------------------

# Existují dva základní typy vektorů:

# Atomické vektory: 
# Ty můžeme dále rozlišit na logické, celá čísla, reálná čísla a textové 
# (logical, integers, double, string)
# Vektory typu integer a double se dohromady nazývyjí numerické
# Integers jsou celá čísla, doubles mohou obsahovat desetinná místa
# vždycky mají jeden prvek, nelze je dále dělit --> proto atomické

# Kromě atomický vektorů pak máme vektory typu list

# Hlavní rozdíl mezi nimi je ten, te atomické vektory obsahují vždy prvky 
# stejného typu, zatímco pro listy to platit nemusí
# O atomických vektorech lze uvažovat jako o jednotlivých proměnných
# Dataframy, se kterými jsme pracovali, jsou vlastně také typu list,
# a skládají se z atomických vektorů --> jsou komplexnější


# Každý vektor má dvě základní vlastnosti
# A to typ, který můžeme zjistit pomocí funkce typeof()

letters
#vyjedou písmenka abecedy
typeof(letters)
#jaké jsou typy
length(letters)

1:10
#range 1-10
typeof(1:10)
length(1:10)

typeof(c(TRUE, FALSE, TRUE))

# A délku čili počet prvků. Tu můžeme zjistit pomocí funkce length()
x <- list("a", "b", 1:10)
length(x)
#výsledek je 3, protože obsahuje 2 charactery a jeden range

length(1:10)

# Vektory také mohou obsahovat další metadata ve formě atributů.
# Tyto atributy se používají pro tvorbu tzv. agumentovaných vektorů
# A mění jejich chování.
#řekneme si později

# Mezi hlavní agumentované vektory patří
# Faktory, které vycházejí z atomických vektorů typu integer
# Dataframes/tibbles, které vycházejí z vektorů typu list.
# A pak také datum a datum + čas, které vycházejí z numerických vektorů

# * Základní typy atomických vektorů --------------------------

### Logical (boolean)
# Logické vektory jsou nejjednodušším typem atomických vektorů
# protože mohou nabývat pouze tří hodnot: FALSE, TRUE, and NA
x <- c(TRUE, TRUE, FALSE, NA)
length(x)

### Numeric (číselné, numerické)
# Vektory typu double a integer dohromady označujeme jako numerické. 
# Defaultě R vytváří vektory typu double (!!!) 
# Abychom vyvořili vektory typu integer, musí přidat za číslo písmeno L:
typeof(1)
typeof(1L)

# Nebo použít tyto funkce
typeof(1:10)
typeof(seq(1, 10))
#čísla v sekvenci/rangy jsou automaticky ints

# Obvykle není potřeba dělat rozdíl mezi vektory typu integer a double,
# Ale vektory typu double jsou pouze aproximací
# Paměť počítače není neomezená, takže si pamatuje pouze určitý počet
# desetinných míst
# Proto někdy můžeme dostat nečekané výsledky
x <- sqrt(2) ^ 2
x
x - 2
#odmocnina z dvojky na druhou je 2
#mínus 2 je nula, ale výsledek není nula, ale tiny číslo

x == 2
#číslo tedy není 2

# Jen na okraj, dplyr obsahuje funkci near(), která umožňuje srovnat dva vektory
# typu double; neověřuje striktní shodu, počítá s nějakou tolerancí

near(x, 2)

### Character (string, text)
# Nejkomplexnějšími atomickými vektory jsou vektory typu character
# Protože mohou zahrnovat neomezený řetězec jakýchkoli znaků 
# včetně čísel a písmen

x <- "This is a reasonably long string."
x

#tedy 3 hlavní typy atomických vektorů: int/dbl, logical

# * Konverze mezi typy ------------------------------------
# Existují dvě cesty, jak jeden typ vektoru přeměnti v jiný:

# Explicitní konverze znamená, že použijeme speciální funkci k tomu určenou
# Tyto funkce začínají na as. a končí typem vektoru
# Např as.logical(), as.integer(), as.double(), or as.character()
as.integer(c(TRUE, FALSE, TRUE))
as.character(c(1, 2, 3, 4))
as.logical(c(0, 1, 0, 1))

as.integer(c(TRUE, FALSE, TRUE)) %>% typeof()
as.character(c(1, 2, 3, 4)) %>% typeof()
as.logical(c(0, 1, 0, 1)) %>% typeof()
#jen kontrola

# Implicitní konverze probíhá, když nějaká funkce očekává určitý typ vektoru
x <- sample(1:20, 100, replace = TRUE)  #sample 1:20, n = 100
x

# Logický vektor
y <- x > 10    #vektor, kde x > 10 --> vektor logicals
y

# Funkce sum() a mean() si jej implicitně převedly na 0/1
sum(y)
mean(y)

# Když pomocí funkce c() slučujeme hodnoty do jednoho vektoru
# Složitější typ má PŘEDNOST
# Protože "jednodušší" typy lze převést na složitější, ale naopak to
# neplatí
c(TRUE, 0L)
typeof(c(TRUE, 0L))

c(TRUE, 0L, 2.5)
typeof(c(TRUE, 0L, 2.5))

c(TRUE, 0L, 2.5, "one")
typeof(c(TRUE, 0L, 2.5, "one"))


# * Ověření typu vektoru ------------------------------------------------
# Někdy chceme dělat s vektory rozdílné operace v závislosti na jejich typu
# Takže nejdříve potřebujeme ověřit, jaký je tedy jejich typ
# Jak už jsme si říkali, můžeme použít funkci typeof()
typeof(c(TRUE, FALSE))
# Jejím outputem je textový vektor obsahující typ vektoru


# Další možností je použití tzv. testovacích funkcí
# které začínají na is_ a končí typem vektoru
# Na rozdíl od funkce typeof() je jejím výstupem LOGICKÝ VEKTOR
# TRUE nebo FALSE, podle toho, zda je daný vektor testovaného typu
is_logical(c(TRUE, FALSE))
is_logical(c(0, 1))

is_integer(c(1L, 2L))
is_integer(c(1, 2))

is_double(c(1.2, 2.4))
is_double(c(1L, 2L))

is_character(c("1", "2"))
is_character(c(1, 2))

is_atomic(c(1, 2))
is_atomic(list(1, c(1, 2)))

is_list(c(1, 2))
is_list(list(1, c(1, 2)))

is_vector(c(1, 2))
is_vector(NULL)

# * Recyklace prvků vektoru -----------------------------------------------
# Každé zřejmě uhádne, co se stane, když sečteme (vynásobíme, odečteme apod.) 
# vektory stejné délky
c(1, 2, 3) + c(4, 5, 6)  #1 + 4 = 5; 2 + 5 = 7; 3 + 6 = 9; == 5 7 9
c(1, 2, 3) * c(4, 5, 6)
c(1, 2, 3) / c(4, 5, 6)

# Co když ale vektory nemají stejnou délku?
# R implicitně "recykluje" prvky kratšího vektoru
# Takže např. tyto dvě operace jsou programově shodné.
c(1, 2, 3) + 10 # 1 + 10; 2 +10 ; 3+10 ... 11 12 13
c(1, 2, 3) + c(10, 10, 10)

#ALE MUSÍ JÍT O NÁSOBKY
c(1, 2, 3) + c(10, 10)
#tohle raisne varování
c(1,2,3,4) + c(5, 10)
#tohle gut

# Děje se tak i tehdy, když kratší vektor není skalár (skalár je jednoduše
# vektor obsahující je jeden prvek)

# Takže tyto operace jsou programově shodné
1:10 + 1:2
c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10) + c(1, 2, 1, 2, 1, 2, 1, 2, 1, 2)

# Tyto dvě operace jsou také shodné
c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10) + c(1, 2, 3, 1, 2, 3, 1, 2, 3, 1)
1:10 + 1:3 #varování, viz výše

# Všimněte si, že R vyhodí varování, pouze pokud délka delšího vektoru
# není dělitelná délkou kratšího vektoru

# * Pojmenování prvků vektoru ---------------
# Prvky vektorů všech typů lze pojmenovat.
# Můžeme to udělat už při jejich vytváření. Když se jedná o atomické vektory,
# používáme funkci c()
x <- c(x = 1, y = 2, z = 4)
x

# Jména prvků jsou vlastně formou metadat/atributů
# Můžeme je vyvolat pomocí funkce names()
names(x)

# Nebo pomocí funkce attributes()
attributes(x)

# Také můžeme jména prvků doplnit dodatečně
x <- c(1, 2, 4)
x

# Pomocí funkce names()
names(x) <-  c("x", "y", "z")
x

# Nebo pomocí funkce set_names()
x <- c(1, 2, 4)
x <- x %>% 
  set_names(c("x", "y", "z"))
x

# * Výběr prvků (subsetting) ---------------------------
# Ten můžeme provést třemi různými způsoby, ale vždy pomocí jiného vektoru
# a s využitím hranatých závorek

# ZAPRVÉ, pomocí jiného vektoru obsahujícího celá čísla.
# Ta musejí být buď všechna záporná, nebo kladná.
x <- c("one", "two", "three", "four", "five")
x[c(3, 2, 5)]

# Opakováním čísel můžeme vytvoř nový vektor, který je delší než vektor původní
x[c(1, 1, 1, 5, 5, 5, 2)]

# Když použijeme záporná celá čísla, vyřadíme tím dané prvky
x[c(-1, -3, -5)]
x[-c(1, 3, 5)]

# Při tomto typu subsettingu nelze kombinovat kladná a záporná čísla
x[c(1, -1)]

# ZADRUHÉ, pomocí logického vektoru (budou ponechány všechny TRUE prvky). 
x <- c(10, 3, NA, 5, 8, 1, NA)
x

x[c(TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE)]
#vyberou se pravdivé: 1., 2. a předposlední
x[c(TRUE, FALSE)]
#tady to jde dle  - vybere, ommitne, vybere, ommitne, ...
x[c(T, F, T, F)]
#nemusím psát celé True/False, stačí T/F

# Všechny non-missing hodnoty
is.na(x)
#vrátí TRUE pro 3. a poslední prvek, které jsou missing
x[!is.na(x)]
#vyber vše, kde is missing je FALSE

# Všechny hodnoty větší než pět (ale missing values jsou ponechány)
x[x > 5]

# Všechny sudé prvky (missing values jsou opět ponechány)
x[x %% 2 == 0]

# ZATŘETÍ, pokud jsou prvky vektoru pojmenovány, můžeme k subsettingu použít 
# jiný, textový vektor
x <- c(abc = 1, def = 2, xyz = 5)
x[c("xyz", "def")]
x[c("xyz", "xyz", "xyz")]

# Pro subsetting můžeme také místo jednoduchých hranatých závorek [] použít
# dvojité hranaté závorky [[]]
# Rozdíl je v tom, že při použití dvojitých hranatých závorek vždy
# jde vybrat jen jeden prvek a zahodí se jeho jméno.
x <- c(abc = 1, def = 2, xyz = 5)
x["def"]

x[["def"]]
x[[c("xyz", "def")]] 

# * Faktory -------------------------------------
# Zaměříme se pouze na jeden typ agmentovaných vektorů, a to faktory
# Ty se používají k reprezentaci KATEGORICKÝCH proměnných, které mohou
# nabývat pouze omezeného počtu hodnot
# Základem vektorů typu faktor jsou vektory typu integer, ale mají navíc
# atribute levels specifikující úrovně
x <- factor(
  c("a1", "a1", "b2", "c3"), 
  levels = c("a1", "b2", "c3")
)

x
class(x)
typeof(x)

# X je vektory typu integer
typeof(x)

# A má atribut levels
attributes(x)
levels(x)

# A také třídu factor
class(x)

# Třída je také atribut a eRku říká, že s ním má zacházet speciálním způsobem
# Např. že výpočet průměru by nedával smysl
mean(x)

# Pro příklad si vytvoříme vektor s pocitovými teplotami
temps <- c("cold", "hot", "hot", "warm", "cold", "cold", "warm")

# Defaultně funkce factor() řadí úrovně podle abecedy
factor(temps)
#defaultně vymaže duplikáty a seřadí dle abecedy

# Argumentem levels ale můžeme pořadí úrovní libovolně změnit
temps_fct <- factor(temps, levels = c("cold", "warm", "hot"))
temps_fct

# Často jsou jednotlivé kategorie nejprve kódovány numericky
temps <- c(1, 3, 3, 2, 1, 1, 2)
temps

# A názvy úrovní přidělíme až argumentem labels
temps_fct <- factor(temps,
                    levels = 1:3,
                    labels = c("cold", "warm", "hot"))
temps_fct

#ty úrovně si můžu setnou jak chci
temps_fct <- factor(temps,
                    levels = c(2, 1, 3),
                    labels = c("warm", "cold", "hot"))
temps_fct


# * Listy ---------------------------------------------
# Listy mohou být komplexnější než atomické vektory, protože mohou zahrnovat
# více atomických vektorů nebo jiné listy a také mít hierarchickou strukturu
x <- list(1, 
          c(2, 3), 
          c("A", "B", "C"))
#list a jaké prvky má list obsahovat
#zde je to číselný vektor, pak číselný o 2 hodnotách a pak textových o 3
x

# Velmi užitečnou funkcí při práci s listy je funkce str()
# pomocí které dostaneme přehled o struktuře listu
str(x)

x[1]
x[3]
x[1:3]

# Prvky listu si klidně můžeme pojmenovat, tak jako jsme to mohli udělat
# u atomických vektorů
x_named <- list(a = 1, 
                b = 2, 
                c = 3)
str(x_named)
x_named['b']
x_named[c('a', 'c')]
x_named['b'] %>% typeof()
#jen vyberu prvek a ponechám kvalitu hierarchie
x_named[['b']] %>% typeof()
#vyříznu, takže ztratí kvalitu

# Na rozdíl od atomických vektorů mohou listy obsahovat mnoho různých
# objektů různého typu
y <- list("a", 1L, 1.5, TRUE)
str(y)

# A také obsahovat jiné listy
z <- list(list(1, 2), list(3, 4))
str(z)

# ** Subsetting listu ------------------------------
# Stejně jako u atomických vektorů lze listy subsetovat
# Nejprve si vytvoříme list my_list s různými prvky

my_list <- list(
  a = 1:3, 
  b = "a string", 
  c = pi, 
  d = list(-1, -5)
) 

str(my_list)

# Pomocí [] můžeme vybrat prvky podle pořadí
# Je to, jako bychom z listu vyřadili jiné než uvedené prvky
# A výsledek operace je pořád typu list
str(my_list[1:2])
str(my_list[4])

# Pomocí [[]] můžeme vyříznout jeden určitý prvky listu
str(my_list[[1]])
my_list[[1]]
#fcí structure po něm chci i metadata, ale jinak je to stejné

# Protože první prvek listu a je atomický vektor,
# je výsledkem operace jeden atomický vektor


my_list[1] %>% typeof() #list
my_list[[1]] %>% typeof() #int

# Pokud mají prvky listu jména, můžeme k subsetingu jednoho prvku 
# z listu použít operátor $ pro kratší zápis
# Tyto operace jsou tedy stejné:
my_list$a
my_list[["a"]]
#výsledkem obou je 1 2 3

my_list$a %>% typeof()
my_list[["a"]] %>% typeof()
#oba integers, protože vyřezávám


# Dataframes -----------------------------------------------
# Data Frame je datová matice tak, jak ji chápeme při analýze dat
# Sloupce datové matice představují jednotlivé proměnné a řádky jednotlivá 
# pozorování
#jsou to taky takové listy, ale prvky MUSÍ BÝT STEJNĚ DLOUHÉ
#a prvky jsou sloupce


# Tvorba dataframe
df <- data.frame(
  name = c("Mercury", "Venus", "Earth", "Mars", 
           "Jupiter", "Saturn", "Uranus", "Neptune"),
  type = c("Terrestrial", "Terrestrial", "Terrestrial","Terrestrial", 
           "Gas giant", "Gas giant", "Gas giant", "Gas giant"),
  diameter = c(0.382, 0.949, 1, 0.532, 11.209, 9.449, 4.007, 3.883),
  rotation = c(58.64, -243.02, 1, 1.03, 0.41, 0.43, -0.72, 0.67),
  rings = c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE)
  #,row.names =  c("Mercury", "Venus", "Earth", "Mars", 
  #               "Jupiter", "Saturn", "Uranus", "Neptune")
)
df
#můžeme vytisknout jako cokoliv jiného
#jinak row.names se moc nepoužívá, protože to stačí mít jako jiný vzorce

length(df)
#5 protože je tam 5 sloupců

dimnames(df)

# Dataframes jsou také typu list
is_list(df)
#TRUE

# Ale mají omezení: jednotlivé prkvy (sloupce) jsou většinou atomické vektory
# a tyto atomické vektory jsou stejné délky

# V base R by subsetting probíhal takto:

# Pomocí numerických indexů bychom takto například vybrali první ŘÁDEK 
# a druhý SLOUPEC
df[1, 2]
#na levo řádky, na pravo sloupce
df[c(1, 2), c(2, 3)]

#nahoře výběr pomocí base R, níže pomocí dplyr
df %>% 
  select(2, 3)
  #vyber 2. a 3. sloupec

# Pomocí jmen řádků a sloupců
df[c("Earth", "Mars"), 
   c("name" , "diameter")]

df[, c("name", "diameter")]

# Pomocí logických vektorů
df$diameter > 1

df[df$diameter > 1, ]
#tohle je base R
#níže dplyr
df %>% 
  filter(diameter > 1)
#může se zdát, že dplyr je přívětivější
#stačí se odkázat jednou
#přehlednější skript a tak

df[df$diameter > 1,
   ]

df[df$diameter > 1, 
   colnames(df) %in% c("name", "type")]
#btw jde o relativní průměr (1 má země)

df %>% 
  filter(diameter > 1) %>% 
  select(name, type)
#tohle je zase dplyr

# Pomocí funkce subset
subset(df,
       subset = diameter > 1, #vybíráme řádky
       select = name)         #vybíráme sloupce
#tohle je base R, která je možná blíž tomu dplyru

# Tibble je uživatelsky přívětivější dataframe, nezahlcující konzoli
as_tibble(df)
df

df <- tibble(
  name = c("Mercury", "Venus", "Earth", "Mars", 
           "Jupiter", "Saturn", "Uranus", "Neptune"),
  type = c("Terrestrial", "Terrestrial", "Terrestrial","Terrestrial", 
           "Gas giant", "Gas giant", "Gas giant", "Gas giant"),
  diameter = c(0.382, 0.949, 1, 0.532, 11.209, 9.449, 4.007, 3.883),
  rotation = c(58.64, -243.02, 1, 1.03, 0.41, 0.43, -0.72, 0.67),
  rings = c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE)
)

df
#přívětivější v tom, že říká i typy sloupců automaticky
#navíc nezahltí konzoli, když je o hodně velký dataset