#   ____________________________________________________________________________
#   Uvedení do R                                                            ####

1
# * Klávesové zkratky ----------------------------
# Psaní speciálních znaků na české klávesnici skrze pravý alt:
# RAlt + x = #

# RAlt + F = []

# RAlt + B = {}

# RAlt + , = <

# RAlt + .= >

# RAlt + ů = $

# RAlt + C = &

# RAlt + š, poté jakýkoli znak = ^

# RAlt + ý, poté jakýkoli znak = `` (zpětný apostrof)

### Modifikovatelné klávesové zkratky RStudia
# Alt + Shift + K   pro seznam klávesových zkratek RStudia
# Alt + -           vloží assigment operátor <- pro tvorbu objektů
# Ctrl + Enter      pro spuštění aktuálního kódu na označeném řádku
# Ctrl + Alt + P    znovu spustí kód, který byl spuštěn naposledy

# Ctrl + Alt + R    spustí všechen kód
# Alt + L           zabalí aktuální blok kód
# Alt + Shift + L   rozbalí aktuální blok kód
# Alt + O           zabalí všechen kód
# Alt + Shift + 0   rozbalí všechen kód
# Ctrl + Shift + R  vloží nový nadpis
# Ctrl + Shift + M  vloží pipe operátor %>% 

# * Základní operace -------------------------------------------
# Matematické operace
# Sčítání
5 + 5 

# Odčítání
5 - 5 

# Násobení
3 * 5

# Dělení
(5 + 5) / 2 

# Umocňování
2^5

# Odmocňování
sqrt(9)
9^(1/2)

# Celočíselné dělení
16 %/% 6

# Zbytek po celočíselném dělení
16 %% 6

# Tvorba objektů
# K tvorbě nových objektů se používá assignment operátor <-:
# název_objektu <- jeho_hodnoty
x <- 3 * 4

# sloučit více hodnot dohromady do jednoho objektu (vektoru) lze pomocí
# funkce c()...jakože combine
# vektor je v podstatě jiné slovo pro objekt
primes <-  c(2, 3, 5, 7, 11, 13)
primes

# na vektor lze uplatnit různé operace, např. matematické
primes * 2 # každý prvek je vynásoben číslem dva
primes - 1 # od každého prvku je odečtena 1

# * Komentáře -------------------------------------------------------------
# Komentáře vkládáme pomocí hashtagu #
# Cokoli napravo od hashtagu tedy nebude R považovat za spustitelný kód
# Speciálním typem komentáře je nadpis, který můžeme vložit pomocí 
# CTRL + Shift + R nebo doplněním ---

# vytvoř vektor prvočísel
primes <- c(2, 3, 5, 7, 11, 13)

# vynásob prvočísla dvěma
primes * 2
not_primes = primes * 2


# * Test sekce --------------------------------------------------------------
#pomocí ctrl + shift + r
#nebo stačí pomoctí # * nazev_sekce ---
#4 a více pomlček

# * Pojmenování objektů -----------------------------------------------------
# Názvy objektů musejí začínat písmenem a smějí obsahovat pouze
# písmena, čísla, podtržítka (_) a tečky (.)

# doporučuji používat tzv. snake case pro tvorbu delších jmen objektů
i_use_snake_case
otherPeopleUseCamelCase
some.people.dot.case
And_aFew.People_RENOUNCEconvention

# vyvolat objekt do konzole lze jednoduše napsáním jeho jména
x
r_rocks <- 2^3
r_rocks

for (i in 1:10) {
  print(i)
}

# jméno objektu musí být napsáno přesně, jinak vám to objekt nenajde
r_rock # zde chybí na konci "s"
R_rocks # R rozlišuje malá a velká písmena

# * Používání funkcí ---------------------------------------------------------
# R obsahuje velké množství funkcí
# K jejich vyvolání stačí znát název funkce a její argumenty
# function_name(argument1 = value1, argument2 = value2, ...)

# nápovědu k funkci seq() získáme takto
help(seq)
?seq

# zkuste pomocí funkce seq() vygenerovat řadu celých čísel od 1 do 10
seq(1, 10)
1:10
for (i in seq(1, 10)) {
  print(i)
}
seq(1, 10, 2)
seq(10, 1)
seq(1, 10, 0.5)

#   ____________________________________________________________________________
#   Základy vizualizace dat                                                 ####
install.packages("palmerpenguins")
install.packages("ggthemes")

library(tidyverse)
library(palmerpenguins)
#obsahuje dataset penguins
library(ggthemes)

penguins
print(penguins)
#factor = kategorické proměnné
#double = float
#int = int
glimpse(penguins)
#glimpse() vypíše sloupce pro případ, že bychom ji měli mnoho

# Našim cílem bude vytvořit scatterplot ukazující vztah mezi 
# délkou ploutví (na ose X) a tělesnou hmotností (na ose Y) tučňáků
# Body grafu bychom pak měli rozlišit podle druhu tučňáka
# a to jak pomocí barvy, tak pomocí tvaru bodů
# a také znázornit lineární trend pro všechna data 
#graf budeme dělat pomocí balíčku ggplot()

# Prvním krokem je volba datasetu pomocí argumentu data
ggplot()
#vytvoří blank plot
ggplot(penguins)
ggplot(data = penguins)
#tohle taky vytvoří blank plot, jen načteme data

# Poté musíme určit, jaké proměnné z datasetu chceme mít na ose X a Y
# a to pomocí argumentu mapping a funkce aes()
ggplot(penguins, aes(flipper_length_mm, body_mass_g))
ggplot(data = penguins, 
       mapping = aes(x = flipper_length_mm, 
                     y = body_mass_g))
#tady už mám osy, ale zatím bez bodů


# Dále musíme určit jakým typem graf (geomu) chceme použít
# neboli jaký geometrický objekt má vyjádřit jednotlivá pozorování
# K tomu slouží různé funkce začínající slovem geom_
ggplot(penguins, aes(flipper_length_mm, body_mass_g)) + geom_point()
#POZOR...geom_point() se dává ke/do ggplot pomocí pluska
#jinak fce je geom_nazevTypuGrafu

# všimněte si chybového hlášení --> vyřazeny missing values

# Nyní chceme doplnit informace o druhu tučnáka a vyjádřit jej barvou bodů
# Je např. možné, že se vztah mezi délkou ploutví a tělesnou hmotností liší
# právě podle druhu tučnáka
# Takže proměnnou species "mapujeme" na barvu
ggplot(data = penguins, 
       mapping = aes(x = flipper_length_mm, 
                     y = body_mass_g,
                     color = species)) +
geom_point()
#x a y mapujeme na proměnné...takže barvu analogicky


# Regresní křivku pak doplníme jako nový geom pomocí funkce geom_smooth
ggplot(data = penguins, 
       mapping = aes(x = flipper_length_mm, 
                     y = body_mass_g,
                     color = species)) +
  geom_point() +
  geom_smooth()


# Defaultně ggplot použije neparametrickou regresi (loess křivku)
# NEpředpokládající lineární vztah... i když vztahy jsou docela lineární
# to šedé jsou CIs
# My ale chceme použít lineární model
ggplot(data = penguins, 
       mapping = aes(x = flipper_length_mm, 
                     y = body_mass_g,
                     color = species)) +
  geom_point() +
  geom_smooth(method = "lm") #nemusí být v ""


# Vykreslilo nám to regresní přímky pro jednotlivé druhy zvlášť,
# ale my chceme jednu přímku pro celý soubor dohromady
# Musíme proto mapování species na barvu provést jen v rámci geom_point
#...protože to mapování je "děděno" do dalších aspektů skriptu
#...takže pokud chceme specifikovat mapování jen pro daný graf/aspekt...
#...tak to mapujeme v samotném geomu, NE ggplotu
ggplot(data = penguins, 
       mapping = aes(x = flipper_length_mm, 
                     y = body_mass_g)) +
  geom_point(mapping = aes(color = species)) +
  geom_smooth(method = "lm")


# Protože někdo může být barvoslepý, můžeme druh vyjádřit nejen pomocí barvy 
# bodů, ale také pomocí jejich tvaru (v RÁMCI mapování)
ggplot(data = penguins, 
       mapping = aes(x = flipper_length_mm, 
                     y = body_mass_g)) +
  geom_point(mapping = aes(color = species,
                           shape = species)) +
  geom_smooth(method = "lm")


# Aby byly tvary bodů lépe rozlišitelné, zvětšíme jejich velikost pomocí
# argumentu size (MIMO mapování)
ggplot(data = penguins, 
       mapping = aes(x = flipper_length_mm, 
                     y = body_mass_g)) +
  geom_point(mapping = aes(color = species,
                           shape = species),
             size = 2.5) +
  geom_smooth(method = "lm")


# Můžeme také vybrat jinou paletu barev, která bude více color-blind-friendly
# pomocí funkce scale_color_colorblind() z balíčku ggthemes
ggplot(data = penguins, 
       mapping = aes(x = flipper_length_mm, 
                     y = body_mass_g)) +
  geom_point(mapping = aes(color = species,
                           shape = species),
             size = 2.5) +
  geom_smooth(method = "lm") +
  scale_color_colorblind() #fce vybere barvy dobře rozlišitelné


# A nakonec doplníme ke grafu různé popisky pomocí funkce labs()
?labs
ggplot(data = penguins, 
       mapping = aes(x = flipper_length_mm, 
                     y = body_mass_g)) +
  geom_point(mapping = aes(color = species,
                           shape = species),
             size = 2.5) +
  geom_smooth(method = "lm") +
  scale_color_colorblind() + 
  labs(title = "Hmotnost a délka ploutví tučňáků",
       x = "Délka ploutví v mm",
       y = "Hmotnost v g",
       color = "Druh",
       shape = "Druh")
#takže moje chápání ggplotu a těch aspektů je, že ty aspekty jsou...
#...de facto argumenty, ale sypuju je tam, jak to přijde, ...
#...tak kinda proto ty pluska

?geom_point
#btw název balíčku, odkud je fce, je vedle názvu v {}
?ggplot
?aes

typeof("d")
typeof("1")
typeof(1)
dice <- c(1,2,3,4,5,6)
typeof(dice)
six <- 6
typeof(six)
six_point_five <- 6.5
typeof(six_point_five)

integs <- c(-1L, 2L, 4L)
integs
typeof(integs)

sqrt(2)^2 - 2
# floating-point error - mělo by to vrátit nulu, ale nevrátí

logics <- c(TRUE, FALSE, FALSE)
typeof(logics)
3 > 4

complx <- c(1 + 1i, 1 + 2i, 1 + 3i)
complx
typeof(complx)
#asi je nepoužiju v analytics tbh

raw(3)
typeof(raw(3))
#ukládá raw bytes

hand <- c("ace", "king", "queen", "jack", "ten")
hand
typeof(hand)

attributes(dice)
#něco, co lze dát objektům/atomickým vektorům

#např. jméno
names(dice)
names(dice) <- c("one", "two", "three", "four", "five", "six")
names(dice)
attributes(dice)
attributes(dice + 1)
dice
dice + 1
names(dice) <- c("uno", "dos", "tres", "quatro", "cinco", "seis")
dice
names(dice) <- NULL
dice
#DOPLNIT POKRAČOVÁNÍ Z DOKUMENTACE
#https://rstudio-education.github.io/hopr/r-objects.html#dim

