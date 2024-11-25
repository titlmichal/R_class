
##%######################################################%##
#                                                          #
####                 Statistical Power                  ####
#                                                          #
##%######################################################%##


# Packages ----------------------------------------------------------------
library(WebPower)
library(paramtest)
library(tidyverse)

# WebPower -------------------------------------------------------------

# balíček WebPower poskytuje je takovou eRkovou obdobou GPower
# v tom smyslu, že umožňuje základní odhad statistické síly pro 
# základní statistické testy

#teda kolik participantů dle statistické síly bychom chtěli mít
#tedy abychom mohli zamítnou nulovou hypotézu = najít podporu
#pro výzkumnou hypotézu
#řešíme u toho 4 proměnné:
#síla testu, počet participantů, velikost účinku, hladinu alfa
#když máme 3 z nich, 4. lze dopočítat

#btw webpower má i internetové prostředí, kde si lze poweranalýzu udělat

# * Korelace ------------------------------------------------------------
# Nejdříve si vyzkoušíme odhad statistické síly pro korelace
# Dejme tomu, že zkoumáme vztah mezi stresem a well-beingem
# Na základě předchozích výzkumů očekáváme korelaci r = 0.3.
# Můžeme si položit otázku: kdybychom získali měřili stres a well-being
# u 50 osob, jakou statistickou sílu bychom měli při alfa = 0.05

# můžeme použít funkci wp.correlation
# Jeden z klíčových argumentů (power, n, r, alpha) musíme nechat prázdný,
# protože jsme neuvedli power, vypočte se právě ona
wp.correlation(n = 50, r = 0.3, alpha = .05)

wp.correlation(r = 0.3, alpha = .05, power = 0.58)
wp.correlation(r = 0.3, alpha = .05, power = 0.8)
#tady není n, ale zase už to je power


# Co když chceme zjistit power pro různé velikosti vzorku, různé r a různou alfa
# Vytvoříme si všechny kombinace relevantních hodnot
input <- expand.grid(
  n = seq(10, 100, by = 1), # velkost vzorku od 10 do 100
  r = seq(0.1, 0.6, by = 0.1), # velikost korelace od 0.1 po 0.6
  alpha = c(0.05, 0.01) # hladina alfa
) %>% 
  as_tibble()

input
  #takhle vypadá ta tabulka, resp tibble všech kombinací

# Iterativně mapujeme parametry
#fce pmap() je z balíčku purr (součást tidyverse)
#dělá to, že aplikuje funkci v závorce, na každý řádek
output <- input %>% 
  pmap(wp.correlation)

str(output[[1]])
  #jeden prvek je list
  #pracuje se s tím blbě, když to chceme dál použít
  #proto použití map_df()

# Převedeme si výstup do srozumitelnějšího formátu
output <- output %>% 
  map_df(~.x[c("n", "r", "alpha", "power")])
  #fce map_df() extrahuje prvky
  #a udělá z nich df, resp tibble

output

#když už máme ten df
# Vyvtoříme si graf s výsledky
#tzv powerplots
#osa x = n
#osa y = síla
#barvy dle faktorů = korelace, resp účinek
output %>% 
  ggplot(aes(x = n, 
             y = power, 
             color = factor(r))) +
  geom_line(linewidth = 1) +
  facet_wrap(~alpha)
  #kdyby velikost účinku byla silná (třeba 0,6)
  #tak nám stačí asi 30 lidí (na power 0,8)
  #ale zase když by byla nízká, tak ani 100 nestačí

# Nejmenší velikost vzorku pro alespoň 80% sílu
output %>% 
  filter(power >= .8) %>% 
  group_by(alpha, r) %>% 
  slice_min(n)
  #pro zjištění, jaký vzorek potřebujeme
  #na statistickou sílu aspoň 0,8
  #s co nejmenším vzorkem


# Taky samozřejmě můžeme chtít zjistit potřebnou velikost vzorku pro
# stanovenou velikost účinku, statistickou sílu a hladinu alfa
input <- expand.grid(
  r = seq(0.2, 0.6, by = 0.001),
  alpha = c(0.05, 0.01),
  power = c(.8, .9, .95)
) %>% 
  as_tibble()
  #to stejné jak výše, jen bez velikostí vzorku,
  #které budeme chtít zjistit

# Další postup kopíruje předchozí analýzu
output <- input %>% 
  pmap(wp.correlation)

str(output[[1]])

output <- output %>% 
  map_df(~.x[c("n", "r", "alpha", "power")])
  #zase z listu na df

# A nakonec si vytvoříme graf s výsledky
#tentokrát x = velikost účinku
#barevně jsou statistické síly
#na y = velikost vzorku
#a jeden graf pro alfa 0,01, druhý 0,05
output %>% 
  ggplot(aes(x = r,
             y = n,
             color = factor(power))) +
  geom_line(linewidth = 1) +
  facet_wrap(~alpha)

# * t-tests -----------------------------------------------------------------

### Párový t-test
# Dejme tomu, že chceme testovat efekt nějaké intervence; máme skupinu
# studentů a měříme je před a po intervenci (čili párová data)
# Očekáváme velikost účinku d = 0.3
# Kolik účastníků potřebujeme k dosažení statistické síly 0.8
wp.t(d = .3, power = 0.8, alpha = 0.05, type = "paired")
  #pro t-testy je fce wp.t
  #velikost účinku jako Cohenovo d
  #argumentem type určíme zda párový nebo nezávislý
  #zde zase chybí n --> fce vrací 90
  #je to ale počet PÁRŮ (!)

### Nezávislý t-test
# Dejme tomu, že místo vnitrosubjektového použijeme mezisubjektový desing,
# takže máme dvě nezávislé skupiny, jedna bude vystavena placebo-intervenci
# a druhá skutečné intervenci

# Očekáváme stejnou velikost účinku a chceme mít vyvážený počet osob
# v obou skupinách
wp.t(d = .3, power = 0.8, alpha = 0.05, type = "two.sample")
  #vrací 175 jako počet v KAŽDÉ skupině
  #tj. celkem 350 lidí


# Co kdyby ale poměr skupin nebyl vyvážený
# Např. kdybychom chtěli poskytnout skutečnou intervenci co nejvíce lidem,
# ale věděli bychom, že celková velikost vzorku, kterou můžeme získat, 
# je 500 osob. Stále bychom ale chtěli mít sílu testu aspoň 0.8
# Jak moc mohou být skupiny nevyvážené?
input <- tibble(
  n1 = 2:250,    #jedna skupina od 2 do 250 lidí
  n2 = 500 - n1, #druhá jako doplněk k první
  d = 0.3,
  alpha = 0.05,
  type = "two.sample.2n"  #tenhle argument je právě jiný kvůli nevyvážeností
)

output <- input %>% 
  pmap(wp.t)
  #zase pmap na input

str(output[[1]])

output <- output %>% 
  map_df(~.x[c("n1", "n2", "power")])
  #zde df místo listu

output %>% 
  ggplot(aes(n1, power)) +
  geom_line(linewidth = 1) +
  geom_hline(aes(yintercept = .8)) +
  labs(x = "n1 (n2 = 500 - n1)")
  #a zase graf
  #x je velikost první skupiny (druhá je doplněk do 500)
  #y je síla
  #plus ta horizontální čára na vykreslení 0,8 síly

output %>% 
  filter(power >= .8) %>% 
  slice_min(n1)
  #tady hledáme nejmenší n1, takže vlastně největší nevyváženost
  #jinak je vidět, že s vyvážeností skupin roste síla

# * ANOVA -------------------------------------------------------------------

### One-Way ANOVA
# Dejme tomu, že chceme srovnat čtyři skupiny a očekáváme velikost účinku
# R2 = 0.2
# Kdybychom mohli získat pouze pro každou skupinu pouze 10 účastníků
# (tj. dohromady 40 osob), jakou statistickou sílu by měl F-test ověřující
# shodu průměrů skupin.

#u power-analýzy se u anovy uvádí Cohenovo f
#víceméně jako vysvětlený rozptyl (od 0 do 1)
#~ jako poměr přírůstku vůči nevysvětlenému
#~ to Cohenovo f se moc nereportuje

# Nejdříve vypočteme Cohenovo f: 
# odmocninu z (přírůstku) vysvětleného rozptylu dělená podílem
# nevysvětleného rozptylu po zahrnutí daného efektu
#  sqrt((r2_full - r2_null) / (1 - r2_full))

# (r2_full - r2_null) je vlastně přírůstek vysvětleného rozptylu 
# ve srovnání s výchozím modelem
# a (1 - r2_full) je podíl (ještě) nevysvětleného rozptylu


#když víme, jaký je vysvětlený roztypl/přírůstek, tak se lze spočítat
#viz proto zde ta fce()
r2_to_f <- function(r2_full, r2_null = 0, squared = FALSE) {
  
  out <- (r2_full - r2_null) / (1 - r2_full)
  
  if (squared == TRUE) {
    out
  } else {
    sqrt(out)
  }

}
#~odmocnina z poměru přírůstku a zbývajícího nevysvětleného
#viz to out <- ...

cohen_f <- r2_to_f(.2)
cohen_f

wp.anova(k = 4, n = 40, alpha = .05, f = cohen_f)
#pro anovu tedy wp.anova

# Anebo požadovanou velikost vzorku pro danou velikost účinku, alfu a počet 
# skupin
wp.anova(f = cohen_f, k = 4, n = NULL, power = 0.8, alpha = 0.05)
#tady chybí vzorek, takže dopočítá jej

# Funkce wp.kanova je zobecněním pro více nezávislých proměnných (k-way ANOVA)
#tedy více nezávislých proměnných, případně i interakce
wp.kanova(n = NULL,  #chceme velikost vzorku --> n = NULL
          power = .8, # požadovaná statistická síla
          ndf = 3, # stupně volnosti pro daný efekt, pro hlavní efekty: počet skupin/úrovní - 1
          ng = 4, # celkový počet skupin (součin počtů úrovní jednotlivých faktorů)
          f = cohen_f, # velikost účinku
          alpha = .05) #klasika PST chyba 1. typu
        #stejné jako výše, protože stejné argumenty

# * Lineární regrese -------------------------------------------------------

#u regrese taky Cohenovo f, ale NEodmocněné, tedy umocněné

# Výzkumník se domnívá, že průměr známek ze střední školy a výsledek TSP
# vysvětluje 25 % rozptylu v průměru známek na vysoké škole.
# Pokud má vzorek 50 osob, jaká je statistická síla tohoto testu
# (při alpha = 0.01)
f2 <- r2_to_f(0.25, squared = TRUE)
f2

wp.regression(n = 50, 
              p1 = 2, # počet prediktorů
              f2 = f2, #Cohen f, ale viz výše, neodmocněné
              alpha = 0.01) #default je 0,05
      #vrací power asi 84 %

# Jiný výzkumník se domnívá, že hodnocení z přijímacího pohovoru
# vysvětluje dalších 5 % rozptylu.
# Jakou velikost vzorku potřebujeme získat pro dosažení 80% statistické 
# síly při alpha = 0.01
#řešíme tedy PŘÍRŮSTEK rozptylu třetím prediktorem
f2 <- r2_to_f(r2_full = 0.3, #model se všemi prediktory
              r2_null = 0.25, #model předchozí
              squared = TRUE)
f2


wp.regression(n = NULL, 
              power = .8, # požadovaná statistická síla
              p1 = 3, # Celkový počet prediktorů
              p2 = 2, # Počet prediktorů v původním modelu
              f2 = f2, # Velikost účinku
              alpha = 0.01) # Hladina alfa
    #n = 167

# Balíček paramtest -------------------------------------------------------
# Nejflexibilnější přístup pro power analýzu pomocí simulace,
# Ale zároveň nejsložitější pro naprogramování
#tu simulaci nám umožňuje právě balíček paramtest
#dá se použít i na jiné rozdělení parametrů
#chce to hlubší znalost Rka - zejména kvůli fci() ke generování dat

# Začněme jednoduše výpočtem power pro nezávislý t-test
# Samozřejmě bychom mohli použít přesné, analytické řešení bez simulace
# např. pomocí wp.t() funkce z WebPower
wp.t(n1 = 20, n2 = 40, d = 0.5, alpha = .05, type = "two.sample.2n")

# Ale tento přístup má omezení, protože např. předpokládá shodu rozptylů skupin

# * Nezávislý t-test a funkce run_test() ------------------------------------

# paramtest balíček nejprve vyžaduje napsat si vlastní funkci pro generování
# dat, provedení daného statistického testu a uložení výsledků

# První argument by měl být vžy simNum, ten slouží ke stanovení počtu simulací
# další argumenty pak upřesňují, jaká data budeme generovat
# a závisejí na tom, které funkce k tomu použijeme a jaký test chceme použít

# Obecně musí funkce mít následující strukturu
# 1. Kód sloužící ke generování dat

# 2. Kód sloužící k odhadu modelu/ uložení objektu (fit-objekt) s odhady

# 3. Kód sloužící k extrakci potřebných informací z fit-objektu
# obvykle se jedná o klíčové koeficienty společně s p-hodnotami apod.

# 4. Uložení výstupních hodnot v podobě atomického numerického vektoru
# jehož prvky musejí být pojmenovány


#TADY FCE ZAČÍNÁ
t_fun <- function(simNum, # argument sloužící ke stanovení počtu simulací
                  n1, n2, #velikosti obou skupin
                  m1, m2, #průměry/populační průměry
                  sd1, sd2, # argumenty sloužící pro simulaci vzorků z populace
                  var.equal = FALSE){ # argument používaný funkcí t.test

  x1 <- rnorm(n1, mean =  m1, sd = sd1) # simulace dat pro první skupinu
  x2 <- rnorm(n2, mean = m2, sd = sd2) # simulace dat pro druhou skupinu
    #rnorm je z Base R a slouží ke generaci dat
    #z normálního rozdělení (počet, průměr, sd)
    #uložení do objektů X1 a x2 pro další užití
  
  t <- t.test(x1, x2, 
              var.equal = var.equal)  # provedení t-testu
    #t.test() zase z Base R
    #uložení do objektu t, ze kterého pak extrahuje
  
  stat <- t$statistic[[1]] # uložení testové statistiky - extrakce z t
  p <- t$p.value # uložení p-hodnoty - uložení p value z t
  
  return(c(t = stat, p = p)) # 
  # výstupní hodnoty funkce t_fun: named vektor s 
  # testovou statistikou a p-hodnotu
}
#TADY FCE KONČÍ

# Pro provedení simulace samotné pak použijeme funkci run_test()
pwr_ttest <- run_test(
  t_fun, # použitá funkce
  n.iter = 2000, # počet simulací (kolikrát je funkce spuštěna, aspoň 2k doporučeno)
  output = "data.frame", # formát výstupu: list nebo data.frame - v čem chceme output
  n1 = 20, n2 = 40, m1 = 0, m2 = 0.5, sd1 = 0.5, sd2 = 1.5) 
    #co budeme dosazovat do fce() t_fun
    #(!) jména argumentů MUSÍ logicky sedět k fci()!

# nakonec argumenty doplněné do funkce t_func
# chceme simulovat data pro dvě skupiny o počtu 20 a 40 osob
# předpokládáme populační průměry 0 a 0.5
# a populační sd 0.5 a 1.5
# arugmenty var.equal a alpha mají defaultní hodnoty,
# můžeme je ale klidně změnit, kdybychom chtěli použít klasický
# t-test s rovností rozptylů nebo jinou hladinu alfa

# Výsledky extrahujeme pomocí funkce results()
# každý řádek je jedna simulace
output <- results(pwr_ttest) %>% 
  as_tibble()

# Odhad power pak vypočteme jednoduše jako podíl signifikantních testů
results(pwr_ttest) %>%
  summarise(power = mean(p <= 0.05))
  #původně tam bylo p <= 0.5
  #tedy to vracelo podíl p-hodnot pod 0.5, ne 0.05

# * Různé vstupní hodnoty a funkce grid_search() --------------------------

# V předchozím případě jsem použili funkci run_test(), protože jsme
# jsme použili jenom jedny vstupní hodnoty.
# Pomocí funkce grid_search jich ale můžeme použít několik 
#a tentokrát ty parametry předáme jako params = v listu
#a zase pojmenovat argumenty dle vstupní fce(!)

pwr_ttest <- grid_search(
  t_fun, # použitá funkce
  params = list(n1 = c(10, 20, 30), 
                n2 = c(40, 50, 60)), # různé velikosti vzorku
  n.iter = 2000, # počet simulací
  output= "data.frame", # výstupní formát 
  parallel = 'snow', # paralelní zpracování (využití více jader procesoru)
  ncpus = 6, # počet jader procesoru k využití
  m1 = 0, m2 = 0.5, # ostatní vstupní argumenty
  sd1 = 0.5, sd2 = 1.5) 

output <- pwr_ttest %>% 
  results() %>% 
  as_tibble()
  #zase si řekneme o výsledky fcí results()
  #a převod na tibble

output

output %>% 
  group_by(n1.test, n2.test) %>% 
  summarise(p_sig = mean(p <= .05))
  # zase síla jako poměr p pod 0.05
  #ani kombinace 30 a 60 osob pro skupiny nestačí na 70% sílu

output %>% 
  group_by(n1.test, n2.test) %>% 
  summarise(p_sig = mean(p <= .05)) %>% 
  ggplot(aes(x = factor(n1.test), #převedeme na faktor - použije diskrétní barvy
             y = p_sig,
             color = factor(n2.test))) +
  geom_point(size = 3) + #body v grafu
  geom_line(aes(group = factor(n2.test))) + #spojení čar
  coord_cartesian(ylim = c(0, 1)) #přeškálování osy y