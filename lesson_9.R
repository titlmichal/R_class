# Načtení balíčků a dat --------------------------------------------------

library(betaDelta) #balíček na odhad regresí vč. CIs
library(car)       #hlavně pro diagnostiku
library(broom)     #maskování objektů
library(tidyverse)

theme_set(
  theme_classic(base_size = 15)
)
#klasika nastavení vizuálů


df <- read_rds("https://is.muni.cz/go/ccaz4r")
head(df)
glimpse(df)
#dat z cest do dospělosti

# Odhad regresních modelů ---------------------------------------------
# Budeme predikovat depresivitu na základě pohlaví, věku, průměru známek,
# důvěry ve vrstevníky/rodiče a optimismu a sebehodnocení (Self-esteem)


# Výběr proměnných a vyčištění dat
df_clean <- df %>% 
  select(id, skola, trida, deprese, pohlavi, vekr, 
         gpa, duv_v, duv_r, selfe, optim) %>% 
  drop_na()
#list-wise analýza, ale teď tolik nebudeme řešit NAs -> prostě je vymažeme

# Odhad prvního modelu pomocí funkce lm() jako "linear model"
fit_1 <- lm(deprese ~ pohlavi + vekr, 
            data = df_clean)
    #deprese predikovaná pohlavím a věkem
    #...prostě je to jako rovnice: deprese = pohlavi + vekr
    #pohlavi je factor --> dummy

fit_1 # pouze rovnice a koeficienty
summary(fit_1) # Celkový přehled modelu
  #přehled, rezidua, kvartily, odhady, chyby, fity modelu (R-sqrd)
  #s množstvím prediktorů poroste vysvětlený rozptyl
  #intercept se moc neinterpretuje, protože je to hodnota s prediktory 0
  #a často některé prediktory nemohou být nulové (např. věk)
  #ale lze to řešit centrováním (věk - průměr věku) --> interpretace interceptu
  #pohlavi zenske bylo signifikatní - o cca 0,14 bodu vyšší deprese u žen
  #efekt věku ale nebyl významný (a malý)
  #R-sqrd 0,021 --> 3 % vysvětleného rozptylu

# Odhad druhého modelu
fit_2 <- lm(deprese ~ pohlavi + vekr + gpa + duv_v + duv_r + selfe + optim, 
             data = df_clean)
          #stejná logika jako výše, jen více prediktorů
summary(fit_2)
  #efekt pohlaví stále významný
  #přidané signifikantní: gpa, self, optim, duv_v
  #až 34% vysvětleného rozptylu
  #F-test má H0 = vysvětlený rozptyl je nulový
  #zde dost malé --> zamítáme H0

# Přírůstek R2
summary(fit_1)$r.squared
summary(fit_2)$r.squared
summary(fit_2)$r.squared - summary(fit_1)$r.squared
  #takže vyšší o až 32% (!)

# Srovnání obou modelů pomocí funkce anova()...zda je významný rozdíl
# nulovou hypotézou je nulový přírůstek vysvětleného rozptylu
anova(fit_1, fit_2)

# Standardizované regresní koeficienty
betaDelta::BetaDelta(fit_2, alpha = .05)
  #o tom, když vzroste o bod prediktor, o kolik vzroste závislá proměnná
  #vč. chyby, t-statistik, df, p-values a CIs

# Pro základní grafickou diagnostiku modelu stačí na fit-objekt použít funkci
# plot()
plot(fit_2)
  #vrací X grafů vlastně - viz níže

plot(fit_2, which = 1, id.n = 10) # Residuals vs. fitted
plot(fit_2, which = 2, id.n = 10) # QQ-plot of residuals
plot(fit_2, which = 3, id.n = 10) # Absolute residuals vs. fitted
plot(fit_2, which = 4, id.n = 10) # Cook's distances
plot(fit_2, which = 5, id.n = 10) # Residuals vs. leverage
plot(fit_2, which = 6, id.n = 10) # Cook's distances vs leverage
  #leverage je potenciál ovlivnit ten model
  #vyšší leverage mají ty případy, které mají vysoké prediktory
  #jsou to vlastně pak případy, které predikuje model špatně
  #mají vysoké reziduum (i to leverage interpretovat relativně)

# Uložení reziduí
residuals(fit_2) %>% 
  head()
  #případně zkráceně resids()

res <- tibble(resid = residuals(fit_2),
              std_resid = as.double(scale(resid)))
              #rezidua a jejich převod na z-skory
res 

# Histogram reziduí
res %>% 
  ggplot() +
  geom_histogram(aes(x = std_resid,
                     y = after_stat(density)),
                 color = "black", fill = "grey") +
  stat_function(fun = dnorm,
                color = "red",
                linewidth = 1) +
  coord_cartesian(xlim = c(-3.5, 3.5))
  #nejsou moc normálně rozdělená
  #ale zase ne asi tolik, aby to byl takový problém

# Q-Q graf reziduí
res %>% 
  ggplot(aes(sample = std_resid)) +
  geom_qq_line(color = "red",
               linewidth = 1) +
  geom_qq(size = 3,
          alpha = .2)


# Funkce balíčku car -----------------------------------------
#obsahuje víc grafů

# Residual plots (vs predictor values)
car::residualPlots(fit_2)
  #rezidua modelu vůči hodnotám prediktorů
  #nechtěli bychom v grafech výraznější trend
  #znamenalo by to totiž, že vztah prediktor a proměnné
  #není lineární --> chceme chumel

# Component+residual plots
car::crPlots(fit_2)
  #podobně zde, rezidua bez zahrnutí prediktoru na ose x
  #modrou čarou lineární trend
  #modrou to, co tam je
  #chceme to cca podobné

# Added-variable, also called partial-regression plots
car::avPlots(fit_2)
  #k identifikaci outlierů
  #čísla odpovídaj řádkům v datech
df_clean %>% 
  slice(250)
  #tím se přímo podíváme na onen případ 250

# variance inflation factor
car::vif(fit_2)
  #multikolinearita
  #zde to asi nebude takový problém

# Durbin-watson test (autokorelace reziduí)
car::durbinWatsonTest(fit_2)
  #zde se blíží nule
  #nezdá se, že by závislost reziduí měl být problém
  #i když jsem vybírali žáky po třídách
  #--> asi depresivita není závislá na třídním kontextu
  #oproti třeba známkám

# Funkce balíčku broom ----------------------------------------

# balíček broom nabízí funkce tidy() a glance() pro zobrazení modelu
# v jedné tibble
# tidy() extrahuje informace o koeficientech modelu
# glance() o celkovém fitu modelu
#a augment()
broom::tidy(fit_2, conf.int = TRUE, conf.level = 0.95)
  #získáme tím data z odhadu v hezčí tabulce
  #odhad, chyba, stastitiky, p hodnoty a CIs
  #typicky na většinu modelů z basic R, třeba na t-testy

broom::glance(fit_1)  #na informace o fittech
broom::glance(fit_2)

bind_rows(
  m1 = broom::glance(fit_1),
  m2 = broom::glance(fit_2),
  .id = "model"
)
    #pomocí bind_rows() sváže řádky
    #nejlepší je, když mají datové matice stejné sloupce

df_aug <- broom::augment(fit_2, data = df_clean)
head(df_aug)
  #fce augment()
  #doplní informace z odhadů do původního datasetu


# Výběr pozorování s nejvyšší Cookovou vzdáleností
df_aug %>% 
  slice_max(order_by = .cooksd, n = 10) %>% 
  print(width = Inf)
  #pak si můžeme vzít ty věci z predikcí
  #a třeba jako zde koukat na ty hodně vlivné případy

# Výběr pozorování s nejvyšším reziduem
df_aug %>% 
  slice_max(order_by = abs(.resid), n = 10) %>% 
  print(width = Inf)

#pak se můžeme dívat na divné případy, co tam dělají nepořádek
#třeba účastníci odpovídající jen extrémně
#nebo velmi rychle a tak ... tohle na to ukáže

# Intervaly spolehlivosti a predikční intervaly ----------------------------

#predikce pro model a jejich zobrazení
#prvně si musíme nastavit hodnoty prediktory --> fce expand.grid()
#použijeme ji k tvorbě nových dat, když chceme zjistit predikovanou hodnotu

# Hodnoty prediktorů, pro které chceme zjistit predikované hodnoty
new_data <- expand.grid(
  pohlavi = c("muzske", "zenske"), #info pro muže a ženy zvlášť
  vekr = 13,
  gpa = 2,
  duv_v = 2.7, 
  duv_r = 2.8,
  selfe = seq(1, 4, length.out = 1000), #daty si generujeme 1000 hodnot 1-4
  optim = 17
) %>% as_tibble()
  #výsledkem je pak tibble nových dat pro muže i ženy a různé combos
  #to se pak hodí na vizualizace predikcí!

# Intervaly spolehlivosti
ci <- broom::augment(fit_2,
                     newdata = new_data,
                     interval = "confidence")
    #doplním predikci dat k novým řádkům
ci <- ci %>% 
  rename(ci_lower = .lower,
         ci_upper = .upper)
  
ci

# Graficky
ci %>% 
  ggplot(aes(x = selfe,
             y = .fitted)) +      #zde jen predikované hodnoty
  geom_ribbon(aes(ymin = ci_lower,   #geom_ribbon vytváří oblast
                  ymax = ci_upper),  #potřebuje proto argumenty rozpětí
              fill = "blue",
              alpha = .3) +
  geom_line(color = "blue",
            linewidth = 1) +       #zobrazení predikce
  facet_wrap(~pohlavi, nrow = 2) +  #rozdělení dle pohlaví
  coord_cartesian(ylim = c(1, 4)) + #nastavení hranic vizuálu
  labs(x = "Self-esteem",
       y = "Predikovaná depresivita") #nastavení popisků

# Predikční intervaly
#intervaly pro samotné odhady
pi <- broom::augment(fit_2,
                     newdata = new_data,
                     interval = "prediction") #říkáme jestli chceme interval
                                              #resp jaký
pi <- pi %>% 
  rename(pi_lower = .lower,
         pi_upper = .upper)
  #tady jen rename pro čistotu

cpi <- ci %>% 
  left_join(pi)
cpi

cpi %>% 
  ggplot(aes(selfe, .fitted)) +
  geom_ribbon(aes(ymin = pi_lower,
                  ymax = pi_upper),
              fill = "pink",          #tady je to stejné jak výše
              alpha = .5) +           #jen je je tady navíc ribbon odhadu
  geom_ribbon(aes(ymin = ci_lower,   #geom_ribbon vytváří oblast
                  ymax = ci_upper),  #potřebuje proto argumenty rozpětí
              fill = "blue",
              alpha = .3) +
  geom_line(color = "blue") +
  facet_wrap(~pohlavi, nrow = 2) +
  coord_cartesian(ylim = c(0.5, 4)) +
  labs(x = "Self-esteem",
       y = "Predikovaná depresivita")
#modrá oblast = podmíněný průměr
#červená oblast = samotné odhadnuté hodnoty


# Moderace ----------------------------------------------------------------
#nebo interakce
#moderace = efekt proměnné A na proměnnou B se liší dle proměnné C

# Kategorická proměnná jako moderátor
math <- read_csv("https://is.muni.cz/go/oqd8xr")
math
#zde training --> math (ovlivněn genderem)

math <- math %>% 
  mutate(gender = factor(gender,
                         levels = c(0, 1),
                         labels = c("Male", "Female")))
math
summary(math)

fit_1 <- lm(math ~ gender + training, 
            data = math)
        #BEZ moderace
fit_2 <- lm(math ~ gender * training, 
            data = math)
        #S moderací
        #místo plus je tam * (jako násobek)
        #protože statisticky je moderace násobek prediktorů

#btw Rko automaticky převádí factory na dummy proměnné
#a 0 má první kategorie
summary(fit_1)
summary(fit_2)
  #druhý model má navíc interakcí člen genderFemale:training
  #asi o 20% vyšší vysvětlený rozptyl (!)
  #ten rozdíl -2.75 pro ženy je zase pro prediktory s nulou
  #-> je to vidět na začátku/vlevo na grafu
  #trénink je pak sklon přímky u mužů
  #efekt interakce je pak o kolik je sklon u žen vyšší

#zobrazení moderace
math %>% 
  ggplot(aes(x = training, 
             y = math,
             color = gender,
             fill = gender)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm") +
  scale_fill_manual(values = c("lightblue", "lightpink")) +
  scale_color_manual(values = c("blue", "pink"))
  #zde je zobrazení moderace
  #stačí jedna křivka pro ženy, jedna pro muže

# Reparametrizace modelu, pokud chceme znát tzv. simple effects
math <- math %>% 
  mutate(
    m = as.integer(gender == "Male"),
    f = as.integer(gender == "Female"),
    m_train = m*training,
    f_train = f*training
  )


fit_3 <- lm(math ~ -1 + m + f + m_train + f_train, 
            data = math)
summary(fit_3)
#tohle je když bychom chtěli vidět konkrétní data
#bez odhadu interceptu
#s odhadu parametrů je vidět hodnota bez tréninku pro muže, pro ženy
#a efekt tréninku pro muže a efekt pro ženy
#resp sklon pro může a rozdílnost sklonu pro ženy


# Spojitá proměnná jako moderátor --------------
depress <- read_csv("https://is.muni.cz/go/p5psox")
depress
#předpoklad, že efekt stresu na depresi je o sociální opoře

summary(depress)

# Vycentrování prediktorů
depress <-  depress %>% 
  mutate(
    stress_cnt = stress - 5,
    support_cnt = support - 5,
  )
    #vycentrování prediktorů střední hodnotou


fit_1 <- lm(depress ~ stress_cnt + support_cnt, 
            data = depress)
        #BEZ interakce

fit_2 <- lm(depress ~ stress_cnt * support_cnt, 
            data = depress)
        #S interakcí

summary(fit_1)
summary(fit_2)
  #z 80 na 96% vysvětleného rozptylu
  #efekt podpory samotné je nižší

#jak zobrazit spojitý moderátor --> rozdělení do skupin
#--> diskretizace --> fce cut_number() na určitý počet skupin
# Diskretizace support 
depress <- depress %>% 
  mutate(
    support_cat = cut_number(support, n = 3)
  )

depress %>% 
  ggplot(aes(stress, depress,
             color = support_cat,
             fill = support_cat)) +
  geom_jitter(size = 3) +
  geom_smooth(method = "lm")
  #efekt stresu byl dokonce trochu negativní na depresivitu
  #u vyšší sociální opory
  #a zase naopak pro nízkou podporu

summary(depress)

# Hodnoty prediktorů, pro které chceme zjistit predikované hodnoty
new_data <- expand.grid(
  stress_cnt = seq(-4, 5, length.out = 1000),  #tady míra stresu od -4 do 5
  support_cnt = c(-2.5, 0, 2.5)  #tady 3 úrovně podpory
) %>% 
  as_tibble()
  #připravíme si hodnoty prediktorů, pro které budeme chtít predikce

ci <- broom::augment(fit_2,
                     newdata = new_data,
                     interval = "confidence")
  #tady už doplníme predikované hodnoty

ci <- ci %>% 
  mutate(
    group = factor(support_cnt,
                   labels = c(2.5, 5.0, 7.5))
  ) #jen převod na faktory


ci %>% 
  ggplot(aes(x = stress_cnt + 5,
             y = .fitted,
             color = group,
             fill = group)) +
  geom_ribbon(aes(ymin = .lower,
                  ymax = .upper),
              alpha = .3) +
  geom_line() +
  labs(x = "Stress",
       y = "Predikovaná depresivita",
       color = "Sociální podpora",
       fill = "Sociální podpora")
  #zobrazení predikovaného
#příště budeme řešit reportování pomocí takového lepšího R markdownu