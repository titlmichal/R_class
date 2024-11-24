library(haven)
library(betaDelta)
library(car)
library(broom)
library(tidyverse)

df <- haven::read_sav("https://is.muni.cz/go/moixq6")
df <- df %>% 
  mutate(protest = as_factor(protest))

# Data pocházejí ze studie Garciové et al. týkající se genderové diskriminaci, 

# Autoři se zabývala tím, jak ženy vnímají jiné ženy, které se ohradily proti 
# genderové diskriminaci. Respondentky si přečetly popis jedné situace 
# v právnické firmě, kdy byl muž-právník povýšen na úkor očividně 
# kvalifikovanější ženy (celá situace byla úmyslně popsána tak, aby bylo jasné, 
# že jde o diskriminaci).

# Vyprávění mělo tři možná pokračování: diskriminovaná žena (Catherine) buďto 
# neprotestovala (i když byl zklamaná, s rozhodnutím se smířila), 
# vznesla „individuální protest“ (souhlasila s tím, že většina žen možná 
# není dostatečně kompetentní na danou pozici, ale ona ano), 
# nebo „kolektivní protest“ (stěžovala si na to, že firma se chová celkově 
# k ženám nespravedlivě). Každé z žen četla vždy ale pouze jedno z pokračování, 
# čímž vznikly tři výzkumné skupiny. 

# Kromě toho respondentky vyplňovaly Modern Sexism Scale (MSS), 
# čítající 8 položek a měřící přesvědčení o rozšířenosti sexismu 
# (diskriminace žen na úkor mužů) ve společnosti (příklad položky: 
# „Discrimination against women is no longer a problem in the United States.“). 
# Respondentky odpovídaly na škále od 0 = „silně nesouhlasím“ 
# do 6 „silně souhlasím“ a celkový skór byl vypočten jako průměr položek.

# Hlavní závislou proměnnou byly sympatie vůči ženě (liking), 
# která se stala obětí genderové diskriminace. Jednalo se o šest položek 
# hodnocených na škále od 0 = „silně nesouhlasím“ do 6 = „silně souhlasím 
# (příklad položky: „I like Catherine.“).

# Autoři předpokládali, že protestující žena bude hodnocena pozitivněji 
# a že se mohou lišit i typy protestu, ale neměli ohledně nich žádné 
# specifické hypotézy. Dále také předpokládali interakci mezi reakcí oběti 
# na genderovou diskriminaci a vnímanou mírou rozšířenosti sexismu. 

# Konkrétně očekáváme, že respondentky, které nevnímají sexismus jako 
# společensky rozšířený problém, budou kladněji hodnotit „neprotestující“ ženu, 
# zatímco respondentky, které vnímají sexismus jako společensky rozšířený jev, 
# budou kladněji hodnotit ženu, která se proti genderové diskriminaci postavila 
# a „protestovala“. “ ženu, zatímco respondentky, které vnímají sexismus jako 
# společensky rozšířený jev, budou kladněji hodnotit ženu, která se proti 
# genderové diskriminaci postavila a „protestovala“. 

# Protože interakce je „symetrická“, mohli bychom to vyjádřit i tak, 
# že přesvědčení o rozšířenosti sexismu bude kladně souviset s hodnocením 
# protestující oběti, ale záporně s hodnocením neprotestující oběti.

# ZADÁNÍ SAMOTNÉ ANALÝZY:
# Odhadněte dva regresní modely s liking jako závislou proměnnou
# V prvním modelu jako prediktory použijte protest + sexism
# V druhém modelu doplňte i jejich interakci
# Porovnejte fit obou modelů.
# Znázorněte graficky povahu interakce mezi protest * sexism pomocí
# bodového grafu s regresními přímkami podle úrovní proměnné protest
# Výsledky krátce okomentujte (hlavně z hlediska toho, jestli podporují 
# výchozí výzkumné hypotézy autorů).

df %>% 
  print(n = 129)

df %>% 
  group_by(protest) %>% 
  count()

# model 1: protest + sexism --> liking ---------------------
mdl1 <- lm(liking ~ protest + sexism,
   data = df)
  #model samotný

summary(mdl1)
  #výstup modelu

summary(mdl1)$r.squared
  #jen R sqrd

betaDelta::BetaDelta(mdl1, alpha = .05)
  #standardizované koeficienty

broom::glance(mdl1)
  #hezčí tabulka dat z modelu

plot(mdl1, which = 1, id.n = 10) #rezidua vs predikce
plot(mdl1, which = 2, id.n = 10) #qq plot
plot(mdl1, which = 3, id.n = 10)
plot(mdl1, which = 4, id.n = 10)
plot(mdl1, which = 5, id.n = 10)
plot(mdl1, which = 6, id.n = 10)

res1 <- tibble(resid = resid(mdl1),
              std_resid = as.double(scale(resid)))
              #z skory reziduí

res1 %>% 
  ggplot() +
  geom_histogram(aes(x = std_resid,
                     y = after_stat(density)),
                 color = "blue", fill = "lightblue") +
      #histogram std reziduí
  stat_function(fun = dnorm,
                color = "red") +
      #normální rozdělení
  coord_cartesian(xlim = c(-5, 5))
  #i bych řekl, že jsou cca normální

res1 %>% 
  ggplot(aes(sample = std_resid)) +
  geom_qq_line(color = "blue",
               linewidth = 1) +
  geom_qq(size = 2,
          alpha = .5)
  #qq plot reziduí
  #+- podobné, jak je vidět v histogramu

car::residualPlots(mdl1)
  #rezidua modelu vůči hodnotám prediktorů

car::crPlots(mdl1)
  #rezidua bez prediktoru
  #zvláštně se tam ty křivky oddělují

car::avPlots(mdl1)
  #k identifikaci outlierů
  #případy 28 a 3

car::vif(mdl1)
  #multikolinearita
  #ok-ish

#model 2: protest + sexism + protest * sexism --> liking  -------------------
mdl2 <- lm(liking ~ protest * sexism,
           data = df)

summary(mdl2)

summary(mdl2)$r.squared

plot(mdl2, which = 1, id.n = 10)
plot(mdl2, which = 2, id.n = 10)
plot(mdl2, which = 3, id.n = 10)
plot(mdl2, which = 4, id.n = 10)
plot(mdl2, which = 5, id.n = 10)
plot(mdl2, which = 6, id.n = 10)

broom::glance(mdl1)

#zobrazení moderace
df %>% 
  ggplot(aes(x = sexism, 
             y = liking,
             color = protest,
             fill = protest)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm") +
  scale_fill_manual(values = c("lightblue", "lightpink", "lightgreen")) +
  scale_color_manual(values = c("blue", "pink", "green"))

#srovnání modelů  ---------------------
summary(mdl1)
summary(mdl2)

summary(mdl2)$r.squared - summary(mdl1)$r.squared
  #druhý model vysvětlil o 8 % více

anova(mdl1, mdl2)
  #nižší suma čtverců reziduí druhého modelu
  #signifikantní rozdíl

bind_rows(
  m1 = broom::glance(mdl1),
  m2 = broom::glance(mdl2),
  .id = "model"
)
  #AIC a BIC také mířně nižší pro druhý model

# Závěr -------

# H1: "Autoři předpokládali, že protestující žena bude hodnocena pozitivněji"
# ANO, v případě jednoduššího modelu to lze říci, ale odhady druhého modelu
# naznačují, že oba typy protestu vedou k nížšímu hodnocení (oproti žádnému).

# H2: Dále také předpokládali interakci mezi reakcí oběti 
# na genderovou diskriminaci a vnímanou mírou rozšířenosti sexismu. 
# ANO, interakce se ukázala jako signifikantní a navíc významn pro vysvětlený
# rozptyl, jakkoliv druhý model se zdá být zatížený více outliery.

# H3: Konkrétně očekáváme, že respondentky, které nevnímají sexismus jako 
# společensky rozšířený problém, budou kladněji hodnotit „neprotestující“ ženu,
# ANO, průsečík křivky neprotestující ženy je výše než ostatní 2 protesty, tudíž
# u žen s nízkým hodnocením sexismu ve společnosti je nejvyšší hodnocení
# neprotestující ženy.

# H4: zatímco respondentky, které vnímají sexismus jako společensky rozšířený jev, 
# budou kladněji hodnotit ženu, která se proti genderové diskriminaci postavila 
# a „protestovala“.
# ANO, ženy vnímající sexismus jako rozšířený problémy (tedy jsou na škále dále),
# hodnotí osobu z příběhu lépe, pokud protestuje (a naopak hůře, pokud ne)

# Protože interakce je „symetrická“, mohli bychom to vyjádřit i tak, 
# že přesvědčení o rozšířenosti sexismu bude kladně souviset s hodnocením 
# protestující oběti, ale záporně s hodnocením neprotestující oběti.
# ANO, data ukazují tento trend.