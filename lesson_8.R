library(rstatix)
library(effectsize)
library(rcompanion)
library(tidyverse)

# Import dat ------------------------------------------------------------
df <- haven::read_sav("data/eukids.sav")
df
glimpse(df)
head(df)

# Převedení některých proměnných na faktory
df %>% 
  distinct(pohlavi, kohorta)

df <- df %>% 
  mutate(
    sex = factor(pohlavi,        #factor jako kategorická typ
                 levels = c(1, 2),
                 labels = c("Boys", "Girls")),
    cohort = factor(kohorta,
                    levels = c(6, 10),
                    labels = c("Younger", "Older")),
    .after = id
  )
df

df %>% 
  distinct(pohlavi, sex, kohorta, cohort)

# Pohlaví - četnosti
df %>% 
  count(sex) %>% #spočítá proměnné sex a vrátí df s počtem n
  mutate(p = n/sum(n)) #vytvoří nový sloupec p o relativní četnosti

df %>% 
  count(cohort) %>% 
  mutate(p = n/sum(n)) #to stejné akorát pro kohorty

# Odstranění respondentů s chybějícími hodnotami v sex a cohort
df %>% 
  count(sex) # Tři missing values

df %>% 
  count(cohort) # Jen jedna missing value

df <- df %>% 
  drop_na(sex, cohort) #fce drop_na odstraní všechny řádky s na value

# Jenom přemístění sloupců někde na začátek
df <- df %>% 
  relocate(monit, warm_m, warm_o,
           .after = cohort)

df

df %>% 
  count(sex, cohort)
# Srovnání dvou nezávislých výběrů ------------------------------
# Budeme analyzovat rozdíly mezi kohortami v monitorování rodiči

# Chybějících dat moc není
df %>% 
  count(missing = is.na(monit))
  #is.na() fce vrací tru, když chybí hodnota
  #TRUE jsou missing --> 5

# * Deskriptivní statistiky ------------------------------------

# funkce pro výpočet deskriptivních statistik
my_stats <- function(x, level = .95) {
  x <- x[!is.na(x)] #vybere z x vše, co není missing
  n <- length(x)    #počet hodnot
  m <- mean(x)      #průměr hodnot
  s <- sd(x)        #sd hodnot
  se <- s/sqrt(n)   #standardní chyba průměru
  p <- level + (1-level)/2     #p-value (default 0,95)
  me <- qt(p, df = n - 1)*se   #střední chyba v CI pomocí kvantilů
  
  q <- quantile(x, probs = c(0, .25, .50, .75, 1),
                na.rm = TRUE)   #quantily
  #tibble pro datový rámec na výpočty
  tibble(m = m, # průměr
         m_lo = m - me, # spodní limit CI
         m_hi = m + me, # horní limit CI
         n = n, # počet validních hodnot
         se = se, # standardní chyba
         skew = psych::skew(x), # šikmost
         kurt = psych::kurtosi(x), # špičatost
         min = q[1], # minimum
         q25 = q[2], # první kvartil
         mdn = q[3], # medián
         q75 = q[4], # třetí kvartil
         max = q[5]) # maximum
}
#tible je občas lepší skrze
#lepší zobrazení outputu
#nemění názvy sloupců (jako občas df)
#tibble vrací output tibble (df může vrátit vektor)
#aplikaci v dalších balíčcích
#lepší pro větší data
#při chybném výběr sloupce tak nevrací chybu

my_stats(df$monit)

# Deskriptivní statistiky pro monitorování podle kohorty
monit <- df %>% 
  group_by(cohort) %>% 
  summarise(my_stats(monit))

monit

# * Testy ověřujíc některé předpoklady --------------------------

# Levenův test shody rozptylů
df %>% 
  rstatix::levene_test(monit ~ cohort)
  #~ cohort, že je proměnná rozdělená kohortou
  #obecně se řeší p-value 0,05
  #vyšší --> nelze zamítnou rozdílnost
  #nižší --> statisticky významný rozdíl
  #test se hodí u Anovy nebo t-testu, kde chci homo. rozptyly
  #H0 = rozptyly jsou homogenní; H1 = alespoň dvojice skupin má jiný
  #test založen na abs odchylkách od Md/M každé skupiny
  #průměr odchylek každé skupiny --> jejich rozdíly --> F-rozdělení
  #robustní vůči outlierům

# Shapirův-Wilkův test of normality
df %>% 
  group_by(cohort) %>% 
  rstatix::shapiro_test(monit)
  #prvně groupnuto --> test pro každou skupinu
  #p-value MENŠÍ než 0,05 = NEnormální rozdělení
  #opět ANOVA nebo t-test chce normální data
  #vhodný na malé až střední vzorky

# * Grafy ----------------------------------------------

# Jenom změna estetického schématu
theme_set(
  theme_classic(base_size = 15)
)
  #theme_set fce na globální setup tématu
  #theme_classic ... jedna z možností
  #base_zie ... velikost písma

# Boxplot
df %>% 
  ggplot(aes(cohort, monit)) +
  geom_boxplot()
  #klasika gg plot s aes argumentem
  #který bere, které proměnné chci zobrazit
  #+ musím říct, čím zobrazit
  #zjišťuji střední hodnotu, rozptyl a symetričnost rozdělení, outliery

# Boxplot i s průměry
df %>% 
  ggplot(aes(cohort, monit)) +
  geom_boxplot() +
  stat_summary(fun.data = mean_cl_normal, # funkce pro výpočet CI pro průměr
               geom = "pointrange", # Použitý geom
               color = "red")
  #viz výše (aes, boxplot, ...) plus:
  #stat_summary fce k přidání souhrnných hodnot do grafu
  #automaticky bere data z aes argumentu/fce
  #zvolený geom "pointrange" vrátí bod a CIs vykreslené

# Boxplot + violinplot
df %>% 
  ggplot(aes(cohort, monit)) +
  geom_violin(width = .5) +
  geom_boxplot(width =.25)
  #stejné jak boxplot výše ale přidá violinplot
  #violinplot zobrazuje hustotu rozdělení
  #širší --> víc hodnot v daném bodě
  #widthem oddělujeme šířku vizualizace, aby byla dobře vidět
  #lze vyvozovat dojem normality dat

# Line charts of means
df %>% 
  ggplot(aes(cohort, monit)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "pointrange",
               size = 1)
  #jak výše ale bez box/violinplotu
  #fun.data je pro fce vracející více hodnot
  #zde průměr a horní/dolní hranice CIs

# Kdybychom neměli dopředu vypočtené deskriptivní statistiky
df %>% 
  ggplot(aes(cohort, monit)) +
  stat_summary(fun.data = mean_cl_normal, # vypočíst CI pro průměr
               geom = "errorbar", # Chybové úsečky pro CI průměru
               width = .1) +
  #chybové úsečky a průměr CIs
  stat_summary(fun = mean, # vypočíst průměr
               geom = "point", # tečkou vyznačit průměr
               size = 5) +
  #průměr
  stat_summary(fun = mean, # vypočíst průměr
               geom = "line", # Spojit body čárou
               mapping = aes(group = 1)) # Jen jednou
  #spojení průměrů

# Stejný graf, ale využívající dopředu vypočtených deskriptivních statistik
monit %>% 
  ggplot(aes(x = cohort,
             y = m)) +
  #cohort na x ose, m průměr na y ose
  geom_point(size = 5) +
  #vykreslí body výše popsaného
  geom_line(mapping = aes(group = 1)) +
  #spojí body (i díky group = 1 argumentu)
  geom_errorbar(mapping = aes(ymin = m_lo,
                              ymax = m_hi),
                width = .1)
  #přidá CIs kolem průměrů
  #tibble monit vznikl pomocí fce my_stats
  #ta vytvoří tibble z dané proměnné a jeho různé popisné stats
  #mezi než patří m, m_lo, m_hi
  #ymin, ymax jsou názvy parametrů z balíčku
  

# Jittered points 
df %>% 
  ggplot(aes(cohort, monit)) +
  geom_jitter(width = .1,
              height = .05,
              size = 5,
              alpha = .25)
  #klasika bodový graf
  #width mírně posouvá horizontálně body
  #aby byly vidět
  #height podobně ve vertikální ose
  #size je o velikosti (duh) a alpha o průhlednosti

# Ověření normality
# Převod na z-skóry
scaled <- df %>% 
  select(cohort, monit) %>% 
  group_by(cohort) %>% 
  mutate(
    zscores = as.double(scale(monit))
  )
  #zvolí proměnné, seskupí a vytvoří zscores
  #fce scale standardizuje, as.double převádí na typ dbl
  #z skor = (x-průměr)/sd(x)
  #díky seskupení jsou z-skóry pro jednotlivé skupiny (!)

scaled

# Histogramy a grafy hustoty pravděpodobnosti
scaled %>% 
  ggplot(aes(zscores)) +
  geom_histogram(
    mapping = aes(y = after_stat(density)), # Na ose Y hustota pravděpodobnosti místo absolutních četností
    color = "black", fill = "grey"
  ) +
  geom_density(linewidth = 1, 
               color = "blue") +
  stat_function(fun = dnorm, # Zakrelit i křivku normálního rozdělení
                color = "red",
                linewidth = 1,
                linetype = "11") +
  facet_wrap(~cohort, nrow = 2)
  #ggplot inicalizuje zscores na osu x
  #geom_histrogram vykreslí + doplní na osu y
  #kde bude relativní četnost (pomocí after_stat(density))
  #(plus to udělá šedé a černé okraje)
  #pak dokreslí modrý geom relativních četností křivkou
  #tj. odhad hustoty pravděpdoboností
  #a stat_function(fun = dnorm) dokreslí normální rozdělení
  #a facet_wrap rozdělí grafy dle skupin kohort

# Q-Q grafy
scaled %>% 
  ggplot(aes(sample = zscores)) +
  geom_qq_line() +       #qq křivky nakreslí
  geom_qq() +            #dokreslí qq plot pro proměnné
  facet_wrap(~cohort)    #rozdělí dle proměnné
  #data na přímce --> spíš normální

# * Statistické testy pro srovnání dvou nezávislých výběrů -----------------

# Nezávislý t-test 
t.test(monit ~ cohort, 
       data = df)
  #dvouvýběrový (Studentův t-test)
  #monit ~ cohort ... rozdíl průměrů monit dle cohort
  #t-hodnota ... dle variabiltiy standardizovaný rozdíl
  #p-value ... MENŠÍ než 0,05 --> alternativní hypotéza
  #(H1: skutečný rozdíl mezi skupinami NENÍ 0)
  #df ... klasika stupně volnosti
  #CIs. ... same
  #přepokládá přibližně normálně rozdělená dta a homogennost rozptylů
  #při nesplnění --> Welchův test

t.test(monit ~ cohort, 
       data = df,
       var.equal = TRUE) # Předpokládat shodný rozptyl
  #předpoklad --> už ne Welch, ale student klasika

t.test(monit ~ cohort, 
       data = df,
       conf.level = .99, # Nastavení úrovně spolehlivosti pro CI
       var.equal = TRUE) # Předpokládat shodný rozptyl

# Wilcox-Mann-Whitney test (neparametrická alternativa nezávislého t-testu)
wilcox.test(monit ~ cohort, 
            data = df,
            conf.int = TRUE)  #přidá CIs pro rozdíl mediánů
  #aka Mann-Whitney
  #když nejsou předpoklady pro ty testy výše
  #W ... součet pořadí pro 1 z kategorií
  #p-value MENŠÍ než 0,05 --> JE rozdíl významný
  #CIs mimo nulu --> pravděpodobně rozdíl
  #CIs řeší medián rozdílu

# * Velikosti účinku -------------------------------------------

# Cohenovo d
effectsize::cohens_d(monit ~ cohort, 
                     data = df)
effectsize::cohens_d(monit ~ cohort, 
                     pooled_sd = FALSE, # Předpoládat shodu rozptylů?
                     ci = .99, # Nastavení úrovně spolehlivosti
                     data = df)
  #cohenovo d pro effect size rozdílu 2 skupin
  #jak silný je rozdíl mezi 2 skupinami (0,2/0,5/0,8 thumb rules)


# Velikost účinku: Pravděpodobnost superiority
rcompanion::wilcoxonPS(monit ~ cohort,
                       ci = TRUE,
                       data = drop_na(df, monit))
  #Wilcoxonův test párových pořadí
  # a výpočet effect size pomocí PS
  #PS řeší PST, že random hodnota ze skupiny A je větší
  #než random hodnota ze skupiny B
  #PS kolem 0.5 ... nic moc rozdíl (PST jak u náhody)
  #vyšší výrazně než 0.5 ... spíš bude rozdíl
  #PS 0.633 => PST je 63,3%, že random hodnota monit je větší v jedné kohortě

# Srovnání dvou závislých výběrů --------------------------------
# Budeme testovat rozdíl mezi vnímanou vřelostí matky a otce

df %>% 
  count(warm_o_missing = is.na(warm_o)) # Více chybějích hodnot než u warm_m
  #fce is.na vrací true když chybí/false když ne
  #takže sčítá 2 výsledky: true a false

df %>% 
  count(warm_m_missing = is.na(warm_m))
  #analogicky jak výše

# Příprava dat
df_clean <- df %>% 
  drop_na(warm_m, warm_o) %>% 
  select(id, warm_m, warm_o)
  #dropn missing

df_clean
# Převod na delší formát
df_long <- df_clean %>% 
  pivot_longer(warm_m:warm_o,
               names_to = "variable")
  #transformuje ze širokého do dlouhého formátu
  #tj místo id - warm_m - warm_o
  #z toho je id - variable(warm_m nebo warm_o) - value
  #hodit se třeba při vizualizaci ggplotem
df_long

# * Deskriptivní statistiky a test normality rozdílových skórů -----------
warmth <- df_long %>% 
  group_by(variable) %>% 
  summarise(my_stats(value))
warmth


df_clean %>% 
  mutate(diff = warm_m - warm_o) %>% 
  rstatix::shapiro_test(diff)

# Grafy ---------------------------------------------------------------

# Boxplot 
df_long %>% 
  ggplot(aes(variable, value)) +
  geom_boxplot()

# Boxplot + violin plot
df_long %>% 
  ggplot(aes(variable, value)) +
  geom_violin(width = .50) +
  geom_boxplot(width = .25)

# Line graphs of means
warmth %>% 
  ggplot(aes(x = variable,
             y = m,
             ymin = m_lo,
             ymax = m_hi,
             group = 1)) +
  geom_point(size = 5) +
  geom_line() +
  geom_errorbar(width = .1) +
  scale_y_continuous(breaks = seq(3, 4, by = .05))
  #zase vizualizace bodů, čar, CIs a spojení
  #scale_y_continuous s breaks argumentem nastavuje čárky osy y
  #od 3 do 4 po 0,05

# Bodový graf 1:1
df_clean %>% 
  ggplot(aes(warm_m, warm_o)) +
  geom_jitter(height = .03,
              width = .03,
              alpha = .25,
              size = 3) +
  coord_equal(xlim = c(1, 4),
              ylim = c(1, 4)) +
  geom_abline()
  #coord_equal fce s xlim/ylim nastavuje limity os x a y
  #geom_abline bez argumentů vykresluje přímku y = a + bx

# Histogram a graf hustoty pravděpodobnosti pro rozdílové skóry   
df_clean <- df_clean %>% 
  mutate(diff = warm_m - warm_o)
  #přidání diffu

df_clean

df_clean %>% 
  ggplot(aes(x = diff)) +
  geom_histogram(mapping = aes(y = after_stat(density)),
                 fill = "grey",
                 color = "black") +
  geom_density(linewidth = 1, 
               color = "blue") +
  stat_function(fun = dnorm, # Zakreslit i křivku normálního rozdělení
                color = "red",
                linewidth = 1,
                linetype = "11",
                args = list(mean = mean(df_clean$diff), # nastavit stejný průměr a směrodatnou odchylku jako data
                            sd = sd(df_clean$diff)))
  #histogram rozdělení rozdílů
  #plus pravděpodobnostní rozdělení křivkou
  #plus normální rozdělení se stejným M a SD jako mají data

# Q-Q graf
df_clean %>% 
  ggplot(aes(sample = diff)) +
  geom_qq_line() +
  geom_qq()
  #klasika ... asi to není úplně normálně rozdělené

# Těžké chvosty jsou jasně vidět z obou předchozích grafů

# * Statistické testy pro srovnání dvou závislých výběrů -----------------

# Párový t-test 
t.test(df_clean$warm_m, df_clean$warm_o,
       paired = TRUE)
    #paired definuje, že je to párový t-test (jsou závislé!)
    #p-value = PST pozorování rozdílu, pokud by rozdíl byl nulový reálně
    #zde úplně tiny p-value --> pravděpodobně se liší ty průměry

t.test(df_clean$warm_m, df_clean$warm_o,
       paired = TRUE,
       data = df_clean,
       conf.level = .99)
    #jen širší CIs

# Alternativní zápis
t.test(Pair(warm_m, warm_o) ~ 1,
       data = df_clean)


# Wilcoxonův test (neparametrická alternativa závislého t-testu)
# Pracuje s pořadím (ranks)
wilcox.test(df_clean$warm_m, df_clean$warm_o,
            paired = TRUE)
  #alt hypotéza = MD rozdílu mezi páry je nenulový
  #p-value = PST pozorování při nulové hypotéze (rozdíl tam není)
  #zde zase tiny p-value --> rozdíl asi je

wilcox.test(Pair(warm_m, warm_o) ~ 1,
            data = df_clean)
  #to stejné, jen fce pair z balíčku pairedData


# Znaménkový test: sleduje pouze to, kolikrát  warm_m > warm_o nebo warm_m < warm_o
df_clean %>% 
  count(warm_m_higher = warm_m > warm_o)

df_clean %>% 
  count(nontied = warm_m != warm_o)

binom.test(x = 373, # kolikrát warm_m > warm_o
           n = 576) # celkový počet non-ties
  #binomický test testuje, zda rozdíl, kdy je jedna proměnná vyšší,
  #odpovídá očekávanému poměru (0.5), vzhledem k množství pozorování
  #alt hypotéza: PST rozdílu není 0.5
  #p-value: zase PST pozorování při H0
  #takže zase asi nejsou stejné
  #plus je tam ten výsledek PST rozdílu (jak test superiority)


# Velikost účinku --------------------------------
# Cohenovo d
effectsize::rm_d(df_clean$warm_m, df_clean$warm_o,
                 paired = TRUE,
                 ci = .95)

# Pravděpodobnost superiority
# Spočítal už vlastně binomický test (jako probability of success)
binom.test(x = 373, # kolikrát warm_m > warm_o
           n = 576) # celkový počet non-ties
