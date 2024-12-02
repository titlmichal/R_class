
##%######################################################%##
#                                                          #
####                        SEM                         ####
#                                                          #
##%######################################################%##


# Tutorial: http://lavaan.ugent.be/tutorial/
# Manual: http://users.ugent.be/~yrosseel/lavaan/lavaanIntroduction.pdf
# Lavaan cheat sheet: http://jeromyanglim.tumblr.com/post/33556941601/lavaan-cheat-sheet

# Preface -----------------------------------------------------------------

# Balíčky
library(psych)
library(corrplot)
library(semTools)
library(dynamic)
library(pheatmap)
library(lavaan)    #ten hlavně, obecně se hodí na SEM (laten var analyses = lavaan)
library(tidyverse)

#dnes to bude hlavně o CFA

### Import dat
df <- haven::read_sav("https://is.muni.cz/go/ol7a9l") %>% 
  select(gender:dcq_12)

#DCQ - data o vývojových krizích/životních tranzicích
#3 faktory

glimpse(df)

# Jména položek
items <- str_c("dcq_", 
               str_pad(1:12, pad = 0, width = 2))

# Znění položek
wording <- df %>% 
  select(all_of(items)) %>% 
  map_chr(~attributes(.)$label) %>% 
  unname()
wording

# Převrácení reverzních
reversed <- items[5:8]

df <- df %>% 
  mutate(
    across(
      all_of(reversed),
      ~6 - .x
    )
  )

# Popisné statistiky položek ----------------------------------------------

# Položková analýza a odhad reliability
df %>% 
  select(all_of(items)) %>% 
  psych::alpha()
  #korelace položek s celkovým skorem

# Převod na dlouhý formát (pro sloupcové grafy)
long <- df %>% 
  pivot_longer(all_of(items),
               names_to = "item",
               values_to = "response")
df
long

# Sloupcové grafy s četmostmi odpovědí na jednotlivé položky
response_counts <- long %>% 
  drop_na(response) %>% 
  count(item, response) %>% 
  mutate(p = n / sum(n),
         response = as_factor(response),
         .by = item)

# Obyčejné sloupcové grafy
response_counts %>% 
  ggplot(aes(x = response, 
             y = p)) +
  geom_col() +
  facet_wrap(~item)

# Skládané sloupcové grafy
response_counts %>% 
  ggplot(aes(x = p,
             y = item,
             fill = fct_rev(response))) +
  geom_col() +
  scale_fill_brewer(palette = "RdYlGn",
                    direction = -1)

# Libovolné deskriptivní statistiky s pomocí summarise
long %>% 
  drop_na(response) %>% 
  group_by(item) %>% 
  summarise(
    m = mean(response),
    sd = sd(response),
    p = (m - 1) / 4,
    skew = psych::skew(response)
  )

# Korelace mezi položkami
rmat <- df %>% 
  select(all_of(items)) %>% 
  cor()

round(rmat, digits = 2)

corrplot::corrplot.mixed(rmat, 
                         lower.col = "black",
                         order = 'hclust')

# SMC: Squared multiple correlations
# koeficient vicenasobné korelace
# položka predikována všemi ostatními položkami to je jakoby
# Rozptyl vysvětlený všemi ostatními položkami
# Položky s nízkými hodnotami SMC moc společného rozptylu s ostatními položkami
# Nesdílejí
r2 <- df %>% 
  select(all_of(items)) %>% 
  psych::smc()

tibble(
  item = items,
  smc = r2
) %>% 
  ggplot(aes(x = fct_reorder(item, smc, 
                             .desc = TRUE), 
             y = smc)) +
  geom_point(size = 3) +
  geom_line(mapping = aes(group = 1)) +
  expand_limits(y = 0)

# Scree-plot aka sutinový graf vlastních čísel
df %>% 
  select(all_of(items)) %>% 
  psych::scree()

# Paralelní analýza
df %>% 
  select(all_of(items)) %>% 
  psych::fa.parallel(quant = .50,
                     n.iter = 100,
                     sim = FALSE)

#nasvědčuje asi 3 faktorám
#...odpovídá teorii

# Ověření normality --------------------------------------------------
# U ordinálních promměných dopředu víme, že jejich rozdělení nemůže být
# normální
# ale třeba ML estimátory předpokládají normalitu
#jakkoliv z principu to nelze splnit

# Normalita jen zřídka naplněna, multivariační zvlášť.
df %>% 
  select(all_of(items)) %>% 
  psych::mardia() # Mardia's test

# Henze-Zirkler test a Robustní Mahalanobisovy vzdálenostii
df %>% 
  select(all_of(items)) %>% 
  MVN::mvn(multivariateOutlierMethod = "quan",
           showOutliers = TRUE)

# Specifikace modelů -------------------------------------------------------
#tohle je ta hlavní část skript

# Pomocné funkce

# Pro odhad CFA
my_cfa <- function(data, model, ...) {
  lavaan::cfa(data = data,
              model = model,
              estimator = "ml",
              missing = "listwise", #nejsou chybějící - stačí listwise
              ...)
}
#argumenty: data, model k odhadu, odhad a nakládání s missing

# Pro output modelu
output <- function(fit) {
  summary(fit, std = TRUE, ci = TRUE, fit = TRUE)
}
  #jinak by bylo nutné vypisovat ručně

# Pro indexy shody s daty
fit_measures <- function(fit) {
  out <- fitMeasures(
    fit, 
    fit.measures = c("chisq", "chisq.scaled", "df", "cfi", "cfi.robust",
                     "rmsea", "rmsea.ci.lower", "rmsea.ci.upper",
                     "rmsea.robust", "rmsea.ci.lower.robust",
                     "rmsea.ci.upper.robust",
                     "srmr")) %>% 
    as.list()
  out$AIC <- AIC(fit)
  out$BIC <- BIC(fit)
  out
}

# Jak si ulehčit psaní názvů položek
items %>% 
  str_c(collapse = " + ") %>% 
  writeLines()

# Model 1 - jednoduchý, jednodimenzionální
#nalevo napíšeme název proměnné, která by se neměla vyskytovat v datech
#zde DCQ
# =~ značí že je měřena
# za =~ říkáme čím je měřena
m_1 <- "
DCQ =~ dcq_01 + dcq_02 + dcq_03 + dcq_04 + dcq_05 + dcq_06 + dcq_07 + 
       dcq_08 + dcq_09 + dcq_10 + dcq_11 + dcq_12

"

fit_1 <- my_cfa(df, m_1)
output(fit_1)
  #škála je fixována jako reliabilní rozptyl prvního faktoru
  #proto je odhad u něj 1.000
#jinak to teda vrací hodnoty, fity, náboje a rozptyly

# Model 2 - jednoduchý, třífaktorový model
m_2 <- "
# Disconnection & Distress
DD =~ dcq_01 + dcq_02 + dcq_03 + dcq_04

# Lack of Clarity & Control
LCC =~  dcq_05 + dcq_06 + dcq_07 + dcq_08

# Transition & Turning Point
TTP =~ dcq_09 + dcq_10 + dcq_11 + dcq_12
"
  #křížky v modelu jsou jen komentáře naše

fit_2 <- my_cfa(df, m_2)
output(fit_2)

# Model 3 - třífaktorový model s komplikacemi
m_3 <- "
# Disconnection & Distress
DD =~ dcq_01 + dcq_02 + dcq_03 + dcq_04

# Lack of Clarity & Control
LCC =~ dcq_05 + dcq_06 + dcq_07 + dcq_08

# Transition & Turning Point
TTP =~ dcq_09 + dcq_10 + dcq_11 + dcq_12

#reziduální korelace položek 9 a 10 -> ZDŮRAZNIT NEJEN FITY ALE TEORIÍ!
dcq_09 ~~ dcq_10
"

fit_3 <- my_cfa(df, m_3)
output(fit_3)

# S–1 bifactor model - 1 OBECNÝ faktor a série ortogonálních faktorů
m_4 <- "
G =~ dcq_01 + dcq_02 + dcq_03 + dcq_04 + dcq_05 + dcq_06 + dcq_07 + 
     dcq_08 + dcq_09 + dcq_10 + dcq_11 + dcq_12

# Disconnection & Distress
### Rererence factor DD =~ dcq_01 + dcq_02 + dcq_03 + dcq_04

# Lack of Clarity & Control
LCC =~ dcq_05 + dcq_06 + dcq_07 + dcq_08

# Transition & Turning Point
TTP =~ dcq_09 + dcq_10 + dcq_11 + dcq_12

dcq_09 ~~ dcq_10

# Orthogonality constraints
G ~~ 0*LCC + 0*TTP
LCC ~~ 0*TTP
#když je nulový ten koeficient, tak je fixován na nulovou souvislost
"

fit_4 <- my_cfa(df, m_4)
output(fit_4)
#komplexnější model obecně fitne líp, ale dává taky smysl?

#hierarchický model
#lze udělat tak, že hlavní faktor bude sycen subfaktory
#výše tedy jako G =~ DD + LCC + TTP
#je prý třeba brát v úvahu, že jsou ortogonální
#G ~ 0*DD + 0*TTP ... nebou použít argument orthogonal = TRUE v my_cfa()
#jinak by ale ten model vypadal stejně jako 3faktorový,
#protože je statisticky ekvivalentní

# Odhad více modelů najednou ------------------------------------------------

# Můžeme si vytvořit list se všemi modely 
m_list <- list(
  one = m_1,
  three = m_2,
  three_tuned = m_3,
  bifac = m_4
)

# Odhad všech modelů pomocí fce map(), která bere každý prvek listu
# a uplatní na něj, co je specifikováno
fits <- m_list %>% 
  map(~cfa(data = df, 
           model = .x,
           estimator = "ml",
           std.lv = TRUE,
           missing = "listwise"))
  #výsledek je taky list
  #--> uplatnit na něj fci např. fit_measures() na shodu s daty

# Přehled fit indexů
fit_indices <- fits %>% 
  map_df(fit_measures, .id = "model")

fit_indices

# Jednofaktorový má hodně špatný fit
# Třífaktorový - vytuněný má dobrý fit,
# Bifaktorový má nejlepší fit, ale je zároveň nesložitější


# Rozdílový chi2 test
#exaktní test 2 a víc modelu
lavTestLRT(fits$one, fits$three)
lavTestLRT(fits$three, fits$three_tuned)
  #velký vzorek --> i mizivé rozdíly jsou signifikantní
  #to ale neznamená, že to je obecně dobře

# Výsledky ----------------------------------------------------------------

# Finální model, který chceme použít 
final <- fits$three_tuned

# Stručný souhrn výsledků.
output(final)
summary(final, std = TRUE, ci = TRUE, fit = TRUE) #defaultně jsou FALSE

#Přehled odhadnutých parametrů - nepřehledný, technický.
parameterEstimates(final, level = .99) #nestandardizované
standardizedSolution(final, level = .99) # standardizované řešení
  #rozptyl pozorovaných i latentních na 1

#Všechny indikátory shody počítané lavaanem.
fitmeasures(final)

# Jen vybrané, které jsme si definovali ve custom funkci
fit_measures(final)

# Shrnutí fitů - pro všechny modely zvlášť
fit_indices

# dynamic cut-offs for fit indexy
#nověji se upouští od pevných cutoffů --> dynamické se používají
#fungují tak, že si necháme simulovat výběrové rozdělení
#kdyby náš model fitoval populaci
#a když by tam něco chybělo (např. cross-loading)
#...relativně dlouho to trvá (generuje data + třeba 500x odhaduje fity)
#výsledek: magnitued - chybějící cross-loading
# a doporučený cutoff
#když se distribuce hodně překrývají (je to dobrý model), tak vrátí None
#takže díky tomu máme vědět, kdy vhodně zamítnout modely
#btw v grafu jsou standardní cutoffy (pevné) tou necelou čárou
#v tomhle konkrétním případě by model neodpovídal ani nejmírnějšímu cutoffu
# => takže ta misspecifikace toho naše modelu je horší, než kdyby tam chyběly 2 crossloading
#nejmírnější se určí dle toho x/x (kolik podrží/kolik zamítne)
dynamic::cfaHB(final, plot = TRUE, reps = 500)

# Modelem implikovaná variačně-kovarianční matice a vektor průměrů.
fitted(final)

# Vstupní kovarianční matice
inspect(final, 'sampstat')

# Koeficienty modelu
inspect(final, what = "est")  # vše
inspect(final, what = "est")$lambda  # faktorové náboje
inspect(final, what = "est")$theta   # residuální variance/kovariance pozorovaných proměnných
inspect(final, what = "est")$psi     # kovariance/korelace faktorů


#Nahlédnutí do objektu lavaan. Defaultně dá přehled parametrů.
inspect(final) # Volné parametry (nulu mají parametry zafixované na 0)
inspect(final, what = "patterns") # missing data patterns
inspect(final, what = "std.nox") # Kompletně standardizované řešení

# Modifikační indexy a reziduální kovariance/korelace -----------------------
#odhad jak by se mohla shoda modelu s daty zlepšit (chi-kvadrát)
modificationindices(fits$three, sort. = TRUE)

# Reziduální matice.
lavResiduals(fits$three, type = "raw") # variance/kovariance
lavResiduals(fits$three, type = "cor") # korelace a z-test

# Můžeme se podívat na to, které pozorované vztahy model špatně reprodukuje
# Nejlépe graficky
lavResiduals(fits$three, type = "cor")$cov %>% 
  corrplot::corrplot.mixed(upper = "circle", 
                           lower = 'number',
                           lower.col = "black")

# Graf standardizovaných reziduí
lavResiduals(fits$three_tuned, type = "cor.bentler")$cov.z %>% 
  pheatmap::pheatmap(display_numbers = T)
  #z-test
  #např. vysoký misfit položky 1 a 4
  #hodí se to na diagnostiku, proč model špatně fituje
  #protože globální indexy řeknou dobrá X špatná shoda
  #na lokální úrovni chceme reziduální korelace
  #abychom zjistili, kde hůře reprodukuje pozorovanou korelaci
lavResiduals(fits$three_tuned, type = "cor.bentler")$cov %>% 
  pheatmap::pheatmap(display_numbers = T)

# Reliabilita ------------------------------------------------------
#btw má i jiné pomocné fce

semTools::compRelSEM(final, return.total = TRUE) # Omega
semTools::compRelSEM(final, return.total = TRUE, tau.eq = TRUE) # Alpha
  #cronbach má víc předpokladů (tau a 1dimenzionalita)
  #--> proto omega zde lepší
  #tau-ekvivalence = položky mají shodné faktorové náboje
  #což v psychologii de facto nejde

# SEM Diagram -----------------------------------------------------
#to je asi největší piplačka

library(tidySEM) #asi nejvíc flexible

#prvně vytvořit layout, jak bude graf vypadat
# uvést tam ve formě matice názvy proměnných a položek
lay <- c(
  "", "DD", "", "", "", "", "LCC", "", "", "", "", "TTP", "",
  "", "", "", "", "", "", "", "", "", "", "", "", "",
  "", "", "", "", "", "", "", "", "", "", "", "", "",
  "dcq_01", "dcq_02", "dcq_03", "dcq_04", "dcq_05", "dcq_06", "dcq_07", "dcq_08", "dcq_09", "", "dcq_10", "dcq_11", "dcq_12"
) %>% 
  matrix(nrow = 4,
         byrow = TRUE)

lay #typicky latentní proměnné nahoře, pod nimi položky

gdata <- prepare_graph(final, 
                       layout = lay,
                       ellipses_width = 2,
                       ellipses_height = 2,
                       rect_width = 1.5,
                       rect_height = 0.8)
    #pak stačí fce prepare_graph() a který layout použít

plot(gdata) #...a pak to plottnout
#1. verze je málokdy ideální --> iterovat

# Nezobrazovat rozptyl latentních proměnných, který je stejně fixován na 1
edges(gdata) <- edges(gdata) %>% 
  filter(label != "1.00")

plot(gdata)

# Šipky vyjadřující reziduální rozptyl vždy ukazovat dole
edges(gdata) <- edges(gdata) %>% 
  mutate(
    connect_from = if_else(from == to, "bottom", connect_from),
    connect_to = if_else(from == to, "bottom", connect_to)
    )

plot(gdata)

# Šipky pro faktorové náboje připojovat shora
edges(gdata) <- edges(gdata) %>% 
  mutate(
    connect_to = if_else(arrow == "last", "top", connect_to)
  )  

plot(gdata)

# Doplnit do labelů i interval spolehlivosti a smazat leading zero i hvězdičky
edges(gdata) <- edges(gdata) %>% 
  mutate(
    label = str_c(est_std, "\n",  confint_std) %>% 
      str_replace_all(pattern = fixed("0."), replacement = ".")
  )

plot(gdata)

# Labely položek velkými písmeny
nodes(gdata) <- nodes(gdata) %>% 
  mutate(label = str_replace(label,
                             pattern = "dcq_",
                             replacement = "DCQ"))

plot(gdata)

# Změnit pozice labelů pro edges
gdata <- gdata %>% 
  edit_graph({label_location = c(
    .8, .5, .8, .5, 
    .8, .5, .8, .5, 
    .8, .5, .8, .5, 
    rep(0.5, 16)
  )}, element = "edges"
  )

plot(gdata)

#...od té matice to byly všechny změny potřebné k publikaci


nodes(gdata)
# název proměnné, tvar v grafu, label nadepsaný uprostřed, souřadnice nodu

edges(gdata)
#jak pro nodes, ale hrany
#odkud kam (proměnné), odkud (zespodu/shora) kam (...)
#zašikmení
#typ linky
#a pak hodnoty z lavaanu

#uložení grafu (protože to používá podobné věci jako ggpplot)
ggsave("first_SEM_graph.jpg")

#lavaan používá obecně FIML (full information maximum likelihood)
#který tolik neřeší NAs

#BTW LAVAAN BY PRÝ NĚJAK UMĚL ŘEŠIT I FORMATIVNÍ KONSTRUKTY!
#ale teda to chce nějak i predikovat u toho, aby byl model identifikovaný