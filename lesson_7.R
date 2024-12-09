library(psych)
library(tidyverse)
library(GGally)  #rozšiřuje ggplot
library(qgraph)  #na síťové grafy
#téma: KORELACE

# Načtení dat - stará data z projektu cesty do dospělosti
df <- haven::read_sav("https://is.muni.cz/go/j6bdrc")
df <- df %>% 
  select(id:neshody)

glimpse(df)

# Extracting metadata
metadata <- tibble(
  var_names = names(df[-1]),
  var_labels = map_chr(df[-1], 
                       ~attributes(.)$label)
)

metadata %>% 
  print(n = Inf)

# Korelace mezi optimismem a životní spokojeností - grafy
df %>% 
  ggplot(aes(optim, ziv_sp)) +
  geom_point()
#kvanti proměnné -> bodový graf
#hodně stejných datapoints --> není jasné kolik jich tam je na dané XY coordi
#--> vychýlit náhodnout odchylkou

# geom_point() a position_jitter
df %>% 
  ggplot(aes(optim, ziv_sp)) +
  geom_point(position = position_jitter(width = .4, height = .4,
                                        seed = 123))
#k vychýlení argumentem position a fcí position_jitter

#nebo rovnou použít geom na to
# geom_jitter()
df %>% 
  ggplot(aes(optim, ziv_sp)) +
  geom_jitter(height = .1, width = .3)

#nemusí se nám líbit ten default vizuál
#--> použití + theme_classic() na jiné předdefinované
#(base_size zvyšuje velikost)
df %>% 
  ggplot(aes(optim, ziv_sp)) +
  geom_jitter(height = .1, width = .3) +
  theme_classic(base_size = 15)

#tímhle nastavím schéma pro všechny grafy to go
theme_set(theme_classic(base_size = 15))
#viz below (už tam není theme_classic())
df %>% 
  ggplot(aes(optim, ziv_sp)) +
  geom_jitter(height = .1, width = .3)

#přidání regresní křivky
#pomocí + geom_smooth()
df %>% 
  ggplot(aes(optim, ziv_sp)) +
  geom_jitter(height = 0.1, width = .3) +
  geom_smooth()

# můžu ji pak upravovat
df %>% 
  ggplot(aes(optim, ziv_sp)) +
  geom_jitter(height = 0.1, width = .3) +
  geom_smooth(method = "lm", #specifikuju model
              level = .99,   #konfidenční intervaly
              color = "red", #barvu přímly
              fill = "red",  #výplň CIs
              alpha = .2,    #sílu výplně
              linetype = "31") #typ čáry

df %>% 
  ggplot(aes(optim, ziv_sp)) +
  geom_jitter(height = 0.1, width = .3) +
  geom_smooth(se = FALSE) +  #NEPARAMETRICKÁ křivka
  geom_smooth(method = "lm", #klasická (abych určil linearitu)
              color = "red",
              linetype = "11")

#přidání popisků
#pomocí +labs(x = ..., y = ...)
df %>% 
  ggplot(aes(optim, ziv_sp)) +
  geom_jitter(height = 0.1, width = .3) +
  geom_smooth(method = "lm",
              level = .99,
              color = "red",
              fill = "red",
              alpha = .2) +
  labs(x = "Optimismus",
       y = "Životní spokojenost")

#nebo můžu upravit rozdělení škála X a/nebo Y
#pomocí + scale_x_continuous(breaks = seq(od, do, by = po))
df %>% 
  ggplot(aes(optim/5, ziv_sp)) +  #plus jsem tady vydělil škálu 5 (původně suma)
  geom_jitter(height = 0.1, width = .3, color = "grey") +
  geom_smooth(method = "lm",
              level = .99,
              color = "red",
              fill = "red",
              alpha = .2) +
  labs(x = "Optimismus",
       y = "Životní spokojenost") +
  scale_x_continuous(breaks = seq(1, 5, by = .5)) +
  scale_y_continuous(breaks = seq(1, 4, by = .5)) 

# Base R functions
#zde prostě spearman R
#není na to potřeba balíček --> stačí fce cor.test(var1, var2)
#vyběr z datasetu pomocí data$var
cor.test(df$optim, df$ziv_sp)

#tohle je ekvivalentní zápis
cor.test(~optim + ziv_sp,
         data = df)

#definice metody explicitně
cor.test(df$optim, df$ziv_sp,
         method = "spearman")

#nastavení CIs
cor.test(df$optim, df$ziv_sp,
         conf.level = .99)

#korelace víc proměnných
#prvně si z toho musím udělat objekt
variables <- c("ziv_sp", "optim", "selfe", "effi", "zdravi", "deprese")
#a df
df_cor <- select(df, all_of(variables))

#pak můžu korelovat
cor(df_cor)
#ale musím nastavit use
#protože už jedna missing value --> celé missing
#užití pairwise --> využijí se všechny dostupné případy
cor(df_cor,
    use = "pairwise")
#a případně i zakrouhlení třeba
cor(df_cor,
    use = "pairwise") %>% 
  round(digits = 2)
#a když už korelejume, chceme kontrolovat grafy
#na vícero se hodí ggaly

# GGally a funkce ggpairs()
#prvně specifikuje data a pak sloupce, co chci korelovat
#pod diagonálou grafy, nad koeficienty, na diag. je rozdělení
ggpairs(df, 
        columns = variables) 

#na úpravu už ale potřebujeme vlastní fci
plot_scatter <- function(data, mapping){
  ggplot(data = data, mapping = mapping) + 
    geom_jitter(alpha = .05) + 
    geom_smooth(color = "blue",
                se = FALSE,
                method = "lm",
                linetype = "11") +
    geom_smooth(color = "red",
                fill = "red",
                alpha = .2)
}

ggpairs(df, 
        columns = variables,
        lower = list(continuous = plot_scatter)) 
#pak tu fci hodit do původního přístupu
#zde lower jako pod diagonálou

plot_hist <- function(data, mapping){
  ggplot(data = data, mapping = mapping) + 
    geom_histogram(fill = "grey",
                   color = "black")
}

ggpairs(df, 
        columns = variables,
        lower = list(continuous = plot_scatter),
        diag = list(continuous = plot_hist)) 
#zde diag jako na diagonále


# Q-Q plots and histograms
#pomocí gg plot
#1) vybrat proměnné k analýze
#2) pivot_longer na změnu formátu (alternativně pivot_wider)
#ke konverzi dat do jednoho sloupce (everything()) (names_to k definici názvu)
#teď máme jeden sloupec hodnot a jeden sloupec, odkud pocházejí
#3) proměnné převedem na Z-skóry (.by = variable, aby se to udělalo pro každou)
df %>% 
  select(all_of(variables)) %>% 
  pivot_longer(everything(), 
               names_to = "variable") %>% 
  mutate(value = as.double(scale(value)),
         .by = variable)

df_long <- df %>% 
  select(all_of(variables)) %>% 
  pivot_longer(everything(), 
               names_to = "variable") %>% 
  mutate(value = as.double(scale(value)),
         .by = variable)

#pak si vyjet kontrolu, že to převedlo dobře
#m je nula, jen tam je ta přesnost "jen" na 16 míst nebo kolik
#proto ty skoro nulové
df_long %>% 
  drop_na() %>% 
  group_by(variable) %>% 
  summarise(
    m = mean(value),
    sd = sd(value)
  )

#zde rozdělení proložené normálním
df_long %>% 
  ggplot(aes(value)) +
  geom_histogram(aes(y = after_stat(density)),
                 binwidth = .25) +
  facet_wrap(~variable) +
  stat_function(fun = dnorm,
                color = "red", 
                linewidth = 1)

#zde hustoty
df_long %>% 
  ggplot(aes(value)) +
  geom_density(color = "blue") +
  facet_wrap(~variable) +
  stat_function(fun = dnorm,
                color = "red", 
                linewidth = 1,
                linetype = "11")

#qq plot
df_long %>% 
  ggplot(aes(sample = value)) +
  geom_qq_line() +
  geom_qq() +
  facet_wrap(~variable)
#takže tak
#btw existuje balíček apa tables pro data korelací, regresí, anov a tak
#ve smyslu reportování

# -----------------Balíčej psych
#base R nedává další metriky ke korelacím (CIs, p values a tak)
# Funce corr.test() z balíčku psych
out <- psych::corr.test(df_cor)  #vložení 6 sloupců
print(out, short = FALSE)       #vytisknou (a delší formát vč. CIs)
str(out)
out$stars

# Use complete cases only
out <- psych::corr.test(df_cor,
                        use = "complete")
print(out, short = FALSE)

psych::corr.test(df_cor)
psych::corr.test(df_cor, 
                 adjust = "holm", # jakou korekci p-hodnot použít
                 method = "spearman", # typ korelačního koef
                 alpha = 0.01) %>% # hladina spolehlivosti bude 1 - alpha
  print(short = FALSE)

str(out)
out$stars
print(out, short = FALSE)
#jinými slovy, když chceme více testů korelací
#hodí se fce corr.test z balíčku psych

# Korelogramy pomocí balíčku corrplot a stejnojmenné funkce corplot()
r_mat <- cor(df_cor, use = "pairwise")
r_mat # korelační matice

corrplot::corrplot(r_mat)    #defaultně udělá kruhové obrazce dle síly
corrplot::corrplot(r_mat, 
                   method = "number", #definujeme formu zobrazení
                   order = "hclust")
corrplot::corrplot.mixed(r_mat, 
                         lower = "number",
                         upper = "ellipse",
                         lower.col = "black",
                         order = "hclust") #zde seřazení dle podobnosti vztahů

# Network plots
#když bychom jich měli už moc
#stačí tady korelace vytvořené base R cor
df_cor <- df %>% 
  select(warm_m:neshody)
#chystám data

names(df_cor)
#(názvy proměnných)

r_mat <- cor(df_cor,
             use = "pairwise",
             method = "spearman")
#vytvářím korelační matici pomocí base r

diag(r_mat) <- 0
#nastavuju diagonálu v matici na nulu

#layout spring dává k sobě blízce korelující
#treshold specifikuje hladinu korelací, pod kterou nezobrazuje
#repulsion je pro layout --> určuje, jak moc jsou nody od sebe "odpuzovány"
qgraph(r_mat,
       layout = "spring",
       theme = "colorblind",
       threshold = .2,
       repulsion = .7,
       groups = list(VŘE = 1:2,   #argument groups specifikuje barevné odlišení
                     DŮV = 4:5,   #a které proměnné tvoří ty skupiny
                     IND = 7:9,
                     NEG = 10:12,
                     CRO = 14:16,
                     CSK = 17:20,
                     VM = 26:28),
       labels = c("VŘ.M", "VŘ.O", "MON", "DUV.R", "DUV.V", "VZsR", "IND.1", "IND.2", 
                  "IND.3", "NEG.1", "NEG.2", "NEG.3", "ROD.E", "ROD.P", "ROD.A", "R.S",
                  "ŠK.E", "ŠK.P", "ŠK.A", "ŠK.S", "OPT", "ZSP", "EST", "EFF",
                  "PSS", "VM.U", "VM.S", "VM.N", "DEP", "NESH"),
       #labels udává labely použité uprostřed bodů
       GLratio = 9) #vzdálenost legendy, resp poměr grafu a legendy
#tady jsou sice použity klasické (bivariační) korelace
#ale často se používají parciální korelace (viz síťové modely v psychometrice)