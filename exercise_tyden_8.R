library(tidyverse)

# Pracujte s tímto datasetem
df <- read_csv2("https://is.muni.cz/go/g26j7j")
df

# který obsahuje údaje o pohlaví/sex/, ročníku/grade a dále výsledky psychologických
# dotazníků měřících potřebu poznávání/nfc/ a osobnostní rysy 
# Big Five (extraverzi/bfi_ext/, přívětivost/bfi_agr/, 
# svědomitost/bfi_con/, emoční stabilitu/bfi_est/, a intelekt/bfi_int/)


# Vyberte si jednu z psychologických proměnných, kde očekáváte, 
# že by se chlapci a dívky měli lišit, a ověřte rozdíl mezi nimi pomocí
# vhodného testu
#Předpokládám rozdíl v extraverzi...
df %>% 
  select(id, gender, grade, bfi_ext) %>% 
  distinct(grade)

df_limited <- df %>% 
  select(id, gender, grade, bfi_ext)
df_limited

df_limited %>% 
  count(!is.na(bfi_ext), !is.na(gender), !is.na(grade))

df_limited <- df_limited %>% 
  mutate(pohlavi = factor(gender,
                          levels = c("Girl", "Boy")),
         trida = factor(grade,
                        levels = c(6,7,8,9),
                        labels = c("Sesta", "Sedma", "Osma", "Devata")),
         .after = id)
df_limited

df_limited %>% 
  count(pohlavi) %>% 
  mutate(p = n/sum(n))

df_limited %>% 
  count(trida) %>% 
  mutate

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

df_limited %>% 
  group_by(pohlavi) %>% 
  summarise(my_stats(bfi_ext))
  #popisné statistiky

df_limited %>% 
  rstatix::levene_test(bfi_ext ~ pohlavi)
  #rozptyly jsou spíše homogenní

t.test(bfi_ext ~ pohlavi, 
       data = df_limited,
       conf.level = .95,
       var.equal = TRUE)
  #dívky a chlapci se asi neliší (p value a CIs)

# Výsledky doplňte vhodnými popisnými statistikami a grafy
bfi_ext <- df_limited %>% 
  group_by(pohlavi) %>% 
  summarise(my_stats(bfi_ext))
bfi_ext

bfi_ext %>% 
  ggplot(aes(x = pohlavi,
             y = m)) +
  #cohort na x ose, m průměr na y ose
  geom_point(size = 5) +
  #vykreslí body výše popsaného
  geom_line(mapping = aes(group = 1)) +
  #spojí body (i díky group = 1 argumentu)
  geom_errorbar(mapping = aes(ymin = m_lo,
                              ymax = m_hi),
                width = .1)


# Také se pokuste ověřit, zda je rozdělení proměnné, kterou jste si vybrali, 
# přibližně normální u obou skupin (grafy jsou zde obvykle vhodnější 
# než testy ověřující exaktní normalitu) a zda se rozptyly moc neliší.

scaled_df <- df_limited %>% 
  select(pohlavi, bfi_ext) %>% 
  group_by(pohlavi) %>% 
  mutate(z_scores = as.double(scale(bfi_ext)))

scaled_df

scaled_df %>% 
  ggplot(aes(z_scores)) +
  geom_histogram(
    mapping = aes(y = after_stat(density)),  #histogram dat
    color = "black", fill = "grey"
  ) +
  geom_density(linewidth = 1,         #zploštělá data
               color = "blue") +
  stat_function(fun = dnorm,           #normání rozdělení
                color = "red",
                linewidth = 1,
                linetype = "11") +
  facet_wrap(~pohlavi, nrow = 2)
  #roughly je to normální pro holky, u kluků jsou zajímavé ty 2 vrcholy

# Kdo by chtěl zkusit výrazně obtížnější úlohu:
# Zkuste vytvořit grafy zobrazující průměry s intervaly spolehlivosti všech faktorů 
# Big Five (na ose Y) v závislosti na ročníku/grade/ (na ose X) a pohlaví/sex/ 
# (odlišené barevně) v grafu rozděleném do fazet podle faktorů Big Five

#nestíhám :/