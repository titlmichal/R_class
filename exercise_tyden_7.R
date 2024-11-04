library(psych)
library(tidyverse)
library(GGally)
library(qgraph)

# Načtení dat
df <- haven::read_sav("https://is.muni.cz/go/j6bdrc")
df <- df %>% 
  select(id:neshody)

df
glimpse(df)

df %>% 
  distinct(neshody)

# Extracting metadata
metadata <- tibble(
  var_names = names(df[-1]),
  var_labels = map_chr(df[-1], 
                       ~attributes(.)$label)
)

metadata %>% 
  print(n = Inf)

# Vyberte si jakékoli dvě kvantativní proměnné z datasetu datasetu importovaného
# výše, u kterých si myslíte, že by mezi nimi měl být vztah.
# Prozkoumejte, zda jsou tyto proměnné přibližně normálně rozděleny pomocí
# histogramu (nejlépe doplněného i křivkou normálního rozdělení) a 
# q-q grafu. Prozkoumejte vztah mezi nimi a jeho linearitu pomocí scatterplotu.
# Otestujte tento vztah pomocí vhodného korelačního koeficientu.
vis_df <- df %>% 
  select(duv_r, neshody) %>% 
  pivot_longer(everything(),
               names_to = "var_name") %>% 
  mutate(value = as.double(scale(value)),
         .by = var_name)

#histogram - ověření normálního rozdělení
vis_df %>% 
  ggplot(aes(value)) +
  geom_histogram(aes(y = after_stat(density)),
                 binwidth = .3) +
  facet_wrap(~var_name) +
  stat_function(fun = dnorm,
                color = "red",
                linewidth = 2)

#qq plot
vis_df %>% 
  ggplot(aes(sample = value)) +
  geom_qq_line() +
  geom_qq() +
  facet_wrap(~var_name)

#scatter plot
df %>% 
  ggplot(aes(duv_r, neshody)) +
  geom_jitter(height = .25, width = .25) +
  geom_smooth(se = FALSE,
              color = "blue",
              linewidth = 2) +
  geom_smooth(method = "lm", 
              level = .95,
              color = "red",
              fill = "red",
              alpha = .1,
              linetype = "31",
              linewidth = 1)

#korelace
cor_df <- df %>% 
  select(duv_r, neshody)
cor_df

psych::corr.test(cor_df)
psych::corr.test(cor_df, 
                 adjust = "holm",
                 method = "pearson",
                 alpha = .05) %>%
  print(short = FALSE)

cor.test(cor_df$duv_r, cor_df$neshody,
         method = "pearson",
         conf.level = .95,
         use = "pairwise")
