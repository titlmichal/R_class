library(tidyverse)

# 1) ---------------------------------------------------
# Funkce rescale_01 níže slouží k převodu proměnné na rozpětí 0–1
# Zkuste ji zobecnit a napsat funkci rescale, kde uživatel bude moci 
# specifikovat jakékoli nové rozpětí, např. 1–10.


rescale_01 <- function(x) {
  rng <- range(x, finite = TRUE) # Ignorovat nekonečné hodnoty 
  (x - rng[1]) / (rng[2] - rng[1])
}

# Ukažte pak cvičné použití nové funkce např. na vektoru
1:100

# 2) ---------------------------------------------------
# Dokončete tělo funkce summary_mdn, tak aby funkce umožňovala vložený dataset
# nejprve rozdělit do skupin (podle group_vars) a poté vypočíst dvě
# souhrnné statistiky: medián a mediánovou absolutní odchylku od mediánu

summary_mdn <- function(data, group_vars, x_var) {
  data %>%  
    group_by() %>%
    summarise()

}

# Ukažte cvičně použití této funkce např. na datasetu diamonds
diamonds
?diamonds

# 3) ---------------------------------------------------
# Takto bychom 
diamonds %>% 
  count(pick(cut, color)) %>% 
  mutate(p = n / sum(n),
         .by = cut)

# Takto bychom vytvořili funkci, která vezme dataframe, vypočte 
# četnosti jednotlivých kategorií proměnných var1 a var2
# a nakonec vypočte relativní četnosti proměnné var2 v rámci každé 
# kategorie var1
count_prop <- function(data, var1, var2) {
  data %>% 
    count(pick(c({{ var1 }},  {{ var2 }}))) %>% 
    mutate(p = n / sum(n),
           .by = {{ var1 }})
}

count_prop(diamonds, cut, color)

# Rozšířením tohoto kódu vytvořte funkci plot_bar, která bude mít stejné 
# argumenty, ale navíc nám vytvoří sloupcový graf s proměnnou var1 na 
# ose X, četnostmi p na ose Y; a sloupce budou barevně odlišeny 
# podle úrovní proměnné var2




