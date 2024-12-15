library(tidyverse)

# 1) ---------------------------------------------------
# Funkce rescale_01 níže slouží k převodu proměnné na rozpětí 0–1
# Zkuste ji zobecnit a napsat funkci rescale, kde uživatel bude moci 
# specifikovat jakékoli nové rozpětí, např. 1–10.


rescale_01 <- function(x, min = 0, max = 1) {
  #lze tady ještě dopsat stopifnot ... a podmínky pro max > min, x v rozmezí apod.
  rng <- range(x, finite = TRUE) # Ignorovat nekonečné hodnoty 
  output <- (x - rng[1]) / (rng[2] - rng[1])
  new_range <- max - min
  output <- output*new_range + min
  return(output)
}

# Ukažte pak cvičné použití nové funkce např. na vektoru
1:100
rescale_01(1:100, min = 5, max = 15)

#z cvičení z hodiny:
#když převracím proměnné, tak odečítám hodnotu od x, které je vlastně
#součet nejvyšší a nejnižší hodnoty
reverse <- function(x) {
  y <- sum(range(x))
  y - x
  #POZOR, předpokládá přítomnost krajních hodnot v datech
}

reverse(2:6)

# 2) ---------------------------------------------------
# Dokončete tělo funkce summary_mdn, tak aby funkce umožňovala vložený dataset
# nejprve rozdělit do skupin (podle group_vars) a poté vypočíst dvě
# souhrnné statistiky: medián a mediánovou absolutní odchylku od mediánu

summary_mdn <- function(data, group_vars, x_var) {
  data %>%  
    group_by(across(all_of(group_vars))) %>%
    summarise(
      median = median({{x_var}}, na.rm = TRUE),
      mad = mad({{x_var}}, na.rm = TRUE)
    )
}

# Ukažte cvičně použití této funkce např. na datasetu diamonds
diamonds
?diamonds
summary_mdn(diamonds, c("cut", "color"), carat)


# 3) ---------------------------------------------------
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

plot_bar <- function(data, var1, var2) {
  data %>% 
    count(pick(c({{ var1 }}, {{ var2 }}))) %>% 
    mutate(p = n/sum(n),
           .by = {{ var1}}) %>% 
    ggplot(aes(x = {{ var1 }}, y = n, fill = {{ var2 }})) +
    geom_bar(stat = "identity", position = "stack")
}

plot_bar(diamonds, cut, color)

# Rozšířením tohoto kódu vytvořte funkci plot_bar, která bude mít stejné 
# argumenty, ale navíc nám vytvoří sloupcový graf s proměnnou var1 na 
# ose X, četnostmi p na ose Y; a sloupce budou barevně odlišeny 
# podle úrovní proměnné var2