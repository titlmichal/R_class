library(WebPower)
library(tidyverse)

# Předpokládejme, že chceme odhadnout lineárně regresní model s 5 prediktory
# Očekáváme, že mohou vysvětlovat 5 % (pesimistický odhad), 
# 10% (realistický odhad) nebo 15% (optimistický odhad) rozptylu v populaci.

# Uvažujeme o tom, že bychom mohli pracovat s hladinou alfa = 0.05 a možná i 
# 0.01

# A že můžeme získat vzorek o velikosti 30 až 1000 osob.

# Vypočtěte statistickou sílu pro všechny kombinace uvedených vstupních
# parametrů a výsledky znázorněte pomocí spojnicového grafu s velikostí 
# vzorku na ose X, statistickou silou na ose Y. Barevně odlište různé 
# velikosti účinku (podíl vysvětleného rozptylu na úrovni populace) a 
# rozdělte graf do dvou fazet podle hladiny alfa.

#fce() na Cohenovo f
r2_to_f <- function(r2_full, r2_null = 0) {
  
  out <- (r2_full - r2_null) / (1 - r2_full)
}


#vytvoření tibblu kombinací n, rozptylu a p-values
input <- expand.grid(
  n = seq(30, 1000, by = 1),
  p1 = 5,
  f2 = r2_to_f(seq(0.05, 0.15, by = 0.05)),
  alpha = c(0.05, 0.01)
) %>% 
  as_tibble()
  #proměnné definuji, aby seděly do wp.regression()

#test na regresi
output <- input %>% 
  pmap(wp.regression)
  #pmap() provede fci na všechny řádky

#převod listu na tibble
output <- output %>% 
  map_df(~.x[c("n", "p1", "p2", "f2", "alpha", "power")])
  #map_df musí mít definované řádky k převodu

#vizualizace powertestu
output %>% 
  ggplot(aes(x = n,      #estetics
             y = power,
             color = factor(round(f2, 2)))) +
  geom_line() +         #spojovací čára
  facet_wrap(~alpha) +  #rozdělení alfou
  geom_hline(aes(yintercept = .8), linetype = "dashed") +  #treshold s labelem
  annotate("text", x = 100, y = 0.82, label = "80% treshold") +
  labs(x = "n participants",
       y = "statistical power",
       color = "f2 effect size") +  #labely x, y a faktorů
  scale_x_continuous(breaks = seq(50, 1000, by = 100)) + #rozdělení X na vícero
  theme_minimal()  #jiný vizuál grafu