library(tidyverse)

# 1) Pracujte nejprve s tímto vektorem
# A pokuste se aspoň dvěma různými způsoby vybrat z něj všechny sudé prvky
x <- c(one = 1,
       two = 2, 
       three = 3, 
       four = 4, 
       five = 5, 
       six = 6,
       seven = 7,
       eight = 8,
       nine = 9,
       ten = 10)

x[x %% 2 == 0]
x[c(2,4,6,8,10)]


# 2) Pracujte s tímto dataframem (resp. tibblem)
df <- tibble(
  name = c("Mercury", "Venus", "Earth", "Mars", 
           "Jupiter", "Saturn", "Uranus", "Neptune"),
  type = c("Terrestrial", "Terrestrial", "Terrestrial","Terrestrial", 
           "Gas giant", "Gas giant", "Gas giant", "Gas giant"),
  diameter = c(0.382, 0.949, 1, 0.532, 11.209, 9.449, 4.007, 3.883),
  rotation = c(58.64, -243.02, 1, 1.03, 0.41, 0.43, -0.72, 0.67),
  rings = c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE)
)

df

# A podle zadání vyberte příslušné řádky a/nebo sloupce
# Klidně můžete použít funkce balíčku dplyr (který patří pod tidyverse)
# (Dobrovolný, těžší úkol: zkuste to pak pouze s využitím zákldních funkcí
# R, tj. bez použití dplyr funkcí jako select() nebo filter())
# Vyberte všechny informace o Marsu
df %>% 
  filter(name == "Mars")

# Vyberte sloupec diameter
df %>% 
  select(diameter)

# Vyberte planety, které mají prstence
df %>% 
  filter(rings == TRUE)

# Vyberte sloupec typu planety, ale jen planety s prstenci
df %>% 
  filter(rings == TRUE) %>% 
  select(type)

# Konvertujte sloupec type na factor tak, aby první úroveň byla
# "Terrestial"
df %>% 
  mutate(
    type = factor(type,
                  levels = c("Terrestial", "Gas giant"))
  )

# Která planeta se otáčí opačně než Země a zároveň nemá prstence?
df %>% 
  filter(rotation < 0 & rings == FALSE) %>% 
  select(name)


# Vyberte nejmenší planetu
df %>% 
  filter(diameter == min(df$diameter)) %>% 
  select(name)

# Seřaďte planety podle velikosti (diameter)
df %>% 
  arrange(desc(diameter))

# Seřaďte planety podle toho, jestli mají prstence, a až poté podle velikosti
df %>% 
  arrange(rings, desc(diameter))


# Kolik je planet, které se otáčejí opačně než Země (záporná rotace)?
df %>% 
  filter(rotation < 0) %>% 
  count()

# Vyberte planetu s nejrychlejší rotací (bez ohledu na směr rotace)
df %>% 
  filter(rotation == max(df$rotation)) %>% 
  select(name)

# Vyberte 2 planety s nejrychlejší rotací (opět bez ohledu na směr)
df %>% 
  arrange(desc(rotation)) %>% 
  slice_head(n = 2)
