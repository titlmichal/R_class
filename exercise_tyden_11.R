library(lavaan)
library(tidyverse)

# Použijte tento dataset
df <- haven::read_sav("https://is.muni.cz/go/ol7a9l") %>% 
  select(gender, age, PSS1:PSS10)

df %>% 
  summary()

# Pro odhad CFA
my_cfa <- function(data, model, ...) {
  lavaan::cfa(data = data,
              model = model,
              estimator = "ml",
              missing = "listwise", #nejsou chybějící - stačí listwise
              ...)
}

# Pro output modelu
output <- function(fit) {
  summary(fit, std = TRUE, ci = TRUE, fit = TRUE)
}

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

# Pracujte s položkami Škály vnímaného stresu (PSS - Perceived Stress Scale)
# tj. sloupce PSS1 až PSS10

# Znění položek bylo následující.

# Jak často jste se za poslední měsíc...
# 1. ...naštval/a kvůli něčemu, co se stalo nečekaně?
# 2. ...měl/a pocit, že nejste schopen/a kontrolovat důležité věci ve vaše životě?
# 3. ...pociťoval/a nervozitu a stres? *
# 4. ...pociťoval/a jistotu ve své schopnosti, že jste schopen/a zvládat vaše osobní problémy?
# 5. ...cítil/a, že se věci vyvíjejí podle vašich představ?
# 6. ...zjistil/a, že si neumíte poradit se všemi povinnostmi, které jste měl/a udělat?
# 7. ...dokázal/a kontrolovat vaši podrážděnost?
# 8. ...měsíc měl/a pocit, že jste nad věcí?
# 9. ...rozhněval/a kvůli něčemu, co se stalo, tak, že jste se nebyl/a schopen/a ovládat?
# 10. ...cítil/a, že se na vás hrnou potíže tak moc, že je nejste schopen/a překonat?


# Pomocí balíčku lavaan odhadněte dva CFA modely
# Jednofaktorový model, kde jeden faktor sytí všechny položky

# Jména položek
items <- str_c("PSS", 
               str_pad(1:10, pad = 0, width = 1))
items
# Převrácení reverzních
reversed <- items[c(4, 5, 7, 8)]

df <- df %>% 
  mutate(
    across(
      all_of(reversed),
      ~4 - .x
    )
  )

m_1 <- "
PSS =~ PSS1 + PSS2 + PSS3 + PSS4 + PSS5 + PSS6 + PSS7 + PSS8 + PSS9 + PSS10

"

fit_1 <- my_cfa(df, m_1)
output(fit_1)
fit_1_measures <- fit_measures(fit_1)

# A model dvoufaktorový, kde jeden z faktorů (Zvládání stresu) sytí
# "pozitivní" položky (4, 5, 7 a 8) a druhý faktor (Nezvládání stresu) 
# "negativní" položky (1, 2, 3, 6, 9, 10)

m_2 <- "
PSS_poz =~ PSS4 + PSS5 + PSS7 + PSS8
PSS_neg =~ PSS1 + PSS2 + PSS3 + PSS6 + PSS9 + PSS10

"

fit_2 <- my_cfa(df, m_2)
output(fit_2)
fit_2_measures <- fit_measures(fit_2)

# Porovnejte oba modely z hlediska shody s daty
data.frame(
  Model_1 = round(unlist(fit_1_measures), 4),
  Model_2 = round(unlist(fit_2_measures), 4)
)

#celkově tedy lepší shoda pro model 2, která je smysluplná
#zejména tedy z podstaty, že komplikovanější modely většinou
#vysvětlují data lépe než méně komplikované
#každopádně m_2 fituje na všech ukazatelích lépe
#(a dokonce u některých jde o zlepšení přes hranici pravidla palce)