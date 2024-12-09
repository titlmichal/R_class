library(lavaan)
library(tidyverse)

# Použijte tento dataset
df <- haven::read_sav("https://is.muni.cz/go/ol7a9l") %>% 
  select(gender, age, PSS1:PSS10)


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

# A model dvoufaktorový, kde jeden z faktorů (Zvládání stresu) sytí
# "pozitivní" položky (4, 5, 7 a 8) a druhý faktor (Nezvládání stresu) 
# "negativní" položky (1, 2, 3, 6, 9, 10)

# Porovnejte oba modely z hlediska shody s daty








