###### Baròmetre d'Opinió Política. 2a onada 2020 (CEO)
# Pckgs
library(dplyr)
library(forcats)
library(validate)

# Import
barometro <- read.csv2("input/microdades anonimitzades -974.csv",
                       encoding = "UTF-8")

# Rename variables
barometro <- barometro %>%
  rename(duration = DURADA, sex = SEXE, age = EDAT,
                                  lang_interview = LLENGUA_ENQUESTA,
                                  pob = LLOC_NAIX, party = P27,
                                  left_right_0_10 = P28, cat_independence = P34,
                                  children = P71)

# Order left-right-scale
barometro <- barometro %>%
  mutate(left_right_0_10 = ordered(
    fct_recode(fct_relevel(left_right_0_10, "0" = "Extrema esquerra"),
               NULL = "No contesta", NULL = "No ho sap")),
    cat_independence = ifelse(cat_independence == "Sí", "Si", cat_independence))

### Validate data
(cf_duration <- check_that(barometro, duration >= 25 * 60 & duration <= 30 * 60))
summary(cf_duration)

(cf_independence <- check_that(barometro, if (party == "Vox") cat_independence != "Si"))
summary(cf_independence)
head(values(cf_independence))
barometro[which(values(cf_independence) == FALSE), c("party", "cat_independence")]

(cf_location <- check_that(barometro, is.na(MUNICIPI) | (MUNICIPI ~ COMARCA)))
summary(cf_location)

rules <- validator(dur = duration >= 25 * 60 & duration <= 30 * 60,
                   party_vs_ind = if (party == "Vox") cat_independence != "Si",
                   city_vs_region = is.na(MUNICIPI) | (MUNICIPI ~ COMARCA))

(cf_all <- confront(barometro, rules))
summary(cf_all)
head(values(cf_all))


# Referencing external sources
codelist <- data.frame(
  leftside =  c("CUP", "Podemos", "ERC"),
  rightside = c("Vox", "C's", "PPC"),
  stringsAsFactors = FALSE)


(rules_ideology <- validator(if (party %in% ref$rightside) left_right_0_10 > 4))

(cf_ideo <- confront(barometro, rules_ideology)) # This will run an error!!!
errors(cf_ideo) # You need to reference your codelist
(cf_ideo <- confront(barometro, rules_ideology, ref = codelist))

summary(cf_ideo)

barometro[which(values(cf_ideo)==FALSE), c("party", "left_right_0_10")]

# Read rules from external files
(v <- validator(.file = "input/rules.txt"))
label(v)
description(v)

(cf <- confront(barometro, v, ref = codelist))
summary(cf)
plot(cf, main = "Data Validation @ BarcelonaR")


### Fin