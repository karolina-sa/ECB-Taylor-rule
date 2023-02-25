sl <- locale("sl", decimal_mark=",", grouping_mark=".")
library(readr)
library(dplyr)
library(tibble)
library(tidyverse)
library(readxl)

# DRŽAVE
country_vec <- c("Austria", "Belgium", "Cyprus", "Estonia", "Finland", "France", 
                 "Germany", "Greece", "Ireland", "Italy", "Latvia", "Lithuania", 
                 "Luxembourg", "Malta", "Netherlands", "Portugal", "Slovakia", 
                 "Slovenia", "Spain")

# VEKTOR ČETRTLETIJ
cetrtletje <- function(mesec, leto) {
  ifelse(mesec %in% c(1, 2, 3), paste0('Q1 ', leto),
         ifelse(mesec %in% c(4, 5, 6), paste0('Q2 ', leto),
                ifelse(mesec %in% c(7, 8, 9), paste0('Q3 ', leto), paste0('Q4 ', leto))))
}

# čas od 2001 do 2019
cas_obdobje <- c()
for (l in 2001 : 2019) {
  cas_obdobje <- append(cas_obdobje, c(paste0('Q1 ', l), paste0('Q2 ', l), paste0('Q3 ', l), paste0('Q4 ', l)))
}

# ==============================================================================
# KEY INTEREST RATES
key.interest.rates <- read_csv("interest_rate.csv",
                         skip = 4,
                         locale = locale(encoding = "Windows-1250"),
                         col_names = TRUE, 
                         col_types = cols(.default = col_guess()))
key.interest.rates <- subset(key.interest.rates, select = c(1, 13, 14))
colnames(key.interest.rates) <- c("Time", "I", "II")
key.interest.rates$"Interest rate" = rowSums( cbind (key.interest.rates$I, key.interest.rates$II), na.rm=TRUE)
key.interest.rates <- subset(key.interest.rates, select = c(1, 4))
key.interest.rates <- key.interest.rates %>%
  mutate(
    month = str_extract(Time, "(?<=\\d{4}-)\\d{2}"),
    year = str_extract(Time, "\\d{4}"),
    quarter = cetrtletje(as.numeric(month), as.numeric(year)),
    Time = quarter
  )
key.interest.rates <- subset(key.interest.rates, select = c(1, 2))
key.interest.rates <- key.interest.rates[key.interest.rates$Time %in% cas_obdobje, ]


# ==============================================================================
# POTENTIAL GDP
potential.gdp.osnova <- read_csv("potential_GDP.csv",
                          locale = locale(encoding = "Windows-1250"),
                          col_names = TRUE, 
                          col_types = cols(.default = col_guess()))
potential.gdp.osnova <- subset(potential.gdp.osnova, select = c(2, 7, 15))
colnames(potential.gdp.osnova) <- c("Country", "Time", "Potential GDP")
potential.gdp.osnova <- potential.gdp.osnova %>%
  mutate(Country = ifelse(Country == "Slovak Republic", "Slovakia", Country))

potential.gdp <- data.frame("Country" = c(1),
                            "Time" = c(1),
                            "PotGDP" = c(1))
country <- potential.gdp.osnova[1,1]
i <- 1
for (row in 1:(nrow(potential.gdp.osnova)-1)) {
  current_country <- as.character(potential.gdp.osnova[row, 1])
  next_country <- as.character(potential.gdp.osnova[row + 1, 1])
  year <- as.numeric(potential.gdp.osnova[row, 2])
  current_val <- as.numeric(potential.gdp.osnova[row, 3])
  next_val <- as.numeric(potential.gdp.osnova[row + 1, 3])
  if (current_country == country) {
    step <- (next_val - current_val) / 4
    potential.gdp[i,] <- c(country, paste0('Q1 ', year), current_val)
    potential.gdp[i + 1,] <- c(country, paste0('Q2 ', year), current_val + 1 * step)
    potential.gdp[i + 2,] <- c(country, paste0('Q3 ', year), current_val + 2 * step)
    potential.gdp[i + 3,] <- c(country, paste0('Q4 ', year), current_val + 3 * step)
    i <- i + 4
  }
  if (current_country != next_country) {
    country <- next_country
  }
}
potential.gdp <- potential.gdp[potential.gdp$Country %in% country_vec, ]
potential.gdp <- potential.gdp[potential.gdp$Time %in% cas_obdobje, ]

potential.gdp$"Year" <- potential.gdp$Time
potential.gdp <- potential.gdp %>%
  mutate(Year = str_replace_all(Year, "(Q\\d{1}) (\\d{4})", "\\2")) %>%
  mutate(PotGDP = ifelse(Year < 2007 & Country == "Slovenia", NA, PotGDP)) %>%
  mutate(PotGDP = ifelse(Year < 2008 & Country == "Cyprus", NA, PotGDP)) %>%
  mutate(PotGDP = ifelse(Year < 2008 & Country == "Malta", NA, PotGDP)) %>%
  mutate(PotGDP = ifelse(Year < 2009 & Country == "Slovakia", NA, PotGDP)) %>%
  mutate(PotGDP = ifelse(Year < 2011 & Country == "Estonia", NA, PotGDP)) %>%
  mutate(PotGDP = ifelse(Year < 2014 & Country == "Latvia", NA, PotGDP)) %>%
  mutate(PotGDP = ifelse(Year < 2015 & Country == "Lithuania", NA, PotGDP))
potential.gdp <- subset(potential.gdp, select = c(1, 2, 3))


# ==============================================================================
# GDP:
GDP <- read_excel("GDP.xlsx")
colnames(GDP) <- GDP[9,]
GDP <- GDP[18:54, -which(is.na(GDP[9,]))]
GDP <- GDP %>%
  pivot_longer(
    cols = colnames(GDP)[-c(1)],
    names_to = "Time",
    values_to = "GDP"
  ) %>%
  mutate(Time = str_replace_all(Time, "(\\d{4})-(Q\\d{1})", "\\2 \\1"))
colnames(GDP) <- c("Country", "Time", "GDP")
GDP <- GDP[GDP$Country %in% country_vec, ]
GDP <- GDP[GDP$Time %in% cas_obdobje, ]

GDP$"Year" <- GDP$Time
GDP <- GDP %>%
  mutate(Year = str_replace_all(Year, "(Q\\d{1}) (\\d{4})", "\\2")) %>%
  mutate(GDP = ifelse(Year < 2007 & Country == "Slovenia", NA, GDP)) %>%
  mutate(GDP = ifelse(Year < 2008 & Country == "Cyprus", NA, GDP)) %>%
  mutate(GDP = ifelse(Year < 2008 & Country == "Malta", NA, GDP)) %>%
  mutate(GDP = ifelse(Year < 2009 & Country == "Slovakia", NA, GDP)) %>%
  mutate(GDP = ifelse(Year < 2011 & Country == "Estonia", NA, GDP)) %>%
  mutate(GDP = ifelse(Year < 2014 & Country == "Latvia", NA, GDP)) %>%
  mutate(GDP = ifelse(Year < 2015 & Country == "Lithuania", NA, GDP))
GDP <- subset(GDP, select = c(1, 2, 3))


# ==============================================================================
# INFLATION
inflation <- read_excel("inflation.xlsx")
colnames(inflation) <- inflation[8,]
inflation <- inflation[16:51, -which(is.na(inflation[8,]))]
inflation <- inflation %>%
  pivot_longer(
    cols = colnames(inflation)[-c(1)],
    names_to = "Time",
    values_to = "inflation"
  )
colnames(inflation) <- c("Country", "Time", "Inflation")
inflation <- inflation[inflation$Country %in% country_vec, ]
inflation <- inflation[order(inflation$Country, inflation$Time), ]

# VERIŽNI INDEKSI:
vec_for_country <- list()
final_vec <- list()
country <- as.character(inflation[1,1])
for (row in 1:nrow(inflation)) {
  c_country <- as.character(inflation[row,1])
  if (c_country == country) {
    vec_for_country <- append(vec_for_country, as.numeric(inflation[row, 3]))
  }
  if (c_country != country) {
    verizni_indeksi <- rep(NA, length(vec_for_country))
    verizni_indeksi[1] <- 100
    for (i in 2:length(vec_for_country)) {
      verizni_indeksi[i] <- as.numeric(verizni_indeksi[i - 1]) * (1 + as.numeric(vec_for_country[i]) / 100)
    }
    final_vec <- append(final_vec, verizni_indeksi)
    vec_for_country <- list(as.numeric(inflation[row, 3]))
    country <- c_country
  }
}
verizni_indeksi <- rep(NA, length(vec_for_country))
verizni_indeksi[1] <- 100
for (i in 2:length(vec_for_country)) {
  verizni_indeksi[i] <- as.numeric(verizni_indeksi[i - 1]) * (1 + as.numeric(vec_for_country[i]) / 100)
}
final_vec <- append(final_vec, verizni_indeksi)

inflation$"Verizni indeksi" <- final_vec

inflation <- inflation %>%
  mutate(
    month = str_extract(Time, "(?<=\\d{4}-)\\d{2}"),
    year = str_extract(Time, "\\d{4}"),
    cetrtletje = cetrtletje(as.numeric(month), as.numeric(year))
  )
inflation <- subset(inflation, select = c(1, 2, 3, 4, 7))

# INDEKSI ČETRTLETIJ:
inflation.cetrtletja <- data.frame("Country" = c(1),
                            "Time" = c(1),
                            "Verizni indeks" = c(1))
i <- 1
for (row in seq(1, (nrow(inflation) - 3), by=3)) {
  inflation.cetrtletja[i,] <- c(as.character(inflation[row,1]), 
                                as.character(inflation[row,5]), 
                                as.numeric(inflation$`Verizni indeksi`[row]) * as.numeric(inflation$`Verizni indeksi`[row+1]) * as.numeric(inflation$`Verizni indeksi`[row+2]))
  i <- i + 1
  }
inflation.cetrtletja <- inflation.cetrtletja[inflation.cetrtletja$Time %in% append("Q4 2000", cas_obdobje), ]

# IZRAČUN INFLACIJE (RAST GLEDE NA ČETRTLETJE):
vec_for_country <- list(NA)
final_vec <- list()
country <- as.character(inflation.cetrtletja[1,1])
for (row in 2 : nrow(inflation.cetrtletja)) {
  c_country <- as.character(inflation.cetrtletja[row,1])
  if (c_country == country) {
    vec_for_country <- append(vec_for_country, (as.numeric(inflation.cetrtletja[row, 3]) / as.numeric(inflation.cetrtletja[row-1, 3])) - 1)
  }
  if (c_country != country) {
    final_vec <- append(final_vec, vec_for_country)
    vec_for_country <- list(NA)
    country <- c_country
  }
}
final_vec <- append(final_vec, vec_for_country)
inflation.cetrtletja$"Inflation_rate" <- as.numeric(final_vec)

inflation.cetrtletja$"Year" <- inflation.cetrtletja$Time
inflation.cetrtletja <- inflation.cetrtletja %>%
  mutate(Year = str_replace_all(Year, "(Q\\d{1}) (\\d{4})", "\\2")) %>%
  mutate(Inflation_rate = ifelse(Year < 2007 & Country == "Slovenia", NA, Inflation_rate)) %>%
  mutate(Inflation_rate = ifelse(Year < 2008 & Country == "Cyprus", NA, Inflation_rate)) %>%
  mutate(Inflation_rate = ifelse(Year < 2008 & Country == "Malta", NA, Inflation_rate)) %>%
  mutate(Inflation_rate = ifelse(Year < 2009 & Country == "Slovakia", NA, Inflation_rate)) %>%
  mutate(Inflation_rate = ifelse(Year < 2011 & Country == "Estonia", NA, Inflation_rate)) %>%
  mutate(Inflation_rate = ifelse(Year < 2014 & Country == "Latvia", NA, Inflation_rate)) %>%
  mutate(Inflation_rate = ifelse(Year < 2015 & Country == "Lithuania", NA, Inflation_rate))
inflation.cetrtletja <- subset(inflation.cetrtletja, select = c(1, 2, 3, 4))


# ==============================================================================
# ZDRUŽITEV (!!!! TRENUTNO SAMO INFLACIJA, GDP IN GDP* !!!)
pod <- full_join(GDP, potential.gdp, by = c("Time", "Country"))
podatki <- full_join(pod, subset(inflation.cetrtletja, select = c(1, 2, 4)), by = c("Time", "Country"))
colnames(podatki) <- c("Country", "Time", "GDP", "Potential_GDP" , "Key_inflation_rate")


