# Introduction ------------------------------------------------------------
#
# 2018-02-14: The data supplied by AP were inconsistent with the database
#             entries. This was due in part to the fact that some isolates had
#             their identifiers changed in the Steadman lab database and others
#             had perished after genotyping and were discarded. We now have two
#             data sets that we will use to update the data to use for the 
#             analysis.
#
# Setup -------------------------------------------------------------------


library("poppr")
library("tidyverse")
library("readxl")
library("lubridate")
if (!interactive()) options(width = 200)
enc <- getOption("encoding")
options(encoding = "iso-8859-1")


# Merging Data ------------------------------------------------------------

genotypes <- read.genalex(here::here("data/data.csv"), ploidy = 1) %>% 
  genind2df() %>%
  rownames_to_column("AP-GenoID") %>%
  as_tibble()
genotypes

# reading in the excel sheet has its own problems since the date column contains
# part dates and part text and they get screwed up no matter what you do. The
# way I've dealt with this: import as dates and then convert what didn't parse
# into the number of days since 1899-12-30
metadata  <- read_excel(here::here("data/MasterGenoMCGDataBrazilPaper2018.xlsx"), 
                        col_types = "text",
                        na = c("NA", "")) %>%
  mutate(date = as.Date(parse_date_time(`JRS-Collection Date`, c("mdy", "y")))) %>%
  mutate(date = case_when(
    is.na(date) ~ as.Date("1899-12-30") + days(as.integer(`JRS-Collection Date`)),
    TRUE        ~ date
  ))
metadata
full_data <- left_join(metadata, genotypes, by = "AP-GenoID")


# Data Cleaning -----------------------------------------------------------
# 
# Because there are discrepancies between the locations, we will rely on the
# location information from the JRS database. Unfortunately, there is no 
# consistent pattern in naming, so will will manually create a table from the
# data and use that to match genotypes. Those without region names will have the
# country or state name in place.


full_data %>% 
  select(`JRS-Geographical Location`, pop) %>% 
  distinct() %>% 
  mutate(reglen = max(nchar(`JRS-Geographical Location`), na.rm = TRUE) - nchar(`JRS-Geographical Location`)) %>% 
  filter(!is.na(`JRS-Geographical Location`)) %>%
  rowwise() %>% 
  mutate(trail = paste(rep(" ", reglen), collapse = "")) %>% 
  glue::glue_data("'{`JRS-Geographical Location`}'{trail} ~ '{pop}',")

full_data <- mutate(full_data, 
  continent_country_state_region = case_when(
  # Because there is only one point from CO in the data, we will compress both
  # CO and NE into a single region called "Midwest"
  `JRS-Geographical Location` == 'Greeley, CO'                            ~ 'North America_United States_Midwest_Greeley, CO',
  `JRS-Geographical Location` == 'Ithaca, NE'                             ~ 'North America_United States_Midwest_Ithaca, NE',
  `JRS-Geographical Location` == 'Platte Co., NE'                         ~ 'North America_United States_Midwest_Platte Co., NE',
  `JRS-Geographical Location` == 'Tekamah, Burt Co., NE'                  ~ 'North America_United States_Midwest_Tekamah, NE',
  `JRS-Geographical Location` == 'Saunders Co., NE'                       ~ 'North America_United States_Midwest_Saunders Co., NE',
  `JRS-Geographical Location` == 'Bellwood, NE'                           ~ 'North America_United States_Midwest_Bellwood, NE',
  `JRS-Geographical Location` == 'Herman, NE'                             ~ 'North America_United States_Midwest_Herman, NE',
  `JRS-Geographical Location` == 'Ord, NE'                                ~ 'North America_United States_Midwest_Ord, NE',
  `JRS-Geographical Location` == 'UNL PN, Lincoln, NE'                    ~ 'North America_United States_Midwest_Lincoln, NE',
  `JRS-Geographical Location` == 'Mead, Nebraska'                         ~ 'North America_United States_Midwest_Mead, NE',
  `JRS-Geographical Location` == 'Ewing, NE'                              ~ 'North America_United States_Midwest_Ewing, NE',
  `JRS-Geographical Location` == 'Auburn, NE'                             ~ 'North America_United States_Midwest_Auburn, NE',
  `JRS-Geographical Location` == 'Argentina'                              ~ 'South America_Argentina_Argentina_Argentina',
  `JRS-Geographical Location` == 'Rio Verde/GO, Brazil'                   ~ 'South America_Brazil_Goiás_Rio Verde',
  `JRS-Geographical Location` == 'Campo Mourão/PR, Brazil'                ~ 'South America_Brazil_Paraná_Campo Mourão',
  `JRS-Geographical Location` == 'São Miguel do Passo Quatro/GO'          ~ 'South America_Brazil_Goiás_São Miguel do Passo',
  `JRS-Geographical Location` == 'Pinhão/PR, Brazil'                      ~ 'South America_Brazil_Paraná_Pinhão',  # Note: in Anthony's data, one isolate labeled as South America_Brazil_Goiás
  `JRS-Geographical Location` == 'Formoso, GO, Brazil'                    ~ 'South America_Brazil_Goiás_Formoso',
  `JRS-Geographical Location` == 'Guarapuava, PR, Brazil'                 ~ 'South America_Brazil_Paraná_Guarapuava',
  `JRS-Geographical Location` == 'Luiz Eduardo Magalhães/BA, Brazil'      ~ 'South America_Brazil_Bahia_Bahia',
  `JRS-Geographical Location` == 'Mauá da Serra/PR, Brazil'               ~ 'South America_Brazil_Paraná_Mauá da Serra',
  `JRS-Geographical Location` == 'Nᾶo me Toque/Rio Grande do Sul, Brazil' ~ 'South America_Brazil_Rio Grande do Sul_Não me Toque',
  `JRS-Geographical Location` == 'Sᾶo Desidério/Bahia, Brazil'            ~ 'South America_Brazil_Bahia_São Desidério',
  `JRS-Geographical Location` == 'Jataí/GO, Brazil'                       ~ 'South America_Brazil_Goiás_Jataí',
  `JRS-Geographical Location` == 'Cristalina/GO, Brazil'                  ~ 'South America_Brazil_Goiás_Cristalina',
  `JRS-Geographical Location` == 'Formosa/GO, Brazil'                     ~ 'South America_Brazil_Goiás_Formosa',
  `JRS-Geographical Location` == 'Sudeste/GO, Brazil'                     ~ 'South America_Brazil_Goiás_Sudeste',
  `JRS-Geographical Location` == 'Uberlândia/MG, Brazil'                  ~ 'South America_Brazil_Minas Gerais_Uberlândia',
  `JRS-Geographical Location` == 'Correntina/BA, Brazil'                  ~ 'South America_Brazil_Bahia_Correntina',
  `JRS-Geographical Location` == 'Bahia, Brazil'                          ~ 'South America_Brazil_Bahia_Bahia',
  `JRS-Geographical Location` == 'Vacaria/RS, Brazil'                     ~ 'South America_Brazil_Rio Grande do Sul_Vacaria',
  `JRS-Geographical Location` == 'Coxilha/RS, Brazil'                     ~ 'South America_Brazil_Rio Grande do Sul_Coxilha',
  `JRS-Geographical Location` == 'Faxinal/PR, Brazil'                     ~ 'South America_Brazil_Paraná_Faxinal',
  `JRS-Geographical Location` == 'Chapadão do Sul/MS, Brazil'             ~ 'South America_Brazil_Mato Grosso do Sul_Chapadão do Sul',
                             TRUE                                         ~ `JRS-Geographical Location`
))


# Saving Cleaned Data as CSV -----------------------------------------------
# 
# Now that the data are cleaned, I will save the important bits for reproduction
# as both a CSV and a genclone object. 

clean_data <- full_data %>% 
  select(GenoID = `AP-GenoID`, 
         MCG, 
         Year = date,
         continent_country_state_region, 
         matches("\\d-\\d")) %>%
  mutate(Year = year(Year)) %>%
  filter(!is.na(continent_country_state_region)) %>% # remove isolate that has no info
  separate(continent_country_state_region, 
           c("Continent", "Country", "Population", "Subpop"),
           sep = "_") %>%
  write_csv(path = here::here("data/clean-genotypes.csv"))

# Head off any encoding issues
readLines(here::here("data/clean-genotypes.csv")) %>% 
  iconv(from = "UTF-8", to = "ISO-8859-1") %>%
  writeLines(con = here::here("data/clean-genotypes.csv"))

print(clean_data, n = 100)


# Converting to genclone, adding Repeat Lengths, Palette ------------------
gid <- df2genind(select(clean_data, matches("\\d-\\d")),
                 ploidy = 1,
                 ind.names = clean_data$GenoID,
                 strata = select(clean_data, 
                                 GenoID, Continent, Country, Population, Subpop, MCG, Year)) %>%
  as.genclone() %>%
  setPop(~Population)

# This is a color-blind friendly palette
other(gid)$palette <- c("Midwest" = "#000000",
                        "Argentina" = "#F0E442", # "#E69F00",
                        "Bahia" = "#56B4E9",
                        "Goiás" = "#009E73",
                        "Mato Grosso do Sul" = "#E69F00",
                        "Minas Gerais" = "#0072B2",
                        "Paraná" = "#D55E00",
                        "Rio Grande do Sul" = "#CC79A7")

# These are the repeat lengths that we are correcting to avoid rounding errors
(other(gid)$REPLEN <- fix_replen(gid, c(2, 6, 2, 2, 2, 2, 4, 4, 4, 4, 3)))

write_rds(gid, path = here::here("data/full-genclone-object.rds"))

# Session Information -----------------------------------------------------


sessioninfo::session_info()
options(encoding = enc)
