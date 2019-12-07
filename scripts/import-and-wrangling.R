# ----------------------------------------------------------------------------
#
# IMPORTING AND WRANGLING THE FULL DATASET
# 2017 1-YEAR ACS PUMS DATA
# RETRIEVED FROM https://www.census.gov/programs-surveys/acs/data/pums.html
#
# NOTE: Only run this script in an empty environment, since it will clear
# everything once done
#
# ----------------------------------------------------------------------------

# Loading required packages
library(data.table)
library(dplyr)
library(forcats)

# Reading in the population records
ACSPersonRaw1 <- fread("data/raw/csv_pus/psam_pusa.csv")
ACSPersonRaw2 <- fread("data/raw/csv_pus/psam_pusb.csv")
ACSPersonRaw <- rbindlist(list(ACSPersonRaw1, ACSPersonRaw2))

# Reading in the population records
ACSHousingRaw1 <- fread("data/raw/csv_hus/psam_husa.csv")
ACSHousingRaw2 <- fread("data/raw/csv_hus/psam_husb.csv")
ACSHousingRaw <- rbindlist(list(ACSHousingRaw1, ACSHousingRaw2))

# Selecting the variables of interest from the population records
ACSPerson <- ACSPersonRaw %>%
  select(SEX, AGEP, CIT, RAC1P, MIL, DIS, # general demographics
         MAR, # family and household
         SCHL, FOD1P, FOD2P, SCIENGP, # educational background
         ESR, WKHP, NAICSP, # employment
         WAGP, ADJINC, # income
         REGION, ST, # location
         SERIALNO) # merging key

# Selecting the variables of interest from the housing records
ACSHousing <- ACSHousingRaw %>%
  select(NP, NRC, # family and household
         SERIALNO) # merging key

# Merging the datasets by SERIALNO
ACSRaw <- merge(ACSPerson, ACSHousing, by = "SERIALNO")

# -----------------------------------------------------------------------------

# Setting factor levels
sex_levels <- c(M = "1",
                "F" = "2")

# will only be used for tallying
race_levels <- c(White = "1",
                 Black = "2",
                 Native = "3",
                 Native = "4",
                 Native = "5",
                 Asian = "6",
                 Native = "7",
                 Other = "8",
                 Multiple = "9")

# will only be used for tallying
marital_status_levels <- c(Married = "1",
                           Widowed = "2",
                           Divorced = "3",
                           Separated = "4",
                           Single = "5")

# collapsing degree
degree_levels <- c("Agriculture" = "11",
                   "Environmental" = "13",
                   "Architecture" = "14",
                   "Ethnic Studies" = "15",
                   "Media and Journalism" = "19",
                   "Communication" = "20",
                   "Computer Science and IT" = "21",
                   "Cosmetology and Gastronomy" = "22",
                   "Education" = "23",
                   "Engineering" = "24",
                   "Engineering" = "25",
                   "Languages" = "26",
                   "Consumer Sciences" = "29",
                   "Law and Policy" = "32",
                   "English Literature" = "33",
                   "Liberal Arts" = "34",
                   "Library Science" = "35",
                   "Biological Sciences" = "36",
                   "Mathematics and Statistics" = "37",
                   "Military" = "38",
                   "Interdisciplinary" = "40",
                   "Fitness" = "41",
                   "Philosophy" = "48",
                   "Theology" = "49",
                   "Physical Sciences" = "50",
                   "Physical Sciences" = "51",
                   "Psychology" = "52",
                   "Law and Policy" = "53",
                   "Law and Policy" = "54",
                   "Social Sciences" = "55",
                   "Construction" = "56",
                   "Construction" = "57",
                   "Transportation" = "59",
                   "Arts" = "60",
                   "Medicine" = "61",
                   "Business and Finance" = "62",
                   "History" = "64")

# will be used for filtering
employment_levels <- c("Employed working" = "1",
                       "Employed not working" = "2",
                       Unemployed = "3",
                       "Military working" = "4",
                       "Military not working" = "5",
                       "Not in labor force" = "6")

region_levels <- c(Northeast = "1",
                   Midwest = "2",
                   South = "3",
                   West = "4")

state_levels <- c(AL = "1",
                  AK = "2",
                  AZ = "4",
                  AR = "5",
                  CA = "6",
                  CO = "8",
                  CT = "9",
                  DE = "10",
                  DC = "11",
                  FL = "12",
                  GA = "13",
                  HI = "15",
                  ID = "16",
                  IL = "17",
                  IN = "18",
                  IA = "19",
                  KS = "20",
                  KY = "21",
                  LA = "22",
                  ME = "23",
                  MD = "24",
                  MA = "25",
                  MI = "26",
                  MN = "27",
                  MS = "28",
                  MO = "29",
                  MT = "30",
                  NE = "31",
                  NV = "32",
                  NH = "33",
                  NJ = "34",
                  NM = "35",
                  NY = "36",
                  NC = "37",
                  ND = "38",
                  OH = "39",
                  OK = "40",
                  OR = "41",
                  PA = "42",
                  RI = "44",
                  SC = "45",
                  SD = "46",
                  TN = "47",
                  TX = "48",
                  UT = "49",
                  VT = "50",
                  VA = "51",
                  WA = "53",
                  WV = "54",
                  WI = "55",
                  WY = "56")

# collapsing industry
industry_levels <- c("Agriculture, Forestry, Fishing and Hunting" = "11",
                     "Mining, Quarrying, and Oil and Gas Extraction" = "21",
                     "Utilities" = "22",
                     "Construction" = "23",
                     "Manufacturing" = "31",
                     "Manufacturing" = "32",
                     "Manufacturing" = "33",
                     "Manufacturing" = "3M",
                     "Wholesale Trade" = "42",
                     "Retail Trade" = "44",
                     "Retail Trade" = "45",
                     "Transportation and Warehousing" = "48",
                     "Transportation and Warehousing" = "49",
                     "Information" = "51",
                     "Finance and Insurance" = "52",
                     "Real Estate and Rental and Leasing" = "53",
                     "Professional, Scientific, and Technical Services" = "54",
                     "Management of Companies and Enterprises" = "55",
                     "Administrative and Support and Waste Management 
                     and Remediation Services" = "56",
                     "Educational Services" = "61",
                     "Health Care and Social Assistance" = "62",
                     "Arts, Entertainment, and Recreation" = "71",
                     "Accommodation and Food Services" = "72",
                     "Other Services (except Public Administration)" = "81",
                     "Public Administration" = "92")

# Tidying up the data
ACS <- ACSRaw %>%
  
  # Adjusting income
  mutate(ADJINC = ADJINC / 10^6, # adding decimal point to ADJINC
         WAGP = WAGP * ADJINC, # adjusting dollar amounts for inflation
         WAGP = round(WAGP)) %>% # rounding to nearest dollar
  
  # Renaming the variables
  rename(sex = SEX,
         age = AGEP,
         citizenship = CIT,
         race = RAC1P,
         military = MIL,
         disabled = DIS,
         people_in_household = NP,
         marital_status = MAR,
         children = NRC,
         education = SCHL,
         degree_1 = FOD1P,
         degree_2 = FOD2P,
         stem_degree = SCIENGP,
         employment = ESR,
         hours_per_week = WKHP,
         industry = NAICSP,
         wage_income = WAGP,
         region = REGION,
         state = ST) %>%
  
  # Converting entries to the appropriate data types
  mutate(sex = as.factor(sex) %>%
           forcats::fct_recode(!!!sex_levels),
         
         employment = as.factor(employment) %>%
           forcats::fct_recode(!!!employment_levels),
         
         race = as.factor(race) %>%
           forcats::fct_recode(!!!race_levels),
         
         marital_status = as.factor(marital_status) %>%
           forcats::fct_recode(!!!marital_status_levels),
         
         region = as.factor(region) %>%
           forcats::fct_recode(!!!region_levels),
         
         state = as.factor(state) %>%
           forcats::fct_recode(!!!state_levels),
         
         # for citizenship, 5 stands for not a citizen
         citizenship = ifelse(citizenship == 5, "No", "Yes") %>% 
           as.factor(),
         
         privilege = ifelse(race %in% c("White", "Asian"), "Yes", "No") %>%
           as.factor(),
         
         # for military, 4 stands for never enlisted
         military = ifelse(military == 4, "No", "Yes"),
         military = ifelse(is.na(military), "No", military) %>%
           as.factor(),
         
         ever_married = ifelse(marital_status == "Single", "No", "Yes") %>% 
           as.factor(),
         
         # for education, 22, 23, and 24 stand for graduate degrees
         grad_degree = ifelse(education %in% c(22, 23, 24), "Yes", "No"),
         grad_degree = ifelse(is.na(grad_degree), "No", grad_degree) %>%
           as.factor(),
         
         # for STEMdegree, 1 stands for a STEM degree
         stem_degree = ifelse(stem_degree == 1, "Yes", "No"),
         stem_degree = ifelse(is.na(stem_degree), "No", stem_degree) %>%
           as.factor(),
         
         disabled = ifelse(disabled == 1, "Yes", "No") %>% 
           as.factor(),
         
         # Filling empty industry fields with NAs
         industry = ifelse(industry == "", NA, industry),
         # Collapsing industry codes into broad NAICS sectors (only taking the first 
         # two digits of the NAICS code, which represent the broader industry sectors)
         industry = substr(industry, start = 1, stop = 2) %>%
           as.factor() %>%
           fct_recode(!!!industry_levels),
         
         # Collapsing education codes into broader fields (only taking the first 
         # two digits of each code, which represent the broader field)
         degree_1 = substr(degree_1, start = 1, stop = 2) %>%
           as.factor() %>%
           fct_recode(!!!degree_levels),
         degree_2 = substr(degree_2, start = 1, stop = 2) %>%
           as.factor() %>%
           fct_recode(!!!degree_levels),
         # Merging the two degree variables, taking care of NAs and repeated values
         degree = ifelse(is.na(degree_1) | is.na(degree_2),
                         as.character(degree_1),
                         ifelse(as.character(degree_1) == as.character(degree_2),
                                as.character(degree_1),
                                paste(degree_1, degree_2, sep = " and ")))) %>%
  
  # Filtering the individuals of interest
  filter(!(is.na(wage_income)) & wage_income > 0, # salary income is positive
         employment %in% c("Employed working",
                           "Employed not working",
                           "Military working"), # employed and/or working
         age >= 18) %>% # only people over 18
  
  # Removing merged variables and those used for filtering or joining
  select(-SERIALNO, -ADJINC, -employment, -education, -degree_1, -degree_2)  %>%
  
  # Dropping unused factor levels
  mutate_if(is.factor, fct_drop)

# -----------------------------------------------------------------------------

# Saving the dataset
saveRDS(ACS, file = "data/ACSBig.Rds")

# Freeing memory
rm(list = ls())
