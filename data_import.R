# IMPORTING THE DATA

# Loading required packages
library(data.table)

# Reading in the dataset
acs_sample_raw1 <- fread("data/full/psam_pusa.csv")
acs_sample_raw2 <- fread("data/full/psam_pusb.csv")
acs_sample_raw <- rbindlist(list(acs_sample_raw1, acs_sample_raw2))

saveRDS(acs_sample_raw, file = "data/acs_sample_raw.rds")
