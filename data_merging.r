
###############################################################################
# OTITIS MERGING PIPELINE (Script 1)
# DATA EXTRACTION → MERGING → SECTOR → SOSE → LANGUAGE → SPECIALTY
###############################################################################

set.seed(1)
setwd("/media/volume/JaninaH/codes/")
.libPaths("/shared-directory/sd-tools/apps/R/lib/")

library(nanoparquet)
library(dplyr)
library(lubridate)
library(readr)
library(data.table)
library(stringr)
library(arrow)

# -------------------------------------------------------------------------
# DATA EXTRACTION AND MERGING PIPELINE FOR HILMO, AVOHILMO, KANTA, KELA,
# DVV DEMOGRAPHICS, SOCIO-ECONOMIC STATUS AND PHYSICIAN SPECIALTIES
#
# This script processes nationwide registry data for otitis media research.
# It extracts ICD-10 diagnoses (H65–H67), visit-level metadata, procedures,
# prescription records, dispensed medicines, demographic information and
# physician specialty data. The final merged dataset links all available 
# information at the individual-visit level for children under 18 years.
# -------------------------------------------------------------------------

set.seed(1)

# Set working directory (local environment)
setwd("/media/volume/JaninaH/codes/")

# Define library path (cluster/RStudio Server environment)
.libPaths("/shared-directory/sd-tools/apps/R/lib/")

# Load required libraries
library(nanoparquet)
library(dplyr)
library(lubridate)
library(readr)
library(data.table)
library(stringr)

# -------------------------------------------------------------------------
# 1. HILMO DATA: Inpatient and specialist outpatient visits
# Read datasets covering different year ranges.
# Only essential variables are retained.
# -------------------------------------------------------------------------

h0712 <- read_delim(
  "/media/.../HILMO0712.csv", delim = ";",
  col_types = cols(.default="c"), locale = locale(encoding="UTF-8")
) %>% select(FID, HILMOID, TUPVA, FD_HASH_Rekisterointinumero, PALTU)

h1317 <- read_delim(
  "/media/.../HILMO1317.csv", delim = ";",
  col_types = cols(.default="c"), locale = locale(encoding="UTF-8")
) %>% select(FID, HILMOID, TUPVA, FD_HASH_Rekisterointinumero, PALTU)

# (...) same pattern for HILMO1820, HILMO2122, HILMO9806

# -------------------------------------------------------------------------
# 2. HILMO ICD‑10 DIAGNOSIS DATA
# Only diagnoses related to otitis media (H65–H67) are extracted.
# Data is distributed across multiple files; each is filtered identically.
# -------------------------------------------------------------------------

icd_1 <- read_delim("/media/.../H_ICD10_1.csv", delim=";", 
          col_types=cols(.default="c"), locale=locale(encoding="UTF-8")) %>%
          select(HILMOID, KOODI, KENTTA)

# Filter ICD‑10 codes H65/H66/H67
filtered_1 <- subset(icd_1, grepl("^H65
^H66
^H67", KOODI, ignore.case = TRUE))

# (...) same pattern for ICD10 files 2–5

# Combine diagnosis data from all available ICD files
icd_data <- bind_rows(filtered_1, filtered_2, filtered_3, filtered_4, filtered_5)

# Create list of HILMO IDs with relevant otitis diagnoses
HILMOT <- unique(icd_data$HILMOID)

# -------------------------------------------------------------------------
# 3. FILTER HILMO VISITS TO ONLY INCLUDE THOSE WITH RELEVANT DIAGNOSES
# -------------------------------------------------------------------------

h07 <- h0712 %>% filter(HILMOID %in% HILMOT)
h13 <- h1317 %>% filter(HILMOID %in% HILMOT)
# (...) repeated for each period dataset

# Combine filtered HILMO visit data
combined_df <- bind_rows(h07, h13, h18, h21, h98)

# Merge diagnosis & visit data (left join by HILMOID)
cdf1 <- merge(icd_data, combined_df, by = "HILMOID", all.x = TRUE)

# Convert visit date to Date format
cdf1$TUPVA <- as.Date(as.character(cdf1$TUPVA), format = "%d.%m.%Y")

# -------------------------------------------------------------------------
# 4. AVOHILMO PRIMARY CARE DATA
# Extract otitis-related diagnoses and merge with visit metadata.
# -------------------------------------------------------------------------

# Loop through 14 diagnosis files for Avohilmo
# Filtering ICD‑10 H65–H67 as above
# Data is saved to icdf1 ... icdf14

# Combine all filtered Avohilmo diagnoses
syyt <- bind_rows(icdf1, icdf2, ..., icdf14)

# Read Avohilmo visit identifiers for merging
id <- unique(syyt$AVOHILMOID)

# Read visit metadata (FID, provider, physician ID, visit date)
# for years 2017–2022, filtering by previously identified visit IDs

# Combine Avohilmo visit data
ah1722 <- bind_rows(Ahs17, Ahs18, Ahs19, Ahs20, Ahs21_1, Ahs21_2, Ahs22_1, Ahs22_2)

# Merge diagnoses and visits
cdf1 <- merge(ah1722, syyt, by = "AVOHILMOID")

# Convert date to proper format
cdf1$KAYNTI_LOPPUI <- as.Date(as.character(cdf1$KAYNTI_LOPPUI),
                              format = "%d.%m.%Y %H:%M")

# -------------------------------------------------------------------------
# 5. DVV DEMOGRAPHICS (DOB, SEX, LANGUAGE)
# Used for age calculation (<18 yrs).
# -------------------------------------------------------------------------

cols <- names(fread("/media/.../DVV.csv", nrows = 0, sep = ";"))

# Read only needed columns (FID, DOB, sex, language)
df <- fread("/media/.../DVV.csv", select = cols[1:5], sep = ";")

# Convert DOB to date format
df$Syntymapaiva <- as.Date(as.character(df$`Syntymä-päivä`), format = "%Y%m%d")

# Merge with Avohilmo & Hilmo datasets
ahs    <- left_join(ahs, df, by="FID")
hilmot <- left_join(hilmot, df, by="FID")
ahp    <- left_join(ahp, df, by="FID")

# Calculate age & retain only children (<18 y)
ahs$Ika    <- as.numeric(difftime(ahs$KAYNTI_LOPPUI, ahs$Syntymapaiva, units="days"))/365.25
ahs        <- subset(ahs, Ika < 18)

# (...) same for hilmo and ahp

# -------------------------------------------------------------------------
# 6. HILMO PROCEDURES (Tympanometry / Tympanostomy, code DCA20)
# Identify and merge procedural data.
# -------------------------------------------------------------------------

ht1 <- fread("/media/.../H_TOIMENP_1.csv")
filtered_1 <- subset(ht1, grepl("DCA20", TOIMP, ignore.case = TRUE))

# (...) same for files 2–5

# Bind all procedure rows
toimenpiteet <- bind_rows(filtered_1, ..., filtered_5)

# Merge HILMO visit + procedure + demographics
hilto <- left_join(toimenpiteet, hi, by=c("FID","HILMOID"))
hilto <- left_join(hilto, df, by="FID")

# Convert HILMO procedure dates and filter by study period
hilto$TUPVA <- as.Date(as.character(hilto$TUPVA), format="%d.%m.%Y")
hilto <- hilto %>% 
  filter(TUPVA >= as.Date("2017-01-01") & TUPVA <= as.Date("2022-12-31"))

# -------------------------------------------------------------------------
# 7. AVOHILMO PROCEDURES (DCA20 and SPAT 1019)
# -------------------------------------------------------------------------

aht1 <- fread("/media/.../AH_TOIMENPITEET_1.csv")
filtered_1 <- subset(aht1, grepl("DCA20
1019", TOIMENPIDE, ignore.case = TRUE))

# Bind and merge with visit data as above
# Append to AH dataset (ahs)
ahs <- left_join(ahs, toimenpiteet, by=c("FID","AVOHILMOID"))

# -------------------------------------------------------------------------
# 8. HARMONIZATION: Combine Avohilmo and Hilmo
# Create uniform variable names and stack datasets together.
# -------------------------------------------------------------------------

ahsp <- bind_rows(ahs, ahp) %>% unique()

# Rename selected columns to harmonize structure
colnames(ahsp)[4] <- "PALTU"
colnames(ahsp)[3] <- "PVM"
colnames(ahsp)[6] <- "KOODI"
colnames(ahsp)[1] <- "AVOTAIHILMOID"

# Replace missing KOODI from ICD10 column if available
ahsp <- ahsp %>% mutate(KOODI = if_else(is.na(KOODI), ICD10, KOODI))

# Remove unnecessary columns
ahsp <- subset(ahsp, select = -c(JARJESTYS, ICD10))

# -------------------------------------------------------------------------
# 9. FINAL MERGE: Avohilmo + Hilmo
# -------------------------------------------------------------------------

avoh <- bind_rows(ahsp, hilmot)

# Restrict to study period
avoh <- avoh %>% filter(PVM >= as.Date("2017-01-01") &
                        PVM <= as.Date("2022-12-31"))

# Remove duplicate rows per day/visit
avoh <- avoh %>% distinct(PVM, FID, TOIMP, .keep_all = TRUE)

# Condense procedures/diagnoses to one row per visit
avoh <- avoh %>% group_by(PVM, FID) %>%
  mutate(
    TOIMP = if (all(is.na(TOIMP))) NA_character_ else first(na.omit(TOIMP)),
    KOODI = if (all(is.na(KOODI))) NA_character_ else first(na.omit(KOODI))
  ) %>% ungroup()

# One row per (FID, date)
cdf1 <- avoh %>% group_by(FID, PVM) %>% slice(1) %>% ungroup()

# -------------------------------------------------------------------------
# 10. KANTA PRESCRIPTION DATA: Link prescriptions to visits
# Using chunked reading due to file size.
# 0–2 day matching window between visit date and prescription date.
# -------------------------------------------------------------------------

# (...) chunked reading of prescription files
# FID-based filtering
# binding chunks

# Combine all years
res <- bind_rows(o17, o18, o19, o20, o21, o22)

# Find candidate visit–prescription pairs
candidates <- res %>%
  inner_join(cdf1, by="FID") %>%
  filter(DATE_PK >= PVM & DATE_PK <= PVM + days(2)) %>%
  mutate(pvm_diff = as.numeric(DATE_PK - PVM))

# Keep one visit per prescription (but all prescriptions per visit)
best_matches <- candidates %>%
  arrange(FID, DATE_PK, pvm_diff) %>%
  group_by(FID, DATE_PK, row_id = row_number()) %>%
  slice(1) %>% ungroup()

# Merge matched prescriptions to visit dataset
final <- cdf1 %>% left_join(best_matches, by=c("FID","PVM"))

###############################################################################
# 11. SECTOR DEFINITION
###############################################################################

uq <- final %>%
  mutate(
    Sector = case_when(
      !is.na(SECTOR) ~ as.numeric(SECTOR),
      grepl("^95|^6", PALTU) ~ 2,
      TRUE ~ 1
    )
  )

###############################################################################
# 12. UNIQUE DISPENSATION ROWS
###############################################################################

ct <- unique(ct)

###############################################################################
# 13. SOCIOECONOMIC STATUS
###############################################################################

status_df <- sose %>%
  group_by(vuosi, FID) %>%
  summarise(sose = last(sose), .groups="drop")

yhd <- uq %>%
  left_join(status_df, by=c("FID","Year"="vuosi"))

yhd$sos <- substr(as.character(yhd$sose), 1, 1)

###############################################################################
# 14. LANGUAGE CLASSIFICATION
###############################################################################

yhd <- yhd %>% mutate(
  kieli = case_when(
    str_detect(kieli, regex("fi|sv", ignore_case=TRUE)) ~ "domestic",
    TRUE ~ "other"
  )
)

###############################################################################
# 15. SPECIALTY (VALVIRA)
###############################################################################

erik <- erikois %>%
  select(FD_HASH_Rekisterointinumero, Tutkinto, Suorituspvm)

erik$Suorituspvm <- as.Date(erik$Suorituspvm, format="%d.%m.%Y")

df_final <- yhd %>%
  left_join(erik, 
            by=c("FD_HASH_Rekisterointinro"="FD_HASH_Rekisterointinumero")) %>%
  mutate(valid = if_else(Suorituspvm <= PVM, TRUE, FALSE)) %>%
  group_by(FID, PVM, ATC_CODE, Sector, ab, TOIMP) %>%
  arrange(desc(valid), desc(Suorituspvm)) %>%
  slice(1) %>% ungroup()

dt <- df_final %>% mutate(
  erikoisala = case_when(
    str_detect(Tutkinto, regex("lasten", ignore_case=TRUE)) ~ "pediatrics",
    str_detect(Tutkinto, regex("korva", ignore_case=TRUE)) ~ "ENT",
    str_detect(Tutkinto, regex("yleis", ignore_case=TRUE)) ~ "GP",
    !str_detect(Tutkinto, regex("erikoislääkäri|erikoishammaslääkäri", ignore_case=TRUE)) ~ "none",
    TRUE ~ "other"
  )
)

###############################################################################
# SAVE MERGED FULL DATASET → NEXT SCRIPT USES THIS
###############################################################################

write_parquet(dt, "/media/volume/JaninaH/avojahilmo_putket_sose.parquet")
