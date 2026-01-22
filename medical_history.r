#script 2


library(data.table)
library(dplyr)
library(readr)
library(arrow)
library(stringr)

# ============================================================
# 1. Load otitis visit dataset and extract unique FIDs
# ============================================================

# Main dataset of otitis visits (Avohilmo + Hilmo combined earlier)
dt <- read_parquet("/media/volume/JaninaH/avojahilmo_putket_sose.parquet")

# FIDs define which individuals will be included in history extraction
fids <- unique(dt$FID)


# ============================================================
# 2. Load HILMO ICD10 diagnosis files (1–5) and filter by FID
# ============================================================

# Helper: read and filter ICD10 files in a loop
read_icd <- function(path) {
  read_delim(path, delim = ";", col_types = cols(.default="c"),
             locale = locale(encoding = "UTF-8")) %>%
    select(HILMOID, FID, KOODI) %>%
    filter(FID %in% fids)
}

# List of the 5 Hilmo ICD10 files
icd_paths <- sprintf(
  "/media/volume/Data_THL_2698_14.02.00_2023_pk1_uudelleen/THL/FD_2698_THL2023_2698_H_ICD10_%s.csv",
  1:5
)

# Read and combine
hist <- bind_rows(lapply(icd_paths, read_icd))

# Save diagnosis-only dataset
write_parquet(hist, "/media/volume/JaninaH/hilmo_sairaush.parquet")


# ============================================================
# 3. Load HILMO visit date data (TUPVA) and filter by HILMOID
# ============================================================

read_hilmo_date <- function(file) {
  read_delim(file, delim=";", col_types=cols(.default="c"),
             locale=locale(encoding="UTF-8")) %>%
    select(FID, HILMOID, TUPVA)
}

hilmo_date_files <- c(
  "HILMO0712", "HILMO1317", "HILMO1820", "HILMO2122", "HILMO9806"
)

hilmo_date_paths <- sprintf(
  "/media/volume/Data_THL_2698_14.02.00_2023_pk1_uudelleen/THL/FD_2698_THL2023_2698_%s.csv",
  hilmo_date_files
)

# Read all visit date files
hilmo_dates <- lapply(hilmo_date_paths, read_hilmo_date) %>% bind_rows()

# Keep only visit dates for HILMOIDs that have diagnoses
HILMOT <- unique(hist$HILMOID)
hilmo_dates <- hilmo_dates %>% filter(HILMOID %in% HILMOT)

# Merge diagnosis + visit date
hilf <- merge(hist, hilmo_dates, by="HILMOID", all.x=TRUE)

# Standardize column names
colnames(hilf)[1:4] <- c("AVOTAIHILMOID", "FID", "KOODI", "PVM")

# Convert HILMO TUPVA to Date
hilf$PVM <- as.Date(hilf$PVM, format="%d.%m.%Y")

write_parquet(hilf, "/media/volume/JaninaH/hilmo_sairaush.parquet")


# ============================================================
# 4. Load Avohilmo diagnoses (14 files), filter by FID
# ============================================================

read_ah_diag <- function(i) {
  path <- sprintf(
    "/media/volume/Data_THL_2698_14.02.00_2023_pk1_kayntisyy_folk/THL/FD_2698_THL2023_2698_AH_KAYNTISYY_%s.csv",
    i
  )
  read_delim(path, delim=";", col_types=cols(.default="c"),
             locale=locale(encoding="UTF-8")) %>%
    select(AVOHILMOID, FID, LUOKITUS) %>%
    filter(FID %in% fids)
}

syyt <- bind_rows(lapply(1:14, read_ah_diag))
write_parquet(syyt, "/media/volume/JaninaH/avohilmo_sairaush.parquet")


# ============================================================
# 5. Load Avohilmo visit data (files 16–22 & 21_1, 21_2, 22_1, 22_2)
# ============================================================

# Standard visit files 16–20
read_ah_visit <- function(i) {
  path <- sprintf(
    "/media/volume/Data_THL_2698_14.02.00_2023_pk1_uudelleen/THL/FD_2698_THL2023_2698_AH_%s.csv",
    i
  )
  read_delim(path, ";", col_types=cols(.default="c"),
             locale=locale(encoding="UTF-8")) %>%
    select(AVOHILMOID, FID, PALVELUNTUOTTAJA, KAYNTI_LOPPUI, FD_HASH_Rekisterointinumero)
}

# Special case files
read_special_ah <- function(name) {
  path <- sprintf(
    "/media/volume/Data_THL_2698_14.02.00_2023_pk1_uudelleen/THL/FD_2698_THL2023_2698_%s.csv",
    name
  )
  read_delim(path, ";", col_types=cols(.default="c"),
             locale=locale(encoding="UTF-8")) %>%
    select(FID, AVOHILMOID, KAYNTI_LOPPUI)
}

# Load all visit files
ah_visits <- bind_rows(
  lapply(16:20, read_ah_visit),
  read_special_ah("AH_21_1"),
  read_special_ah("AH_21_2"),
  read_special_ah("AH_22_1"),
  read_special_ah("AH_22_2")
)

# Filter visits belonging to Avohilmo diagnosis dataset
id <- unique(syyt$AVOHILMOID)
ah_visits <- ah_visits %>% filter(AVOHILMOID %in% id)

# Merge Avohilmo diagnoses + visit data
avof <- merge(syyt, ah_visits, by="AVOHILMOID")

# Standardize names for later merging
colnames(avof)[1:4] <- c("AVOTAIHILMOID", "FID", "KOODI", "PVM")

# Convert timestamp to Date
avof$PVM <- as.Date(avof$PVM, format="%d.%m.%Y %H:%M")

write_parquet(avof, "/media/volume/JaninaH/avohilmo_sairaush.parquet")


# ============================================================
# 6. Long-term diagnoses (pitkadiag) – extract chronic conditions
# ============================================================

read_pitkadiag <- function(i) {
  path <- sprintf(
    "/media/volume/Data_THL_2698_14.02.00_2023_pk1_uudelleen/THL/FD_2698_THL2023_2698_AH_pitkadiag_%s.csv",
    i
  )
  read_delim(path, ";", col_types=cols(.default="c"),
             locale=locale(encoding="UTF-8")) %>%
    select(AVOHILMOID, ICD10)
}

# Read & filter chronic diagnoses
chronic_pattern <- "^L20|^J30|^D8|^Q90|^J45|^J46"
p12 <- bind_rows(
  read_pitkadiag(1),
  read_pitkadiag(2)
) %>% filter(grepl(chronic_pattern, ICD10, ignore.case=TRUE))

# Join chronic diagnoses to visit data
avoht <- left_join(ah_visits, p12, by="AVOHILMOID") %>% filter(!is.na(ICD10))

# Standardize names
colnames(avoht)[1:4] <- c("AVOTAIHILMOID", "FID", "PVM", "KOODI")
avoht$PVM <- as.Date(avoht$PVM, format="%d.%m.%Y %H:%M")


# ============================================================
# 7. Combine all (HILMO + Avohilmo + long-term)
# ============================================================

avoh <- bind_rows(avof, hilf, avoht)
write_parquet(avoh, "/media/volume/JaninaH/avojahilmop_sairausf.parquet")


# ============================================================
# 8. Extract chronic diagnoses & create indicator flags
# ============================================================

avoh <- read_parquet("/media/volume/JaninaH/avojahilmop_sairausf.parquet")

# Keep only chronic-related ICD10 codes
diags <- avoh %>%
  filter(grepl(chronic_pattern, KOODI, ignore.case=TRUE)) %>%
  rename(diagnosis_date = PVM, diagnoosi = KOODI)

# ICD code patterns used for flag creation
codes <- list(
  J30 = "^J30", L20 = "^L20",
  J45_J46 = "^J4[5-6]", D80_D89 = "^D8[0-9]"
  , Q90 = "^Q90"
)

# For each visit in dt → check whether diagnosis existed before that visit
df_flags <- dt %>%
  left_join(diags, by="FID") %>%
  filter(diagnosis_date <= PVM) %>%
  group_by(FID, PVM, Sector, ATC_CODE, ab, TOIMP) %>%
  summarise(
    J30 = as.integer(any(str_detect(diagnoosi, codes$J30))),
    L20 = as.integer(any(str_detect(diagnoosi, codes$L20))),
    J45_J46 = as.integer(any(str_detect(diagnoosi, codes$J45_J46))),
    D80_D89 = as.integer(any(str_detect(diagnoosi, codes$D80_D89))),
    Q90 = as.integer(any(str_detect(diagnoosi, codes$Q90))),
    .groups="drop"
  )

# Join flags back to visit data
df_final <- dt %>%
  left_join(df_flags, by=c("FID","PVM","Sector","ATC_CODE","TOIMP","ab")) %>%
  mutate(across(J30:Q90, ~replace_na(.x, 0)))

write_parquet(df_final, "/media/volume/JaninaH/avojahilmop_november.parquet")


# ============================================================
# 9. RAOM calculation (H65–H67 otitis diagnoses)
# ============================================================

# Extract otitis-related diagnoses
otitis_pattern <- "^H67|^H66|^H65"
avoh <- avoh %>% filter(grepl(otitis_pattern, KOODI))

write_parquet(avoh, "/media/volume/JaninaH/korva_historia")


# ============================================================
# 10. Collapse visits into 30-day otitis episodes
# ============================================================

df <- avoh %>%
  mutate(date_only = as.Date(PVM)) %>%
  arrange(FID, date_only) %>%
  group_by(FID) %>%
  mutate(
    # Start new episode if >30 days since previous
    new_period = {
      period_id <- 0L
      start_date <- NA
      result <- integer(n())
      for (i in seq_along(date_only)) {
        if (is.na(start_date) || (date_only[i] - start_date) > 30) {
          period_id <- period_id + 1L
          start_date <- date_only[i]
        }
        result[i] <- period_id
      }
      result
    }
  ) %>% ungroup()


# ============================================================
# 11. Episode-level counts (visits within 6 & 12 months)
# ============================================================

setDT(df)
df[, PVM := as.Date(PVM)]

# One row per FID + date to count episodes
visits <- df[, .(new_period = first(new_period)), by=.(FID, PVM)]
setorder(visits, FID, PVM)

# Count previous episodes in rolling windows
visits[, count_6m  := sapply(seq_len(.N), function(i) sum(PVM[1:(i-1)] >= PVM[i] - 180)), by=FID]
visits[, count_12m := sapply(seq_len(.N), function(i) sum(PVM[1:(i-1)] >= PVM[i] - 365)), by=FID]

# Flag recurrent
visits[, recurrent := fifelse(count_6m >= 3 | count_12m >= 4, 1L, 0L)]

# Attach RAOM marker back to full data
df <- visits[df, on=.(FID, PVM)]


# ============================================================
# 12. Mark RAOM within 1-year lookback window
# ============================================================

df[, rec_date := fifelse(recurrent == 1L, PVM, NA_integer_)]
setorder(df, FID, PVM)

# Last recurrent episode carried forward
df[, anchor := nafill(rec_date, type="locf"), by=FID]

# raom = 1 if visit within 365 days of a recurrent episode
df[, raom := as.integer(!is.na(anchor) & (PVM - anchor) <= 365)]

write_parquet(df, "/media/volume/JaninaH/korva_historia")