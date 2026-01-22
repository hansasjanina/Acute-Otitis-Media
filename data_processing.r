
###############################################################################
# OTITIS PROCESSING PIPELINE (Script 3)
# EPISODES → RAOM → ATC → VISIT FAILURE → AB FAILURE → SEASON
###############################################################################

set.seed(4)
setwd("/media/volume/JaninaH/codes/")

library(arrow)
library(dplyr)
library(lubridate)
library(readr)
library(data.table)
library(stringr)
library(tidyr)

###############################################################################
# LOAD MERGED DATA FROM SCRIPT 1
###############################################################################

df <- read_parquet("/media/volume/JaninaH/avojahilmo_putket_sose.parquet")

###############################################################################
# 1. BASIC FILTERING
###############################################################################

df <- df %>% filter(is.na(TOIMP))
df <- df %>% filter(Sector != 3)

###############################################################################
# 2. 30-DAY EPISODES
###############################################################################

df <- df %>%
  mutate(date_only = as.Date(PVM)) %>%
  arrange(FID, date_only) %>%
  group_by(FID) %>%
  mutate(
    new_period = {
      period_id <- 0L
      start_date <- NA
      res <- integer(n())
      for (i in seq_along(date_only)) {
        if (is.na(start_date) || date_only[i] - start_date > 30) {
          period_id <- period_id + 1L
          start_date <- date_only[i]
        }
        res[i] <- period_id
      }
      res
    }
  ) %>% ungroup()

###############################################################################
# 3. REPEAT VISIT FLAGS
###############################################################################

df <- df %>%
  group_by(FID, new_period) %>%
  mutate(
    visit_order_in_period = dense_rank(date_only),
    repeat_count = if_else(
      visit_order_in_period == 1,
      as.integer(n_distinct(date_only) - 1),
      NA_integer_
    )
  ) %>% ungroup()

df$is_repetative <- ifelse(grepl("1", df$visit_order_in_period), 0, 1)

###############################################################################
# 4. RAOM CALCULATION
###############################################################################

setDT(df)
df[, PVM := as.Date(PVM)]
visits <- df[is_repetative == 0, .(new_period = first(new_period)), by=.(FID,PVM)]
setorder(visits, FID, PVM)

visits[, count_6m := sapply(seq_len(.N),
                            function(i) sum(PVM[1:(i-1)] >= PVM[i] - 180)), by=FID]
visits[, count_12m := sapply(seq_len(.N),
                             function(i) sum(PVM[1:(i-1)] >= PVM[i] - 365)), by=FID]

visits[, recurrent := fifelse(count_6m >= 3 | count_12m >= 4, 1, 0)]

df <- visits[df, on=.(FID,PVM)]

###############################################################################
# 5. AGE GROUPS + COMORBIDITIES
###############################################################################

korvat <- read_parquet("/media/volume/JaninaH/korva_historia") %>%
  select(FID, PVM, raom)

dt <- left_join(df, korvat, by=c("FID","PVM"))

dt$Ikalk <- cut(
  dt$Ika,
  breaks = c(0,2,5,12,18),
  labels = c("0–2","2–5","5–12","12–18"),
  right = FALSE
)

dt <- distinct(dt)

dt <- dt %>%
  mutate(
    conditions = if_else(
      J30 == 1 | L20 == 1 | J45_J46 == 1 | D80_D89 == 1 | Q90 == 1,
    1, 0)
  )

###############################################################################
# 6. ANTIBIOTICS (J01)
###############################################################################

dt <- dt %>% filter(grepl("^J01", ATC_CODE))
dt <- dt %>% distinct(FID, PVM, Sector, .keep_all = TRUE)

dt <- dt %>%
  filter(PVM >= as.Date("2017-01-01") & PVM <= as.Date("2022-12-31"))

write_parquet(dt, "/media/volume/JaninaH/strategy")

###############################################################################
# 7. ATC PRIORITY AND FAILURE CALCULATIONS
# (all your existing code unchanged)
###############################################################################

# --------------------------------------
# 8) ATC classification for guideline adherence
# --------------------------------------
# Amoxicillin (J01CA04), Amoxicillin-clavulanic (J01CR02), other J01
dt <- dt %>% mutate(ablk = case_when(
  ATC_CODE %in% c("J01CA04") ~ "Amoxicillin", 
  ATC_CODE %in% c("J01CR02") ~ "Amoxicillin-clavulanic",
  grepl("^J01", ATC_CODE, ignore.case = TRUE) & !ATC_CODE %in% c("J01CR02", "J01CA04") ~ "other",
  TRUE ~ NA_character_

#we use the worst case scenario, if only one doctor involved the worst antibiotic selection retained for analysis

library(data.table)

DT <- as.data.table(dt)
DT[, PVM := as.IDate(PVM)] 

DT[, prio := fcase(
  ablk == "other",                   1L,
  ablk == "Amoxicillin-clavulanic",  2L,
  ablk == "Amoxicillin",             3L,
  default = 99L   # tuntemattomat heikoimmaksi tai käsittele erikseen
)]

setorder(DT, FID, PVM, FD_HASH_Rekisterointinro, prio)
DT_one_ab <- DT[, .SD[1L], by = .(FID, PVM, FD_HASH_Rekisterointinro)]
DT_one_ab[, prio := NULL]


#now we have data ready for guideline-adherence analysis, next unique visits for mangement and treatment-failures
DT_one_ab%>%write_parquet(paste0("/media/volume/JaninaH/", "avojahilmop_guideline_december.parquet"))
dt<-read_parquet(paste0("/media/volume/JaninaH/", "avojahilmop_guideline_november.parquet"))
#calculate unique visits (for visit failure) (one row/fid+pvm) so that when there is dispensation (ab==1) it comes with, and if not then any ATC_CODE=J01*, if no condition fills then just one row


uq <-dt %>%
  group_by(FID, PVM, Sector) %>%
  mutate(priority = case_when(
    ab == 1 ~ 1,
    grepl("^J01", ATC_CODE) ~ 2,
    TRUE ~ 3
  )) %>%
  slice_min(priority, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(-priority)
#check data
table(uq$recurrent, uq$Sector)

#calculate visit_failure
uq <- uq %>%
  arrange(FID, PVM, new_period) %>%
  group_by(visit_order_in_period) %>%
  mutate(
    ab_reps = ab == "1" & is_repetative == "1", #when ab ==1 (dispensed antibiotic)and is_repetative == 1
    has_ab_after = ab_reps >= 1,
    failed_visit = if_else(is_repetative == 1 & has_ab_after, 1, 0)
  ) %>%
  ungroup() %>%
  select(-has_ab_after)

#calculate index sector (sector that began the treatment of otitis episode)
uq <- uq %>%
  group_by(FID, new_period) %>%
  mutate(index_sector = Sector[is_repetative == 0][1]) %>%  
  ungroup()

#calculate index visits with antibiotic initiation (=antibiotic prescribed at index visit)
#select episode index_visit with abp==1 (antibiotic prescribed)
abp_initials <- uq %>%
  filter(is_repetative == 0 & abp == 1) %>%
  select(FID, new_period)

# Mark abfailure for rows, when failed_visit == 1 and episode is found in abp_initials
uq <- uq %>%
  mutate(abfailure = if_else(
    failed_visit == 1 & (FID %in% abp_initials$FID & new_period %in% abp_initials$new_period),
    1,
    0
  ))
#check data
table(uq$abfailure, uq$index_sector)

#toim<-read_parquet(paste0("/media/volume/JaninaH/fidien_kaikki_toimitukset.parquet"))
#fds<-toim%>%filter
colnames(uq)
#remove extra columns
uq<-subset(uq, select =-c(16, 17, 29, 38, 50))
#save data
uq%>%write_parquet(paste0("/media/volume/JaninaH/", "failuredata"))

uq$first_vis_no_ab <-as.integer(uq$is_repetative == 0 & uq$abp == FALSE)
uq$failuredate <- ifelse(uq$failed_visit == 1, as.character(uq$PVM), NA)
uq$failuredate <- as.Date(uq$failuredate)
uq$startdate <- ifelse(uq$is_repetative == 0, as.character(uq$PVM), NA)
table(uq$failed_visit, uq$Sector)
#Arrange data by FID and date
uq <- uq %>% arrange(FID, PVM)

dats <- uq %>%
  mutate(
    startdate = as.Date(startdate),
    failuredate = as.Date(failuredate)
  )
library(tidyr)
# insert startdate for each visit of the same episode
dats <- dats %>%
  arrange(FID, new_period, PVM) %>%  
    group_by(FID, new_period) %>%
      fill(startdate, .direction = "downup") %>% 
        ungroup()

#calculate early antibiotic treatment failure
data <- dats %>%
  mutate(
    failuredate = as.Date(failuredate),
    startdate = as.Date(startdate),
    abfailure7 = case_when(
      abfailure == 1 &
        !is.na(failuredate) &
        !is.na(startdate) &
        failuredate >= startdate &
        failuredate <= startdate + 7 ~ 1,
      TRUE ~ 0
    )
  )

table(data$abfailure7, data$index_sector)
#calculate early managementfailure
data <- data %>%
  mutate(
    failuredate = as.Date(failuredate),
    startdate = as.Date(startdate),
    failure7 = case_when(
      failed_visit == 1 &
        !is.na(failuredate) &
        !is.na(startdate) &
        failuredate >= startdate &
        failuredate <= startdate + 7 ~ 1,
      TRUE ~ 0
    )
  )
#calculate season
data <- data %>%
  mutate(
    PVM = as.Date(PVM),
    month = month(PVM),
    season = case_when(
      month %in% c(12, 1, 2)  ~ "winter",
      month %in% c(3, 4, 5)   ~ "spring",
      month %in% c(6, 7, 8)   ~ "summer",
      month %in% c(9, 10, 11) ~ "autumn",
      TRUE ~ NA_character_
    ),
    dg = if_else(grepl("H66", KOODI, fixed = TRUE), "H66", "H65_H67")
  ) %>%
  select(-month)
table(data$failure7, data$Sector)

write_parquet(data, "/media/volume/JaninaH/failuredata")

###############################################################################
# END
###############################################################################