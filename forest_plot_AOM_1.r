library(tidyverse)
library(ragg)
library(scales)

df <- tribble(
  ~outcome, ~OR, ~CI_low, ~CI_high, ~private_pct, ~public_pct,

  # -------------------------
  "Management strategy",                   NA_real_, NA_real_, NA_real_, NA_real_, NA_real_,
  "no antibiotic treatment",                         1,       NA_real_, NA_real_, "29.0(48 824)",     "19.8(83 338)",
  "antibiotic treatmentᵃ",                     0.46,    0.45,     0.47,     "71.0(119 809)",     "80.2(337 863)",
  "antibiotic treatmentᵇ",                  1.07,    1.04,     1.10,     71.0(119 809),     80.2(337 863),
  "",                   NA_real_, NA_real_, NA_real_, NA_real_, NA_real_,


  # -------------------------
  "Antibiotic guideline adherenceᶜ",        NA_real_, NA_real_, NA_real_, NA_real_, NA_real_,
  "other antibiotic",                                 1,       NA_real_, NA_real_,  13.7(16 356),     12.3(41 854),
  "amoxicillin or amoxicillin-clavulanicᵃ", 0.65, 0.62, 0.69,     86.3(103 111),     87.7(298 165),
  "amoxicillin or amoxicillin-clavulanicᵇ", 0.63, 0.59, 0.67,   86.3(103 111),     87.7(298 165),
   "",                   NA_real_, NA_real_, NA_real_, NA_real_, NA_real_,

  "Antibiotic treatment strategy",        NA_real_, NA_real_, NA_real_, NA_real_, NA_real_,
  "amoxicillin",                           1,       NA_real_, NA_real_, 53.3(63 659),      79.4(269 954),
  "amoxicillin-clavulanicᵃ",         7.11,    6.95,     7.28,     33.0(39 452),      8.3(28 211),
  "other antibioticᵃ",               1.82,    1.77,     1.87,     13.7(16 356),     12.3(41 854),
  "amoxicillin-clavulanicᵇ",      3.01,    2.92,     3.11,     33.0(39 452),      8.3(28 211),
  "other antibioticᵇ",            1.59,    1.53,     1.66,     13.7(16 356),     12.3(41 854),
   "",                   NA_real_, NA_real_, NA_real_, NA_real_, NA_real_,
  
# -------------------------
  "Management failure ᵈ",    NA_real_, NA_real_, NA_real_, NA_real_, NA_real_,
  "no management failure",                            1,       NA_real_, NA_real_, 89.3(213 334),      94.8(476 255),
  "management failureᵃ",                        2.13,    2.09,     2.17,     10.7(25 675),      5.2(26 262),
  "management failureᵇ",                     1.48,    1.44,     1.53,     10.7(25 675),      5.2(26 262),
  "",                   NA_real_, NA_real_, NA_real_, NA_real_, NA_real_,


 
  # -------------------------
  "Early management failureᵉ", NA_real_, NA_real_, NA_real_, NA_real_, NA_real_,
  "no early management failure",                      1,       NA_real_, NA_real_, 98.2(234 616),      98.9(496 911),
  "early management failureᵃ",                  1.18,    1.10,     1.28,      1.8(4 393),      1.1(5 606),
  "early management failureᵇ",               1.01,    0.92,     1.10,      1.8(4 393),      1.1(5 606),
  "",                   NA_real_, NA_real_, NA_real_, NA_real_, NA_real_,


 
  # -------------------------
  "Antibiotic treatment failureᶠ", NA_real_, NA_real_, NA_real_, NA_real_, NA_real_,
  "no treatment failure",                 1,       NA_real_, NA_real_, 89.4(213 632),      84.9(476 941),
  "early treatment failureᵃ",       2.16,    2.11,     2.20,     10.6(25 377),      5.1(25 576),
  "early treatment failureᵇ",    1.52,    1.47,     1.56,     10.6(25 377),      5.1(25 576),
  "",                   NA_real_, NA_real_, NA_real_, NA_real_, NA_real_,



    # -------------------------
  "Early antibiotic treatment failureᵍ", NA_real_, NA_real_, NA_real_, NA_real_, NA_real_,
  "no early treatment failure",           1,       NA_real_, NA_real_, 98.2(234 703),      98.9(497 206),
  "early treatment failureᵃ",       1.21,    1.12,     1.30,      1.8(4 306),      1.1(5 311),
  "early treatment failureᵇ",    1.04,    0.95,     1.14,      1.8(4 306),      1.1(5 311),
  "",                   NA_real_, NA_real_, NA_real_, NA_real_, NA_real_,


  # -------------------------
  "Eligibility for tympanostomyʰ",          NA_real_, NA_real_, NA_real_, NA_real_, NA_real_,
  "eligibility criteria is not met",                   1,       NA_real_, NA_real_, 32.3(861),     45.7(1 771),
  "eligibility criteria is metᵃ",                1.84,    1.64,     2.06,     67.7(1 842),     54.3(2 087),
  "eligibility criteria is metᵇ",             1.33,    1.18,     1.51,     67.7(1 842),     54.3(2 087)
)

library(tibble)

df <- tribble(
  ~outcome, ~OR, ~CI_low, ~CI_high, ~private_pct, ~public_pct,

  # -------------------------
  "",                   NA_real_, NA_real_, NA_real_, NA_character_, NA_character_,
  "Management strategy",                   NA_real_, NA_real_, NA_real_, NA_character_, NA_character_,
  "no antibiotic treatment",                         1,       NA_real_, NA_real_, "29.0(48 824)",     "19.8(83 338)",
  "antibiotic treatmentᵃ",                     0.46,    0.45,     0.47,     "71.0(119 809)",     "80.2(337 863)",
  "antibiotic treatmentᵇ",                  1.07,    1.04,     1.10,     "71.0(119 809)",     "80.2(337 863)",
  "",                   NA_real_, NA_real_, NA_real_, NA_character_, NA_character_,

  # -------------------------
  "Antibiotic guideline adherenceᶜ",        NA_real_, NA_real_, NA_real_, NA_character_, NA_character_,
  "other antibiotic",                                 1,       NA_real_, NA_real_,  "13.7(16 356)",     "12.3(41 854)",
  "amoxicillin or amoxicillin-clavulanicᵃ", 0.65, 0.62, 0.69,     "86.3(103 111)",     "87.7(298 165)",
  "amoxicillin or amoxicillin-clavulanicᵇ", 0.63, 0.59, 0.67,   "86.3(103 111)",     "87.7(298 165)",
  "",                   NA_real_, NA_real_, NA_real_, NA_character_, NA_character_,

  # -------------------------
  "Antibiotic treatment strategy",        NA_real_, NA_real_, NA_real_, NA_character_, NA_character_,
  "amoxicillin",                           1,       NA_real_, NA_real_, "53.3(63 659)",      "79.4(269 954)",
  "amoxicillin-clavulanicᵃ",         7.11,    6.95,     7.28,     "33.0(39 452)",      "8.3(28 211)",
  "other antibioticᵃ",               1.82,    1.77,     1.87,     "13.7(16 356)",     "12.3(41 854)",
  "amoxicillin-clavulanicᵇ",      3.01,    2.92,     3.11,     "33.0(39 452)",      "8.3(28 211)",
  "other antibioticᵇ",            1.59,    1.53,     1.66,     "13.7(16 356)",     "12.3(41 854)",
  "",                   NA_real_, NA_real_, NA_real_, NA_character_, NA_character_,

    # -------------------------
  "Management failureᵈ",    NA_real_, NA_real_, NA_real_, NA_character_, NA_character_,
  "no management failure",                            1,       NA_real_, NA_real_, "89.3(213 334)",      "94.8(476 255)",
  "management failureᵃ",                        2.13,    2.09,     2.17,     "10.7(25 675)",      "5.2(26 262)",
  "management failureᵇ",                     1.48,    1.44,     1.53,     "10.7(25 675)",      "5.2(26 262)",
  "",                   NA_real_, NA_real_, NA_real_, NA_character_, NA_character_,

   # -------------------------
  "Early management failurerᵉ", NA_real_, NA_real_, NA_real_, NA_character_, NA_character_,
  "no early management failure",                      1,       NA_real_, NA_real_, "98.2(234 616)",      "98.9(496 911)",
  "early management failureᵃ",                  1.18,    1.10,     1.28,      "1.8(4 393)",      "1.1(5 606)",
  "early management failureᵇ",               1.01,    0.92,     1.10,      "1.8(4 393)",      "1.1(5 606)",
  "",                   NA_real_, NA_real_, NA_real_, NA_character_, NA_character_,
  # -------------------------
  "Antibiotic treatment failureᶠ", NA_real_, NA_real_, NA_real_, NA_character_, NA_character_,
  "no treatment failure",                 1,       NA_real_, NA_real_, "89.4(213 632)",      "84.9(476 941)",
  "early treatment failureᵃ",       2.16,    2.11,     2.20,     "10.6(25 377)",      "5.1(25 576)",
  "early treatment failureᵇ",    1.52,    1.47,     1.56,     "10.6(25 377)",      "5.1(25 576)",
  "",                   NA_real_, NA_real_, NA_real_, NA_character_, NA_character_,

    # -------------------------
  "Early antibiotic treatment failureᵍ", NA_real_, NA_real_, NA_real_, NA_character_, NA_character_,
  "no early treatment failure",           1,       NA_real_, NA_real_, "98.2(234 703)",      "98.9(497 206)",
  "early treatment failureᵃ",       1.21,    1.12,     1.30,      "1.8(4 306)",      "1.1(5 311)",
  "early treatment failureᵇ",    1.04,    0.95,     1.14,      "1.8(4 306)",      "1.1(5 311)",
  "",                   NA_real_, NA_real_, NA_real_, NA_character_, NA_character_,

  # -------------------------
  "Eligibility for tympanostomyʰ",          NA_real_, NA_real_, NA_real_, NA_character_, NA_character_,
  "eligibility criteria is not met",                   1,       NA_real_, NA_real_, "32.3(861)",     "45.7(1 771)",
  "eligibility criteria is metᵃ",                1.84,    1.64,     2.06,     "67.7(1 842)",     "54.3(2 087)",
  "eligibility criteria is metᵇ",             1.33,    1.18,     1.51,     "67.7(1 842)",     "54.3(2 087)"
)
 # ----------------------------
library(tibble)

df <- tribble(
  ~outcome, ~OR, ~CI_low, ~CI_high, ~private_pct, ~public_pct,

  # -------------------------
  "",                                   NA_real_, NA_real_, NA_real_, NA_character_, NA_character_,
  "Management strategy",                NA_real_, NA_real_, NA_real_, NA_character_, NA_character_,
  "no antibiotic treatment",                      1,       NA_real_, NA_real_, "48 824 (29.0)",        "83 338 (19.8)",
  "antibiotic treatmentᵃ",                      0.46,        0.45,      0.47, "119 809 (71.0)",       "337 863 (80.2)",
  "antibiotic treatmentᵇ",                      1.07,        1.04,      1.10, "119 809 (71.0)",       "337 863 (80.2)",
  "",                                   NA_real_, NA_real_, NA_real_, NA_character_, NA_character_,

  # -------------------------
  "Antibiotic guideline adherenceᶜ",    NA_real_, NA_real_, NA_real_, NA_character_, NA_character_,
  "other antibiotic",                              1,       NA_real_, NA_real_, "16 356 (13.7)",        "41 854 (12.3)",
  "amoxicillin or amoxicillin-clavulanicᵃ",      0.65,        0.62,      0.69, "103 111 (86.3)",       "298 165 (87.7)",
  "amoxicillin or amoxicillin-clavulanicᵇ",      0.63,        0.59,      0.67, "103 111 (86.3)",       "298 165 (87.7)",
  "",                                   NA_real_, NA_real_, NA_real_, NA_character_, NA_character_,

  # -------------------------
  "Antibiotic treatment strategy",      NA_real_, NA_real_, NA_real_, NA_character_, NA_character_,
  "amoxicillin",                                   1,       NA_real_, NA_real_, "63 659 (53.3)",        "269 954 (79.4)",
  "amoxicillin-clavulanicᵃ",                     7.11,        6.95,      7.28, "39 452 (33.0)",         "28 211 (8.3)",
  "other antibioticᵃ",                            1.82,        1.77,      1.87, "16 356 (13.7)",        "41 854 (12.3)",
  "amoxicillin-clavulanicᵇ",                     3.01,        2.92,      3.11, "39 452 (33.0)",         "28 211 (8.3)",
  "other antibioticᵇ",                            1.59,        1.53,      1.66, "16 356 (13.7)",        "41 854 (12.3)",
  "",                                   NA_real_, NA_real_, NA_real_, NA_character_, NA_character_,

  # -------------------------
  "Management failureᵈ", NA_real_, NA_real_, NA_real_, NA_character_, NA_character_,
  "no management failure",                         1,       NA_real_, NA_real_, "213 334 (89.3)",       "476 255 (94.8)",
  "management failureᵃ",                         2.13,        2.09,      2.17, "25 675 (10.7)",         "26 262 (5.2)",
  "management failureᵇ",                         1.48,        1.44,      1.53, "25 675 (10.7)",         "26 262 (5.2)",
  "",                                   NA_real_, NA_real_, NA_real_, NA_character_, NA_character_,

   # -------------------------
  "Early management failureᵉ", NA_real_, NA_real_, NA_real_, NA_character_, NA_character_,
  "no early management failure",                   1,       NA_real_, NA_real_, "234 616 (98.2)",       "496 911 (98.9)",
  "early management failureᵃ",                   1.18,        1.10,      1.28, "4 393 (1.8)",           "5 606 (1.1)",
  "early management failureᵇ",                   1.01,        0.92,      1.10, "4 393 (1.8)",           "5 606 (1.1)",
  "",                                   NA_real_, NA_real_, NA_real_, NA_character_, NA_character_,

   # -------------------------
  "Antibiotic treatment failureᶠ", NA_real_, NA_real_, NA_real_, NA_character_, NA_character_,
  "no treatment failure",                          1,       NA_real_, NA_real_, "213 632 (89.4)",       "476 941 (94.9)",
  "antibiotic treatment failureᵃ",                    2.32,        2.27,      2.36, "25 377 (12.0)",         "25 576 (5.6)",
  "antibiotic treatment failureᵇ",                    1.49,        1.45,      1.54, "25 377 (12.0)",         "25 576 (5.6)",
  "",                                   NA_real_, NA_real_, NA_real_, NA_character_, NA_character_,

  # -------------------------
  "Early antibiotic treatment failureᵍ", NA_real_, NA_real_, NA_real_, NA_character_, NA_character_,
  "no early treatment failure",                   1,       NA_real_, NA_real_, "234 703 (98.2)",       "497 206 (98.9)",
  "early antibiotic treatment failureᵃ",                    1.35,        1.23,      1.48, "4 306 (1.9)",           "5 311 (1.1)",
  "early antibiotic treatment failureᵇ",                    1.17,        1.05,      1.30, "4 306 (1.9)",           "5 311 (1.1)",
  "",                                   NA_real_, NA_real_, NA_real_, NA_character_, NA_character_,

  # -------------------------
  "Eligibility for tympanostomyʰ",       NA_real_, NA_real_, NA_real_, NA_character_, NA_character_,
  "eligibility criteria is not met",                1,       NA_real_, NA_real_, "1 498 (43.8)",           "2 251 (47.0)",
  "eligibility criteria is metᵃ",                 1.15,        1.04,      1.26, "1 926 (56.2)",          "2 541 (53.0)",
  "eligibility criteria is metᵇ",                 0.84,        0.76,      0.94, "1 926 (56.2)",          "2 541 (53.0)"
)

# 4) Layout paikat
 # ----------------------------


# ======================
# Custom-transformaatio:
# 0–3.5 lineaarinen, 3.5–7 puristetaan 15% tilaan
# ======================
compress_x <- trans_new(
  name = "compress_x",
  transform = function(x) ifelse(x <= 3.5, x,
                                 3.5 + (x - 3.5) * 0.15),
  inverse   = function(x) ifelse(x <= 3.5, x,
                                 (x - 3.5)/0.15 + 3.5)
)

# ======================
# Data-esikäsittely
# ======================
df2 <- df %>%
  mutate(
    OR = ifelse(OR == "ref", 1, as.numeric(OR)),
    is_header = is.na(OR) & is.na(CI_low) & is.na(CI_high),
    is_ref = OR == 1 & (is.na(CI_low) | is.na(CI_high)),
    row_id = row_number(),

    OR_label = case_when(
      is_header ~ "",
      is_ref ~ "ref",
      TRUE ~ sprintf("%.2f (%.2f–%.2f)", OR, CI_low, CI_high)
    ),

    private_label = ifelse(is_header, "", sprintf("%s", private_pct)),
    public_label  = ifelse(is_header, "", sprintf("%s", public_pct))
  )

 

# ======================
# Layout: sarakepaikat
# ======================
text_left   <- -4.9
text_right  <- -0.2
forest_left <-  0
forest_right <- 7.7

x_outcome <- text_left
x_private <- -2.0
x_public  <- -1.05
x_or      <- -0.05

header_y <- min(df2$row_id) - 0.8
header_df <- tibble(row_id = header_y)

# ======================
# Harmaa-alueen pystyaluerajat
# ======================
first_ref_row <- df2 %>% 
  filter(is_ref) %>% 
  summarise(min_row = min(row_id, na.rm = TRUE)) %>% pull(min_row)

y_top_grey    <- first_ref_row - 0.5
y_bottom_grey <- max(df2$row_id) + 0.5

# ======================
# Plot
# ======================
final_plot <- ggplot(df2, aes(y = row_id)) +

  # Valkoinen tekstialue
  annotate("rect",
           xmin = text_left, xmax = text_right,
           ymin = -Inf, ymax = Inf,
           fill = "white") +

  # Harmaa forest-alue rajattuna rivien mukaan
  annotate("rect",
           xmin = forest_left, xmax = forest_right,
           ymin = y_top_grey,  ymax = y_bottom_grey,
           fill = "grey92") +

  # Sarakeotsikot (geom_text = täydellinen linjaus)
  geom_text(data = header_df,
            aes(x = x_outcome, y = row_id, label = "Outcome"),
            fontface = "bold", hjust = 0, size = 5,
            family = "Arial") +
  geom_text(data = header_df,
            aes(x = -2.0, y = row_id, label = "Private,No.(%)"),
            fontface = "bold", hjust = 1, size = 5,
            family = "Arial") +
  geom_text(data = header_df,
            aes(x = -1.05, y = row_id, label = "Public,No.(%)"),
            fontface = "bold", hjust = 1, size = 5,
            family = "Arial") +
  geom_text(data = header_df,
            aes(x = x_or, y = row_id, label = "OR (95% CI)"),
            fontface = "bold", hjust = 1, size = 5,
            family = "Arial") +

  # Ref-viiva ja katkoviiva, rajattu harmaaseen alueeseen
  geom_segment(aes(x = 1, xend = 1, y = y_bottom_grey, yend = y_top_grey),
               inherit.aes = FALSE, linetype = "solid") +
  geom_segment(aes(x = 3.5, xend = 3.5, y = y_bottom_grey, yend = y_top_grey),
               inherit.aes = FALSE, linetype = "dashed") +

  # CI-viivat
  geom_errorbarh(
    data = subset(df2, !is_header & !is_ref),
    aes(xmin = CI_low, xmax = CI_high),
    height = 0.25, size = 0.6
  ) +

  # OR-pisteet
  geom_point(
    data = subset(df2, !is_header & !is_ref),
    aes(x = OR),
    shape = 15,
    size = 4     # <-- suurenna tästä pallojen kokoa
  ) +

  # Vasemman puolen tekstit
  geom_text(aes(x = x_outcome, y = row_id,
                label = outcome,
                fontface = ifelse(is_header, "bold", "plain")),
            hjust = 0, size = 4.7, family = "Arial") +
  geom_text(aes(x = x_private, y = row_id, label = private_label),
            hjust = 1, size = 4.7, family = "Arial") +
  geom_text(aes(x = x_public, y = row_id, label = public_label),
            hjust = 1, size = 4.7, family = "Arial") +
  geom_text(aes(x = x_or, y = row_id, label = OR_label),
            hjust = 1, size = 4.7, family = "Arial") +

  # X-akseli: 0–3.5 lineaarinen, 3.5–7 puristettu
  scale_x_continuous(
    trans = compress_x,
    limits = c(text_left, 7.8),
    breaks = c(0, 1, 2, 3, 3.5, 7),
    labels = c(0, 1, 2, 3, 3.5, 7),
    expand = c(0, 0)
  ) +

  scale_y_reverse(expand = c(0,0)) +
  coord_cartesian(clip = "off") +

  theme_minimal() +
  theme(
    text = element_text(family = "Arial", color = "black"),
    axis.text.x = element_text(color = "black", size = 14),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),

  )+
labs(x = NULL, y = NULL)
# ======================
# Tallennus
# ======================

y_risk <- y_top_grey - 0.6
 p<- final_plot +
  annotate(
    "text",
    x = 0.6,              # juuri ref-viivan vasemmalle puolelle
    y = y_risk,
    label = "Lower risk",
    family = "Arial",
    fontface = "bold",
    hjust = 0.5,
    size = 5
  ) +
  annotate(
    "text",
    x = 1.95,            # tämän saat säätää halutulle kohdalle
    y = y_risk,
    label = "Higher risk for private sector",
    family = "Arial",
    fontface = "bold",
    hjust = 0.5,
    size = 5
  )
ragg::agg_png("forest.png", 
              width = 9200, height = 8970, res = 700)
print(p)
dev.off()


