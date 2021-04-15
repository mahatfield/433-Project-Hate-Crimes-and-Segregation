library(readr)
library(dplyr)
library(tidyr)
library(stringr)

seg <- read_csv("msaalld.csv")
seg <- seg %>% mutate(State = str_extract(metroname, "([A-Z])([A-Z])"),
                      City = str_remove(str_extract(metroname, "\\w.+,"), ","))

seg$m_dbw_a_00 <- NULL
seg$m_dbw_a_09 <- NULL
seg$m_dbw_a_90 <- NULL
seg$m_dbw_a_80 <- NULL
seg$m_dhw_a_00 <- NULL
seg$m_dhw_a_09 <- NULL
seg$m_dhw_a_90 <- NULL
seg$m_dhw_a_80 <- NULL
seg$m_dhb_a_00 <- NULL
seg$m_dhb_a_09 <- NULL
seg$m_dhb_a_90 <- NULL
seg$m_dhb_a_80 <- NULL
seg$m_daw_a_00 <- NULL
seg$m_daw_a_09 <- NULL
seg$m_daw_a_90 <- NULL
seg$m_daw_a_80 <- NULL
seg$m_dab_a_00 <- NULL
seg$m_dab_a_09 <- NULL
seg$m_dab_a_90 <- NULL
seg$m_dab_a_80 <- NULL
seg$m_dah_a_00 <- NULL
seg$m_dah_a_09 <- NULL
seg$m_dah_a_90 <- NULL
seg$m_dah_a_80 <- NULL


new_names <- c("metroid" = "metroID", "metroname" = "metroName", 
               "m_w_a_09" = "09WhiteCount", "m_b_a_09" = "09BlackCount", 
               "m_h_a_09" = "09HispCount", "m_a_a_09" = "09AsianCount",
               "m_o_a_09" = "09OtherCount", "m_t_a_09" = "09TotalCount",
               "m_pw_a_09" = "09WhitePerc", "m_pb_a_09" = "09BlackPerc",
               "m_ph_a_09" = "09HispPerc", "m_pa_a_09" = "09AsianPerc",
               "m_po_a_09" = "09OtherPerc", "m_dwb_a_09" = "09WhiteBlackDiss",
               "m_dwh_a_09" = "09WhiteHispDiss", "m_dwa_a_09" = "09WhiteAsianDiss",
               "m_dbh_a_09" = "09BlackHispDiss", "m_dba_a_09" = "09BlackAsianDiss",
               "m_dha_a_09" = "09HispAsianDiss",
               "m_w_a_00" = "00WhitsCount", "m_b_a_00" = "00BlackCount", 
               "m_h_a_00" = "00HispCount", "m_a_a_00" = "00AsianCount",
               "m_o_a_00" = "00OtherCount", "m_t_a_00" = "00TotalCount",
               "m_pw_a_00" = "00WhitePerc", "m_pb_a_00" = "00BlackPerc",
               "m_ph_a_00" = "00HispPerc", "m_pa_a_00" = "00AsianPerc",
               "m_po_a_00" = "00OtherPerc", "m_dwb_a_00" = "00WhiteBlackDiss",
               "m_dwh_a_00" = "00WhiteHispDiss", "m_dwa_a_00" = "00WhiteAsianDiss",
               "m_dbh_a_00" = "00BlackHispDiss", "m_dba_a_00" = "00BlackAsianDiss",
               "m_dha_a_00" = "00HispAsianDiss",
               "m_w_a_90" = "90WhitsCount", "m_b_a_90" = "90BlackCount", 
               "m_h_a_90" = "90HispCount", "m_a_a_90" = "90AsianCount",
               "m_o_a_90" = "90OtherCount", "m_t_a_90" = "90TotalCount",
               "m_pw_a_90" = "90WhitePerc", "m_pb_a_90" = "90BlackPerc",
               "m_ph_a_90" = "90HispPerc", "m_pa_a_90" = "90AsianPerc",
               "m_po_a_90" = "90OtherPerc", "m_dwb_a_90" = "90WhiteBlackDiss",
               "m_dwh_a_90" = "90WhiteHispDiss", "m_dwa_a_90" = "90WhiteAsianDiss",
               "m_dbh_a_90" = "90BlackHispDiss", "m_dba_a_90" = "90BlackAsianDiss",
               "m_dha_a_90" = "90HispAsianDiss",
               "m_w_a_80" = "80WhitsCount", "m_b_a_80" = "80BlackCount", 
               "m_h_a_80" = "80HispCount", "m_a_a_80" = "80AsianCount",
               "m_o_a_80" = "80OtherCount", "m_t_a_80" = "80TotalCount",
               "m_pw_a_80" = "80WhitePerc", "m_pb_a_80" = "80BlackPerc",
               "m_ph_a_80" = "80HispPerc", "m_pa_a_80" = "80AsianPerc",
               "m_po_a_80" = "80OtherPerc", "m_dwb_a_80" = "80WhiteBlackDiss",
               "m_dwh_a_80" = "80WhiteHispDiss", "m_dwa_a_80" = "80WhiteAsianDiss",
               "m_dbh_a_80" = "80BlackHispDiss", "m_dba_a_80" = "80BlackAsianDiss",
               "m_dha_a_80" = "80HispAsianDiss",
               "State" = "State", "City" = "City")

for (i in 1:length(colnames(seg))) {
  colnames(seg)[i] <- new_names[[colnames(seg)[i]]]
}

seg <- seg[,c(1,2,72,71,3:70)]

seg$State <- as.factor(seg$State)
seg$City <- as.factor(seg$City)
seg$metroID <- as.character(seg$metroID)

#summary(seg)
write_csv(seg, "Segregation.csv")
