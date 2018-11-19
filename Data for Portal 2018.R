# SAP Data for TAF Survey Portal (2018)




library(tidyverse)
rm(list = ls())

# import
# sap <- feather::read_feather("~/Documents/SAP 2018/Data/TAF Merge W1-13 Client Version v8.4.1.feather")
# sap.w <- survey::svydesign(id = ~1, data = sap[!is.na(sap$MergeWgt10), ], weights = ~MergeWgt10)
# save(sap.w, file = "~/Documents/SAP 2018/Data/TAF Merge W1-13 Client Version v8.4.1.RData")
# load("~/Documents/SAP 2018/Data/TAF Merge W1-13 Client Version v8.4.1.RData")

# keep questions asked in 2018
sap <- feather::read_feather("~/Documents/SAP 2018/Data/TAF Merge W1-13 Client Version v8.4.1.feather")
empty <- sapply(sap[sap$m8==2018,], function(k) all(is.na(k))); sum(empty==0)
empty["z6"] <- FALSE
sap <- sap[!empty]; rm(empty)

# remove GIZ and WB booster questions
sap <- select(sap, -starts_with("G"), starts_with("gov2stay"))
sap <- select(sap, -starts_with("w"), starts_with("wrong"), starts_with("well"), starts_with("where"))

# keep random sample / remove intercept interviews
sap <- sap[!is.na(sap$MergeWgt10) & sap$MergeWgt10>0, ]

# remove a few other unwanted variables
sap <- select(sap, -M23B, -Sample, -i1, -i2, -m1, -m2, -m5, -m9, -m10, -m11, -m12, -m13, -m14, 
              -m18, -m19, -Region, -devid, -Strata_W13, -PSU_W13, -finalwgt_W13, 
              -finalwgtNoInt_W13, -CCUniformwgt_W13, -CCProportionalwgt_W13, -MergeWgt9, -MergeWgt1, 
              -MergeWgt2, -MergeWgt3, -MergeWgt4, -MergeWgt5, -MergeWgt6, -MergeWgt7, -MergeWgt8,
              starts_with("Q"), -z13a, -z13_1)

# fix labels
for (i in names(sap)) {
  if (is.factor(sap[[i]]) & i != "AdminArea") {
    sap[[i]] <- paste0(as.numeric(sap[[i]]), ": ", sap[[i]])
    sap[[i]] <- gsub(pattern = "NA: NA", replacement = NA, x = sap[[i]])
  }
}

# multiple response questions
mr <- data.frame(a = c("right_a","wrong_a","well_a","plocal_a","pYouth_a","pWomen_a","stay_a","leave_a","where_a","relativeAbroad_a","gov2stay_a"),
           b = c("right_b","wrong_b","well_b","plocal_b","pYouth_b","pWomen_b","stay_b","leave_b","where_b","relativeAbroad_b","gov2stay_b"),
           mergename = c("x5", "x6", "x381", "x8", "x380", "x66", "z60", "z20b", "z20c", "z20e_a", "z61"),
           qname = c("q2", "q3", "q4", "q5", "q6", "q7", "q83b", "q83c", "q83d", "q90a", "q84")); mr

# responses to multiple response questions
df <- data.frame()
for (i in 1:nrow(mr)) {
  temp <- unique(c(unique(sap[[paste(mr[i,1])]]), unique(sap[[paste(mr[i,2])]]))) %>%
    data.frame() %>% 
    rename(nm = ".") %>% 
    mutate(values = gsub(":", "", word(nm, 1)), 
           n = as.numeric(values), 
           a = mr[i,1], 
           b = mr[i,2], 
           mergename = mr[i,3], 
           qname = mr[i,4],
           newvar = paste0(mergename, "_#", str_pad(n, 2, pad = "0"))) %>% 
    filter(!is.na(n)) %>% 
    arrange(n)
  df <- bind_rows(list(df, temp))
}; rm(temp); head(df,10)

# create dummy variables for mutlipe response, open-ended questions
for (i in 1:nrow(df)) {
  sap[[paste(df[i,"newvar"])]] <- sap[[paste(df[i,"a"])]]==df[i,1] | sap[[paste(df[i,"b"])]]==df[i,1]
}; rm(df); names(sap)

# remove mutlipe response, open-ended questions
sap <- select(sap, -c(as.character(mr$a), as.character(mr$b))); rm(mr); names(sap)

# categorize continuous variables
sap <- sap %>% 
  mutate(women_marriage_age = case_when(x369 <= 15 ~ "1: 10-15",
                                        x369 <= 18 ~ "2: 16-18",
                                        x369 <= 20 ~ "3: 19-20",
                                        x369 <= 25 ~ "4: 21-25",
                                        x369 <= 30 ~ "5: 26–30",
                                        x369 <= 35 ~ "6: 31–35",
                                        x369 %in% 36:38 ~ "7: 36-38",
                                        x369 == 98 ~ "8: Refused",
                                        x369 == 99 ~ "9: Don't know"),
         men_marriage_age = case_when(x370 <= 15 ~ "1: 10-15",
                                      x370 <= 18 ~ "2: 16-18",
                                      x370 <= 20 ~ "3: 19-20",
                                      x370 <= 25 ~ "4: 21-25",
                                      x370 <= 30 ~ "5: 26–30",
                                      x370 <= 35 ~ "6: 31–35",
                                      x370 %in% 35:45 ~ "7: 36-45",
                                      x370 == 98 ~ "8: Refused",
                                      x370 == 99 ~ "9: Don't know"),
         madrasa_edu_years = case_when(z40 == 0 ~ "1: 0",
                                       z40 == 1 ~ "2: 1",
                                       z40 == 2 ~ "3: 2",
                                       z40 == 3 ~ "4: 3",
                                       z40 %in% 4:5 ~ "5: 4-5",
                                       z40 %in% 6:10 ~ "6: 6-10",
                                       z40 %in% 11:18 ~ "7: 11-18",
                                       z40 == 98 ~ "8: Refused",
                                       z40 == 99 ~ "9: Don't know"),
         education = case_when(z55 %in% c(0, 97) | z6 %in% c("1: never went to school","8: informal schooling at home or at a literacy class","14: Islamic education at Madrassa") ~ "1: No formal education",
                               z55 %in% 1:6 | z6 %in% c("2: primary school, incomplete (classes 1 to 5)","3: primary school, complete (finished class 6)") ~ "2: Primary school, complete or incomplete",
                               z55 %in% 7:9 | z6 %in% c("4: secondary education, incomplete (classes 7 to 8)","5: secondary education, complete (finished class 9)") ~ "3: Secondary school, complete or incomplete",
                               z55 %in% 10:12 | z6 %in% c("6: high school (classes 10 to 12)","9: high school incomplete (classes 10-11)","10: high school complete (finished class 12)") ~ "4: High school, complete or incomplete",
                               z55 %in% 13:19 | z6 %in% c("7: university education or above","11: 14th grade incomplete (class 13)","12: 14th grade complete (finished class 14)","13: university education incomplete (have no degree diploma)") ~ "5: University education, complete or incomplete",
                               z55 == 98 | z6 == "15: refused (volunteered only)" ~ "6: Refused",
                               z55 == 99 | z6 == "16: don't know (volunteered only)" ~ "7: Don't know")); names(sap)

# remove a few other redundant variables
sap <- select(sap, -x369, -x370, -z40, -z55, -z6, -edu, -z10, -z20c_1, -z20c_2, -z20b_1, -z20b_2, -z20e_a1, -z20e_a2, 
              -respid, -sl, -Method1, -z63, -M22, -x5a, -x5b, -x6a, -x6b, -x381a, -x381b, -x8a, -x8b, -x66, -x66b, 
              -x380a, -x380b, -z60a, -z60b, -z61a, -z61b, -z64); names(sap)

# remove open ended questions that are not coded as themes
sap <- select(sap, -x412a_1, -x412a_2, -z50b)


# key questions
sap <- sap %>%
  mutate(ID = as.numeric(rownames(sap)), Sample = "General population") %>%
  rename(Wave = m8, QuestionWeight = MergeWgt10, AdminArea = m7, x277 = x277_M) %>% 
  select(ID, Wave, Sample, QuestionWeight, AdminArea, m4, z1, m6b, age, education, everything()); names(sap)

# import variable tracker
tracker <- read_excel("~/Documents/SAP 2018/Data/TAF variable tracker 1-13 v2.xlsx")

# filter out unnecessary rows (e.g. variables not asked in 2018)
tracker <- select(tracker, 1, 2, ncol(tracker)) %>% 
  mutate(X__2 = tolower(X__2),
         number = str_remove_all(X__16, "[a-zA-Z]"),
         cat = tolower(str_sub(X__16, 1,1))) %>% 
  filter(cat %in% c("m", "q", "d", "z", "w")); tracker
tracker$cat <- factor(tracker$cat, levels = c("d", "m", "q", "z", "w"), ordered = T)
tracker <- cbind(tracker, str_split(tracker$number, "_", simplify = T)) %>% 
  rename(one = `1`, two = `2`) %>% 
  mutate(one = as.numeric(as.character(one)), two = as.numeric(as.character(two))) %>% 
  arrange(cat, one, two) %>% 
  {mutate(., n = as.numeric(rownames(.)))}; head(tracker, 10)

# 
select(sap, 6:length(sap), -contains("#")) %>% names()

