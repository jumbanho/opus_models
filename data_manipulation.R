## change data frame to match 
corfu_1991_2 <- read.csv("Corfu.1991nb2.NB_RECAPS_ALL.csv")
corfu_1992_1 <- read.csv("Corfu.1992nb1.NB_RECAPS_ALL.csv")
corfu_1992_2 <- read.csv("Corfu.1992nb2.NB_RECAPS_ALL.csv")

library(dplyr)
library(tidyr)
library(stringr)
corfu_1991_2 <- mutate(corfu_1991_2, date = AGE + as.Date(REL_DATE, format = "%m/%d/%Y"))
corfu_1991_2_wide <- pivot_wider(corfu_1991_2, id_cols = c(ID:FWL,REL_DATE,SPECIES,SITE,NOTEBOOK), names_from = c(date), values_from= AGE, values_fn = length,values_fill = 0,names_repair = "universal") ## deal with NA
filter(corfu_1991_2, is.na(REL_DATE))
## ID 110 had a release date of NA
unique(corfu_1991_2$date)
## No collection on 1991-07-25?

corfu_1991_2_wide <- corfu_1991_2_wide[,c(1:9,9+order(names(corfu_1991_2_wide)[10:20]))[1:19]] ## missing dates in other times

corfu_1991_2_wide$response <-  apply(corfu_1991_2_wide[10:19],1,paste,collapse = "")
corfu_1991_2_wide$ch <- str_replace_all(corfu_1991_2_wide$response, "[1-9]", "1")
corfu_1991_2_wide <- filter(corfu_1991_2_wide, str_detect(ch,"1")) ## Data check for all zero individual

corfu_1992_1 <- mutate(corfu_1992_1, date = AGE + as.Date(REL_DATE, format = "%m/%d/%Y"))
unique(corfu_1992_1$date)
# Missing 1992-05-16
filter(corfu_1992_1, is.na(REL_DATE))
# Missing release date for ID 427, 439
corfu_1992_1_wide <- pivot_wider(corfu_1992_1, id_cols = c(ID:FWL,REL_DATE,SPECIES,SITE,NOTEBOOK), names_from = c(date), values_from= AGE, values_fn = length,values_fill = 0,names_repair = "universal") ## deal with NA
corfu_1992_1_wide <- corfu_1992_1_wide[,c(1:9,9+order(names(corfu_1992_1_wide)[10:23]))[1:22]] ## missing dates in other times

corfu_1992_1_wide$response <-  apply(corfu_1992_1_wide[10:22],1,paste,collapse = "")
corfu_1992_1_wide$ch <- str_replace_all(corfu_1992_1_wide$response, "[1-9]", "1")
corfu_1992_1_wide <- filter(corfu_1992_1_wide, str_detect(ch,"1")) ## Data check for all zero individual

corfu_1992_2 <- corfu_1992_2 %>% mutate(REL_DATE=ifelse(REL_DATE == "27-Jul", "7/27/1992",REL_DATE))
corfu_1992_2 <- mutate(corfu_1992_2, date = AGE + as.Date(REL_DATE, format = "%m/%d/%Y"))
unique(corfu_1992_2$date)
# Continuous dates!
filter(corfu_1992_2, is.na(date))
# No missing release dates!
corfu_1992_2_wide <- pivot_wider(corfu_1992_2, id_cols = c(ID:FWL,REL_DATE,SPECIES,SITE,NOTEBOOK), names_from = c(date), values_from= AGE, values_fn = length,values_fill = 0,names_repair = "universal") ## deal with NA
dim(corfu_1992_2_wide)
corfu_1992_2_wide <- corfu_1992_2_wide[,c(1:9,9+order(names(corfu_1992_2_wide)[10:18]))] ## missing dates in other times

corfu_1992_2_wide$response <-  apply(corfu_1992_2_wide[10:18],1,paste,collapse = "")
corfu_1992_2_wide$ch <- str_replace_all(corfu_1992_2_wide$response, "[1-9]", "1")
#corfu_1992_2_wide <- filter(corfu_1992_2_wide, str_detect(ch,"2")) ## Data check for all zero individual


corfu_1991_2_long <- corfu_1991_2_wide %>% pivot_longer(..1991.07.16:..1991.07.26, names_to = "date", values_to = "capture")
corfu_1991_2_long$date <- as.Date(corfu_1991_2_long$date, format = "..%Y.%m.%d")
corfu_1991_2_long <- corfu_1991_2_long %>% mutate(capture = ifelse(capture > 0, 1, 0))
