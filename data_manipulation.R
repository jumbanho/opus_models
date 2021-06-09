corfu1 <- read.csv("Corfu.1991nb2.NB_RECAPS_ALL.csv")
corfu2 <- read.csv("Corfu.1992nb1.NB_RECAPS_ALL.csv")
corfu3 <- read.csv("Corfu.1992nb2.NB_RECAPS_ALL.csv")

library(dplyr)
library(tidyr)
library(stringr)
corfu1 <- mutate(corfu1, date = AGE + as.Date(REL_DATE, format = "%m/%d/%Y"))
corfu1_wide <- pivot_wider(corfu1, id_cols = c(ID:FWL,REL_DATE,SPECIES,SITE,NOTEBOOK), names_from = c(date), values_from= AGE, values_fn = length,values_fill = 0,names_repair = "universal") ## deal with NA
filter(corfu1, is.na(REL_DATE))
## ID 110 had a release date of NA
unique(corfu1$date)
## No collection on 1991-07-25?

corfu1_wide <- corfu1_wide[,c(1:9,9+order(names(corfu1_wide)[10:20]))[1:19]] ## missing dates in other times

corfu1_wide$response <-  apply(corfu1_wide[10:19],1,paste,collapse = "")
corfu1_wide$ch <- str_replace_all(corfu1_wide$response, "[1-9]", "1")
corfu1_wide <- filter(corfu1_wide, str_detect(ch,"1")) ## Data check for all zero individual

corfu2 <- mutate(corfu2, date = AGE + as.Date(REL_DATE, format = "%m/%d/%Y"))
unique(corfu2$date)
# Missing 1992-05-16
filter(corfu2, is.na(REL_DATE))
# Missing release date for ID 427, 439
corfu2_wide <- pivot_wider(corfu2, id_cols = c(ID:FWL,REL_DATE,SPECIES,SITE,NOTEBOOK), names_from = c(date), values_from= AGE, values_fn = length,values_fill = 0,names_repair = "universal") ## deal with NA
corfu2_wide <- corfu2_wide[,c(1:9,9+order(names(corfu2_wide)[10:23]))[1:22]] ## missing dates in other times

corfu2_wide$response <-  apply(corfu2_wide[10:22],1,paste,collapse = "")
corfu2_wide$ch <- str_replace_all(corfu2_wide$response, "[1-9]", "1")
corfu2_wide <- filter(corfu2_wide, str_detect(ch,"1")) ## Data check for all zero individual

corfu3 <- corfu3 %>% mutate(REL_DATE=ifelse(REL_DATE == "27-Jul", "7/27/1992",REL_DATE))
corfu3 <- mutate(corfu3, date = AGE + as.Date(REL_DATE, format = "%m/%d/%Y"))
unique(corfu3$date)
# Continuous dates!
filter(corfu3, is.na(date))
# No missing release dates!
corfu3_wide <- pivot_wider(corfu3, id_cols = c(ID:FWL,REL_DATE,SPECIES,SITE,NOTEBOOK), names_from = c(date), values_from= AGE, values_fn = length,values_fill = 0,names_repair = "universal") ## deal with NA
dim(corfu3_wide)
corfu3_wide <- corfu3_wide[,c(1:9,9+order(names(corfu3_wide)[10:18]))] ## missing dates in other times

corfu3_wide$response <-  apply(corfu3_wide[10:18],1,paste,collapse = "")
corfu3_wide$ch <- str_replace_all(corfu3_wide$response, "[1-9]", "1")
corfu3_wide <- filter(corfu3_wide, str_detect(ch,"2")) ## Data check for all zero individual


corfu1_long <- corfu1_wide %>% pivot_longer(..1991.07.16:..1991.07.26, names_to = "date", values_to = "capture")
corfu1_long$date <- as.Date(corfu1_long$date, format = "..%Y.%m.%d")
corfu1_long <- corfu1_long %>% mutate(capture = ifelse(capture > 0, 1, 0))
