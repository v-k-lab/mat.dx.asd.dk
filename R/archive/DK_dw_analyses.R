
### Loading the required librabies
```{r include=FALSE}
library(dplyr)
library(stringr)
library(tidyr)
library(survival)
library(rms)
library(lubridate)
library(openxlsx)
```


### Loading the data sets
While loading the data sets, the variable PID in fam_anc was renamed to CID to adapt to the code.
The function data.table::fread() has been used to read the tables

```{r}
fam_anc <- data.table::fread('D:/Data/Workdata/703934/EliasA/Create dataset/Data/fam_anc.csv') %>% rename(CID = PID)
demographics <- data.table::fread('D:/Data/Workdata/703934/EliasA/Create dataset/Data/demographics.csv')
health_cond <- data.table::fread('D:/Data/Workdata/703934/EliasA/Create dataset/Data/health_cond.csv')
birth <- data.table::fread('D:/Data/Workdata/703934/EliasA/Create dataset/Data/birth.csv')
ASD <- data.table::fread('D:/Data/Workdata/703934/EliasA/Create dataset/Data/ASD.csv')
```


###Limiting dx to ICD-10 only
```{r}
health_cond <- health_cond[health_cond$COND_DXSYS==3,]
```

### Loading the data sets
While loading the data sets, the variable PID in fam_anc was renamed to CID to adapt to the code.
The function data.table::fread() has been used to read the tables

### Merging birth table with a) family ancestors, b) demographics, and c) maternal health conditions
```{r}
birth_cond <- (birth %>% left_join(fam_anc, by = "CID") %>%
                 left_join(health_cond, by = c("MID" = "PID")) %>%
                 left_join(demographics, by = c("CID" = "PID")))
```

### Formatting the date variables, YYYY-MM-DD 
```{r}
birth_cond$DOB <- as.Date(birth_cond$DOB, "%Y-%m-%d")
birth_cond$DATE_DX <- as.Date(birth_cond$DATE_DX, "%Y-%m-%d")
```

### This is not required as all children were born in 1998 or after
```{r}
length(birth_cond$CID) #before filtering
birth_cond <- filter(birth_cond, DOB > as.Date('1997-12-31'))
length(birth_cond$CID) #after filtering
```

### Checking distinct number of children in the sample
```{r}
n_distinct(birth_cond$CID, na.rm = FALSE)
```

### Checking distinct number of mothers in the sample
```{r}
n_distinct(birth_cond$MID, na.rm = FALSE)
```

### Creating a variable capturing the time difference between maternal diagnosis and date of delivery (child's DOB)
```{r}
birth_cond$date_dif <- as.numeric (difftime(birth_cond$DATE_DX, birth_cond$DOB , units = c("days")))
```

### function for filtering out diagnosis based on time period
```{r}
dx_period <- function(source_database, a, b){
  
  df <- mutate(source_database, dx_yn=ifelse(date_dif < a & date_dif >= b, 1, 0))
  df <- filter(df, dx_yn==1)
  return(df)
}
```


### filtering out all dxs that did not occur during the 12 months before the birth period
```{r}
birth_cond_00_12 <- dx_period(birth_cond, 0, -365)
```

### filtering out all dxs that did not occur during the 12-24 months before the birth period
```{r}
birth_cond_12_24 <- dx_period(birth_cond, -365, -730)
```

### filtering out all dxs that did not occur during the 24-36 months before the birth period
```{r}
birth_cond_24_36 <- dx_period(birth_cond, -730, -1095)
```

### filtering out all dxs that did not occur during the 36-48 months before the birth period
```{r}
birth_cond_36_48 <- dx_period(birth_cond, -1095, -1460)
```

### filtering out all dxs that did not occur during the 24-48 months before the birth period
```{r}
birth_cond_24_48 <- dx_period(birth_cond, -730, -1460)
```

### filtering out all dxs that did not occur during the 48 months before the birth period
```{r}
birth_cond_00_48 <- dx_period(birth_cond, 0, -1460)
```

### Type of ICD codes used for the relevant diagnoses in the sample
```{r}
table(birth_cond_00_12$COND_DXSYS)
```


# count number of all unique diagnostic/medical codes (the finest available cateogry) in pregnancy (0-12 months)
# count number of number of days with a registered diagnostic/medical in pregnancy (0-12 months)
```{r}
birth_cond_00_12$COND_CODE <- str_sub(birth_cond_00_12$COND_CODE, 1, 4)
birth_cond_00_12_temp_1 <- distinct(birth_cond_00_12, CID, COND_CODE, .keep_all = TRUE)
count_p1 <- as.data.frame(birth_cond_00_12_temp_1 %>% group_by(CID) %>% filter (dx_yn==1) %>% tally())         
count_p1$p1_all <- count_p1$n
count_p1$n <- NULL

birth_cond_00_12_temp_2 <- distinct(birth_cond_00_12, CID, DATE_DX, .keep_all = TRUE)
count_p2 <- as.data.frame(birth_cond_00_12_temp_2 %>% group_by(CID) %>% filter (dx_yn==1) %>% tally())         
count_p2$p2_all <- count_p2$n
count_p2$n <- NULL
birth_cond_00_12_temp_1 <- NULL
birth_cond_00_12_temp_2 <- NULL
```

### Removing the "D" that is in front of ICD-10 codes (This assumes that all codes are ICD-10, but will be modified as we have the results from the table obtained by the R code right above this line)
### Removing ICD-10 codes that start with "U"
```{r}
dx_cleaning <- function(source_database){
  
  source_database$start_d <- str_sub(source_database$COND_CODE, 1, 1)
  source_database <- filter(source_database, source_database$start_d == "D")
  
  source_database$COND_CODE <- str_sub(source_database$COND_CODE, 2)
  
  source_database$start_d <- str_sub(source_database$COND_CODE, 1, 1)
  df <- filter(source_database, source_database$start_d != "U")
  
  return(df)
}

birth_cond_00_12 <- dx_cleaning (birth_cond_00_12)
birth_cond_12_24 <- dx_cleaning (birth_cond_12_24)
birth_cond_24_36 <- dx_cleaning (birth_cond_24_36)
birth_cond_36_48 <- dx_cleaning (birth_cond_36_48)
birth_cond_24_48 <- dx_cleaning (birth_cond_24_48)
birth_cond_00_48 <- dx_cleaning (birth_cond_00_48)
```

### creating level 3 ICD-10 codes
```{r}
birth_cond_00_12$COND_CODE <- str_sub(birth_cond_00_12$COND_CODE, 1, 3)
birth_cond_12_24$COND_CODE <- str_sub(birth_cond_12_24$COND_CODE, 1, 3)
birth_cond_24_36$COND_CODE <- str_sub(birth_cond_24_36$COND_CODE, 1, 3)
birth_cond_36_48$COND_CODE <- str_sub(birth_cond_36_48$COND_CODE, 1, 3)
birth_cond_24_48$COND_CODE <- str_sub(birth_cond_24_48$COND_CODE, 1, 3)
birth_cond_00_48$COND_CODE <- str_sub(birth_cond_00_48$COND_CODE, 1, 3)
```

### adding "prefix" in front of dx
```{r}
birth_cond_00_12$COND_CODE <- paste("m0012", birth_cond_00_12$COND_CODE, sep = "_")
birth_cond_12_24$COND_CODE <- paste("m1224", birth_cond_12_24$COND_CODE, sep = "_")
birth_cond_24_36$COND_CODE <- paste("m2436", birth_cond_24_36$COND_CODE, sep = "_")
birth_cond_36_48$COND_CODE <- paste("m3648", birth_cond_36_48$COND_CODE, sep = "_")
birth_cond_24_48$COND_CODE <- paste("m2448", birth_cond_24_48$COND_CODE, sep = "_")
birth_cond_00_48$COND_CODE <- paste("m0048", birth_cond_00_48$COND_CODE, sep = "_")
```

### limiting sampel to distinct dxs per CID and cehcking number of distinct CIDs and total CIDs in long dx data
```{r}
birth_cond_00_12_a <- distinct(birth_cond_00_12, CID, COND_CODE, .keep_all = TRUE)
birth_cond_12_24_a <- distinct(birth_cond_12_24, CID, COND_CODE, .keep_all = TRUE)
birth_cond_24_36_a <- distinct(birth_cond_24_36, CID, COND_CODE, .keep_all = TRUE)
birth_cond_36_48_a <- distinct(birth_cond_36_48, CID, COND_CODE, .keep_all = TRUE)
birth_cond_24_48_a <- distinct(birth_cond_24_48, CID, COND_CODE, .keep_all = TRUE)
birth_cond_00_48_a <- distinct(birth_cond_00_48, CID, COND_CODE, .keep_all = TRUE)
```

### merging birth cohort with maternal ID, child demographic characteristics and maternal diagnoses. This makes sure that children who had mothers with no diagnosis are also in the full sample.
```{r}
birth_cond_00_12_b <- (birth %>% left_join(birth_cond_00_12_a, by = "CID") %>%
                         select(CID, COND_CODE, dx_yn))

birth_cond_12_24_b <- (birth %>% left_join(birth_cond_12_24_a, by = "CID") %>%
                         select(CID, COND_CODE, dx_yn))

birth_cond_24_36_b <- (birth %>% left_join(birth_cond_24_36_a, by = "CID") %>%
                         select(CID, COND_CODE, dx_yn))

birth_cond_36_48_b <- (birth %>% left_join(birth_cond_36_48_a, by = "CID") %>%
                         select(CID, COND_CODE, dx_yn))

birth_cond_24_48_b <- (birth %>% left_join(birth_cond_24_48_a, by = "CID") %>%
                         select(CID, COND_CODE, dx_yn))

birth_cond_00_48_b <- (birth %>% left_join(birth_cond_00_48_a, by = "CID") %>%
                         select(CID, COND_CODE, dx_yn))
```

### transferring long to wide based on CID. The column names are ICD-10 codes and "dx_yn" supplies the values
### replacing all "NA" diagnosis values with "0"
```{r}
birth_cond_0012_wide <- as.data.frame(data.table::dcast(birth_cond_00_12_b, CID ~ COND_CODE, value.var="dx_yn"))
birth_cond_0012_wide[,2:ncol(birth_cond_0012_wide)][is.na(birth_cond_0012_wide[,2:ncol(birth_cond_0012_wide)])] <- 0
length(birth_cond_0012_wide$CID)
n_distinct(birth_cond_0012_wide$CID)

birth_cond_1224_wide <- as.data.frame(data.table::dcast(birth_cond_12_24_b, CID ~ COND_CODE, value.var="dx_yn"))
birth_cond_1224_wide[,2:ncol(birth_cond_1224_wide)][is.na(birth_cond_1224_wide[,2:ncol(birth_cond_1224_wide)])] <- 0
length(birth_cond_1224_wide$CID)
n_distinct(birth_cond_1224_wide$CID)

birth_cond_2436_wide <- as.data.frame(data.table::dcast(birth_cond_24_36_b, CID ~ COND_CODE, value.var="dx_yn"))
birth_cond_2436_wide[,2:ncol(birth_cond_2436_wide)][is.na(birth_cond_2436_wide[,2:ncol(birth_cond_2436_wide)])] <- 0
length(birth_cond_2436_wide$CID)
n_distinct(birth_cond_2436_wide$CID)

birth_cond_3648_wide <- as.data.frame(data.table::dcast(birth_cond_36_48_b, CID ~ COND_CODE, value.var="dx_yn"))
birth_cond_3648_wide[,2:ncol(birth_cond_3648_wide)][is.na(birth_cond_3648_wide[,2:ncol(birth_cond_3648_wide)])] <- 0
length(birth_cond_3648_wide$CID)
n_distinct(birth_cond_3648_wide$CID)

birth_cond_2448_wide <- as.data.frame(data.table::dcast(birth_cond_24_48_b, CID ~ COND_CODE, value.var="dx_yn"))
birth_cond_2448_wide[,2:ncol(birth_cond_2448_wide)][is.na(birth_cond_2448_wide[,2:ncol(birth_cond_2448_wide)])] <- 0
length(birth_cond_2448_wide$CID)
n_distinct(birth_cond_2448_wide$CID)

birth_cond_0048_wide <- as.data.frame(data.table::dcast(birth_cond_00_48_b, CID ~ COND_CODE, value.var="dx_yn"))
birth_cond_0048_wide[,2:ncol(birth_cond_0048_wide)][is.na(birth_cond_0048_wide[,2:ncol(birth_cond_0048_wide)])] <- 0
length(birth_cond_0048_wide$CID)
n_distinct(birth_cond_0048_wide$CID)
```


### merging ASD diagnosis with the wide child indexed maternal diagnosis data
### removing the variable indicating date of first ASD
```{r}
asd_dx_merge <- function(source_database){
  df <- (ASD %>% select(CID, ASD, DO1ST_ASD) %>% right_join(source_database, by = "CID"))
  df$ASD <- as.numeric(df$ASD)
  df$ASD[is.na(df$ASD)] <- 0
  df$ASD[df$DO1ST_ASD > as.Date('2016-12-31')] <- 0
  df <- select(df, -DO1ST_ASD)
  return(df)
}

asd_0012_wide <- asd_dx_merge(birth_cond_0012_wide)
asd_1224_wide <- asd_dx_merge(birth_cond_1224_wide)
asd_2436_wide <- asd_dx_merge(birth_cond_2436_wide)
asd_3648_wide <- asd_dx_merge(birth_cond_3648_wide)
asd_2448_wide <- asd_dx_merge(birth_cond_2448_wide)
asd_0048_wide <- asd_dx_merge(birth_cond_0048_wide)
```


### aggregating maternal ICD-10 level 3 specific diagnosis prevalence by ASD category
```{r}
asd_dx_aggregate <- function(source_database){
  df <- (source_database %>%
           group_by(ASD) %>%
           summarise_all(funs(sum)))
  df <- as.data.frame(df)
  return(df)
}

asd_0012_wide <- asd_dx_aggregate(asd_0012_wide)
asd_1224_wide <- asd_dx_aggregate(asd_1224_wide)
asd_2436_wide <- asd_dx_aggregate(asd_2436_wide)
asd_3648_wide <- asd_dx_aggregate(asd_3648_wide)
asd_2448_wide <- asd_dx_aggregate(asd_2448_wide)
asd_0048_wide <- asd_dx_aggregate(asd_0048_wide)
```

#list of ICD 10 acute and chronic codes that we are intrested in (based on earlier runs in the same dataset)
```{r}
list_acute <- c("A08", "A09", "A63", "B34", "B37", "D23", "D24", "D25", "D27", "D69", "H10", "H52", "I80", "I83", "J03", "J18", "K29", "K30", "K35", "K40", "K52", "K59", "K80", "L02", "L03", "L05", "L08", "L30", "L50", "L68", "M20", "M25", "M53", "M54", "M62", "M65", "M67", "M75", "M77", "M79", "N10", "N13", "N20", "N30", "N63", "N64", "N70", "N71", "N75", "N76", "N83", "N84", "N87", "N96", "N98", "O00", "O02", "O03", "O04", "O08", "O12", "O13", "O14", "O20", "O21", "O22", "O23", "O24", "O26", "O28", "O30", "O31", "O32", "O33", "O34", "O35", "O36", "O40", "O41", "O42", "O43", "O44", "O45", "O46", "O47", "O48", "O60", "O61", "O62", "O63", "O64", "O65", "O66", "O67", "O68", "O69", "O70", "O71", "O72", "O73", "O74", "O75", "O80", "O82", "O85", "O86", "O87", "O90", "O91", "O92", "O98", "O99", "R00", "R06", "R07", "R10", "R11", "R31", "R33", "R42", "R50", "R51", "R52", "R53", "R55", "R87", "S00", "S01", "S02", "S05", "S06", "S10", "S13", "S20", "S30", "S40", "S43", "S50", "S52", "S60", "S61", "S62", "S63", "S70", "S80", "S81", "S82", "S83", "S90", "S91", "S92", "S93", "T14", "T15", "T39", "T40", "T43", "T63", "T78", "T81", "T88", "Z37", "Z39", "Z47", "Z48", "Z76", "Z90", "Z91")

list_acute <- paste("m0012", list_acute, sep = "_")

list_chronic <- c("A60", "B18", "C50", "D64", "D68", "E03", "E04", "E05", "E10", "E11", "E28", "E66", "F10", "F11", "F12", "F17", "F19", "F20", "F21", "F31", "F32", "F33", "F34", "F40", "F41", "F42", "F43", "F50", "F52", "F60", "F99", "G35", "G40", "G43", "G44", "G56", "H36", "H90", "I10", "I26", "I47", "I49", "J30", "J32", "J35", "J45", "K21", "K50", "K51", "K58", "L40", "M06", "M17", "M22", "M23", "M35", "M51", "N39", "N60", "N80", "N81", "N91", "N92", "N93", "N94", "N97", "O10", "R32")

list_chronic <- paste("m0048", list_chronic, sep = "_") # CHANGED BY ELIAS

full_list <- c(list_acute, list_chronic)
```

### removing maternal diagnosis columns with a frequency of less than 10 in either of ASD categories (i.e. ASD or no ASD). The name of remaining columns will be used for selecting variables from the full diagnoses dataframe.
```{r}
asd_0012_wide <- asd_0012_wide[,!sapply(asd_0012_wide, function(x) any(x<10))]
head(asd_0012_wide, 2) 
asd_0012_wide <- select(asd_0012_wide, -CID)
ncol(asd_0012_wide)
head(asd_0012_wide, 2)
col_0012_original <- subset(colnames(asd_0012_wide), colnames(asd_0012_wide) %in% full_list)
```

### removing maternal diagnosis columns with a frequency of less than 20 in either of ASD categories (i.e. ASD or no ASD). The name of remaining columns will be used for selecting variables from the full diagnoses dataframe.
```{r}
asd_0048_wide <- asd_0048_wide[,!sapply(asd_0048_wide, function(x) any(x<20))]
head(asd_0048_wide, 2) 
asd_0048_wide <- select(asd_0048_wide, -CID)
ncol(asd_0048_wide)
head(asd_0048_wide, 2)
col_0048_original <- subset(colnames(asd_0048_wide), colnames(asd_0048_wide) %in% list_chronic)
```

### limiting the maternal diagnoses data to only columns with a frequency of 10 or more across ASD categories (0012 month)
```{r}
dx_0012_wide_full <- birth_cond_0012_wide[, c ("CID", col_0012_original)]
length(dx_0012_wide_full$CID)
n_distinct(dx_0012_wide_full$CID)
head(dx_0012_wide_full, 2)
```

### limiting the maternal diagnoses data to only columns with a frequency of 10 or more across ASD categories (1224 month)
```{r}
col_0012 <- substr(col_0012_original, 7, 10)
col_1224 <- paste("m1224", col_0012, sep = "_")
col_1224 <- subset(col_1224, col_1224 %in% colnames(asd_1224_wide))

dx_1224_wide_full <- birth_cond_1224_wide[, c("CID", col_1224)]
length(dx_1224_wide_full$CID)
n_distinct(dx_1224_wide_full$CID)
head(dx_1224_wide_full, 2)
```

### limiting the maternal diagnoses data to only columns with a frequency of 10 or more across ASD categories (2436 month)
```{r}
col_0012 <- substr(col_0012_original, 7, 10)
col_2436 <- paste("m2436", col_0012, sep = "_")
col_2436 <- subset(col_2436, col_2436 %in% colnames(asd_2436_wide))

dx_2436_wide_full <- birth_cond_2436_wide[, c("CID", col_2436)]
length(dx_2436_wide_full$CID)
n_distinct(dx_2436_wide_full$CID)
head(dx_2436_wide_full, 2)
```

### limiting the maternal diagnoses data to only columns with a frequency of 10 or more across ASD categories (3648 month)
```{r}
col_0012 <- substr(col_0012_original, 7, 10)
col_3648 <- paste("m3648", col_0012, sep = "_")
col_3648 <- subset(col_3648, col_3648 %in% colnames(asd_3648_wide))

dx_3648_wide_full <- birth_cond_3648_wide[, c("CID",col_3648)]
length(dx_3648_wide_full$CID)
n_distinct(dx_3648_wide_full$CID)
head(dx_3648_wide_full, 2)
```

### limiting the maternal diagnoses data to only columns with a frequency of 10 or more across ASD categories (2448 month)
```{r}
col_0012 <- substr(col_0012_original, 7, 10)
col_2448 <- paste("m2448", col_0012, sep = "_")
col_2448 <- subset(col_2448, col_2448 %in% colnames(asd_2448_wide))

dx_2448_wide_full <- birth_cond_2448_wide[, c ("CID", col_2448)]
length(dx_2448_wide_full$CID)
n_distinct(dx_2448_wide_full$CID)
head(dx_2448_wide_full, 2)
```

### limiting the maternal diagnoses data to only columns with a frequency of 20 or more across ASD categories (00-48)
```{r}
dx_0048_wide_full <- birth_cond_0048_wide[, c ("CID", col_0048_original)]
length(dx_0048_wide_full$CID)
n_distinct(dx_0048_wide_full$CID)
head(dx_0048_wide_full, 2)
```


### Merging birth table with a) family ancestors, b) demographics,  c) health care utilization, and d) ASD 
```{r}
cov_asd <-  (birth %>%
               left_join(fam_anc, by = "CID") %>%
               left_join(demographics, by = c("CID" = "PID")) %>%
               left_join(count_p1, by ="CID") %>%
               left_join(count_p2, by ="CID") %>%
               left_join(ASD, by ="CID"))

### Formatting the date variables, YYYY-MM-DD 
cov_asd$DOB <- as.Date(cov_asd$DOB, "%Y-%m-%d")

cov_asd$DOBY <- as.factor(str_sub(cov_asd$DOB, 1, 4))
cov_asd[, "p1_all"][is.na(cov_asd[, "p1_all"])] <- 0
cov_asd[, "p2_all"][is.na(cov_asd[, "p2_all"])] <- 0
cov_asd$ASD[is.na(cov_asd$ASD)] <- 0

class(cov_asd$p1_all)
cov_asd$p1_all <- as.numeric(cov_asd$p1_all)
class(cov_asd$p2_all)
cov_asd$p2_all <- as.numeric(cov_asd$p2_all)

cov_asd$p1_all_c <- ntile(cov_asd$p1_all, 5)
#cov_asd$p2_all_c <- ntile(cov_asd$p2_all, 5)
cov_asd$p2_all_c <- cut(cov_asd$p2_all, breaks=c(-Inf, 0, 3, 9, Inf), labels=c("0", "1-3", "4-9", "10+"))

cov_asd$p1_all_c <- as.factor(cov_asd$p1_all_c)
cov_asd$p2_all_c <- as.factor(cov_asd$p2_all_c)

cov_asd$ASD[cov_asd$DO1ST_ASD > as.Date('2016-12-31')] <- 0
table(cov_asd$ASD)

#table(cond_all_wide_full$DO1ST_ASD)
cov_asd$DO1ST_ASD[is.na(cov_asd$DO1ST_ASD)] <- "2016-12-31"

cov_asd$DOEM_FIRST[ cov_asd$DOEM_FIRST == as.Date('0000-01-01')] <- "2017-01-01"

#table(cond_all_wide_full$DO1ST_ASD)
cov_asd$DOD[is.na(cov_asd$DOD)] <- "2016-12-31"
cov_asd <- transform(cov_asd, fu_min = pmin(DOEM_FIRST, DOLF_LATEST, DO1ST_ASD, DOD, na.rm=T))
cov_asd$time_fu <- as.numeric (difftime(cov_asd$fu_min , cov_asd$DOB , units = c("days")))

cov_asd$ASD[cov_asd$DO1ST_ASD>cov_asd$fu_min] <- 0 # ADDED BY ELIAS: If censoring happens before ASD dx, we change ASD to 0

birth_year_asd <-table(cov_asd$DOBY, cov_asd$ASD)
birth_year_asd
prop.table(birth_year_asd, 2)

cov_asd$MOM_EDU_BC <- cov_asd$MOM_EDU_B
cov_asd$MOM_EDU_BC[cov_asd$MOM_EDU_B > 1  & cov_asd$MOM_EDU_B < 4] <- 2
cov_asd$MOM_EDU_BC[cov_asd$MOM_EDU_B > 3  & cov_asd$MOM_EDU_B < 7] <- 3
cov_asd$MOM_EDU_BC[cov_asd$MOM_EDU_B > 6  & cov_asd$MOM_EDU_B < 10] <- 4


cov_asd$DAD_EDU_BC <- cov_asd$DAD_EDU_B
cov_asd$DAD_EDU_BC[cov_asd$DAD_EDU_B > 1  & cov_asd$DAD_EDU_B < 4] <- 2
cov_asd$DAD_EDU_BC[cov_asd$DAD_EDU_B > 3  & cov_asd$DAD_EDU_B < 7] <- 3
cov_asd$DAD_EDU_BC[cov_asd$DAD_EDU_B > 6  & cov_asd$DAD_EDU_B < 10] <- 4

#cov_asd$EDU <- pmax(cov_asd$MOM_EDU_BC, cov_asd$DAD_EDU_BC, na.rm=TRUE)
cov_asd$EDU_B <- cov_asd$MOM_EDU_BC
cov_asd$EDU_B <- as.factor(cov_asd$EDU_B)

#cov_asd$INCOME_B1 <- pmax(cov_asd$MOM_INCOME_B, cov_asd$DAD_INCOME_B)
#cov_asd$INCOME_B1[is.na(cov_asd$MOM_INCOME_B)] <- cov_asd$DAD_INCOME_B[is.na(cov_asd$MOM_INCOME_B)]
#cov_asd$INCOME_B1[is.na(cov_asd$DAD_INCOME_B)] <- cov_asd$MOM_INCOME_B[is.na(cov_asd$DAD_INCOME_B)]
cov_asd$INCOME_B1 <- cov_asd$MOM_INCOME_B
cov_asd$INCOME_B2 <- cov_asd$INCOME_B1^2
cov_asd$INCOME_B3 <- cov_asd$INCOME_B1^3


cov_asd$EDU_D <- cov_asd$DAD_EDU_BC
cov_asd$EDU_D <- as.factor(cov_asd$EDU_D)

cov_asd$INCOME_D1 <- cov_asd$DAD_INCOME_B
cov_asd$INCOME_D2 <- cov_asd$INCOME_D1^2
cov_asd$INCOME_D3 <- cov_asd$INCOME_D1^3
```

### Merging birth table with a) maternal health conditions (0012)
```{r}
birth_CID <- select(birth, CID)
b_dx0012_w_full <- (dx_0012_wide_full %>% right_join(birth_CID, by = "CID")) 
b_dx0012_w_full[,2:ncol(b_dx0012_w_full)][is.na(b_dx0012_w_full[,2:ncol(b_dx0012_w_full)])] <- 0
b_dx0012_w_full <- b_dx0012_w_full[, colnames(dx_0012_wide_full)]

#b_dx0012_w_full_i <-  (b_dx0012_w_full %>%
#                    left_join(cov_asd, by = "CID"))
```


### Merging birth table with a) maternal health conditions (1224)
```{r}
b_dx1224_w_full <- (dx_1224_wide_full %>% right_join(birth_CID, by = "CID")) 
b_dx1224_w_full[,2:ncol(b_dx1224_w_full)][is.na(b_dx1224_w_full[,2:ncol(b_dx1224_w_full)])] <- 0
b_dx1224_w_full <- b_dx1224_w_full[, colnames(dx_1224_wide_full)]

#b_dx1224_w_full_i <-  (b_dx1224_w_full %>%
#                    left_join(cov_asd, by = "CID"))
```

### Merging birth table with a) maternal health conditions (2436)
```{r}
b_dx2436_w_full <- (dx_2436_wide_full %>% right_join(birth_CID, by = "CID")) 
b_dx2436_w_full[,2:ncol(b_dx2436_w_full)][is.na(b_dx2436_w_full[,2:ncol(b_dx2436_w_full)])] <- 0
b_dx2436_w_full <- b_dx2436_w_full[, colnames(dx_2436_wide_full)]

#b_dx2436_w_full_i <-  (b_dx2436_w_full %>%
#                    left_join(cov_asd, by = "CID"))
```

### Merging birth table with a) maternal health conditions (3648)
```{r}
b_dx3648_w_full <- (dx_3648_wide_full %>% right_join(birth_CID, by = "CID")) 
b_dx3648_w_full[,2:ncol(b_dx3648_w_full)][is.na(b_dx3648_w_full[,2:ncol(b_dx3648_w_full)])] <- 0
b_dx3648_w_full <- b_dx3648_w_full[, colnames(dx_3648_wide_full)]

#b_dx3648_w_full_i <-  (b_dx3648_w_full %>%
#                    left_join(cov_asd, by = "CID"))
```

### Merging birth table with a) maternal health conditions (2448)
```{r}
b_dx2448_w_full <- (dx_2448_wide_full %>% right_join(birth_CID, by = "CID")) 
b_dx2448_w_full[,2:ncol(b_dx2448_w_full)][is.na(b_dx2448_w_full[,2:ncol(b_dx2448_w_full)])] <- 0
b_dx2448_w_full <- b_dx2448_w_full[, colnames(dx_2448_wide_full)]

#b_dx2448_w_full_i <-  (b_dx2448_w_full %>%
#                    left_join(cov_asd, by = "CID"))
```

### Merging birth table with a) maternal health conditions (0048)
```{r}
b_dx0048_w_full <- (dx_0048_wide_full %>% right_join(birth_CID, by = "CID")) 
b_dx0048_w_full[,2:ncol(b_dx0048_w_full)][is.na(b_dx0048_w_full[,2:ncol(b_dx0048_w_full)])] <- 0
b_dx0048_w_full <- b_dx0048_w_full[, colnames(dx_0048_wide_full)]

#b_dx0048_w_full_i <-  (b_dx0048_w_full %>%
#                   left_join(cov_asd, by = "CID"))
```

### Merging birth table with a) maternal health conditions (0012) & (0048)
```{r}
b_dx0012_0048_w_full <- (b_dx0012_w_full %>% left_join(b_dx0048_w_full, by = "CID") %>% right_join(birth_CID, by = "CID")) 
b_dx0012_0048_w_full[,2:ncol(b_dx0012_0048_w_full)][is.na(b_dx0012_0048_w_full[,2:ncol(b_dx0012_0048_w_full)])] <- 0
b_dx0012_0048_w_full <- b_dx0012_0048_w_full[, colnames(b_dx0012_0048_w_full)]

b_dx0012_0048_w_full <-  (b_dx0012_0048_w_full %>%
                            left_join(cov_asd, by = "CID"))
```

### Merging birth table with a) maternal health conditions (everything)
```{r}
b_dxall_w_full <-  (b_dx0012_w_full %>%
                      left_join(b_dx1224_w_full, by = "CID") %>%
                      left_join(b_dx2436_w_full, by = "CID") %>%
                      left_join(b_dx3648_w_full, by = "CID") %>%
                      left_join(b_dx2448_w_full, by = "CID") %>%
                      left_join(b_dx0048_w_full, by = "CID") %>%
                      left_join(cov_asd, by = "CID"))
```


```{r}
summary(b_dxall_w_full$MOM_AGEY)
sd(b_dxall_w_full$MOM_AGEY)
tapply(b_dxall_w_full$MOM_AGEY, b_dxall_w_full$ASD, summary)
tapply(b_dxall_w_full$MOM_AGEY, b_dxall_w_full$ASD, sd)

summary(b_dxall_w_full$p1_all)
sd(b_dxall_w_full$p1_all)
tapply(b_dxall_w_full$p1_all, b_dxall_w_full$ASD, summary)
tapply(b_dxall_w_full$p1_all, b_dxall_w_full$ASD, sd)

p1_all_c_asd <-table(b_dxall_w_full$p1_all_c, b_dxall_w_full$ASD)
p1_all_c_asd
prop.table(p1_all_c_asd, 2)

p2_all_c_asd <-table(b_dxall_w_full$p2_all_c, b_dxall_w_full$ASD)
p2_all_c_asd
prop.table(p2_all_c_asd, 2)

summary(b_dxall_w_full$time_fu)
tapply(b_dxall_w_full$time_fu, b_dxall_w_full$ASD, summary)

head(b_dxall_w_full, 2)

#Output of Interest (specifically, how many observations have missing value for each of the variables below)
table(b_dxall_w_full$MOM_EDU_B)
#table(b_dxall_w_full$MOM_INCOME_B)
table(b_dxall_w_full$MOM_OCCUP_B)
```

## Model 1 Model 1 Model 1 Model 1 Model 1 Model 1 Model 1 Model 1 Model 1 Model 1 Model 1 Model 1 Model 1 Model 1 Model 1
## Running Cox proportional hazard model to examine the association between diagnoses and ASD -- only birth year (Model 1)
```{r}
n <- which(colnames(b_dx0012_0048_w_full)=="PREG_ID") -1
model_1_00_12_48 <- data.frame()
for(i in 2:n){  ## for each dx, calculate HRs + CIs
  a <- coxph(Surv(time_fu, ASD)~b_dx0012_0048_w_full[,i] + strata(DOBY) + cluster(MID), data=b_dx0012_0048_w_full) # CHANGED BY ELIAS
  model_1_00_12_48[i,1] <- summary(a)$coefficients[1,6]  # p-val
  model_1_00_12_48[i,2] <- summary(a)$coefficients[1,2]  # HR
  model_1_00_12_48[i,3] <- exp(confint(a))[1,1]          # 2.5%
  model_1_00_12_48[i,4] <- exp(confint(a))[1,2]          # 97.5%
  model_1_00_12_48[i,5] <- colnames(b_dx0012_0048_w_full[i])    # icd code
}

model_1_00_12_48[,6] <- p.adjust(model_1_00_12_48[,1], method = "BH") # CHANGED BY ELIAS
model_1_00_12_48[,7] <- p.adjust(model_1_00_12_48[,1], method = "bonferroni") # CHANGED BY ELIAS
colnames(model_1_00_12_48) <- c("p-val", "HR", "2.5%", "97.5%", "ICD10", "q-val", "Bonferroni")
model_1_00_12_48 <- model_1_00_12_48[!is.na(model_1_00_12_48[,1]),] #Output of interest
```


### Save results (added by Elias)
```{r}
path_file <- "D:/Data/Workdata/703934/EliasA/Run for Vahe/Results/DK_R01_2023_03_22_EA.xlsx"
name_sheet <- "model_1_00_12_48"
wb <- createWorkbook()
addWorksheet(wb, name_sheet)
writeData(wb, name_sheet, model_1_00_12_48, colNames=TRUE, rowNames=FALSE)
saveWorkbook(wb, path_file, overwrite=TRUE)
```

## Model 2 Model 2 Model 2 Model 2 Model 2 Model 2 Model 2 Model 2 Model 2 Model 2 Model 2 Model 2 Model 2 Model 2 Model 2
## Running Cox proportional hazard model to examine the association between diagnoses and ASD -- Model 1 + sex + mom's age + health care utilization (Model 2)
```{r}
model_2_00_12_48 <- data.frame()
for(i in 2:n){  ## for each dx, calculate HRs + CIs
  a <- coxph(Surv(time_fu, ASD)~b_dx0012_0048_w_full[,i] + strata(DOBY) + SEX + rcs(MOM_AGEY, quantile(MOM_AGEY, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + p2_all_c + cluster(MID), data=b_dx0012_0048_w_full) # CHANGED BY ELIAS
  model_2_00_12_48[i,1] <- summary(a)$coefficients[1,6]  # p-val
  model_2_00_12_48[i,2] <- summary(a)$coefficients[1,2]  # HR
  model_2_00_12_48[i,3] <- exp(confint(a))[1,1]          # 2.5%
  model_2_00_12_48[i,4] <- exp(confint(a))[1,2]          # 97.5%
  model_2_00_12_48[i,5] <- colnames(b_dx0012_0048_w_full[i])    # icd code
}

model_2_00_12_48[,6] <- p.adjust(model_2_00_12_48[,1], method = "BH")
model_2_00_12_48[,7] <- p.adjust(model_2_00_12_48[,1], method = "bonferroni")
colnames(model_2_00_12_48) <- c("p-val", "HR", "2.5%", "97.5%", "ICD10", "q-val", "Bonferroni")
model_2_00_12_48 <- model_2_00_12_48[!is.na(model_2_00_12_48[,1]),] #Output of interest
```

### Save results (added by Elias)
```{r}
name_sheet <- "model_2_00_12_48"
wb <- loadWorkbook(path_file)
addWorksheet(wb, name_sheet)
writeData(wb, name_sheet, model_2_00_12_48, colNames=TRUE)
saveWorkbook(wb, path_file, overwrite=TRUE)
```

## Model 3 Model 3 Model 3 Model 3 Model 3 Model 3 Model 3 Model 3 Model 3 Model 3 Model 3 Model 3 Model 3 Model 3 Model 3
## Running Cox proportional hazard model to examine the association between diagnoses and ASD -- Model 2 + Mom's Edu (Model 3)
```{r}
model_3_00_12_48 <- data.frame()
for(i in 2:n){  ## for each dx, calculate HRs + CIs
  a <- coxph(Surv(time_fu, ASD)~b_dx0012_0048_w_full[,i] + strata(DOBY) + SEX + rcs(MOM_AGEY, quantile(MOM_AGEY, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + p2_all_c + EDU_B + cluster(MID), data=b_dx0012_0048_w_full) # CHANGED BY ELIAS
  model_3_00_12_48[i,1] <- summary(a)$coefficients[1,6]  # p-val
  model_3_00_12_48[i,2] <- summary(a)$coefficients[1,2]  # HR
  model_3_00_12_48[i,3] <- exp(confint(a))[1,1]          # 2.5%
  model_3_00_12_48[i,4] <- exp(confint(a))[1,2]          # 97.5%
  model_3_00_12_48[i,5] <- colnames(b_dx0012_0048_w_full[i])    # icd code
}

model_3_00_12_48[,6] <- p.adjust(model_3_00_12_48[,1], method = "BH")
model_3_00_12_48[,7] <- p.adjust(model_3_00_12_48[,1], method = "bonferroni")
colnames(model_3_00_12_48) <- c("p-val", "HR", "2.5%", "97.5%", "ICD10", "q-val", "Bonferroni")
model_3_00_12_48 <- model_3_00_12_48[!is.na(model_3_00_12_48[,1]),] #Output of interest
```

### Save results (added by Elias)
```{r}
name_sheet <- "model_3_00_12_48"
wb <- loadWorkbook(path_file)
addWorksheet(wb, name_sheet)
writeData(wb, name_sheet, model_3_00_12_48, colNames=TRUE)
saveWorkbook(wb, path_file, overwrite=TRUE)
```

## Model 4 Model 4 Model 4 Model 4 Model 4 Model 4 Model 4 Model 4 Model 4 Model 4 Model 4 Model 4 Model 4 Model 4 Model 4
## Running Cox proportional hazard model to examine the association between diagnoses and ASD -- Model 3 + Mom's Income (Model 4)
```{r}
model_4_00_12_48 <- data.frame()
for(i in 2:n){  ## for each dx, calculate HRs + CIs
  a <- coxph(Surv(time_fu, ASD)~b_dx0012_0048_w_full[,i] + strata(DOBY) + SEX + rcs(MOM_AGEY, quantile(MOM_AGEY, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + p2_all_c + EDU_B + rcs(INCOME_B1, quantile(INCOME_B1, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + cluster(MID), data=b_dx0012_0048_w_full) # CHANGED BY ELIAS
  model_4_00_12_48[i,1] <- summary(a)$coefficients[1,6]  # p-val
  model_4_00_12_48[i,2] <- summary(a)$coefficients[1,2]  # HR
  model_4_00_12_48[i,3] <- exp(confint(a))[1,1]          # 2.5%
  model_4_00_12_48[i,4] <- exp(confint(a))[1,2]          # 97.5%
  model_4_00_12_48[i,5] <- colnames(b_dx0012_0048_w_full[i])    # icd code
}

model_4_00_12_48[,6] <- p.adjust(model_4_00_12_48[,1], method = "BH")
model_4_00_12_48[,7] <- p.adjust(model_4_00_12_48[,1], method = "bonferroni")
colnames(model_4_00_12_48) <- c("p-val", "HR", "2.5%", "97.5%", "ICD10", "q-val", "Bonferroni")
model_4_00_12_48 <- model_4_00_12_48[!is.na(model_4_00_12_48[,1]),] #Output of interest
sig<- subset(model_4_00_12_48, model_4_00_12_48$`q-val` <0.05)
sig_dx <- sig$ICD10
sig_dx_0012 <- subset(sig_dx, sig_dx %in% list_acute) # CHANGED BY ELIAS
sig_dx_0048 <- subset(sig_dx, sig_dx %in% list_chronic) # CHANGED BY ELIAS
```

### Save results (ADDED BY ELIAS)
```{r}
name_sheet <- "model_4_00_12_48"
wb <- loadWorkbook(path_file)
addWorksheet(wb, name_sheet)
writeData(wb, name_sheet, model_4_00_12_48, colNames=TRUE)
saveWorkbook(wb, path_file, overwrite=TRUE)
```

# Code for reference
```{r}
sig_0012 <- paste(sig_dx_0012, collapse = '+')
sig_0048 <- paste(sig_dx_0048, collapse = '+')
sig_0012_icd <- substr(sig_dx_0012, 7, 10)
sig_0048_icd <- substr(sig_dx_0048, 7, 10)

dx_1224 <- paste("m1224", sig_0012_icd, sep = "_")
dx_1224 <- subset(dx_1224, dx_1224 %in% col_1224) # CHANGED BY ELIAS
dx_1224_f <- paste(dx_1224, collapse = '+')

dx_2436 <- paste("m2436", sig_0012_icd, sep = "_")
dx_2436 <- subset(dx_2436, dx_2436 %in% col_2436) # CHANGED BY ELIAS
dx_2436_f <- paste(dx_2436, collapse = '+')

dx_3648 <- paste("m3648", sig_0012_icd, sep = "_")
dx_3648 <- subset(dx_3648, dx_3648 %in% col_3648) # CHANGED BY ELIAS
dx_3648_f <- paste(dx_3648, collapse = '+')

dx_2448 <- paste("m2448", sig_0012_icd, sep = "_")
dx_2448 <- subset(dx_2448, dx_2448 %in% col_2448) # CHANGED BY ELIAS
dx_2448_f <- paste(dx_2448, collapse = '+')



as_model_11 <- as.formula(paste('Surv(time_fu, ASD)~', sig_0012, "+", sig_0048, "+ strata(DOBY) + SEX + rcs(MOM_AGEY, quantile(MOM_AGEY, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + p2_all_c + EDU_B + rcs(INCOME_B1, quantile(INCOME_B1, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + cluster(MID)")) # CHANGED BY ELIAS

as_model_11_nsex <- as.formula(paste('Surv(time_fu, ASD)~', sig_0012, "+", sig_0048, "+ strata(DOBY) + rcs(MOM_AGEY, quantile(MOM_AGEY, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + p2_all_c + EDU_B + rcs(INCOME_B1, quantile(INCOME_B1, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + cluster(MID)")) # CHANGED BY ELIAS

as_model_12 <- as.formula(paste('Surv(time_fu, ASD)~', sig_0012, "+", dx_1224_f, "+", sig_0048, "+ strata(DOBY) + SEX + rcs(MOM_AGEY, quantile(MOM_AGEY, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + p2_all_c + EDU_B + rcs(INCOME_B1, quantile(INCOME_B1, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + cluster(MID)")) # CHANGED BY ELIAS

as_model_13 <- as.formula(paste('Surv(time_fu, ASD)~', sig_0012, "+", dx_2436_f, "+", sig_0048, "+ strata(DOBY) + SEX + rcs(MOM_AGEY, quantile(MOM_AGEY, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + p2_all_c + EDU_B + rcs(INCOME_B1, quantile(INCOME_B1, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + cluster(MID)")) # CHANGED BY ELIAS

as_model_14 <- as.formula(paste('Surv(time_fu, ASD)~', sig_0012, "+", dx_3648_f, "+", sig_0048, "+ strata(DOBY) + SEX + rcs(MOM_AGEY, quantile(MOM_AGEY, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + p2_all_c + EDU_B + rcs(INCOME_B1, quantile(INCOME_B1, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + cluster(MID)")) # CHANGED BY ELIAS

as_model_15 <- as.formula(paste('Surv(time_fu, ASD)~', sig_0012, "+", dx_2448_f, "+", sig_0048, "+ strata(DOBY) + SEX + rcs(MOM_AGEY, quantile(MOM_AGEY, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + p2_all_c + EDU_B + rcs(INCOME_B1, quantile(INCOME_B1, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + cluster(MID)")) # CHANGED BY ELIAS

as_model_16 <- as.formula(paste('Surv(time_fu, ASD)~', sig_0012, "+", dx_1224_f, "+", dx_2436_f, "+", dx_3648_f, "+", sig_0048, "+ strata(DOBY) + SEX + rcs(MOM_AGEY, quantile(MOM_AGEY, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + p2_all_c + EDU_B + rcs(INCOME_B1, quantile(INCOME_B1, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + cluster(MID)")) # CHANGED BY ELIAS

#sibling analysis of model 11 + parity 
as_model_21 <- as.formula(paste('Surv(time_fu, ASD)~', sig_0012, "+", sig_0048, "+ strata(DOBY) + SEX + rcs(MOM_AGEY, quantile(MOM_AGEY, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + p2_all_c + EDU_B + rcs(INCOME_B1, quantile(INCOME_B1, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + parity + strata(MID)")) # CHANGED BY ELIAS

as_model_11_s1 <- as.formula(paste('Surv(time_fu, ASD)~', sig_0012, "+", sig_0048, "+ strata(DOBY) + SEX + rcs(MOM_AGEY, quantile(MOM_AGEY, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + p2_all_c + EDU_B + rcs(INCOME_B1, quantile(INCOME_B1, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + EDU_D + rcs(INCOME_D1, quantile(INCOME_D1, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + cluster(MID)")) # Changed by Elias: added 2 parentheses and na.rm=TRUE

as_model_11_s2 <- as.formula(paste('Surv(time_fu, ASD)~', sig_0012, "+", sig_0048, "+ strata(DOBY) + SEX + rcs(MOM_AGEY, quantile(MOM_AGEY, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + p2_all_c + EDU_B + rcs(INCOME_B1, quantile(INCOME_B1, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + parity + cluster(MID)"))

as_model_11_s2_sib <- as.formula(paste('Surv(time_fu, ASD)~', sig_0012, "+", sig_0048, "+ DOBY + SEX + rcs(MOM_AGEY, quantile(MOM_AGEY, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + p2_all_c + EDU_B + rcs(INCOME_B1, quantile(INCOME_B1, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + parity + cluster(MID)"))

as_model_11_s2_sib_nsex <- as.formula(paste('Surv(time_fu, ASD)~', sig_0012, "+", sig_0048, "+ DOBY+ rcs(MOM_AGEY, quantile(MOM_AGEY, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + p2_all_c + EDU_B + rcs(INCOME_B1, quantile(INCOME_B1, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + parity + cluster(MID)"))
```

### Models
```{r}
# Comorbidity model
model_11 <- coxph(as_model_11, data=b_dxall_w_full) # Output needed (model_11)
summary(model_11)

# Timing 12-24 
model_12 <- coxph(as_model_12, data=b_dxall_w_full)
summary(model_12)

# Timing 24-36 
model_13 <- coxph(as_model_13, data=b_dxall_w_full)
summary(model_13)

# Timing 36-48 
model_14 <- coxph(as_model_14, data=b_dxall_w_full)
summary(model_14)

# Timing 24-48 
model_15 <- coxph(as_model_15, data=b_dxall_w_full)
summary(model_15)

# Timing 12-24, 24-36, 36-48
model_16 <- coxph(as_model_16, data=b_dxall_w_full)
summary(model_16)

# Sensitivity 1-Ref & Sensitivity 1
model_11_s1_db <- b_dxall_w_full %>% filter(!is.na(EDU_D) & !is.na(INCOME_D1))
model_11_s1_ref <- coxph(as_model_11, data=model_11_s1_db) # Output needed (model_11_s1_red)
model_11_s1 <- coxph(as_model_11_s1, data=model_11_s1_db) # Output needed (model_11_s1)

# Sensitivity 2
model_11_s2_db <- b_dxall_w_full
model_11_s2_db$parity <- 1
model_11_s2_db$parity[model_11_s2_db$PARITY1 > 1] <- 2
model_11_s2_db$parity[model_11_s2_db$PARITY1 > 3] <- 3
model_11_s2_db$parity[model_11_s2_db$PARITY1 > 5] <- 4
model_11_s2_db$parity <- as.factor(model_11_s2_db$parity)
model_11_s2 <- coxph(as_model_11_s2, data=model_11_s2_db) # Output needed (model_11_s2)

# Sensitivity 3
model_11_s3_db <- model_11_s2_db %>% filter(PARITY1 == 1)
model_11_s3 <- coxph(as_model_11, data=model_11_s3_db) # Output needed (model_11_s3)

# Sensitivity 4
model_11_s4_db <- b_dxall_w_full %>% mutate(DoB_S = DOB %m+% months(-1), DoB_E = DOB %m+% months(1))
model_11_s4_db$inter1 <- interval(as.POSIXct(model_11_s4_db$DoB_S),as.POSIXct(model_11_s4_db$DoB_E))

model_11_s4_db <- (model_11_s4_db %>%
                     group_by(MID) %>%
                     arrange(int_start(inter1), .by_group = TRUE) %>%
                     mutate(overlap = (lead(int_start(inter1)) < int_end(inter1)) | (lag(int_end(inter1)) > int_start(inter1))))

model_11_s4_db$overlap[is.na(model_11_s4_db$overlap)] <- FALSE
model_11_s4_db <- filter(model_11_s4_db, overlap == FALSE)
model_11_s4 <- coxph(as_model_11, data=model_11_s4_db) # Output needed (model_11_s4)

# Sensitivity 5
model_11_s5_db <- b_dxall_w_full %>% filter((SIMPLEX1_REL1 != "M" | is.na(SIMPLEX1_REL1)) & (SIMPLEX1_REL2 != "M" | is.na(SIMPLEX1_REL1)) & (SIMPLEX1_REL3 != "M"  | is.na(SIMPLEX1_REL3)))
model_11_s5 <- coxph(as_model_11, data=model_11_s5_db) # Output needed (model_11_s5)
```

#### Save results (ADDED BY ELIAS)
```{r}
# Model 11
model_11_df <- cbind(data.frame(p_val=summary(model_11)$coefficients[,6]), data.frame(summary(model_11)$conf.int[,c(1,3,4)]))
name_sheet <- "Model 11"
wb <- loadWorkbook(path_file)
addWorksheet(wb, name_sheet)
writeData(wb, name_sheet, model_11_df, colNames=TRUE, rowNames=TRUE)
saveWorkbook(wb, path_file, overwrite=TRUE)

# Model 12
model_12_df <- cbind(data.frame(p_val=summary(model_12)$coefficients[,6]), data.frame(summary(model_12)$conf.int[,c(1,3,4)]))
name_sheet <- "Model 12"
wb <- loadWorkbook(path_file)
addWorksheet(wb, name_sheet)
writeData(wb, name_sheet, model_12_df, colNames=TRUE, rowNames=TRUE)
saveWorkbook(wb, path_file, overwrite=TRUE)

# Model 13
model_13_df <- cbind(data.frame(p_val=summary(model_13)$coefficients[,6]), data.frame(summary(model_13)$conf.int[,c(1,3,4)]))
name_sheet <- "Model 13"
wb <- loadWorkbook(path_file)
addWorksheet(wb, name_sheet)
writeData(wb, name_sheet, model_13_df, colNames=TRUE, rowNames=TRUE)
saveWorkbook(wb, path_file, overwrite=TRUE)

# Model 14
model_14_df <- cbind(data.frame(p_val=summary(model_14)$coefficients[,6]), data.frame(summary(model_14)$conf.int[,c(1,3,4)]))
name_sheet <- "Model 14"
wb <- loadWorkbook(path_file)
addWorksheet(wb, name_sheet)
writeData(wb, name_sheet, model_14_df, colNames=TRUE, rowNames=TRUE)
saveWorkbook(wb, path_file, overwrite=TRUE)

# Model 15
model_15_df <- cbind(data.frame(p_val=summary(model_15)$coefficients[,6]), data.frame(summary(model_15)$conf.int[,c(1,3,4)]))
name_sheet <- "Model 15"
wb <- loadWorkbook(path_file)
addWorksheet(wb, name_sheet)
writeData(wb, name_sheet, model_15_df, colNames=TRUE, rowNames=TRUE)
saveWorkbook(wb, path_file, overwrite=TRUE)

# Model 16
model_16_df <- cbind(data.frame(p_val=summary(model_16)$coefficients[,6]), data.frame(summary(model_16)$conf.int[,c(1,3,4)]))
name_sheet <- "Model 16"
wb <- loadWorkbook(path_file)
addWorksheet(wb, name_sheet)
writeData(wb, name_sheet, model_16_df, colNames=TRUE, rowNames=TRUE)
saveWorkbook(wb, path_file, overwrite=TRUE)

# Sensitivity 1-Ref & Sensitivity 1
model_11_s1_ref_df <- cbind(data.frame(p_val=summary(model_11_s1_ref)$coefficients[,6]), data.frame(summary(model_11_s1_ref)$conf.int[,c(1,3,4)]))
name_sheet <- "Model 11 s1 ref"
wb <- loadWorkbook(path_file)
addWorksheet(wb, name_sheet)
writeData(wb, name_sheet, model_11_s1_ref_df, colNames=TRUE, rowNames=TRUE)
saveWorkbook(wb, path_file, overwrite=TRUE)

model_11_s1_df <- cbind(data.frame(p_val=summary(model_11_s1)$coefficients[,6]), data.frame(summary(model_11_s1)$conf.int[,c(1,3,4)]))
name_sheet <- "Model 11 s1"
wb <- loadWorkbook(path_file)
addWorksheet(wb, name_sheet)
writeData(wb, name_sheet, model_11_s1_df, colNames=TRUE, rowNames=TRUE)
saveWorkbook(wb, path_file, overwrite=TRUE)

# Sensitivity 2
model_11_s2_df <- cbind(data.frame(p_val=summary(model_11_s2)$coefficients[,6]), data.frame(summary(model_11_s2)$conf.int[,c(1,3,4)]))
name_sheet <- "Model 11 s2"
wb <- loadWorkbook(path_file)
addWorksheet(wb, name_sheet)
writeData(wb, name_sheet, model_11_s2_df, colNames=TRUE, rowNames=TRUE)
saveWorkbook(wb, path_file, overwrite=TRUE)

# Sensitivity 3
model_11_s3_df <- cbind(data.frame(p_val=summary(model_11_s3)$coefficients[,6]), data.frame(summary(model_11_s3)$conf.int[,c(1,3,4)]))
name_sheet <- "Model 11 s3"
wb <- loadWorkbook(path_file)
addWorksheet(wb, name_sheet)
writeData(wb, name_sheet, model_11_s3_df, colNames=TRUE, rowNames=TRUE)
saveWorkbook(wb, path_file, overwrite=TRUE)

# Sensitivity 4
model_11_s4_df <- cbind(data.frame(p_val=summary(model_11_s4)$coefficients[,6]), data.frame(summary(model_11_s4)$conf.int[,c(1,3,4)]))
name_sheet <- "Model 11 s4"
wb <- loadWorkbook(path_file)
addWorksheet(wb, name_sheet)
writeData(wb, name_sheet, model_11_s4_df, colNames=TRUE, rowNames=TRUE)
saveWorkbook(wb, path_file, overwrite=TRUE)

# Sensitivity 5
model_11_s5_df <- cbind(data.frame(p_val=summary(model_11_s5)$coefficients[,6]), data.frame(summary(model_11_s5)$conf.int[,c(1,3,4)]))
name_sheet <- "Model 11 s5"
wb <- loadWorkbook(path_file)
addWorksheet(wb, name_sheet)
writeData(wb, name_sheet, model_11_s5_df, colNames=TRUE, rowNames=TRUE)
saveWorkbook(wb, path_file, overwrite=TRUE)
```


# will run this part later
```{r}
dx <- sig_0012_icd # ADDED BY ELIAS
cx <- sig_0048_icd
sig_0012_icd

m0012 <- sig_dx_0012
m1224 <- dx_1224
m1224
m2436 <- dx_2436
m2436
m3648 <- dx_3648
m3648
m2448 <- dx_2448
m2448

dx_list <- as.data.frame(cbind(dx, m0012, m1224, m2436, m3648, m2448))
dx_list$formula <- paste(dx_list$m0012, paste(dx_list$m1224, paste(dx_list$m2436, dx_list$m3648, sep = " + "), sep = " + "),  sep = " + ")

# 
# c0012 <- paste("m0012", sig_0048_icd, sep = "_")
# c0012 <- subset(c0012, c0012 %in% col_0012_original) 
# c0012
# 
# c1224 <- paste("m1224", sig_0048_icd, sep = "_")
# c1224 <- subset(c1224, c1224 %in% col_1224)
# c1224
# 
# c2436 <- paste("m2436", sig_0048_icd, sep = "_")
# c2436 <- subset(c2436, c2436 %in% col_2436)
# c2436
# 
# c3648 <- paste("m3648", sig_0048_icd, sep = "_")
# c3648 <- subset(c3648, c3648 %in% col_3648)
# c3648
# 
# c0048 <- sig_dx_0048
# 
# cx_list <- as.data.frame(cbind(cx, c0012, c1224, c2436, c3648))
# cx_list$formula <- paste(cx_list$c0012, paste(cx_list$c1224, paste(cx_list$c2436, cx_list$c3648, sep = " +  "), sep = " + "),  sep = " + ")



#individual acute timing
model_17 <- data.frame()
for(i in 1:length(dx)){  ## for each dx, calculate HRs + CIs Changed by Elias
  as_model_17 <- as.formula(paste('Surv(time_fu, ASD)~', dx_list[i,"formula"],  '+ strata(DOBY) + SEX + rcs(MOM_AGEY, quantile(MOM_AGEY, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + p2_all_c + EDU_B + rcs(INCOME_B1, quantile(INCOME_B1, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + cluster(MID)'))
  
  a <- coxph(as_model_17, data=b_dxall_w_full)
  model_17[(i*4)-3,1] <- summary(a)$coefficients[1,6]  # p-val
  model_17[(i*4)-3,2] <- summary(a)$coefficients[1,2]  # HR
  model_17[(i*4)-3,3] <- exp(confint(a))[1,1]          # 2.5%
  model_17[(i*4)-3,4] <- exp(confint(a))[1,2]          # 97.5%
  model_17[(i*4)-3,5] <- dx_list[i,"dx"]   # icd code
  model_17[(i*4)-2,1] <- summary(a)$coefficients[2,6]  # p-val
  model_17[(i*4)-2,2] <- summary(a)$coefficients[2,2]  # HR
  model_17[(i*4)-2,3] <- exp(confint(a))[2,1]          # 2.5%
  model_17[(i*4)-2,4] <- exp(confint(a))[2,2]          # 97.5%
  model_17[(i*4)-2,5] <- "12-24"    # icd code
  model_17[(i*4)-1,1] <- summary(a)$coefficients[3,6]  # p-val
  model_17[(i*4)-1,2] <- summary(a)$coefficients[3,2]  # HR
  model_17[(i*4)-1,3] <- exp(confint(a))[3,1]          # 2.5%
  model_17[(i*4)-1,4] <- exp(confint(a))[3,2]          # 97.5%
  model_17[(i*4)-1,5] <- "24-36"  # icd code
  model_17[(i*4)-0,1] <- summary(a)$coefficients[4,6]  # p-val
  model_17[(i*4)-0,2] <- summary(a)$coefficients[4,2]  # HR
  model_17[(i*4)-0,3] <- exp(confint(a))[4,1]          # 2.5%
  model_17[(i*4)-0,4] <- exp(confint(a))[4,2]          # 97.5%
  model_17[(i*4)-0,5] <- "36-48"  # icd code
}

# Output needed (model_17)
```

#### Save results (ADDED BY ELIAS)

```{r}
# Model 17
name_sheet <- "Model 17"
wb <- loadWorkbook(path_file)
addWorksheet(wb, name_sheet)
writeData(wb, name_sheet, model_17, colNames=TRUE, rowNames=TRUE)
saveWorkbook(wb, path_file, overwrite=TRUE)
```


```{r}
count_MID <- as.data.frame(b_dxall_w_full %>% group_by(MID) %>% tally())         # MID Sibs
count_MID$MID_freq <- count_MID$n
count_MID$n <- NULL

n_distinct(b_dxall_w_full$CID)
n_distinct(b_dxall_w_full$MID)

b_dxall_w_full_sib <- (model_11_s2_db %>% left_join(count_MID, by = "MID") %>%
                         filter(MID_freq > 1))
n_distinct(b_dxall_w_full_sib$CID)
n_distinct(b_dxall_w_full_sib$MID)

#sibling sample individual acute
# model_18 <- data.frame()
# for(i in 1:length(dx_list$m0012)){  ## for each dx, calculate HRs + CIs
# model_18_formula <- as.formula(paste('Surv(time_fu, ASD)~', dx_list[i,"m0012"],  '+ strata(DOBY) + SEX + rcs(MOM_AGEY, quantile(MOM_AGEY, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + p2_all_c + EDU_B + rcs(INCOME_B1, quantile(INCOME_B1, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + cluster(MID)' ))
#   a <- coxph(model_18_formula, data=b_dxall_w_full_sib)
#   model_18[i,1] <- summary(a)$coefficients[1,6]  # p-val
#   model_18[i,2] <- summary(a)$coefficients[1,2]  # HR
#   model_18[i,3] <- exp(confint(a))[1,1]          # 2.5%
#   model_18[i,4] <- exp(confint(a))[1,2]          # 97.5%
#   model_18[i,5] <- dx_list[i,"dx"]   # icd code
# }

#sibling analysis individual acute
# model_19 <- data.frame()
# for(i in 1:length(dx_list$m0012)){  ## for each dx, calculate HRs + CIs CHANGED BY ELIAS
# model_19_formula <- as.formula(paste('Surv(time_fu, ASD)~', dx_list[i,"m0012"],  '+ strata(DOBY) + SEX + rcs(MOM_AGEY, quantile(MOM_AGEY, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + p2_all_c + EDU_B + rcs(INCOME_B1, quantile(INCOME_B1, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + parity + strata(MID)' ))
#   a <- coxph(model_19_formula, data=b_dxall_w_full_sib)
#   model_19[i,1] <- summary(a)$coefficients[1,5]  # p-val CHANGED BY ELIAS
#   model_19[i,2] <- summary(a)$coefficients[1,2]  # HR
#   model_19[i,3] <- exp(confint(a))[1,1]          # 2.5%
#   model_19[i,4] <- exp(confint(a))[1,2]          # 97.5%
#   model_19[i,5] <- dx_list[i,"dx"]   # icd code
# }

#sibling sample acute + chronic (has parity and DOBY as factor rather than strata)
model_20 <- coxph(as_model_11_s2_sib, data=b_dxall_w_full_sib) # Output needed (model_20)
summary(model_20)

# ADDED BY ELIAS: save results
model_20_df <- cbind(data.frame(p_val=summary(model_20)$coefficients[,6]), data.frame(summary(model_20)$conf.int[,c(1,3,4)]))
name_sheet <- "Model 20"
wb <- loadWorkbook(path_file)
addWorksheet(wb, name_sheet)
writeData(wb, name_sheet, model_20_df, colNames=TRUE, rowNames=TRUE)
saveWorkbook(wb, path_file, overwrite=TRUE)

#sibling analysis acute + chronic (has parity)
model_21 <- coxph(as_model_21, data=b_dxall_w_full_sib)
summary(model_21)

# ADDED BY ELIAS: save results
model_21_df <- cbind(data.frame(p_val=summary(model_21)$coefficients[,5]), data.frame(summary(model_21)$conf.int[,c(1,3,4)]))
name_sheet <- "Model 21"
wb <- loadWorkbook(path_file)
addWorksheet(wb, name_sheet)
writeData(wb, name_sheet, model_21_df, colNames=TRUE, rowNames=TRUE)
saveWorkbook(wb, path_file, overwrite=TRUE)

#individual chronic timing
#model_22 <- data.frame()
#model_22
#for(i in 1:24){  ## for each dx, calculate HRs + CIs
#model_22 <- as.formula(paste('Surv(time_fu, ASD)~', cx_list[i,"formula"],  '+ DOBY + SEX + MOM_AGEY + #p2_all_c + EDU_B + INCOME_B1 + INCOME_B2 + INCOME_B3 + cluster(MID)' ))
#  a <- coxph(model_22, data=b_dxall_w_full)
#  model_22[(i*4)-3,1] <- summary(a)$coefficients[1,6]  # p-val
#  model_22[(i*4)-3,2] <- summary(a)$coefficients[1,2]  # HR
#  model_22[(i*4)-3,3] <- exp(confint(a))[1,1]          # 2.5%
#  model_22[(i*4)-3,4] <- exp(confint(a))[1,2]          # 97.5%
#  model_22[(i*4)-3,5] <- cx_list[i,"cx"]   # icd code
#  model_22[(i*4)-2,1] <- summary(a)$coefficients[2,6]  # p-val
#  model_22[(i*4)-2,2] <- summary(a)$coefficients[2,2]  # HR
#  model_22[(i*4)-2,3] <- exp(confint(a))[2,1]          # 2.5%
#  model_22[(i*4)-2,4] <- exp(confint(a))[2,2]          # 97.5%
#  model_22[(i*4)-2,5] <- "12-24"    # icd code
#  model_22[(i*4)-1,1] <- summary(a)$coefficients[3,6]  # p-val
#  model_22[(i*4)-1,2] <- summary(a)$coefficients[3,2]  # HR
#  model_22[(i*4)-1,3] <- exp(confint(a))[3,1]          # 2.5%
#  model_22[(i*4)-1,4] <- exp(confint(a))[3,2]          # 97.5%
#  model_22[(i*4)-1,5] <- "24-36"  # icd code
#  model_22[(i*4)-0,1] <- summary(a)$coefficients[4,6]  # p-val
#  model_22[(i*4)-0,2] <- summary(a)$coefficients[4,2]  # HR
#  model_22[(i*4)-0,3] <- exp(confint(a))[4,1]          # 2.5%
#  model_22[(i*4)-0,4] <- exp(confint(a))[4,2]          # 97.5%
#  model_22[(i*4)-0,5] <- "36-48"  # icd code
#}
```

## ADDED BY ELIAS: issues using both strata(MID) and strata(DOBY), try with DOBY instead of strata(DOBY) as this closest resembles Vahe's idea

```{r}
# model_19b <- data.frame()
# for(i in 1:length(dx_list$m0012)){  ## for each dx, calculate HRs + CIs
# model_19b_formula <- as.formula(paste('Surv(time_fu, ASD)~', dx_list[i,"m0012"],  '+ DOBY + SEX + rcs(MOM_AGEY, quantile(MOM_AGEY, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + p2_all_c + EDU_B + rcs(INCOME_B1, quantile(INCOME_B1, c(0.05, 0.35, 0.65, 0.95) + parity, na.rm=TRUE)) + strata(MID)' ))
#   a <- coxph(model_19b_formula, data=b_dxall_w_full_sib)
#   model_19b[i,1] <- summary(a)$coefficients[1,5]  # p-val
#   model_19b[i,2] <- summary(a)$coefficients[1,2]  # HR
#   model_19b[i,3] <- exp(confint(a))[1,1]          # 2.5%
#   model_19b[i,4] <- exp(confint(a))[1,2]          # 97.5%
#   model_19b[i,5] <- dx_list[i,"dx"]   # icd code
# }

#sibling sample acute + chronic (has parity and DOBY as factor rather than strata)
as_model_21b <- as.formula(paste('Surv(time_fu, ASD)~', sig_0012, "+", sig_0048, "+ DOBY + SEX + rcs(MOM_AGEY, quantile(MOM_AGEY, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + p2_all_c + EDU_B + rcs(INCOME_B1, quantile(INCOME_B1, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + parity + strata(MID)"))

as_model_21b_nsex <- as.formula(paste('Surv(time_fu, ASD)~', sig_0012, "+", sig_0048, "+ DOBY + rcs(MOM_AGEY, quantile(MOM_AGEY, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + p2_all_c + EDU_B + rcs(INCOME_B1, quantile(INCOME_B1, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + parity + strata(MID)"))

model_21b <- coxph(as_model_21b, data=b_dxall_w_full_sib) # Output needed (model_21b)

# ADDED BY ELIAS: save results
model_21b_df <- cbind(data.frame(p_val=summary(model_21b)$coefficients[,5]), data.frame(summary(model_21b)$conf.int[,c(1,3,4)]))
name_sheet <- "Model 21b"
wb <- loadWorkbook(path_file)
addWorksheet(wb, name_sheet)
writeData(wb, name_sheet, model_21b_df, colNames=TRUE, rowNames=TRUE)
saveWorkbook(wb, path_file, overwrite=TRUE)


# Sensitivity 6
b_dxall_w_full_sib_s6_db <- b_dxall_w_full_sib %>% filter(SEX == "M")
#sibling sample acute + chronic
model_20_s6 <- coxph(as_model_11_s2_sib_nsex, data=b_dxall_w_full_sib_s6_db) # Output needed (model_20_s6)
summary(model_20_s6)

# ADDED BY ELIAS: save results
model_20_s6_df <- cbind(data.frame(p_val=summary(model_20_s6)$coefficients[,6]), data.frame(summary(model_20_s6)$conf.int[,c(1,3,4)]))
name_sheet <- "Model 20 s6"
wb <- loadWorkbook(path_file)
addWorksheet(wb, name_sheet)
writeData(wb, name_sheet, model_20_s6_df, colNames=TRUE, rowNames=TRUE)
saveWorkbook(wb, path_file, overwrite=TRUE)

#sibling analysis acute + chronic
model_21_s6 <- coxph(as_model_21b_nsex, data=b_dxall_w_full_sib_s6_db) # Output needed (model_21_s6)
summary(model_21_s6)

# ADDED BY ELIAS: save results
model_21_s6_df <- cbind(data.frame(p_val=summary(model_21_s6)$coefficients[,5]), data.frame(summary(model_21_s6)$conf.int[,c(1,3,4)]))
name_sheet <- "Model 21 s6"
wb <- loadWorkbook(path_file)
addWorksheet(wb, name_sheet)
writeData(wb, name_sheet, model_21_s6_df, colNames=TRUE, rowNames=TRUE)
saveWorkbook(wb, path_file, overwrite=TRUE)
```


```{r}
# Sensitivity 7
model_11_s7m_db <- b_dxall_w_full %>% filter(SEX == "M")
model_11_s7m <- coxph(as_model_11_nsex, data=model_11_s7m_db) # Output needed (model_11_s7m)
model_11_s7f_db <- b_dxall_w_full %>% filter(SEX == "F")
model_11_s7f <- coxph(as_model_11_nsex, data=model_11_s7f_db) # Output needed (model_11_s7f)

## ADDED BY ELIAS: Save results
model_11_s7m_df <- cbind(data.frame(p_val=summary(model_11_s7m)$coefficients[,6]), data.frame(summary(model_11_s7m)$conf.int[,c(1,3,4)]))
name_sheet <- "Model 11 s7m"
wb <- loadWorkbook(path_file)
addWorksheet(wb, name_sheet)
writeData(wb, name_sheet, model_11_s7m_df, colNames=TRUE, rowNames=TRUE)
saveWorkbook(wb, path_file, overwrite=TRUE)

model_11_s7f_df <- cbind(data.frame(p_val=summary(model_11_s7f)$coefficients[,6]), data.frame(summary(model_11_s7f)$conf.int[,c(1,3,4)]))
name_sheet <- "Model 11 s7f"
wb <- loadWorkbook(path_file)
addWorksheet(wb, name_sheet)
writeData(wb, name_sheet, model_11_s7f_df, colNames=TRUE, rowNames=TRUE)
saveWorkbook(wb, path_file, overwrite=TRUE)
```


```{r}
#Sensitivity 8
model_11_s8id_db <- b_dxall_w_full %>% filter(COMORB_ID == 1)
model_11_s8id <- coxph(as_model_11, data=model_11_s8id_db) # Output needed (model_11_s8id)

model_11_s8nid_db <- b_dxall_w_full %>% filter(COMORB_ID == 0)
model_11_s8nid <- coxph(as_model_11, data=model_11_s8nid_db) # Output needed (model_11_s8nid)

## ADDED BY ELIAS: Save results
model_11_s8id_df <- cbind(data.frame(p_val=summary(model_11_s8id)$coefficients[,6]), data.frame(summary(model_11_s8id)$conf.int[,c(1,3,4)]))
name_sheet <- "Model 11 s8id"
wb <- loadWorkbook(path_file)
addWorksheet(wb, name_sheet)
writeData(wb, name_sheet, model_11_s8id_df, colNames=TRUE, rowNames=TRUE)
saveWorkbook(wb, path_file, overwrite=TRUE)

model_11_s8nid_df <- cbind(data.frame(p_val=summary(model_11_s8nid)$coefficients[,6]), data.frame(summary(model_11_s8nid)$conf.int[,c(1,3,4)]))
name_sheet <- "Model 11 s8nid"
wb <- loadWorkbook(path_file)
addWorksheet(wb, name_sheet)
writeData(wb, name_sheet, model_11_s8nid_df, colNames=TRUE, rowNames=TRUE)
saveWorkbook(wb, path_file, overwrite=TRUE)
```

# Added by Elias: including strata(MID) in model_21_s6 may result in unstable estimates. Remove it.

```{r}
as_model_21c_nsex <- as.formula(paste('Surv(time_fu, ASD)~', sig_0012, "+", sig_0048, "+ DOBY + rcs(MOM_AGEY, quantile(MOM_AGEY, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + p2_all_c + EDU_B + rcs(INCOME_B1, quantile(INCOME_B1, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + parity"))
#sibling analysis acute + chronic
model_21c_s6 <- coxph(as_model_21c_nsex, data=b_dxall_w_full_sib_s6_db) # Output needed (model_21_s6)

# Save results
model_21c_s6_df <- cbind(data.frame(p_val=summary(model_21c_s6)$coefficients[,5]), data.frame(summary(model_21c_s6)$conf.int[,c(1,3,4)]))
name_sheet <- "Model 21c s6"
wb <- loadWorkbook(path_file)
addWorksheet(wb, name_sheet)
writeData(wb, name_sheet, model_21c_s6_df, colNames=TRUE, rowNames=TRUE)
saveWorkbook(wb, path_file, overwrite=TRUE)

# Model 21b also has some quite wide CI's, so we run that without strata(MID) as well
as_model_21c <- as.formula(paste('Surv(time_fu, ASD)~', sig_0012, "+", sig_0048, "+ DOBY + SEX + rcs(MOM_AGEY, quantile(MOM_AGEY, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + p2_all_c + EDU_B + rcs(INCOME_B1, quantile(INCOME_B1, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + parity"))
model_21c <- coxph(as_model_21c, data=b_dxall_w_full_sib) # Output needed (model_21b)

# ADDED BY ELIAS: save results
model_21c_df <- cbind(data.frame(p_val=summary(model_21c)$coefficients[,5]), data.frame(summary(model_21c)$conf.int[,c(1,3,4)]))
name_sheet <- "Model 21c"
wb <- loadWorkbook(path_file)
addWorksheet(wb, name_sheet)
writeData(wb, name_sheet, model_21c_df, colNames=TRUE, rowNames=TRUE)
saveWorkbook(wb, path_file, overwrite=TRUE)
```



















#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*


DK_R01_2023_03_22_EA_fix_issues.Rmd

---
  title: "Untitled"
author: "Elias Arildskov"
date: '2023-08-23'
output: html_document
---
  
  # !!! NOTE: First run everything from DK_R01_2023_03_22_EA until the run of model 1 needs to be run.!!!
  
  # Things needed before we get started, and datasets created
  
  Check that the ASD variable is the same regardless of data set
```{r}
temp <- select(b_dx0012_0048_w_full, CID, ASD1=ASD) %>% left_join(select(cov_asd, CID, ASD2=ASD))

sum(temp$ASD1!=temp$ASD2)
```

```{r}
# The original placing of results
path_file_old <- "D:/Data/Workdata/703934/EliasA/Run for Vahe/Results/DK_R01_2023_03_22_EA.xlsx"

# The file to place the results after the check
path_file <- "D:/Data/Workdata/703934/EliasA/Run for Vahe/Results/DK_R01_2023_03_22_EA_fix_issues.xlsx"
```

## Create datasets etc. needed later

Find significant diagnoses from model 4
```{r}
model_4_00_12_48 <- readWorkbook(path_file_old, sheet="model_4_00_12_48")
sig<- subset(model_4_00_12_48, model_4_00_12_48$`q-val` <0.05)
sig_dx <- sig$ICD10
sig_dx_0012 <- subset(sig_dx, sig_dx %in% list_acute) # CHANGED BY ELIAS
sig_dx_0048 <- subset(sig_dx, sig_dx %in% list_chronic) # CHANGED BY ELIAS
```

Create vectors (and/or collapse them) with relevant information to be used in the analyses
```{r}
sig_0012 <- paste(sig_dx_0012, collapse = '+')
sig_0048 <- paste(sig_dx_0048, collapse = '+')
sig_0012_icd <- substr(sig_dx_0012, 7, 10)
sig_0048_icd <- substr(sig_dx_0048, 7, 10)

dx_1224 <- paste("m1224", sig_0012_icd, sep = "_")
dx_1224 <- subset(dx_1224, dx_1224 %in% col_1224) # CHANGED BY ELIAS
dx_1224_f <- paste(dx_1224, collapse = '+')

dx_2436 <- paste("m2436", sig_0012_icd, sep = "_")
dx_2436 <- subset(dx_2436, dx_2436 %in% col_2436) # CHANGED BY ELIAS
dx_2436_f <- paste(dx_2436, collapse = '+')

dx_3648 <- paste("m3648", sig_0012_icd, sep = "_")
dx_3648 <- subset(dx_3648, dx_3648 %in% col_3648) # CHANGED BY ELIAS
dx_3648_f <- paste(dx_3648, collapse = '+')

dx_2448 <- paste("m2448", sig_0012_icd, sep = "_")
dx_2448 <- subset(dx_2448, dx_2448 %in% col_2448) # CHANGED BY ELIAS
dx_2448_f <- paste(dx_2448, collapse = '+')
```

Create datasets 
```{r}
model_11_s1_db <- b_dxall_w_full %>% filter(!is.na(EDU_D) & !is.na(INCOME_D1))

model_11_s2_db <- b_dxall_w_full
model_11_s2_db$parity <- 1
model_11_s2_db$parity[model_11_s2_db$PARITY1 > 1] <- 2
model_11_s2_db$parity[model_11_s2_db$PARITY1 > 3] <- 3
model_11_s2_db$parity[model_11_s2_db$PARITY1 > 5] <- 4
model_11_s2_db$parity <- as.factor(model_11_s2_db$parity)

model_11_s3_db <- model_11_s2_db %>% filter(PARITY1 == 1)

model_11_s4_db <- b_dxall_w_full %>% mutate(DoB_S = DOB %m+% months(-1), DoB_E = DOB %m+% months(1))
model_11_s4_db$inter1 <- interval(as.POSIXct(model_11_s4_db$DoB_S),as.POSIXct(model_11_s4_db$DoB_E))

model_11_s4_db <- (model_11_s4_db %>%
                     group_by(MID) %>%
                     arrange(int_start(inter1), .by_group = TRUE) %>%
                     mutate(overlap = (lead(int_start(inter1)) < int_end(inter1)) | (lag(int_end(inter1)) > int_start(inter1))))

model_11_s4_db$overlap[is.na(model_11_s4_db$overlap)] <- FALSE
model_11_s4_db <- filter(model_11_s4_db, overlap == FALSE)

# Sensitivity 5
model_11_s5_db <- b_dxall_w_full %>% filter((SIMPLEX1_REL1 != "M" | is.na(SIMPLEX1_REL1)) & (SIMPLEX1_REL2 != "M" | is.na(SIMPLEX1_REL1)) & (SIMPLEX1_REL3 != "M"  | is.na(SIMPLEX1_REL3)))
```

Prepare/collapse more vectors
```{r}
dx <- sig_0012_icd # ADDED BY ELIAS
cx <- sig_0048_icd
sig_0012_icd

m0012 <- sig_dx_0012
m1224 <- dx_1224
m1224
m2436 <- dx_2436
m2436
m3648 <- dx_3648
m3648
m2448 <- dx_2448
m2448

dx_list <- as.data.frame(cbind(dx, m0012, m1224, m2436, m3648, m2448))
dx_list$formula <- paste(dx_list$m0012, paste(dx_list$m1224, paste(dx_list$m2436, dx_list$m3648, sep = " + "), sep = " + "),  sep = " + ")
```

Create more datasets
```{r}
count_MID <- as.data.frame(b_dxall_w_full %>% group_by(MID) %>% tally())         # MID Sibs
count_MID$MID_freq <- count_MID$n
count_MID$n <- NULL

b_dxall_w_full_sib <- (model_11_s2_db %>% left_join(count_MID, by = "MID") %>%
                         filter(MID_freq > 1))
```

```{r}
b_dxall_w_full_sib_s6_db <- b_dxall_w_full_sib %>% filter(SEX == "M")
```





# Check if we have any issues, and save those without, rerun the others

## Model 1 and 2

Check if everything is okay
```{r}
temp <- b_dx0012_0048_w_full
n <- which(colnames(temp)=="PREG_ID") -1
model_1 <- rep(NA, n-1)
asd_indx <- which(colnames(temp)=="ASD")
for(i in 2:n){
  model_1[i-1] <- min(table(temp[,c(i, asd_indx)]))
}
min(model_1)
```

Save the "approved" models
```{r}
# Model 1
name_sheet <- "model_1_00_12_48"

df_model <- readWorkbook(path_file_old, sheet=name_sheet)

wb <- createWorkbook()
addWorksheet(wb, name_sheet)
writeData(wb, name_sheet, df_model, colNames=TRUE, rowNames=FALSE)
saveWorkbook(wb, path_file, overwrite=TRUE)

# Model 2
name_sheet <- "model_2_00_12_48"

df_model <- readWorkbook(path_file_old, sheet=name_sheet)

addWorksheet(wb, name_sheet)
writeData(wb, name_sheet, df_model, colNames=TRUE, rowNames=FALSE)
saveWorkbook(wb, path_file, overwrite=TRUE)
```

## Model 3

Check if everything is okay
```{r}
temp <- b_dx0012_0048_w_full %>% filter(!is.na(EDU_B))
model_3 <- rep(NA, n-1)
asd_indx <- which(colnames(temp)=="ASD")
for(i in 2:n){
  model_3[i-1] <- min(table(temp[,c(i, asd_indx)]))
}
min(model_3)
```

Save the "approved" model
```{r}
# Model 3
name_sheet <- "model_3_00_12_48"

df_model <- readWorkbook(path_file_old, sheet=name_sheet)

addWorksheet(wb, name_sheet)
writeData(wb, name_sheet, df_model, colNames=TRUE, rowNames=FALSE)
saveWorkbook(wb, path_file, overwrite=TRUE)
```

## Model 4

Check if everything is okay
```{r}
temp <- b_dx0012_0048_w_full %>% filter(!is.na(EDU_B) & !is.na(INCOME_B1))
model_4 <- rep(NA, n-1)
asd_indx <- which(colnames(temp)=="ASD")
for(i in 2:n){
  model_4[i-1] <- min(table(temp[,c(i, asd_indx)]))
}
min(model_4)
```

Save the "approved" model
```{r}
# Model 4
name_sheet <- "model_4_00_12_48"

df_model <- readWorkbook(path_file_old, sheet=name_sheet)

addWorksheet(wb, name_sheet)
writeData(wb, name_sheet, df_model, colNames=TRUE, rowNames=FALSE)
saveWorkbook(wb, path_file, overwrite=TRUE)
```

## Model 11

Check if everything is okay
```{r}
temp <- b_dxall_w_full %>% filter(!is.na(EDU_B) & !is.na(INCOME_B1))
temp_dx <- c(sig_dx)
model_11 <- rep(NA, length(temp_dx))
for(i in 1:length(temp_dx)){
  model_11[i] <- min(table(temp[,c(temp_dx[i], "ASD")]))
}
min(model_11)
```

Save the "approved" model
```{r}
name_sheet <- "Model 11"

df_model <- readWorkbook(path_file_old, sheet=name_sheet)
df_model <- df_model[substr(unlist(df_model[,1]), start=1, stop=3)=="m00",]

addWorksheet(wb, name_sheet)
writeData(wb, name_sheet, df_model, colNames=TRUE, rowNames=FALSE)
saveWorkbook(wb, path_file, overwrite=TRUE)
```

## Model 12

Check if everything is okay
```{r}
temp <- b_dxall_w_full %>% filter(!is.na(EDU_B) & !is.na(INCOME_B1))
temp_dx <- c(sig_dx, dx_1224)
model_12 <- rep(NA, length(temp_dx))
for(i in 1:length(temp_dx)){
  model_12[i] <- min(table(temp[,c(temp_dx[i], "ASD")]))
}
min(model_12)
```

Save the "approved" model
```{r}
name_sheet <- "Model 12"

df_model <- readWorkbook(path_file_old, sheet=name_sheet)
df_model <- df_model[substr(unlist(df_model[,1]), start=1, stop=1)=="m",]

addWorksheet(wb, name_sheet)
writeData(wb, name_sheet, df_model, colNames=TRUE, rowNames=FALSE)
saveWorkbook(wb, path_file, overwrite=TRUE)
```


## Model 13

Check if everything is okay
```{r}
temp <- b_dxall_w_full %>% filter(!is.na(EDU_B) & !is.na(INCOME_B1))
temp_dx <- c(sig_dx, dx_2436)
model_13 <- rep(NA, length(temp_dx))
for(i in 1:length(temp_dx)){
  model_13[i] <- min(table(temp[,c(temp_dx[i], "ASD")]))
}
min(model_13)
```

Save the "approved" model
```{r}
name_sheet <- "Model 13"

df_model <- readWorkbook(path_file_old, sheet=name_sheet)
df_model <- df_model[substr(unlist(df_model[,1]), start=1, stop=1)=="m",]

addWorksheet(wb, name_sheet)
writeData(wb, name_sheet, df_model, colNames=TRUE, rowNames=FALSE)
saveWorkbook(wb, path_file, overwrite=TRUE)
```

## Model 14

Check if everything is okay
```{r}
temp <- b_dxall_w_full %>% filter(!is.na(EDU_B) & !is.na(INCOME_B1))
temp_dx <- c(sig_dx, dx_3648)
model_14 <- rep(NA, length(temp_dx))
for(i in 1:length(temp_dx)){
  model_14[i] <- min(table(temp[,c(temp_dx[i], "ASD")]))
}
min(model_14)
```

Save the "approved" model
```{r}
name_sheet <- "Model 14"

df_model <- readWorkbook(path_file_old, sheet=name_sheet)
df_model <- df_model[substr(unlist(df_model[,1]), start=1, stop=1)=="m",]

addWorksheet(wb, name_sheet)
writeData(wb, name_sheet, df_model, colNames=TRUE, rowNames=FALSE)
saveWorkbook(wb, path_file, overwrite=TRUE)
```

## Model 15

Check if everything is okay
```{r}
temp <- b_dxall_w_full %>% filter(!is.na(EDU_B) & !is.na(INCOME_B1))
temp_dx <- c(sig_dx, dx_2448)
model_15 <- rep(NA, length(temp_dx))
for(i in 1:length(temp_dx)){
  model_15[i] <- min(table(temp[,c(temp_dx[i], "ASD")]))
}
min(model_15)
```

Save the "approved" model
```{r}
name_sheet <- "Model 15"

df_model <- readWorkbook(path_file_old, sheet=name_sheet)
df_model <- df_model[substr(unlist(df_model[,1]), start=1, stop=1)=="m",]

addWorksheet(wb, name_sheet)
writeData(wb, name_sheet, df_model, colNames=TRUE, rowNames=FALSE)
saveWorkbook(wb, path_file, overwrite=TRUE)
```

## Model 16

Follows from model 12-14 that these diagnoses are okay

Save the "approved" model
```{r}
name_sheet <- "Model 16"

df_model <- readWorkbook(path_file_old, sheet=name_sheet)
df_model <- df_model[substr(unlist(df_model[,1]), start=1, stop=1)=="m",]

addWorksheet(wb, name_sheet)
writeData(wb, name_sheet, df_model, colNames=TRUE, rowNames=FALSE)
saveWorkbook(wb, path_file, overwrite=TRUE)
```

## Model 11 s1 ref

Check if everything is okay
```{r}
temp <- model_11_s1_db %>% filter(!is.na(EDU_B) & !is.na(INCOME_B1))
temp_dx <- c(sig_dx)
model_11_s1_ref <- rep(NA, length(temp_dx))
for(i in 1:length(temp_dx)){
  model_11_s1_ref[i] <- min(table(temp[,c(temp_dx[i], "ASD")]))
}
min(model_11_s1_ref)
```

Save the "approved" model
```{r}
name_sheet <- "Model 11 s1 ref"

df_model <- readWorkbook(path_file_old, sheet=name_sheet)
df_model <- df_model[substr(unlist(df_model[,1]), start=1, stop=1)=="m",]

addWorksheet(wb, name_sheet)
writeData(wb, name_sheet, df_model, colNames=TRUE, rowNames=FALSE)
saveWorkbook(wb, path_file, overwrite=TRUE)
```

## Model 11 s1

Check if everything is okay
```{r}
temp <- model_11_s1_db %>% filter(!is.na(EDU_B) & !is.na(INCOME_B1) & !is.na(EDU_D) & !is.na(INCOME_D1))
temp_dx <- c(sig_dx)
model_11_s1 <- rep(NA, length(temp_dx))
for(i in 1:length(temp_dx)){
  model_11_s1[i] <- min(table(temp[,c(temp_dx[i], "ASD")]))
}
min(model_11_s1)
```

Save the "approved" model
```{r}
name_sheet <- "Model 11 s1"

df_model <- readWorkbook(path_file_old, sheet=name_sheet)
df_model <- df_model[substr(unlist(df_model[,1]), start=1, stop=1)=="m",]

addWorksheet(wb, name_sheet)
writeData(wb, name_sheet, df_model, colNames=TRUE, rowNames=FALSE)
saveWorkbook(wb, path_file, overwrite=TRUE)
```

## Model 11 s2

Check if everything is okay
```{r}
temp <- model_11_s2_db %>% filter(!is.na(EDU_B) & !is.na(INCOME_B1) & !is.na(parity))
temp_dx <- c(sig_dx)
model_11_s2 <- rep(NA, length(temp_dx))
for(i in 1:length(temp_dx)){
  model_11_s2[i] <- min(table(temp[,c(temp_dx[i], "ASD")]))
}
min(model_11_s2)
```

Save the "approved" model
```{r}
name_sheet <- "Model 11 s2"

df_model <- readWorkbook(path_file_old, sheet=name_sheet)
df_model <- df_model[substr(unlist(df_model[,1]), start=1, stop=1)=="m",]

addWorksheet(wb, name_sheet)
writeData(wb, name_sheet, df_model, colNames=TRUE, rowNames=FALSE)
saveWorkbook(wb, path_file, overwrite=TRUE)
```

## Model 11 s3

Check if everything is okay
```{r}
temp <- model_11_s3_db %>% filter(!is.na(EDU_B) & !is.na(INCOME_B1) & !is.na(parity))
temp_dx <- c(sig_dx)
model_11_s3 <- rep(NA, length(temp_dx))
for(i in 1:length(temp_dx)){
  model_11_s3[i] <- min(table(temp[,c(temp_dx[i], "ASD")]))
}
min(model_11_s3)
```

Save the "approved" model
```{r}
name_sheet <- "Model 11 s3"

df_model <- readWorkbook(path_file_old, sheet=name_sheet)
df_model <- df_model[substr(unlist(df_model[,1]), start=1, stop=1)=="m",]

addWorksheet(wb, name_sheet)
writeData(wb, name_sheet, df_model, colNames=TRUE, rowNames=FALSE)
saveWorkbook(wb, path_file, overwrite=TRUE)
```


## Model 11 s4

Check if everything is okay
```{r}
temp <- model_11_s4_db %>% filter(!is.na(EDU_B) & !is.na(INCOME_B1))
temp_dx <- c(sig_dx)
model_11_s4 <- rep(NA, length(temp_dx))
for(i in 1:length(temp_dx)){
  model_11_s4[i] <- min(table(temp[,c(temp_dx[i], "ASD")]))
}
min(model_11_s4)
```

Save the "approved" model
```{r}
name_sheet <- "Model 11 s4"

df_model <- readWorkbook(path_file_old, sheet=name_sheet)
df_model <- df_model[substr(unlist(df_model[,1]), start=1, stop=1)=="m",]

addWorksheet(wb, name_sheet)
writeData(wb, name_sheet, df_model, colNames=TRUE, rowNames=FALSE)
saveWorkbook(wb, path_file, overwrite=TRUE)
```


## Model 11 s5

Check if everything is okay
```{r}
temp <- model_11_s5_db %>% filter(!is.na(EDU_B) & !is.na(INCOME_B1))
temp_dx <- c(sig_dx)
model_11_s5 <- rep(NA, length(temp_dx))
for(i in 1:length(temp_dx)){
  model_11_s5[i] <- min(table(temp[,c(temp_dx[i], "ASD")]))
}
min(model_11_s5)
```

Save the "approved" model
```{r}
name_sheet <- "Model 11 s5"

df_model <- readWorkbook(path_file_old, sheet=name_sheet)
df_model <- df_model[substr(unlist(df_model[,1]), start=1, stop=1)=="m",]

addWorksheet(wb, name_sheet)
writeData(wb, name_sheet, df_model, colNames=TRUE, rowNames=FALSE)
saveWorkbook(wb, path_file, overwrite=TRUE)
```


## Model 17

It has been checked in model 11-15 that these diagnoses are not violating any rules

Save the "approved" model
```{r}
name_sheet <- "Model 17"

df_model <- readWorkbook(path_file_old, sheet=name_sheet)
df_model <- df_model[, 2:ncol(df_model)]

addWorksheet(wb, name_sheet)
writeData(wb, name_sheet, df_model, colNames=FALSE, rowNames=FALSE)
saveWorkbook(wb, path_file, overwrite=TRUE)
```

## Model 20, 21 og 21b

Check if everything is okay
```{r}
temp <- b_dxall_w_full_sib %>% filter(!is.na(EDU_B) & !is.na(INCOME_B1) & !is.na(parity))
temp_dx <- c(sig_dx)
model_20 <- rep(NA, length(temp_dx))
for(i in 1:length(temp_dx)){
  model_20[i] <- min(table(temp[,c(temp_dx[i], "ASD")]))
}
min(model_20)

model_20_dx <- temp_dx[model_20>=5]
```

§§§§§§§§§§§§§§ MODEL 20

Correct potential issue with too few in the above
```{r}
# original: as_model_11_s2_sib <- as.formula(paste('Surv(time_fu, ASD)~', sig_0012, "+", sig_0048, "+ DOBY + SEX + rcs(MOM_AGEY, quantile(MOM_AGEY, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + p2_all_c + EDU_B + rcs(INCOME_B1, quantile(INCOME_B1, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + parity + cluster(MID)"))

as_model_11_s2_sib <- as.formula(paste('Surv(time_fu, ASD)~', paste(model_20_dx, collapse = '+'), "+ DOBY + SEX + rcs(MOM_AGEY, quantile(MOM_AGEY, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + p2_all_c + EDU_B + rcs(INCOME_B1, quantile(INCOME_B1, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + parity + cluster(MID)"))

model_20 <- coxph(as_model_11_s2_sib, data=b_dxall_w_full_sib) # Output needed (model_20)

```

Save the "approved" model
```{r}
name_sheet <- "Model 20"

df_model <- cbind(data.frame(p_val=summary(model_20)$coefficients[,6]), data.frame(summary(model_20)$conf.int[,c(1,3,4)]))
df_model <- df_model[substr(rownames(df_model), start=1, stop=1)=="m",]

addWorksheet(wb, name_sheet)
writeData(wb, name_sheet, df_model, colNames=TRUE, rowNames=TRUE)
saveWorkbook(wb, path_file, overwrite=TRUE)
```

§§§§§§§§§§§§§§ MODEL 21

Correct the potential issue with too few in the above
```{r}
# original: as_model_21 <- as.formula(paste('Surv(time_fu, ASD)~', sig_0012, "+", sig_0048, "+ strata(DOBY) + SEX + rcs(MOM_AGEY, quantile(MOM_AGEY, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + p2_all_c + EDU_B + rcs(INCOME_B1, quantile(INCOME_B1, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + parity + strata(MID)"))

as_model_21 <- as.formula(paste('Surv(time_fu, ASD)~', paste(model_20_dx, collapse = '+'), "+ strata(DOBY) + SEX + rcs(MOM_AGEY, quantile(MOM_AGEY, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + p2_all_c + EDU_B + rcs(INCOME_B1, quantile(INCOME_B1, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + parity + strata(MID)"))

model_21 <- coxph(as_model_21, data=b_dxall_w_full_sib)

```

Save the "approved" model
```{r}
name_sheet <- "Model 21"

df_model <- cbind(data.frame(p_val=summary(model_21)$coefficients[,5]), data.frame(summary(model_21)$conf.int[,c(1,3,4)]))
df_model <- df_model[substr(rownames(df_model), start=1, stop=1)=="m",]

addWorksheet(wb, name_sheet)
writeData(wb, name_sheet, df_model, colNames=TRUE, rowNames=TRUE)
saveWorkbook(wb, path_file, overwrite=TRUE)
```

§§§§§§§§§§§§§§ MODEL 21b

Correct the potential issue with too few in the above
```{r}

# original: as_model_21b <- as.formula(paste('Surv(time_fu, ASD)~', sig_0012, "+", sig_0048, "+ DOBY + SEX + rcs(MOM_AGEY, quantile(MOM_AGEY, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + p2_all_c + EDU_B + rcs(INCOME_B1, quantile(INCOME_B1, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + parity + strata(MID)"))

as_model_21b <- as.formula(paste('Surv(time_fu, ASD)~', paste(model_20_dx, collapse = '+'), "+ DOBY + SEX + rcs(MOM_AGEY, quantile(MOM_AGEY, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + p2_all_c + EDU_B + rcs(INCOME_B1, quantile(INCOME_B1, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + parity + strata(MID)"))

model_21b <- coxph(as_model_21b, data=b_dxall_w_full_sib) # Output needed (model_21b)

```

Save the "approved" model
```{r}
name_sheet <- "Model 21b"

df_model <- cbind(data.frame(p_val=summary(model_21b)$coefficients[,5]), data.frame(summary(model_21b)$conf.int[,c(1,3,4)]))
df_model <- df_model[substr(rownames(df_model), start=1, stop=1)=="m",]

addWorksheet(wb, name_sheet)
writeData(wb, name_sheet, df_model, colNames=TRUE, rowNames=TRUE)
saveWorkbook(wb, path_file, overwrite=TRUE)
```

## Model 20 s6 and 21 s6

Check if everything is okay
```{r}
temp <- b_dxall_w_full_sib_s6_db %>% filter(!is.na(EDU_B) & !is.na(INCOME_B1) & !is.na(parity))
temp_dx <- c(sig_dx)
model_20_s6 <- rep(NA, length(temp_dx))
for(i in 1:length(temp_dx)){
  model_20_s6[i] <- min(table(temp[,c(temp_dx[i], "ASD")]))
}
min(model_20_s6)

model_20_s6_dx <- temp_dx[model_20_s6>=5]
```

§§§§§§§§§§§§§§§§§ Model 20 s6

Correct the potential issue with too few in the above
```{r}
# original: as_model_11_s2_sib_nsex <- as.formula(paste('Surv(time_fu, ASD)~', sig_0012, "+", sig_0048, "+ DOBY+ rcs(MOM_AGEY, quantile(MOM_AGEY, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + p2_all_c + EDU_B + rcs(INCOME_B1, quantile(INCOME_B1, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + parity + cluster(MID)"))

as_model_11_s2_sib_nsex <- as.formula(paste('Surv(time_fu, ASD)~', paste(model_20_s6_dx, collapse = '+'), "+ DOBY+ rcs(MOM_AGEY, quantile(MOM_AGEY, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + p2_all_c + EDU_B + rcs(INCOME_B1, quantile(INCOME_B1, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + parity + cluster(MID)"))

model_20_s6 <- coxph(as_model_11_s2_sib_nsex, data=b_dxall_w_full_sib_s6_db)
```

Save the "approved" model
```{r}
name_sheet <- "Model 20 s6"

df_model <- cbind(data.frame(p_val=summary(model_20_s6)$coefficients[,6]), data.frame(summary(model_20_s6)$conf.int[,c(1,3,4)]))
df_model <- df_model[substr(rownames(df_model), start=1, stop=1)=="m",]

addWorksheet(wb, name_sheet)
writeData(wb, name_sheet, df_model, colNames=TRUE, rowNames=TRUE)
saveWorkbook(wb, path_file, overwrite=TRUE)
```

§§§§§§§§§§§§§§§§§ Model 21 s6

Correct the potential issue with too few in the above
```{r}
# original: as_model_21b_nsex <- as.formula(paste('Surv(time_fu, ASD)~', sig_0012, "+", sig_0048, "+ DOBY + rcs(MOM_AGEY, quantile(MOM_AGEY, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + p2_all_c + EDU_B + rcs(INCOME_B1, quantile(INCOME_B1, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + parity + strata(MID)"))

as_model_21b_nsex <- as.formula(paste('Surv(time_fu, ASD)~', paste(model_20_s6_dx, collapse = '+'), "+ DOBY + rcs(MOM_AGEY, quantile(MOM_AGEY, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + p2_all_c + EDU_B + rcs(INCOME_B1, quantile(INCOME_B1, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + parity + strata(MID)"))

model_21_s6 <- coxph(as_model_21b_nsex, data=b_dxall_w_full_sib_s6_db)
```

Save the "approved" model
```{r}
name_sheet <- "Model 21 s6"

df_model <- cbind(data.frame(p_val=summary(model_21_s6)$coefficients[,5]), data.frame(summary(model_20_s6)$conf.int[,c(1,3,4)]))
df_model <- df_model[substr(rownames(df_model), start=1, stop=1)=="m",]

addWorksheet(wb, name_sheet)
writeData(wb, name_sheet, df_model, colNames=TRUE, rowNames=TRUE)
saveWorkbook(wb, path_file, overwrite=TRUE)
```

## Model 11 s7m/f

Check if everything is okay for the males
```{r}
model_11_s7m_db <- b_dxall_w_full %>% filter(SEX == "M")
temp <- model_11_s7m_db %>% filter(!is.na(EDU_B) & !is.na(INCOME_B1))
temp_dx <- c(sig_dx)
model_11_s7m <- rep(NA, length(temp_dx))
for(i in 1:length(temp_dx)){
  model_11_s7m[i] <- min(table(temp[,c(temp_dx[i], "ASD")]))
}
min(model_11_s7m)

model_11_s7m_dx <- temp_dx[model_11_s7m>=5]
```
Check if everything is okay for the females
```{r}
model_11_s7f_db <- b_dxall_w_full %>% filter(SEX == "F")
temp <- model_11_s7f_db %>% filter(!is.na(EDU_B) & !is.na(INCOME_B1))
temp_dx <- c(sig_dx)
model_11_s7f <- rep(NA, length(temp_dx))
for(i in 1:length(temp_dx)){
  model_11_s7f[i] <- min(table(temp[,c(temp_dx[i], "ASD")]))
}
min(model_11_s7f)

model_11_s7f_dx <- temp_dx[model_11_s7f>=5]
```

The diagnoses that are okay for both males and females (we use these for the stratified analysis)

```{r}
model_11_s7_dx <- sig_dx[(sig_dx %in% model_11_s7f_dx) & (sig_dx %in% model_11_s7m_dx)]
```


§§§§§§§§§§§§§§§§§§§ Model 11 s7m

Correct the potential issue with too few in the above
```{r}
# original: as_model_11_nsex <- as.formula(paste('Surv(time_fu, ASD)~', sig_0012, "+", sig_0048, "+ strata(DOBY) + rcs(MOM_AGEY, quantile(MOM_AGEY, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + p2_all_c + EDU_B + rcs(INCOME_B1, quantile(INCOME_B1, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + cluster(MID)"))
as_model_11_nsex <- as.formula(paste('Surv(time_fu, ASD)~', paste(model_11_s7_dx, collapse = '+'), "+ strata(DOBY) + rcs(MOM_AGEY, quantile(MOM_AGEY, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + p2_all_c + EDU_B + rcs(INCOME_B1, quantile(INCOME_B1, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + cluster(MID)"))

model_11_s7m <- coxph(as_model_11_nsex, data=model_11_s7m_db) # Output needed (model_11_s7m)
```

Save the "approved" model
```{r}
name_sheet <- "Model 11 s7m"

df_model <- cbind(data.frame(p_val=summary(model_11_s7m)$coefficients[,6]), data.frame(summary(model_11_s7m)$conf.int[,c(1,3,4)]))
df_model <- df_model[substr(rownames(df_model), start=1, stop=1)=="m",]

addWorksheet(wb, name_sheet)
writeData(wb, name_sheet, df_model, colNames=TRUE, rowNames=TRUE)
saveWorkbook(wb, path_file, overwrite=TRUE)
```


§§§§§§§§§§§§§§§§§§§ Model 11 s7f

Correct the potential issue with too few in the above
```{r}
# original: as_model_11_nsex <- as.formula(paste('Surv(time_fu, ASD)~', sig_0012, "+", sig_0048, "+ strata(DOBY) + rcs(MOM_AGEY, quantile(MOM_AGEY, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + p2_all_c + EDU_B + rcs(INCOME_B1, quantile(INCOME_B1, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + cluster(MID)"))
as_model_11_nsex <- as.formula(paste('Surv(time_fu, ASD)~', paste(model_11_s7_dx, collapse = '+'), "+ strata(DOBY) + rcs(MOM_AGEY, quantile(MOM_AGEY, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + p2_all_c + EDU_B + rcs(INCOME_B1, quantile(INCOME_B1, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + cluster(MID)"))

model_11_s7f <- coxph(as_model_11_nsex, data=model_11_s7f_db) # Output needed (model_11_s7m)
```

Save the "approved" model
```{r}
name_sheet <- "Model 11 s7f"

df_model <- cbind(data.frame(p_val=summary(model_11_s7f)$coefficients[,6]), data.frame(summary(model_11_s7f)$conf.int[,c(1,3,4)]))
df_model <- df_model[substr(rownames(df_model), start=1, stop=1)=="m",]

addWorksheet(wb, name_sheet)
writeData(wb, name_sheet, df_model, colNames=TRUE, rowNames=TRUE)
saveWorkbook(wb, path_file, overwrite=TRUE)
```

## Model 11 s8id/nid

Check if everything is okay for those with ID
```{r}
model_11_s8id_db <- b_dxall_w_full %>% filter(COMORB_ID == 1)
temp <- model_11_s8id_db %>% filter(!is.na(EDU_B) & !is.na(INCOME_B1))
temp_dx <- c(sig_dx)
model_11_s8id <- rep(NA, length(temp_dx))
for(i in 1:length(temp_dx)){
  model_11_s8id[i] <- min(table(temp[,c(temp_dx[i], "ASD")]))
}
min(model_11_s8id)

model_11_s8id_dx <- temp_dx[model_11_s8id>=5]
```

Check if everything is okay for those without ID
```{r}
model_11_s8nid_db <- b_dxall_w_full %>% filter(COMORB_ID == 0)
temp <- model_11_s8nid_db %>% filter(!is.na(EDU_B) & !is.na(INCOME_B1))
temp_dx <- c(sig_dx)
model_11_s8nid <- rep(NA, length(temp_dx))
for(i in 1:length(temp_dx)){
  model_11_s8nid[i] <- min(table(temp[,c(temp_dx[i], "ASD")]))
}
min(model_11_s8nid)

model_11_s8nid_dx <- temp_dx[model_11_s8nid>=5]
```

The diagnoses that are okay for both with and without ID (we use these for the stratified analysis)

```{r}
model_11_s8_dx <- sig_dx[(sig_dx %in% model_11_s8id_dx) & (sig_dx %in% model_11_s8nid_dx)]
```

§§§§§§§§§§§§§§§§§§§§§ Model 11 s8 id

Correct the potential issue with too few in the above
```{r}
# original: as_model_11 <- as.formula(paste('Surv(time_fu, ASD)~', sig_0012, "+", sig_0048, "+ strata(DOBY) + SEX + rcs(MOM_AGEY, quantile(MOM_AGEY, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + p2_all_c + EDU_B + rcs(INCOME_B1, quantile(INCOME_B1, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + cluster(MID)"))
as_model_11 <- as.formula(paste('Surv(time_fu, ASD)~', paste(model_11_s8_dx, collapse = '+'), "+ strata(DOBY) + SEX + rcs(MOM_AGEY, quantile(MOM_AGEY, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + p2_all_c + EDU_B + rcs(INCOME_B1, quantile(INCOME_B1, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + cluster(MID)"))
model_11_s8id <- coxph(as_model_11, data=model_11_s8id_db)
```

Save the "approved" model
```{r}
name_sheet <- "Model 11 s8id"

df_model <- cbind(data.frame(p_val=summary(model_11_s8id)$coefficients[,6]), data.frame(summary(model_11_s8id)$conf.int[,c(1,3,4)]))
df_model <- df_model[substr(rownames(df_model), start=1, stop=1)=="m",]

addWorksheet(wb, name_sheet)
writeData(wb, name_sheet, df_model, colNames=TRUE, rowNames=TRUE)
saveWorkbook(wb, path_file, overwrite=TRUE)
```


§§§§§§§§§§§§§§§§§§§§§ Model 11 s8 nid

Correct the potential issue with too few in the above
```{r}
# original: as_model_11 <- as.formula(paste('Surv(time_fu, ASD)~', sig_0012, "+", sig_0048, "+ strata(DOBY) + SEX + rcs(MOM_AGEY, quantile(MOM_AGEY, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + p2_all_c + EDU_B + rcs(INCOME_B1, quantile(INCOME_B1, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + cluster(MID)"))
as_model_11 <- as.formula(paste('Surv(time_fu, ASD)~', paste(model_11_s8_dx, collapse = '+'), "+ strata(DOBY) + SEX + rcs(MOM_AGEY, quantile(MOM_AGEY, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + p2_all_c + EDU_B + rcs(INCOME_B1, quantile(INCOME_B1, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + cluster(MID)"))
model_11_s8nid <- coxph(as_model_11, data=model_11_s8nid_db)
```

Save the "approved" model
```{r}
name_sheet <- "Model 11 s8nid"

df_model <- cbind(data.frame(p_val=summary(model_11_s8nid)$coefficients[,6]), data.frame(summary(model_11_s8nid)$conf.int[,c(1,3,4)]))
df_model <- df_model[substr(rownames(df_model), start=1, stop=1)=="m",]

addWorksheet(wb, name_sheet)
writeData(wb, name_sheet, df_model, colNames=TRUE, rowNames=TRUE)
saveWorkbook(wb, path_file, overwrite=TRUE)
```


## Model 21c s6

Check if everything is okay
```{r}
temp <- b_dxall_w_full_sib_s6_db %>% filter(!is.na(EDU_B) & !is.na(INCOME_B1) & !is.na(parity))
temp_dx <- c(sig_dx)
model_21c_s6 <- rep(NA, length(temp_dx))
for(i in 1:length(temp_dx)){
  model_21c_s6[i] <- min(table(temp[,c(temp_dx[i], "ASD")]))
}
min(model_21c_s6)

model_21c_s6_dx <- temp_dx[model_21c_s6>=5]
```

Correct the potential issue with too few in the above
```{r}
as_model_21c_nsex <- as.formula(paste('Surv(time_fu, ASD)~', paste(model_21c_s6_dx, collapse = '+'), "+ DOBY + rcs(MOM_AGEY, quantile(MOM_AGEY, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + p2_all_c + EDU_B + rcs(INCOME_B1, quantile(INCOME_B1, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + parity"))
#sibling analysis acute + chronic
model_21c_s6 <- coxph(as_model_21c_nsex, data=b_dxall_w_full_sib_s6_db)
```

Save the "approved" model
```{r}
name_sheet <- "Model 21c s6"

df_model <- cbind(data.frame(p_val=summary(model_21c_s6)$coefficients[,5]), data.frame(summary(model_21c_s6)$conf.int[,c(1,3,4)]))
df_model <- df_model[substr(rownames(df_model), start=1, stop=1)=="m",]

addWorksheet(wb, name_sheet)
writeData(wb, name_sheet, df_model, colNames=TRUE, rowNames=TRUE)
saveWorkbook(wb, path_file, overwrite=TRUE)
```

## Model 21c

Check if everything is okay
```{r}
temp <- b_dxall_w_full_sib %>% filter(!is.na(EDU_B) & !is.na(INCOME_B1) & !is.na(parity))
temp_dx <- c(sig_dx)
model_21c <- rep(NA, length(temp_dx))
for(i in 1:length(temp_dx)){
  model_21c[i] <- min(table(temp[,c(temp_dx[i], "ASD")]))
}
min(model_21c)

model_21c_dx <- temp_dx[model_21c>=5]
```

Correct the potential issue with too few in the above
```{r}
as_model_21c <- as.formula(paste('Surv(time_fu, ASD)~', paste(model_21c_dx, collapse = '+'), "+ DOBY + SEX + rcs(MOM_AGEY, quantile(MOM_AGEY, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + p2_all_c + EDU_B + rcs(INCOME_B1, quantile(INCOME_B1, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + parity"))
model_21c <- coxph(as_model_21c, data=b_dxall_w_full_sib) # Output needed (model_21b)
```

Save the "approved" model
```{r}
name_sheet <- "Model 21c"

df_model <- cbind(data.frame(p_val=summary(model_21c)$coefficients[,5]), data.frame(summary(model_21c)$conf.int[,c(1,3,4)]))
df_model <- df_model[substr(rownames(df_model), start=1, stop=1)=="m",]

addWorksheet(wb, name_sheet)
writeData(wb, name_sheet, df_model, colNames=TRUE, rowNames=TRUE)
saveWorkbook(wb, path_file, overwrite=TRUE)
```




















#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*

DK_R01_2023_11_27_EA.Rmd

---
  title: "DK R01"
author: "Vahe/Elias"
date: '2023-12-12'
output: html_document
---
  
  # NOTE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  First, run DK_R01_2023_03_22_EA down to (but not including) the model 1 analyses.

## Model 1b Model 1b Model 1b Model 1b Model 1b Model 1b Model 1b Model 1b Model 1b Model 1b Model 1b Model 1b Model 1b Model 1b Model 1b
## Running Cox proportional hazard model to examine the association between diagnoses and ASD -- Model 1 + sex + mom's age (Model 1b)
```{r}
n <- which(colnames(b_dx0012_0048_w_full)=="PREG_ID") -1
model_1b_00_12_48 <- data.frame()
for(i in 2:n){  ## for each dx, calculate HRs + CIs
  a <- coxph(Surv(time_fu, ASD)~b_dx0012_0048_w_full[,i] + strata(DOBY) + SEX + rcs(MOM_AGEY, quantile(MOM_AGEY, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + cluster(MID), data=b_dx0012_0048_w_full) # CHANGED BY ELIAS
  model_1b_00_12_48[i,1] <- summary(a)$coefficients[1,6]  # p-val
  model_1b_00_12_48[i,2] <- summary(a)$coefficients[1,2]  # HR
  model_1b_00_12_48[i,3] <- exp(confint(a))[1,1]          # 2.5%
  model_1b_00_12_48[i,4] <- exp(confint(a))[1,2]          # 97.5%
  model_1b_00_12_48[i,5] <- colnames(b_dx0012_0048_w_full[i])    # icd code
}

model_1b_00_12_48[,6] <- p.adjust(model_1b_00_12_48[,1], method = "BH") # CHANGED BY ELIAS
model_1b_00_12_48[,7] <- p.adjust(model_1b_00_12_48[,1], method = "bonferroni") # CHANGED BY ELIAS
colnames(model_1b_00_12_48) <- c("p-val", "HR", "2.5%", "97.5%", "ICD10", "q-val", "Bonferroni")
model_1b_00_12_48 <- model_1b_00_12_48[!is.na(model_1b_00_12_48[,1]),] #Output of interest
```

Save the results
```{r}
path_file <- "D:/Data/Workdata/703934/EliasA/Run for Vahe/Results/DK_R01_2023_11_27_EA.xlsx"
name_sheet <- "model_1b_00_12_48"
wb <- createWorkbook()
addWorksheet(wb, name_sheet)
writeData(wb, name_sheet, model_1b_00_12_48, colNames=TRUE, rowNames=FALSE)
saveWorkbook(wb, path_file, overwrite=TRUE)
```

## Model 4 sibling sample

Add sig_dx
```{r}
path_file_old <- "D:/Data/Workdata/703934/EliasA/Run for Vahe/Results/DK_R01_2023_03_22_EA.xlsx"

model_4_00_12_48 <- readWorkbook(path_file_old, sheet="model_4_00_12_48")
sig<- subset(model_4_00_12_48, model_4_00_12_48$`q-val` <0.05)
sig_dx <- sig$ICD10
sig_dx_0012 <- subset(sig_dx, sig_dx %in% list_acute)
sig_dx_0048 <- subset(sig_dx, sig_dx %in% list_chronic)
```


Create b_dxall_w_full_sib, the data set that is used for sibling analysis
```{r}
count_MID <- as.data.frame(b_dxall_w_full %>% group_by(MID) %>% tally())         # MID Sibs
count_MID$MID_freq <- count_MID$n
count_MID$n <- NULL

model_11_s2_db <- b_dxall_w_full
model_11_s2_db$parity <- 1
model_11_s2_db$parity[model_11_s2_db$PARITY1 > 1] <- 2
model_11_s2_db$parity[model_11_s2_db$PARITY1 > 3] <- 3
model_11_s2_db$parity[model_11_s2_db$PARITY1 > 5] <- 4
model_11_s2_db$parity <- as.factor(model_11_s2_db$parity)

b_dxall_w_full_sib <- (model_11_s2_db %>% left_join(count_MID, by = "MID") %>%
                         filter(MID_freq > 1))
```


```{r}
## Model 4 among sibling sample
model_4_sib_sample <- data.frame()
for(i in 1:length(sig_dx)){  ## for each dx, calculate HRs + CIs
  as_model_4_sib_sample <- as.formula(paste('Surv(time_fu, ASD)~', sig_dx[i], '+ DOBY + SEX + rcs(MOM_AGEY, quantile(MOM_AGEY, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + p2_all_c + EDU_B + rcs(INCOME_B1, quantile(INCOME_B1, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + parity + cluster(MID)'))
  a <- coxph(as_model_4_sib_sample, data=b_dxall_w_full_sib) 
  model_4_sib_sample[i,1] <- summary(a)$coefficients[1,6]  # p-val
  model_4_sib_sample[i,2] <- summary(a)$coefficients[1,2]  # HR
  model_4_sib_sample[i,3] <- exp(confint(a))[1,1]          # 2.5%
  model_4_sib_sample[i,4] <- exp(confint(a))[1,2]          # 97.5%
  model_4_sib_sample[i,5] <- sig_dx[i]    # icd code
}

colnames(model_4_sib_sample) <- c("p-val", "HR", "2.5%", "97.5%", "ICD10")
model_4_sib_sample <- model_4_sib_sample[!is.na(model_4_sib_sample[,1]),] #Output of interest

# Save results
name_sheet <- "model_4_sib_sample"
wb <- loadWorkbook(path_file)
addWorksheet(wb, name_sheet)
writeData(wb, name_sheet, model_4_sib_sample, colNames=TRUE)
saveWorkbook(wb, path_file, overwrite=TRUE)

## Sibling analysis of model 4
model_4_sib_analysis <- data.frame()
for(i in 1:length(sig_dx)){  ## for each dx, calculate HRs + CIs
  as_model_4_sib_analysis <- as.formula(paste('Surv(time_fu, ASD)~', sig_dx[i], '+ DOBY + SEX + rcs(MOM_AGEY, quantile(MOM_AGEY, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + p2_all_c + EDU_B + rcs(INCOME_B1, quantile(INCOME_B1, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + parity + strata(MID)'))
  a <- coxph(as_model_4_sib_analysis, data=b_dxall_w_full_sib) 
  model_4_sib_analysis[i,1] <- summary(a)$coefficients[1,5]  # p-val
  model_4_sib_analysis[i,2] <- summary(a)$coefficients[1,2]  # HR
  model_4_sib_analysis[i,3] <- exp(confint(a))[1,1]          # 2.5%
  model_4_sib_analysis[i,4] <- exp(confint(a))[1,2]          # 97.5%
  model_4_sib_analysis[i,5] <- sig_dx[i]    # icd code
}

colnames(model_4_sib_analysis) <- c("p-val", "HR", "2.5%", "97.5%", "ICD10")
model_4_sib_analysis <- model_4_sib_analysis[!is.na(model_4_sib_analysis[,1]),] #Output of interest

# Save results
name_sheet <- "model_4_sib_analysis"
wb <- loadWorkbook(path_file)
addWorksheet(wb, name_sheet)
writeData(wb, name_sheet, model_4_sib_analysis, colNames=TRUE)
saveWorkbook(wb, path_file, overwrite=TRUE)


# Model 4 sib analysis in males only
b_dxall_w_full_sib_s6_db_2 <- b_dxall_w_full_sib %>% filter(SEX == "M") %>% select(-MID_freq)
count_MID_sib_M <- as.data.frame(b_dxall_w_full_sib_s6_db_2 %>% group_by(MID) %>% tally()) #MID sibs
count_MID_sib_M$MID_freq <- count_MID_sib_M$n
count_MID_sib_M$n <- NULL 

b_dxall_w_full_sib_s6_db_2 <- (b_dxall_w_full_sib_s6_db_2 %>% left_join(count_MID_sib_M, by="MID") %>%
                                 filter(MID_freq>1))

n_distinct(b_dxall_w_full_sib_s6_db_2$CID)
n_distinct(b_dxall_w_full_sib_s6_db_2$MID)

model_4_sib_analysis_M <- data.frame()
for(i in 1:length(sig_dx)){  ## for each dx, calculate HRs + CIs
  as_model_4_sib_analysis_M <- as.formula(paste('Surv(time_fu, ASD)~', sig_dx[i], '+ DOBY + rcs(MOM_AGEY, quantile(MOM_AGEY, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + p2_all_c + EDU_B + rcs(INCOME_B1, quantile(INCOME_B1, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + parity + strata(MID)'))
  a <- coxph(as_model_4_sib_analysis_M, data=b_dxall_w_full_sib_s6_db_2) 
  model_4_sib_analysis_M[i,1] <- summary(a)$coefficients[1,5]  # p-val
  model_4_sib_analysis_M[i,2] <- summary(a)$coefficients[1,2]  # HR
  model_4_sib_analysis_M[i,3] <- exp(confint(a))[1,1]          # 2.5%
  model_4_sib_analysis_M[i,4] <- exp(confint(a))[1,2]          # 97.5%
  model_4_sib_analysis_M[i,5] <- sig_dx[i]    # icd code
}

colnames(model_4_sib_analysis_M) <- c("p-val", "HR", "2.5%", "97.5%", "ICD10")
model_4_sib_analysis_M <- model_4_sib_analysis_M[!is.na(model_4_sib_analysis_M[,1]),] #Output of interest

# Save results
name_sheet <- "model_4_sib_analysis_M"
wb <- loadWorkbook(path_file)
addWorksheet(wb, name_sheet)
writeData(wb, name_sheet, model_4_sib_analysis_M, colNames=TRUE)
saveWorkbook(wb, path_file, overwrite=TRUE)
```

## Check all diagnoses okay to export in the different analyses, and remove/correct diagnoses not allowed to be exported


Get all the models run
```{r}
ls_bef_check <- rio::import_list(path_file)
```


Model 1b

```{r}
check_b <- b_dx0012_0048_w_full %>% filter_at(vars(c(DOBY, SEX, MOM_AGEY)), all_vars(!is.na(.)))
b <- c()

dx_1b <- ls_bef_check[["model_1b_00_12_48"]]$ICD10
for(i in 1:length(dx_1b)){
  tb <- table(check_b$ASD, check_b[[dx_1b[i]]])
  b[i] <- min(tb)
}

names(b) <- dx_1b
min(b)
```


Check that all the diagnoses from the sibling sample are okay to run

```{r}
check <- b_dxall_w_full_sib %>% filter_at(vars(c(DOBY, SEX, MOM_AGEY, p2_all_c, EDU_B, INCOME_B1, parity)), all_vars(!is.na(.)))
s <- c()
for(i in 1:length(sig_dx)){
  tb <- table(check$ASD, check[[sig_dx[i]]])
  s[i] <- min(tb)
}

names(s) <- sig_dx
s
```

Check that all the diagnoses from the male sibling sample are okay to run

```{r}
check_M <- b_dxall_w_full_sib_s6_db_2 %>% filter_at(vars(c(DOBY, MOM_AGEY, p2_all_c, EDU_B, INCOME_B1, parity)), all_vars(!is.na(.)))

m <- c()
for(i in 1:length(sig_dx)){
  tb <- table(check_M$ASD, check_M[[sig_dx[i]]])
  m[i] <- min(tb)
}

names(m) <- sig_dx
m 
```

Correct "mistakes" - remove too rare diagnoses

```{r}
ls_after_check <- list()
ls_after_check[["model_1b_00_12_48"]] <- ls_bef_check[["model_1b_00_12_48"]][b>=5,]

ls_after_check[["model_4_sib_sample"]] <- ls_bef_check[["model_4_sib_sample"]][s>=5,]

ls_after_check[["model_4_sib_analysis"]] <- ls_bef_check[["model_4_sib_analysis"]][s>=5,]

ls_after_check[["model_4_sib_analysis_M"]] <- ls_bef_check[["model_4_sib_analysis_M"]][m>=5,]

rio::export(ls_after_check, "D:/Data/Workdata/703934/EliasA/Run for Vahe/Results/DK_R01_2023_11_27_EA_export.xlsx")
```




















#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*

miscellaneous_items_export_2024_01_08.R


# Data preparation ------------------------------------

# First run DK_R01_2023_02_22_EA.Rmd until "## Model 1 Model 1 ..."

res_ls <- list()

# quartiles, means etc. ----------------------------------


# Function to calculate pseudo quartiles
pseudo_quartiles <- function(x){
  # Remove missing observations
  x <- x[!is.na(x)]
  # Sort x from lowest to highest
  x <- sort(x)
  n <- length(x)
  # Median §§§§§§§§§§
  if(n/2==round(n/2)){ # If n/2 integer
    med <- mean(x[(n/2-2):(n/2+3)])
  } else { # If n/2 not integer
    med <- mean(x[(ceiling(n/2)-2):(ceiling(n/2)+2)])
  }
  
  # Quartile 1 §§§§§§§§§
  if(n/4==round(n/4)){ # If n/4 integer
    q1 <- mean(x[(n/4-2):(n/4+3)])
  } else{ # If n/4 not integer
    q1 <- mean(x[(ceiling(n/4)-2):(ceiling(n/4)+2)])
  }
  
  # Quartile 3 §§§§§§§§§
  if(n*3/4==round(n*3/4)){ # If n*3/4 integer
    q3 <- mean(x[(n*3/4-2):(n*3/4+3)])
  } else{ # If n*3/4 not integer
    q3 <- mean(x[(ceiling(n*3/4)-2):(ceiling(n*3/4)+2)])
  }
  return(c(med, q1, q3))
}

# Create function that calculates all the summary statistics we are intereted in
sum_stat <- function(x){
  res <- c(pseudo_quartiles(x), mean(x), sd(x))
  names(res) <- c("Pseudo median", "Pseudo Q1", "pseudo Q2", "Mean", "SD")
  return(res)
}

# Prepare data for the ASD age summary statistic
# NOTE!!!!!!!!!!!!!! THE WAY ASD AGE WAS ORIGINALLY DONE IS WRONG!!! SEE AT THE END OF THE DOCUMENT, WHERE THE MISTAKE IS CORRECTED. IF REDOING THIS IN A DIFFERENT DATA SET, REPLACE WITH THE "CORRECT" METHOD.


res_tb <- sum_stat(b_dx0012_0048_w_full$time_fu) # Follow-up time for everyone

rownames(res_tb) <- c("Follow-up time in days", "Age at ASD diagnosis in days")

res_ls[["Summary statistics"]] <- data.frame(res_tb)


# n's ---------------------------------------------------

res_n <- c()

# Mothers in the analytical sample §§§§§§§§§§§§§§§§§§§§§§§§

res_n <- n_distinct(b_dx0012_0048_w_full$MID)
names(res_n) <- "Mothers"


# Mothers in sibling analytical sample §§§§§§§§§§§§§§§§§§§§

# Create data set used for sibling analysis
model_11_s2_db <- b_dxall_w_full
model_11_s2_db$parity <- 1
model_11_s2_db$parity[model_11_s2_db$PARITY1 > 1] <- 2
model_11_s2_db$parity[model_11_s2_db$PARITY1 > 3] <- 3
model_11_s2_db$parity[model_11_s2_db$PARITY1 > 5] <- 4
model_11_s2_db$parity <- as.factor(model_11_s2_db$parity)

count_MID <- as.data.frame(b_dxall_w_full %>% group_by(MID) %>% tally())         # MID Sibs
count_MID$MID_freq <- count_MID$n
count_MID$n <- NULL
b_dxall_w_full_sib <- (model_11_s2_db %>% left_join(count_MID, by = "MID") %>%
                         filter(MID_freq > 1))

# Find number of mothers in the sibling sample
res_n <- c(res_n, n_distinct(b_dxall_w_full_sib$MID))
names(res_n)[length(res_n)] <- "Mothers sibling sample"

# Number of unique ICD-10 level 3 dx observed in the sample §§§§§§§§§§§§§§§§§§

# Unique codes in the 4 years prior to the birth
dx_0048_all <- demographics %>% filter(IN_COHORT==1) %>% # Find cohort children
  select(CID=PID, DOB) %>% left_join(select(fam_anc, CID, MID)) %>% # Add the mothers
  left_join(select(health_cond, MID=PID, COND_CODE, COND_DXSYS, DATE_DX)) %>% # Add maternal diagnoses
  filter(COND_DXSYS==3) %>% # Only ICD-10 codes
  mutate(date_dif=as.numeric(difftime(DATE_DX, DOB , units = c("days")))) %>% # Time between birth and maternal diagnosis
  mutate(COND_CODE=str_sub(COND_CODE, 2, 4)) %>% # Use level-3 ICD-10 diagnosis
  filter(date_dif < 0 & date_dif >= -1460) # Limit it to 4 years prior to birth of child

res_n <- c(res_n, n_distinct(dx_0048_all$COND_CODE)) # Find number of distinct diagnoses
names(res_n)[length(res_n)] <- "Total n.o. diagnoses, 4 years"

# Unique codes in the year prior to the birth
dx_0012_all <- dx_0048_all %>% 
  filter(date_dif < 0 & date_dif >= -365) # Limit it to diagnosis in the year prior to the birth

res_n <- c(res_n, n_distinct(dx_0012_all$COND_CODE)) # Find number of distinct diagnoses
names(res_n)[length(res_n)] <- "Total n.o. diagnoses, 1 year"


res_ls[["n's"]] <- data.frame("n"=res_n)

# temp <- dx_0012_all %>% select(CID, COND_CODE) %>% distinct() %>% 
#   left_join(select(b_dx0012_0048_w_full, CID, ASD))
# 
# temp2 <- table(temp$COND_CODE, temp$ASD)
# dim(temp2[temp2[,2]>10,])

# Sensitivity 5, with correctly removed maternal ASD cases ----------------------------

# Find significant diagnoses

path_file_old <- "D:/Data/Workdata/703934/EliasA/Run for Vahe/Results/DK_R01_2023_03_22_EA.xlsx"

model_4_00_12_48 <- readWorkbook(path_file_old, sheet="model_4_00_12_48")
sig<- subset(model_4_00_12_48, model_4_00_12_48$`q-val` <0.05)
sig_dx <- sig$ICD10
sig_dx_0012 <- subset(sig_dx, sig_dx %in% list_acute) # CHANGED BY ELIAS
sig_dx_0048 <- subset(sig_dx, sig_dx %in% list_chronic) # CHANGED BY ELIAS

sig_0012 <- paste(sig_dx_0012, collapse = '+')
sig_0048 <- paste(sig_dx_0048, collapse = '+')

# Create dataset
model_11_s5_db <- b_dxall_w_full %>% filter(MAT_ASD==0)

# See if all diagnoses common enough
temp <- model_11_s5_db %>% filter(!is.na(DOBY) & !is.na(SEX) & !is.na(MOM_AGEY) & !is.na(p2_all_c) & !is.na(EDU_B) & !is.na(INCOME_B1))

tb <- rep(NA, length(sig_dx))
for(i in 1:length(sig_dx)){
  tb[i] <- min(table(temp[,c(sig_dx[i], "ASD")]))
}

min(tb)

# The model

as_model_11 <- as.formula(paste('Surv(time_fu, ASD)~', sig_0012, "+", sig_0048, "+ strata(DOBY) + SEX + rcs(MOM_AGEY, quantile(MOM_AGEY, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + p2_all_c + EDU_B + rcs(INCOME_B1, quantile(INCOME_B1, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + cluster(MID)")) # CHANGED BY ELIAS

# The analysis sensitivity 5
model_11_s5 <- coxph(as_model_11, data=model_11_s5_db) # Output needed (model_11_s5)

model_11_s5_df <- cbind(data.frame(p_val=summary(model_11_s5)$coefficients[,6]), data.frame(summary(model_11_s5)$conf.int[,c(1,3,4)]))

model_11_s5_df <- model_11_s5_df[1:length(sig_dx),]


res_ls[["Sensitivity 5"]] <- model_11_s5_df

# res_ls_old <- res_ls
# for(i in 1:length(res_ls_old)){
#   res_ls[[names(res_ls_old)[i]]] <- data.frame(res_ls_old[[i]])
# }
# colnames(res_ls$`n's`) <- NULL
#rio::export(res_ls, file="D:/Data/Workdata/703934/EliasA/Run for Vahe/Results/miscellaneous_items.xlsx")

write.xlsx(res_ls, file="D:/Data/Workdata/703934/EliasA/Run for Vahe/Results/miscellaneous_items.xlsx", rowNames=TRUE)


###########################################################################

# Added later: correct ASD age ---------------------------------

temp <- b_dx0012_0048_w_full %>% filter(ASD==1)

summary(temp$time_fu)

# Find the age at ASD diagnosis
age_asd <- sum_stat(temp$time_fu/365.25)

names(age_asd)[3] <- "Pseudo Q3"

age_asd <- data.frame(age_asd)

write.xlsx(age_asd, file="D:/Data/Workdata/703934/EliasA/Run for Vahe/Results/age_asd.xlsx", rowNames=TRUE, sheetName="Age at ASD diagnosis, years")



















#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*

paternal_2024_05_06.Rmd

---
  title: "Paternal analysis"
author: "Elias Arildskov"
date: '2023-10-04'
output: html_document
---
  
  ### Loading the required librabies
  ```{r include=FALSE}
library(dplyr)
library(stringr)
library(tidyr)
library(survival)
library(rms) # ADDED BY ELIAS
library(lubridate)
library(openxlsx) # Added by Elias
```

### Loading the data sets
While loading the data sets, the variable PID in fam_anc was renamed to CID to adapt to the code.
The function data.table::fread() has been used to read the tables

```{r}
fam_anc <- data.table::fread('D:/Data/Workdata/703934/EliasA/Create dataset/Data/fam_anc.csv') %>% rename(CID = PID)
demographics <- data.table::fread('D:/Data/Workdata/703934/EliasA/Create dataset/Data/demographics.csv')
health_cond <- data.table::fread('D:/Data/Workdata/703934/EliasA/Create dataset/Data/health_cond.csv')
birth <- data.table::fread('D:/Data/Workdata/703934/EliasA/Create dataset/Data/birth.csv')
ASD <- data.table::fread('D:/Data/Workdata/703934/EliasA/Create dataset/Data/ASD.csv')
```

Model 4 from the first set of analyses
```{r}
dx_model4_prev <- readWorkbook('D:/Data/Workdata/703934/EliasA/Run for Vahe/Results/DK_R01_2023_03_22_EA.xlsx', sheet="model_4_00_12_48")
```


## Create variables to do with diagnoses

Health conditions/diagnoses: only ICD-10, create 3 level codes
```{r}
health_cond <- health_cond[health_cond$COND_DXSYS==3,] %>% # ICD-10 only
  mutate(COND_CODE=str_sub(COND_CODE, start=2, end=4)) # Level 3 only
```


Only parents of cohort children

```{r}
# Selecting the cohort children and their parents
cohort <- select(demographics, CID=PID, DOB, IN_COHORT) %>% 
  filter(IN_COHORT==1) %>% 
  left_join(select(fam_anc, CID, MID, FID)) %>% 
  select(-IN_COHORT)

# Paternal diagnoses
health_cond_pat <- health_cond %>% filter(PID %in% unique(cohort$FID)) %>% mutate(mom_dad="DAD") %>% rename(FID=PID)

# Maternal diagnoses
health_cond_mat <- health_cond %>% filter(PID %in% unique(cohort$MID)) %>% mutate(mom_dad="MOM") %>% rename(MID=PID)

# Add time difference between birth and diagnosis
all_dx_pat <- health_cond_pat %>% left_join(cohort) %>% 
  mutate(diff_time=as.numeric(difftime(DATE_DX, DOB, units="days")))

all_dx_mat <- health_cond_mat %>% left_join(cohort) %>% 
  mutate(diff_time=as.numeric(difftime(DATE_DX, DOB, units="days")))
```

Find number of days with diagnoses (in the year leading up to the birth of the child)

```{r}
# For fathers
dx_pat_cov <- all_dx_pat %>% 
  filter(diff_time>=-365 & diff_time<0) %>% # Only diagnoses in year prior to birth
  select(CID, DATE_DX) %>% # Only care about date of diagnosis
  distinct() %>% 
  group_by(CID) %>% 
  tally() %>% # Count diagnoses for that child's father
  right_join((select(cohort, CID))) %>% # Add children whose fathers may not have had a diagnosis
  mutate(p2_all=if_else(is.na(n), as.integer(0),n)) %>% # Replace missing with 0
  select(-n) %>% 
  mutate(p2_all_c_dad=cut(p2_all, breaks=c(-Inf, 0, 3, 9, Inf), labels=c("0", "1-3", "4-9", "10+"))) %>% # Create categorical variable
  select(CID, p2_all_c_dad)

# For mothers
dx_mat_cov <- all_dx_mat %>% 
  filter(diff_time>=-365 & diff_time<0) %>%  # Only diagnoses in year prior to birth
  select(CID, DATE_DX) %>% # Only care about date of diagnosis
  distinct() %>% 
  group_by(CID) %>% 
  tally() %>% # Count diagnoses for that child's mother
  right_join((select(cohort, CID))) %>% # Add children whose mothers may not have had a diagnosis
  mutate(p2_all=if_else(is.na(n), as.integer(0),n)) %>% # Replace missing with 0
  select(-n) %>% 
  mutate(p2_all_c_mom=cut(p2_all, breaks=c(-Inf, 0, 3, 9, Inf), labels=c("0", "1-3", "4-9", "10+"))) %>% # Create categorical variable
  select(CID, p2_all_c_mom)

dx_both_cov <- full_join(dx_pat_cov, dx_mat_cov)
```

### Create exposure variables

Choose diagnoses to be considered for this analysis (q<.05 in model 4 main analysis)

```{r}
dx_all_prev <- dx_model4_prev %>% filter(`q-val`<.05) %>% # Limit to diagnoses of interest
  mutate(dx=substr(ICD10, start=7, stop=9)) %>% # The level 3 diagnosis
  mutate(acute=(substr(ICD10, 1, 6)=="m0012_")) # Indicate whether the diagnosis is acute or not

dx_acute_prev <- (dx_all_prev %>% filter(acute==TRUE))[["dx"]] # The acute diagnoses
dx_chronic_prev <- (dx_all_prev %>% filter(acute==FALSE))[["dx"]] # The chronic diagnoses

```


Create diagnosis variables

```{r}
# For fathers §§§§§§§§§§§§

# Acute diagnoses
exp_pat_acute <- all_dx_pat %>% # Paternal diagnoses
  filter(COND_CODE %in% dx_acute_prev) %>% # Only acute
  filter(diff_time>=-365 & diff_time<0) %>% # Year prior to birth
  mutate(COND_CODE=paste0("d_", COND_CODE)) %>% # Add d_ to diagnosis - to indicate it is the dad's diagnosis
  select(CID, COND_CODE) %>% # Only interested in child ID and diagnosis
  distinct() %>% 
  mutate(val=1) %>% 
  reshape2::dcast(CID ~ COND_CODE, value.var="val", fun.aggregate=length) %>% # Separate variables for each diagnosis
  right_join((select(cohort, CID) %>% distinct())) %>% 
  replace(is.na(.), 0) # If no diagnoses, put down "0"

# Chronic diagnoses
exp_pat_chronic <- all_dx_pat %>% # Paternal diagnoses
  filter(COND_CODE %in% dx_chronic_prev) %>% # Only chronic
  filter(diff_time>=-4*365 & diff_time<0) %>% # 4 years prior to birth
  mutate(COND_CODE=paste0("d_", COND_CODE)) %>% # Add d_ to diagnosis - to indicate it is the dad's diagnosis
  select(CID, COND_CODE) %>% # Only interested in child ID and diagnosis
  distinct() %>% 
  mutate(val=1) %>% 
  reshape2::dcast(CID ~ COND_CODE, value.var="val", fun.aggregate=length) %>% # Separate variables for each diagnosis
  right_join((select(cohort, CID) %>% distinct())) %>% 
  replace(is.na(.), 0) # If no diagnoses, put down "0"

# For mothers §§§§§§§§§§§§
exp_mat_acute <- all_dx_mat %>% # Maternal diagnoses
  filter(COND_CODE %in% dx_acute_prev) %>% # Only acute
  filter(diff_time>=-365 & diff_time<0) %>% # Year prior to birth
  mutate(COND_CODE=paste0("m_", COND_CODE)) %>% # Add m_ to diagnosis - to indicate it is the mom's diagnosis
  select(CID, COND_CODE) %>% # Only interested in child ID and diagnosis
  distinct() %>% 
  mutate(val=1) %>% 
  reshape2::dcast(CID ~ COND_CODE, value.var="val", fun.aggregate=length) %>% # Separate variables for each diagnosis
  right_join((select(cohort, CID) %>% distinct())) %>% 
  replace(is.na(.), 0) # If no diagnoses, put down "0"

exp_mat_chronic <- all_dx_mat %>% # Maternal diagnoses
  filter(COND_CODE %in% dx_chronic_prev) %>% # Only chronic
  filter(diff_time>=-4*365 & diff_time<0) %>% # 4 years prior to birth
  mutate(COND_CODE=paste0("m_", COND_CODE)) %>% # Add m_ to diagnosis - to indicate it is the mom's diagnosis
  select(CID, COND_CODE) %>% # Only interested in child ID and diagnosis
  distinct() %>% 
  mutate(val=1) %>% 
  reshape2::dcast(CID ~ COND_CODE, value.var="val", fun.aggregate=length) %>% # Separate variables for each diagnosis
  right_join((select(cohort, CID) %>% distinct())) %>% 
  replace(is.na(.), 0) # If no diagnoses, put down "0"
```

## Find covariates and gather it all

```{r}
cov_asd <- select(cohort, CID, MID, FID) %>% 
  left_join(select(demographics, CID=PID, SEX, DOB, DOB_YEAR, MOM_AGEY, DAD_AGEY, MOM_EDU_B, DAD_EDU_B, MOM_INCOME_B, DAD_INCOME_B, DOD, DOEM_FIRST, DOLF_LATEST)) %>% 
  left_join(select(ASD, CID, ASD, DO1ST_ASD)) %>% # Join ASD information
  mutate(ASD=if_else(is.na(ASD), as.integer(0), ASD)) %>% # Change missing ASD to 0 ASD
  mutate(DOEM_FIRST=if_else(DOEM_FIRST==as.Date('0000-01-01'), as.Date("2016-12-31"), as.Date(DOEM_FIRST))) %>% # Correct the information of first emigration 
  mutate(DOD=if_else(is.na(DOD), as.Date("2016-12-31"), as.Date(DOD))) %>% # If date of death missing, change to 2016-12-31 - this is the end of follow-up, so the time censoring occurs
  mutate(fu_min = pmin(DOEM_FIRST, DOLF_LATEST, DO1ST_ASD, DOD, na.rm=T)) %>% # The day of censoring/an outcome
  mutate(time_fu=as.numeric(difftime(fu_min, DOB, units=c("days")))) %>% # Total time of follow-up
  mutate(MOM_EDU_BC=as.factor(1 + (MOM_EDU_B>=2) + (MOM_EDU_B>=4) + (MOM_EDU_B>=7))) %>% # Create mother's education variable
  mutate(DAD_EDU_BC=as.factor(1 + (DAD_EDU_B>=2) + (DAD_EDU_B>=4) + (DAD_EDU_B>=7))) %>% # Create father's education variable
  mutate(DOB_YEAR=as.factor(DOB_YEAR)) %>% # Make year of birth into categorical variable
  left_join(dx_both_cov) %>% # Add covariates regarding parents' n.o. days with diagnoses 
  select(CID, MID, FID, SEX, DOB_YEAR, MOM_AGEY, DAD_AGEY, MOM_INCOME_B, DAD_INCOME_B, ASD, fu_min, time_fu, MOM_EDU_BC, DAD_EDU_BC, p2_all_c_dad, p2_all_c_mom, DO1ST_ASD)

table(cov_asd$ASD, useNA="ifany")

cov_asd$ASD[cov_asd$DO1ST_ASD>cov_asd$fu_min] <- 0 # If censoring happens before ASD dx, we change ASD to 0

cov_asd <- cov_asd %>% select(-DO1ST_ASD)

table(cov_asd$ASD, useNA="ifany")
```

Gather the variables, and remove observations where the covariate(s) is missing

```{r}
dx_w_full <- cov_asd %>% 
  full_join(exp_pat_acute) %>% 
  full_join(exp_pat_chronic) %>% 
  full_join(exp_mat_acute) %>% 
  full_join(exp_mat_chronic) %>% 
  na.omit() %>% 
  data.frame()
```

## Identify diagnoses to be used


```{r}
count_acute_d <- rep(NA, length(dx_acute_prev))
count_acute_m <- rep(NA, length(dx_acute_prev))
count_chronic_d <- rep(NA, length(dx_chronic_prev))
count_chronic_m <- rep(NA, length(dx_chronic_prev))

# For acute diagnoses, count how many (maternal and paternal) exposed and unexposed with and without ASD we have, and save the smallest count
for(a in 1:length(dx_acute_prev)){
  if(paste0("d_", dx_acute_prev[a]) %in% colnames(dx_w_full)){
    # Paternal
    count_acute_d[a] <- min(table(dx_w_full[, c("ASD", paste0("d_", dx_acute_prev[a]))]))
  }
  # Maternal
  count_acute_m[a] <- min(table(dx_w_full[, c("ASD", paste0("m_", dx_acute_prev[a]))]))
}

# For chronic diagnoses, count how many (maternal and paternal) exposed and unexposed with and without ASD we have, and save the smallest count
for(a in 1:length(dx_chronic_prev)){
  if(paste0("d_", dx_chronic_prev[a]) %in% colnames(dx_w_full)){
    # Paternal
    count_chronic_d[a] <- min(table(dx_w_full[, c("ASD", paste0("d_", dx_chronic_prev[a]))]))
  }
  # Maternal
  count_chronic_m[a] <- min(table(dx_w_full[, c("ASD", paste0("m_", dx_chronic_prev[a]))]))
}

count_acute_d
count_acute_m
count_chronic_d
count_chronic_m

# Limit acute diagnoses to the ones where we have at least 10 exposed and unexposed mothers and fathers with and without ASD, and also remove paternal diagnosis that is linked to the mother
dx_acute <- dx_acute_prev[(count_acute_d>10) & (count_acute_m>10) & !is.na(count_acute_d) & dx_acute_prev!="O28"]
# Limit chronic diagnoses to the ones where we have at least 20 exposed and unexposed mothers and fathers with and without ASD
dx_chronic <- dx_chronic_prev[(count_chronic_d>20) & (count_chronic_m>20)  & !is.na(count_chronic_d)]

# Vector with the diagnoses to be used in the paternal analyses
dx <- c(dx_acute, dx_chronic)
dx_m <- paste0("m_", dx)
dx_d <- paste0("d_", dx)
```

## Extra checks

Covariates common enough
```{r}
table(dx_w_full[,c("ASD", "SEX")])

table(dx_w_full[,c("ASD", "p2_all_c_dad")])

table(dx_w_full[,c("ASD", "p2_all_c_mom")])

table(dx_w_full[,c("ASD", "MOM_EDU_BC")])

table(dx_w_full[,c("ASD", "DAD_EDU_BC")])
```
Diagnoses common enough
```{r}
for(d in 1:length(dx)){
  print( "\ " )
  print(paste0("Diagnosis ", dx[d]))
  print(table(dx_w_full[,c("ASD", dx_m[d])]))
  print(table(dx_w_full[,c("ASD", dx_d[d])]))
}
```



## Do the analyses

### Model 4

Run model 4 - each diagnosis is run through separately, but the maternal and paternal diagnosis co-occur

```{r}
model_4 <- data.frame()
i <- 1
for(i in 1:length(dx)){
  as_model4 <- as.formula(paste0("Surv(time_fu, ASD)~", dx_m[i], "+", dx_d[i], "+ strata(DOB_YEAR) + SEX + rcs(MOM_AGEY, quantile(MOM_AGEY, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + rcs(DAD_AGEY, quantile(DAD_AGEY, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + p2_all_c_dad + p2_all_c_mom + MOM_EDU_BC + DAD_EDU_BC + rcs(MOM_INCOME_B, quantile(MOM_INCOME_B, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + rcs(DAD_INCOME_B, quantile(DAD_INCOME_B, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + cluster(MID)")) 
  a <- coxph(as_model4, data=dx_w_full)
  indx <- (i*2-1):(i*2)
  model_4[indx,1] <- summary(a)$coefficients[1:2,6]  # p-val
  model_4[indx,2] <- summary(a)$coefficients[1:2,2]  # HR
  model_4[indx,3] <- exp(confint(a))[1:2,1]          # 2.5%
  model_4[indx,4] <- exp(confint(a))[1:2,2]          # 97.5%
  model_4[indx,5] <- c(dx_m[i], dx_d[i])    # icd code
}
```

OBS: THIS WAS NOT USED - HAS cluster(FID), WE CHOSE THE ONE USING cluster(MID)
```{r}
# model_4_FID <- data.frame()
# i <- 1
# for(i in 1:length(dx)){
#   as_model4 <- as.formula(paste0("Surv(time_fu, ASD)~", dx_m[i], "+", dx_d[i], "+ strata(DOB_YEAR) + SEX + rcs(MOM_AGEY, quantile(MOM_AGEY, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + rcs(DAD_AGEY, quantile(DAD_AGEY, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + p2_all_c_dad + p2_all_c_mom + MOM_EDU_BC + DAD_EDU_BC + rcs(MOM_INCOME_B, quantile(MOM_INCOME_B, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + rcs(DAD_INCOME_B, quantile(DAD_INCOME_B, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + cluster(FID)"))
#   a <- coxph(as_model4, data=dx_w_full)
#   indx <- (i*2-1):(i*2)
#   model_4_FID[indx,1] <- summary(a)$coefficients[1:2,6]  # p-val
#   model_4_FID[indx,2] <- summary(a)$coefficients[1:2,2]  # HR
#   model_4_FID[indx,3] <- exp(confint(a))[1:2,1]          # 2.5%
#   model_4_FID[indx,4] <- exp(confint(a))[1:2,2]          # 97.5%
#   model_4_FID[indx,5] <- c(dx_m[i], dx_d[i])    # icd code
# }
```


Find significant (paternal) diagnoses

```{r}
colnames(model_4) <- c("p", "HR", "2.5%", "97.5%", "ICD10")

# Add q-values and Bonferroni correction - using paternal diagnoses only
model_4_pat <- model_4 %>% filter(str_starts(ICD10, "d")) %>% 
  mutate(q=p.adjust(p, method = "BH")) %>% 
  mutate(Bonferroni=p.adjust(p, method = "bonferroni"))

# Find significant diagnoses (for the paternal diagnoses, q<.05)
sig_dx <- substr(model_4_pat$ICD10[model_4_pat$q<.05], start=3, stop=5)

sig_dx_pat <- paste0("d_", sig_dx)
sig_dx_mat <- paste0("m_", sig_dx)
```


Add q-values and Bonferroni correction

```{r}
model_4_extra <- model_4 %>% 
  mutate(pat=str_starts(ICD10, "d")) %>% 
  group_by(pat) %>% 
  mutate(q=p.adjust(p, method = "BH")) %>% 
  mutate(Bonferroni=p.adjust(p, method = "bonferroni")) %>% 
  data.frame()
colnames(model_4_extra)[1:4] <- colnames(model_4)[1:4]
```

```{r}
model_4_extra
```

```{r}
sig_dx
```


### Model 11

Run the model 11 analysis - all diagnoses, maternal and paternal, co-occur

```{r}
as_model_11 <- as.formula(paste0("Surv(time_fu, ASD)~", paste(sig_dx_pat, collapse=" + "), "+", paste(sig_dx_mat, collapse=" + "), "+ strata(DOB_YEAR) + SEX + rcs(MOM_AGEY, quantile(MOM_AGEY, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + rcs(DAD_AGEY, quantile(DAD_AGEY, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + p2_all_c_dad + p2_all_c_mom + MOM_EDU_BC + DAD_EDU_BC + rcs(MOM_INCOME_B, quantile(MOM_INCOME_B, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + rcs(DAD_INCOME_B, quantile(DAD_INCOME_B, c(0.05, 0.35, 0.65, 0.95), na.rm=TRUE)) + cluster(MID)"))

a_11 <- coxph(as_model_11, data=dx_w_full)
```

Gather the relevant statistics
```{r}
model_11 <- data.frame()
indx <- 1:(2*length(sig_dx))
model_11[indx,1] <- summary(a_11)$coefficients[indx,6]  # p-val
model_11[indx,2] <- summary(a_11)$coefficients[indx,2]  # HR
model_11[indx,3] <- exp(confint(a_11))[indx,1]          # 2.5%
model_11[indx,4] <- exp(confint(a_11))[indx,2]          # 97.5%
model_11[indx,5] <- rownames(summary(a_11)$coefficients)[indx]

colnames(model_11) <- c("p", "HR", "2.5%", "97.5%", "ICD10")
```

Add q-values and Bonferroni correction
```{r}
model_11_extra <- model_11 %>% 
  mutate(pat=str_starts(ICD10, "d")) %>% 
  group_by(pat) %>% 
  mutate(q=p.adjust(p, method = "BH")) %>% 
  mutate(Bonferroni=p.adjust(p, method = "bonferroni")) %>% 
  data.frame()

colnames(model_11_extra)[1:4] <- colnames(model_11)[1:4]
```

### Save results

```{r}

res_ls <- list("model_4"=model_4_extra, "model_11"=model_11_extra)

rio::export(res_ls, "D:/Data/Workdata/703934/EliasA/Run for Vahe/Results/paternal_2024_05_06.xlsx")
```


















#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*

table1_2023_08_15.R

library(dplyr)
library(stringr)
library(tidyr)
library(survival)
library(rms) # ADDED BY ELIAS
library(lubridate)
library(openxlsx) # Added by Elias
library(janitor) # For frequency tables
# library(table1)
# library(flextable) # For saving table 1

# Create data sets -----
fam_anc <- data.table::fread('D:/Data/Workdata/703934/EliasA/Create dataset/Data/fam_anc.csv') %>% rename(CID = PID)
demographics <- data.table::fread('D:/Data/Workdata/703934/EliasA/Create dataset/Data/demographics.csv')
health_cond <- data.table::fread('D:/Data/Workdata/703934/EliasA/Create dataset/Data/health_cond.csv')
birth <- data.table::fread('D:/Data/Workdata/703934/EliasA/Create dataset/Data/birth.csv')
ASD <- data.table::fread('D:/Data/Workdata/703934/EliasA/Create dataset/Data/ASD.csv')

# Copied from Vahe to make sure the data sets align

health_cond <- health_cond[health_cond$COND_DXSYS==3,]

birth_cond <- (birth %>% left_join(fam_anc, by = "CID") %>%
                 left_join(health_cond, by = c("MID" = "PID")) %>%
                 left_join(demographics, by = c("CID" = "PID")))

birth_cond$DOB <- as.Date(birth_cond$DOB, "%Y-%m-%d")
birth_cond$DATE_DX <- as.Date(birth_cond$DATE_DX, "%Y-%m-%d")

length(birth_cond$CID) #before filtering
birth_cond <- filter(birth_cond, DOB > as.Date('1997-12-31'))
length(birth_cond$CID) #after filtering

birth_cond$date_dif <- as.numeric (difftime(birth_cond$DATE_DX, birth_cond$DOB , units = c("days")))

dx_period <- function(source_database, a, b){
  
  df <- mutate(source_database, dx_yn=ifelse(date_dif < a & date_dif >= b, 1, 0))
  df <- filter(df, dx_yn==1)
  return(df)
}

birth_cond_00_12 <- dx_period(birth_cond, 0, -365)
birth_cond_12_24 <- dx_period(birth_cond, -365, -730)
birth_cond_24_36 <- dx_period(birth_cond, -730, -1095)
birth_cond_36_48 <- dx_period(birth_cond, -1095, -1460)
birth_cond_24_48 <- dx_period(birth_cond, -730, -1460)
birth_cond_00_48 <- dx_period(birth_cond, 0, -1460)

birth_cond_00_12$COND_CODE <- str_sub(birth_cond_00_12$COND_CODE, 1, 4)
birth_cond_00_12_temp_1 <- distinct(birth_cond_00_12, CID, COND_CODE, .keep_all = TRUE)
count_p1 <- as.data.frame(birth_cond_00_12_temp_1 %>% group_by(CID) %>% filter (dx_yn==1) %>% tally())         
count_p1$p1_all <- count_p1$n
count_p1$n <- NULL

birth_cond_00_12_temp_2 <- distinct(birth_cond_00_12, CID, DATE_DX, .keep_all = TRUE)
count_p2 <- as.data.frame(birth_cond_00_12_temp_2 %>% group_by(CID) %>% filter (dx_yn==1) %>% tally())         
count_p2$p2_all <- count_p2$n
count_p2$n <- NULL
birth_cond_00_12_temp_1 <- NULL
birth_cond_00_12_temp_2 <- NULL

dx_cleaning <- function(source_database){
  
  source_database$start_d <- str_sub(source_database$COND_CODE, 1, 1)
  source_database <- filter(source_database, source_database$start_d == "D")
  
  source_database$COND_CODE <- str_sub(source_database$COND_CODE, 2)
  
  source_database$start_d <- str_sub(source_database$COND_CODE, 1, 1)
  df <- filter(source_database, source_database$start_d != "U")
  
  return(df)
}

birth_cond_00_12 <- dx_cleaning (birth_cond_00_12)
birth_cond_12_24 <- dx_cleaning (birth_cond_12_24)
birth_cond_24_36 <- dx_cleaning (birth_cond_24_36)
birth_cond_36_48 <- dx_cleaning (birth_cond_36_48)
birth_cond_24_48 <- dx_cleaning (birth_cond_24_48)
birth_cond_00_48 <- dx_cleaning (birth_cond_00_48)

birth_cond_00_12$COND_CODE <- str_sub(birth_cond_00_12$COND_CODE, 1, 3)
birth_cond_12_24$COND_CODE <- str_sub(birth_cond_12_24$COND_CODE, 1, 3)
birth_cond_24_36$COND_CODE <- str_sub(birth_cond_24_36$COND_CODE, 1, 3)
birth_cond_36_48$COND_CODE <- str_sub(birth_cond_36_48$COND_CODE, 1, 3)
birth_cond_24_48$COND_CODE <- str_sub(birth_cond_24_48$COND_CODE, 1, 3)
birth_cond_00_48$COND_CODE <- str_sub(birth_cond_00_48$COND_CODE, 1, 3)

birth_cond_00_12$COND_CODE <- paste("m0012", birth_cond_00_12$COND_CODE, sep = "_")
birth_cond_12_24$COND_CODE <- paste("m1224", birth_cond_12_24$COND_CODE, sep = "_")
birth_cond_24_36$COND_CODE <- paste("m2436", birth_cond_24_36$COND_CODE, sep = "_")
birth_cond_36_48$COND_CODE <- paste("m3648", birth_cond_36_48$COND_CODE, sep = "_")
birth_cond_24_48$COND_CODE <- paste("m2448", birth_cond_24_48$COND_CODE, sep = "_")
birth_cond_00_48$COND_CODE <- paste("m0048", birth_cond_00_48$COND_CODE, sep = "_")

birth_cond_00_12_a <- distinct(birth_cond_00_12, CID, COND_CODE, .keep_all = TRUE)
birth_cond_12_24_a <- distinct(birth_cond_12_24, CID, COND_CODE, .keep_all = TRUE)
birth_cond_24_36_a <- distinct(birth_cond_24_36, CID, COND_CODE, .keep_all = TRUE)
birth_cond_36_48_a <- distinct(birth_cond_36_48, CID, COND_CODE, .keep_all = TRUE)
birth_cond_24_48_a <- distinct(birth_cond_24_48, CID, COND_CODE, .keep_all = TRUE)
birth_cond_00_48_a <- distinct(birth_cond_00_48, CID, COND_CODE, .keep_all = TRUE)

birth_cond_00_12_b <- (birth %>% left_join(birth_cond_00_12_a, by = "CID") %>%
                         select(CID, COND_CODE, dx_yn))

birth_cond_12_24_b <- (birth %>% left_join(birth_cond_12_24_a, by = "CID") %>%
                         select(CID, COND_CODE, dx_yn))

birth_cond_24_36_b <- (birth %>% left_join(birth_cond_24_36_a, by = "CID") %>%
                         select(CID, COND_CODE, dx_yn))

birth_cond_36_48_b <- (birth %>% left_join(birth_cond_36_48_a, by = "CID") %>%
                         select(CID, COND_CODE, dx_yn))

birth_cond_24_48_b <- (birth %>% left_join(birth_cond_24_48_a, by = "CID") %>%
                         select(CID, COND_CODE, dx_yn))

birth_cond_00_48_b <- (birth %>% left_join(birth_cond_00_48_a, by = "CID") %>%
                         select(CID, COND_CODE, dx_yn))

birth_cond_0012_wide <- as.data.frame(data.table::dcast(birth_cond_00_12_b, CID ~ COND_CODE, value.var="dx_yn"))
birth_cond_0012_wide[,2:ncol(birth_cond_0012_wide)][is.na(birth_cond_0012_wide[,2:ncol(birth_cond_0012_wide)])] <- 0
length(birth_cond_0012_wide$CID)
n_distinct(birth_cond_0012_wide$CID)

birth_cond_1224_wide <- as.data.frame(data.table::dcast(birth_cond_12_24_b, CID ~ COND_CODE, value.var="dx_yn"))
birth_cond_1224_wide[,2:ncol(birth_cond_1224_wide)][is.na(birth_cond_1224_wide[,2:ncol(birth_cond_1224_wide)])] <- 0
length(birth_cond_1224_wide$CID)
n_distinct(birth_cond_1224_wide$CID)

birth_cond_2436_wide <- as.data.frame(data.table::dcast(birth_cond_24_36_b, CID ~ COND_CODE, value.var="dx_yn"))
birth_cond_2436_wide[,2:ncol(birth_cond_2436_wide)][is.na(birth_cond_2436_wide[,2:ncol(birth_cond_2436_wide)])] <- 0
length(birth_cond_2436_wide$CID)
n_distinct(birth_cond_2436_wide$CID)

birth_cond_3648_wide <- as.data.frame(data.table::dcast(birth_cond_36_48_b, CID ~ COND_CODE, value.var="dx_yn"))
birth_cond_3648_wide[,2:ncol(birth_cond_3648_wide)][is.na(birth_cond_3648_wide[,2:ncol(birth_cond_3648_wide)])] <- 0
length(birth_cond_3648_wide$CID)
n_distinct(birth_cond_3648_wide$CID)

birth_cond_2448_wide <- as.data.frame(data.table::dcast(birth_cond_24_48_b, CID ~ COND_CODE, value.var="dx_yn"))
birth_cond_2448_wide[,2:ncol(birth_cond_2448_wide)][is.na(birth_cond_2448_wide[,2:ncol(birth_cond_2448_wide)])] <- 0
length(birth_cond_2448_wide$CID)
n_distinct(birth_cond_2448_wide$CID)

birth_cond_0048_wide <- as.data.frame(data.table::dcast(birth_cond_00_48_b, CID ~ COND_CODE, value.var="dx_yn"))
birth_cond_0048_wide[,2:ncol(birth_cond_0048_wide)][is.na(birth_cond_0048_wide[,2:ncol(birth_cond_0048_wide)])] <- 0
length(birth_cond_0048_wide$CID)
n_distinct(birth_cond_0048_wide$CID)

asd_dx_merge <- function(source_database){
  df <- (ASD %>% select(CID, ASD, DO1ST_ASD) %>% right_join(source_database, by = "CID"))
  df$ASD <- as.numeric(df$ASD)
  df$ASD[is.na(df$ASD)] <- 0
  df$ASD[df$DO1ST_ASD > as.Date('2016-12-31')] <- 0
  df <- select(df, -DO1ST_ASD)
  return(df)
}

asd_0012_wide <- asd_dx_merge(birth_cond_0012_wide)
asd_1224_wide <- asd_dx_merge(birth_cond_1224_wide)
asd_2436_wide <- asd_dx_merge(birth_cond_2436_wide)
asd_3648_wide <- asd_dx_merge(birth_cond_3648_wide)
asd_2448_wide <- asd_dx_merge(birth_cond_2448_wide)
asd_0048_wide <- asd_dx_merge(birth_cond_0048_wide)

asd_dx_aggregate <- function(source_database){
  df <- (source_database %>%
           group_by(ASD) %>%
           summarise_all(funs(sum)))
  df <- as.data.frame(df)
  return(df)
}

asd_0012_wide <- asd_dx_aggregate(asd_0012_wide)
asd_1224_wide <- asd_dx_aggregate(asd_1224_wide)
asd_2436_wide <- asd_dx_aggregate(asd_2436_wide)
asd_3648_wide <- asd_dx_aggregate(asd_3648_wide)
asd_2448_wide <- asd_dx_aggregate(asd_2448_wide)
asd_0048_wide <- asd_dx_aggregate(asd_0048_wide)

list_acute <- c("A08", "A09", "A63", "B34", "B37", "D23", "D24", "D25", "D27", "D69", "H10", "H52", "I80", "I83", "J03", "J18", "K29", "K30", "K35", "K40", "K52", "K59", "K80", "L02", "L03", "L05", "L08", "L30", "L50", "L68", "M20", "M25", "M53", "M54", "M62", "M65", "M67", "M75", "M77", "M79", "N10", "N13", "N20", "N30", "N63", "N64", "N70", "N71", "N75", "N76", "N83", "N84", "N87", "N96", "N98", "O00", "O02", "O03", "O04", "O08", "O12", "O13", "O14", "O20", "O21", "O22", "O23", "O24", "O26", "O28", "O30", "O31", "O32", "O33", "O34", "O35", "O36", "O40", "O41", "O42", "O43", "O44", "O45", "O46", "O47", "O48", "O60", "O61", "O62", "O63", "O64", "O65", "O66", "O67", "O68", "O69", "O70", "O71", "O72", "O73", "O74", "O75", "O80", "O82", "O85", "O86", "O87", "O90", "O91", "O92", "O98", "O99", "R00", "R06", "R07", "R10", "R11", "R31", "R33", "R42", "R50", "R51", "R52", "R53", "R55", "R87", "S00", "S01", "S02", "S05", "S06", "S10", "S13", "S20", "S30", "S40", "S43", "S50", "S52", "S60", "S61", "S62", "S63", "S70", "S80", "S81", "S82", "S83", "S90", "S91", "S92", "S93", "T14", "T15", "T39", "T40", "T43", "T63", "T78", "T81", "T88", "Z37", "Z39", "Z47", "Z48", "Z76", "Z90", "Z91")

list_acute <- paste("m0012", list_acute, sep = "_")

list_chronic <- c("A60", "B18", "C50", "D64", "D68", "E03", "E04", "E05", "E10", "E11", "E28", "E66", "F10", "F11", "F12", "F17", "F19", "F20", "F21", "F31", "F32", "F33", "F34", "F40", "F41", "F42", "F43", "F50", "F52", "F60", "F99", "G35", "G40", "G43", "G44", "G56", "H36", "H90", "I10", "I26", "I47", "I49", "J30", "J32", "J35", "J45", "K21", "K50", "K51", "K58", "L40", "M06", "M17", "M22", "M23", "M35", "M51", "N39", "N60", "N80", "N81", "N91", "N92", "N93", "N94", "N97", "O10", "R32")

list_chronic <- paste("m0048", list_chronic, sep = "_") # CHANGED BY ELIAS

full_list <- c(list_acute, list_chronic)

asd_0012_wide <- asd_0012_wide[,!sapply(asd_0012_wide, function(x) any(x<10))]
head(asd_0012_wide, 2) 
asd_0012_wide <- select(asd_0012_wide, -CID)
ncol(asd_0012_wide)
head(asd_0012_wide, 2)
col_0012_original <- subset(colnames(asd_0012_wide), colnames(asd_0012_wide) %in% full_list)

asd_0048_wide <- asd_0048_wide[,!sapply(asd_0048_wide, function(x) any(x<20))]
head(asd_0048_wide, 2) 
asd_0048_wide <- select(asd_0048_wide, -CID)
ncol(asd_0048_wide)
head(asd_0048_wide, 2)
col_0048_original <- subset(colnames(asd_0048_wide), colnames(asd_0048_wide) %in% list_chronic)

dx_0012_wide_full <- birth_cond_0012_wide[, c ("CID", col_0012_original)]
length(dx_0012_wide_full$CID)
n_distinct(dx_0012_wide_full$CID)
head(dx_0012_wide_full, 2)

col_0012 <- substr(col_0012_original, 7, 10)
col_1224 <- paste("m1224", col_0012, sep = "_")
col_1224 <- subset(col_1224, col_1224 %in% colnames(asd_1224_wide))

dx_1224_wide_full <- birth_cond_1224_wide[, c("CID", col_1224)]
length(dx_1224_wide_full$CID)
n_distinct(dx_1224_wide_full$CID)
head(dx_1224_wide_full, 2)

col_0012 <- substr(col_0012_original, 7, 10)
col_2436 <- paste("m2436", col_0012, sep = "_")
col_2436 <- subset(col_2436, col_2436 %in% colnames(asd_2436_wide))

dx_2436_wide_full <- birth_cond_2436_wide[, c("CID", col_2436)]
length(dx_2436_wide_full$CID)
n_distinct(dx_2436_wide_full$CID)
head(dx_2436_wide_full, 2)

col_0012 <- substr(col_0012_original, 7, 10)
col_3648 <- paste("m3648", col_0012, sep = "_")
col_3648 <- subset(col_3648, col_3648 %in% colnames(asd_3648_wide))

dx_3648_wide_full <- birth_cond_3648_wide[, c("CID",col_3648)]
length(dx_3648_wide_full$CID)
n_distinct(dx_3648_wide_full$CID)
head(dx_3648_wide_full, 2)

col_0012 <- substr(col_0012_original, 7, 10)
col_2448 <- paste("m2448", col_0012, sep = "_")
col_2448 <- subset(col_2448, col_2448 %in% colnames(asd_2448_wide))

dx_2448_wide_full <- birth_cond_2448_wide[, c ("CID", col_2448)]
length(dx_2448_wide_full$CID)
n_distinct(dx_2448_wide_full$CID)
head(dx_2448_wide_full, 2)

dx_0048_wide_full <- birth_cond_0048_wide[, c ("CID", col_0048_original)]
length(dx_0048_wide_full$CID)
n_distinct(dx_0048_wide_full$CID)
head(dx_0048_wide_full, 2)

cov_asd <-  (birth %>%
               left_join(fam_anc, by = "CID") %>%
               left_join(demographics, by = c("CID" = "PID")) %>%
               left_join(count_p1, by ="CID") %>%
               left_join(count_p2, by ="CID") %>%
               left_join(ASD, by ="CID"))

### Formatting the date variables, CIDY-MM-DD 
cov_asd$DOB <- as.Date(cov_asd$DOB, "%Y-%m-%d")

cov_asd$DOBY <- as.factor(str_sub(cov_asd$DOB, 1, 4))
cov_asd[, "p1_all"][is.na(cov_asd[, "p1_all"])] <- 0
cov_asd[, "p2_all"][is.na(cov_asd[, "p2_all"])] <- 0
cov_asd$ASD[is.na(cov_asd$ASD)] <- 0

class(cov_asd$p1_all)
cov_asd$p1_all <- as.numeric(cov_asd$p1_all)
class(cov_asd$p2_all)
cov_asd$p2_all <- as.numeric(cov_asd$p2_all)

cov_asd$p1_all_c <- ntile(cov_asd$p1_all, 5)
#cov_asd$p2_all_c <- ntile(cov_asd$p2_all, 5)
cov_asd$p2_all_c <- cut(cov_asd$p2_all, breaks=c(-Inf, 0, 3, 9, Inf), labels=c("0", "1-3", "4-9", "10+"))

cov_asd$p1_all_c <- as.factor(cov_asd$p1_all_c)
cov_asd$p2_all_c <- as.factor(cov_asd$p2_all_c)

cov_asd$ASD[cov_asd$DO1ST_ASD > as.Date('2016-12-31')] <- 0
table(cov_asd$ASD)

#table(cond_all_wide_full$DO1ST_ASD)
cov_asd$DO1ST_ASD[is.na(cov_asd$DO1ST_ASD)] <- "2016-12-31"

cov_asd$DOEM_FIRST[ cov_asd$DOEM_FIRST == as.Date('0000-01-01')] <- "2017-01-01"

#table(cond_all_wide_full$DO1ST_ASD)
cov_asd$DOD[is.na(cov_asd$DOD)] <- "2016-12-31"
cov_asd <- transform(cov_asd, fu_min = pmin(DOEM_FIRST, DOLF_LATEST, DO1ST_ASD, DOD, na.rm=T))
cov_asd$time_fu <- as.numeric (difftime(cov_asd$fu_min , cov_asd$DOB , units = c("days")))

cov_asd$ASD[cov_asd$DO1ST_ASD>cov_asd$fu_min] <- 0 # ADDED BY ELIAS: If censoring happens before ASD dx, we change ASD to 0

birth_year_asd <-table(cov_asd$DOBY, cov_asd$ASD)
birth_year_asd
prop.table(birth_year_asd, 2)

cov_asd$MOM_EDU_BC <- cov_asd$MOM_EDU_B
cov_asd$MOM_EDU_BC[cov_asd$MOM_EDU_B > 1  & cov_asd$MOM_EDU_B < 4] <- 2
cov_asd$MOM_EDU_BC[cov_asd$MOM_EDU_B > 3  & cov_asd$MOM_EDU_B < 7] <- 3
cov_asd$MOM_EDU_BC[cov_asd$MOM_EDU_B > 6  & cov_asd$MOM_EDU_B < 10] <- 4


cov_asd$DAD_EDU_BC <- cov_asd$DAD_EDU_B
cov_asd$DAD_EDU_BC[cov_asd$DAD_EDU_B > 1  & cov_asd$DAD_EDU_B < 4] <- 2
cov_asd$DAD_EDU_BC[cov_asd$DAD_EDU_B > 3  & cov_asd$DAD_EDU_B < 7] <- 3
cov_asd$DAD_EDU_BC[cov_asd$DAD_EDU_B > 6  & cov_asd$DAD_EDU_B < 10] <- 4

#cov_asd$EDU <- pmax(cov_asd$MOM_EDU_BC, cov_asd$DAD_EDU_BC, na.rm=TRUE)
cov_asd$EDU_B <- cov_asd$MOM_EDU_BC
cov_asd$EDU_B <- as.factor(cov_asd$EDU_B)

#cov_asd$INCOME_B1 <- pmax(cov_asd$MOM_INCOME_B, cov_asd$DAD_INCOME_B)
#cov_asd$INCOME_B1[is.na(cov_asd$MOM_INCOME_B)] <- cov_asd$DAD_INCOME_B[is.na(cov_asd$MOM_INCOME_B)]
#cov_asd$INCOME_B1[is.na(cov_asd$DAD_INCOME_B)] <- cov_asd$MOM_INCOME_B[is.na(cov_asd$DAD_INCOME_B)]
cov_asd$INCOME_B1 <- cov_asd$MOM_INCOME_B
cov_asd$INCOME_B2 <- cov_asd$INCOME_B1^2
cov_asd$INCOME_B3 <- cov_asd$INCOME_B1^3


cov_asd$EDU_D <- cov_asd$DAD_EDU_BC
cov_asd$EDU_D <- as.factor(cov_asd$EDU_D)

cov_asd$INCOME_D1 <- cov_asd$DAD_INCOME_B
cov_asd$INCOME_D2 <- cov_asd$INCOME_D1^2
cov_asd$INCOME_D3 <- cov_asd$INCOME_D1^3

birth_CID <- select(birth, CID)
b_dx0012_w_full <- (dx_0012_wide_full %>% right_join(birth_CID, by = "CID")) 
b_dx0012_w_full[,2:ncol(b_dx0012_w_full)][is.na(b_dx0012_w_full[,2:ncol(b_dx0012_w_full)])] <- 0
b_dx0012_w_full <- b_dx0012_w_full[, colnames(dx_0012_wide_full)]

b_dx1224_w_full <- (dx_1224_wide_full %>% right_join(birth_CID, by = "CID")) 
b_dx1224_w_full[,2:ncol(b_dx1224_w_full)][is.na(b_dx1224_w_full[,2:ncol(b_dx1224_w_full)])] <- 0
b_dx1224_w_full <- b_dx1224_w_full[, colnames(dx_1224_wide_full)]

b_dx2436_w_full <- (dx_2436_wide_full %>% right_join(birth_CID, by = "CID")) 
b_dx2436_w_full[,2:ncol(b_dx2436_w_full)][is.na(b_dx2436_w_full[,2:ncol(b_dx2436_w_full)])] <- 0
b_dx2436_w_full <- b_dx2436_w_full[, colnames(dx_2436_wide_full)]

b_dx3648_w_full <- (dx_3648_wide_full %>% right_join(birth_CID, by = "CID")) 
b_dx3648_w_full[,2:ncol(b_dx3648_w_full)][is.na(b_dx3648_w_full[,2:ncol(b_dx3648_w_full)])] <- 0
b_dx3648_w_full <- b_dx3648_w_full[, colnames(dx_3648_wide_full)]

b_dx2448_w_full <- (dx_2448_wide_full %>% right_join(birth_CID, by = "CID")) 
b_dx2448_w_full[,2:ncol(b_dx2448_w_full)][is.na(b_dx2448_w_full[,2:ncol(b_dx2448_w_full)])] <- 0
b_dx2448_w_full <- b_dx2448_w_full[, colnames(dx_2448_wide_full)]

b_dx0048_w_full <- (dx_0048_wide_full %>% right_join(birth_CID, by = "CID")) 
b_dx0048_w_full[,2:ncol(b_dx0048_w_full)][is.na(b_dx0048_w_full[,2:ncol(b_dx0048_w_full)])] <- 0
b_dx0048_w_full <- b_dx0048_w_full[, colnames(dx_0048_wide_full)]

b_dx0012_0048_w_full <- (b_dx0012_w_full %>% left_join(b_dx0048_w_full, by = "CID") %>% right_join(birth_CID, by = "CID")) 
b_dx0012_0048_w_full[,2:ncol(b_dx0012_0048_w_full)][is.na(b_dx0012_0048_w_full[,2:ncol(b_dx0012_0048_w_full)])] <- 0
b_dx0012_0048_w_full <- b_dx0012_0048_w_full[, colnames(b_dx0012_0048_w_full)]

b_dx0012_0048_w_full <-  (b_dx0012_0048_w_full %>%
                            left_join(cov_asd, by = "CID"))

b_dxall_w_full <-  (b_dx0012_w_full %>%
                      left_join(b_dx1224_w_full, by = "CID") %>%
                      left_join(b_dx2436_w_full, by = "CID") %>%
                      left_join(b_dx3648_w_full, by = "CID") %>%
                      left_join(b_dx2448_w_full, by = "CID") %>%
                      left_join(b_dx0048_w_full, by = "CID") %>%
                      left_join(cov_asd, by = "CID"))

############# Create table 1 -------------------------

# The data set needed. Make sure all categorical variables are listed as such (and assign "readable" labels), and create necessary variables

tb_all <- b_dxall_w_full
tb_all$ASD <- factor(tb_all$ASD, levels=c(1, 0), labels=c("ASD", "No ASD"))
tb_all$SEX <- factor(tb_all$SEX, levels=c("F", "M"), labels=c("Female", "Male"))
tb_all$EDU_B <- factor(tb_all$EDU_B, labels=c("Primary and lower secondary level", "Upper secondary level and secondary vocational education", "Short cycle tertiary, bachelor or equivalent", "Master, doctoral or equivalent"))
tb_all$EDU_D <- factor(tb_all$EDU_B, labels=c("Primary and lower secondary level", "Upper secondary level and secondary vocational education", "Short cycle tertiary, bachelor or equivalent", "Master, doctoral or equivalent")) # NOTE!!!!!!!!!! MISTAKE HERE - USED EDU_B, NOT EDU_D - CORRECT IF RUN AGAIN
tb_all$parity <- 1
tb_all$parity[tb_all$PARITY1 > 1] <- 2
tb_all$parity[tb_all$PARITY1 > 3] <- 3
tb_all$parity[tb_all$PARITY1 > 5] <- 4
tb_all$parity <- factor(tb_all$parity, labels=c("1", "2-3", "4-5", "6+"))
tb_all$COMORB_ID <- factor(tb_all$COMORB_ID, labels=c("No", "Yes"))
# tb_all$DOBY <- factor(tb_all$DOBY)

count_MID <- as.data.frame(b_dxall_w_full %>% group_by(MID) %>% tally())         # MID Sibs
count_MID$MID_freq <- count_MID$n
count_MID$n <- NULL
tb_all <- left_join(tb_all, count_MID)
tb_all$sib_mother <- as.numeric((tb_all$MID_freq>1))
tb_all$sib_mother <- factor((tb_all$sib_mother), labels=c("No", "Yes"))
# 
# label(tb_all$SEX) <- "Sex, n (%)"
# label(tb_all$DOBY) <- "Child's year of birth, n (%)"

#§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
## CREATE THE TABLE
#§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§

# Mother's age at birth
var <- tb_all$MOM_AGEY
sd_v <- round(c(aggregate(var, list(tb_all$ASD), FUN=sd)[,2], sd(var)), digits=1)
m_v <- round(c(aggregate(var, list(tb_all$ASD), FUN=mean)[,2], mean(var)), digits=1)
tb1 <- c("Maternal age at delivery, mean (SD)", paste0(m_v, " (", sd_v, ")"))

# Child's sex
temp <- tb_all %>% tabyl(SEX, ASD) %>% 
  adorn_totals(where="col") %>% 
  adorn_percentages("col") %>% 
  adorn_pct_formatting(digits=1) %>% 
  adorn_ns(position="front")
temp[,1] <- as.character(temp[,1])
temp <- rbind(c("Child's sex, n (%)", "", "", ""), temp)
tb1 <- rbind(tb1, temp)
colnames(tb1) <- c("Variable", paste0("ASD (n=", sum(tb_all$ASD=="ASD"), ")"), paste0("No ASD (n=", sum(tb_all$ASD=="No ASD"), ")"), paste0("Total (N=", nrow(tb_all), ")"))

# Year of birth
temp <- tb_all %>% tabyl(DOBY, ASD) %>% 
  adorn_totals(where="col") %>% 
  adorn_percentages("col") %>% 
  adorn_pct_formatting(digits=1) %>% 
  adorn_ns(position="front")
temp[,1] <- as.character(temp[,1])
temp <- rbind(c("Child's year of birth, n (%)", "", "", ""), temp)
colnames(temp) <- colnames(tb1)
tb1 <- rbind(tb1, temp)

# Maternal income
var <- tb_all$INCOME_B1
sd_v <- round(c(aggregate(var, list(tb_all$ASD), FUN=sd, na.rm=TRUE)[,2], sd(var, na.rm=TRUE)), digits=0)
m_v <- round(c(aggregate(var, list(tb_all$ASD), FUN=mean, na.rm=TRUE)[,2], mean(var, na.rm=TRUE)), digits=0)
temp <- c("Maternal income in DKK the year before delivery, mean (SD)", paste0(m_v, " (", sd_v, ")"))
tb1 <- rbind(tb1, temp)

tb_all$var <- is.na(var)
temp <- tb_all %>% 
  tabyl(var, ASD) %>% 
  adorn_totals(where="col") %>% 
  adorn_percentages("col") %>% 
  adorn_pct_formatting(digits=1) %>% 
  adorn_ns(position="front")
#temp[,1] <- as.character(temp[,1])
temp <- unlist(temp[2,])
temp[1] <- "missing, n (%)"
tb1 <- rbind(tb1, temp)

# Paternal income
var <- tb_all$INCOME_D1
sd_v <- round(c(aggregate(var, list(tb_all$ASD), FUN=sd, na.rm=TRUE)[,2], sd(var, na.rm=TRUE)), digits=0)
m_v <- round(c(aggregate(var, list(tb_all$ASD), FUN=mean, na.rm=TRUE)[,2], mean(var, na.rm=TRUE)), digits=0)
temp <- c("Paternal income in DKK the year before delivery, mean (SD)", paste0(m_v, " (", sd_v, ")"))
tb1 <- rbind(tb1, temp)

tb_all$var <- is.na(var)
temp <- tb_all %>% 
  tabyl(var, ASD) %>% 
  adorn_totals(where="col") %>% 
  adorn_percentages("col") %>% 
  adorn_pct_formatting(digits=1) %>% 
  adorn_ns(position="front")
#temp[,1] <- as.character(temp[,1])
temp <- unlist(temp[2,])
temp[1] <- "missing, n (%)"
tb1 <- rbind(tb1, temp)

# Maternal education
temp <- tb_all %>% tabyl(EDU_B, ASD) %>% 
  adorn_totals(where="col") %>% 
  adorn_percentages("col") %>% 
  adorn_pct_formatting(digits=1) %>% 
  adorn_ns(position="front")
temp[,1] <- as.character(temp[,1])
temp <- rbind(c("Maternal education the year before delivery, n (%)", "", "", ""), temp)
colnames(temp) <- colnames(tb1)
tb1 <- rbind(tb1, temp)

# Paternal education
temp <- tb_all %>% tabyl(EDU_D, ASD) %>% 
  adorn_totals(where="col") %>% 
  adorn_percentages("col") %>% 
  adorn_pct_formatting(digits=1) %>% 
  adorn_ns(position="front")
temp[,1] <- as.character(temp[,1])
temp <- rbind(c("Paternal education the year before delivery, n (%)", "", "", ""), temp)
colnames(temp) <- colnames(tb1)
tb1 <- rbind(tb1, temp)

# Maternal diagnosis days
temp <- tb_all %>% tabyl(p2_all_c, ASD) %>% 
  adorn_totals(where="col") %>% 
  adorn_percentages("col") %>% 
  adorn_pct_formatting(digits=1) %>% 
  adorn_ns(position="front")
temp[,1] <- as.character(temp[,1])
temp <- rbind(c("Maternal number of days with health care visits the year leading up to delivery, n (%)", "", "", ""), temp)
colnames(temp) <- colnames(tb1)
tb1 <- rbind(tb1, temp)

# Parity
temp <- tb_all %>% tabyl(parity, ASD) %>% 
  adorn_totals(where="col") %>% 
  adorn_percentages("col") %>% 
  adorn_pct_formatting(digits=1) %>% 
  adorn_ns(position="front")
temp[,1] <- as.character(temp[,1])
temp <- rbind(c("Child's parity, n (%)", "", "", ""), temp)
colnames(temp) <- colnames(tb1)
tb1 <- rbind(tb1, temp)

# §§§§§§§§ What we stratify by

# Intellectual disability
temp <- tb_all %>% tabyl(COMORB_ID, ASD) %>% 
  adorn_totals(where="col") %>% 
  adorn_percentages("col") %>% 
  adorn_pct_formatting(digits=1) %>% 
  adorn_ns(position="front")
temp[,1] <- as.character(temp[,1])
temp <- rbind(c("Child has an intellectual disability, n (%)", "", "", ""), temp)
colnames(temp) <- colnames(tb1)
tb1 <- rbind(tb1, temp)

# Sibling through the mother
temp <- tb_all %>% tabyl(sib_mother, ASD) %>% 
  adorn_totals(where="col") %>% 
  adorn_percentages("col") %>% 
  adorn_pct_formatting(digits=1) %>% 
  adorn_ns(position="front")
temp[,1] <- as.character(temp[,1])
temp <- rbind(c("Mother has at least two children in the cohort, n (%)", "", "", ""), temp)
colnames(temp) <- colnames(tb1)
tb1 <- rbind(tb1, temp)



tb1[,1] <- replace_na(tb1[,1], "Missing")
tb1


wb <- createWorkbook()
addWorksheet(wb, "Table 1")
writeData(wb, "Table 1", tb1)


####### Create table S1 -----------------------------
tb_dx <- b_dxall_w_full
tb_dx$ASD <- factor(tb_dx$ASD, levels=c(1, 0), labels=c("ASD", "No ASD"))

# Find relevant diagnoses
dx_ea <- colnames(b_dx0012_0048_w_full)[2:(which(colnames(b_dx0012_0048_w_full)=="PREG_ID") -1)]
dx_ea_acute <- substr(list_acute, start=7, stop=9)
dx_ea_chronic <- substr(list_chronic, start=7, stop=9)

dx_ea_sig_a <- c("O02", "O14", "O24", "O28", "O30", "O35", "O36", "O42", "O47", "O62", "O68", "O72", "O82", "O99", "S02", "S05", "S93", "T14", "T43", "Z37")


#§§§§§§§§§§§§§§§ The acute diagnoses §§§§§§§§§§§§§§§§§§§§§§§§§§§

# Time period 0-12
temp <- aggregate(tb_dx[, paste0("m0012_", dx_ea_acute[1])], list(tb_dx$ASD), FUN=sum)[,2]
for(i in 2:length(dx_ea_acute)){
  temp <- rbind(temp, aggregate(tb_dx[, paste0("m0012_", dx_ea_acute[i])], list(tb_dx$ASD), FUN=sum)[,2])
}

tbs1 <- temp

# Time period 12-24
temp <- aggregate(tb_dx[, paste0("m1224_", dx_ea_acute[1])], list(tb_dx$ASD), FUN=sum)[,2]
for(i in 2:length(dx_ea_acute)){
  temp <- rbind(temp, aggregate(tb_dx[, paste0("m1224_", dx_ea_acute[i])], list(tb_dx$ASD), FUN=sum)[,2])
}

tbs1 <- cbind(tbs1, temp)

# Time period 24-36
temp <- aggregate(tb_dx[, paste0("m2436_", dx_ea_acute[1])], list(tb_dx$ASD), FUN=sum)[,2]
for(i in 2:length(dx_ea_acute)){
  temp <- rbind(temp, aggregate(tb_dx[, paste0("m2436_", dx_ea_acute[i])], list(tb_dx$ASD), FUN=sum)[,2])
}

tbs1 <- cbind(tbs1, temp)

# Time period 36-48
temp <- aggregate(tb_dx[, paste0("m3648_", dx_ea_acute[1])], list(tb_dx$ASD), FUN=sum)[,2]
for(i in 2:length(dx_ea_acute)){
  temp <- rbind(temp, aggregate(tb_dx[, paste0("m3648_", dx_ea_acute[i])], list(tb_dx$ASD), FUN=sum)[,2])
}

tbs1 <- cbind(tbs1, temp)

rownames(tbs1) <- dx_ea_acute
colnames(tbs1) <- c("0-12: ASD", "0-12: no ASD", "12-24: ASD", "12-24: no ASD", "24-36: ASD", "24-36: no ASD", "36-48: ASD", "36-48: no ASD")

tbs1_all <- tbs1[,1:2]
tbs1_sig <- tbs1[rownames(tbs1_all) %in% dx_ea_sig_a,]

addWorksheet(wb, "Table S1, 0-12")
writeData(wb, "Table S1, 0-12", tbs1_all, rowNames=TRUE)

addWorksheet(wb, "Table S1, sig dx")
writeData(wb, "Table S1, sig dx", tbs1_sig, rowNames=TRUE)

#§§§§§§§§§§§§§§§ The chronic diagnoses §§§§§§§§§§§§§§§§§§§§§§§§§§§

# Time period 0-48
temp <- aggregate(tb_dx[, paste0("m0048_", dx_ea_chronic[1])], list(tb_dx$ASD), FUN=sum)[,2]
for(i in 2:length(dx_ea_chronic)){
  temp <- rbind(temp, aggregate(tb_dx[, paste0("m0048_", dx_ea_chronic[i])], list(tb_dx$ASD), FUN=sum)[,2])
}

tbs2 <- temp

rownames(tbs2) <- dx_ea_chronic
colnames(tbs2) <- c("0-48: ASD", "0-48: no ASD")

addWorksheet(wb, "Table S2")
writeData(wb, "Table S2", tbs2, rowNames=TRUE)

saveWorkbook(wb, "D:/Data/Workdata/703934/EliasA/Run for Vahe/Results/table1.xlsx", overwrite=TRUE)



















#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*

tables_birth_paternal_dx_2023_07_13.Rmd

---
  title: "Paternal diagnoses"
author: "Elias Arildskov"
date: '2023-07-07'
output: html_document
---
  
  ### Loading the required librabies
  ```{r include=FALSE}
library(dplyr)
library(stringr)
library(tidyr)
library(survival)
library(rms) # ADDED BY ELIAS
library(lubridate)
library(openxlsx) # Added by Elias
```

### Loading the data sets
While loading the data sets, the variable PID in fam_anc was renamed to CID to adapt to the code.
The function data.table::fread() has been used to read the tables

```{r}
fam_anc <- data.table::fread('D:/Data/Workdata/703934/EliasA/Create dataset/Data/fam_anc.csv') %>% rename(CID = PID)
demographics <- data.table::fread('D:/Data/Workdata/703934/EliasA/Create dataset/Data/demographics.csv')
health_cond <- data.table::fread('D:/Data/Workdata/703934/EliasA/Create dataset/Data/health_cond.csv')
birth <- data.table::fread('D:/Data/Workdata/703934/EliasA/Create dataset/Data/birth.csv')
ASD <- data.table::fread('D:/Data/Workdata/703934/EliasA/Create dataset/Data/ASD.csv')
dx_model4 <- readWorkbook('D:/Data/Workdata/703934/EliasA/Run for Vahe/Results/DK_R01_2023_03_22_EA.xlsx', sheet="model_4_00_12_48") # CORRECT WHEN KNOW WHAT MODEL/FILE TO USE
birth <- data.table::fread('D:/Data/Workdata/703934/EliasA/Create dataset/Data/birth.csv')
```

## Paternal diagnoses

### Find diagnoses

Identify diagnoses that are significant in model 4 and thus to be used:
  
  ```{r}
dx_sig <- dx_model4 %>% 
  filter(`q-val`<0.05) %>% 
  pull(ICD10)

# dx_sig_0012 <- str_detect(dx_sig$ICD10, "^m0012_")

dx_sig_0012 <- dx_sig[str_detect(dx_sig, "^m0012_")] %>% str_sub(start=7)

dx_sig_0048 <- dx_sig[str_detect(dx_sig, "^m0048")] %>% str_sub(start=7)

dx_sig <- c(dx_sig_0012, dx_sig_0048)
```


Extract relevant information

```{r}
tb_cohort <- demographics %>% 
  filter(IN_COHORT==1) %>% 
  select(PID, SEX, DOB, DOB_YEAR, MOM_AGEY, DAD_AGEY, MOM_EDU_B, MOM_INCOME_B, DAD_EDU_B, DAD_INCOME_B) %>% 
  left_join(select(fam_anc, PID=CID, MID, FID))
```

Limit to diagnoses that would be seen in cis fathers
```{r}
dx_tb <- dx_sig[!(substr(dx_sig,1,1)=="O") & !(dx_sig %in% c("Z37", "N94"))]
```



Find diagnoses in fathers

```{r}
health_cond_sig <- health_cond %>% 
  filter(COND_DXSYS==3) %>% 
  mutate(COND_CODE=substr(COND_CODE, 2,4)) %>% 
  filter(COND_CODE %in% dx_sig) %>% 
  filter(PID %in% tb_cohort$FID) %>% 
  rename(FID=PID) %>% 
  left_join(select(tb_cohort, PID, FID, DOB)) %>% 
  mutate(diff_time=as.numeric(difftime(DATE_DX, DOB, units="days"))) %>% 
  filter((diff_time>=-365*4 & diff_time<0 & (COND_CODE %in% dx_sig_0048)) | (diff_time>=-365 & diff_time<0 & (COND_CODE %in% dx_sig_0012)))

dx_dad <- health_cond_sig %>% 
  select(PID, COND_CODE) %>% 
  distinct() %>% 
  mutate(val=1) %>% 
  reshape2::dcast(PID ~ COND_CODE, value.var="val", fun.aggregate=length) %>% 
  right_join((select(tb_cohort, PID) %>% distinct())) %>% 
  as_tibble()

dx_dad[is.na(dx_dad)] <- 0
```


Remove observations with missing

```{r}
tb_m4 <- tb_cohort %>% 
  left_join(dx_dad) %>% 
  left_join(select(ASD, PID=CID, ASD)) %>% 
  filter(!is.na(SEX) & !is.na(MOM_AGEY) & !is.na(MOM_EDU_B) & !is.na(MOM_INCOME_B)) %>% # Non-missing model 4
  filter(!is.na(FID)) %>%  # Non-missing FID
  as_tibble()

tb_m4$ASD[is.na(tb_m4$ASD)] <- 0
tb_m4$ASD <- as.factor(tb_m4$ASD)

tb_m4_father <- tb_m4 %>% 
  filter(!is.na(DAD_INCOME_B)) %>% 
  as_tibble()
```


### Create tables

```{r}

#table(tb_m4$get(dx_tb[1]))
```

```{r}
table_m4 <- matrix(NA,nrow=2, ncol=length(dx_tb))
rownames(table_m4) <- c("ASD 0", "ASD 1")
colnames(table_m4) <- dx_tb
table_m4_father <- table_m4

for(i in 1:length(dx_tb)){
  table_m4[,i] <- table(tb_m4$ASD[tb_m4[,dx_tb[i]]==1])
  table_m4_father[,i] <- table(tb_m4_father$ASD[tb_m4_father[,dx_tb[i]]==1])
}

table_m4
table_m4_father
```

Rounded tables
```{r}
rounded_m4 <- matrix(cut(c(table_m4[1,], table_m4[2,]), breaks=c(-Inf, 5, 9, 20, Inf), labels=c("0-5", "6-9", "10-20", ">20")), nrow=2, byrow=TRUE)
colnames(rounded_m4) <- colnames(table_m4)
rownames(rounded_m4) <- rownames(table_m4)
table_m4
rounded_m4

rounded_m4_father <- matrix(cut(c(table_m4_father[1,], table_m4_father[2,]), breaks=c(-Inf, 5, 9, 20, Inf), labels=c("0-5", "6-9", "10-20", ">20")), nrow=2, byrow=TRUE)
colnames(rounded_m4_father) <- colnames(table_m4_father)
rownames(rounded_m4_father) <- rownames(table_m4_father)
```

## From the birth table

Create gestational age in days variable which is the mean of the 5 surrounding 
```{r}
gest <- birth[order(GESTAGE_DAYS),] %>% 
  select(GESTAGE_DAYS) %>% 
  mutate(lag2=lag(GESTAGE_DAYS,2)) %>% 
  mutate(lag1=lag(GESTAGE_DAYS,1)) %>% 
  mutate(lead1=lead(GESTAGE_DAYS,1)) %>% 
  mutate(lead2=lead(GESTAGE_DAYS,2))

gest$GESTAGE_DAYS_mean5 <- rowMeans(gest)


#GESTAGE_DAYS_mean5 <- rowMeans(matrix(c(lag(birth$GESTAGE_DAYS,2), lag(birth$GESTAGE_DAYS,1), birth$GESTAGE_DAYS, lead(birth$GESTAGE_DAYS,1), lead(birth$GESTAGE_DAYS,2)), na.rm=TRUE))
#GESTAGE_DAYS_mean5 <- rowMeans(gest)
```


```{r}
hypox <- table(birth$HYPOXIA_FETUS, useNA = 'ifany')
rounded_hypox <- round(hypox, -2) # Round to nearest 100

m_hypox <- matrix(rounded_hypox, nrow=1)
colnames(m_hypox) <- names(rounded_hypox)

apgar5 <- table(birth$APGAR5)
rounded_apgar5 <- round(apgar5, -2) # Round to nearest 100
rounded_apgar5[12] <- round(mean(is.na(birth$APGAR5))*100, 3)
names(rounded_apgar5)[12] <- "% missing"

m_apgar5 <- matrix(rounded_apgar5, nrow=1)
colnames(m_apgar5) <- names(rounded_apgar5)

gestage <- quantile(gest$GESTAGE_DAYS_mean5, probs=c(0.05, 0.25, 0.5, 0.75, 0.95), na.rm=TRUE)
gestage[6] <- mean(gest$GESTAGE_DAYS, na.rm=TRUE)
gestage[7] <- mean(is.na(gest$GESTAGE_DAYS))*100
gestage <- round(gestage, 3) # Round to nearest 1/1000
names(gestage) <- c(names(gestage)[1:5], "mean", "% missing")

m_gestage <- matrix(gestage, nrow=1)
colnames(m_gestage) <- names(gestage)

```


## Export
```{r}
wb <- createWorkbook()
addWorksheet(wb, "Paternal dx")
writeData(wb, "Paternal dx", "COVARIATES IN MODEL 4 NON-MISSING", startRow = 1, startCol = 1)
writeData(wb, "Paternal dx", "N.o. diagnoses in fathers (where the child has ASD and where it doesn't) in a period leading up to the birth", startRow = 2, startCol = 1)
writeData(wb, "Paternal dx", rounded_m4, startRow = 3, startCol = 1, colNames = TRUE, rowNames = TRUE)

writeData(wb, "Paternal dx", "COVARIATES IN MODEL 4 AND PATERNAL INCOME NON-MISSING", startRow = 8, startCol = 1)
writeData(wb, "Paternal dx", "N.o. diagnoses in fathers (where the child has ASD and where it doesn't) in a period leading up to the birth", startRow = 9, startCol = 1)
writeData(wb, "Paternal dx", rounded_m4_father, startRow = 10, startCol = 1, colNames = TRUE, rowNames = TRUE)

addWorksheet(wb, "Variables birth table")
writeData(wb, "Variables birth table", "N.o. births with and without hypoxia in the fetus (rounded to nearest 100)", startRow = 1, startCol = 1)
writeData(wb, "Variables birth table", m_hypox, startRow = 2, startCol = 1, colNames = TRUE, rowNames=FALSE)

writeData(wb, "Variables birth table", "Table of apgar score 5 minutes after birth, rounded to nearest 100", startRow = 6, startCol = 1)
writeData(wb, "Variables birth table", m_apgar5, startRow = 7, startCol = 1, colNames = TRUE, rowNames=FALSE)

writeData(wb, "Variables birth table", "Gestational age (in days) in pseudo-percentiles, mean and % mising (rounded)", startRow = 11, startCol = 1)
writeData(wb, "Variables birth table", m_gestage, startRow = 12, startCol = 1, colNames = TRUE, rowNames=FALSE)


saveWorkbook(wb, "D:/Data/Workdata/703934/EliasA/Run for Vahe/Results/tables_birth_paternal_dx_2023_07_13.xlsx", overwrite = TRUE)
```



