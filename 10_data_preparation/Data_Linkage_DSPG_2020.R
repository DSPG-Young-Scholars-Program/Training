library(data.table)
library(RecordLinkage)

setwd("C:/Users/abrahamt/Desktop/Data Linkage (DSPG)")

########################################################################################################

#Load Cleaned System Files to Match

file_1_dedup_nosplit <- fread("Cleaned_Unsplit_ds.csv", colClasses = "character")
file_2_dedup_nosplit <- fread("Cleaned_Unsplit_frx.csv", colClasses = "character")

########################################################################################################

#Create Split Names Files

 ### A Set of Functions to Split Multiple Names into Multiple Rows (Thanks Again Aaron!)

    string2vector <- function(string, delimeter=",") {
                              unlist(strsplit(string, delimeter))
                             }

     new_row_per_string_item <- function(single_row_dt, fname_col, lname_col, delims = "[-, ]") {
                                 dt <- data.table::setDT(single_row_dt)
                                  fnames <- string2vector(dt[, get(fname_col)], delimeter = delims)
                                  if (is.na(fnames[1]) == TRUE) fnames <- ""
                                   lnames <- string2vector(dt[, get(lname_col)], delimeter = delims)
                                    if (is.na(lnames[1]) == TRUE) lnames <- ""
                                     for (fn in fnames) {
                                      for (ln in lnames) {
                                       new_dt <- data.table::copy(dt)
                                        new_dt[, eval(fname_col) := fn]
                                         new_dt[, eval(lname_col) := ln]
                                     if (exists("out_dt") == TRUE) {
                                         out_dt <- data.table::rbindlist(list(out_dt, new_dt))
                                         }
                                      else{
                                       out_dt <- new_dt
                                      }
                                     }
                                    }
                              out_dt
                             }

 #CREATE NEW ROWS FOR MULTIPLE FIRST AND LAST NAME ENTRIES

  file_1_prepared <- file_1_dedup_nosplit[, new_row_per_string_item(.SD, fname_col = "first_name", lname_col = "last_name"), uid]
  file_2_prepared <- file_2_dedup_nosplit[, new_row_per_string_item(.SD, fname_col = "first_name", lname_col = "last_name"), uid]

  #Take a look

   head(file_1_prepared[uid %in% file_1_prepared[, .N, uid][N > 1, uid]], n = 20)
   head(file_2_prepared[uid %in% file_2_prepared[, .N, uid][N > 1, uid]], n = 20)
   
   #Duplication? Due to Same Name Twice in Either First or Last Field
   
    file_1_prepared[duplicated(file_1_prepared[, c(1:7)]), ]
    file_1_prepared <- file_1_prepared[!duplicated(file_1_prepared[, c(1:7)]), ]

    file_2_prepared[duplicated(file_2_prepared[, c(1:7)]), ]
    file_2_prepared <- file_2_prepared[!duplicated(file_2_prepared[, c(1:7)]), ]
    
########################################################################################################

#SET SYSTEM DATASET IDS ---- Change leader depending on Systems

 file_1_abbr <- "ds"
 file_2_abbr <- "frx"

 file_1_id <- paste0("id_", file_1_abbr)
 file_2_id <- paste0("id_", file_2_abbr)

########################################################################################################

#CREATE MATCHED PAIRS WITH BLOCKING

 pairs <- compare.linkage(dataset1 = file_1_prepared,
                         dataset2 = file_2_prepared,
                         blockfld = list(c("last_name", "first_name", "birth_yr"),
                                         c("last_name", "birth_yr", "birth_mo"),
                                         c("first_name", "birth_mo", "birth_dy")),
                         exclude = c("uid"),
                         strcmp = 2:3,
                         strcmpfun = jarowinkler)

 #SET PROBABILITY WEIGHTING (M Probabilities)

          #[first  last  gender  year  month  day]

  m_probs <- c(.8, .9, .85, .9, .85, .75)
  
  #APPLY PROBABILITY WEIGHTING & JARO-WINKLER CUTOFF

   fs_pairs <- fsWeights(rpairs = pairs, m = m_probs, u = pairs$frequencies, cutoff = .9)

   #EXAMINE WEIGHTS AND DETERMINE MINIMUM WEIGHT

    table(fs_pairs$Wdata)

    extract <- setDT(getPairs(fs_pairs, min.weight = 44, max.weight = 45, single.rows = FALSE))
    
    View(extract)
    
    extract2 <- setDT(getPairs(fs_pairs, min.weight = 30, max.weight = 30.9, single.rows = FALSE))
    
    View(extract2)
    
    extract3 <- setDT(getPairs(fs_pairs, min.weight = 23, max.weight = 23.9, single.rows = FALSE))
    
    View(extract3)

    #SELECT MATCHES BASED ON MINIMUM ACCEPTABLE MATCH QUALITY

     match <- setDT(getPairs(fs_pairs, min.weight = 23, single.rows = TRUE))
    
     View(match)
    
     match_ids <- match[, .(uid.1, uid.2, wt = Weight)]

     colnames(match_ids) <- c(file_1_id, file_2_id, "wt")
    
     View(match_ids)
    
    #SELECT HIGHEST WEIGTHED MATCHES FOR EACH SYSTEM ID

     #Sorted by 1st System ID and Descending Weight. Highest weight retained within ID
    
      match_max_wts <- match_ids[, .SD][order(get(file_1_id), -wt)][!duplicated(get(file_1_id))]

     #Sorted by 2nd System ID and Descending Weight. Highest remaining weight retained within ID

      match_max_wts <- match_max_wts[, .SD][order(get(file_2_id), -wt)][!duplicated(get(file_2_id))]

########################################################################################################
      
#MERGE MATCHED IDS WITH ORIGINAL NON-SPLIT FILES TO GET USER INFO

 #Returns original demographic information from deduplicated files for each system

  ds_merge <- merge(file_1_dedup_nosplit, match_max_wts, by.x = "uid", by.y = eval(file_1_id), all.x = F)
  
  View(ds_merge)    
      
  ds_frx_merge <- merge(ds_merge, file_2_dedup_nosplit, by.x = eval(file_2_id), by.y = "uid", all.x = F)
  
  View(ds_frx_merge)

########################################################################################################
  
#FIX COLUMN NAMES

 names(ds_frx_merge)[names(ds_frx_merge) == "uid"] <- eval(file_1_id)

 names(ds_frx_merge) <- gsub("\\.y", paste0("_", file_2_abbr), gsub("\\.x", paste0("_", file_1_abbr), names(ds_frx_merge)))

 View(ds_frx_merge)

 #Check Duplicates
 
  ds_frx_merge[duplicated(ds_frx_merge[, id_ds]), ]
  ds_frx_merge[duplicated(ds_frx_merge[, id_frx]), ]

  #Write Matched File (Inner Joined)
  
   fwrite(ds_frx_merge, "ds_frx_matched_only.csv")
   
########################################################################################################
   
#COMBINING MATCHED IDS WITH NON-MATCHED IDS FROM BOTH SETS (FULL-JOIN)
   
 #The ds file (file 1) - Dimensioning to Matched File

  file_1_nomatch <- file_1_dedup_nosplit[!uid %in% ds_frx_merge[, get(file_1_id)], .SD]
   
   names(file_1_nomatch)[names(file_1_nomatch) == "uid"] <- eval(file_1_id)
   
    file_1_nomatch <- file_1_nomatch[, eval(file_2_id) := NA]
   
     colnum <- length(colnames(file_1_nomatch))
   
      endcol <- colnum - 1
   
       file_1_nomatch <- file_1_nomatch[, c(colnum, 1:endcol), with=F]
   
        file_1_nomatch <- cbind(file_1_nomatch, data.table(NA, NA, NA, NA, NA, NA, NA))
   
  View(file_1_nomatch)

 #The frx file (file 2) - Dimensioning to Matched File
    
  file_2_nomatch <- file_2_dedup_nosplit[!uid %in% ds_frx_merge[, get(file_2_id)], .SD]
   
   names(file_2_nomatch)[names(file_2_nomatch) == "uid"] <- eval(file_2_id)
   
    file_2_nomatch <- file_2_nomatch[, eval(file_1_id) := NA]
   
     colnum <- length(colnames(file_2_nomatch))
   
      endcol <- colnum - 1
   
       file_2_nomatch <- file_2_nomatch[, c(1, colnum, 2:endcol), with=F]
   
        file_2_nomatch <- cbind(file_2_nomatch[, 1:2], data.table(NA, NA, NA, NA, NA, NA, NA), file_2_nomatch[, 3:colnum])
   
  View(file_2_nomatch)

#Stacking the Files - rbindlist has changed?  
     
 ds_frx_full_match <- rbindlist(list(ds_frx_merge, file_1_nomatch, file_2_nomatch), use.names = FALSE)
   
 View(ds_frx_full_match)

#Write Matched and Unmatched File (Full Joined)
 
 fwrite(ds_frx_full_match, "ds_frx_matched_all.csv")
