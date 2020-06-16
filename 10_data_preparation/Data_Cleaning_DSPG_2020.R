#LOAD LIBRARIES AND FUNCTIONS ----

library(data.table)
library(lubridate)
library(stringr)

########################################################################################################

setwd("C:/Users/abrahamt/Desktop")

 demo <- fread("Data Cleaning (DSPG)/Data_Cleaning_DSPG_2020.csv", colClasses = "character", check.names = TRUE)

 #1. LOOK AT THE DATA!

     str(demo)
 
     head(demo, n = 20)
    
     tail(demo, n = 20)

     demo[sample(nrow(demo), 20, replace = FALSE), ]

 #2. LOOK AGAIN (Closely)
    
     ### A function to speed up missing value identification (Thank Aaron!)
    
         is_blank <- function(x) {
                     return(is.null(x) |
                            length(x) == 0 |
                            is.na(x) |
                            x == "")
                                    }

#############################################################################
        
#ID NUMBERS
    
 #Examine records with missing/invalid IDs ----     
    
  demo[is_blank(c_id), ]
    
  head(demo[order(c_id)], n = 20)
     
  tail(demo[order(c_id)], n = 20)

 #Duplicated IDs
    
  demo[duplicated(c_id), ]
     
  demo[duplicated(c_id) | duplicated(c_id, fromLast=TRUE), ] 
    
 #Create New IDs?
     
   set.seed(20200601)
   demo$uid <- paste("cid", sample(100000:999999, nrow(demo), replace = FALSE), sep = "_")
     
   demo[duplicated(uid), ]
     
   #Dropping the Duplicated Case
     
    demo[duplicated(c_id) | duplicated(c_id, fromLast=TRUE), ]
     
    demo <- demo[uid != "cid_515695", ]

#############################################################################    
     
 #DATES
    
  #Examine records with missing birth dates ----  
      
   demo[is_blank(birth_date), ] 
     
  #Format Dates - If Using as Dates
     
   demo$dob <- mdy(demo$birth_date)
     
   demo[is.na(dob), ]   #demo[is_blank(as.character(dob)), ]
     
  #Parse birthdate into separate columns - If Using as Component Pieces
     
   ### A function to speed up parsing dates
     
       parse_dob_to_cols <- function(df, date_field, date_format = "ymd") {
           dt <- data.table::setDT(df)
            if(date_format == "ymd") {
               dt[, birth_yr := lubridate::year(lubridate::ymd(dt[, get(date_field)]))]
               dt[, birth_mo := lubridate::month(lubridate::ymd(dt[, get(date_field)]))]
               dt[, birth_dy := lubridate::day(lubridate::ymd(dt[, get(date_field)]))]
            }
            else if (date_format == "mdy") {
              dt[, birth_yr := lubridate::year(lubridate::mdy(dt[, get(date_field)]))]
              dt[, birth_mo := lubridate::month(lubridate::mdy(dt[, get(date_field)]))]
              dt[, birth_dy := lubridate::day(lubridate::mdy(dt[, get(date_field)]))]
            }
              data.table::setDF(dt)
            }
     
   demo <- setDT(parse_dob_to_cols(demo, "birth_date", date_format = "mdy"))
     
   colnames(demo)
     
   demo[is_blank(demo$birth_yr), ]
     
  #Examine birth date elements
     
   #Using Quick Tables
     
    table(demo$birth_yr)
    table(demo$birth_mo)
    table(demo$birth_dy)
     
   #Using Histograms
     
    hist(demo$birth_dy)
     
   #Using Numeric Values
     
    summary(demo$birth_dy)
    
   #Using Multiway Tables - Sometimes Helpful
     
    ftable(table(demo$birth_yr, demo$birth_mo, demo$birth_dy))
     
   #Possible Placeholders - Which MM/DD Should Occur Most/Least Often 
     
     as.data.table(table(do.call(paste, list(demo$birth_mo, demo$birth_dy))))[order(N, decreasing = TRUE)]
      
    #Repairs (Maybe)
      
     demo[, 'birth_yr'][demo[, 'birth_date'] == "2/31/2013", ] <- 2013
     demo[, 'birth_mo'][demo[, 'birth_date'] == "2/31/2013", ] <- 2
      
     demo[, 'birth_yr'][demo[, 'birth_date'] == "5/76/2013", ] <- 2013
     demo[, 'birth_mo'][demo[, 'birth_date'] == "5/76/2013", ] <- 5
     
#############################################################################    
      
 #CATEGORIES
      
  #Examine records with missing data ----  
      
   demo[is_blank(gender), ]
      
  #Standardize if necessary ---- LOOK FIRST
      
   table(demo$gender)  
      
   ### A function to standardize gender
      
       standardize_gender <- function(dt, gender_col_nm = "gender") {
          dt[toupper(get(gender_col_nm)) == "FEMALE", eval(gender_col_nm) := "F"]
          dt[toupper(get(gender_col_nm)) == "1", eval(gender_col_nm) := "F"]
          dt[toupper(get(gender_col_nm)) == "MALE", eval(gender_col_nm) := "M"]
          dt[toupper(get(gender_col_nm)) == "0", eval(gender_col_nm) := "M"]
          dt[get(gender_col_nm) != "" &
              !is.na(get(gender_col_nm)) &
               toupper(get(gender_col_nm)) != "F" &
              toupper(get(gender_col_nm)) != "M", eval(gender_col_nm) := "O"]
          dt
         }
      
   demo <- standardize_gender(demo, gender_col_nm = "gender")
      
   table(demo$gender)
      
#############################################################################    
      
 #CHARACTER STRINGS
      
  #Standardize Text Strings  
      
   demo$first_name <- toupper(demo$first_name)
   demo$last_name <- toupper(demo$last_name)
      
  #Examine records with missing names ----     
     
   demo[is_blank(first_name), ]
   demo[is_blank(last_name), ]
     
   ### A Function for Fixing Names
     
      get_between_separators <- function(string = "", first_separator = ",", second_separator = ",", include_sep_in_output = FALSE) {
        special_chars <- c(".", "?", "#")
        if (first_separator %in% special_chars) first_sep <- paste0("\\", first_separator) else first_sep <- first_separator
        if (second_separator %in% special_chars) second_sep <- paste0("\\", second_separator) else second_sep <- second_separator
            idx_fst_sep <- regexpr(first_sep, string)
            idx_scd_sep <- regexpr(second_sep, substr(string, idx_fst_sep + 1, nchar(string)))
          for (i in 1:length(idx_scd_sep)) {
               if (idx_fst_sep[i] == -1) idx_scd_sep[i] <- -1
           }
             for (i in 1:length(idx_scd_sep)) {
                  if (idx_scd_sep[i] == -1) idx_fst_sep[i] <- -1
            }
              idx_fst_plus_scd <- idx_fst_sep + idx_scd_sep
              out <- substr(string, idx_fst_sep, idx_fst_plus_scd)
               if (include_sep_in_output == FALSE) out <- substr(out, 2, nchar(out) - 1)
                   out <- trimws(out)
                   out
         }

   #c_376107: " " EDWARD DE LOS SANTOS
         
   demo <- demo[(str_count(last_name, " ") > 0 & is_blank(first_name)),
                    c("first_name", "last_name") := list(
                        get_between_separators(last_name, first_separator =  
                                                 "^", second_separator = " ",            
                                               include_sep_in_output = TRUE),
                        get_between_separators(last_name, first_separator =  
                                                 " ", second_separator = "$",            
                                               include_sep_in_output = TRUE))]    
         
  #DID IT WORK?
     
   demo[(c_id == "c_376107" | c_id == "c_831694" | c_id == "c_294959"), ] 
         
   #c_294959: PATSY NOELLA " "
   #c_831694: KRISIN SOLEDAD " " 
         
   demo <- demo[(str_count(first_name, " ") > 0 & is_blank(last_name)),
                    c("first_name", "last_name") := list(
                      get_between_separators(first_name, first_separator =  
                                               "^", second_separator = " ",            
                                             include_sep_in_output = TRUE),
                      get_between_separators(first_name, first_separator =  
                                               " ", second_separator = "$",            
                                             include_sep_in_output = TRUE))]
  #DID IT WORK?
     
   demo[(c_id == "c_246857" | c_id == "c_831694" | c_id == "c_376107" | c_id == "c_294959"), ]
     
  #Short Names
     
   demo[nchar(demo$first_name) < 3]
   demo[nchar(demo$last_name) < 3]
     
  #Extra Spacing in Names - Do This Before Removing Characters
     
   table(str_count(demo$first_name, "  "))
   table(str_count(demo$last_name, "  "))
     
   demo$first_name <- gsub(' +', ' ', demo$first_name)
   demo$last_name <- gsub(' +', ' ', demo$last_name)
     
   demo$first_name <- trimws(demo$first_name)
   demo$last_name <- trimws(demo$last_name)
     
   demo[str_count(first_name, " ") > 1, ]
   demo[str_count(last_name, " ") > 1, ]
     
  #Numbers in Names
     
   demo[(grepl("[[:digit:]]", demo$first_name) == TRUE), ]
   demo[(grepl("[[:digit:]]", demo$last_name) == TRUE), ]
     
   demo$last_name <- gsub('2$', ' II', demo$last_name)
   demo$last_name <- gsub('3RD', 'III', demo$last_name)
     
   ### A Function for Removing Numbers in Names
     
        remove_numeric <- function(string) {
          out <- gsub("[[:digit:]]", "", string)
          out
         }
     
   demo$last_name <- remove_numeric(demo$last_name)
     
   ### A Function for Removing Pesky Suffixes
     
       move_suffix <- function(df, name_field, suffix_list, return_type = "dt") {
            dt <- data.table::setDT(df)
            lapply(suffix_list, function(sfx) {
              field <- dt[, get(name_field)]
              dt[
                grepl(paste0("( |,|\"|-|\\(| )", sfx), field) == TRUE, suffix := sfx
                ][
                  , eval(name_field):=sub(paste0("( |,|\"|-|\\(| )", sfx), "", field)
                  ]
            })
            ifelse(return_type == "df", d <- data.table::setDF(dt), d <- dt)
            d
          } 
     
   demo <- move_suffix(demo, "last_name", list("III", "II"))
         
   head(demo[!is_blank(suffix)])
         
  #Punctuation in Names
     
   demo[(grepl("[[:punct:]]", demo$first_name) == TRUE), c(1:3)]
   demo[(grepl("[[:punct:]]", demo$last_name) == TRUE), c(1:3)]
         
  #Separate hyphenated names with spaces - There's a reason
      
   ###A Function for Replacing Hyphens
       
      hyphen_split <- function(string) {
        out <- gsub("-", " ", string)
        out
           }
     
   demo$first_name <- hyphen_split(demo$first_name) 
   demo$last_name <- hyphen_split(demo$last_name) 
    
  #Replacing "ST. Name" with "STName" - Also a Reason
     
   demo$last_name <- gsub("\\bST\\. ", "ST", demo$last_name)
     
  #Other Things in Names 
     
   demo[(grepl(".*[^\x20, \x27, \x2C-\x2E, \x41-\x5A].*", demo$first_name) == TRUE), c(1:3)]
   demo[(grepl(".*[^\x20, \x27, \x2C-\x2E, \x41-\x5A].*", demo$last_name) == TRUE), c(1:3)]
      
  #Replacing Specific Text Elements
      
   demo$first_name <- gsub("@", "A", demo$first_name)
     
   ### A Function to Clean Up the Rest
      
       remove_non_alphanum <- function(string, keep_spaces = TRUE) {
        if (keep_spaces == TRUE) out <- gsub("[^[:alnum:] ]", " ", string)
        if (keep_spaces == FALSE) out <- gsub("[^[:alnum:]]", "", string)
        out
      }
         
   demo$first_name <- remove_non_alphanum(demo$first_name)
   demo$last_name <- remove_non_alphanum(demo$last_name)

 #DUPLICATED RECORDS: PART II 
     
  #Some Quick Reordering of Columns - New IDs, Suffixes, Etc.
     
   demo <- demo[, c(1, 6, 2:3, 11, 4:5, 7:10)]
     
   demo[duplicated(demo[, 3:11]) | duplicated(demo[, 3:11], fromLast=TRUE), ]
     
  #Remove Exact Duplicated Rows
     
   demo <- demo[!duplicated(demo[, 3:11]), ]
      
   demo[first_name == "LATOYIA AAYANAH", ]

#############################################################################    
      
 #WRITE CLEANED DATA FILE

  fwrite(demo, "Data Cleaning (DSPG)/Cleaned_Demo_Data_DSPG_2020.csv")