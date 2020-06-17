library(data.table)

setwd("C:/Users/abrahamt/Desktop/Data Restructuring (DSPG)")

#AGGREGATING FILES VERTICALLY

 sy_2015 <- fread("schools_2015_DSPG_2020.csv", check.names = TRUE)
 sy_2016 <- fread("schools_2016_DSPG_2020.csv", check.names = TRUE)
 sy_2017 <- fread("schools_2017_DSPG_2020.csv", check.names = TRUE)
 
 #Examine Column Headers
 
  colnames(sy_2015)
  colnames(sy_2016)
  colnames(sy_2017)
  
  #Differences Across Files
  
   setdiff(colnames(sy_2015), colnames(sy_2016))
   setdiff(colnames(sy_2016), colnames(sy_2015))
   
   #Column Set Difference Across Files (unions and intersections)
   
    setdiff(union(colnames(sy_2015), colnames(sy_2016)),
                  intersect(colnames(sy_2015), colnames(sy_2016)))
    
    setdiff(union(colnames(sy_2015), colnames(sy_2017)),
                  intersect(colnames(sy_2015), colnames(sy_2017)))
    
    #Renaming Columns to Standardize
    
     colnames(sy_2016)[7] <- colnames(sy_2015)[7]
     
     #Binding Rows - Multiple Options but Be Cautious
     
      schools_data <- rbindlist(list(sy_2015, sy_2016, sy_2017), use.names = TRUE)
      
      head(setorder(schools_data, "school_id", "student_id"), n = 30)
  
##########################################################################################      
      
#JOINING/MERGING FILES HORIZONTALLY
  
 #Left Joining - Keeps all 2015 records but only matches in 2016 (2015 Cohort)      
         
  sy_1516 <- merge(x = sy_2015, y = sy_2016, by = 'student_id', all.x = TRUE)
  
  sy_1516[is.na(sy_1516$school_id.y), ]
  
  #Clean Up Column Names
   
   names(sy_1516) <- gsub("\\.x", paste0("_", "2015"), gsub("\\.y", paste0("_", "2016"), names(sy_1516)))
  
   head(sy_1516, n = 10)  
  
 #Right Joining - Keeps all 2016 records regardless of 2015 matches (New 2016 Kids)
   
  sy_1615 <- merge(x = sy_2015, y = sy_2016, by = 'student_id', all.y = TRUE)
   
  sy_1615[is.na(sy_1615$school_id.x), ]
   
  #Clean Up Column Names
   
   names(sy_1615) <- gsub("\\.x", paste0("_", "2015"), gsub("\\.y", paste0("_", "2016"), names(sy_1615)))
   
   head(sy_1615, n = 10)  
     
 #Inner Joining - Keeps only 2015 records with 2016 matches (2015-2016 Cohort)
   
   sy_1516_pairs <- merge(x = sy_2015, y = sy_2016, by = 'student_id')
   
   sy_1516_pairs[(is.na(sy_1516_pairs$school_id.x) | is.na(sy_1516_pairs$school_id.y)), ]
   
   #Clean Up Column Names
   
    names(sy_1516_pairs) <- gsub("\\.x", paste0("_", "2015"), gsub("\\.y", paste0("_", "2016"), names(sy_1516_pairs)))
   
    head(sy_1516_pairs, n = 10)     
   
 #Full Joining - Keeps all 2015 records and all 2016 records (2015-2016 Students)
    
   sy_1516_all <- merge(x = sy_2015, y = sy_2016, by = 'student_id', all.x = TRUE, all.y = TRUE)
    
   sy_1516_all[(is.na(sy_1516_all$school_id.x) | is.na(sy_1516_all$school_id.y)), ]
    
   #Clean Up Column Names
    
    names(sy_1516_all) <- gsub("\\.x", paste0("_", "2015"), gsub("\\.y", paste0("_", "2016"), names(sy_1516_all)))
    
    sy_1516_all[sample(nrow(sy_1516_all), 20, replace = FALSE), ]
    
 #Merging the full Matched Cohort - 2015, 2016, 2017
  
  #Already have the 2015-2016 Inner Join - Inner Join to 2017
    
   sy_151617_sample <- merge(x = sy_1516_pairs, y = sy_2017, by = 'student_id')
   
   #Clean Up Column Names
   
    #Already set column names _2015 and _2016 in the sy_1516_pairs table
   
     names(sy_151617_sample)[20:28] <- paste0(names(sy_151617_sample)[20:28], "_", "2017") 
    
     sy_151617_sample[sample(nrow(sy_151617_sample), 10, replace = FALSE), ]

##########################################################################################      
    
#STRETCHING FILES (LONG-TO-WIDE): Suppose the schools_data file was the original source
    
 #Putting things in order 
    
  schools_long <- setorder(schools_data, "student_id", "sch_yr")
    
  #Create an ID Counter (Optional)
  
   schools_long[, freqID := .N, by = student_id]
   
   table(schools_long$freqID)
  
  #Create an Observation Counter (Optional)
  
   schools_long$obs <- ave(schools_long$student_id, schools_long$student_id, FUN = seq_along)
    
   table(schools_long$obs)
   
   table(schools_long$freqID, schools_long$obs)
    
   schools_wide <- reshape(schools_long,
                           timevar = "sch_yr",
                           idvar = c("student_id", "freqID"),
                           direction = "wide")
    
   schools_wide[sample(nrow(schools_wide), 10, replace = FALSE), ]
   
##########################################################################################      
   
#STACKING FILES (WIDE-TO-LONG): Suppose the sy151617 Sample was the Original Source File   
  
 long_151617 <- reshape(sy_151617_sample,
                        idvar = "student_id",
                        varying = list(c(2, 11, 20), c(3, 12, 21), c(4, 13, 22),
                                       c(5, 14, 27), c(6, 15, 28), c(7, 16, 23),
                                       c(8, 17, 24), c(9, 18, 26), c(10, 19, 25)),
                         v.names = c("school_id", "year", "grade",
                                     "enrolled", "attended", "frl",
                                     "ell", "reading", "math"),
                         direction = "long")
   
  head(setorder(long_151617, "student_id", "year"), n = 30)
   
  table(long_151617$time)

##########################################################################################        

#TRANSFORMATIONS
     
 #MISSING DATA: Suppose Analysis Requires Test Scores
  
  long_151617[(is.na(reading) | is.na(math)), ]
  
  #Drop Missing Cases
  
   long_151617_sub <- long_151617[!(is.na(reading) | is.na(math)), ]
     
 #RECODING VALUES: Convert ELL Y/N TO 0 = No; 1 = Yes
   
  #IMPORTANT - NO MISSING VALUES 
   
   long_151617_sub$ell <- as.integer(ifelse(long_151617_sub$ell == "Y", "1", "0")) 
     
 #COUNTS: Cases with frl AND ell status
   
  long_151617_sub$atrisk=rowSums(cbind(long_151617_sub$frl,long_151617_sub$ell))
  
  #This works too
   
   #long_151617_sub$atrisk2= long_151617_sub$frl + long_151617_sub$ell
  
 #Statistical Summaries: Average Math and Reading Scores across all Three Years
  
  #Recall Data are in Long form - Single Column of Scores
  
   #(math_2015 + math_2016 + math_2017) / 3 Will NOT Work
  
     long_151617_sub$math_avg <- ave(long_151617_sub$math, long_151617_sub$student_id, FUN = mean)
     long_151617_sub$reading_avg <- ave(long_151617_sub$reading, long_151617_sub$student_id, FUN = mean)
  
 #Mathematical Transformations: % Days Attended each Year
     
  #Number of Days Attended can be Misleading - Depends on Enrollment   
     
   long_151617_sub$attend_pct <- (long_151617_sub$attended / long_151617_sub$enrolled) * 100
   
##########################################################################################        
   
#SUBSETS
   
 #MATH < 182 AND READING < 190 - Proficiency Cutoffs at 2nd Grade
   
   long_151617_2nd_np <- long_151617_sub[(grade == "2nd" & reading < 190 & math < 182), ]
   
   #MATH < 182 AND READING < 190 - Proficiency Cutoffs at 2nd Grade AND At Risk = 1 or 2
   
   long_151617_2nd_np_atrisk <- subset(long_151617_sub, (grade == "2nd" & reading < 190 & math < 182 & atrisk > 0))
   
     
     
     