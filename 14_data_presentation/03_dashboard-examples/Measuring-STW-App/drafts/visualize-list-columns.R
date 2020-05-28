library(ggplot2)
library(stringr)
library(stringi)
library(tidyr)
library(dplyr)

test <- read.csv("https://raw.githubusercontent.com/uva-bi-sdad/Measuring-STW-App/sarah/data-discovery-feb-3.csv", sep = ",",stringsAsFactors = FALSE, header=TRUE, encoding="UTF-8")

test <- test %>% select(Data.Source.Name, Dataset.Name, "Subject")
test[colnames(test) == "Subject"] <- trimws(test[, "Subject"])


test[["Subject"]] <- as.list(str_split(test[["Subject"]], ", "))

vars <- unique(unlist(test$Data.Type))


test[, "Subject"]<- list(test[, "Subject"])


test2 <-test %>% unnest(Subject) %>% 
group_by(Data.Source.Name, Dataset.Name)
#%>% 
#spread(key=Data.Type, value=Data.Type)

for(i in 1:length(vars)){
  i <- vars[i]

test2[is.na(test2[, i]) == FALSE, i] <- "Yes"
test2[is.na(test2[, i]) == TRUE, i] <- "No"
}





ggplot(test2, aes(x = test2$Subject, fill = test2$Subject))+
  geom_bar()

ggplot(test2, aes(x =test2$Skills..Yes.No., fill = test2$Data.Type))+ 
 # scale_fill_manual(values = c(theme_Palette[1], theme_Palette[5], theme_Palette[4], theme_Palette[2]))+
  geom_bar(width = .66) +
  theme_minimal() +
  #labs(title = paste("Data Sources Containing", input$category3, "Data by", input$category2), y = "Number of Sources", x = paste(input$category2), fill = paste(input$category3) ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18), 
    axis.title.x = element_text(size = 18), 
    axis.title.y = element_text(size = 18))



check <- responses %>% select(`Data Source Name`, `Dataset Name`, input$category1)
check[[input$category1]] <- trimws(check[[input$category1]])
check[[input$category1]] <- as.list(str_split(check[[input$category1]], ", "))
check <- check %>% unnest(input$category1) %>% group_by(`Data Source Name`, `Dataset Name`)
check <- as.data.frame(check)


  ggplot(check, aes(x = check[ , input$category1] , fill =check[, input$category1] ))+
    geom_bar()+
    geom_bar(width = 0.66) +
    theme_minimal() +
    labs(title = paste("Data Sources Containing", input$category1), y = "Number of Sources", x = "") +
    theme(
      legend.position = "none", 
      plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
      axis.text.x = element_text(size = 18, angle = 20),
      axis.text.y = element_text(size = 18), 
      axis.title.x = element_text(size = 18), 
      axis.title.y = element_text(size = 18))
  
  
  
  
  # graph subject and jobs
  
  
check2 <- responses %>% select(`Data Source Name`, `Dataset Name`, `Data Type`, Jobs)
check2[["Data Type"]] <- trimws(check2[["Data Type"]])
check2[["Data Type"]] <- as.list(str_split(check2[["Data Type"]], ", "))
check2 <- check2 %>% unnest("Data Type") %>% group_by(`Data Source Name`, `Dataset Name`)
check2 <- as.data.frame(check2)
  
ggplot(check2, aes(x = check2[ , "Subject"] , fill =check2[, "Jobs"] ))+
  geom_bar()+
  geom_bar(width = 0.66) +
  theme_minimal() +
  #labs(title = paste("Data Sources Containing", input$category1), y = "Number of Sources", x = "") +
  theme(
    #legend.position = "none", 
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    axis.text.x = element_text(size = 18, angle = 20),
    axis.text.y = element_text(size = 18), 
    axis.title.x = element_text(size = 18), 
    axis.title.y = element_text(size = 18))


ggplot(check2, aes(x = check2[ , "Jobs"] , fill =check2[, "Subject"] ))+
  geom_bar()+
  geom_bar(width = 0.66) +
  theme_minimal() +
  #labs(title = paste("Data Sources Containing", input$category1), y = "Number of Sources", x = "") +
  theme(
    #legend.position = "none", 
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    axis.text.x = element_text(size = 18, angle = 20),
    axis.text.y = element_text(size = 18), 
    axis.title.x = element_text(size = 18), 
    axis.title.y = element_text(size = 18))

#Subject and Data Type


expand_responses <- responses
expand_responses[["Subject"]] <- trimws(expand_responses[["Subject"]])
expand_responses[["Subject"]] <- as.list(str_split(expand_responses[["Subject"]], ", "))
expand_responses <- expand_responses %>% unnest("Subject") %>% group_by(`Data Source Name`, `Dataset Name`)
expand_responses <- as.data.frame(expand_responses)

expand_responses[["Data Type"]] <- trimws(expand_responses[["Data Type"]])
expand_responses[["Data Type"]] <- as.list(str_split(expand_responses[["Data Type"]], ", "))
expand_responses <- expand_responses %>% unnest("Data Type") %>% group_by(`Data Source Name`, `Dataset Name`)
expand_responses <- as.data.frame(expand_responses)




check3 <- responses %>% select(`Data Source Name`, `Dataset Name`, Subject, `Data Type`)
check3[["Subject"]] <- trimws(check3[["Subject"]])
check3[["Subject"]] <- as.list(str_split(check3[["Subject"]], ", "))
check3 <- check3 %>% unnest("Subject") %>% group_by(`Data Source Name`, `Dataset Name`)
check3 <- as.data.frame(check3)

check3[["Data Type"]] <- trimws(check3[["Data Type"]])
check3[["Data Type"]] <- as.list(str_split(check3[["Data Type"]], ", "))
check3 <- check3 %>% unnest("Data Type") %>% group_by(`Data Source Name`, `Dataset Name`)
check3 <- as.data.frame(check3)



ggplot(check3, aes(x = check3[ , "Subject"] , fill =check3[, "Data Type"] ))+
  geom_bar()+
  geom_bar(width = 0.66) +
  theme_minimal() +
  #labs(title = paste("Data Sources Containing", input$category1), y = "Number of Sources", x = "") +
  theme(
    #legend.position = "none", 
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    axis.text.x = element_text(size = 18, angle = 20),
    axis.text.y = element_text(size = 18), 
    axis.title.x = element_text(size = 18), 
    axis.title.y = element_text(size = 18))

ggplot(check3, aes(x = check3[ , "Data Type"] , fill =check3[, "Subject"] ))+
  geom_bar()+
  geom_bar(width = 0.66) +
  theme_minimal() +
  #labs(title = paste("Data Sources Containing", input$category1), y = "Number of Sources", x = "") +
  theme(
    #legend.position = "none", 
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    axis.text.x = element_text(size = 18, angle = 20),
    axis.text.y = element_text(size = 18), 
    axis.title.x = element_text(size = 18), 
    axis.title.y = element_text(size = 18))


ggplot(expand_responses[!duplicated(expand_responses[ ,c('Dataset Name', "Subject")]),], 
       aes(x = expand_responses[!duplicated(expand_responses[ ,c('Dataset Name', "Subject")]), "Subject"], 
           fill = expand_responses[!duplicated(expand_responses[ ,c('Dataset Name', "Subject")]),"Jobs"]))+
  geom_bar()+
  geom_bar(width = 0.66) +
  theme_minimal() +
  #labs(title = paste("Data Sources Containing", input$category1), y = "Number of Sources", x = "") +
  theme(
    #legend.position = "none", 
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    axis.text.x = element_text(size = 18, angle = 20),
    axis.text.y = element_text(size = 18), 
    axis.title.x = element_text(size = 18), 
    axis.title.y = element_text(size = 18))


############ three var visualization


##### job, credentials, Skills, employers

#{2,2,2} 
#if length(unique(test$[[inputcategory4]]))==2 
#length(unique(test$[[inputcategory5]]))==2) & length(unique(test$[[inputcategory6]]))==2)

#{2,2,4} {2,3,4} {3,2,4}{3,3,4}{4,2,2}{2,4,2}{4,2,3}  {4,3,2} {4,3,3} {3,4,2} {3,4,3} {2,4,3} 
if (input$category4== "Data Type" & input$category5 != "Subject" & input$category6 != "Subject")|
  (input$category5== "Data Type" & input$category4 != "Subject" & input$category6 != "Subject") | 
  (input$category6== "Data Type" & input$category5 != "Subject" & input$category4 != "Subject") |
  (input$category4== "Subject" & input$category5 != "Data Type" & input$category4 != "Data Type") |
  (input$category5== "Subject"& input$category5 != "Data Type" & input$category4 != "Data Type") | 
  (input$category6== "Subject" & input$category5 != "Data Type" & input$category4 != "Data Type") )








ggplot(expand_responses[!duplicated(expand_responses[,c('Dataset Name', "Subject")]),], aes(x = expand_responses[!duplicated(expand_responses[,c('Dataset Name', "Subject")]), "Subject"], fill = expand_responses[!duplicated(expand_responses[,c('Dataset Name', "Subject")]), "Data Type"]))+
  geom_bar()+
  facet_grid(~expand_responses[!duplicated(expand_responses[,c('Dataset Name', "Subject")]), "Jobs"])




(input$category4== "Data Type" & input$category5 != "Subject")|
  (input$category4== "Data Type" & input$category5 != "Data Type")|
    (input$category4== "Subject" & input$category5 != "Data Type")|
      (input$category4== "Subject" & input$category5 != "Subject")|
        (input$category5== "Data Type" & input$category6 != "Subject")|
          (input$category5== "Data Type" & input$category6 != "Data Type")|
            (input$category5== "Subject" & input$category6 != "Data Type")|
              (input$category5== "Subject" & input$category6 != "Subject")|
                (input$category6== "Data Type" & input$category4 != "Subject")|
                  (input$category6== "Data Type" & input$category4 != "Data Type")|
                    (input$category6== "Subject" & input$category4 != "Data Type")|
                      (input$category6== "Subject" & input$category4 != "Subject")
  
  
#   {4,4,2} 
#{4,4,3} 

#{2,4,4} 
#{3,4,4} 
#{4,2,4} {4,3,4} 
#{4,4,4}







#{2,2,3}  {2,3,2}  {3,2,2} 

#{2,3,3}   
# {3,2,3}  {3,3,2} {3,3,3} 


#mosaic plot


#MOSAIC PLOT

#subject, data type 
#facets


### cats 
#




input4 <- "Subject"
input5 <- "Data Type"
input6 <- "Credentials"


ggplot(expand_responses[!duplicated(expand_responses[,c('Dataset Name', 'Data Source Name', input$category4, input$category5, input$category6)]), ], 
       aes(x = expand_responses[!duplicated(expand_responses[,c('Dataset Name', 'Data Source Name', input$category4, input$category5, input$category6)]), 
                                if((input$category4== "Data Type" & input$category5 != "Subject" & input$category6 != "Subject")|
                                   (input$category4== "Subject" & input$category5 != "Data Type" & input$category6 != "Data Type")){
                                  input$category5
                                
                                  } else if((input$category5== "Data Type" & input$category4 != "Subject" & input$category6 != "Subject") |
                                         (input$category5== "Subject"& input$category4 != "Data Type" & input$category6 != "Data Type")){
                                  input$category6
                                  } else if ((input$category6== "Data Type" & input$category5 != "Subject" & input$category4 != "Subject") |
                                  (input$category6== "Subject" & input$category5 != "Data Type" & input$category4 != "Data Type")){
                                  input$category4
                                  } else if((input$category4== "Data Type" & input$category5 == "Subject")|
                                            (input$category4== "Data Type" & input$category5 == "Data Type")|
                                            (input$category4== "Subject" & input$category5 == "Data Type")|
                                            (input$category4== "Subject" & input$category5 == "Subject")){
                                  input$category6
                                } else if((input$category5== "Data Type" & input$category6 == "Subject")|
                                          (input$category5== "Data Type" & input$category6 == "Data Type")|
                                          (input$category5== "Subject" & input$category6 == "Data Type")|
                                          (input$category5== "Subject" & input$category6 == "Subject")){
                                  input$category4
                                } else if( (input$category6== "Data Type" & input$category4 == "Subject")|
                                           (input$category6== "Data Type" & input$category4 == "Data Type")|
                                           (input$category6== "Subject" & input$category4 == "Data Type")|
                                           (input$category6== "Subject" & input$category4 == "Subject")){
                                  input$category5
                                } else{
                                  input$category4
                                }
                                ], 
           
           
           fill =expand_responses[!duplicated(expand_responses[,c('Dataset Name', 'Data Source Name', input$category4, input$category5, input$category6)]), 
                                  if((input$category4== "Data Type" & input$category5 != "Subject" & input$category6 != "Subject")|
                                     (input$category4== "Subject" & input$category5 != "Data Type" & input$category6 != "Data Type")){
                                    input$category6
                                  }else if((input$category5== "Data Type" & input$category4 != "Subject" & input$category6 != "Subject") |
                                           (input$category5== "Subject"& input$category4 != "Data Type" & input$category6 != "Data Type")){
                                    input$category4
                                  }else if ((input$category6== "Data Type" & input$category5 != "Subject" & input$category4 != "Subject") |
                                            (input$category6== "Subject" & input$category5 != "Data Type" & input$category4 != "Data Type")){
                                    input$category5
                                  } else if((input$category4== "Data Type" & input$category5 == "Subject")|
                                            (input$category4== "Data Type" & input$category5 == "Data Type")|
                                            (input$category4== "Subject" & input$category5 == "Data Type")|
                                            (input$category4== "Subject" & input$category5 == "Subject")){
                                    input$category4
                                  }else if((input$category5== "Data Type" & input$category6 == "Subject")|
                                           (input$category5== "Data Type" & input$category6 == "Data Type")|
                                           (input$category5== "Subject" & input$category6 == "Data Type")|
                                           (input$category5== "Subject" & input$category6 == "Subject")){
                                  input$category5
                                  }else if( (input$category6== "Data Type" & input$category4 == "Subject")|
                                            (input$category6== "Data Type" & input$category4 == "Data Type")|
                                            (input$category6== "Subject" & input$category4 == "Data Type")|
                                            (input$category6== "Subject" & input$category4 == "Subject")){
                                    input$category6
                                  } else{
                                    input$category5
                                  }
                                  
                                  
                                  ]))+
  
  
  facet_grid(~expand_responses[!duplicated(expand_responses[,c('Dataset Name', 'Data Source Name', input$category4, input$category5, input$category6)]), 
                               
                               if((input$category4== "Data Type" & input$category5 != "Subject" & input$category6 != "Subject")|
                                  (input$category4== "Subject" & input$category5 != "Data Type" & input$category6 != "Data Type")){
                                 input$category4
                               }else if((input$category5== "Data Type" & input$category4 != "Subject" & input$category6 != "Subject") |
                                        (input$category5== "Subject"& input$category4 != "Data Type" & input$category6 != "Data Type")){
                                 input$category5
                               }else if ((input$category6== "Data Type" & input$category5 != "Subject" & input$category4 != "Subject") |
                                         (input$category6== "Subject" & input$category5 != "Data Type" & input$category4 != "Data Type")){
                                input$category6 
                               } else if((input$category4== "Data Type" & input$category5 == "Subject")|
                                         (input$category4== "Data Type" & input$category5 == "Data Type")|
                                         (input$category4== "Subject" & input$category5 == "Data Type")|
                                         (input$category4== "Subject" & input$category5 == "Subject")){
                                 input$category5
                               }else if((input$category5== "Data Type" & input$category6 == "Subject")|
                                        (input$category5== "Data Type" & input$category6 == "Data Type")|
                                        (input$category5== "Subject" & input$category6 == "Data Type")|
                                        (input$category5== "Subject" & input$category6 == "Subject")){
                                 input$category6
                               }else if( (input$category6== "Data Type" & input$category4 == "Subject")|
                                         (input$category6== "Data Type" & input$category4 == "Data Type")|
                                         (input$category6== "Subject" & input$category4 == "Data Type")|
                                         (input$category6== "Subject" & input$category4 == "Subject")){
                                 input$category4
                               } else{
                                 input$category6
                               }
                               
                               
                               
                               ])+
  geom_bar(width = 0.66) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 24, face = "bold"),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18), 
    axis.title.x = element_text(size = 18), 
    axis.title.y = element_text(size = 18))



