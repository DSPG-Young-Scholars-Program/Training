
library(vcd)
##### DATA #####
responses<-read.csv("data-discovery-feb-3.csv", sep = ",",stringsAsFactors = FALSE, header=TRUE, encoding="UTF-8")

# Assign columns readable names
names(responses) <- stri_trim(gsub("..Yes.No.|i\\.e\\..+or\\.|i\\.e\\..+|\\.{53}.+|\\.+", " ", names(responses)), side = "right")

colnames(responses) <- gsub(" ", "", colnames(responses))

# yes no options

mosaic(~ Skills + Jobs + Credentials, data = responses, main = paste("Data Containing ", "Skills", ", ", "Jobs", ", and ", "Credentials", sep = ""), 
       shade = T,  gp = gpar(fill = c("#9fd1ca", "#6b9caa", "#6b9caa", "#426888", "#6b9caa", "#426888", "#426888", "#1b3766")))

mosaic(~ Skills + Jobs + Credentials, data = responses, main = paste("Data Containing ", "Skills", ", ", "Jobs", ", and ", "Credentials", sep = ""), 
       shade = T,  gp = gpar(fill = c("#72dbc7", "#58a0a6", "#58a0a6", "#3c6a86", "#58a0a6", "#3c6a86", "#3c6a86", "#1b3766")))



data("Titanic")
mosaic(Titanic)

mosaic(Titanic, labeling = labeling_left)
labeling_left

mosaic(Titanic, labeling = labeling_cboxed)
labeling_cboxed

mosaic(Titanic, labeling = labeling_lboxed)
labeling_lboxed



## change variable names
mosaic(Titanic, labeling_args = list(set_varnames = c(Sex = "Gender")))

## change labels
mosaic(Titanic, labeling_args = list(set_varnames = c(Survived = "Status"),
                                     set_labels = list(Survived = c("Survived", "Not Survived")), rep = FALSE))

 mosaic(xtabs(~ Skills + Credentials + Jobs, data = responses  ), data = responses, margin = c(3, 10, 2, 10),
       shade = T,  gp = gpar(fill = c("#72dbc7", "#58a0a6", "#58a0a6", "#3c6a86", "#58a0a6", "#3c6a86", "#3c6a86", "#1b3766")), 
       labeling_args = list(rot_labels = c(0, 0, 90), rot_varnames = c(0, 0, 90) , 
                            offset_varnames = c(0,2,0, 2),  grid_legend(x = unit(1.05,'npc'),
                                                                        y = unit(.45,'npc'),
                                                                        just = c(0,0),
                                                                        frame = FALSE,
                                                                        pch = c(15,15,15, 15),
                                                                        col = c("#72dbc7", "#58a0a6", "#3c6a86", "#1b3766"),
                                                                        labels = c("b",'r','g', 'l'))
                        ))

 mosaic(xtabs(~ Skills + Credentials + Jobs, data = responses  ), data = responses, margin = c(3, 10, 2, 10),
        shade = T,  gp = gpar(fill = c("#72dbc7", "#58a0a6", "#58a0a6", "#3c6a86", "#58a0a6", "#3c6a86", "#3c6a86", "#1b3766")), 
        labeling_args = list(rot_labels = c(0, 0, 90), rot_varnames = c(0, 0, 90) , 
                             offset_varnames = c(0,2,0, 2),  legend = ( col = c(1, 2, 3), legend = c("red", "blue", "green")
        )))
 


