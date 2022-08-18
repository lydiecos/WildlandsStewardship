library(rgeos)
library(rgdal)
library(sf)
library(dplyr)

# Check working directory
getwd()

gdb <- path.expand("C:/Users/aradecki/OneDrive - Wildlands Engineering Inc/Documents/Stewardship/Stewardship_3v")

ogrListLayers("C:/Users/aradecki/OneDrive - Wildlands Engineering Inc/Documents/Stewardship/Stewardship_3v.gdb")

Point <- st_read("Stewardship_3v.gdb", layer = 'Issue_Point')
Line <- st_read("Stewardship_3v.gdb", layer = 'Issue_Line')
Polygon <- st_read("Stewardship_3v.gdb", layer = 'Issue_Area')

RevisedLine <- st_read("Stewardship_3v.gdb", layer = 'Revised_Line')
RevisedArea <- st_read("Stewardship_3v.gdb", layer = 'Revised_Area')

MngActionPoint <- st_read("Stewardship_3v.gdb", layer = 'MngActionPoint')
MngActionLine <- st_read("Stewardship_3v.gdb", layer = 'MngActionLine')
MngActionArea <- st_read("Stewardship_3v.gdb", layer = 'MngActionArea')

site_names_pt = data.frame(sort(unique(Point$Site)))
site_names_ln = data.frame(sort(unique(Line$Site)))
site_names_poly = data.frame(sort(unique(Polygon$Site)))

cc=data.frame() 
dd=data.frame()                
for (gi in sort(MngActionArea$REL_GLOBALID)){
  aa=subset(Polygon, GlobalID==gi)
  bb=subset(MngActionArea, REL_GLOBALID==gi)
  cc=rbind(cc,aa)
  dd=rbind(dd,bb)
}

dd=dd[c(1:8,10:18)]
colnames(dd)=c("Treat_Desc","Contractor","InspDate","Treat_Method_1","Chemical_1","Treat_Method_2","Chemical_2","Adjuvant","GlobalID",
               "Subtype", "CreationDate","Creator","EditDate","Editor","Percent_CntrlEffective_","TreatDate","ISSUE_ID")      

cc=distinct(cc)
dd=distinct(dd)
ee=merge.data.frame(cc,dd,by="GlobalID")
ee=distinct(ee)
