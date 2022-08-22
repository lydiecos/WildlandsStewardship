# Script for exporting Stewardship GDB from AGOL
# Wildlands Engineering
# by Lydie Costes and Andrew Radecki

# Instructions for connecting to ArcGIS via ArcPro:
# https://pro.arcgis.com/en/pro-app/latest/help/analysis/geoprocessing/basics/geoprocessing-options.htm#ESRI_SECTION1_E0E5BD3DB3134FC690CD68E44CF2D5EE
# Alternative method:
# https://github.com/R-ArcGIS/r-bridge-install

library(rgeos)
library(rgdal)
library(sf)
library(dplyr)
library(arcgisbinding)
arc.check_product()

# Check you are logged in to AGOL
arc.check_portal()
# (if not, go log in!)

# Check working directory
getwd()

# Import GDB workspace
GDBworkspace <- arc.open("https://services2.arcgis.com/n5rY677NVyRuW3Ht/arcgis/rest/services/StewardshipGDB_v3a/FeatureServer")

# Extract issue features as spatial dataframes
Point <- arc.select(arc.open("https://services2.arcgis.com/n5rY677NVyRuW3Ht/arcgis/rest/services/StewardshipGDB_v3a/FeatureServer/0"))
Line <- arc.select(arc.open("https://services2.arcgis.com/n5rY677NVyRuW3Ht/arcgis/rest/services/StewardshipGDB_v3a/FeatureServer/1"))
Poly <- arc.select(arc.open("https://services2.arcgis.com/n5rY677NVyRuW3Ht/arcgis/rest/services/StewardshipGDB_v3a/FeatureServer/2"))

# Extract revised features 
RevisedLine <- arc.select(arc.open("https://services2.arcgis.com/n5rY677NVyRuW3Ht/arcgis/rest/services/StewardshipGDB_v3a/FeatureServer/3"))
RevisedArea <- arc.select(arc.open("https://services2.arcgis.com/n5rY677NVyRuW3Ht/arcgis/rest/services/StewardshipGDB_v3a/FeatureServer/4"))

# Extract management action tables
MngActionLine <- arc.select(arc.open("https://services2.arcgis.com/n5rY677NVyRuW3Ht/arcgis/rest/services/StewardshipGDB_v3a/FeatureServer/5"))
MngActionArea <- arc.select(arc.open("https://services2.arcgis.com/n5rY677NVyRuW3Ht/arcgis/rest/services/StewardshipGDB_v3a/FeatureServer/6"))
MngActionPoint <- arc.select(arc.open("https://services2.arcgis.com/n5rY677NVyRuW3Ht/arcgis/rest/services/StewardshipGDB_v3a/FeatureServer/7"))

# Extract site names for each data type
site_names_pt <- data.frame(sort(unique(trimws(Point$Site))))
site_names_ln <- data.frame(sort(unique(trimws(Line$Site))))
site_names_poly <- data.frame(sort(unique(trimws(Poly$Site))))

# Extract data with management actions (polygons)
cc=data.frame() 
dd=data.frame()                
for (gi in sort(MngActionArea$REL_GLOBALID)){
  aa=subset(Poly, GlobalID==gi)
  bb=subset(MngActionArea, REL_GLOBALID==gi)
  cc=rbind(cc,aa)
  dd=rbind(dd,bb)
}

# Rename GlobalID column to match
dd=dd[c(2:9,11:19)]
colnames(dd)[9] <- "GlobalID"

# Combine issue data with management data
cc=distinct(cc)
dd=distinct(dd)
ee=merge.data.frame(cc,dd,by="GlobalID")
ee=distinct(ee)

# Change "na" text to actual NAs
ee <- ee %>%
  mutate(across(.cols=c("InvSpecies_1", "InvSpecies_2", "InvSpecies_3", 
                        "InvSpecies_4", "InvSpecies_5", "Issue", 
                        "Issue_LocationOrFeature", "Percent_CoverAffected_Veg", 
                        "Percent_CoverAffected_1", "Percent_CoverAffected_2", 
                        "Percent_CoverAffected_3", "Percent_CoverAffected_4",
                        "Percent_CoverAffected_5", "Adjuvant"), na_if, "na"))

# Add in data with no management actions
ff <- data.frame(ee)
for (gi in Poly$GlobalID){
  if (gi %in% ee$GlobalID) {
    } else ff <- bind_rows(ff, Poly[which(Poly$GlobalID == gi),])
}

