# Script for exporting Stewardship GDB from AGOL
# Wildlands Engineering
# by Lydie Costes and Andrew Radecki
# 2022

# Note that the Lead Steward document must be kept up-to-date
# Last updated 8/23/2022.

# Instructions for connecting to ArcGIS via ArcPro:
# https://pro.arcgis.com/en/pro-app/latest/help/analysis/geoprocessing/basics/geoprocessing-options.htm#ESRI_SECTION1_E0E5BD3DB3134FC690CD68E44CF2D5EE
# Alternative method:
# https://github.com/R-ArcGIS/r-bridge-install

library(rgeos)
library(rgdal)
library(sf)
library(dplyr)
library(rmarkdown)
library(arcgisbinding)
arc.check_product()

# Check you are logged in to Arc
arc.check_portal()
# (if not, go log in!)

# Check working directory
getwd()

# Bring in Lead Steward data
LeadSteward <- read.csv("data/LeadSteward.csv")

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


#### POLYGON DATA ####

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
colnames(ee)[3] <- "Subtype"

# Add in data with no management actions
Issue_Mgmt_Poly <- data.frame(ee)
for (gi in Poly$GlobalID){
  if (gi %in% ee$GlobalID) {
    } else Issue_Mgmt_Poly <- bind_rows(Issue_Mgmt_Poly, Poly[which(Poly$GlobalID == gi),])
}

# Change "na" text to actual NAs
Issue_Mgmt_Poly <- Issue_Mgmt_Poly %>%
  mutate(across(.cols=c("InvSpecies_1", "InvSpecies_2", "InvSpecies_3", 
                        "InvSpecies_4", "InvSpecies_5", "Issue", 
                        "Issue_LocationOrFeature", "Percent_CoverAffected_Veg", 
                        "Percent_CoverAffected_1", "Percent_CoverAffected_2", 
                        "Percent_CoverAffected_3", "Percent_CoverAffected_4",
                        "Percent_CoverAffected_5", "Treat_Method_1", 
                        "Chemical_1", "Adjuvant"), na_if, "na"))

# Specify subtype
Issue_Mgmt_Poly <- Issue_Mgmt_Poly %>%
  mutate(Subtype = as.character(Subtype), 
         Subtype = recode(Subtype,'0' = 'Misc', '1' = 'Weed Occurrence', 
                           '2' = 'Vegetation Issue', '3' = 'CE Encroachment'))


#### LINE DATA ####

# Extract data with management actions (lines)
cc=data.frame() 
dd=data.frame()                
for (gi in sort(MngActionLine$REL_GLOBALID)){
  aa=subset(Line, GlobalID==gi)
  bb=subset(MngActionLine, REL_GLOBALID==gi)
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
colnames(ee)[3] <- "Subtype"

# Add in data with no management actions
Issue_Mgmt_Line <- data.frame(ee)
for (gi in Line$GlobalID){
  if (gi %in% ee$GlobalID) {
  } else Issue_Mgmt_Line <- bind_rows(Issue_Mgmt_Line, Line[which(Line$GlobalID == gi),])
}

# Change "na" text to actual NAs
Issue_Mgmt_Line <- Issue_Mgmt_Line %>%
  mutate(across(.cols=c("Issue", "Issue_LocationOrFeature",
                        "Percent_CoverAffected", "Treat_Method_1", 
                        "Chemical_1"), na_if, "na"))

# Specify subtype
Issue_Mgmt_Line <- Issue_Mgmt_Line %>%
  mutate(Subtype = as.character(Subtype), 
         Subtype = recode(Subtype,'0' = 'Misc', '1' = 'Boundary Issue', 
                          '2' = 'Channel Stability/Erosion', 
                          '3' = 'In-Stream Vegetation',
                          '4' = 'Needs Livestakes'))


#### POINT DATA ####

# Extract data with management actions (points)
cc=data.frame() 
dd=data.frame()                
for (gi in sort(MngActionPoint$REL_GLOBALID)){
  aa=subset(Point, GlobalID==gi)
  bb=subset(MngActionPoint, REL_GLOBALID==gi)
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
colnames(ee)[3] <- "Subtype"

# Add in data with no management actions
Issue_Mgmt_Point <- data.frame(ee)
for (gi in Point$GlobalID){
  if (gi %in% ee$GlobalID) {
  } else Issue_Mgmt_Point <- bind_rows(Issue_Mgmt_Point, Point[which(Point$GlobalID == gi),])
}

# Change "na" text to actual NAs
Issue_Mgmt_Point <- Issue_Mgmt_Point %>%
  mutate(across(.cols=c("InvSpecies_1", "InvSpecies_2", "InvSpecies_3", 
                        "InvSpecies_4", "InvSpecies_5", "Mature", 
                        "Percent_CoverAffected_1", "Severity", 
                        "Percent_CoverAffected_2", "Percent_CoverAffected_3", 
                        "Percent_CoverAffected_4", "Percent_CoverAffected_5", 
                        "Issue", "Issue_LocationOrFeature", "Treat_Method_1", 
                        "Chemical_1", "Treat_Method_2", "Chemical_2",
                        "Adjuvant", "Percent_CntrlEffective_"), na_if, "na"))

# Specify subtype
Issue_Mgmt_Point <- Issue_Mgmt_Point %>%
  mutate(Subtype = as.character(Subtype), 
         Subtype = recode(Subtype,'0' = 'Misc', '1' = 'Weed Occurrence', 
                          '2' = 'Failed Structure', '3' = 'Beaver Dam',
                          '4' = 'Soil Sample'))


#### Combined Data ####

Issue_Mgmt_All <- bind_rows(Issue_Mgmt_Poly, Issue_Mgmt_Line, Issue_Mgmt_Point)

# Remove white spaces to consolidate site names
Issue_Mgmt_All$Site <- trimws(Issue_Mgmt_All$Site)

# Add Lead Steward
n <- 1
for (s in Issue_Mgmt_All$Site){
  if (s %in% LeadSteward$Site){
    Issue_Mgmt_All$Lead_Steward[n] <- LeadSteward$Steward[LeadSteward$Site == s]
  } else {
    Issue_Mgmt_All$Lead_Steward < NA
  }
  n = n + 1
}

# Export complete dataset
write.csv("./data/Issue_Mgmt_All")


#### Sort by Issue ####

Boundary <- Issue_Mgmt_All %>%
  filter(Subtype == c("CE Encroachment", "Boundary Issue")) %>%
  select(Site, Year, Issue, Issue_Desc, Treated, Treat_Desc, Resolved, 
         Lead_Steward) %>%
  arrange(Site, desc(Year), desc(Treated))

Channel <- Issue_Mgmt_All %>%
  filter(Subtype == c("Needs Livestakes", "Channel Stability/Erosion", 
                      "Failed Structure")) %>%
  select(Site, Year, Subtype, Issue, Issue_Desc, Treated, Treat_Desc, Resolved,
         Lead_Steward) %>%
  arrange(Site, desc(Year), desc(Treated))

Vegetation <- Issue_Mgmt_All %>% 
  filter(Subtype == "Vegetation Issue") %>%
  select(Site, Year, Issue, Issue_Desc, Percent_CoverAffected_Veg, Treated,
         Treat_Desc, Treat_Method_1, Resolved,
         Lead_Steward) %>%
  arrange(Site, desc(Year), desc(Treated))

Weed <- Issue_Mgmt_All %>%
  filter(Subtype == c("Weed Occurrence", "In-Stream Vegetation")) %>%
  select(Site, Year, InvSpecies_1, InvSpecies_2,
         Issue_Desc, Treat_Desc, Treat_Method_1, TreatDate,
         Resolved, Lead_Steward) %>%
  mutate(TreatDate = format(TreatDate, "%m/%Y")) %>%
  arrange(Site, desc(Year), InvSpecies_1)

Misc <- Issue_Mgmt_All %>% 
  filter(Subtype == c("Misc", "Beaver Dam", "Soil Sample")) %>%
  select(Site, Year, Subtype, Issue_Desc, Issue_LocationOrFeature, Treated, 
         Treat_Desc, Resolved, Lead_Steward) %>%
  arrange(Site, desc(Year), desc(Treated))


### Create PDFs

# It is necessary to install TinyTeX (or perhaps MIKTEX) before these will work
# tinytex::install_tinytex

# Loop through Lead Stewards
# Sorting by site makes more sense - don't bother running this one
# for (user in unique(Issue_Mgmt_All$Lead_Steward)){
#   LeadStewB <- Boundary[Boundary$Lead_Steward == user,]
#   LeadStewC <- Channel[Channel$Lead_Steward == user,]
#   LeadStewV <- Vegetation[Vegetation$Lead_Steward == user,]
#   LeadStewW <- Weed[Weed$Lead_Steward == user,]
#   LeadStewM <- Misc[Misc$Lead_Steward == user,]
#   render("./scripts/StewGDB_Export.Rmd",
#          output_file = paste0('../reports/bySteward/report.', user, '.pdf'))
# }

# Loop through sites
for (s in unique(Issue_Mgmt_All$Site)){
  SiteAll <- Issue_Mgmt_All[Issue_Mgmt_All$Site == s,]
  SiteB <- Boundary[Boundary$Site == s,]
  SiteC <- Channel[Channel$Site == s,]
  SiteV <- Vegetation[Vegetation$Site == s,]
  SiteW <- Weed[Weed$Site == s,]
  SiteM <- Misc[Misc$Site == s,]
  render("./scripts/StewGDB_sites.Rmd", 
         output_file = paste0('../reports/bySite/report.', s, '.pdf'))
}

# To do:
# Work on further optimization of table displays
# Add Lead Scientist and PM info to titles of PDFs
# Explore adding maps to PDFs
# Explore exporting shapefiles for each site
