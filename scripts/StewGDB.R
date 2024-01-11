# Script for exporting Stewardship GDB from AGOL
# Wildlands Engineering
# by Lydie Costes and Andrew Radecki
# 2024

# Note that the Lead Steward document must be kept up-to-date.
# Last updated 1/10/2024.

# Instructions for connecting to ArcGIS via ArcPro:
# https://pro.arcgis.com/en/pro-app/latest/help/analysis/geoprocessing/basics/geoprocessing-options.htm#ESRI_SECTION1_E0E5BD3DB3134FC690CD68E44CF2D5EE
# Alternative method:
# https://github.com/R-ArcGIS/r-bridge-install

library(rgeos)
#library(rgdal)
library(sf)
library(dplyr)
library(rmarkdown)
library(ggmap)
library(ggrepel)
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
#GDBworkspace <- arc.open("https://services2.arcgis.com/n5rY677NVyRuW3Ht/arcgis/rest/services/StewardshipGDB_v3a/FeatureServer")
GDBworkspace <- arc.open("https://services2.arcgis.com/n5rY677NVyRuW3Ht/arcgis/rest/services/StewardshipGDB_v4a/FeatureServer")

# Extract issue features as spatial dataframes
Point <- arc.select(arc.open("https://services2.arcgis.com/n5rY677NVyRuW3Ht/arcgis/rest/services/StewardshipGDB_v4a/FeatureServer/2"))
Line <- arc.select(arc.open("https://services2.arcgis.com/n5rY677NVyRuW3Ht/arcgis/rest/services/StewardshipGDB_v4a/FeatureServer/1"))
Poly <- arc.select(arc.open("https://services2.arcgis.com/n5rY677NVyRuW3Ht/arcgis/rest/services/StewardshipGDB_v4a/FeatureServer/5"))

# Extract revised features 
RevisedLine <- arc.select(arc.open("https://services2.arcgis.com/n5rY677NVyRuW3Ht/arcgis/rest/services/StewardshipGDB_v4a/FeatureServer/4"))
RevisedArea <- arc.select(arc.open("https://services2.arcgis.com/n5rY677NVyRuW3Ht/arcgis/rest/services/StewardshipGDB_v4a/FeatureServer/3"))

# Extract management action tables
MngActionPoint <- arc.select(arc.open("https://services2.arcgis.com/n5rY677NVyRuW3Ht/arcgis/rest/services/StewardshipGDB_v4a/FeatureServer/10"))
MngActionLine <- arc.select(arc.open("https://services2.arcgis.com/n5rY677NVyRuW3Ht/arcgis/rest/services/StewardshipGDB_v4a/FeatureServer/9"))
MngActionArea <- arc.select(arc.open("https://services2.arcgis.com/n5rY677NVyRuW3Ht/arcgis/rest/services/StewardshipGDB_v4a/FeatureServer/8"))



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
dd=dd[c(2:18,20)]
colnames(dd)[18] <- "GlobalID"

# Combine issue data with management data
cc=distinct(cc)
dd=distinct(dd)
ee=merge.data.frame(cc,dd,by="GlobalID")
ee=distinct(ee)
colnames(ee)[3] <- "Subtype"

# reformat date columns bc they're causing issues
#as.POSIXct(Poly$CreationDate, format = "%Y-%M-%D %H:%M:%S")
Poly$CreationDate <- as.POSIXct.Date(Poly$CreationDate)
Poly$EditDate <- as.POSIXct.Date(Poly$EditDate)


# Add in data with no management actions
Issue_Mgmt_Poly <- data.frame(ee)

for (gi in Poly$GlobalID){
  if (gi %in% ee$GlobalID) {
    } else Issue_Mgmt_Poly <- bind_rows(Issue_Mgmt_Poly, 
                                        Poly[which(Poly$GlobalID == gi),])
}

# Change "na" text to actual NAs
Issue_Mgmt_Poly <- Issue_Mgmt_Poly %>%
  mutate(across(.cols=c("InvSpecies_1", "InvSpecies_2", "InvSpecies_3", 
                        "InvSpecies_4", "InvSpecies_5", "Issue", 
                        "Issue_LocationOrFeature", "Percent_CoverAffected_Veg", 
                        "Percent_CoverAffected_1", "Percent_CoverAffected_2", 
                        "Percent_CoverAffected_3", "Percent_CoverAffected_4",
                        "Percent_CoverAffected_5", "Resolved", "Treat_Method_1", 
                        "Chemical_1", "Adjuvant"), na_if, "na"))

# Specify poly subtype
Issue_Mgmt_Poly <- Issue_Mgmt_Poly %>%
  mutate(Subtype = as.character(Subtype), 
         Subtype = recode(Subtype,'0' = 'Misc', '1' = 'Weed Occurrence', 
                           '2' = 'Vegetation Issue', '3' = 'CE Encroachment'),
         Geom = "Poly")


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
dd=dd[c(2:18,20)]
colnames(dd)[18] <- "GlobalID"

# Combine issue data with management data
cc=distinct(cc)
dd=distinct(dd)
ee=merge.data.frame(cc,dd,by="GlobalID")
ee=distinct(ee)
colnames(ee)[3] <- "Subtype"

# reformat date columns bc they're causing issues
#as.POSIXct(Poly$CreationDate, format = "%Y-%M-%D %H:%M:%S")
Line$CreationDate <- as.POSIXct.Date(Line$CreationDate)
Line$EditDate <- as.POSIXct.Date(Line$EditDate)

# Add in data with no management actions
Issue_Mgmt_Line <- data.frame(ee)
for (gi in Line$GlobalID){
  if (gi %in% ee$GlobalID) {
  } else Issue_Mgmt_Line <- bind_rows(Issue_Mgmt_Line, Line[which(Line$GlobalID == gi),])
}

# Change "na" text to actual NAs
Issue_Mgmt_Line <- Issue_Mgmt_Line %>%
  mutate(across(.cols=c("Issue", "Issue_LocationOrFeature", "Resolved",
                        "Percent_CoverAffected", "Treat_Method_1", 
                        "Chemical_1"), na_if, "na"))

# Specify line subtype
Issue_Mgmt_Line <- Issue_Mgmt_Line %>%
  mutate(Subtype = as.character(Subtype), 
         Subtype = recode(Subtype,'0' = 'Misc', '1' = 'Boundary Issue', 
                          '2' = 'Channel Stability/Erosion', 
                          '3' = 'In-Stream Vegetation',
                          '4' = 'Needs Livestakes'),
         Geom = "Line")

# Fix NumTreatDays format
Issue_Mgmt_Line$NumTreatDays <- as.integer(Issue_Mgmt_Line$NumTreatDays)


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
dd=dd[c(2:18,20)]
colnames(dd)[18] <- "GlobalID"

# Combine issue data with management data
cc=distinct(cc)
dd=distinct(dd)
ee=merge.data.frame(cc,dd,by="GlobalID")
ee=distinct(ee)
colnames(ee)[3] <- "Subtype"

# reformat date columns bc they're causing issues
#as.POSIXct(Poly$CreationDate, format = "%Y-%M-%D %H:%M:%S")
Point$CreationDate <- as.POSIXct.Date(Point$CreationDate)
Point$EditDate <- as.POSIXct.Date(Point$EditDate)

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
                        "Resolved", "Issue", "Issue_LocationOrFeature", 
                        "Treat_Method_1", "Chemical_1", "Treat_Method_2", 
                        "Chemical_2","Adjuvant", "Percent_CntrlEffective_"), 
                na_if, "na"))

# Specify point subtype
Issue_Mgmt_Point <- Issue_Mgmt_Point %>%
  mutate(Subtype = as.character(Subtype), 
         Subtype = recode(Subtype,'0' = 'Misc', '1' = 'Weed Occurrence', 
                          '2' = 'Failed Structure', '3' = 'Beaver Dam',
                          '4' = 'Soil Sample'),
         Geom = "Point")

# Fix NumTreatDays format
Issue_Mgmt_Point$NumTreatDays <- as.integer(Issue_Mgmt_Point$NumTreatDays)


#### Combined Data ####

Issue_Mgmt_All <- bind_rows(Issue_Mgmt_Poly, Issue_Mgmt_Line, Issue_Mgmt_Point)

# Remove white spaces to consolidate site names
Issue_Mgmt_All$Site <- trimws(Issue_Mgmt_All$Site)

# Make site names uniform
# Issue_Mgmt_All <- Issue_Mgmt_All %>%
#   mutate(Site = if_else(Site == "Alexander Farm Wildlands", "Alexander Farm", Site),
#          Site = if_else(Site == "Deep Meadow Mitigation Site", "Deep Meadow", Site),
#          Site = if_else(Site == "Liberty Rock Site", "Liberty Rock", Site),
#          Site = if_else(Site == "Little River Ford I and II", "Little River Ford", Site),
#          Site = if_else(Site == "Lone Hickory Mitigation Site", "Lone Hickory", Site),
#          Site = if_else(Site == "McClenny Acres Bank", "McClenny", Site),
#          Site = if_else(Site == "Moccasin Creek-Buffer (G)", "Moccasin Creek", Site))

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

# Make Subtype a factor
Issue_Mgmt_All$Subtype <- factor(Issue_Mgmt_All$Subtype)

# Remove underscores that can interfere with TeX
#names(Issue_Mgmt_All) <- gsub("_", "", names(Issue_Mgmt_All))

# Export complete dataset
write.csv("./data/Issue_Mgmt_All")


#### Sort by Issue ####

Boundary <- Issue_Mgmt_All %>%
  filter(Subtype %in% c("CE Encroachment", "Boundary Issue")) %>%
  select(OBJECTID, Site, Year, Issue, Issue_Desc, Treat_Desc, TreatDate, Resolved, 
         Lead_Steward, Geom) %>%
  mutate(TreatDate = format(TreatDate, "%m/%Y")) %>%
  arrange(Site, desc(Year), desc(Resolved))

Channel <- Issue_Mgmt_All %>%
  filter(Subtype %in% c("Needs Livestakes", "Channel Stability/Erosion", 
                      "Failed Structure")) %>%
  select(OBJECTID, Site, Year, Subtype, Issue, Issue_Desc, Treat_Desc, TreatDate, Resolved,
         Lead_Steward, Geom) %>%
  mutate(TreatDate = format(TreatDate, "%m/%Y")) %>%
  arrange(Site, desc(Year), desc(Resolved))

Vegetation <- Issue_Mgmt_All %>% 
  filter(Subtype == "Vegetation Issue") %>%
  select(OBJECTID, Site, Year, Issue, Issue_Desc,
         Treat_Desc, Treat_Method_1, TreatDate, Resolved,
         Lead_Steward, Geom) %>%
  mutate(TreatDate = format(TreatDate, "%m/%Y")) %>%
  arrange(Site, desc(Year), desc(Resolved))

Weed <- Issue_Mgmt_All %>%
  filter(Subtype %in% c("Weed Occurrence", "In-Stream Vegetation")) %>%
  select(OBJECTID, Site, Year, InvSpecies_1, InvSpecies_2,
         Issue_Desc, Treat_Desc, Treat_Method_1, TreatDate,
         Resolved, Lead_Steward, Geom) %>%
  mutate(TreatDate = format(TreatDate, "%m/%Y")) %>%
  arrange(Site, desc(Year), InvSpecies_1, desc(Resolved))

Misc <- Issue_Mgmt_All %>% 
  filter(Subtype %in% c("Misc", "Beaver Dam", "Soil Sample")) %>%
  select(OBJECTID, Site, Year, Subtype, Issue_Desc, Issue_LocationOrFeature, 
         Treat_Desc, TreatDate, Resolved, Lead_Steward, Geom) %>%
  mutate(TreatDate = format(TreatDate, "%m/%Y")) %>%
  arrange(Site, desc(Year), desc(Resolved))








#### Create PDFs ####

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
  unloadNamespace("kableExtra")
  render("./scripts/StewGDB_sites.Rmd", 
         output_file = paste0('../reports/PDFbySite/report.', s, '.pdf'))
}



#### Create CSVs ####

for (s in unique(Issue_Mgmt_All$Site)){
  SiteAll <- Issue_Mgmt_All %>%
    filter(Site == s & !is.na(Site)) %>%
    arrange(desc(Year), Subtype) %>%
    relocate(c(Site, Year, Subtype), .before = GlobalID) %>%
    relocate(GlobalID, .after = Lead_Steward) %>%
    rename(CreationDate.Mgmt = CreationDate.y,
           Creator.Mgmt = Creator.y,
           EditDate.Mgmt = EditDate.y,
           Editor.Mgmt = Editor.y) %>%
    mutate(CreationDate.Issue = if_else(is.na(CreationDate), CreationDate.x, 
                                        CreationDate),
           Creator.Issue = if_else(is.na(Creator), Creator.x, Creator),
           EditDate.Issue = if_else(is.na(EditDate), EditDate.x, EditDate),
           Editor.Issue = if_else(is.na(Editor), Editor.x, Editor)) %>%
    #relocate(c(CreationDate.Issue, Creator.Issue, EditDate.Issue, Editor.Issue), 
             #.before = ISSUE_ID_) %>%
    select(-c(CreationDate, Creator, EditDate, Editor, CreationDate.x, Creator.x,
              EditDate.x, Editor.x))
  write.csv(SiteAll, paste0('./reports/Spreadsheets/', s, '.csv'))
}


#### Create Maps ####

#Import CE Workspace
#CEworkspace <- arc.open("https://services2.arcgis.com/n5rY677NVyRuW3Ht/arcgis/rest/services/WEI_MitigationProject_CEs/FeatureServer")
# Extract issue features as spatial dataframes
CEpoly <- arc.open("https://services2.arcgis.com/n5rY677NVyRuW3Ht/arcgis/rest/services/WEI_MitigationProject_CEs/FeatureServer/0")
CEpoly_sel <- arc.select(CEpoly)

#Import Streams layer & extract
Streams <- arc.open("https://services2.arcgis.com/n5rY677NVyRuW3Ht/arcgis/rest/services/AssetGDB_v1a/FeatureServer/10")
Streams_sel <- arc.select(Streams)

# Extract features as spatial dataframes
Point <- arc.open("https://services2.arcgis.com/n5rY677NVyRuW3Ht/arcgis/rest/services/StewardshipGDB_v4a/FeatureServer/2")
Point_sel <- arc.select(Point)
Line <- arc.open("https://services2.arcgis.com/n5rY677NVyRuW3Ht/arcgis/rest/services/StewardshipGDB_v4a/FeatureServer/1")
Line_sel <- arc.select(Line)
Poly <- arc.open("https://services2.arcgis.com/n5rY677NVyRuW3Ht/arcgis/rest/services/StewardshipGDB_v4a/FeatureServer/5")
Poly_sel <- arc.select(Poly)

# Loop through sites
# Create data subsets
# If-then loops only produce tables and maps if data exist
for (s in unique(Issue_Mgmt_All$Site)){
  SiteAll <- Issue_Mgmt_All[Issue_Mgmt_All$Site == s,]
  SiteB <- Boundary[Boundary$Site == s,]
  SiteC <- Channel[Channel$Site == s,]
  SiteV <- Vegetation[Vegetation$Site == s,]
  SiteW <- Weed[Weed$Site == s,]
  SiteM <- Misc[Misc$Site == s,]
  unloadNamespace("kableExtra")
  if(!is.na(any(CEpoly_sel$Name == s))){
    CE_site <- arc.data2sf(arc.select(CEpoly, where_clause = paste("Name = '", s, "'", sep = "")))}
  if(!is.na(any(Streams_sel$Site == s))){
    Streams_site <- arc.data2sf(arc.select(Streams, where_clause = paste("Site = '", s, "'", sep = "")))}
  if(any(Point_sel[Point_sel$Site==s,]$Subtype == 2)){
    Point_C <- arc.data2sf(arc.select(Point, where_clause = paste("Site = '", s, "' AND Subtype = 2", sep = "")))}
  if(any(Point_sel[Point_sel$Site==s,]$Subtype == 1)){
    Point_W <- arc.data2sf(arc.select(Point, where_clause = paste("Site = '", s, "' AND Subtype = 1", sep = "")))}
  if(any(Point_sel[Point_sel$Site==s,]$Subtype == c(0,3,4))){
    Point_M <- arc.data2sf(arc.select(Point, where_clause = paste("Site = '", s, "' AND (Subtype = 0 OR Subtype = 3 OR Subtype = 4)", sep = "")))
    Point_M <- Point_M %>%
      mutate(Subtype = as.character(Subtype),
             Subtype = recode(Subtype, 
                              '0' = 'Misc', 
                              '3' = 'Beaver Dam',
                              '4' = 'Soil Sample'))}
  if(any(Line_sel[Line_sel$Site==s,]$Subtype == 1)){
    Line_B <- arc.data2sf(arc.select(Line, where_clause = paste("Site = '", s, "' AND Subtype = 1", sep = "")))}
  if(any(Line_sel[Line_sel$Site==s,]$Subtype == c(2,4))){
    Line_C <- arc.data2sf(arc.select(Line, where_clause = paste("Site = '", s, "' AND (Subtype = 2 OR Subtype = 4)", sep = "")))}
  if(any(Line_sel[Line_sel$Site==s,]$Subtype == 3)){
    Line_W <- arc.data2sf(arc.select(Line, where_clause = paste("Site = '", s, "' AND Subtype = 3", sep = "")))}
  if(any(Line_sel[Line_sel$Site==s,]$Subtype == 0)){
    Line_M <- arc.data2sf(arc.select(Line, where_clause = paste("Site = '", s, "' AND Subtype = 0", sep = "")))
    Line_M <- Line_M %>%
      mutate(Subtype = as.character(Subtype),
             Subtype = recode(Subtype, 
                              '0' = 'Misc'))}
  if(any(Poly_sel[Poly_sel$Site==s,]$Subtype == 3)){
    Poly_B <- arc.data2sf(arc.select(Poly, where_clause = paste("Site = '", s, "' AND Subtype = 3", sep = "")))}
  if(any(Poly_sel[Poly_sel$Site==s,]$Subtype == 2)){
    Poly_V <- arc.data2sf(arc.select(Poly, where_clause = paste("Site = '", s, "' AND Subtype = 2", sep = "")))}
  if(any(Poly_sel[Poly_sel$Site==s,]$Subtype == 1)){
    Poly_W <- arc.data2sf(arc.select(Poly, where_clause = paste("Site = '", s, "' AND Subtype = 1", sep = "")))}
  if(any(Poly_sel[Poly_sel$Site==s,]$Subtype == 0)){
    Poly_M <- arc.data2sf(arc.select(Poly, where_clause = paste("Site = '", s, "' AND Subtype = 0", sep = "")))
    Poly_M <- Poly_M %>%
      mutate(Subtype = as.character(Subtype),
             Subtype = recode(Subtype, 
                              '0' = 'Misc'))}
  render("./scripts/StewGDB_sites_mapped.Rmd", 
         output_file = paste0('../reports/PDFMapbySite/report.', s, '.pdf'))
}


# Extract layers for a single site and convert to sf objects
CE_site <- arc.select(CEpoly, where_clause = "Name = 'Alexander Farm'")
CE_site.sf <- arc.data2sf(CE_site)
#plot(CE_site.sf)
Point_site <- arc.select(arc.open("https://services2.arcgis.com/n5rY677NVyRuW3Ht/arcgis/rest/services/StewardshipGDB_v4a/FeatureServer/2"),
                    where_clause = "Site = 'Alexander Farm' AND Subtype = 1")
Point_site.sf <- arc.data2sf(Point_site)
#plot(Point_site.sf)

# Map a single site - point features (try to add labels if possible?)
ggplot() +
  theme_void() +
  geom_sf(data = Point_site.sf) +
  #geom_sf_text(data = Point_site.sf, aes(label = OBJECTID), size = 3) +
  geom_text_repel(data = Point_site.sf, aes(label = OBJECTID, geometry = geom),
                   stat = "sf_coordinates", size = 3) +
  geom_sf(data = CE_site.sf, colour = "red", fill = NA)

#proj4string(Point.sp)=CRS("+init=epsg:2264") 
#Point.sp <- spTransform(Point.sp, CRS("+proj=longlat +datum=WGS84"))
#plot(Point.sp)
#plot(CEpoly.sp)
# 
# mybaseMap <- get_stamenmap(bbox = c(left = -84.321869,
#                                 bottom = 33.842316,
#                                 right = -75.460621,
#                                 top = 36.588117),
#                        maptype = "terrain",
#                        crop = FALSE)
# 
# ggmap(mybaseMap) +
#   geom_sf(data = as.data.frame(coordinates(Point.sp)), 
#           aes(x = coords.x1, y = coords.x2))
# 
# geom_sf(data = Point.sp)
# To do:
# Work on further optimization of table displays
# Add Lead Scientist and PM info to titles of PDFs
# Explore adding maps to PDFs
# Explore exporting shapefiles for each site
