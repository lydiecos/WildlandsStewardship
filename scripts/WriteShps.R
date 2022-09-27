# Radecki's code to write shapefiles from the GDB
# Copied Sept 19, 2022

library(rgeos)
library(readr)
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


Poly_fs = arc.open("https://services2.arcgis.com/n5rY677NVyRuW3Ht/arcgis/rest/services/StewardshipGDB_v3a/FeatureServer/2")
Poly_s = arc.select(Poly_fs)
Poly_Issue = arc.data2sp(Poly_s)

Poly_Issuedata = Poly_Issue@data



# rr=data.frame()
# for (i in sort(unique(Issue_Mgmt_Poly$Site))){
#   zz_poly=subset(Poly_Issue, Site==i)
#   zzz = zz_poly@data
#   yy=subset(Issue_Mgmt_Poly, Site==i)
#   
#   ##zz_poly@data=yy
#   zzzz = zz_poly@data
#   
#   c1=nrow(zzz)
#   c2=nrow(zzzz)
#   tf = isTRUE(c1==c2)
#   df= data.frame(cbind(i, tf))
#   df= cbind(df,c1)
#   df= cbind(df,c2)
#   rr=rbind(rr,df)               
#   
#   dir_out = paste("C:/Users/aradecki/OneDrive - Wildlands Engineering Inc/Documents/Stewardship/PDFs/")
#   pdf(paste(dir_out, i,".pdf",sep=""), width=8.5, height=11)
#   for (n in zz_poly$GlobalID ){
#     nn = subset(zz_poly, GlobalID==n)
#     st = subset(yy, GlobalID==n)
#     nndata=nn@data[c(3:5)]
#     issue_desr = nn$Issue_Desc
#     subtype= unique(st$Subtype)
#     plot(nn, col='orange',
#          main=paste(i,'\n',subtype,'\n','Issue Description:','\n',issue_desr))
#     text(2,3,issue_desr)
#     pushViewport(viewport(y=.25,height=.25))
#     grid.table(nndata)
#     
#   }
#   dev.off()
#   
# }
##write.csv(yy ,paste("C:/Users/aradecki/OneDrive - Wildlands Engineering Inc/Documents/Stewardship/Stewardship/Exceldata",paste(i,".csv", sep = ""), sep ="/"))
##writeOGR(zz, dsn ="Stewardship/GISdata", layer = paste(i,'poly'), driver ='ESRI Shapefile', overwrite_layer = T)





Line_fs = arc.open(https://services2.arcgis.com/n5rY677NVyRuW3Ht/arcgis/rest/services/StewardshipGDB_v3a/FeatureServer/1)
Line_s = arc.select(Line_fs)
Line_Issue = arc.data2sp(Line_s)


rr=data.frame()
for (i in sort(unique(Issue_Mgmt_Line$Site))){
  zz_line=subset(Line_Issue, Site==i)
  zzz = zz_line@data
  yy=subset(Issue_Mgmt_Line, Site==i)
  zz@data=yy
  zzzz = zz_line@data
  
  c1=nrow(zzz)
  c2=nrow(zzzz)
  tf = isTRUE(c1==c2)
  df= data.frame(cbind(i, tf))
  df= cbind(df,c1)
  df= cbind(df,c2)
  rr=rbind(rr,df)               
  
  
  ##write.csv(yy ,paste("C:/Users/aradecki/OneDrive - Wildlands Engineering Inc/Documents/Stewardship/Stewardship/Exceldata",paste(i,".csv", sep = ""), sep ="/"))
  ##writeOGR(zz, dsn ="Stewardship/GISdata", layer = paste(i,'Line'), driver ='ESRI Shapefile', overwrite_layer = T)
  
}


Point_fs = arc.open(https://services2.arcgis.com/n5rY677NVyRuW3Ht/arcgis/rest/services/StewardshipGDB_v3a/FeatureServer/0)
Point_s = arc.select(Point_fs)
Point_Issue = arc.data2sp(Point_s)


rr=data.frame()
for (i in sort(unique(Issue_Mgmt_Point$Site))){
  zz=subset(Point_Issue, Site==i)
  zzz = zz@data
  yy=subset(Issue_Mgmt_Point, Site==i)
  zz@data=yy
  zzzz = zz@data
  
  c1=nrow(zzz)
  c2=nrow(zzzz)
  tf = isTRUE(c1==c2)
  df= data.frame(cbind(i, tf))
  df= cbind(df,c1)
  df= cbind(df,c2)
  rr=rbind(rr,df)               
  
  
  ## write.csv(yy ,paste("C:/Users/aradecki/OneDrive - Wildlands Engineering Inc/Documents/Stewardship/Stewardship/Exceldata",paste(i,".csv", sep = ""), sep ="/"))
  ## writeOGR(zz, dsn ="Stewardship/GISdata", layer = paste(i,'Point'), driver ='ESRI Shapefile', overwrite_layer = T)
}
