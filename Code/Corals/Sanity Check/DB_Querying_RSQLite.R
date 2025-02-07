library(RSQLite)
library(DBI)
library(dplyr)

#establish connection to database
new <- dbConnect(SQLite(), "C:/Users/ckratochwill2010/Dropbox/PC (2)/Desktop/Recovery_Data/Manuscript documents/Heatwaves and Coral Recovery Database (HeatCRD).db") #connect to the database
dbListTables(new) #shows all the tables in the database
dbListFields(new, "Site_Description_tbl") #shows all the fields in the specified table
dbListFields(new, "Sample_Event_tbl")#shows all the fields in the specified table
dbListFields(new, "Cover_tbl")


#Queries for all sites
All_Sites <- dbSendQuery(new, "SELECT Site_Description_tbl.Site_ID, Site_Description_tbl.Latitude_Degrees, Site_Description_tbl.Longitude_Degrees, Ocean_Name_LUT.Ocean_Name, Realm_Name_LUT.Realm_Name, Ecoregion_Name_LUT.Ecoregion_Name, Site_Description_tbl.Ecoregion_distance, Country_Name_LUT.Country_Name, State_Island_Province_Name_LUT.State_Island_Province_Name, Location_Name_LUT.Location_Name, Site_Description_tbl.Site_Name, Habitat_Type_LUT.Habitat_Type, Site_Description_tbl.Habitat_Distance, Site_Level_LUT.Estimated_Resolution, Site_Description_tbl.Distance_to_shore, Exposure_LUT.Exposure, Site_Description_tbl.Turbidity, Sources_tbl.Citation, Site_Description_tbl.Comments
FROM State_Island_Province_Name_LUT RIGHT JOIN (Sources_tbl RIGHT JOIN (Site_Level_LUT RIGHT JOIN (Realm_Name_LUT INNER JOIN (Ocean_Name_LUT RIGHT JOIN (Location_Name_LUT RIGHT JOIN (Habitat_Type_LUT RIGHT JOIN (Exposure_LUT RIGHT JOIN (Ecoregion_Name_LUT RIGHT JOIN (Country_Name_LUT RIGHT JOIN Site_Description_tbl ON Country_Name_LUT.Country_Name_ID = Site_Description_tbl.Country_Name) ON Ecoregion_Name_LUT.Ecoregion_Name_ID = Site_Description_tbl.Ecoregion_Name) ON Exposure_LUT.Exposure_ID = Site_Description_tbl.Exposure) ON Habitat_Type_LUT.Habitat_Type_ID = Site_Description_tbl.Habitat_Type) ON Location_Name_LUT.Location_Name_ID = Site_Description_tbl.Location_Name) ON Ocean_Name_LUT.Ocean_Name_ID = Site_Description_tbl.Ocean_Name) ON Realm_Name_LUT.Realm_ID = Site_Description_tbl.Realm_Name) ON Site_Level_LUT.Site_Level_ID = Site_Description_tbl.Site_Level) ON Sources_tbl.Source_ID = Site_Description_tbl.Source) ON State_Island_Province_Name_LUT.State_Island_Province_ID = Site_Description_tbl.State_Island_Province_Name;")
dbFetch(All_Sites) #shows results of query

#Queries for all sample events
Sample_Events <- dbSendQuery(new, "SELECT Site_Description_tbl.Site_ID, Site_Description_tbl.Latitude_Degrees, Site_Description_tbl.Longitude_Degrees, Ocean_Name_LUT.Ocean_Name, Realm_Name_LUT.Realm_Name, Ecoregion_Name_LUT.Ecoregion_Name, Country_Name_LUT.Country_Name, Location_Name_LUT.Location_Name, Site_Description_tbl.Site_Name, Sample_Event_tbl.Date_Day, Sample_Event_tbl.Date_Month, Sample_Event_tbl.Date_Year, Sample_Event_tbl.Depth
FROM Location_Name_LUT INNER JOIN (Country_Name_LUT INNER JOIN (Ecoregion_Name_LUT INNER JOIN (Realm_Name_LUT INNER JOIN (Ocean_Name_LUT INNER JOIN (Site_Description_tbl INNER JOIN Sample_Event_tbl ON Site_Description_tbl.Site_ID = Sample_Event_tbl.Site_ID) ON Ocean_Name_LUT.Ocean_Name_ID = Site_Description_tbl.Ocean_Name) ON Realm_Name_LUT.Realm_ID = Site_Description_tbl.Realm_Name) ON Ecoregion_Name_LUT.Ecoregion_Name_ID = Site_Description_tbl.Ecoregion_Name) ON Country_Name_LUT.Country_Name_ID = Site_Description_tbl.Country_Name) ON Location_Name_LUT.Location_Name_ID = Site_Description_tbl.Location_Name;") 
#query for all sampling events
dbFetch(Sample_Events)#show results of query

#Queries for all cover data
Coral_Cover <- dbSendQuery(new, "SELECT Site_Description_tbl.Site_ID, Site_Description_tbl.Latitude_Degrees, Site_Description_tbl.Longitude_Degrees, Ocean_Name_LUT.Ocean_Name, Realm_Name_LUT.Realm_Name, Ecoregion_Name_LUT.Ecoregion_Name, Country_Name_LUT.Country_Name, Location_Name_LUT.Location_Name, Site_Description_tbl.Site_Name, Sample_Event_tbl.Date_Day, Sample_Event_tbl.Date_Month, Sample_Event_tbl.Date_Year, Sample_Event_tbl.Depth, Cover_tbl.[Percent_Hard_Coral_Cover]
FROM (Location_Name_LUT INNER JOIN (Country_Name_LUT INNER JOIN (Ecoregion_Name_LUT INNER JOIN (Realm_Name_LUT INNER JOIN (Ocean_Name_LUT INNER JOIN (Site_Description_tbl INNER JOIN Sample_Event_tbl ON Site_Description_tbl.Site_ID = Sample_Event_tbl.Site_ID) ON Ocean_Name_LUT.Ocean_Name_ID = Site_Description_tbl.Ocean_Name) ON Realm_Name_LUT.Realm_ID = Site_Description_tbl.Realm_Name) ON Ecoregion_Name_LUT.Ecoregion_Name_ID = Site_Description_tbl.Ecoregion_Name) ON Country_Name_LUT.Country_Name_ID = Site_Description_tbl.Country_Name) ON Location_Name_LUT.Location_Name_ID = Site_Description_tbl.Location_Name) INNER JOIN Cover_tbl ON Sample_Event_tbl.Sample_ID = Cover_tbl.Sample_ID;")
#query for all coral cover data
dbFetch(Coral_Cover) #show results of query

#review data from the sample event table
Sample_Events <- dbReadTable(new, "Sample_Event_tbl")
#shows number of samples by year
Sample_Events %>% group_by(Date_Year) %>%
  summarize(total_samples = n())

#review data from the site description table
Site_Description_tbl <- dbReadTable(new, 'Site_Description_tbl')
#Show how many sites occur by country
Site_Description_tbl %>% group_by(Country_Name) %>% 
  summarize(total_country = n())


dbDisconnect(new) #closes connection to the database
