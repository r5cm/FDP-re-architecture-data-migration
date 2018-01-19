
# 1. Map objects and fields from objects_v1 to objects_v2 ---------------------------

# RETRIEVE OBJECTS AND FIELDS V1
# Login to Salesforce admin V1
library(RForcecom)
username.1 <- "admin@utzmars.org"
password.1 <- "gfutzmars2018n0ljYwQQqYVWfu9RIfPqWIn8"
session.1 <- rforcecom.login(username.1, password.1)
# List of objects_V1 to migrate
fdp.objects.1 <- c("FDP_submission__c", "farmer__c", "farmer_BL__c", "Farm__c", 
                 "Farm_BL__c", "plot__c", "AODiagnostic__c", "Adoption_observations__c",
                 "PL_FDP__c", "FDP_calendar__c")
# Retrieve objects_V1 meta-data
metadata.1 <- list()
for(i in seq_along(fdp.objects.1)) {
      metadata.1[[i]] <- rforcecom.getObjectDescription(session.1, fdp.objects.1[i])
      names(metadata.1)[i] <- fdp.objects.1[i]
}
# Create data frame with object_v1, field_v1 and  type_v1 columns
obj.fields.1 <- data.frame()
for(i in seq_along(metadata.1)) {
            temp <- data.frame(object_v1 = rep(names(metadata.1)[i]),
                               field_v1 = metadata.1[[i]]$name,
                               type_v1 = metadata.1[[i]]$type)
            obj.fields.1 <- rbind(obj.fields.1, temp)
            rm(temp)
}

# RETRIEVE OBJECTS AND FIELDS V2
# Login to Salesforce admin V2
username.2 <- "fdp@gf.org"
password.2 <- "gf2018**pzokaC5MiV6mJGjbiisTQU9fl"
apiversion.2 <- "41.0"
url.2 <- "https://fdp-dev-ed.my.salesforce.com/"
session.2 <- rforcecom.login(username.2, password.2, url.2)
# List of objects_V2 to insert
fdp.objects.2 <- c("Submission__c", "Answer__c", "fdp_farmer__c", "fdp_farmer_BL__c",
                   "fdp_Farm__c", "fdp_Farm_BL__c", "fdp_plot__c",
                   "fdp_Diagnostic_Monitoring__c", "fdp_PL__c", "fdp_Calendar__c",
                   "observation__c")
# Retrieve objects_V2 meta-data
metadata.2 <- list()
for(i in seq_along(fdp.objects.2)) {
      metadata.2[[i]] <- rforcecom.getObjectDescription(session.2, fdp.objects.2[i])
      names(metadata.2)[i] <- fdp.objects.2[i]
}
# Create data frame with object_v2, field_v2, type_v2 and calculated_v2 columns
obj.fields.2 <- data.frame()
for(i in seq_along(metadata.2)) {
      temp <- data.frame(object_v2 = rep(names(metadata.2)[i]),
                         field_v2 = metadata.2[[i]]$name,
                         type_v2 = metadata.2[[i]]$type,
                         calculated_v2 = metadata.2[[i]]$calculated)
      obj.fields.2 <- rbind(obj.fields.2, temp)
      rm(temp)
}

# MAP V1 TO V2
# Access google sheet
library(googlesheets)
library(dplyr)
my_sheets <- gs_ls()
template.info <- gs_title("V1 to V2 mapping")
# Load objects and fields to google sheet (WARNING!!!!! ONLY DO THIS ONCE!!!!!!!!)
gs_edit_cells(template.info, ws = "V1", input = obj.fields.1, col_names = TRUE, trim = TRUE)
gs_edit_cells(template.info, ws = "V2", input = obj.fields.2, col_names = TRUE)

# Map objects_v1 to objects_v2 -> Done in google sheets edited above


# 2. Extract data from objects_v1 and structure as in objects_v2 --------------------

# Retrieve v2 to v1 mapping table
map.v2v1 <- data.frame(gs_read(template.info, "V2", "A1:G306"))
# Remove formula fields from map.v2v1
map.v2v1 <- map.v2v1[map.v2v1$calc_v2 == FALSE, ]
# Remove fields that won't be loaded (N/A in field_v1 column)
map.v2v1 <- map.v2v1[map.v2v1$field_v1 != "N/A", ]

# Create list with empty data frames with objects_v2 to load (P&L and Calendar not incl.)
obj.2 <- c("Submission__c", "fdp_farmer__c", "fdp_farmer_BL__c", "fdp_Farm__c",
           "fdp_Farm_BL__c", "fdp_plot__c", "fdp_Diagnostic_Monitoring__c", 
           "observation__c")
data.2 <- list()

# Retrieve data and order as in objects_v2
for(i in seq_along(obj.2)) {
      map.temp <- map.v2v1[map.v2v1$object_v2 == obj.2[i], c(1:3, 6, 7)]

      # Change variable names from v1 to v2 and value NA if field is reference
      for(j in seq_along(data.2[[i]])) {
            # Retrieve data from v1
            query.temp <- paste("SELECT", map.temp$field_v1[j], "FROM", fdp.objects.1[i],
                                "LIMIT 100", sep = " ")
            data.temp <- rforcecom.query(session.1, query.temp)
            data.temp <- select(data.temp, map.temp$field_v1[j])
            names(data.temp)[1] <- map.temp$field_v2[j]
            # reference fields value == NA
            if(map.temp$type_v2[j] == "reference") {data.temp <- NA}
            # add field to i-th object_v2
            if(j == 1) {
                  data.2[[i]] <- data.temp
            } else {
                  data.2[[i]] <- data.frame(data.2[[i]], data.temp)
            }
      }
      names(data.2)[i] <- obj.2[i]
}


for(i in seq_along(obj.2)) {
      # Retrieve data from i-th object_v1
      map.temp <- map.v2v1[map.v2v1$object_v2 == obj.2[i], c(1:3, 6, 7)]
      
      for(j in 1:nrow(map.temp)) {
            data.temp <- rforcecom.retrieve(session.1, fdp.objects.1[i], 
                                            map.temp$field_v1[j], limit = 100)
            
            if(map.temp[j, 3] == "reference") {data.temp <- NA}
            ifelse(j == i, data[[i]] <- data.temp, data[[i]] <- data.frame(data[[i]],
                                                                           data.temp))
            names(data.2[[i]])[j] <- map.temp[j, 2]
      }
      names(data.2)[i] <- obj.2[i]
      
      
      data.2[[i]] <- rforcecom.retrieve(session.1, fdp.objects.1[i], map.temp$field_v1[7])
      
      # Name data frame as the i-th object_v2
      
      
      # Change the names to v2 objects names and all values for NA if id field
      
}

     # REMOVE THE 100 LIMIT ABOVE
      
      
      
      

# MAKE SURE TO NOT TRY TO FILL REFERENCES, FORMULAS OR ROLLUPS
# TO CLEAN DATA: LEAVE ONLY FARMERS UPLOADED USING LISTS FROM MARS
#     LEAVE ONLY SYNCHED FARMERS?