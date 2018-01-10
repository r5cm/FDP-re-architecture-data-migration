
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
                 "AOFU__c", "PL_FDP__c", "FDP_calendar__c")
# Retrieve objects_V1 meta-data
metadata.1 <- list()
for(i in seq_along(fdp.objects.1)) {
      metadata.1[[i]] <- rforcecom.getObjectDescription(session.1, fdp.objects.1[i])
      names(metadata.1)[i] <- fdp.objects.1[i]
}
# Create data frame with object_v1 and field_v1 columns
obj.fields.1 <- data.frame()
for(i in seq_along(metadata.1)) {
            temp <- data.frame(object_v1 = rep(names(metadata.1)[i]),
                               field_v1 = metadata.1[[i]]$name)
            obj.fields.1 <- rbind(obj.fields.1, temp)
            rm(temp)
}

# RETRIEVE OBJECTS AND FIELDS V2
# Login to Salesforce admin V2
username.2 <- "fdp@gf.org"
password.2 <- "gf2017**TkgrdRR3OXn2Olak6pcIDj694"
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
# Create data frame with object_v1 and field_v1 columns
obj.fields.2 <- data.frame()
for(i in seq_along(metadata.2)) {
      temp <- data.frame(object_v2 = rep(names(metadata.2)[i]),
                         field_v2 = metadata.2[[i]]$name)
      obj.fields.2 <- rbind(obj.fields.2, temp)
      rm(temp)
}

# MAP V1 TO V2
# Access google sheet
library(googlesheets)
library(dplyr)
my_sheets <- gs_ls()
template.info <- gs_title("V1 to V2 mapping")
# Load objects and fields to google sheet
gs_edit_cells(template.info, ws = "V1", input = obj.fields.1, col_names = TRUE, trim = TRUE)
gs_edit_cells(template.info, ws = "V2", input = obj.fields.2, col_names = TRUE)


# 2. Extract data from objects_v1 and structure as in objects_v2 --------------------

# Extract data from objects_v1
data.1 <- list()
for(i in seq_along(metadata.1)) {
            data.1[[i]] <- rforcecom.retrieve(session.1, names(metadata.1)[[i]],
                                              metadata.1[[i]]$name)
            names(data.1)[[i]] <- names(metadata.1)[[i]]
}
