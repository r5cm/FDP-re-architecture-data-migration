
# 1. Map objects and fields from objects_v1 to objects_v2 ---------------------------

# Login to Salesforce admin
library(RForcecom)
username <- "admin@utzmars.org"
password <- "gfutzmars2018n0ljYwQQqYVWfu9RIfPqWIn8"
session <- rforcecom.login(username, password)

# List of objects to keep
fdp.objects <- c("FDP_submission__c", "farmer__c", "farmer_BL__c", "Farm__c", 
                 "Farm_BL__c", "plot__c", "AODiagnostic__c", "Adoption_observations__c",
                 "AOFU__c", "PL_FDP__c", "FDP_calendar__c")
# Retrieve objects meta-data
metadata <- list()
for(i in seq_along(fdp.objects)) {
      metadata[[i]] <- rforcecom.getObjectDescription(session, fdp.objects[i])
      names(metadata)[i] <- fdp.objects[i]
}

# Retrieve objects data
data <- list()