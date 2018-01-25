
# 0. Login to Salesforce instances --------------------------------------------------

# Login to Salesforce admin V1
library(RForcecom)
username.1 <- "admin@utzmars.org"
password.1 <- "gfutzmars2018n0ljYwQQqYVWfu9RIfPqWIn8"
session.1 <- rforcecom.login(username.1, password.1)

# Login to Salesforce admin V2
username.2 <- "admin@utzmars.org.fdpv2"
password.2 <- "gfutzmars2018Sssksxym22rKU4SlDQcM2vWMV"
apiversion.2 <- "41.0"
url.2 <- "https://taroworks-1410--fdpv2.cs58.cloudforce.com/"
session.2 <- rforcecom.login(username.2, password.2, url.2)

# Remove unused objects
rm(username.1, password.1, username.2, password.2, apiversion.2, url.2)

# 1. Map objects and fields from objects_v1 to objects_v2 ---------------------------

# RETRIEVE OBJECTS AND FIELDS V1

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


# 2. Extract first batch of data (no references) ------------------------------------


# Retrieve v2 to v1 mapping table
map.v2v1 <- data.frame(gs_read(template.info, "V2", "A1:H329"))
# Remove formula fields from map.v2v1
map.v2v1 <- map.v2v1[map.v2v1$calc_v2 == FALSE, ]
# Remove fields that won't be loaded (N/A in field_v1 column)
map.v2v1 <- map.v2v1[map.v2v1$field_v1 != "N/A", ]
# Remove fields that are empty in v1
map.v2v1 <- map.v2v1[map.v2v1$empty_v1 == "no", ]

# Vector with objects to load
obj.2 <- c("Submission__c", "fdp_farmer__c", "fdp_farmer_BL__c", "fdp_Farm__c",
           "fdp_Farm_BL__c", "fdp_plot__c", "fdp_Diagnostic_Monitoring__c", 
           "observation__c")
data.2 <- list()
fields.ret <- c("object_v2", "field_v2", "type_v2", "object_v1", "field_v1")

# Retrieve corresponding data from V1 for each object_V2 and field_V2
for(i in 1:length(obj.2)) {
      map.temp <- map.v2v1[map.v2v1$object_v2 == obj.2[i], fields.ret]
      # Create empty column for reference fields
      query.temp <-  paste("SELECT", map.temp$field_v1[1], "FROM", fdp.objects.1[i],
                           "WHERE test_mig2__c = 1", sep = " ")
      empty.temp <- rep(NA, nrow(rforcecom.query(session.1, query.temp)))
      # For each field_v2 assign data from corresponding field_v1
      for(j in 1:nrow(map.temp)) {
            if(map.temp$type_v2[j] != "reference") {
                  query.temp <- paste("SELECT", map.temp$field_v1[j], "FROM",
                                      fdp.objects.1[i],
                                      "WHERE test_mig2__c = 1 ORDER BY Id", sep = " ")
                  data.temp <- rforcecom.query(session.1, query.temp)
                  if(length(data.temp) > 1) {data.temp <- select(data.temp,
                                                                 map.temp$field_v1[j])}
            } else {
                  data.temp <- data.frame(empty.temp)
            }
            names(data.temp)[1] <- map.temp$field_v2[j]
            if(j == 1) {
                  data.2[[i]] <- data.temp
            } else {
                  data.2[[i]] <- data.frame(data.2[[i]], data.temp)
            }
      }
      names(data.2)[i] <- obj.2[i]
      rm(map.temp, data.temp, query.temp)
}

# Save data to be retrieved in next section
saveRDS(data.2, "data_2.RDS")

# 3. Restructure and load -----------------------------------------------------------

# Retrieve data to load
data.2 <- readRDS("data_2.RDS")

# COUNTRY
# TBD
# ...
# DISTRICTS
# TBD
# ...


# FAMILY MEMBERS

# Create spouse data frame
spouse <- select(data.2$fdp_farmer__c, Id_v1__c, spouseBirthday__c,
                 spouseEducationalLevel__c, spouseName__c)
# nombres inventados, completar cuando esté creado
names(spouse) <- c("Id_v1__c", "Birthdate__c", "educ__c", "Name__c")
# Generic family member financials
fm.name <- rep("Generic member", nrow(spouse))
fm.name <- data.frame(fm.name) #include financials
# Create Family members data frame (Pending creation of)

# Remove family members fields from farmer__c
rm.fields <- c("spouseBirthday__c", "spouseEducationalLevel__c", "spouseName__c")
data.2$fdp_farmer__c <- select(data.2$fdp_farmer__c, -one_of(rm.fields))

# Add family members to data.2


# FARMER(ALL fields except submission)

# Organization: all to Mars
data.2$fdp_farmer__c$organization__c <- "a0l0l000000FY1M"
# Village: all to Test Village while villages are created
data.2$fdp_farmer__c$village__c <- "a0k0l000004kmlv"
# Run insert job
job_info <- rforcecom.createBulkJob(session.2, 
                                    operation='insert', 
                                    object='fdp_farmer__c')
# Insert
batches_info <- rforcecom.createBulkBatch(session.2, 
                                          jobId=job_info$id, 
                                          select(data.2$fdp_farmer__c[test.rec, ],
                                                 -one_of(new.empty)),
                                          multiBatch = TRUE, 
                                          batchSize=500)
# check on status of each batch
batches_status <- lapply(batches_info, 
                         FUN=function(x){
                               rforcecom.checkBatchStatus(session.2, 
                                                          jobId=x$jobId, 
                                                          batchId=x$id)
                         })
# get details on each batch
batches_detail <- lapply(batches_info, 
                         FUN=function(x){
                               rforcecom.getBatchDetails(session.2, 
                                                         jobId=x$jobId, 
                                                         batchId=x$id)
                         })
# Close job
close_job_info <- rforcecom.closeBulkJob(session.2, jobId=job_info$id)

# SUBMISSION
# Submissions(status) - CONFIRM WITH AAN
# Submissions(farmer)
# Load submissions

# FARMER
# Farmer(submission)
subm.12 <- rforcecom.retrieve(session.2, "Submission__c", c("Id", "Id_v1__c"))
data.2$fdp_farmer__c$FDP_submission__c <- left_join(data.2$fdp_farmer__c, subm.12,
                                                    by = "Id_v1__c")[, "FPD_submission__c"]
# Farmer(organization)
# Farmer(status = Active)

# AO DIAGNOSTIC

# Type (all = 'Diagnostic')

