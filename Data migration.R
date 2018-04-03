# 0. Login to Salesforce instances and gsheets --------------------------------------

# Login to Salesforce admin V1
library(RForcecom)
username.1 <- "admin@utzmars.org"
password.1 <- "gfutzmars2018*hn5OC4tzSecOhgHKnUtZL05C"
session.1 <- rforcecom.login(username.1, password.1)

# Login to Salesforce admin V2
username.2 <- "admin@utzmars.org.fdpv2"
password.2 <- "gfutzmars2018**pwUFl8FEl9ojmpSGjqeMy62K"
apiversion.2 <- "41.0"
url.2 <- "https://taroworks-1410--fdpv2.cs58.cloudforce.com/"
session.2 <- rforcecom.login(username.2, password.2, url.2)

# Access google sheet
library(googlesheets)
library(dplyr)
my_sheets <- gs_ls()
template.info <- gs_title("V1 to V2 mapping")

# Remove unused objects
rm(username.1, password.1, username.2, password.2, apiversion.2, url.2)

# 1. Map objects and fields from objects_v1 to objects_v2 ---------------------------

# RETRIEVE OBJECTS AND FIELDS V1

# List of objects_V1 to migrate
fdp.objects.1 <- c("FDP_submission__c", "farmer__c", "farmer_BL__c", "Farm__c", 
                   "Farm_BL__c", "plot__c", "AODiagnostic__c", "Adoption_observations__c",
                   "User_performance__c", "Performance_detail__c")
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
fdp.objects.2 <- c("fdp_Submission__c", "Answer__c", "fdp_farmer__c", "fdp_farmer_BL__c",
                   "fdp_Farm__c", "fdp_Farm_BL__c", "fdp_plot__c",
                   "fdp_Diagnostic_Monitoring__c", "fdp_PL__c", "fdp_Calendar__c",
                   "fpd_observation__c", "fpd_User_performance__c",
                   "fdp_Performance_detail__c")
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
# Load objects and fields to google sheet (WARNING!!!!! ONLY DO THIS ONCE!!!!!!!!)
gs_edit_cells(template.info, ws = "V1", input = obj.fields.1, col_names = TRUE, trim = TRUE)
gs_edit_cells(template.info, ws = "V2", input = obj.fields.2, col_names = TRUE)

# Map objects_v1 to objects_v2 -> Done in google sheets edited above



# 2. Extract first batch of data (no references) ------------------------------------


# Retrieve v2 to v1 mapping table
map.v2v1 <- data.frame(gs_read(template.info, "V2", "A1:I364"))
# Remove formula fields from map.v2v1
map.v2v1 <- map.v2v1[map.v2v1$calc_v2 == FALSE, ]
# Remove fields that won't be loaded (N/A in field_v1 column)
map.v2v1 <- map.v2v1[map.v2v1$field_v1 != "N/A", ]
# Remove fields that are empty in v1
map.v2v1 <- map.v2v1[map.v2v1$empty_v1 == "no", ]

# Vector with objects to load
obj.2 <- c("fpd_Submission__c", "fdp_farmer__c", "fdp_farmer_BL__c", "fdp_Farm__c",
           "fdp_Farm_BL__c", "fdp_plot__c", "fdp_Diagnostic_Monitoring__c", 
           "fpd_observation__c", "fpd_User_performance__c", "fdp_Performance_detail__c")
data.2 <- list()
fields.ret <- c("object_v2", "field_v2", "type_v2", "rclass_v2", "object_v1", "field_v1")

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
                  # class(data.temp[, 1]) <- map.temp$rclass_v2[j]
                  if(length(data.temp) > 1) {
                        data.temp <- select(data.temp, map.temp$field_v1[j])
                  }
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

# save data
saveRDS(data.2, "data.2_200_import.RDS")



# 3. Clean data -------------------------------------------------------------------

# Check that there are no repeated records (then manually delete repeated in SF)
check.rep <- c("fpd_Submission__c", "fdp_farmer__c", "fdp_farmer_BL__c", "fdp_Farm__c",
               "fdp_Farm_BL__c")
for(i in seq_along(check.rep)) {
      temp.dup <- duplicated(data.2[[i]]$farmer.code)
      if(sum(temp.dup) > 1) {
            print(paste("DUPLICATED records in ", check.rep[i], ":", sep = ""))
            data.2[[i]]$farmer.code[temp.dup]
      } else {
            print(paste("NO DUPLICATED records in ", check.rep[i]))
      }
}

# SELECT MARS FARMERS
mars.farmers <- read.csv("Mars Indo farmers.csv")
# Select farmers
for(i in seq_along(data.2)) {
      if(obj.2[i] != "fdp_farmer__c") {
            print(obj.2[i])
            print(nrow(data.2[[i]]))
            data.2[[i]] <- data.2[[i]][data.2[[i]]$farmer.code %in%
                                             mars.farmers$farmer.code, ]
            print(nrow(data.2[[i]]))
      } else {
            data.2[[i]] <- data.2[[i]][data.2[[i]]$farmerCode__c %in%
                                             mars.farmers$farmer.code, ]
      }
}
# Remove farmer.code variable
for(i in seq_along(data.2)) {
      if(obj.2[i] != "fdp_farmer__c") {
            data.2[[i]] <- select(data.2[[i]], -farmer.code)
      }
}

# Save data to be retrieved in next section
saveRDS(data.2, "data_2_200.RDS")

# 4. Transform and load -----------------------------------------------------------

# Retrieve data to load
data.2 <- readRDS("data_2.RDS")


# FAMILY MEMBERS (TRANSFORM)

# Spouse
# Create family members data frame for all spouses
family.members <- select(data.2$fdp_farmer__c, Id_v1__c, spouseName__c, gender__c,
                         spouseBirthday__c, spouseEducationalLevel__c, Spouse_income__c,
                         have_spouse_indic, FDP_submission_v1__c)
family.members <- family.members[family.members$have_spouse_indic != "0.0", ]
family.members <- select(family.members, -have_spouse_indic)
# Add missing variables that will be loaded
num.fm <- rep(NA, nrow(family.members))
relationship.fm <- rep("Husband/wife", nrow(family.members))
family.members <- data.frame(family.members, num.fm, num.fm, relationship.fm)
fm.names <- c("Id_farmer_v1__c", "Name__c", "Gender__c", "Year_of_birth__c", 
              "Education_level__c", "Income_contribute__c",
              "FDP_submission_v1__c", "Work_on_farm_without_compensation__c",
              "Depend_on_income__c", "Relationship_with_head_of_household__c")
names(family.members) <- fm.names
# Check that all values in gender are either Pria, Wanita or null
table(data.2$fdp_farmer__c$gender__c, useNA = 'always')
data.2$fdp_farmer__c$gender__c <- as.character(data.2$fdp_farmer__c$gender__c)
data.2$fdp_farmer__c$gender__c[data.2$fdp_farmer__c$gender__c == "N/A"] <- NA
# Gender of spouse is opposite of farmer (most likely!)
family.members$Gender__c <- with(family.members, 
                                 ifelse(Gender__c == "Pria", "Wanita",
                                        ifelse(Gender__c == "Wanita", "Pria", NA)))

# # Farmer as family member
# farmers.null <- rep(NA, nrow(data.2$fdp_farmer__c))
# farmer.fm <- with(data.2$fdp_farmer__c,
#                   data.frame(Id_v1__c, fullName__c, gender__c, birthday__c, 
#                              educationalLevel__c, farmers.null, FDP_submission_v1__c,
#                              farmers.null, farmers.null, farmers.null))
# names(farmer.fm) <- fm.names
# family.members <- rbind(family.members, farmer.fm)

# Generic member to family members data frame
# Check that all spouse indicators are not empty
table(data.2$fdp_farmer__c$have_spouse_indic, useNA = 'always')
# Check that all family members are not empty
table(data.2$fdp_farmer__c$Family_members__c, useNA = 'always')
data.2$fdp_farmer__c$farmerCode__c[is.na(data.2$fdp_farmer__c$Family_members__c)]
# Check that all with spouse should have at least two family members
test.fm <- with(data.2$fdp_farmer__c,
                have_spouse_indic == "1.0" & as.numeric(as.character(Family_members__c)) < 2)
sum(test.fm)
data.2$fdp_farmer__c$Family_members__c <- as.character(data.2$fdp_farmer__c$Family_members__c)
data.2$fdp_farmer__c$Family_members__c[test.fm] <- "2"
# Replace all 0 family members with 1 (the farmer)
fm.0 <- data.2$fdp_farmer__c$Family_members__c == "0.0"
data.2$fdp_farmer__c$Family_members__c[fm.0] <- "1.0"
data.2$fdp_farmer__c$Family_members__c <- as.factor(data.2$fdp_farmer__c$Family_members__c)
# For each record in farmer,
fm.name <- "Generic member FDP V1"
for(i in 1:nrow(data.2$fdp_farmer__c)) {
      # Get farmer id, family members, income per capita, hh income and members 
      # w/out compensation.
      record.i <- data.2$fdp_farmer__c[i, ]
      fr.farmer.id <- record.i$Id_v1__c
      fr.family.members <- ifelse(as.numeric(as.character(record.i$have_spouse_indic)) > 0,
                                  as.numeric(as.character(record.i$Family_members__c)) - 1,
                                  as.numeric(as.character(record.i$Family_members__c)))
      fr.income.pc <- as.numeric(as.character(record.i$Income_contribute_by_members__c)) /
            fr.family.members
      fr.depend.hh <- as.numeric(as.character(record.i$Depend_on_hh_income_avg__c))
      fr.no.compens <- as.numeric(as.character(record.i$Family_members_work_on_farm_mig__c))
      fr.no.compens <- ifelse(is.na(fr.no.compens), 0, fr.no.compens)
      # For each family member
      for(j in 1:(fr.family.members)) {
            # Add depend on hh income (based on # depend on hh income)
            if(fr.depend.hh > 0) {
                   fm.depend.hh <- "Ya"
                   fr.depend.hh = fr.depend.hh - 1
            } else {
                   fm.depend.hh <- "Tidak"
            }
            # Add work on farm w/out compensation (based on # work w/out comp)
            if(fr.no.compens > 0) {
                  fm.no.compens <- "Ya"
                  fr.no.compens = fr.no.compens - 1
            } else {
                  fm.no.compens <- "Tidak"
            }
            # Create observation with depend_inc, wout_compens, farmer_id and
            # income_contribute, add observation to fm data frame
            temp.record <- data.frame(Id_farmer_v1__c = fr.farmer.id,
                                      Name__c = fm.name,
                                      Gender__c = NA,
                                      Year_of_birth__c = NA, 
                                      Education_level__c = NA,
                                      Income_contribute__c = as.factor(fr.income.pc),
                                      FDP_submission_v1__c = record.i$FDP_submission_v1__c,
                                      Work_on_farm_without_compensation__c = fm.no.compens, 
                                      Depend_on_income__c = fm.depend.hh,
                                      Relationship_with_head_of_household__c = NA)
            family.members <- rbind(family.members, temp.record, stringsAsFactors = FALSE)
      }
      rm(fr.farmer.id, fr.family.members, fr.income.pc, fr.depend.hh, fr.no.compens,
         fm.depend.hh, fm.no.compens, temp.record)
}
# Validation: FMs created = sum of farmers' FMs (must return TRUE)
sum(as.numeric(as.character(data.2$fdp_farmer__c$Family_members__c))) == nrow(family.members)

# Remove family members fields from Farmer
rm.farmer <- c("spouseBirthday__c", "spouseEducationalLevel__c", "spouseName__c",
               "Spouse_income__c", "have_spouse_indic", "Spouse_income__c",
               "Depend_on_hh_income_avg__c", "Income_contribute_by_members__c",
               "Family_members_work_on_farm_mig__c")
data.2$fdp_farmer__c <- select(data.2$fdp_farmer__c, -one_of(rm.farmer))
# Remove family members fields from Farmer Baseline
rm.farmerbl <- c("dependEconomically__c", "familyMembersPaidWork__c",
                 "receivesPaymentFarmLabor__c", "spouseIncome__c", "Have_spouse__c",
                 "spouseHavePaidWork__c", "familyMembersIncome__c")
data.2$fdp_farmer_BL__c <- select(data.2$fdp_farmer_BL__c, -one_of(rm.farmerbl))
# Trim year of birth, leave year only
family.members$Year_of_birth__c <- substr(family.members$Year_of_birth__c, 1, 4)
# Add family members to data.2
data.2[[length(data.2) + 1]] <- family.members
names(data.2)[length(data.2)] <- "fdp_Family_members__c"


# FARMER(ALL fields except submission)

# Children questions (moved from Farmer Baseline)
temp.children <- select(data.2$fdp_farmer_BL__c, farmer_v1__c, Number_of_children__c,
                        under17__c, under17InSchool__c)
names(temp.children) <- c("farmer_v1__c", "Number_of_children__c",
                          "Children_between_5_17__c", "Children_5_17_enrolled_in_school__c")
data.2$fdp_farmer__c <- left_join(data.2$fdp_farmer__c, temp.children,
                                  by = c("Id_v1__c" = "farmer_v1__c"))
rm(temp.children)
# Organization: all to Mars (UPDATE FOR PRODUCTION INSTANCE)
data.2$fdp_farmer__c$organization__c <- "a1M0l0000002FbKEAU"

# Village:
id.villages.2 <- rforcecom.retrieve(session.2, "fpd_village__c", c("Id", "Name"))
names(id.villages.2) <- c("Id.village.2", "Name")
# Check that there are no repeated names
if(sum(duplicated(id.villages.2$Name)) != 0) warning("There are repeated villages")
# Add villages
temp.villages <- left_join(data.2$fdp_farmer__c, id.villages.2, 
                           by = c("village_v1__c" = "Name"))
data.2$fdp_farmer__c$village__c <- temp.villages$Id.village.2
rm(temp.villages)
      
# Status
data.2$fdp_farmer__c$status__c <- "Active"
# User (only for test, delete for production migration, already contained in data.2)
data.2$fdp_farmer__c$OwnerId <- "00528000003lvpw"

# Run insert job
job_info <- rforcecom.createBulkJob(session.2, 
                                    operation='insert',
                                    object='fdp_farmer__c')
# Insert
batches_info <- rforcecom.createBulkBatch(session.2, 
                                          jobId=job_info$id, 
                                          data.2$fdp_farmer__c,
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

# Status
data.2$fpd_Submission__c$Status__c <- "Agreed"
# farmer
id.farmer.2 <- rforcecom.retrieve(session.2, "fdp_farmer__c", c("Id", "Id_v1__c"))
names(id.farmer.2) <- c("Id.farmer.2", "Id.farmer.1")
library(dplyr)
temp.sub <- left_join(data.2$fpd_Submission__c, id.farmer.2, 
                      by = c("Respondent_v1__c" = "Id.farmer.1"))
data.2$fpd_Submission__c$Respondent__c <- temp.sub$Id.farmer.2
rm(temp.sub)
# surveyor: DELETE THIS PART WHEN IN PRODUCTION INSTANCE
data.2$fpd_Submission__c$Surveyor__c <- "0050K000007ts87QAA"

# Load submissions
# Run insert job
job_info <- rforcecom.createBulkJob(session.2, 
                                    operation='insert',
                                    object='fpd_Submission__c')
# Insert
batches_info <- rforcecom.createBulkBatch(session.2, 
                                          jobId=job_info$id, 
                                          data.2$fpd_Submission__c,
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


# FARMER (II)

# submission
id.subm.2 <- rforcecom.retrieve(session.2, "fpd_Submission__c", 
                                c("Id", "Id_v1__c", "Respondent__c"))
names(id.subm.2) <- c("Id.subm.2", "Id.subm.1", "Respondent__c")
# temp.farmersub <- left_join(data.2$fdp_farmer__c, id.subm.2,
#                             by = c("FDP_submission_v1__c" = "Id.subm.1"))
# data.2$fdp_farmer__c$FDP_submission__c <- temp.farmersub$Id.subm.2
# rm(temp.farmersub)

# data to update
farmers.update <- left_join(id.farmer.2, id.subm.2, 
                            by = c("Id.farmer.2" = "Respondent__c"))
farmers.update <- select(farmers.update, Id.farmer.2, Id.subm.2)
names(farmers.update) <- c("Id", "FDP_submission__c")

# update farmers with submission
# Run update job
job_info <- rforcecom.createBulkJob(session.2, 
                                    operation='update',
                                    object='fdp_farmer__c')
# Insert
batches_info <- rforcecom.createBulkBatch(session.2, 
                                          jobId=job_info$id, 
                                          farmers.update,
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


# FAMILY MEMBERS (LOAD)

# Submission and farmer
temp.fmsub <- left_join(family.members, id.subm.2, 
                        by = c("FDP_submission_v1__c" = "Id.subm.1"))
data.2$fdp_Family_members__c <- data.frame(data.2$fdp_Family_members__c,
                                           FDP_Submission__c = temp.fmsub$Id.subm.2,
                                           Farmer__c = temp.fmsub$Respondent__c)

# Load family members
# Run insert job
job_info <- rforcecom.createBulkJob(session.2, 
                                    operation='insert',
                                    object='fdp_Family_members__c')
# Insert
batches_info <- rforcecom.createBulkBatch(session.2, 
                                          jobId=job_info$id, 
                                          data.2$fdp_Family_members__c,
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

      
# FARM - Farmer and submission

# Prepare farms to load
# Submission
temp.farmsub <- left_join(data.2$fdp_Farm__c, id.subm.2, 
                          by = c("FDP_submission_v1__c" = "Id.subm.1"))
data.2$fdp_Farm__c$FDP_submission__c <- temp.farmsub$Id.subm.2
rm(temp.farmsub)
# Farmer
temp.farmfarmer <- left_join(data.2$fdp_Farm__c, id.farmer.2,
                             by = c("farmer_v1__c" = "Id.farmer.1"))
data.2$fdp_Farm__c$farmer__c <- temp.farmfarmer$Id.farmer.2
# Village (NUEVO: ESTO NO SE HA PROBADO)
temp.villages <- left_join(data.2$fdp_Farm__c, id.villages.2, 
                           by = c("village_v1__c" = "Name"))
data.2$fdp_Farm__c$village__c <- temp.villages$Id.village.2
rm(temp.villages)
# Load farms
# Run insert job
job_info <- rforcecom.createBulkJob(session.2, 
                                    operation='insert',
                                    object='fdp_Farm__c')
# Insert
batches_info <- rforcecom.createBulkBatch(session.2, 
                                          jobId=job_info$id, 
                                          data.2$fdp_Farm__c,
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


# FARMER (III) - update farm

# Prepare farmers to update
id.farm.2 <- rforcecom.retrieve(session.2, "fdp_Farm__c", c("Id", "Id_v1__c", "farmer__c"))
names(id.farm.2) <- c("Id.farm.2", "Id.farm.1", "Id.farmer.2")
temp.farmerfarm <- left_join(id.farmer.2, id.farm.2, by =  "Id.farmer.2")
farmers.update2 <- select(temp.farmerfarm, Id.farmer.2, Id.farm.2)
names(farmers.update2) <- c("Id", "FDP_Farm__c")
# Update farmers
# Run update job
job_info <- rforcecom.createBulkJob(session.2, 
                                    operation='update',
                                    object='fdp_farmer__c')
# Insert
batches_info <- rforcecom.createBulkBatch(session.2, 
                                          jobId=job_info$id, 
                                          farmers.update2,
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


# FARMER BASELINE

# farmer
temp.fblfarmer <- left_join(data.2$fdp_farmer_BL__c, id.farmer.2,
                            by =c("farmer_v1__c" = "Id.farmer.1"))
data.2$fdp_farmer_BL__c$farmer__c <- temp.fblfarmer$Id.farmer.2
rm(temp.fblfarmer)
# submission
temp.fblsub <- left_join(data.2$fdp_farmer_BL__c, id.subm.2,
                         by = c("FDP_submission_v1__c" = "Id.subm.1"))
data.2$fdp_farmer_BL__c$FDP_submission__c <- temp.fblsub$Id.subm.2
rm(temp.fblsub)

# load farmer baselines
# Run insert job
job_info <- rforcecom.createBulkJob(session.2, 
                                    operation='insert',
                                    object='fdp_farmer_BL__c')
# Insert
batches_info <- rforcecom.createBulkBatch(session.2, 
                                          jobId=job_info$id, 
                                          data.2$fdp_farmer_BL__c,
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


# FAMILY MEMBER (Farmer baseline lookup) - HOLD UNTIL REVIEW WITH JULIAN


# FARM BASELINE

# farm
temp.farmblfarm <- left_join(data.2$fdp_Farm_BL__c, id.farm.2,
                             by = c("farm_v1__c" = "Id.farm.1"))
data.2$fdp_Farm_BL__c$farm__c <- temp.farmblfarm$Id.farm.2
rm(temp.farmblfarm)
# submission
temp.farmsub <- left_join(data.2$fdp_Farm_BL__c, id.subm.2,
                          by = c("FDP_submission_v1__c" = "Id.subm.1"))
data.2$fdp_Farm_BL__c$FDP_submission__c <- temp.farmsub$Id.subm.2
rm(temp.farmsub)
# Load farm baselines
# Run insert job
job_info <- rforcecom.createBulkJob(session.2, 
                                    operation='insert',
                                    object='fdp_Farm_BL__c')
# Insert
batches_info <- rforcecom.createBulkBatch(session.2, 
                                          jobId=job_info$id, 
                                          data.2$fdp_Farm_BL__c,
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


# PLOT

# farm
temp.plotfarm <- left_join(data.2$fdp_plot__c, id.farm.2, 
                           by = c("Farm_v1__c" = "Id.farm.1"))
data.2$fdp_plot__c$Farm__c <- temp.plotfarm$Id.farm.2
rm(temp.plotfarm)
# farm baseline
id.farmbl.2 <- rforcecom.retrieve(session.2, "fdp_Farm_BL__c", c("Id", "Id_v1__c"))
names(id.farmbl.2) <- c("Id.farmbl.2", "Id.farmbl.1")
temp.plotfarmbl <- left_join(data.2$fdp_plot__c, id.farmbl.2,
                             by = c("farmBL_v1__c" = "Id.farmbl.1"))
data.2$fdp_plot__c$farmBL__c <- temp.plotfarmbl$Id.farmbl.2
rm(temp.plotfarmbl)
# submission
temp.plotsubm <- left_join(data.2$fdp_plot__c, id.subm.2,
                           by = c("FDP_submission_v1__c" = "Id.subm.1"))
data.2$fdp_plot__c$FDP_submission__c <- temp.plotsubm$Id.subm.2
rm(temp.plotsubm)
# remove distanceBetweenCocoaTrees__c field
data.2$fdp_plot__c <- select(data.2$fdp_plot__c, -distanceBetweenCocoaTrees__c)
# Insert plots
# Run insert job
job_info <- rforcecom.createBulkJob(session.2, 
                                    operation='insert',
                                    object='fdp_plot__c')
# Insert
batches_info <- rforcecom.createBulkBatch(session.2, 
                                          jobId=job_info$id, 
                                          data.2$fdp_plot__c,
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


# DIAGNOSTIC/MONITORING

# Retrieve data saved up to this  point
data.2 <- readRDS("data.2_2Plot.RDS")

# Submission
temp.dmsubm <- left_join(data.2$fdp_Diagnostic_Monitoring__c, id.subm.2,
                         by = c("FDP_submission_v1__c" = "Id.subm.1"))
data.2$fdp_Diagnostic_Monitoring__c$FDP_submission__c <-  temp.dmsubm$Id.subm.2
rm(temp.dmsubm)
# Plot
id.plot.2 <- rforcecom.retrieve(session.2, "fdp_plot__c", c("Id", "Id_v1__c"))
names(id.plot.2) <- c("Id.plot.2", "Id.plot.1")
temp.dmplot <- left_join(data.2$fdp_Diagnostic_Monitoring__c, id.plot.2,
                         by = c("plot_v1__c" = "Id.plot.1"))
data.2$fdp_Diagnostic_Monitoring__c$plot__c <- temp.dmplot$Id.plot.2
rm(temp.dmplot)
# Type == diagnostic
data.2$fdp_Diagnostic_Monitoring__c$type__c <- "Diagnostic"
# Remove surveyor
data.2$fdp_Diagnostic_Monitoring__c <- select(data.2$fdp_Diagnostic_Monitoring__c, 
                                              -surveyor__c)

# Main recommendation
# Map reco.1 to reco.2
reco.1 <- rforcecom.retrieve(session.1, "recommendation__c", c("Id", "Name"))
reco.2 <- rforcecom.retrieve(session.2, "fpd_recommendation__c", 
                             c("Id", "Name", "Country__r.Name"))
reco.2  <- reco.2[reco.2$fpd_country__c.Name == "Indonesia", ]
id.reco.2 <- left_join(reco.2, reco.1, by = "Name")
names(id.reco.2) <- c("Id.reco.2", "Name", "country", "Id.reco.1")
# Add main recommendation
temp.reco <- left_join(data.2$fdp_Diagnostic_Monitoring__c, id.reco.2,
                       by = c("mainRecommendation_v1__c" = "Id.reco.1"))
data.2$fdp_Diagnostic_Monitoring__c$mainRecommendation__c <- temp.reco$Id.reco.2
rm(temp.reco)
# Insert Diagnostic/Monitorings
# Run insert job
job_info <- rforcecom.createBulkJob(session.2, 
                                    operation='insert',
                                    object='fdp_Diagnostic_Monitoring__c')
# Insert
batches_info <- rforcecom.createBulkBatch(session.2, 
                                          jobId=job_info$id, 
                                          data.2$fdp_Diagnostic_Monitoring__c,
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


# OBSERVATIONS

# Diagnostic/monitoring relationship
id.diag.mon <- rforcecom.retrieve(session.2, "fdp_Diagnostic_Monitoring__c",
                                  c("Id", "Id_v1__c"))
names(id.diag.mon) <- c("Id.diagm.2", "Id.diagm.1")
temp.obsdm <- left_join(data.2$fpd_observation__c, id.diag.mon, 
                         by = c("Diagnostic_Monitoring_v1__c" = "Id.diagm.1"))
data.2$fpd_observation__c$Diagnostic_Monitoring__c <- temp.obsdm$Id.diagm.2
rm(temp.obsdm)

# Add data of tree age and distance btn trees (in plot)
data.2$fpd_observation__c$Result__c <- as.character(data.2$fpd_observation__c$Result__c)
data.2$fpd_observation__c$aux_tree_age <- as.character(data.2$fpd_observation__c$aux_tree_age)
data.2$fpd_observation__c$aux_dist_trees <- as.character(data.2$fpd_observation__c$aux_dist_trees)
for(i in 1:nrow(data.2$fpd_observation__c)) {
      if(data.2$fpd_observation__c$Variable__c[i] == "Distance between trees") {
            data.2$fpd_observation__c$Result__c[i] <- data.2$fpd_observation__c$aux_dist_trees[i]
      } else if(data.2$fpd_observation__c$Variable__c[i] == "Tree age") {
            data.2$fpd_observation__c$Result__c[i] <- data.2$fpd_observation__c$aux_tree_age[i]
      }
}
# Remove aux values before loading
data.2$fpd_observation__c <- select(data.2$fpd_observation__c, 
                                    -one_of("aux_dist_trees", "aux_tree_age"))

# Insert observations
# Run insert job
job_info <- rforcecom.createBulkJob(session.2, 
                                    operation='insert',
                                    object='fpd_observation__c')
# Insert
batches_info <- rforcecom.createBulkBatch(session.2, 
                                          jobId=job_info$id, 
                                          data.2$fpd_observation__c,
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



# USER PERFORMANCE

# ONLY FOR TESTING!!!!!!!!!!1
data.2$fpd_User_performance__c$Assigned_to__c <- "00528000003lvpw"

# Run insert job
job_info <- rforcecom.createBulkJob(session.2, 
                                    operation='insert',
                                    object='fpd_User_performance__c')
# Insert
batches_info <- rforcecom.createBulkBatch(session.2, 
                                          jobId=job_info$id, 
                                          data.2$fpd_User_performance__c,
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


# PERFORMANCE DETAILS

# Filter performance details
# Login salesforce
password <- "lestari45PtkwZcuVt5G3DtiK39kq30MnI"
session.rep <- rforcecom.login("reports@utzmars.org", password)
# Retrieve performance details
fields <- c("Id", "Agree__c", "FDP_Submission__c", "createdDate", "Farmer_name__c",
            "Farmer_code__c", "User_performance__c", "User_performance__r.Activity__c",
            "User_performance__r.Performance__c", "User_performance__r.Mars_profile__c",
            "User_performance__r.Assigned_to_Name__c")
perf.det <- rforcecom.retrieve(session.rep, "Performance_detail__c", fields)
perf.det <- select(perf.det,
                   c("Id", "Agree__c", "FDP_submission__c", "CreatedDate", "Farmer_name__c",
                     "Farmer_code__c", "User_performance__c", "User_performance__c.Activity__c",
                     "User_performance__c.Performance__c", "User_performance__c.Mars_profile__c",
                     "User_performance__c.Assigned_to_Name__c"))
names(perf.det) <- c("Id", "agree", "submission", "createdDate", "farmerName", 
                     "farmerCode", "user.performance", "up.activity", "up.performance",
                     "up.profile", "user")
# Exclude monitoring activities AND performance==NA AND profile contains FC
perf.det <- perf.det[perf.det$up.activity == "FDP diagnostic" & 
                           !is.na(perf.det$up.performance) &
                           grepl("Field Coordinator", perf.det$up.profile), ]
# Filter performance details
data.2$fdp_Performance_detail__c <- 
      data.2$fdp_Performance_detail__c[data.2$fdp_Performance_detail__c$Id_v1__c
                                       %in% perf.det$Id, ]

# Submission (REVISAR POR QUÉ HAY SUBMISSIONS DUPLICADOS)
id.subm.2 <- id.subm.2[!duplicated(id.subm.2$Id.subm.1), ]
temp.pdSub <- left_join(data.2$fdp_Performance_detail__c, id.subm.2,
                        by = c("Submission_v1__c" = "Id.subm.1"))
data.2$fdp_Performance_detail__c$Submission__c <- temp.pdSub$Id.subm.2

# Performance details
id.up <- rforcecom.retrieve(session.2, "fpd_User_performance__c", c("Id", "Id_v1__c"))
names(id.up) <- c("Id.up.2", "Id.up.1")
temp.upSub <- left_join(data.2$fdp_Performance_detail__c, id.up,
                        by = c("User_performance_v1__c" = "Id.up.1"))
data.2$fdp_Performance_detail__c$User_performance__c <- temp.upSub$Id.up.2

# Load performance details
# Run insert job
job_info <- rforcecom.createBulkJob(session.2, 
                                    operation='insert',
                                    object='fdp_Performance_detail__c')
# Insert
batches_info <- rforcecom.createBulkBatch(session.2, 
                                          jobId=job_info$id, 
                                          select(data.2$fdp_Performance_detail__c,
                                                 -one_of(c("Id_v1__c",
                                                           "Submission_v1__c",
                                                           "User_performance_v1__c"))),
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


