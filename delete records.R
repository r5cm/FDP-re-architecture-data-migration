# Login to Salesforce admin V2
library(RForcecom)
# username.2 <- "admin@utzmars.org.fdpv2"
# password.2 <- "gfutzmars2018Sssksxym22rKU4SlDQcM2vWMV"
# apiversion.2 <- "41.0"
# url.2 <- "https://taroworks-1410--fdpv2.cs58.cloudforce.com/"
# session.2 <- rforcecom.login(username.2, password.2, url.2)

username.2 <- "admin@utzmars.org"
password.2 <- "gfutzmars2018*hn5OC4tzSecOhgHKnUtZL05C"
session.2 <- rforcecom.login(username.1, password.1)

# Function to delete object
delete.object <- function(object) {
      # Retrieve data
      delete <- rforcecom.retrieve(session.2, object, "Id")
      # Run delete job
      job_info <- rforcecom.createBulkJob(session.2, 
                                          operation='delete',
                                          object= object)
      # Delete
      batches_info <- rforcecom.createBulkBatch(session.2, 
                                                jobId=job_info$id, 
                                                delete,
                                                multiBatch = TRUE, 
                                                batchSize=500)
      Sys.sleep(15)
      # check on status of each batch
      batches_status <- lapply(batches_info, 
                               FUN=function(x){
                                     rforcecom.checkBatchStatus(session.2, 
                                                                jobId=x$jobId, 
                                                                batchId=x$id)
                               })
      print(batches_status)
      # get details on each batch
      batches_detail <- lapply(batches_info, 
                               FUN=function(x){
                                     rforcecom.getBatchDetails(session.2, 
                                                               jobId=x$jobId, 
                                                               batchId=x$id)
                               })
      print(batches_detail)
      # Close job
      close_job_info <- rforcecom.closeBulkJob(session.2, jobId=job_info$id)
}

# Delete single object
object.to.del <- "fpd_observation__c"
delete.object(object.to.del)

# Delete list of objects
objects.to.del <- c("fpd_observation__c", "fdp_Diagnostic_Monitoring__c",
                    "fdp_Family_members__c", "fdp_farmer_BL__c", "fdp_plot__c", 
                    "fdp_Farm_BL__c", "fdp_Farm__c", "fdp_farmer__c", "fpd_Submission__c")
for(i in seq_along(objects.to.del)) {
      delete.object(objects.to.del[i])
}


