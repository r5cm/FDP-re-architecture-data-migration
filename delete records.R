object <- "fdp_plot__c"
delete <- rforcecom.retrieve(session.2, object, "Id")

# Run delete job
job_info <- rforcecom.createBulkJob(session.2, 
                                    operation='delete',
                                    object= object)
# Insert
batches_info <- rforcecom.createBulkBatch(session.2, 
                                          jobId=job_info$id, 
                                          delete,
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
