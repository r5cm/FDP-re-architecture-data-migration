# FDP-re-architecture-data-migration
This project will generate activity 2 of the following list.

## FDP migration activities
1. All data synced on app_v1 to objects_v1, all field activities stop
2. Migrate data from objects_v1 to objects_v2
3. Sync app_v2 to retrieve data from objects_v2
4. Fill data on app_v2 and sync to objects_v2

## Phases
1. Beta users: small group of FCs
2. Rest of users

## Activity 2 strategy
1. Map objects and fields from objects_v1 to objects_v2
2. Extract data from objects_v1 and structure as in objects_v2
3. Extract data from submission_v1 and structure as in submission_v2 (TBD)
4. Create relationship between objects_v2 and submission_v2
5. Clean data
6. Load submission_v2 data to Salesforce
7. Add Salesforce relationship of objects_v2 to submissions_v2
8. Load objects_v2 to Salesforce
9. Use reports and dashboards to validate data was uploaded correctly

