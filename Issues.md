#ISSUES

#DATA ISSUES
 *Cuba --- cuba data source is currently undergoing maitenance, unsure when will resume.. some small update datasets, that may/may not want to account for...their data goes to Sept 20/
 *Turkey -- Note that Turkey stopped updating subregional numbers in 2020 in the EU WHO dataset. Need to filter this out, or find a new source.
 *Germany --- due to way that the file is updated on the server, not all data may be avaialable at a particular time (e.g. 12:05 pm EST 07/30/2021 -- download contains data for just 2 regions, but all regions there by 1:30pm)... choosing the right timing to update, or updating a saved file from previous update may be the way to go here...
 *Switzerland -- Appenzell Auserhoden - stopped releasing data via canton website. Consider changing resource to the official data source: https://www.covid19.admin.ch/api/data/context
Probably this type of link: https://www.covid19.admin.ch/api/data/20210729-matmggrq/sources/COVID19Cases_geoRegion.csv
 *Algeria --- the API seems to not have updated for a while until very recently. The number of days to calculate 10-day cases may be off. Additionally case numbers are v. low. Be careful. Population estimates are from 2008, long time ago.
 *Afghanistan -- unsure how well supported this dataset will be given the current situation.
 
#CODING/MAPPING
 *Netherlands -- geojson is not quite correct due to municipality redistricting in Jan 2021. Tilburg should fill the empty space.
  
#Future incorporation
 *Turkey -- potentially could scrape cases from here and convert: https://covid19.saglik.gov.tr/
 * Haiti -- https://docs.google.com/spreadsheets/d/10YxLT870MwYJ3Tm_a3WvvU2r1zQbT5F20TSXzw03BxQ/edit#gid=1307875485  or https://github.com/covid19datahub/COVID19/blob/master/R/ds_humdata.org.R . Appears to be updated only intermittently.

