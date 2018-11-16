# Call prerequisite libraries
library(RAdwords)
library(tidyverse) #for cleaner, high level code syntax
library(googledrive)
library(googlesheets) #for importing and exporting to worksheet

# note that prior to running this code, you will need to provide the google_auth credentials in a file called "credentials.RData" in the main directory

worst.arm.chance <- function(clicks=c(2284,663,2016,286,67),impressions=c(30888,7708,21638,3976,1233),N=3e5){
  beta<-data.frame(clicks=clicks,impressions=impressions)
  a <-beta$clicks
  b <- beta$impressions-beta$clicks
  ind_min <- which.max(-beta$impressions)
  probs_min <- log(rbeta(N,beta$clicks[ind_min]+1,beta$impressions[ind_min]-beta$clicks[ind_min]+1))
  beta <- beta[-ind_min,]
  sumprobs <- probs_min; sumnom <- probs_min
  
  for(i in 1:dim(beta)[1]){
    temp<-log(rbeta(N,beta$clicks[i]+1,beta$impressions[i]-beta$clicks[i]+1))
    temp2 <- temp
    temp2[probs_min<temp2] <-NA
    sumnom <- sumprobs + temp2
    sumprobs <- sumprobs + temp
  }
  return(sum(exp(na.omit(sumnom)))/sum(exp(sumprobs)))
}
best.arm.chance <- function(clicks=c(2284,663,2016,286,67),impressions=c(30888,7708,21638,3976,1233),N=3e5){
  beta<-data.frame(clicks=clicks,impressions=impressions)
  a <-beta$clicks
  b <- beta$impressions-beta$clicks
  ind_min <- which.max(beta$impressions)
  probs_min <- log(rbeta(N,beta$clicks[ind_min]+1,beta$impressions[ind_min]-beta$clicks[ind_min]+1))
  beta <- beta[-ind_min,]
  sumprobs <- probs_min; sumnom <- probs_min
  
  for(i in 1:dim(beta)[1]){
    temp<-log(rbeta(N,beta$clicks[i]+1,beta$impressions[i]-beta$clicks[i]+1))
    temp2 <- temp
    temp2[probs_min<temp2] <-NA
    sumnom <- sumprobs + temp2
    sumprobs <- sumprobs + temp
  }
  return(sum(exp(na.omit(sumnom)))/sum(exp(sumprobs)))
}
write.ads.to.remove.to.gsheet <- function(x,gsheet.key='151kgMLc_Pn3mxW9XBsrA214EZ_EW4cGp7CJeAHKOIKU'){
  gs_auth()
  worstAdsSheets <- gs_key(gsheet.key)
  sheetList <- gs_ws_ls(worstAdsSheets)
  account <- x$account
  if(account %in% sheetList){
    worstAdsSheets %>% gs_add_row(ws=account,input=x$worstAdsToRemove)
    print("adding new row to worksheet")
  } else {
    worstAdsSheets %>% gs_ws_new(ws=account,input=x$worstAdsToRemove,trim=TRUE)
    print("creating new worksheet")
  }
}
write.worst.ads.raw.to.gdrive <- function(x){
  drive_auth()
  write_csv(x$worstAdsData,path =paste0(x$account,'-',x$dateEnd,'.csv'))
  drive_upload(paste0(x$account,'-',x$dateEnd,'.csv'),
               path="Creative Research/Raw Files/",
               type = "spreadsheet")
}

remove.worst.ads.in.account <- function(account='flight-id-id',
                                        dateStart='2017-09-04', dateEnd='2017-09-24',action='remove',
                                        criteria.limit=0.05,
                                        gsheet.key='151kgMLc_Pn3mxW9XBsrA214EZ_EW4cGp7CJeAHKOIKU',
                                        write.raw=T,n.iter=3e5){
  # Define initial parameters
  clientIds <- list()
  clientIds[['flight-id-id']] <- '620-486-4059'
  clientIds[['flight-en-my']] <- '996-345-4053'
  clientIds[['flight-ms-my']] <- '870-914-1212'
  clientIds[['flight-en-ph']] <- '868-622-9726'
  clientIds[['flight-en-sg']] <- '965-347-8016'
  clientIds[['flight-en-th']] <- '915-569-8153'
  clientIds[['flight-th-th']] <- '151-022-7822'
  clientIds[['flight-en-vn']] <- '383-019-0036'
  clientIds[['flight-vi-vn']] <- '362-547-7085'
  clientIds[['hotel-id-id']] <- '767-050-2067'
  clientIds[['hotel-en-my']] <- '403-525-6679'
  clientIds[['hotel-ms-my']] <- '689-450-5551'
  clientIds[['hotel-en-ph']] <- '722-679-0663'
  clientIds[['hotel-en-sg']] <- '773-770-6306'
  clientIds[['hotel-en-th']] <- '198-250-7808'
  clientIds[['hotel-th-th']] <- '317-850-6873'
  clientIds[['hotel-en-vn']] <- '358-682-4414'
  clientIds[['hotel-vi-vn']] <- '822-989-7079'
  
  load(file="credentials.RData")

  pullAds <- statement(select=c('Id','AdType','AdGroupId','CampaignName','AdGroupName','Clicks','Impressions','Conversions','Headline','HeadlinePart1','HeadlinePart2','Description','Description1','Description2'),
                       report="AD_PERFORMANCE_REPORT",
                       where="Impressions>0 AND Status = ENABLED AND AdGroupStatus = ENABLED",
                       start=dateStart,
                       end=dateEnd)
  
  # make sure to use the Adwords Account Id (MCC Id will not work)
  adsData <- getData(clientCustomerId=clientIds[[account]], google_auth=google_auth, statement=pullAds)
  
  adsData <- adsData %>% group_by(AdgroupID) %>% 
    mutate(sum_Clicks = sum(Clicks),
           sum_Impressions = sum(Impressions),
           perc_served_impressions = Impressions/sum(Impressions),
           N_ads=length(AdID),
           N_sta=sum(Adtype=='Text ad') ) %>% filter(N_ads>3) %>% 
    arrange(desc(sum_Impressions))
  worstAdsData <- adsData %>% group_by(AdgroupID) %>%  top_n(-1,perc_served_impressions) %>% mutate(crit.value = NA,crit.value2=NA)
  
  for(i in 1:dim(worstAdsData)[1]){
    tempAdsData <- adsData %>% filter(AdgroupID==worstAdsData$AdgroupID[i])
    worstAdsData$crit.value[i]<-worst.arm.chance(clicks=tempAdsData$Clicks,impressions = tempAdsData$Impressions,N=n.iter)
    worstAdsData$crit.value2[i]<-1-best.arm.chance(clicks=tempAdsData$Clicks,impressions = tempAdsData$Impressions,N=n.iter)
  }
  worstAdsData$latestData <- dateEnd
  worstAdsData$verdict <- worstAdsData$crit.value < criteria.limit | worstAdsData$crit.value2 < criteria.limit
  
  worstAdsToRemove <- worstAdsData %>% filter(verdict==TRUE) %>% dplyr::select(`Ad ID`=AdID,`Ad group ID`=AdgroupID) %>% mutate(Action='remove',lastDate=dateEnd)
  
  worstList <-list(account=account,
                   dateEnd=dateEnd,
                   worstAdsToRemove=worstAdsToRemove,
                   worstAdsData=worstAdsData)
  
  write.ads.to.remove.to.gsheet(worstList,gsheet.key)
  
  if(write.raw) write.worst.ads.raw.to.gdrive(worstList)
  return(worstList)
}

## execute the ads removal
remove.worst.ads.in.account(account='flight-id-id',#put your account name here
                            dateStart='2017-09-04',#put your starting date here
                            dateEnd='2017-09-24',#more than 4 weeks after starting date
                            action='remove'#can change to paused if you wanted to
)
