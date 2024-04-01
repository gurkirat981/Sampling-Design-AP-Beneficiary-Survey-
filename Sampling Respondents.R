mandals <- c(
"ADDATEEGALA",
"RAJANAGARAM",
"SAVALYAPURAM",  
"CUMBUM",
"ANANTHASAGARAM",
"AMARAPURAM",
"VEMURU",
"KURNOOL",
"VARADAIAHPALEM",
"BUCHINAIDU KHANDRIGA",
"YADAMARI",
"AINAVILLI",
"GARLADINNE",
"GOSPADU",
"MADANAPALLE",
"MANDHANPALLE",
"AGIRIPALLI",
"GARUGUBILLI",
"BURJA",
"HIRAMANDALAM",
"KALLA",
"KAKUMANU",
"DUVVUR"
)  

round1sec <- secdata_samp[secdata_samp$MANDAL_NAME %in% mandals & secdata_samp$chosen == 1,]

vals <- aggregate(secdata$beneficiary_wt, by = list(secdata$stratum), sum)[,2]

stratum_weights <- vals/mean(vals)

hist(stratum_weights)

x <- c("TOTAL", colSums(distdata[,2:ncol(distdata)], na.rm = T))

distdata2 <- as.data.frame(rbind(distdata, x))

write.csv(distdata2, "DistrictSummary_Benficiaries.csv", row.names = F)
######


rm(list = ls())

library(vroom)


setwd("/Users/nsircar/Dropbox/AP Data Work/Temp/Beneficiary Survey")



files <- list.files()

getdist <- function(x){
  
  m <- nchar(x)
  
  return(substr(x,20,m-4))  
}

dists <- getdist(files) 
dists <- dists[3:28]

secdata <- read.csv("~/Dropbox/AP Data Work/Temp/SecretariatSummary_Benficiaries.csv")
secdata_samp <- read.csv("/Users/nsircar/Dropbox/AP Data Work/Temp/SecretariatSummary_Sample_Benficiaries.csv", stringsAsFactors = F)  
round1sec <- read.csv("~/Dropbox/AP Data Work/Temp/summary_round1.csv", stringsAsFactors = F)
allsec <- read.csv("~/Dropbox/Papers/allsecretariats_update.csv", stringsAsFactors = F)

round2sec <- allsec[allsec$Completed == 0 & (allsec$Region == "Inland_Southern"|allsec$NEW_DISTRICT == "SRI POTTI SRIRAMULU NELLORE"),]

dtcode <- read.csv("~/Dropbox/AP Data Work/KeyFiles_YSRCheyutha/dt_codes_to_name.csv")
hhidmatch <- read.csv("~/Dropbox/AP Data Work/KeyFiles_YSRCheyutha/NAhhid_join.csv")



na1 <- vroom("~/Dropbox/AP Data Work/DistrictGSWS_20220922/NA_six_step.csv", col_types = cols(.default = "c"))
na2 <- vroom("~/Dropbox/AP Data Work/DistrictGSWS_20220922/NA_vol_mapping.csv", col_types = cols(.default = "c"))
na3 <- vroom("~/Dropbox/AP Data Work/DistrictGSWS_20220922/NA_caste.csv", col_types = cols(.default = "c"))


z <- match(na1$MORPHED_UIDNUM, na2$MORPHED_UIDNUM)

nadat <- as.data.frame(cbind(na1, na2[z,]))  

malevals <- c("Boy", "M", "MALE", "MALE ", "Male")
femalevals <- c("F", "FEMALE", "Female", "Girl")
tgvals <- c("TRANSGENDER", "Transgender")

files <- sort(list.files("~/Dropbox/AP Data Work/DistrictGSWS_20220922/"))
v <- unique(as.numeric(strsplit(files, "_caste.csv|_six_step.csv|_vol_mapping.csv")))
distname <- dtcode$DT_Name[match(v, dtcode$DT_Code)]

r <- match(dists, distname)

n.amma <- 10
n.cheyutha <- 10


distname2 <- distname[distname %in% round2sec$NEW_DISTRICT]
o <- order(distname2)
distname2 <- distname2[o]
v <- v[distname %in% round2sec$NEW_DISTRICT][o]
  
r <- na.omit(match(dists, distname2))


set.seed(1235)


for (i in 1:length(distname2)){
  
  subdat <- round2sec
  
  respall <- NULL
  
  dname <- distname2[r[i]]
  
  secs <- subdat$SECRETARIAT_CODE[subdat$NEW_DISTRICT == distname2[r[i]]]  
  
  d1 <- paste("~/Dropbox/AP Data Work/DistrictGSWS_20220922/",v[r[i]], "_six_step.csv", sep = "")
  dat1 <- vroom(d1, col_types = cols(.default = "c"))
  
  d2 <- paste("~/Dropbox/AP Data Work/DistrictGSWS_20220922/",v[r[i]], "_vol_mapping.csv", sep = "")
  dat2 <- vroom(d2, col_types = cols(.default = "c"))
  
  d3 <- paste("~/Dropbox/AP Data Work/DistrictGSWS_20220922/",v[r[i]], "_caste.csv", sep = "")
  dat3pre <- vroom(d3, col_types = cols(.default = "c"))
  
  dist <- distname2[r[i]]
  hhiddist <- hhidmatch$HHID[hhidmatch$District == dist]
  
  
  z <- match(dat1$MORPHED_UIDNUM, dat2$MORPHED_UIDNUM)
  
  
  dat3 <- as.data.frame(rbind(dat3pre, na3[na3$HHID %in% hhiddist,]))
  datpre <- as.data.frame(cbind(dat1, dat2[z,]))  
  datdup <- as.data.frame(rbind(datpre, nadat[nadat$HHID %in% hhiddist,]))
  
  dat <- datdup[duplicated(datdup$MORPHED_UIDNUM) == F,]
  
  orignew <- as.Date("01-11-2022", "%d-%m-%Y")
  agedat <- as.Date(dat$DOB_DT, "%d-%m-%y")
  agemoday <- substr(agedat,5, 10)
  ageyr <- as.numeric(substr(agedat,1, 4))
  yrfix <- ifelse(ageyr > 2022, ageyr - 100, ageyr )
  
  fixeddate <- paste(yrfix, agemoday, sep = "")
  
  dat$age <- orignew - as.Date(fixeddate) ## Age in the GSWS Database (days)
  
  dat$GENDER <- ifelse(dat$GENDER %in% malevals, "MALE", dat$GENDER)
  dat$GENDER <- ifelse(dat$GENDER %in% femalevals, "FEMALE", dat$GENDER)
  dat$GENDER <- ifelse(dat$GENDER %in% tgvals, "TRANSGENDER", dat$GENDER)
  
  hhfile <- vroom(paste("~/Dropbox/AP Data Work/Temp/Beneficiary Survey/Beneficiary New/hhfile_beneficiary_", distname2[r[i]], ".csv", sep = ""))
  
  ## Ammavodi Beneficiary
  
  
  datben_amma <- dat[dat$MORPHED_UIDNUM %in% na.omit(hhfile$amma_uid[hhfile$amma_uidcheck == 1]),]
  datben_cheyutha_1 <- dat[dat$MORPHED_UIDNUM %in% na.omit(hhfile$cheyutha_uid) & (dat$HHID %in% hhfile$HHID[hhfile$thisyr == 1 & hhfile$cheyutha_uidcheck == 1]),]
  datben_cheyutha_2 <- dat[dat$MORPHED_UIDNUM %in% na.omit(hhfile$cheyutha_uid) & (dat$HHID %in% hhfile$HHID[hhfile$lastyrmiss == 1  & hhfile$cheyutha_uidcheck == 1]),]
  datben_cheyutha_3 <- dat[dat$age >= 16436 & dat$age < 21915 & dat$GENDER == "FEMALE" & (dat$HHID %in% hhfile$HHID[hhfile$noapp == 1]),]
  datben_cheyutha_3 <- datben_cheyutha_3[duplicated(datben_cheyutha_3$HHID) == F,]
  datben_cheyutha_3 <- datben_cheyutha_3[!is.na(datben_cheyutha_3$MORPHED_UIDNUM),]
  
  datben_cheyutha <- as.data.frame(rbind(datben_cheyutha_1, datben_cheyutha_2, datben_cheyutha_3))
  
  datben_amma <- datben_amma[datben_amma$age >= 7670  & datben_amma$age <= 20089 & datben_amma$GENDER == "FEMALE" & duplicated(datben_amma$HHID) == F, ]
  datben_cheyutha <- datben_cheyutha[datben_cheyutha$age >= 16436 & datben_cheyutha$age < 21915 & datben_cheyutha$GENDER == "FEMALE" & duplicated(datben_cheyutha$HHID) == F,]
  
  
  for (j in 1:length(secs)){
    
    sec <- as.character(secs[j])
    
    
    datsec <- datben_amma[datben_amma$SECRETARIAT_CODE == sec,]
    datsec <- datsec[!is.na(datsec$MORPHED_UIDNUM),]
    o <- order(datsec$CLUSTER_ID, datsec$HHID)
    datsec <- datsec[o,]
    
    min.strata <- floor(nrow(datsec)/n.amma)
    
    rem <- nrow(datsec) %% min.strata
    
    strata.size <-rep(min.strata, n.amma)
    strata.size[sample(n.amma, rem)] <- min.strata + 1
    
    cuts <- c(0, cumsum(strata.size))
    
    sampled_hh <- clust_hh <- sampled_resp <- respdetails <-  NULL
    
    datsec$clust <- as.numeric(substr(datsec$CLUSTER_NAME, 2, nchar(datsec$CLUSTER_NAME)))
    
    for (q in 1:n.amma){
      
      hhpick <- sample(setdiff(datsec$HHID[(cuts[q] + 1):cuts[q+1]], sampled_hh), 1)
      picked <- which(datsec$HHID == hhpick)
      
      
      datsec2 <- datsec[!(datsec$HHID %in% c(hhpick, sampled_hh)),]  
      
      
      clusts <- as.numeric(substr(unique(datsec2$CLUSTER_NAME), 2, nchar(unique(datsec2$CLUSTER_NAME))))
      clustpick <- as.numeric(substr(datsec$CLUSTER_NAME[picked], 2, nchar(datsec$CLUSTER_NAME[picked])))
      
      distclust <- abs(clusts - clustpick)
      
      distval <- unique(sort(distclust)[1:5])
      m <- length(distval)
      clusters <- NULL
      
      for (i in 1:m){
        
        l <- as.character(clusts[distclust == distval[i]])
        clusters <- c(clusters, sample(l,length(l) ))  
      }
      
      reps <- NULL
      
      for (i in 1:5){
        w <- datsec2$HHID[datsec2$clust == clusters[i]] 
        reps <- c(reps, sample(w, length(w)))
        
        if (length(reps) >= 5)
          break
      }
      
      sampled_hh <- c(sampled_hh, c(hhpick, reps[1:5]))
      
      
    }
    
    
    ### Respondent Details for Ammavodi Respondents
    
    samphh_amma <- sampled_hh[1:60]
    
    z <- match(samphh_amma, datsec$HHID)
    
    respdetails_amma <- datsec[z,]
    respdetails_amma$bencode <- rep(1, nrow(respdetails_amma))
    
    
    ## YSR Cheyutha
    
    totysr <- secdata$totinc[secdata$SECRETARIAT_CODE == sec]
    
    n.noapp1 <- ceiling(n.cheyutha*secdata$noapp[secdata$SECRETARIAT_CODE == sec]/totysr)
    n.noapp2 <- floor(secdata$noapp[secdata$SECRETARIAT_CODE == sec]/6)
    n.noapp <- min(n.noapp1, n.noapp2)
    
    n.misslastyr1 <- ceiling(n.cheyutha*secdata$lastyrmiss[secdata$SECRETARIAT_CODE == sec]/totysr)
    n.misslastyr2 <- floor(secdata$lastyrmiss[secdata$SECRETARIAT_CODE == sec]/6)
    n.misslastyr <- min(n.misslastyr1, n.misslastyr2)
    
    n.included <- n.cheyutha - n.misslastyr - n.noapp
    
    
    respdetails_noapp <- NULL
    
    if (n.noapp > 0){   
      
    datsec <- datben_cheyutha_3[datben_cheyutha_3$SECRETARIAT_CODE == sec,]
    datsec <- datsec[!is.na(datsec$MORPHED_UIDNUM),]
    o <- order(datsec$CLUSTER_ID, datsec$HHID)
    datsec <- datsec[o,]
    
    
    ## No Application
 
    min.strata <- floor(nrow(datsec)/n.noapp)
    
    rem <- nrow(datsec) %% min.strata
    
    strata.size <-rep(min.strata, n.noapp)
    strata.size[sample(n.noapp, rem)] <- min.strata + 1
    
    cuts <- c(0, cumsum(strata.size))
    
    sampled_hh <- clust_hh <- sampled_resp <- respdetails <-  NULL
    
    datsec$clust <- as.numeric(substr(datsec$CLUSTER_NAME, 2, nchar(datsec$CLUSTER_NAME)))
    
    for (q in 1:n.noapp){
      
      hhpick <- sample(setdiff(datsec$HHID[(cuts[q] + 1):cuts[q+1]], sampled_hh), 1)
      picked <- which(datsec$HHID == hhpick)
      
      
      datsec2 <- datsec[!(datsec$HHID %in% c(hhpick, sampled_hh)),]  
      
      
      clusts <- as.numeric(substr(unique(datsec2$CLUSTER_NAME), 2, nchar(unique(datsec2$CLUSTER_NAME))))
      clustpick <- as.numeric(substr(datsec$CLUSTER_NAME[picked], 2, nchar(datsec$CLUSTER_NAME[picked])))
      
      distclust <- abs(clusts - clustpick)
      
      distval <- unique(sort(distclust)[1:5])
      m <- length(distval)
      clusters <- NULL
      
      for (i in 1:m){
        
        l <- as.character(clusts[distclust == distval[i]])
        clusters <- c(clusters, sample(l,length(l) ))  
      }
      
      reps <- NULL
      
      for (i in 1:5){
        w <- datsec2$HHID[datsec2$clust == clusters[i]] 
        reps <- c(reps, sample(w, length(w)))
        
        if (length(reps) >= 5)
          break
      }
      
      sampled_hh <- c(sampled_hh, c(hhpick, reps[1:5]))
      
      
    }
    
    samphh_noapp <- sampled_hh[1:(n.noapp*6)]
    
    z <- match(samphh_noapp, datsec$HHID)
    
    respdetails_noapp <- datsec[z,]
    respdetails_noapp$bencode <- rep(2, nrow(respdetails_noapp))
    
    }
    ## Missed Last Year
    
    respdetails_misslastyr <- NULL
    
    if (n.misslastyr > 0){   
      
    datsec <- datben_cheyutha_2[datben_cheyutha_2$SECRETARIAT_CODE == sec,]
    datsec <- datsec[!is.na(datsec$MORPHED_UIDNUM),]
    o <- order(datsec$CLUSTER_ID, datsec$HHID)
    datsec <- datsec[o,]
    
    min.strata <- floor(nrow(datsec)/n.misslastyr)
    
    rem <- nrow(datsec) %% min.strata
    
    strata.size <-rep(min.strata, n.misslastyr)
    strata.size[sample(n.misslastyr, rem)] <- min.strata + 1
    
    cuts <- c(0, cumsum(strata.size))
    
    sampled_hh <- clust_hh <- sampled_resp <- respdetails <-  NULL
    
    datsec$clust <- as.numeric(substr(datsec$CLUSTER_NAME, 2, nchar(datsec$CLUSTER_NAME)))
    
    for (q in 1:n.misslastyr){
      
      hhpick <- sample(setdiff(datsec$HHID[(cuts[q] + 1):cuts[q+1]], sampled_hh), 1)
      picked <- which(datsec$HHID == hhpick)
      
      
      datsec2 <- datsec[!(datsec$HHID %in% c(hhpick, sampled_hh)),]  
      
      
      clusts <- as.numeric(substr(unique(datsec2$CLUSTER_NAME), 2, nchar(unique(datsec2$CLUSTER_NAME))))
      clustpick <- as.numeric(substr(datsec$CLUSTER_NAME[picked], 2, nchar(datsec$CLUSTER_NAME[picked])))
      
      distclust <- abs(clusts - clustpick)
      
      distval <- unique(sort(distclust)[1:5])
      m <- length(distval)
      clusters <- NULL
      
      for (i in 1:m){
        
        l <- as.character(clusts[distclust == distval[i]])
        clusters <- c(clusters, sample(l,length(l) ))  
      }
      
      reps <- NULL
      
      for (i in 1:5){
        w <- datsec2$HHID[datsec2$clust == clusters[i]] 
        reps <- c(reps, sample(w, length(w)))
        
        if (length(reps) >= 5)
          break
      }
      
      sampled_hh <- c(sampled_hh, c(hhpick, reps[1:5]))
      
      
    }
    
    samphh_misslastyr <- sampled_hh[1:(n.misslastyr*6)]
    
    z <- match(samphh_misslastyr, datsec$HHID)
    
    respdetails_misslastyr <- datsec[z,]
    
    respdetails_misslastyr$bencode <- rep(3, nrow(respdetails_misslastyr))
    }
    
    ## Normal Beneficiary
    
    respdetails_included <- NULL
    
    if (n.included > 0){   
      
    datsec <- datben_cheyutha_1[datben_cheyutha_1$SECRETARIAT_CODE == sec,]
    datsec <- datsec[!is.na(datsec$MORPHED_UIDNUM),]
    o <- order(datsec$CLUSTER_ID, datsec$HHID)
    datsec <- datsec[o,]
    
    
    min.strata <- floor(nrow(datsec)/n.included)
    
    rem <- nrow(datsec) %% min.strata
    
    strata.size <-rep(min.strata, n.included)
    strata.size[sample(n.included, rem)] <- min.strata + 1
    
    cuts <- c(0, cumsum(strata.size))
    
    sampled_hh <- clust_hh <- sampled_resp <- respdetails <-  NULL
    
    datsec$clust <- as.numeric(substr(datsec$CLUSTER_NAME, 2, nchar(datsec$CLUSTER_NAME)))
    
    for (q in 1:n.included){
      
      hhpick <- sample(setdiff(datsec$HHID[(cuts[q] + 1):cuts[q+1]], sampled_hh), 1)
      picked <- which(datsec$HHID == hhpick)
      
      
      datsec2 <- datsec[!(datsec$HHID %in% c(hhpick, sampled_hh)),]  
      
      
      clusts <- as.numeric(substr(unique(datsec2$CLUSTER_NAME), 2, nchar(unique(datsec2$CLUSTER_NAME))))
      clustpick <- as.numeric(substr(datsec$CLUSTER_NAME[picked], 2, nchar(datsec$CLUSTER_NAME[picked])))
      
      distclust <- abs(clusts - clustpick)
      
      distval <- unique(sort(distclust)[1:5])
      m <- length(distval)
      clusters <- NULL
      
      for (i in 1:m){
        
        l <- as.character(clusts[distclust == distval[i]])
        clusters <- c(clusters, sample(l,length(l) ))  
      }
      
      reps <- NULL
      
      for (i in 1:5){
        w <- datsec2$HHID[datsec2$clust == clusters[i]] 
        reps <- c(reps, sample(w, length(w)))
        
        if (length(reps) >= 5)
          break
      }
      
      sampled_hh <- c(sampled_hh, c(hhpick, reps[1:5]))
      
      
    }
    
    samphh_included <- sampled_hh[1:(n.included*6)]
    
    z <- match(samphh_included, datsec$HHID)
    
    respdetails_included <- datsec[z,]
    respdetails_included$bencode <- rep(4, nrow(respdetails_included))
    
    }
    respdetails <- as.data.frame(rbind(respdetails_amma, respdetails_noapp, respdetails_misslastyr, respdetails_included))
 
    respall <- as.data.frame(rbind(respall, respdetails))
    
     }
  
  write.csv(respall, paste("~/Dropbox/AP Data Work/Temp/Beneficiary Survey/Beneficiary New/Facepages/respdetails_",dname, ".csv", sep = "" ), row.names = F)
  
}


## Order of Secretariat Delivery

set.seed(135663) 

z <- sample(1:nrow(round2sec), nrow(round2sec))
round2sec2 <- round2sec[z,]
o <- order(round2sec2$NEW_DISTRICT)

round2sec_order <- round2sec2[o,]

write.csv(round2sec_order, "round2sec_order.csv", row.names = F)
write.csv(round2sec, "round2sec.csv", row.names = F)

