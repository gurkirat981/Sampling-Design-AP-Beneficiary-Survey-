rm(list = ls())

library(pdftools)
library(jpeg)


setwd("/Users/nsircar/Dropbox/AP Data Work/Temp/Beneficiary Survey")



files <- list.files()

getdist <- function(x){
  
  m <- nchar(x)
  
  return(substr(x,20,m-4))  
}

dists <- getdist(files) 
dists <- dists[3:28]

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
s <- unique(as.numeric(strsplit(files, "_caste.csv|_six_step.csv|_vol_mapping.csv")))
distname <- dtcode$DT_Name[match(s, dtcode$DT_Code)]

r <- match(dists, distname)

n.amma <- 10
n.cheyutha <- 10


distname2 <- distname[distname %in% round2sec$NEW_DISTRICT]
o <- order(distname2)
distname2 <- distname2[o]
s <- s[distname %in% round2sec$NEW_DISTRICT][o]

r <- na.omit(match(dists, distname2))



for (p in 1:9){

rpdet <- read.csv(paste("/Users/nsircar/Dropbox/AP Data Work/Temp/Beneficiary Survey/Beneficiary New/Facepages/respdetails_",distname2[p] ,".csv", sep = ""), stringsAsFactors = F)


rpdet$relation <- rep(NA, nrow(rpdet))



  
  d1 <- paste("~/Dropbox/AP Data Work/DistrictGSWS_20220922/",s[r[p]], "_six_step.csv", sep = "")
  dat1 <- vroom(d1, col_types = cols(.default = "c"))
  
  d2 <- paste("~/Dropbox/AP Data Work/DistrictGSWS_20220922/",s[r[p]], "_vol_mapping.csv", sep = "")
  dat2 <- vroom(d2, col_types = cols(.default = "c"))
  
  d3 <- paste("~/Dropbox/AP Data Work/DistrictGSWS_20220922/",s[r[p]], "_caste.csv", sep = "")
  dat3pre <- vroom(d3, col_types = cols(.default = "c"))
  
  dist <- distname2[r[p]]
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

  
    
hhids <- rpdet$HHID
uids <- rpdet$MORPHED_UIDNUM
citizennames <- rpdet$CITIZEN_NAME

rpdet$relation <- rep(NA, length(hhids))


for (i in 1:length(hhids)){
  
  dathh <- dat[dat$HHID == hhids[i],]  
  dathh <- dathh[!is.na(dathh$MORPHED_UIDNUM),]
  
  if (nrow(dathh) == 1) rpdet$relation[i] <- ""
  
  if (nrow(dathh) > 1){
    
    dathh2 <- dathh[dathh$CITIZEN_NAME != citizennames[i] & dathh$GENDER %in% malevals & !is.na(dathh$CITIZEN_NAME),]  
    
    if (nrow(dathh2) > 0){
      
      o <- order(dathh2$age, decreasing = T)  
      
      dathh2 <- dathh2[o,]  
      
      rpdet$relation[i] <- dathh2$CITIZEN_NAME[1]
      
    }  
    
    
    if (nrow(dathh2) == 0){
      
      dathh3 <- dathh[dathh$CITIZEN_NAME != citizennames[i] & !is.na(dathh$CITIZEN_NAME),]  
      
      o <- order(dathh3$age, decreasing = T)  
      
      dathh3 <- dathh3[o,]  
      
      rpdet$relation[i] <- dathh3$CITIZEN_NAME[1]
      
    }  
    
    
  }
  
  
  cat(paste(i, ".", sep = ""))
  
}


n.sec <- length(unique(rpdet$SECRETARIAT_CODE))

setwd(paste("~/Dropbox/AP Data Work/Temp/Beneficiary Survey/Beneficiary New/Facepages/", distname2[p], sep = "" ))

for (q in 1:n.sec){
#hhlist <- hhlist_all[[q]]
#resplist <- resplist_all[[q]]
respdetails <- rpdet[120*(q-1) + 1:120,] 

cat(paste("\n", q, ":\n", sep = ""))  

infoval <- unlist(respdetails[2, c(27, 28, 32, 33, 25, 31)])

dname <- infoval[1]
dcode <- infoval[2]
cname <- infoval[3]
secname <- infoval[4]
seccode <- infoval[5]
ccode <- infoval[6]


for (l in 1:20){
  
  pdf(paste(paste(cname, secname, ccode, seccode, l, sep = "_" ), ".pdf", sep = ""), width = 8.27, height = 11.69)
  par(mar = rep(0,4))
  plot(0,0, xlim = c(0,8.27), ylim = c(0,11.69), type = "n", axes = F)
  jp <- readJPEG('~/Dropbox/AP Data Work/Temp/ben_header.jpg')
  
  #axis(2, at = seq(0,11.69,.5), pos = .5)
  #axis(1, at = seq(0,8.27,.5), pos = 3)
  rasterImage(jp, xleft=.8, xright=7.47, ybottom=10.3, ytop=11.60)
  
  
  jp <- readJPEG('~/Dropbox/AP Data Work/Temp/ben_info.jpg')
  
  #axis(2, at = seq(0,11.69,.5), pos = .5)
  #axis(1, at = seq(0,8.27,.5), pos = 3)
  rasterImage(jp, xleft=.65, xright=7.62, ybottom=7, ytop=10.2)
  
  
  
  
  
  text(1.9, 9.755, dcode, pos = 4)
  text(2.18, 9.1725, ccode, pos = 4)
  text(2.06, 8.59, seccode, pos = 4)
  text(4.25, 9.755, dname, pos = 4)
  text(4.67, 9.1725, cname, pos = 4)
  text(4.5, 8.59, secname, pos = 4)
  
  
  
  respsub <- respdetails[(6*l - 5):(6*l),]
  
  respvals <- cbind(l*100 + 0:5,  respsub[,c(24, 2, 21)] )
  respvals[is.na(respvals)] <- ""
  
  
  relatn <- paste("Relation:", respdetails$relation[(6*l - 5):(6*l)], sep = " ")
  
  n.rows <- nrow(respvals)
  n.cols <- ncol(respvals)
  
  xpos <- c(0, 1.65, 2.13, 4.65)
  
  v <- c(0,.57,  1.7, 2.16, 4.55, 8.27)
  v1 <- v[1:(n.cols + 1)]; v2 <- v[2: (n.cols + 2)]
  
  
  
  text( c((v1 + v2)[1:n.cols]/2), rep(6.2 + .4, n.cols + 1), c("RespID", "S/R","Cluster", "HHID"), font = 2 )
  text(xpos[n.cols] + .1, 6.2+.4, "Name", font = 2, pos = 4)
  
  text(rep(1.3,n.rows), 6.2 - .49*(0:(n.rows - 1)) , c("S", paste("R", 1:(n.rows - 1), sep = "")), font = 2, pos = 4) 
  
  jp <- readJPEG('~/Dropbox/AP Data Work/Temp/whymissed_telugu.jpg')
  
  
  for (i in 1:n.rows){
    
    rasterImage(jp, xleft=.6, xright=1.4, ybottom= 5.96 - .49*(i-1), ytop=6.44 - .49*(i-1))
    
    
    for (j in 1:(n.cols-1)){  
      
      w <- as.character(respvals[i,j])  
      
      if (nchar(w) < 28) text(xpos[j], 6.2 - .49*(i-1),   w , pos = 4)   
      if (nchar(w) >= 28) text(xpos[j], 6.2 - .49*(i-1),   w , pos = 4, cex = .7)   
      
      
      
    }
    
    text(rep(xpos[n.cols], 6), 6.25 - .49*((1:n.rows) - 1), respvals[,n.cols], pos= 4)
    text(rep(xpos[n.cols], 6), 6.06 - .49*((1:n.rows) - 1), relatn, pos= 4, cex = .7)
    
    segments(rep(0, n.rows + 1), 6.2 + .245 - .49*(0:(n.rows)), rep(8.27, n.rows + 1),  6.2 + .245 - .49*(0:(n.rows)))  
    segments( c(0,.57,  1.7, 2.16, 4.55, 8.27), rep(6.2 + .245, n.cols + 2),  c(0,.57, 1.7, 2.16, 4.55, 8.27), rep(6.2 + .245 - .49*(n.rows), n.cols + 2) )  
    
    #axis(2, at = seq(0,11.69,.5), pos = .5)
    #axis(1, at = seq(0,8.27,.5), pos = 3)
    
  }
  
  
  
  jp <- readJPEG('~/Dropbox/AP Data Work/Temp/consentgiven_telugu.jpg')
  rasterImage(jp, xleft=1, xright=3.7, ybottom= 3, ytop=3.3)
  
  
  
  jp <- readJPEG('~/Dropbox/AP Data Work/Temp/roster_telugu.jpg')
  
  #axis(2, at = seq(0,11.69,.5), pos = .5)
  #axis(1, at = seq(0,8.27,.5), pos = 3)
  rasterImage(jp, xleft=.5, xright=7.77, ybottom=0, ytop=2.9)
  
  dev.off()
  
  cat(paste(l, ".", sep = ""))
}
    

sec_filenames <- paste(paste(cname, secname, ccode, seccode, 1:20, sep = "_" ), ".pdf", sep = "")

pdf_combine(sec_filenames, output = paste(paste(cname, seccode, secname, q,  sep = "_" ), ".pdf", sep = "") )
file.remove(sec_filenames)
}

}
