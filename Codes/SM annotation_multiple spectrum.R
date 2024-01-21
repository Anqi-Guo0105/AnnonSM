options(encoding ="UTF-8")

# Load packages.
library(readxl)
library(tidyr)
library(plyr)
library(dplyr)
library(readr)
library(openxlsx)
library(metaMS)
library(stringr)
library(progress)

# Read precursor ion m/z and fragment peaks.
db.name <- 'D:/rdata/Aglycone(73)-30eV.mgf'   # database name (MGF format)
db <- readMgfData(db.name)

# Target Compounds Screening.
## Screen precursor ions in TPIL.(ppm = ±10ppm)
precursorion <- read_excel("D:/rdata/TPIL.xlsx")
precursorion$cal.m.z.H <- round(precursorion$cal.m.z.H,4)

precursorion_filter <- data.frame(matrix(ncol=0, nrow=0),check.names = FALSE)

pb <- progress_bar$new(total = length(db@assayData))

# k <- 1
for (k in 1:length(db@assayData)){
  pb$tick()
  Sys.sleep(0.05)
  title <- sort(names(db@assayData))[k]
  sample <- strsplit(db@featureData@data[["TITLE"]][k], ",", fixed= T)[[1]][1]
  premz <- round(as.numeric(db@assayData[[title]]@precursorMz),4)
  preintensity <- round(as.numeric(db@assayData[[title]]@precursorIntensity),4)
  rt <- round(as.numeric(db@assayData[[title]]@rt),4)
  rec <- data.frame("Compound ID" = c(title),"Exp.m/z" = c(premz),"Intensity" = c(preintensity),"RT" = c(rt),check.names = FALSE)
  
  precursorion$ppm <- abs((premz-precursorion$cal.m.z.H)/precursorion$cal.m.z.H)*1000000
  precursorion_filter1 <- precursorion %>% dplyr::filter(ppm <= 10)
  
  if (nrow(precursorion_filter1) == 0){
    next
  }else{
    precursorion_filter1$"Sample" <- sample
    precursorion_filter <- rbind(precursorion_filter,merge(rec,precursorion_filter1))
  }
  k <- k + 1
}

write.xlsx(precursorion_filter,"D:/rdata/Pre_result.xlsx")


#  Substituent Mode(SM) Matching.
## Screen fragment ions in CFIL.(ppm = ±50ppm)
CFIL <- read_excel("D:/rdata/CFIL.xlsx",sheet = "CFIL")
merge_dffinal1 <- data.frame(matrix(ncol=0, nrow=0),check.names = FALSE)
merge_dffinal2 <- data.frame(matrix(ncol=0, nrow=0),check.names = FALSE)

pb <- progress_bar$new(
  format = 'Progressing [:bar] :percent eta: :eta',
  total = length(unique(precursorion_filter$"Compound ID")), 
  clear = FALSE, width = 80
)


# pre <- 1
for (pre in 1:length(unique(precursorion_filter$"Compound ID"))){
  pb$tick()
  Sys.sleep(0.05)
  title <- unique(precursorion_filter$"Compound ID")[pre]
  sample <- strsplit(db@featureData@data[["TITLE"]][which(sort(names(db@assayData)) == title)], ",", fixed= T)[[1]][1]
  precursorion_filter3 <- subset(precursorion_filter,precursorion_filter$"Compound ID" == title)
  sub_frag <- data.frame("m/z (Exp)" = c(db@assayData[[title]]@mz),"Abund" = c(db@assayData[[title]]@intensity),check.names = FALSE)
  sub_frag$digitsub <- round(sub_frag$'m/z (Exp)',4)
  Abund_all <- sum(sub_frag$Abund)
  
  dffinal <- data.frame(matrix(ncol=0, nrow=0))
  for (prenum in 1:nrow(precursorion_filter3)){
    dfEND <- data.frame(matrix(ncol=0, nrow=0))
    
    combinedCFIL <- data.frame(matrix(ncol=0, nrow=0))
    CFIL$digit<- round(CFIL$`Cal.m/z of Frag`, 4)
    for (subnum in 1:nrow(sub_frag)){
      sub_filter <- sub_frag[subnum,]
      digit_filter <- sub_filter$digitsub
      CFIL$ppm <- abs((digit_filter-CFIL$digit)/CFIL$digit)*1000000
      CFIL_filter <- CFIL %>% dplyr::filter(ppm <= 50)
      
      num <- nrow(CFIL_filter)
      if(num == 0){
        next;
      }
      for (num in 1:nrow(CFIL_filter)){
        CFIL_filter1 <- cbind(CFIL_filter[num,],sub_filter)
        combinedCFIL <- rbind(combinedCFIL,CFIL_filter1)
        num <- num + 1
      }
      subnum <- subnum + 1
    }
    
    
    if(nrow(combinedCFIL)!= 0){
      sum_CFIL_abund <- combinedCFIL %>% group_by(`Substitution modes`) %>% dplyr::summarise(sum_abund = sum(Abund))
      sum_CFIL_F <- merge(combinedCFIL, sum_CFIL_abund, by = "Substitution modes", all = TRUE)
      dffilter <- precursorion_filter3[prenum,]
      T1 <- dffilter$OHsum
      T2 <- dffilter$OCH3sum
      combined_CFIL2 <- merge(sum_CFIL_F, dffilter, by = c("OHsum","OCH3sum"), all = TRUE)
      combined_CFIL2OH <- subset(combined_CFIL2, OHsum == T1)
      combined_CFIL2F <- subset(combined_CFIL2OH, OCH3sum == T2)
      combined_CFIL2E <- arrange(combined_CFIL2F, -combined_CFIL2F[, "sum_abund"])
      dfEND <- rbind(dfEND,combined_CFIL2E)
    }
    
    
    
    if(nrow(dfEND) != 0){
      dfEND <- arrange(dfEND, -dfEND[, "sum_abund"])
      dfEND$filter <- prenum
      dffinal <- rbind(dffinal,dfEND)
    }
    prenum = prenum + 1
  }
  dffinal <- na.omit(dffinal)
  
  if (nrow(dffinal) != 0){
    dffinal_arrange <- arrange(dffinal, -dffinal[, "sum_abund"])
    dffinal_arrange <- na.omit(dffinal_arrange)
    dffinal_all <- dffinal_arrange[,c("Compound ID","Substitution modes","sum_abund")]
    dffinal_unique <- unique(dffinal_all)
    dffinal_unique$"Sample" <- sample
    merge_dffinal1 <- rbind(merge_dffinal1,dffinal_arrange)
    merge_dffinal2 <- rbind(merge_dffinal2,dffinal_unique)
  }
  pre <- pre + 1
}

write.xlsx(merge_dffinal1,"D:/rdata/SMs_result1.xlsx")
write.xlsx(merge_dffinal2,"D:/rdata/SMs_result2.xlsx")
