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

# Read precursor ion m/z and fragment peaks.
db.name <- 'D:/rdata/F121-POS-MSMS-2-70V.mgf'    # database name (MGF format)
db <- readMgfData(db.name)

m_z <- round(db@assayData[["X1"]]@precursorMz,4)
sub_frag <- data.frame('m/z (Exp)' = c(db@assayData[["X1"]]@mz),'Abund' = c(db@assayData[["X1"]]@intensity),check.names = FALSE)
sub_frag$digitsub <- round(sub_frag$'m/z (Exp)',4)
Abund_all <- sum(sub_frag$Abund)

# Target Compounds Screening.
## Screen precursor ions in TPIL.(ppm = ±10ppm)
precursorion1 <- read_excel("D:/rdata/TPIL.xlsx")
precursorion1$cal.m.z.H <- round(precursorion1$cal.m.z.H,4)

precursorion1$ppm <- abs((m_z-precursorion1$cal.m.z.H)/precursorion1$cal.m.z.H)*1000000
precursorion_filter1 <- precursorion1 %>% dplyr::filter(ppm <= 10)

write.xlsx(precursorion_filter1,"D:/rdata/Pre_result.xlsx")



# Substituent Mode(SM) Matching.
## Read CFIL.
CFIL <- read_excel("D:/rdata/CFIL.xlsx",sheet = "CFIL")
dffinal <- data.frame(matrix(ncol=0, nrow=0))


## Screen fragment ions in CFIL.(ppm = ±50ppm)
for (prenum in 1:nrow(precursorion_filter)){
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
  
  sum_CFIL_abund <- combinedCFIL %>% group_by(`Substitution modes`) %>% dplyr::summarise(sum_abund = sum(Abund))
  sum_CFIL_F <- merge(combinedCFIL, sum_CFIL_abund, by = "Substitution modes", all = TRUE)
  dffilter <- precursorion_filter[prenum,]
  T1 <- dffilter$OHsum
  T2 <- dffilter$OCH3sum
  combined_CFIL2 <- merge(sum_CFIL_F, dffilter, by = c("OHsum","OCH3sum"), all = TRUE)
  combined_CFIL2OH <- subset(combined_CFIL2, OHsum == T1)
  combined_CFIL2F <- subset(combined_CFIL2OH, OCH3sum == T2)
  combined_CFIL2E <- arrange(combined_CFIL2F, -combined_CFIL2F[, "sum_abund"])
  
  combined_CFIL2E <- arrange(combined_CFIL2E, -combined_CFIL2E[, "sum_abund"])
  combined_CFIL2E$filter <- prenum
  dffinal <- rbind(dffinal,combined_CFIL2E)
  prenum = prenum + 1
}


dffinal_arrange <- arrange(dffinal, -dffinal[, "sum_abund"])
dffinal_arrange <- na.omit(dffinal_arrange)
dffinal_all <- dffinal_arrange[,c("Substitution modes","sum_abund")]
dffinal_unique <- unique(dffinal_all)


write.xlsx(dffinal_arrange,"D:/rdata/SM_result1.xlsx")
write.xlsx(dffinal_unique,"D:/rdata/SM_result2.xlsx")