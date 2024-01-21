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
db.name <- 'D:/rdata/mono(40)-70eV.mgf'   # database name (MGF format)
db <- readMgfData(db.name)

# Target Compounds Screening.
## Screen precursor ions in TPIL.(ppm = Â±10ppm)
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
  }
  sub_frag <- data.frame("m/z (Exp)" = c(db@assayData[[title]]@mz),"Abund" = c(db@assayData[[title]]@intensity),check.names = FALSE)
  sub_frag$digitsub <- round(sub_frag$'m/z (Exp)',4)
  if (nrow(precursorion_filter1) == 1){
    precursorion_filter1$"Sample" <- sample
    precursorion_filter <- rbind(precursorion_filter,merge(rec,precursorion_filter1))
  }else if((nrow(precursorion_filter1) != 1) &
           (nrow(precursorion_filter1) != 0) == TRUE ){
    
    precursorion_filter2 <- data.frame(matrix(ncol=0, nrow=0),check.names = FALSE)
    precursorion1 <- precursorion[1:66,]
    
    for (s in 1:nrow(sub_frag)){
      digitsub <- sub_frag[s,"digitsub"]
      precursorion1$ppm <- abs((digitsub-precursorion1$cal.m.z.H)/precursorion1$cal.m.z.H)*1000000
      pre_filter1 <- precursorion1 %>% dplyr::filter(ppm <= 10)
      precursorion_filter2 <- rbind(precursorion_filter2,pre_filter1)
      s = s+1
    }
    
    if (nrow(precursorion_filter2) == 0){
      precursorion_filter1$"Sample" <- sample
      precursorion_filter <- rbind(precursorion_filter,merge(rec,precursorion_filter1))
    }else{
      # t <- 1
      precursorion_filter3 <- data.frame(matrix(ncol=0, nrow=0),check.names = FALSE)
      for (t in 1:nrow(precursorion_filter2)){
        OHs <- precursorion_filter2[t,]$OHsum
        OCH3s <- precursorion_filter2[t,]$OCH3sum
        pre_filter2 <- subset(precursorion_filter1,OHsum == OHs & OCH3sum == OCH3s)
        pre_filter2$"Sample" <- sample
        
        precursorion_filter3 <- rbind(precursorion_filter3,merge(rec,pre_filter2))
        t = t+1
      }
      if(nrow(precursorion_filter3) == 0){
        precursorion_filter1$"Sample" <- sample
        precursorion_filter <- rbind(precursorion_filter, merge(rec,precursorion_filter1))
      }else{
        precursorion_filter <- rbind(precursorion_filter,precursorion_filter3)
      }
    }
  }
  k <- k + 1
}

write.xlsx(precursorion_filter,"D:/rdata/Pre_result.xlsx")


#  Substituent Mode(SM) Matching.
## Screen fragment ions in CFIL.(ppm = Â±50ppm)
CFIL <- read_excel("D:/rdata/CFIL.xlsx",sheet = "CFIL")
merge_dffinal1 <- data.frame(matrix(ncol=0, nrow=0),check.names = FALSE)
merge_dffinal2 <- data.frame(matrix(ncol=0, nrow=0),check.names = FALSE)

pb <- progress_bar$new(
  format = 'Progressing [:bar] :percent eta: :eta',
  total = length(unique(precursorion_filter$"Compound ID")), 
  clear = FALSE, width = 80
)


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
    dffinal_all <- dffinal_arrange[,c("Substitution modes","sum_abund")]
    
    dffinal_unique <- unique(dffinal_all)
    
    
    ## Classification of SMs based on CFIs.
    dffinal_arrange$'Class' <- " "
    
    Q2_ref <- c("[M+H-B ring]+-CO","[M+H-B ring]+-2Ã—CO","[M+H-B ring]+-3Ã—CO","[M+H-B ring]+-4Ã—CO",
                "[M+H-B ring]+-CH3-CO","[M+H-B ring]+-CH3-2Ã—CO","[M+H-B ring]+-CH3-3Ã—CO")
    Q4_ref <- c("0ï¼?2B+","0ï¼?2B+-CO")
    Q5_ref <- c("0ï¼?4B+-CO","0ï¼?4B+-2Ã—CO","0ï¼?4B+-3Ã—CO")
    Q6_ref <- c("1ï¼?4B+","1ï¼?4B+-CO","1ï¼?4B+--2Ã—CO")
    Q7_ref <- c("0ï¼?3B+","0ï¼?3B+-CO","0ï¼?3B+-2Ã—CO")
    Q8_ref <- c("[B ring]+","[B ring]+-CO","[B ring]+-CH3","[B ring]+-CH3-CO","[B ring]+-CH3-2Ã—CO")
    
    for (Anno in 1:nrow(dffinal_unique)){
      x <- dffinal_unique[Anno,'Substitution modes']
      subset_Anno <- subset(dffinal_arrange,`Substitution modes` == x)
      Q2_table <- data.frame(matrix(ncol=0, nrow=0))
      Q4_table <- data.frame(matrix(ncol=0, nrow=0))
      Q5_table <- data.frame(matrix(ncol=0, nrow=0))
      Q6_table <- data.frame(matrix(ncol=0, nrow=0))
      Q7_table <- data.frame(matrix(ncol=0, nrow=0))
      Q8_table <- data.frame(matrix(ncol=0, nrow=0))
      Q_table <- data.frame(matrix(ncol=0, nrow=0))
      
      for (Annos in 1:nrow(subset_Anno)){
        subset_Annos <- subset_Anno[Annos,]
        Q <- unlist(strsplit(subset_Annos[,'Annotations'],"/"))
        if (length(intersect(Q2_ref,Q)) != 0){Q2_table <- rbind(Q2_table,subset_Annos)}
        if (length(intersect(Q4_ref,Q)) != 0){Q4_table <- rbind(Q4_table,subset_Annos)}
        if (length(intersect(Q5_ref,Q)) != 0){Q5_table <- rbind(Q5_table,subset_Annos)}
        if (length(intersect(Q6_ref,Q)) != 0){Q6_table <- rbind(Q6_table,subset_Annos)}
        if (length(intersect(Q7_ref,Q)) != 0){Q7_table <- rbind(Q7_table,subset_Annos)}
        if (length(intersect(Q8_ref,Q)) != 0){Q8_table <- rbind(Q8_table,subset_Annos)}
        Annos <- Annos + 1
      }
      if(nrow(Q2_table) != 0){
        T2 <- sum(Q2_table$Abund)
      }else{T2 <- 0}
      if(nrow(Q4_table) != 0){
        T4 <- sum(Q4_table$Abund)
      }else{T4 <- 0}
      if(nrow(Q5_table) != 0){
        T5 <- sum(Q5_table$Abund)
      }else{T5 <- 0}
      if(nrow(Q6_table) != 0){
        T6 <- sum(Q6_table$Abund)
      }else{T6 <- 0}
      if(nrow(Q7_table) != 0){
        T7 <- sum(Q7_table$Abund)
      }else{T7 <- 0}
      if(nrow(Q8_table) != 0){
        T8 <- sum(Q8_table$Abund)
      }else{T8 <- 0}
      T <- length(c(T2, T5, T6, T7, T8)[c(T2, T5, T6, T7, T8) > 0])
      if(T4 > 0){dffinal_arrange$'Class'[which(dffinal_arrange$`Substitution modes` == x)] <- "FLA"}
      if(((T4 == 0) & (T >= 4)) == TRUE){
        dffinal_arrange$'Class'[which(dffinal_arrange$`Substitution modes` == x)] <- "ISO"
      }
      Anno <- Anno + 1
    }
    
    
    ## Annotation of OH Positions on Ring A.
    dffinal_unique_A10 = dffinal_unique %>% filter(stringr::str_detect(`Substitution modes`,'A10'))
    for (A10 in 1:nrow(dffinal_unique_A10)){
      x <- dffinal_unique_A10[A10,'Substitution modes']
      subset_A10 <- subset(dffinal_arrange,`Substitution modes` == x)
      subset_A10_A <- data.frame(matrix(ncol=0, nrow=0))
      subset_A10_B <- data.frame(matrix(ncol=0, nrow=0))
      for (A10s in 1:nrow(subset_A10)){
        if('1ï¼?3A+-CO' %in% unlist(strsplit(subset_A10[A10s,'Annotations'],"/")) &
           '1ï¼?4A+' %in% unlist(strsplit(subset_A10[A10s,'Annotations'],"/")) == TRUE){
          subset_A10_A <- rbind(subset_A10_A,subset_A10[A10s,])
        }else if('1ï¼?3A+' %in% unlist(strsplit(subset_A10[A10s,'Annotations'],"/")) == TRUE){
          subset_A10_B <- rbind(subset_A10_B,subset_A10[A10s,])}
        A10s <- A10s + 1
      }
      
      if(nrow(subset_A10_A)== 0){dffinal_arrange$`Substitution modes`[which(dffinal_arrange$`Substitution modes` == x)] <- sub("A10","A10(5/7)",x)}
      if(nrow(subset_A10_B)== 0){dffinal_arrange$`Substitution modes`[which(dffinal_arrange$`Substitution modes` == x)] <- sub("A10","A10(6/8)",x)}
      if((nrow(subset_A10_A) != 0) & (nrow(subset_A10_B) != 0) == TRUE){
        if(unique(subset_A10_A$Abund)/unique(subset_A10_B$Abund) < 0.3){
          dffinal_arrange$`Substitution modes`[which(dffinal_arrange$`Substitution modes` == x)] <- sub("A10","A10(5/7)",x)
        }else if(unique(subset_A10_A$Abund)/unique(subset_A10_B$Abund) >= 0.3){
          dffinal_arrange$`Substitution modes`[which(dffinal_arrange$`Substitution modes` == x)] <- sub("A10","A10(6/8)",x)
        }else{dffinal_arrange$`Substitution modes`[which(dffinal_arrange$`Substitution modes` == x)] <- sub("A10","A10(5/7)",x)}
      }
      
      A10 <- A10 + 1
    }
    
    
    dffinal_unique_A20 = dffinal_unique %>% filter(stringr::str_detect(`Substitution modes`,'A20'))
    for (A20 in 1:nrow(dffinal_unique_A20)){
      x <- dffinal_unique_A20[A20,'Substitution modes']
      subset_A20 <- subset(dffinal_arrange,`Substitution modes` == x)
      subset_A20_A <- data.frame(matrix(ncol=0, nrow=0))
      subset_A20_B <- data.frame(matrix(ncol=0, nrow=0))
      
      for (A20s in 1:nrow(subset_A20)){
        if('1ï¼?3A+-CO' %in% unlist(strsplit(subset_A20[A20s,'Annotations'],"/")) &
           '1ï¼?4A+' %in% unlist(strsplit(subset_A20[A20s,'Annotations'],"/")) == TRUE){
          subset_A20_A <- rbind(subset_A20_A,subset_A20[A20s,])
        }else if('1ï¼?3A+' %in% unlist(strsplit(subset_A20[A20s,'Annotations'],"/")) == TRUE){
          subset_A20_B <- rbind(subset_A20_B,subset_A20[A20s,])}
        A20s <- A20s + 1
      }
      
      if(nrow(subset_A20_A)== 0){dffinal_arrange$`Substitution modes`[which(dffinal_arrange$`Substitution modes` == x)] <- sub("A20","A20(5&7)",x)}
      if(nrow(subset_A20_B)== 0){dffinal_arrange$`Substitution modes`[which(dffinal_arrange$`Substitution modes` == x)] <- sub("A20","A20(6/8&#)",x)}
      if((nrow(subset_A20_A) != 0) & (nrow(subset_A20_B) != 0) == TRUE){
        if(unique(subset_A20_A$Abund)/unique(subset_A20_B$Abund) < 0.3){
          dffinal_arrange$`Substitution modes`[which(dffinal_arrange$`Substitution modes` == x)] <- sub("A20","A20(5&7)",x)
        }else if(unique(subset_A20_A$Abund)/unique(subset_A20_B$Abund) >= 0.3){
          dffinal_arrange$`Substitution modes`[which(dffinal_arrange$`Substitution modes` == x)] <- sub("A20","A20(6/8&#)",x)
        }else{dffinal_arrange$`Substitution modes`[which(dffinal_arrange$`Substitution modes` == x)] <- sub("A20","A20(5&7)",x)}}
      
      
      A20 <- A20 + 1
    }
    
    
    dffinal_all1 <- dffinal_arrange[,c("Compound ID","Substitution modes","sum_abund","Class")]
    dffinal_unique1 <- unique(dffinal_all1)
    dffinal_unique1$Score <- round(((dffinal_unique1$sum_abund)/Abund_all),4)
    dffinal_unique1$"Sample" <- sample
    dffinal_arrange1 <- merge(dffinal_arrange, dffinal_unique1[,c(2,5)], by = "Substitution modes")
    dffinal_arrange1$"Sample" <- sample
    merge_dffinal1 <- rbind(merge_dffinal1,dffinal_arrange1)
    merge_dffinal2 <- rbind(merge_dffinal2,dffinal_unique1)
  }
  pre <- pre + 1
}

write.xlsx(merge_dffinal1,"D:/rdata/SMs_result1.xlsx")
write.xlsx(merge_dffinal2,"D:/rdata/SMs_result2.xlsx")
