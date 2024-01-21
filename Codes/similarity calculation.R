rm(list = ls())
setwd("D:/rdata")
getwd()

# load packages
library(readxl)
library(tidyr)
library(plyr)
library(dplyr)
library(readr)
library(openxlsx)
library(stringr)

# load libraries
data <- read_excel("CFIL.xlsx",sheet = "CFIL")
Flavones <- subset(data, Class == "Flavones")
IsoFlavones <- subset(data, Class == "IsoFlavones")
Flavonol_alc_C3OH <- subset(data, Class == "Flavonols (C-3OH)")
Flavonol_alc_C3OCH3 <- subset(data, Class == "Flavonols (C-3OCH3)")

# OH_OCH3
OH_OCH3 <- data.frame(matrix(ncol=0, nrow=0))
OH_OCH3 <- rbind(OH_OCH3, Flavones[, c("OHsum","OCH3sum")])
OH_OCH3 <- rbind(OH_OCH3, IsoFlavones[, c("OHsum","OCH3sum")])
OH_OCH3 <- rbind(OH_OCH3, Flavonol_alc_C3OH[, c("OHsum","OCH3sum")])
OH_OCH3 <- rbind(OH_OCH3, Flavonol_alc_C3OCH3[, c("OHsum","OCH3sum")])
OH_OCH3 <- unique(OH_OCH3)


similarity_all <- data.frame(matrix(ncol=0, nrow=0), check.names = FALSE)
for (r in 1:nrow(OH_OCH3)){
  OHs <- OH_OCH3[r,]$OHsum
  OCH3s <- OH_OCH3[r,]$OCH3sum
  
  Flavones_filter <- subset(Flavones, OHsum == OHs & OCH3sum == OCH3s)
  IsoFlavones_filter <- subset(IsoFlavones, OHsum == OHs & OCH3sum == OCH3s)
  Flavonol_alc_C3OH_filter <- subset(Flavonol_alc_C3OH, OHsum == OHs & OCH3sum == OCH3s)
  Flavonol_alc_C3OCH3_filter <- subset(Flavonol_alc_C3OCH3, OHsum == OHs & OCH3sum == OCH3s)
  
  similarity <- data.frame(matrix(ncol=0, nrow=0), check.names = FALSE)
  
  #Flavones&IsoFlavones
  if((nrow(Flavones_filter) != 0) & (nrow(IsoFlavones_filter) != 0)){
    Flavones_label <- unique(Flavones_filter$`Substitution modes`)
    IsoFlavones_label <- unique(IsoFlavones_filter$`Substitution modes`)
    
    for (i in 1:length(Flavones_label)) {
      for (j in 1:length(IsoFlavones_label)) {
        
        L1 <- Flavones_label[i]
        L1_filter <- subset(Flavones_filter, `Substitution modes` == L1)
        L2 <- IsoFlavones_label[j]
        L2_filter <- subset(IsoFlavones_filter, `Substitution modes` == L2)
        
        inter <- intersect(L1_filter$`Cal.m/z of Frag`, L2_filter$`Cal.m/z of Frag`)
        num_inter <- length(inter)
        con <- union(L1_filter$`Cal.m/z of Frag`, L2_filter$`Cal.m/z of Frag`)
        num_con <- length(con)
        ratio <- num_inter/num_con
        rec <- data.frame("OHsum" = c(OHs),"OCH3sum" = c(OCH3s),
                          "SM1" = c(L1), "SM2" = c(L2),
                          "Similarity Score" = c(ratio),check.names = FALSE)
        similarity <- rbind(similarity,rec)
      }    
    }
  }
  
  
  #Flavones&Flavonol_alc_C3OH
  if((nrow(Flavones_filter) != 0) & (nrow(Flavonol_alc_C3OH_filter) != 0)){
    Flavones_label <- unique(Flavones_filter$`Substitution modes`)
    Flavonol_alc_C3OH_label <- unique(Flavonol_alc_C3OH_filter$`Substitution modes`)
    
    for (i in 1:length(Flavones_label)) {
      for (j in 1:length(Flavonol_alc_C3OH_label)) {
        
        L1 <- Flavones_label[i]
        L1_filter <- subset(Flavones_filter, `Substitution modes` == L1)
        L2 <- Flavonol_alc_C3OH_label[j]
        L2_filter <- subset(Flavonol_alc_C3OH_filter, `Substitution modes` == L2)
        
        inter <- intersect(L1_filter$`Cal.m/z of Frag`, L2_filter$`Cal.m/z of Frag`)
        num_inter <- length(inter)
        con <- union(L1_filter$`Cal.m/z of Frag`, L2_filter$`Cal.m/z of Frag`)
        num_con <- length(con)
        ratio <- num_inter/num_con
        rec <- data.frame("OHsum" = c(OHs),"OCH3sum" = c(OCH3s),
                          "SM1" = c(L1), "SM2" = c(L2),
                          "Similarity Score" = c(ratio),check.names = FALSE)
        similarity <- rbind(similarity,rec)
      }    
    }
  }
  
  #Flavones&Flavonol_alc_C3OCH3
  if((nrow(Flavones_filter) != 0) & (nrow(Flavonol_alc_C3OCH3_filter) != 0)){
    Flavones_label <- unique(Flavones_filter$`Substitution modes`)
    Flavonol_alc_C3OCH3_label <- unique(Flavonol_alc_C3OCH3_filter$`Substitution modes`)
    
    for (i in 1:length(Flavones_label)) {
      for (j in 1:length(Flavonol_alc_C3OCH3_label)) {
        
        L1 <- Flavones_label[i]
        L1_filter <- subset(Flavones_filter, `Substitution modes` == L1)
        L2 <- Flavonol_alc_C3OCH3_label[j]
        L2_filter <- subset(Flavonol_alc_C3OCH3_filter, `Substitution modes` == L2)
        
        inter <- intersect(L1_filter$`Cal.m/z of Frag`, L2_filter$`Cal.m/z of Frag`)
        num_inter <- length(inter)
        con <- union(L1_filter$`Cal.m/z of Frag`, L2_filter$`Cal.m/z of Frag`)
        num_con <- length(con)
        ratio <- num_inter/num_con
        rec <- data.frame("OHsum" = c(OHs),"OCH3sum" = c(OCH3s),
                          "SM1" = c(L1), "SM2" = c(L2),
                          "Similarity Score" = c(ratio),check.names = FALSE)
        similarity <- rbind(similarity,rec)
      }    
    }
  }
  
  #IsoFlavones&Flavonol_alc_C3OH
  if((nrow(IsoFlavones_filter) != 0) & (nrow(Flavonol_alc_C3OH_filter) != 0)){
    IsoFlavones_label <- unique(IsoFlavones_filter$`Substitution modes`)
    Flavonol_alc_C3OH_label <- unique(Flavonol_alc_C3OH_filter$`Substitution modes`)
    
    for (i in 1:length(IsoFlavones_label)) {
      for (j in 1:length(Flavonol_alc_C3OH_label)) {
        
        L1 <- IsoFlavones_label[i]
        L1_filter <- subset(IsoFlavones_filter, `Substitution modes` == L1)
        L2 <- Flavonol_alc_C3OH_label[j]
        L2_filter <- subset(Flavonol_alc_C3OH_filter, `Substitution modes` == L2)
        
        inter <- intersect(L1_filter$`Cal.m/z of Frag`, L2_filter$`Cal.m/z of Frag`)
        num_inter <- length(inter)
        con <- union(L1_filter$`Cal.m/z of Frag`, L2_filter$`Cal.m/z of Frag`)
        num_con <- length(con)
        ratio <- num_inter/num_con
        rec <- data.frame("OHsum" = c(OHs),"OCH3sum" = c(OCH3s),
                          "SM1" = c(L1), "SM2" = c(L2),
                          "Similarity Score" = c(ratio),check.names = FALSE)
        similarity <- rbind(similarity,rec)
      }    
    }
  }
  
  #IsoFlavones&Flavonol_alc_C3OCH3
  if((nrow(IsoFlavones_filter) != 0) & (nrow(Flavonol_alc_C3OCH3_filter) != 0)){
    IsoFlavones_label <- unique(IsoFlavones_filter$`Substitution modes`)
    Flavonol_alc_C3OCH3_label <- unique(Flavonol_alc_C3OCH3_filter$`Substitution modes`)
    
    for (i in 1:length(IsoFlavones_label)) {
      for (j in 1:length(Flavonol_alc_C3OCH3_label)) {
        
        L1 <- IsoFlavones_label[i]
        L1_filter <- subset(IsoFlavones_filter, `Substitution modes` == L1)
        L2 <- Flavonol_alc_C3OCH3_label[j]
        L2_filter <- subset(Flavonol_alc_C3OCH3_filter, `Substitution modes` == L2)
        
        inter <- intersect(L1_filter$`Cal.m/z of Frag`, L2_filter$`Cal.m/z of Frag`)
        num_inter <- length(inter)
        con <- union(L1_filter$`Cal.m/z of Frag`, L2_filter$`Cal.m/z of Frag`)
        num_con <- length(con)
        ratio <- num_inter/num_con
        rec <- data.frame("OHsum" = c(OHs),"OCH3sum" = c(OCH3s),
                          "SM1" = c(L1), "SM2" = c(L2),
                          "Similarity Score" = c(ratio),check.names = FALSE)
        similarity <- rbind(similarity,rec)
      }    
    }
  }
  
  #Flavonol_alc_C3OH&Flavonol_alc_C3OCH3
  if((nrow(Flavonol_alc_C3OH_filter) != 0) & (nrow(Flavonol_alc_C3OCH3_filter) != 0)){
    Flavonol_alc_C3OH_label <- unique(Flavonol_alc_C3OH_filter$`Substitution modes`)
    Flavonol_alc_C3OCH3_label <- unique(Flavonol_alc_C3OCH3_filter$`Substitution modes`)
    
    for (i in 1:length(Flavonol_alc_C3OH_label)) {
      for (j in 1:length(Flavonol_alc_C3OCH3_label)) {
        
        L1 <- Flavonol_alc_C3OH_label[i]
        L1_filter <- subset(Flavonol_alc_C3OH_filter, `Substitution modes` == L1)
        L2 <- Flavonol_alc_C3OCH3_label[j]
        L2_filter <- subset(Flavonol_alc_C3OCH3_filter, `Substitution modes` == L2)
        
        inter <- intersect(L1_filter$`Cal.m/z of Frag`, L2_filter$`Cal.m/z of Frag`)
        num_inter <- length(inter)
        con <- union(L1_filter$`Cal.m/z of Frag`, L2_filter$`Cal.m/z of Frag`)
        num_con <- length(con)
        ratio <- num_inter/num_con
        rec <- data.frame("OHsum" = c(OHs),"OCH3sum" = c(OCH3s),
                          "SM1" = c(L1), "SM2" = c(L2),
                          "Similarity Score" = c(ratio),check.names = FALSE)
        similarity <- rbind(similarity,rec)
      }    
    }
  }
  
  similarity_all <- rbind(similarity_all, similarity)
  
}

# write.xlsx(similarity_all,"D:/rdata/Table S6B.xlsx")



# pairwise similarity comparison
# load libraries
data <- read_excel("CFIL.xlsx",sheet = "CFIL")
label_all <- unique(data$`Substitution modes`)

similarity <- data.frame(matrix(ncol=length(label_all), nrow=length(label_all)))
colnames(similarity) <- c(label_all)
rownames(similarity) <- c(label_all)

for (i in 1:length(label_all)) {
  for (j in 1:length(label_all)) {
    L1 <- label_all[i]
    L1_filter <- subset(data, `Substitution modes` == L1)
    L2 <- label_all[j]
    L2_filter <- subset(data, `Substitution modes` == L2)
    
    inter <- intersect(L1_filter$`Cal.m/z of Frag`, L2_filter$`Cal.m/z of Frag`)
    num_inter <- length(inter)
    con <- union(L1_filter$`Cal.m/z of Frag`, L2_filter$`Cal.m/z of Frag`)
    num_con <- length(con)
    ratio <- num_inter/num_con
    similarity[L1, L2] <- ratio
  }
}

# write.xlsx(similarity,"D:/rdata/similarity.xlsx")

res <- data.frame(matrix(ncol=0, nrow=0))
k <- 0
for (i in 1:nrow(similarity)) {
  q <- i + 1
  if(q <= ncol(similarity)){
    for (j in q:ncol(similarity)) {
      k <- k + 1
      res[k, 1] <- label_all[i]
      res[k, 2] <- label_all[j]
      res[k, 3] <- similarity[i, j]
    }
  }
}
res <- read.xlsx("pairwise_comparison.xlsx")
colnames(res) <- c("SM1", "SM2", "Similarity Score")
# write.xlsx(res,"D:/rdata/Table S6A.xlsx")

sheets = list("TableS6A" = res, "TableS6B" = similarity_all)
write.xlsx(sheets,"D:/rdata/Table S6.xlsx")
