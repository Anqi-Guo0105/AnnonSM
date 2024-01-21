rm(list = ls())

# load packages
library(dplyr)
library(readxl)
library(openxlsx)
library(ropls)
library(progress)
library(ggplot2)
library(ggpubr)
library(showtext)


# abundance normalization
# load data
data <- read_excel("D:/rdata/Table S5.xlsx",sheet = "Table S5A")
data[is.na(data)]=0
data_fla_iso <- select(data,-c("Class","Compound ID","Substituent modes", "CE (V)"))
data_fla_iso <- data.frame(data_fla_iso, check.names = FALSE)

# normalization
pb <- progress_bar$new(total = nrow(data_fla_iso))
for (i in 1:nrow(data_fla_iso)) {
  pb$tick()
  Sys.sleep(0.05)
  max_num <- max(data_fla_iso[i,])
  for (j in 1:ncol(data_fla_iso)) {
    data_fla_iso[i,j] <- round((data_fla_iso[i,j]/max_num),4)
  }
}

write.xlsx(data_fla_iso,"D:/rdata/Table S5B.xlsx")


# screen differential CFIs
# load data
data <- read_excel("D:/rdata/Table S5.xlsx",sheet = "Table S5B")
data[is.na(data)]=0
data_fla_iso <- select(data,-c("Class","Compound ID","Substituent modes", "CE (V)"))
data_fla_iso <- data.frame(data_fla_iso, check.names = FALSE)

# extract class results
class_fla_iso = data.matrix(data[, "Class"])

# OPLS-DA
oplsda_fla_iso = opls(data_fla_iso, class_fla_iso, predI = 1, orthoI = NA)

# extract differential CFIs (VIP > 1)
vip_fla_iso <- getVipVn(oplsda_fla_iso)
VIP <- vip_fla_iso[vip_fla_iso > 1] 
VIP

# lollipop chart
VIP <- data.frame(VIP)
VIP$fragname <- rownames(VIP)
VIP_order <- VIP[order(VIP$fragname),]
VIP_order

fragtype = c("Q8","Q8","Q8","Q8","Q8",
             "Q2","Q2","Q2","Q2","Q2","Q2","Q2",
             "Q4","Q4",
             "Q3","Q3",
             "Q7","Q7","Q7",
             "Q5","Q5","Q5",
             "Q1","Q1","Q1","Q1",
             "Q6","Q6","Q6"
)
VIP_order$Type = fragtype

font_add("Arial","arial.ttf")
showtext_auto()

lollipop <- ggdotchart(VIP_order,x = "fragname", y = "VIP", 
                       ylab = FALSE,
                       color = "Type",                                
                       palette = c("#5F9EA0", "#CD7054", "#CD919E", "#9ACD32", "#8B795E", "#FF4500", "#FF8C69", "#3CB371", "#5D478B", "#98FB98", "#FF7F00", "#8B2323"),
                       rotate = TRUE, 
                       add = "segments",                             
                       ggtheme = theme_pubr(),                        
                       group = "fragtype", 
                       sorting = "ascending", 
                       size = 2.5,
                       add.params = list(color = "lightgray", size = 1), 
                       font.label = list(color = "black", 
                                         size = 1, 
                                         vjust = 0.5), 
                       legend.title = ""
                       
)
lollipop


# diagnose six differential fragmentation patterns (Q2 and Q4 - Q8)
# load data (Q1 - Q8)
dataQ <- read_excel("D:/rdata/Table S5.xlsx",sheet = "Table S5C")
dataQ <- dataQ[-1,]
dataQ[is.na(dataQ)]=0
dataQ_fla_iso <- select(dataQ,-c("Class"))
dataQ_fla_iso <- as.data.frame(lapply(dataQ_fla_iso,as.numeric))

# extract class results
classQ_fla_iso = data.matrix(dataQ[, "Class"])

# OPLS-DA
oplsdaQ_fla_iso = opls(dataQ_fla_iso, classQ_fla_iso, predI = 1, orthoI = 3)

# extract six differential fragmentation patterns (VIP > 1)
vipQ_fla_iso <- getVipVn(oplsdaQ_fla_iso)
VIPQ <- vipQ_fla_iso[vipQ_fla_iso > 1] 
VIPQ

