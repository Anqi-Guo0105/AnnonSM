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
library(shiny)
library(shinyWidgets)
library(shinyFiles)
library(DT)
library(progress)
library(data.table)

# Progress bar parameters.
options(DT.options = list(pageLength = 5))

df = as.data.frame(
  cbind(
    matrix(round(rnorm(50), 3), 10),
    sample(0:1, 10, TRUE),
    rep(FALSE, 10)
  )
)

df1 = as.data.frame(
  cbind(
    matrix(round(rnorm(50), 3), 10),
    sample(0:1, 10, TRUE),
    rep(FALSE, 10)
  )
)

# Get the default path.
# dir_pre_fla <- paste(dirname(rstudioapi::getActiveDocumentContext()$path), "/pre_fla.rds", sep = "")
dir_pre_fla <- paste(getwd(), "/pre_fla.rds", sep = "")
#dir_pre <- paste(dirname(rstudioapi::getActiveDocumentContext()$path), "/pre.rds", sep = "")
dir_pre <- paste(getwd(), "/pre.rds", sep = "")
# dir_parent <- paste(dirname(rstudioapi::getActiveDocumentContext()$path), "/TPIL.xlsx", sep = "")
# dir_fragment <- paste(dirname(rstudioapi::getActiveDocumentContext()$path), "/CFIL.xlsx", sep = "")


# Define UI for application.
ui <- tagList(
  navbarPage(
    "AnnoSM",
    tags$style(type = 'text/css', '.navbar { 
                           font-family: Arial;
                           font-size: 18px;
                           font-weight: bold;
                            }',

               '.navbar-default .navbar-brand {
                             color: #483D8B;
                             font-family: Arial;
                             font-weight: bold;
                             font-size: 24px;
                             }'),
    
    # Page label "Flavonoids"
    tabPanel(
      "Flavonoids",
      # Sidebar layout with input and output definitions ----
      sidebarLayout(
        # Sidebar panel for inputs ----
        sidebarPanel(
          helpText("Note: 'Flavonoids' module is designed for the skeletons of flavone,  isoflavone, flavonol aglycones and their O-glycosides",
                   style ="font-family: Arial; font-size:12px; font-weight: bold;"),
          
          helpText("Please Import Required Files",
                   style ="font-family: Arial; font-size:16px; font-weight: bold;"),
          
          fileInput("specfile1",label = "Query Spectra(mgf format)", accept = ".mgf"),
          
          fileInput("TPIL1",label = "Precursor Ion Library(csv, xls or xlsx format)", 
                    accept = c(".csv",".xls",".xlsx")),
          
          fileInput("CFIL1",label = "Characteristic Fragment Ion Library(csv, xls or xlsx format)", 
                    accept = c(".csv",".xls",".xlsx")),
          
          helpText("Mass Tolerance",
                   style ="font-family: Arial; font-size:16px; font-weight: bold;"),
          radioButtons("error_type1",
                       label = "Error Type",
                       choices = list("ppm",
                                      "mDa"),
                       selected = "ppm"),
          
          sliderInput("range1_1", label = "Precursor Ion",
                      min = 0, max = 100, step = 5,
                      value = 10),
          
          tags$head(
            tags$style(
              HTML(".shiny-notification {
                 height: 60px;
                 width: 480px;
                 position:fixed;
                 top: calc(93% - 30px);
                 left: calc(72% - 240px);
                 font-size: 100%;
                 text-align: center;
                }
              ")
            )
          ),
          
          sliderInput("range2_1", label = "Fragment Ion",
                      min = 0, max = 100, step = 5,
                      value = 50),
          
          br(),
          
          helpText("Processing",
                   style ="font-family: Arial; font-size:16px; font-weight: bold;"),
          
          checkboxInput("checkbox_pre1", label = "Screening Compounds", value = FALSE),
          
          actionButton(
            inputId = "action_button_1",
            label = "RUN"
          ),
          
        
          downloadButton("download1","Screened Compounds_result.xlsx"),
          
          checkboxInput("checkbox_one1", label = "SMs of One Compound", value = FALSE),
          
          textInput("one_title1", "Enter a selected compound ID('X + num')"),
          
          actionButton(
            inputId = "action_button_2_SM_one1",
            label = "RUN"
          ),
          
          downloadButton("download2_SM_one1","SM_result.xlsx"),
          
          
          checkboxInput("checkbox_all1", label = "SMs of All Compounds", value = FALSE),
          
          actionButton(
            inputId = "action_button_2_SM_all1",
            label = "RUN"
          ),
          
          downloadButton("download2_SM_all1","SMs_result.xlsx"),
        ),
        
        # Sidebar panel for outputs ----
        mainPanel(
          h2("Results",style ="font-family: Arial; font-size:18px; font-weight: bold;"),
          h2("Screened Compounds",style ="font-family: Arial; font-size:15px; font-weight: bold;"),
          DT::dataTableOutput('df'),
          br(),
          h3("SMs result",style ="font-family: Arial; font-size:15px; font-weight: bold;"),
          textOutput("text1"),
          tableOutput("data_output1"),
          conditionalPanel("$('#data_output1').hasClass('recalculating')", 
                           tags$div('Loading ... ')
          ),
          br(),
          textOutput("text2"),
          DT::dataTableOutput('df1')
        )
      )
    ),
    
    # Page label "Others (customized)"
    tabPanel(
      "Others (customized)",
      # Sidebar layout with input and output definitions ---- 
      sidebarLayout(
        # Sidebar panel for inputs ----
        sidebarPanel(
          helpText("Note: The 'Others (customized)' module is intended for annotating SMs for other compound classes，and researchers need to first build customized TPIL and CFIL",
                   style ="font-family: Arial; font-size:12px; font-weight: bold;"),
          
          
          helpText("Please Import Required Files",
                   style ="font-family: Arial; font-size:18px; font-weight: bold;"),
          
          fileInput("specfile2",label = "Query Spectra(mgf format)", accept = ".mgf"),
          
          fileInput("TPIL2",label = "Precursor Ion Library(csv, xls or xlsx format)", 
                    accept = c(".csv",".xls",".xlsx")),
          
          fileInput("CFIL2",label = "Characteristic Fragment Ion Library(csv, xls or xlsx format)", 
                    accept = c(".csv",".xls",".xlsx")),
          
          helpText("Mass Tolerance",
                   style ="font-family: Arial; font-size:16px; font-weight: bold;"),
          radioButtons("error_type2",
                       label = "Error Type",
                       choices = list("ppm",
                                      "mDa"),
                       selected = "ppm"),
          
          sliderInput("range1_2", label = "Precursor Ion",
                      min = 0, max = 100, step = 5,
                      value = 10),
          
          tags$head(
            tags$style(
              HTML(".shiny-notification {
             height: 60px;
             width: 480px;
             position:fixed;
             top: calc(93% - 30px);
             left: calc(72% - 240px);
             font-size: 100%;
             text-align: center;
            }
          ")
            )
          ),
          
          sliderInput("range2_2", label = "Fragment Ion",
                      min = 0, max = 100, step = 5,
                      value = 50),
          
          br(),
          
          helpText("Processing",
                   style ="font-family: Arial; font-size:16px; font-weight: bold;"),
          
          checkboxInput("checkbox_pre2", label = "Screening Compounds", value = FALSE),
          
          actionButton(
            inputId = "action_button_2",
            label = "RUN"
          ),
          
          downloadButton("download2","Screened Compounds_result.xlsx"),
          
          checkboxInput("checkbox_one2", label = "SMs of One Compound", value = FALSE),
          
          textInput("one_title2", "Enter a selected compound ID('X + num')"),
          
          actionButton(
            inputId = "action_button_2_SM_one2",
            label = "RUN"
          ),
          
          downloadButton("download2_SM_one2","SM_result.xlsx"),
          
          
          checkboxInput("checkbox_all2", label = "SMs of All Compounds", value = FALSE),
          
          actionButton(
            inputId = "action_button_2_SM_all2",
            label = "RUN"
          ),
          
          downloadButton("download2_SM_all2","SMs_result.xlsx"),
        ),
        
        # Sidebar panel for outputs ----
        mainPanel(
          h2("Results",style ="font-family: Arial; font-size:20px; font-weight: bold;"),
          h2("Screened Compounds",style ="font-family: Arial; font-size:15px; font-weight: bold;"),
          DT::dataTableOutput('df2'),
          br(),
          h3("SMs result",style ="font-family: Arial; font-size:15px; font-weight: bold;"),
          textOutput("text3"),
          tableOutput("data_output2"),
          conditionalPanel("$('#data_output2').hasClass('recalculating')", 
                           tags$div('Loading ... ')
          ),
          br(),
          textOutput("text4"),
          DT::dataTableOutput('df3')
        )
      )
    )
  )
)



# Define server logic required.
server <- function(input, output, session){
  
  #common result parameter
  options(shiny.maxRequestSize=100*1024^2)
  value <- reactiveVal(0)
  
  # Flavonoids
  output$text1 <- renderText({
    "The SMs of one selected compound will be displayed here."
  })
  
  output$text2 <- renderText({
    "The SMs of all screened compounds will be displayed here."
  })
  
  # Other Categories
  output$text3 <- renderText({
    "The SMs of one selected compound will be displayed here."
  })
  
  output$text4 <- renderText({
    "The SMs of all screened compounds will be displayed here."
  })
  
  
  # Flavonoids:precursor filt result
  precursorfilter_result_fla <- eventReactive(input$action_button_1,
                                              precur.filt.com.fla()
  )
  
  output$df <- DT::renderDataTable({
    precursorfilter_result_fla()
  })
  
  # Other Categories:precursor filt result
  precursorfilter_result <- eventReactive(input$action_button_2,
                                          precur.filt.com()
  )
  
  output$df2 <- DT::renderDataTable({
    precursorfilter_result()
  })
  
  # Read origin spec file.
  ## Flavonoids
  read.spec.com.fla <- reactive({
    validate(
      need(input$specfile1 == "", "Please select a complex sample file.")
    )
    input$specfile1
  })
  
  ## Other Categories
  read.spec.com <- reactive({
    validate(
      need(input$specfile2 == "", "Please select a complex sample file.")
    )
    input$specfile2
  })
  
  
  # Target compounds screening.
  ## Flavonoids
  preprocess.spec.com.fla <- reactive({
    
    error_parent_com <- input$range1_1
    db <- readMgfData(read.spec.com.fla()$datapath)
    
    TPIL_path_com <- input$TPIL1
    ext_TPIL1 <- tools::file_ext(TPIL_path_com)
    if (ext_TPIL1 == "csv"){
      precursorion <- read_csv(input$TPIL1$datapath)
    }else if(ext_TPIL1 == "xls" | ext_TPIL1 == "xlsx"){
      precursorion <- read_excel(input$TPIL1$datapath)
    }
    
    if(input$checkbox_pre1 == TRUE){
      precursorion$cal.m.z.H <- round(precursorion$cal.m.z.H,4)
      precursorion_filter <- data.frame(matrix(ncol=0, nrow=0),check.names = FALSE)
      
      withProgress(message = 'Calculation in progress...', value = 0,detail="0%", {
        for (k in 1:length(db@assayData)){
          Sys.sleep(0.5)
          newValue <- value() + 1
          value(newValue)
          incProgress(1/length(db@assayData),detail = paste0(round(k/length(db@assayData)*100,2),"%"))
          
          title <- paste("X", k, sep = "")
          premz <- round(as.numeric(db@assayData[[title]]@precursorMz),4)
          preintensity <- round(as.numeric(db@assayData[[title]]@precursorIntensity),4)
          rt <- round(as.numeric(db@assayData[[title]]@rt),4)
          rec <- data.frame("Compound ID" = c(title),"Exp.m/z" = c(premz),"Intensity" = c(preintensity),"RT" = c(rt),check.names = FALSE)
          
          ### error_type = ppm
          if(as.factor(input$error_type1) == "ppm"){
            precursorion$ppm <- abs((premz-precursorion$cal.m.z.H)/precursorion$cal.m.z.H)*1000000
            precursorion_filter1 <- precursorion %>% dplyr::filter(ppm <= error_parent_com)
          }
          ### error_type = ppm
          if(as.factor(input$error_type1) == "mDa"){
            precursorion_filter1 <- precursorion %>% dplyr::filter(cal.m.z.H > (premz - (error_parent_com*0.001+0.0001)) & cal.m.z.H < (premz + (error_parent_com*0.001+0.0001)))
          }
          
          if (nrow(precursorion_filter1) == 0){
            next
          }
          sub_frag <- data.frame("m/z (Exp)" = c(db@assayData[[title]]@mz),"Abund" = c(db@assayData[[title]]@intensity),check.names = FALSE)
          sub_frag$digitsub <- round(sub_frag$'m/z (Exp)',4)
          if (nrow(precursorion_filter1) == 1){precursorion_filter <- rbind(precursorion_filter,merge(rec,precursorion_filter1))
          }else if((nrow(precursorion_filter1) != 1) &
                   (nrow(precursorion_filter1) != 0) == TRUE ){
            
            precursorion_filter2 <- data.frame(matrix(ncol=0, nrow=0),check.names = FALSE)
            precursorion1 <- precursorion[1:66,]
            
            for (s in 1:nrow(sub_frag)){
              digitsub <- sub_frag[s,"digitsub"]
              precursorion1$ppm <- abs((digitsub-precursorion1$cal.m.z.H)/precursorion1$cal.m.z.H)*1000000
              pre_filter1 <- precursorion1 %>% dplyr::filter(ppm <= error_parent_com)
              precursorion_filter2 <- rbind(precursorion_filter2,pre_filter1)
              s = s+1
            }
            
            if (nrow(precursorion_filter2) == 0){precursorion_filter <- rbind(precursorion_filter,merge(rec,precursorion_filter1))
            }else{
              precursorion_filter3 <- data.frame(matrix(ncol=0, nrow=0),check.names = FALSE)
              for (t in 1:nrow(precursorion_filter2)){
                OHs <- precursorion_filter2[t,]$OHsum
                OCH3s <- precursorion_filter2[t,]$OCH3sum
                pre_filter2 <- subset(precursorion_filter1,OHsum == OHs & OCH3sum == OCH3s)
                precursorion_filter3 <- rbind(precursorion_filter3,merge(rec,pre_filter2))
                t = t+1
              }
              if(nrow(precursorion_filter3) == 0){
                precursorion_filter <- rbind(precursorion_filter, merge(rec,precursorion_filter1))
              }else{
                precursorion_filter <- rbind(precursorion_filter,precursorion_filter3)
              }
            }
          }
          k <- k + 1
        }
        Sys.sleep(0.5)
      })
      precursorion_filter
    }
  })
  
  ## Other Categories
  preprocess.spec.com <- reactive({
    
    error_parent_com <- input$range1_2
    db <- readMgfData(read.spec.com()$datapath)
    
    TPIL_path_com <- input$TPIL2
    ext_TPIL2 <- tools::file_ext(TPIL_path_com)
    if (ext_TPIL2 == "csv"){
      precursorion <- read_csv(input$TPIL2$datapath)
    }else if(ext_TPIL2 == "xls" | ext_TPIL2 == "xlsx"){
      precursorion <- read_excel(input$TPIL2$datapath)
    }
    
    if(input$checkbox_pre2 == TRUE){
      precursorion$cal.m.z.H <- round(precursorion$cal.m.z.H,4)
      precursorion_filter <- data.frame(matrix(ncol=0, nrow=0),check.names = FALSE)
      
      withProgress(message = 'Calculation in progress...', value = 0,detail="0%", {
        for (k in 1:length(db@assayData)){
          Sys.sleep(0.5)
          newValue <- value() + 1
          value(newValue)
          incProgress(1/length(db@assayData),detail = paste0(round(k/length(db@assayData)*100,2),"%"))
          
          title <- paste("X", k, sep = "")
          premz <- round(as.numeric(db@assayData[[title]]@precursorMz),4)
          preintensity <- round(as.numeric(db@assayData[[title]]@precursorIntensity),4)
          rt <- round(as.numeric(db@assayData[[title]]@rt),4)
          rec <- data.frame("Compound ID" = c(title),"Exp.m/z" = c(premz),"Intensity" = c(preintensity),"RT" = c(rt),check.names = FALSE)
          
          ### error_type = ppm
          if(as.factor(input$error_type2) == "ppm"){
            precursorion$ppm <- abs((premz-precursorion$cal.m.z.H)/precursorion$cal.m.z.H)*1000000
            precursorion_filter1 <- precursorion %>% dplyr::filter(ppm <= error_parent_com)
          }
          ### error_type = ppm
          if(as.factor(input$error_type2) == "mDa"){
            precursorion_filter1 <- precursorion %>% dplyr::filter(cal.m.z.H > (premz - (error_parent_com*0.001+0.0001)) & cal.m.z.H < (premz + (error_parent_com*0.001+0.0001)))
          }
          
          if (nrow(precursorion_filter1) == 0){
            next
          }else{
            precursorion_filter <- rbind(precursorion_filter,merge(rec,precursorion_filter1))
          }
          k <- k + 1
        }
        Sys.sleep(0.5)
      })
      precursorion_filter
    }
  })
  
  
  # Download screening result.
  ## Flavonoids
  precur.filt.com.fla <- reactive({
    precursorion_filter_1_fla <- preprocess.spec.com.fla()
    saveRDS(precursorion_filter_1_fla,dir_pre_fla)
    precursorion_filter_result_fla <- precursorion_filter_1_fla[,c("Compound ID","Exp.m/z","Intensity","RT","Formulas","OHsum","OCH3sum","Sugar and phenylpropionyl units")] %>% distinct()
    precursorion_filter_result_fla
  })
  
  output$download1 <- downloadHandler(
    filename = "Screened Compounds_result.xlsx",
    content = function(file){
      openxlsx::write.xlsx(precursorfilter_result_fla(), file)
    }
  )
  
  
  ## Other Categories
  precur.filt.com <- reactive({
    precursorion_filter_1 <- preprocess.spec.com()
    saveRDS(precursorion_filter_1,dir_pre)
    precursorion_filter_result <- precursorion_filter_1[,c("Compound ID","Exp.m/z","Intensity","RT","Formulas","OHsum","OCH3sum","Sugar and phenylpropionyl units")] %>% distinct()
    precursorion_filter_result
  })
  
  output$download2 <- downloadHandler(
    filename = "Screened Compounds_result.xlsx",
    content = function(file){
      openxlsx::write.xlsx(precursorfilter_result(), file)
    }
  )
  
  
  
  # Substituent mode(SM) matching.
  ### one candidate compound
  ## Flavonoids
  sm_result_one_fla <- eventReactive(input$action_button_2_SM_one1,
                                     sm.spec.com.one.fla()
  )
  
  output$data_output1 <- renderTable({
    sm_result_one_fla()
  })
  
  sm.spec.com.one.fla <- reactive({
    
    error_fragment <- input$range2_1
    db <- readMgfData(read.spec.com.fla()$datapath)
    
    CFIL_path_com <- input$CFIL1
    ext_CFIL1 <- tools::file_ext(CFIL_path_com)
    if (ext_CFIL1 == "csv"){
      CFIL <- read_csv(input$CFIL1$datapath)
    }else if(ext_CFIL1 == "xls" | ext_CFIL1 == "xlsx"){
      CFIL <- read_excel(input$CFIL1$datapath)
    }
    
    if(input$checkbox_one1 == TRUE){
      
      one_title1 <- toupper(input$one_title1)
      sub_frag <- data.frame('m/z (Exp)' = c(db@assayData[[one_title1]]@mz),'Abund' = c(db@assayData[[one_title1]]@intensity),check.names = FALSE)
      sub_frag$digitsub <- round(sub_frag$'m/z (Exp)',4)
      Abund_all <- sum(sub_frag$Abund)
      
      
      dffinal <- data.frame(matrix(ncol=0, nrow=0),check.names = FALSE)
      precursorion_filter_all <- readRDS(dir_pre_fla)
      precursorion_filter <- precursorion_filter_all %>% dplyr::filter(precursorion_filter_all$'Compound ID' == one_title1)
      for (prenum in 1:nrow(precursorion_filter)){
        combinedCFIL <- data.frame(matrix(ncol=0, nrow=0),check.names = FALSE)
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
      
      
      
      ## Classification of SMs based on CFIs.
      dffinal_arrange$'Class' <- " "
      
      Q2_ref <- c("[M+H-B ring]+-CO","[M+H-B ring]+-2×CO","[M+H-B ring]+-3×CO","[M+H-B ring]+-4×CO",
                  "[M+H-B ring]+-CH3-CO","[M+H-B ring]+-CH3-2×CO","[M+H-B ring]+-CH3-3×CO")
      Q4_ref <- c("0，2B+","0，2B+-CO")
      Q5_ref <- c("0，4B+-CO","0，4B+-2×CO","0，4B+-3×CO")
      Q6_ref <- c("1，4B+","1，4B+-CO","1，4B+--2×CO")
      Q7_ref <- c("0，3B+","0，3B+-CO","0，3B+-2×CO")
      Q8_ref <- c("[B ring]+","[B ring]+-CO","[B ring]+-CH3","[B ring]+-CH3-CO","[B ring]+-CH3-2×CO")
      
      
      for (Anno in 1:nrow(dffinal_unique)){
        x <- dffinal_unique[Anno,'Substitution modes']
        subset_Anno <- subset(dffinal_arrange,`Substitution modes` == x)
        Q2_table <- data.frame(matrix(ncol=0, nrow=0),check.names = FALSE)
        Q4_table <- data.frame(matrix(ncol=0, nrow=0),check.names = FALSE)
        Q5_table <- data.frame(matrix(ncol=0, nrow=0),check.names = FALSE)
        Q6_table <- data.frame(matrix(ncol=0, nrow=0),check.names = FALSE)
        Q7_table <- data.frame(matrix(ncol=0, nrow=0),check.names = FALSE)
        Q8_table <- data.frame(matrix(ncol=0, nrow=0),check.names = FALSE)
        Q_table <- data.frame(matrix(ncol=0, nrow=0),check.names = FALSE)
        
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
        subset_A10_A <- data.frame(matrix(ncol=0, nrow=0),check.names = FALSE)
        subset_A10_B <- data.frame(matrix(ncol=0, nrow=0),check.names = FALSE)
        for (A10s in 1:nrow(subset_A10)){
          if('1，3A+-CO' %in% unlist(strsplit(subset_A10[A10s,'Annotations'],"/")) &
             '1，4A+' %in% unlist(strsplit(subset_A10[A10s,'Annotations'],"/")) == TRUE){
            subset_A10_A <- rbind(subset_A10_A,subset_A10[A10s,])
          }else if('1，3A+' %in% unlist(strsplit(subset_A10[A10s,'Annotations'],"/")) == TRUE){
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
        subset_A20_A <- data.frame(matrix(ncol=0, nrow=0),check.names = FALSE)
        subset_A20_B <- data.frame(matrix(ncol=0, nrow=0),check.names = FALSE)
        
        for (A20s in 1:nrow(subset_A20)){
          if('1，3A+-CO' %in% unlist(strsplit(subset_A20[A20s,'Annotations'],"/")) &
             '1，4A+' %in% unlist(strsplit(subset_A20[A20s,'Annotations'],"/")) == TRUE){
            subset_A20_A <- rbind(subset_A20_A,subset_A20[A20s,])
          }else if('1，3A+' %in% unlist(strsplit(subset_A20[A20s,'Annotations'],"/")) == TRUE){
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
      
      
      dffinal_all1 <- dffinal_arrange[,c("Substitution modes","sum_abund","Class")]
      dffinal_unique1 <- unique(dffinal_all1)
      dffinal_unique1$Score <- (dffinal_unique1$sum_abund)/Abund_all
      dffinal_unique1 <- plyr::rename(dffinal_unique1, c("Substitution modes" = "Substituent Modes"))
      dffinal_unique1 <- dffinal_unique1[,c(1:2,4,3)]
      dffinal_unique1
    }
  })
  
  output$download2_SM_one1 <- downloadHandler(
    filename = "SM_result.xlsx",
    content = function(file){
      openxlsx::write.xlsx(sm_result_one_fla(), file)
    }
  )
  
  
  ## Other Categories
  sm_result_one <- eventReactive(input$action_button_2_SM_one2,
                                 sm.spec.com.one()
  )
  
  output$data_output2 <- renderTable({
    sm_result_one()
  })
  
  sm.spec.com.one <- reactive({
    
    error_fragment <- input$range2_2
    db <- readMgfData(read.spec.com()$datapath)
    
    CFIL_path_com <- input$CFIL2
    ext_CFIL2 <- tools::file_ext(CFIL_path_com)
    if (ext_CFIL2 == "csv"){
      CFIL <- read_csv(input$CFIL2$datapath)
    }else if(ext_CFIL2 == "xls" | ext_CFIL2 == "xlsx"){
      CFIL <- read_excel(input$CFIL2$datapath)
    }
    
    if(input$checkbox_one2 == TRUE){
      
      one_title2 <- toupper(input$one_title2)
      sub_frag <- data.frame('m/z (Exp)' = c(db@assayData[[one_title2]]@mz),'Abund' = c(db@assayData[[one_title2]]@intensity),check.names = FALSE)
      sub_frag$digitsub <- round(sub_frag$'m/z (Exp)',4)
      Abund_all <- sum(sub_frag$Abund)
      
      
      dffinal <- data.frame(matrix(ncol=0, nrow=0),check.names = FALSE)
      precursorion_filter_all <- readRDS(dir_pre)
      precursorion_filter <- precursorion_filter_all %>% dplyr::filter(precursorion_filter_all$'Compound ID' == one_title2)
      for (prenum in 1:nrow(precursorion_filter)){
        combinedCFIL <- data.frame(matrix(ncol=0, nrow=0),check.names = FALSE)
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
      dffinal_all <- plyr::rename(dffinal_all, c("Substitution modes" = "Substituent Modes"))
      dffinal_unique <- unique(dffinal_all)
      dffinal_unique
    }
  })
  
  output$download2_SM_one2 <- downloadHandler(
    filename = "SM_result.xlsx",
    content = function(file){
      openxlsx::write.xlsx(sm_result_one(), file)
    }
  )
  
  
  
  ### all candidates result
  ## Flavonoids
  outdt.com.fla <- eventReactive(input$action_button_2_SM_all1,
                                 sm.spec.com.all.fla()
  )
  
  output$df1 <- DT::renderDataTable({
    outdt.com.fla()
  })
  
  
  sm.spec.com.all.fla <- reactive({
    error_fragment <- input$range2_1
    db <- readMgfData(read.spec.com.fla()$datapath)
    
    CFIL_path_com <- input$CFIL1
    ext_CFIL1 <- tools::file_ext(CFIL_path_com)
    if (ext_CFIL1 == "csv"){
      CFIL <- read.csv(input$CFIL1$datapath)
    }else if(ext_CFIL1 == "xls" | ext_CFIL1 == "xlsx"){
      CFIL <- read_excel(input$CFIL1$datapath)
    }
    
    if(input$checkbox_all1 == TRUE){
      
      merge_dffinal <- data.frame(matrix(ncol=0, nrow=0),check.names = FALSE)
      precursorion_filter <- readRDS(dir_pre_fla)
      
      withProgress(message = 'Calculation in progress', value = 0,detail="0%", {
        for (pre in 1:length(unique(precursorion_filter$"Compound ID"))){
          Sys.sleep(0.5)
          newValue <- value() + 1
          value(newValue)
          incProgress(1/length(unique(precursorion_filter$"Compound ID")),
                      detail = paste0(round(pre/length(unique(precursorion_filter$"Compound ID"))*100,2),"%"))
          
          title <- unique(precursorion_filter$"Compound ID")[pre]
          precursorion_filter3 <- subset(precursorion_filter,precursorion_filter$"Compound ID" == title)
          sub_frag <- data.frame("m/z (Exp)" = c(db@assayData[[title]]@mz),"Abund" = c(db@assayData[[title]]@intensity),check.names = FALSE)
          sub_frag$digitsub <- round(sub_frag$'m/z (Exp)',4)
          Abund_all <- sum(sub_frag$Abund)
          
          dffinal <- data.frame(matrix(ncol=0, nrow=0),check.names = FALSE)
          for (prenum in 1:nrow(precursorion_filter3)){
            dfEND <- data.frame(matrix(ncol=0, nrow=0),check.names = FALSE)
            
            combinedCFIL <- data.frame(matrix(ncol=0, nrow=0),check.names = FALSE)
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
            
            Q2_ref <- c("[M+H-B ring]+-CO","[M+H-B ring]+-2×CO","[M+H-B ring]+-3×CO","[M+H-B ring]+-4×CO",
                        "[M+H-B ring]+-CH3-CO","[M+H-B ring]+-CH3-2×CO","[M+H-B ring]+-CH3-3×CO")
            Q4_ref <- c("0，2B+","0，2B+-CO")
            Q5_ref <- c("0，4B+-CO","0，4B+-2×CO","0，4B+-3×CO")
            Q6_ref <- c("1，4B+","1，4B+-CO","1，4B+--2×CO")
            Q7_ref <- c("0，3B+","0，3B+-CO","0，3B+-2×CO")
            Q8_ref <- c("[B ring]+","[B ring]+-CO","[B ring]+-CH3","[B ring]+-CH3-CO","[B ring]+-CH3-2×CO")
            
            for (Anno in 1:nrow(dffinal_unique)){
              x <- dffinal_unique[Anno,'Substitution modes']
              subset_Anno <- subset(dffinal_arrange,`Substitution modes` == x)
              Q2_table <- data.frame(matrix(ncol=0, nrow=0),check.names = FALSE)
              Q4_table <- data.frame(matrix(ncol=0, nrow=0),check.names = FALSE)
              Q5_table <- data.frame(matrix(ncol=0, nrow=0),check.names = FALSE)
              Q6_table <- data.frame(matrix(ncol=0, nrow=0),check.names = FALSE)
              Q7_table <- data.frame(matrix(ncol=0, nrow=0),check.names = FALSE)
              Q8_table <- data.frame(matrix(ncol=0, nrow=0),check.names = FALSE)
              Q_table <- data.frame(matrix(ncol=0, nrow=0),check.names = FALSE)
              
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
              subset_A10_A <- data.frame(matrix(ncol=0, nrow=0),check.names = FALSE)
              subset_A10_B <- data.frame(matrix(ncol=0, nrow=0),check.names = FALSE)
              for (A10s in 1:nrow(subset_A10)){
                if('1，3A+-CO' %in% unlist(strsplit(subset_A10[A10s,'Annotations'],"/")) &
                   '1，4A+' %in% unlist(strsplit(subset_A10[A10s,'Annotations'],"/")) == TRUE){
                  subset_A10_A <- rbind(subset_A10_A,subset_A10[A10s,])
                }else if('1，3A+' %in% unlist(strsplit(subset_A10[A10s,'Annotations'],"/")) == TRUE){
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
              subset_A20_A <- data.frame(matrix(ncol=0, nrow=0),check.names = FALSE)
              subset_A20_B <- data.frame(matrix(ncol=0, nrow=0),check.names = FALSE)
              
              for (A20s in 1:nrow(subset_A20)){
                if('1，3A+-CO' %in% unlist(strsplit(subset_A20[A20s,'Annotations'],"/")) &
                   '1，4A+' %in% unlist(strsplit(subset_A20[A20s,'Annotations'],"/")) == TRUE){
                  subset_A20_A <- rbind(subset_A20_A,subset_A20[A20s,])
                }else if('1，3A+' %in% unlist(strsplit(subset_A20[A20s,'Annotations'],"/")) == TRUE){
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
            dffinal_unique1 <- plyr::rename(dffinal_unique1, c("Substitution modes" = "Substituent Modes"))
            dffinal_unique1 <- dffinal_unique1[,c(1:3,5,4)]
            dffinal_unique1
            merge_dffinal <- rbind(merge_dffinal,dffinal_unique1)
          }
          pre <- pre + 1
        }
      })
      merge_dffinal
      rownames(merge_dffinal) <- 1:nrow(merge_dffinal)
      merge_dffinal
    }
  })
  
  
  output$download2_SM_all1 <- downloadHandler(
    filename = "SMs_result.xlsx",
    content = function(file){
      openxlsx::write.xlsx(outdt.com.fla(), file)
    }
  )
  
  
  ## Other Categories
  outdt.com <- eventReactive(input$action_button_2_SM_all2,
                             sm.spec.com.all()
  )
  
  output$df3 <- DT::renderDataTable({
    outdt.com()
  })
  
  
  sm.spec.com.all <- reactive({
    error_fragment <- input$range2_2
    db <- readMgfData(read.spec.com()$datapath)
    
    CFIL_path_com <- input$CFIL2
    ext_CFIL2 <- tools::file_ext(CFIL_path_com)
    if (ext_CFIL2 == "csv"){
      CFIL <- read.csv(input$CFIL2$datapath)
    }else if(ext_CFIL2 == "xls" | ext_CFIL2 == "xlsx"){
      CFIL <- read_excel(input$CFIL2$datapath)
    }
    
    if(input$checkbox_all2 == TRUE){
      
      merge_dffinal <- data.frame(matrix(ncol=0, nrow=0),check.names = FALSE)
      precursorion_filter <- readRDS(dir_pre)
      
      withProgress(message = 'Calculation in progress', value = 0,detail="0%", {
        for (pre in 1:length(unique(precursorion_filter$"Compound ID"))){
          Sys.sleep(0.5)
          newValue <- value() + 1
          value(newValue)
          incProgress(1/length(unique(precursorion_filter$"Compound ID")),
                      detail = paste0(round(pre/length(unique(precursorion_filter$"Compound ID"))*100,2),"%"))
          
          title <- unique(precursorion_filter$"Compound ID")[pre]
          precursorion_filter3 <- subset(precursorion_filter,precursorion_filter$"Compound ID" == title)
          sub_frag <- data.frame("m/z (Exp)" = c(db@assayData[[title]]@mz),"Abund" = c(db@assayData[[title]]@intensity),check.names = FALSE)
          sub_frag$digitsub <- round(sub_frag$'m/z (Exp)',4)
          Abund_all <- sum(sub_frag$Abund)
          
          dffinal <- data.frame(matrix(ncol=0, nrow=0),check.names = FALSE)
          for (prenum in 1:nrow(precursorion_filter3)){
            dfEND <- data.frame(matrix(ncol=0, nrow=0),check.names = FALSE)
            
            combinedCFIL <- data.frame(matrix(ncol=0, nrow=0),check.names = FALSE)
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
            dffinal_all <- dffinal_arrange[,c("Compound ID", "Substitution modes","sum_abund")]
            dffinal_all <- plyr::rename(dffinal_all, c("Substitution modes" = "Substituent Modes"))
            dffinal_unique <- unique(dffinal_all)
            merge_dffinal <- rbind(merge_dffinal,dffinal_unique)
          }
          pre <- pre + 1
        }
      })
      merge_dffinal
      rownames(merge_dffinal) <- 1:nrow(merge_dffinal)
      merge_dffinal
    }
  })
  
  
  output$download2_SM_all2 <- downloadHandler(
    filename = "SMs_result.xlsx",
    content = function(file){
      openxlsx::write.xlsx(outdt.com(), file)
    }
  )
}

# Run the application.
shinyApp(ui = ui, server = server) 