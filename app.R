library(shiny)
library(scales)
library(ggplot2)
library(plotly)
# library(seewave)
library(plot.matrix)
library(markdown)
library(rmarkdown)

options(shiny.maxRequestSize = 2000*1024^2)



# Function to create sine waves famyly  

comSin <- function(Wtime,freq){
  f1 = rep(freq,each = length(Wtime))
  Csin <- exp( 1i*2*pi*f1*Wtime )
  Csin1 <- matrix(Csin, nrow = length(Wtime), byrow = F)
  return(Csin1)
}

# Function to create Gausian windows. 

comGau <- function(Wtime,s){
  s1 = rep(s,each = length(Wtime))
  gaus_win  = exp( (-Wtime^2) / (2*s1^2) )
  gaus_win1 <- matrix(gaus_win, nrow = length(Wtime), byrow = F)
  return(gaus_win1)
}

# Set frequency and sine parameters for wavelets


freq = 8:31

# g1 and g2 are the parameters
g1 = 5
g2 = 25
gW   = seq(g1,g2, length.out = length(freq))
s = gW / (2*pi*freq); 


#####################################_________UI____________###################################

ui <- navbarPage("BackAv",
                 
                 
                 #####################_______GENERAL_VIEW_____####################
                 tabPanel("General view", 
                          
                          fluidRow(column = 12,
                                   plotlyOutput("plot"), 
                                   hr(),         
                                   fluidRow(
                                     column( width = 2, offset = 2,
                                             # channel inputs
                                             numericInput(inputId = "EEG_channel",
                                                          label   = "EEG channel",
                                                          value   = 1  ),
                                             numericInput(inputId = "EMG_channel",
                                                          label   = "EMG channel",
                                                          value   = 1 )),
                                     column( width = 5, offset = 2,
                                             # sampling rate
                                             numericInput(inputId = "srate",
                                                          label   = "Sampling Rate",
                                                          value   = 1000 ),
                                             
                                             br(),
                                             fileInput(inputId = "file1", 
                                                       label = "Choose File",
                                                       accept = c(
                                                         "text/plain",
                                                         "text/csv",
                                                         "text/comma-separated-values,text/plain",
                                                         ".csv"))
                                     )
                                     
                                   )
                                   
                          )),
                 
                 
                 
                 ###########################_________TIME_DOMAIN______#####################
                 tabPanel("Time domain",
                          fluidRow(
                            column(width = 2, offset = 10,
                                   textOutput("nseg"))
                          ),
                          br(),
                          hr(), 
                          fluidRow(
                            
                            column = 12,
                            
                            br(),
                            br(),
                            plotlyOutput("plot_time"), 
                            br(),
                            br(),
                            hr(),                  
                            fluidRow(
                              column(width = 4, offset = 1,
                                     h4("Parameters for EMG markers"),
                                     numericInput(inputId = "threshold",
                                                  label   = "Threshold",
                                                  step = 0.01,
                                                  value   = 0.2 ),
                                     numericInput(inputId = "time_after",
                                                  label   = "Time After (in sec)",
                                                  step = 0.01,
                                                  value   = 0.02 ),
                                     numericInput(inputId = "time_before",
                                                  label   = "Time Before (in sec)",
                                                  step = 0.01,
                                                  value   = 0.02)
                              ),
                              column(width = 3, 
                                     br(),
                                     br(),
                                     numericInput(inputId = "after_a",
                                                  label   = "Amplitud After > than",
                                                  step = 0.01,
                                                  value   = 0.15 ),
                                     
                                     numericInput(inputId = "before_a",
                                                  label   = "Amplitud Before < than",
                                                  step = 0.01,
                                                  value   = 0.20 ),
                                     numericInput(inputId = "duration",
                                                  label   = "Burst duration (in sec)",
                                                  step = 0.01,
                                                  value   = 0.40 )
                              ),
                              column(width = 2, 
                                     br(),
                                     br(),
                                     numericInput(inputId = "window",
                                                  label   = "Window (in sec) ",
                                                  step = 0.01,
                                                  value   = 1.2 ),
                                     numericInput(inputId = "onset",
                                                  label   = "Onset (in sec)",
                                                  step = 0.01,
                                                  value   = 1 ),
                                     actionButton("RUN1", "RUN")
                              )
                            )
                          ) 
                          
                 ),
                 ###########################_________Average______#####################
                 tabPanel("Average",
                          column(12,
                                 
                                 plotlyOutput("plot_ave"), 
                                 br(),
                                 hr(),
                                 numericInput(inputId = "baseline",
                                              label   = "baseline correction in sec",
                                              step = 0.01,
                                              value   = .1 ),
                                 actionButton("RUN2","RUN")
                                 
                          )
                 ),
                 ###########################_____Reorder and split______#####################
                 tabPanel("Reorder and split",
                          column(12,
                                 
                                 plotlyOutput("splitp"), 
                                 actionButton("split","Reorder and split")
                                 
                          )
                 )
                 ,
                 ####################### Documentation #######################
                 tabPanel("Documentation",
                          fluidRow(
                            column(8,
                                   includeMarkdown("Documentation.Rmd")
                                   
                                   
                            ))
                          
                 )
                 
                 
                 
)

#####################################________Server___________###################################

server <- function(input, output) {
  
  #### load data
  data <- reactive({
    req(input$file1)
    inFile    <- input$file1
    read.delim(inFile$datapath, header = T, sep = "")
  })
  
  ##### General information
  ext             <- reactive({dim(data())[1]}) # extention of data in pnts
  srate           <- reactive({input$srate})   # to be specify
  tsec            <- reactive({ext()/srate()}) # Extention in seconds
  ttime           <- reactive({seq(1/srate(),tsec(), by = 1/srate())}) #time vector
  threshold       <- reactive({input$threshold})
  time_after      <- reactive({input$time_after * srate()})
  time_before     <- reactive({input$time_before * srate()})
  after_a         <- reactive({input$after_a})
  before_a        <- reactive({input$before_a})
  wind            <- reactive({input$window * srate()})  # window for the average
  onset           <- reactive({input$onset * srate()})   # onset to put the 0   
  timeW           <- reactive({seq(onset()*-1,wind() - onset())}) # time window for the average
  
  # creating wavelets family for the spectrogram 
  
  Wtime           <- reactive(seq(-2,2,by = 1/srate()))  # wavelet time
  half_wav        <- reactive((length(Wtime()) - 1)/2)
  nKern           <- reactive(length(Wtime()));
  
  
  sin_f           <- reactive(t(comSin(Wtime(),freq)))
  gau_f           <- reactive(t(comGau(Wtime(),s)))
  wavelets        <- reactive(sin_f() * gau_f())
  
  zpadD           <- reactive(as.vector( matrix(0L, nrow = 1, ncol = nKern() - 1 ))) # zero padding for data
  
  
  
  
  
  # select ACC and EMG channels
  EEGch <- reactive({input$EEG_channel})
  EMGch <- reactive({input$EMG_channel})
  
  EEG   <- reactive({data()[,EEGch()]})
  EMG   <- reactive({data()[,EMGch()]-mean(data()[,EMGch()])})
  
  # 
  t1 <- reactive({plot_ly(x = ttime(), y = EEG()) %>% add_lines( name = "EEG", line = list(width = .5)) %>% rangeslider()  })
  t2 <- reactive({plot_ly(x = ttime(), y = EMG()) %>% add_lines( name = "EMG", line = list(width = .5))  })
  
  output$plot <- renderPlotly({subplot(t1(),t2(),nrows = 2, shareX = T, shareY = T, titleX = T) })
  
  #####Time Domains#####
  # creating markers
  
  datM <- eventReactive(input$RUN1,{
    
    emg     <- scales::rescale(abs(EMG()))
    
    emg1    <- emg > threshold() 
    emgtrl  <- diff(emg1)
    ons     <- which(emgtrl %in%  1)  # beggening of the burst
    
    
    
    on_after  <- 0;
    on_before <- 0;
    
    for (li in 1:length(ons)) {
      if ((ons[li] - 20) < 1) next
      on_after[li]  <- mean(emg[ons[li]:(ons[li] + time_after())]) 
      on_before[li] <- mean(emg[(ons[li] - time_before()):ons[li]])
    }
    
    ons2 <- ons[which(on_after > after_a() & on_before < before_a() )] 
    
    ons3 <- ons2[which(diff(ons2) > (srate()* input$duration))]
    
    beg    <- ons3 - onset()
    ending <- ons3 + (wind() - onset())
    trial <- data.frame( beg[which(beg > 0)], ending[which( beg > 0)])
    trial
  })
  
  p <- reactive({plot_ly(x = ttime(), y = scales::rescale(abs(EMG()))) %>% add_lines( name = "EMG", line = list(width = .5)) %>% rangeslider()   })
  
  # initiate a line shape object (for each marker)
  
  lines <- reactive({
    
    line <- list(
      type = "line",
      line = list(color = "red"),
      xref = "x",
      yref = "y"
      
    )
    
    l1 <- list()
    
    for (i in 1:dim(datM())[1]) {
      line[["y0"]] <- 0
      line[["y1"]] <- 0.4
      v1 <- datM()[i,1] + onset()
      line[c("x0", "x1")] <- v1/srate()
      l1 <- c(l1, list(line))
    }
    
    l1
  }
  
  
  )
  
  p1 <- reactive(layout(p(), title = 'EMG plus markers', shapes = lines()))
  
  output$plot_time <- renderPlotly({p1()})
  
  output$nseg     <- renderText({paste("Number of markers: ", dim(datM())[1] )})
  
  ## segmentation ##
  
  segDat <- eventReactive(input$RUN2,{
    
    s1 <- matrix(nrow = dim(datM())[1], ncol = wind() + 1)
    s2 <- matrix(nrow = dim(datM())[1], ncol = wind() + 1)
    
    for (ti in 1:dim(datM())[1]) {
      
      s1[ti,] <- EEG()[datM()[ti,1]:datM()[ti,2]]
      s2[ti,] <- EMG()[datM()[ti,1]:datM()[ti,2]]
      
    }
    s3 <- list(s1,s2)
    s3
  })
  
  # baseline correction
  
  EEGpre <- reactive({
    dat <- segDat()[[1]]
    for (ti in 1:dim(datM())[1]) {
      d1 <- mean(dat[ti, 1:(input$baseline*srate())])
      dat[ti,] <- dat[ti,] - d1
    }
    dat
  })
  
  EEGave <- reactive({colMeans(EEGpre())})
  EMGave <- reactive({colMeans(segDat()[[2]])})
  
  EEGp   <- reactive({plot_ly(x = timeW(), y = EEGave()) %>% add_lines( name = "EEG", line = list(width = 1.5)) %>%  layout(yaxis = list(autorange = "reversed"))  })
  EMGp   <- reactive({plot_ly(x = timeW(), y = EMGave()) %>% add_lines( name = "EMG", line = list(width = 1.5)) %>%  layout(yaxis = list(autorange = "reversed"))}) 
  
  
  output$plot_ave <- renderPlotly({subplot(EEGp(),EMGp(), nrows = 2,shareX = T ,shareY = F)})
  
  ############### Split and reorder data ####################
  
  reOrd <- eventReactive(input$split,{
    l1 <- sample(c(1:dim(datM())[1]))
    
    dat <- segDat()[[1]]
    for (ti in 1:dim(datM())[1]) {
      dat[ti,] <- EEGpre()[l1[ti],]
    }
    dat
  })
  
  len2 <- reactive(round(dim(datM())[1]/2))
  
  re1 <- reactive({colMeans(reOrd()[1:len2(),])})
  re2 <- reactive({colMeans(reOrd()[(len2()+1):dim(datM())[1],])})
  
  re1p  <- reactive({plot_ly(x = timeW(), y = re1()) %>% add_lines( name = "First group", line = list(width = 1.5)) %>%  layout(yaxis = list(autorange = "reversed"))  })
  re2p  <- reactive({plot_ly(x = timeW(), y = re2()) %>% add_lines( name = "Second group", line = list(width = 1.5)) %>%  layout(yaxis = list(autorange = "reversed"))  })
  output$splitp <- renderPlotly({subplot(re1p(),re2p(), nrows = 2,shareX = T ,shareY = F)})
  
  
  
  
  
  
}


##########################_______RUN________################################
shinyApp(ui, server)
