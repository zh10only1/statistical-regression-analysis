library(shiny)
library(markdown)
library(shinythemes)
library(sn)


data <- read.csv("fertility_rate.csv")
data_origin <- subset(data, select = -c(1,1))

ui <- navbarPage("Fertility Rate",
                 theme = shinytheme("flatly"),
                 tabPanel("Table",
                          DT::dataTableOutput("table")
                 ),
                 
                 tabPanel("Analysis", 
                          selectInput("datalist", "Choose a year:",
                                      c("All",rownames(t(data_origin)))),
                          column(6,
                                 plotOutput("hist1",width="100%")
                          ),
                          column(3, textInput("minval", 
                                              label = "Probability that fertility rate is more than", 
                                              value = "", 
                                              width = "100%",
                                              placeholder = "Enter fertility rate")),
                          column(3, textInput("maxval", 
                                              label = "Probability that fertility rate is less than", 
                                              value = "", 
                                              width = "100%",
                                              placeholder = "Enter fertility rate")),
                          column(6,
                                 verbatimTextOutput("summary1"),
                                 textOutput("standD"),
                                 textOutput("IQR")
                          ),column(6,
                                   textOutput("probdist")
                          ),
                          
                 ),
                 tabPanel("Regression",
                          selectInput("country", "Choose a Country:",
                                      data[,1]),
                          column(6,
                                 plotOutput("linearplot",width="100%")
                          ),
                          column(3, textInput("year", 
                                              label = "Enter a year to predict fertility rate(1920 onwards)", 
                                              value = "", 
                                              width = "100%",
                                              placeholder = "Enter year")),
                          column(6,
                                 textOutput("corelation")
                          ),
                          column(6,
                                 textOutput("prediction")
                          ),
                 ),
                 
                 
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$table <- DT::renderDataTable({
    DT::datatable(data)
  })
  
  m <- mean(t(subset(data, select = -c(1,1))[2,]))
  row_num <- length(data[,1])
  
  output$linearplot <- renderPlot({
    country <- which(data==input$country)
    Vecy <- t(data_origin)[,country]
    Vecx <- c(1960:2020)
    plot(Vecy~Vecx,main=("Plot"),xlab=c("Year"),ylab = "Fertility Rate")
    linmod=lm(Vecy~Vecx)
    abline(linmod)
  })
  
  output$corelation <- renderText({
    country <- which(data==input$country)
    Vecy <- t(data_origin)[,country]
    Vecx <- c(1960:2020)
    plot(Vecy~Vecx)
    linmod=lm(Vecy~Vecx)
    c("Corelation = ",cor(Vecx,Vecy))
  })
  
  
  output$prediction <- renderText({
      country <- which(data==input$country)
      year <- as.numeric(input$year)
    if(input$year!=""&&year>=1920){
      Vecy <- t(data_origin)[,country]
      Vecx <- c(1960:2020)
      plot(Vecy~Vecx)
      linmod=lm(Vecy~Vecx)
      eq = linmod$coefficients[1] + ((linmod$coefficients[2])*(year))
      if(eq>=0){
        c("Prediction = ",eq)
      }
      else{
        c("Prediction = NA")
      }
    }
    else{
      c("Prediction = NA")
    }
  })
  
  
  
  
  
  
  
  
  output$hist1 <- renderPlot({
    if(input$datalist=="All"){
      myVec=c()
      val <- 2
      while(val <= row_num){
        myVec <- append(myVec,mean(t(data_origin[val,])))
        val<-val+1
      }
      hist(myVec,col="lightblue", main=("Histogram of Average fertility rate"),ylim=c(0,35),xlab="Average Fertility Rate of all Countries (1960 to 2020)",ylab = "Country Count")
    }
    else{
      myVec=c()
      year <-input$datalist
      hist(data_origin[,year],col="lightblue",main=("Histogram of Average fertility rate"),ylim=c(0,60),xlab=c("Fertility Rate of all countries in ",year),ylab = "Country Count")
    }
  })
  
  
  output$summary1 <- renderPrint({
    if(input$datalist=="All"){
      myVec2=c()
      val <- 2
      while(val <= row_num){
        myVec2 <- append(myVec2,mean(t(data_origin[val,])))
        val<-val+1
      }
      summary(myVec2)
    }
    else{
      year <- input$datalist
      rows <- length(t(data_origin[,2]))
      summary(data_origin[2:rows,year])
    }
  })
  
  
  output$standD <- renderText({
    if(input$datalist=="All"){
      myVec2=c()
      val <- 2
      while(val <= row_num){
        myVec2 <- append(myVec2,mean(t(data_origin[val,])))
        val<-val+1
      }
      c("Standard Deviation: ",sd(myVec2))
    }
    else{
      year <-input$datalist
      rows <- length(t(data_origin[,2]))
      c("Standard Deviation: ",sd(data_origin[2:rows,year]))
    }
  })
  
  output$IQR <- renderText({
    if(input$datalist=="All"){
      myVec2=c()
      val <- 2
      while(val <= row_num){
        myVec2 <- append(myVec2,mean(t(data_origin[val,])))
        val<-val+1
      }
      c("IQR: ",IQR(myVec2))
    }
    else{
      year <-input$datalist
      rows <- length(t(data_origin[,2]))
      c("IQR: ",IQR(data_origin[2:rows,year]))
    }
  })
  
  
  
  
  output$probdist <- renderText({
    if(input$datalist=="All"){
      myVec2=c()
      val <- 2
      while(val <= row_num){
        myVec2 <- append(myVec2,mean(t(data_origin[val,])))
        val<-val+1
      }
      
      if(input$minval=="" && input$maxval!=""){
        c("Probability: ", pnorm(as.numeric(input$maxval),mean(myVec2),sd(myVec2)))
      }
      else if(input$minval!="" && input$maxval==""){
        c("Probability: ", 1-pnorm(as.numeric(input$minval),mean(myVec2),sd(myVec2)))
      }
      else if(input$minval!="" && input$maxval!=""){
        if(as.numeric(input$maxval)>as.numeric(input$minval)){
          prob <- pnorm(as.numeric(input$maxval),mean(myVec2),sd(myVec2))-(pnorm(as.numeric(input$minval),mean(myVec2),sd(myVec2)))
          c("Probability: ",prob)
        }
        else if(as.numeric(input$maxval)<as.numeric(input$minval)){
          prob <- pnorm(as.numeric(input$maxval),mean(myVec2),sd(myVec2))+(1-pnorm(as.numeric(input$minval),mean(myVec2),sd(myVec2)))
          c("Probability: ",prob)
        }
        else{
          c("Probability: 0")
        }
      }
    }
    else{
      year <-input$datalist
      rows <- length(t(data_origin[,2]))
      myVec2=data_origin[2:rows,year]
      if(input$minval=="" && input$maxval!=""){
        c("Probability: ", pnorm(as.numeric(input$maxval),mean(myVec2),sd(myVec2)))
      }
      else if(input$minval!="" && input$maxval==""){
        c("Probability: ", 1-pnorm(as.numeric(input$minval),mean(myVec2),sd(myVec2)))
      }
      else if(input$minval!="" && input$maxval!=""){
        if(as.numeric(input$maxval)>as.numeric(input$minval)){
          prob <- pnorm(as.numeric(input$maxval),mean(myVec2),sd(myVec2))-(pnorm(as.numeric(input$minval),mean(myVec2),sd(myVec2)))
          c("Probability: ",prob)
        }
        else if(as.numeric(input$maxval)<as.numeric(input$minval)){
          prob <- pnorm(as.numeric(input$maxval),mean(myVec2),sd(myVec2))+(1-pnorm(as.numeric(input$minval),mean(myVec2),sd(myVec2)))
          c("Probability: ",prob)
        }
        else{
          c("Probability: 0")
        }
      }
    }
    
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
