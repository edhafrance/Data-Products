library(ggplot2)
library(plotly)
library(dplyr)
library(reshape)



##Transforming the data
setwd("/Users/edwardha/Documents/COURSERA/DESIGN")
YIELDCURVE<-read.csv("YIELDCURVE.csv",na.strings=c("N/A","#DIV/0!","","#VALUE!"))
YIELDMUNI<-read.csv("MUNICURVE.csv",na.strings=c("N/A","#DIV/0!","","#VALUE!"))
colnames(YIELDCURVE)<-c("Date","0.08","0.25","0.5","1","2","3","5","7","10","20","30")
colnames(YIELDMUNI)<-c("Date","0.08","0.25","0.5","1","2","3","5","7","10","20","30")

YIELDCURVE<-na.omit(YIELDCURVE)
YIELDMUNI<-na.omit(YIELDMUNI)

id<-seq(2,12)
for (i in id){
  YIELDCURVE[,i]<-as.numeric(as.character(YIELDCURVE[,i]))
  YIELDMUNI[,i]<-as.numeric(as.character(YIELDMUNI[,i]))}

YIELDCURVE$Date<-as.Date(YIELDCURVE$Date,"%m/%d/%y")
YIELDMUNI$Date<-as.Date(YIELDMUNI$Date,"%m/%d/%y")


mYIELDCURVE<-melt(YIELDCURVE,id="Date")
mYIELDMUNI<-melt(YIELDMUNI,id="Date")
server <- function(input, output) {
  
    output$trendPlot <- renderPlotly({
    mYIELDCURVE2<-mYIELDCURVE%>%filter(Date==input$Curve_Date)
    mYIELDMUNI2<-mYIELDMUNI%>%filter(Date==input$Curve_Date)
 
    
    # build graph with ggplot syntax
    
    
    
    mYIELDCURVE2<-mYIELDCURVE%>%filter(Date==input$Curve_Date)
    mYIELDMUNI2<-mYIELDMUNI%>%filter(Date==input$Curve_Date)
    
    mYIELDCURVE2$variable<-as.numeric(as.character(mYIELDCURVE2$variable))
    mYIELDMUNI2$variable<-as.numeric(as.character(mYIELDMUNI2$variable))
    
    TR<-data.frame(curve=c("TR","TR","TR","TR","TR","TR","TR","TR","TR","TR","TR"))
    MUNI<-data.frame(curve=c("MUNI","MUNI","MUNI","MUNI","MUNI","MUNI","MUNI","MUNI","MUNI","MUNI","MUNI"))
    #mYIELDCURVE2$Date<-as.factor(mYIELDCURVE2$Date)
    #mYIELDCURVE2$variable<-as.numeric(as.character(mYIELDCURVE2$variable))
    
    #mYIELDMUNI2$Date<-as.factor(mYIELDMUNI2$Date)
    #mYIELDMUNI2$variable<-as.numeric(as.character(mYIELDMUNI2$variable))
    mYIELDCURVE2<-cbind(mYIELDCURVE2,TR)
    mYIELDMUNI2<-cbind(mYIELDMUNI2,MUNI)
    TOTAL<-rbind(mYIELDCURVE2,mYIELDMUNI2)
    
    
    
    p <- ggplot(data=TOTAL,aes(x=variable,y=value,color=curve))+
      geom_line()+geom_point()+ggtitle("Yield Curve Comparison")+
      labs(x="Maturity",y="Yield (%)")
    
    ggplotly(p) 
    
    
    
  })
    output$RATIO_CALC<-renderText({
      
      mYIELDCURVE2<-mYIELDCURVE%>%filter(Date==input$Curve_Date)
      mYIELDMUNI2<-mYIELDMUNI%>%filter(Date==input$Curve_Date)
      
      mYIELDCURVE2$variable<-as.numeric(as.character(mYIELDCURVE2$variable))
      mYIELDMUNI2$variable<-as.numeric(as.character(mYIELDMUNI2$variable))
      
      TR<-data.frame(curve=c("TR","TR","TR","TR","TR","TR","TR","TR","TR","TR","TR"))
      MUNI<-data.frame(curve=c("MUNI","MUNI","MUNI","MUNI","MUNI","MUNI","MUNI","MUNI","MUNI","MUNI","MUNI"))
      
      
      mYIELDCURVE2<-cbind(mYIELDCURVE2,TR)
      mYIELDMUNI2<-cbind(mYIELDMUNI2,MUNI)
      
      mYIELDCURVE3<-mYIELDCURVE2%>%filter(variable==10)
      mYIELDMUNI3<-mYIELDMUNI2%>%filter(variable==10)
      TOTAL<-rbind(mYIELDCURVE2,mYIELDMUNI2)
      
      if (input$`show model1`){
        round(mYIELDMUNI3$value/mYIELDCURVE3$value,2)
      }  
      
    })   
    
 
  }