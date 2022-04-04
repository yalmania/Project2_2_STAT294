library(shiny)
library("foreign")
library(DT)

ui <- fluidPage(
  
  titlePanel("Analysis of Students' Math Scores"),
  
  sidebarLayout(
    sidebarPanel(
      
      helpText("The file should consist of the following variables:\n
      1.Student No. 2.Teacher 3.Gender 4.Ethnic 5.Freeredu 6.Score 7.wesson"),
      
      fileInput("file1", "Choose SAV File", 
                multiple = FALSE, 
                accept = c(".sav")),
      
      tags$hr(), # horizontal line
      
      
      helpText('For Tab 2'),
      selectInput("var1", 
                  label = "Score vs. var 1:",
                  choices = c("Teacher", 
                              "Gender",
                              "Ethnic", 
                              "Freeredu",
                              "TeachingStyle")),
      

      helpText('For Tab 3'),
      selectInput("var2", 
                  label = "Score vs. var 1:",
                  choices = c("Teacher", 
                              "Gender",
                              "Ethnic",
                              "Freeredu",
                              "TeachingStyle")),
      
      selectInput("var3", 
                  label = "Score vs. var 2:",
                  choices = c("Teacher", 
                              "Gender",
                              "Ethnic",
                              "Freeredu",
                              "TeachingStyle"),
                  selected = "Gender"),
      ),

    mainPanel(
      tabsetPanel(
      tabPanel('Content', DT::dataTableOutput('Content')),
      tabPanel('Score Analysis (against 1 variable)', plotOutput('PLOT1')),
      tabPanel('Score Analysis (against 2 variables)',plotOutput('PLOT2'))
        )
      )
  )
)





server <- function(input, output){

  # code for Tab 1
  output$Content <- DT::renderDataTable({
    req(input$file1)
    d <- read.spss(input$file1$datapath, to.data.frame = TRUE)
    d2 <- na.omit(d)
    d3 <- transform(d2,'TeachingStyle'= ifelse(d2$wesson=='Ruger_Smith', 'standard','traditional'))
    
    df <- subset(d3, select = -c(Student, wesson)) 
  })
  
  
  
  
  # code for Tab 2
  output$PLOT1 <- renderPlot({
    req(input$file1)
    d <- read.spss(input$file1$datapath, to.data.frame = TRUE)
    d2 <- na.omit(d)
    d3 <- transform(d2,'TeachingStyle'= ifelse(d2$wesson=='Ruger_Smith', 'standard','traditional'))
   
    
    x <- switch((input$var1),
                'Teacher'=c('Ruger','Smith','Wesson'),
                'Gender' =c('Female','Male'),
                'Ethnic'=c('Asian','African-American','Hispanic','Caucasian'),
                'Freeredu'=c('Free lunch','Paid lunch'),
                'TeachingStyle'=c("standard", "traditional")
                )

    coln <- input$var1
    
    if (length(x) == 2) {
     RowsforX1 <- d3[ d3[,coln]==x[1] , ]
     AvgMathScore_X1 <- mean(RowsforX1$Score)
     RowsforX2 <- d3[ d3[,coln]==x[2]  , ]
     AvgMathScore_X2 <- mean(RowsforX2$Score)

      barplot(c(AvgMathScore_X1, AvgMathScore_X2)~x, 
              xlab=input$var1,
              ylab='Average Math Scores (out of 100)',
              main=paste("Average Math Scores\nper", input$var1))
    }
    
    
    if (length(x) == 3) {
      Rows_X1 <- d3[ d3[,coln]==x[1] , ]
      AvgMathScore_X1 <- mean(Rows_X1$Score)
      
      Rows_X2 <- d3[ d3[,coln]==x[2] , ]
      AvgMathScore_X2 <- mean(Rows_X2$Score)
      
      Rows_X3 <- d3[ d3[,coln]==x[3] , ]
      AvgMathScore_X3 <- mean(Rows_X3$Score)
      
      barplot(c(AvgMathScore_X1, AvgMathScore_X2, AvgMathScore_X3)~x, 
              xlab=input$var1,
              ylab='Average Math Scores (out of 100)',
              main=paste("Average Math Scores\nper",input$var1))
    }
    
    
    if (length(x) == 4) {
      Rows_X1 <- d3[ d3[,coln]==x[1] , ]
      AvgMathScore_X1 <- mean(Rows_X1$Score)
      
      Rows_X2 <- d3[ d3[,coln]==x[2] , ]
      AvgMathScore_X2 <- mean(Rows_X2$Score)
      
      Rows_X3 <- d3[ d3[,coln]==x[3] , ]
      AvgMathScore_X3 <- mean(Rows_X3$Score)
      
      Rows_X4 <- d3[ d3[,coln]==x[4] , ]
      AvgMathScore_X4 <- mean(Rows_X4$Score)
      
      barplot(c(AvgMathScore_X1,AvgMathScore_X2,AvgMathScore_X3,AvgMathScore_X4)~x, 
              xlab=input$var1,
              ylab='Average Math Scores (out of 100)',
              main=paste("Average Math Scores\nper",input$var1)) 
              
    }
  })
   
   
  
  
  # code for Tab 3
  output$PLOT2 <- renderPlot({
    req(input$file1)
    d <- read.spss(input$file1$datapath, to.data.frame = TRUE)
    d2 <- na.omit(d)
    d3 <- transform(d2,'TeachingStyle'= ifelse(d2$wesson=='Ruger_Smith', 'standard','traditional'))
     
    
    y <- switch((input$var2),
                'Teacher'=c('Ruger','Smith','Wesson'),
                'Gender' =c('Female','Male'),
                'Ethnic'=c('Asian','African-American','Hispanic','Caucasian'),
                'Freeredu'=c('Free lunch','Paid lunch'),
                'TeachingStyle'=c("standard", "traditional") # assuming the user had all options.
    )
    
    z <- switch((input$var3),
                'Teacher'=c('Ruger','Smith','Wesson'),
                'Gender' =c('Female','Male'),
                'Ethnic'=c('Asian','African-American','Hispanic','Caucasian'),
                'Freeredu'=c('Free lunch','Paid lunch'),
                'TeachingStyle'=c("standard", "traditional") 
    )
    
    col_y <- input$var2
    col_z <- input$var3
    
    
    if (length(y)==2)
    { 
      if (length(z)==2)
      { Rows_y1=d3[ d3[,col_y]==y[1] , ]
        Rows_y1z1=Rows_y1[Rows_y1[,col_z]==z[1],]
        AvgMathScore_y1z1=mean(Rows_y1z1$Score)
        
        Rows_y1z2=Rows_y1[Rows_y1[,col_z]==z[2],]
        AvgMathScore_y1z2=mean(Rows_y1z2$Score)

        
        Rows_y2=d3[ d3[,col_y]==y[2] , ]
        Rows_y2z1=Rows_y2[Rows_y2[,col_z]==z[1],]
        AvgMathScore_y2z1=mean(Rows_y2z1$Score)
        
        Rows_y2z2=Rows_y2[Rows_y2[,col_z]==z[2],]
        AvgMathScore_y2z2=mean(Rows_y2z2$Score)
        
        DF=table(d3$Gender,d3$Freeredu) # assuming y is Gender and z is Freeredu
        #         Free lunch   Paid lunch
        #Female         47         49
        #Male           57         63
        
        DF[1,1]=AvgMathScore_y1z1
        DF[1,2]=AvgMathScore_y1z2
        
        DF[2,1]=AvgMathScore_y2z1
        DF[2,2]=AvgMathScore_y2z2
        
        barplot(DF ~ (y), main=paste("Average Math Scores per\n",col_y, "per", col_z),
                xlab=col_y, ylab='Average Math Scores (out of 100)', 
                legend = z, beside=TRUE)}
      
      if (length(z)==3)
      {Rows_y1=d3[ d3[,col_y]==y[1] , ]
      Rows_y1z1=Rows_y1[Rows_y1[,col_z]==z[1],] 
      AvgMathScore_y1z1=mean(Rows_y1z1$Score)
      
      Rows_y1z2=Rows_y1[Rows_y1[,col_z]==z[2],]
      AvgMathScore_y1z2=mean(Rows_y1z2$Score)
      
      Rows_y1z3=Rows_y1[Rows_y1[,col_z]==z[3],]
      AvgMathScore_y1z3=mean(Rows_y1z3$Score)
      
      
      Rows_y2=d3[ d3[,col_y]==y[2] , ]
      Rows_y2z1=Rows_y2[Rows_y2[,col_z]==z[1],]
      AvgMathScore_y2z1=mean(Rows_y2z1$Score)
      
      Rows_y2z2=Rows_y2[Rows_y2[,col_z]==z[2],]
      AvgMathScore_y2z2=mean(Rows_y2z2$Score)
      
      Rows_y2z3=Rows_y2[Rows_y2[,col_z]==z[3],]
      AvgMathScore_y2z3=mean(Rows_y2z3$Score)
      
      
      DF=table(d3$Gender,d3$Teacher)
      #       Ruger  Smith  Wesson
      #Female    32    28     36
      #Male      39    41     40
      
      DF[1,1]=AvgMathScore_y1z1
      DF[1,2]=AvgMathScore_y1z2
      DF[1,3]=AvgMathScore_y1z3
      
      DF[2,1]=AvgMathScore_y2z1
      DF[2,2]=AvgMathScore_y2z2
      DF[2,3]=AvgMathScore_y2z3
      
      barplot(DF ~ y, main=paste("Average Math Scores per\n", col_y, "per", col_z),
              xlab=col_y, ylab='Average Math Scores (out of 100)', 
              legend = z, beside=TRUE)}
      
      
      if (length(z)==4)
      { Rows_y1=d3[ d3[,col_y]==y[1] , ]
        Rows_y1z1=Rows_y1[Rows_y1[,col_z]==z[1],] 
        AvgMathScore_y1z1=mean(Rows_y1z1$Score)
        
        Rows_y1z2=Rows_y1[Rows_y1[,col_z]==z[2],]
        AvgMathScore_y1z2=mean(Rows_y1z2$Score)
        
        Rows_y1z3=Rows_y1[Rows_y1[,col_z]==z[3],]
        AvgMathScore_y1z3=mean(Rows_y1z3$Score)
        
        Rows_y1z4=Rows_y1[Rows_y1[,col_z]==z[4],]
        AvgMathScore_y1z4=mean(Rows_y1z4$Score)
        
        
        Rows_y2=d3[ d3[,col_y]==y[2] , ]
        Rows_y2z1=Rows_y2[Rows_y2[,col_z]==z[1],]
        AvgMathScore_y2z1=mean(Rows_y2z1$Score)
        
        Rows_y2z2=Rows_y2[Rows_y2[,col_z]==z[2],]
        AvgMathScore_y2z2=mean(Rows_y2z2$Score)
        
        Rows_y2z3=Rows_y2[Rows_y2[,col_z]==z[3],]
        AvgMathScore_y2z3=mean(Rows_y2z3$Score)
        
        Rows_y2z4=Rows_y2[Rows_y2[,col_z]==z[4],]
        AvgMathScore_y2z4=mean(Rows_y2z4$Score)
        
        
        DF=table(d3$Gender,d3$Ethnic)
        #        Asian    African-American Hispanic Caucasian
        #Female    26               20       31        19
        #Male      27               32       35        26
        
        DF[1,1]=AvgMathScore_y1z1
        DF[1,2]=AvgMathScore_y1z2
        DF[1,3]=AvgMathScore_y1z3
        DF[1,4]=AvgMathScore_y1z4
        
        DF[2,1]=AvgMathScore_y2z1
        DF[2,2]=AvgMathScore_y2z2
        DF[2,3]=AvgMathScore_y2z3
        DF[2,4]=AvgMathScore_y2z4
        
        barplot(DF ~ y, main=paste("Average Math Scores per\n", col_y, "per", col_z),
                xlab=col_y, ylab='Average Math Scores (out of 100)', 
                legend = z, beside=TRUE)}
    }
    
    
    
    
    if (length(y)==3)
    {
      if (length(z)==2)
      { Rows_y1=d3[ d3[,col_y]==y[1] , ]
      Rows_y1z1=Rows_y1[Rows_y1[,col_z]==z[1],]
      AvgMathScore_y1z1=mean(Rows_y1z1$Score)
      
      Rows_y1z2=Rows_y1[Rows_y1[,col_z]==z[2],]
      AvgMathScore_y1z2=mean(Rows_y1z2$Score)
      
      
      Rows_y2=d3[ d3[,col_y]==y[2] , ]
      Rows_y2z1=Rows_y2[Rows_y2[,col_z]==z[1],]
      AvgMathScore_y2z1=mean(Rows_y2z1$Score)
      
      Rows_y2z2=Rows_y2[Rows_y2[,col_z]==z[2],]
      AvgMathScore_y2z2=mean(Rows_y2z2$Score)
      
      
      Rows_y3=d3[ d3[,col_y]==y[3] , ]
      Rows_y3z1=Rows_y3[Rows_y3[,col_z]==z[1],]
      AvgMathScore_y3z1=mean(Rows_y3z1$Score)
      
      Rows_y3z2=Rows_y3[Rows_y3[,col_z]==z[2],]
      AvgMathScore_y3z2=mean(Rows_y3z2$Score)
      
      DF=table(d3$Teacher,d3$Gender)
      #         Female Male
      #Ruger      32   39
      #Smith      28   41
      #Wesson     36   40
      
      DF[1,1]=AvgMathScore_y1z1
      DF[1,2]=AvgMathScore_y1z2
      
      DF[2,1]=AvgMathScore_y2z1
      DF[2,2]=AvgMathScore_y2z2
      
      DF[3,1]=AvgMathScore_y3z1
      DF[3,2]=AvgMathScore_y3z2
      
      barplot(DF~y, main=paste("Average Math Scores per\n",col_y, "per", col_z),
              xlab=col_y, ylab='Average Math Scores (out of 100)', 
              legend = z, beside=TRUE)}
      
      if (length(z)==4)
      { Rows_y1=d3[ d3[,col_y]==y[1] , ]
        Rows_y1z1=Rows_y1[Rows_y1[,col_z]==z[1],]
        AvgMathScore_y1z1=mean(Rows_y1z1$Score)
        
        Rows_y1z2=Rows_y1[Rows_y1[,col_z]==z[2],]
        AvgMathScore_y1z2=mean(Rows_y1z2$Score)
        
        Rows_y1z3=Rows_y1[Rows_y1[,col_z]==z[3],]
        AvgMathScore_y1z3=mean(Rows_y1z3$Score)
        
        Rows_y1z4=Rows_y1[Rows_y1[,col_z]==z[4],]
        AvgMathScore_y1z4=mean(Rows_y1z4$Score)
        
        
        Rows_y2=d3[ d3[,col_y]==y[2] , ]
        Rows_y2z1=Rows_y2[Rows_y2[,col_z]==z[1],]
        AvgMathScore_y2z1=mean(Rows_y2z1$Score)
        
        Rows_y2z2=Rows_y2[Rows_y2[,col_z]==z[2],]
        AvgMathScore_y2z2=mean(Rows_y2z2$Score)
        
        Rows_y2z3=Rows_y2[Rows_y2[,col_z]==z[3],]
        AvgMathScore_y2z3=mean(Rows_y2z3$Score)
        
        Rows_y2z4=Rows_y2[Rows_y2[,col_z]==z[4],]
        AvgMathScore_y2z4=mean(Rows_y2z4$Score)
        
        
        Rows_y3=d3[ d3[,col_y]==y[3] , ]
        Rows_y3z1=Rows_y3[Rows_y3[,col_z]==z[1],]
        AvgMathScore_y3z1=mean(Rows_y3z1$Score)
        
        Rows_y3z2=Rows_y3[Rows_y3[,col_z]==z[2],]
        AvgMathScore_y3z2=mean(Rows_y3z2$Score)
        
        Rows_y3z3=Rows_y3[Rows_y3[,col_z]==z[3],]
        AvgMathScore_y3z3=mean(Rows_y3z3$Score)
        
        Rows_y3z4=Rows_y3[Rows_y3[,col_z]==z[4],]
        AvgMathScore_y3z4=mean(Rows_y3z4$Score)
        
        
        DF=table(d3$Teacher,d3$Ethnic)
        #        Asian   African-American  Hispanic  Caucasian
        #Ruger     17               18       25        11
        #Smith     20               16       18        15
        #Wesson    16               18       23        19
        
        DF[1,1]=AvgMathScore_y1z1
        DF[1,2]=AvgMathScore_y1z2
        DF[1,3]=AvgMathScore_y1z3
        DF[1,4]=AvgMathScore_y1z4
        
        DF[2,1]=AvgMathScore_y2z1
        DF[2,2]=AvgMathScore_y2z2
        DF[2,3]=AvgMathScore_y2z3
        DF[2,4]=AvgMathScore_y2z4
        
        DF[3,1]=AvgMathScore_y3z1
        DF[3,2]=AvgMathScore_y3z2
        DF[3,3]=AvgMathScore_y3z3
        DF[3,4]=AvgMathScore_y3z4
        
        barplot(DF~y, main=paste("Average Math Scores per\n",col_y, "per", col_z),
                xlab=col_y, ylab='Average Math Scores (out of 100)', 
                legend = z, beside=TRUE)}
    }
    
    
    
    
    if (length(y)==4) # a special case for Ethnic selection, we'll either have z's length to be 2 or 3.
    { 
      if (length(z)==2)
      { Rows_y1=d3[ d3[,col_y]==y[1] , ]
        Rows_y1z1=Rows_y1[Rows_y1[,col_z]==z[1],]
        AvgMathScore_y1z1=mean(Rows_y1z1$Score)
        
        Rows_y1z2=Rows_y1[Rows_y1[,col_z]==z[2],]
        AvgMathScore_y1z2=mean(Rows_y1z2$Score)
        
        
        Rows_y2=d3[ d3[,col_y]==y[2] , ]
        Rows_y2z1=Rows_y2[Rows_y2[,col_z]==z[1],]
        AvgMathScore_y2z1=mean(Rows_y2z1$Score)
        
        Rows_y2z2=Rows_y2[Rows_y2[,col_z]==z[2],]
        AvgMathScore_y2z2=mean(Rows_y2z2$Score)
        
        
        Rows_y3=d3[ d3[,col_y]==y[3] , ]
        Rows_y3z1=Rows_y3[Rows_y3[,col_z]==z[1],]
        AvgMathScore_y3z1=mean(Rows_y3z1$Score)
        
        Rows_y3z2=Rows_y3[Rows_y3[,col_z]==z[2],]
        AvgMathScore_y3z2=mean(Rows_y3z2$Score)
        
        
        Rows_y4=d3[ d3[,col_y]==y[4] , ]
        Rows_y4z1=Rows_y4[Rows_y4[,col_z]==z[1],]
        AvgMathScore_y4z1=mean(Rows_y4z1$Score)
        
        Rows_y4z2=Rows_y4[Rows_y4[,col_z]==z[2],]
        AvgMathScore_y4z2=mean(Rows_y4z2$Score)
        
        
        DF=table(d3$Ethnic,d3$Gender)
        #                  Female  Male
        #Asian                26   27
        #African-American     20   32
        #Hispanic             31   35
        #Caucasian            19   26
        
        DF[1,1]=AvgMathScore_y1z1
        DF[2,1]=AvgMathScore_y2z1
        DF[3,1]=AvgMathScore_y3z1
        DF[4,1]=AvgMathScore_y4z1
        
        DF[1,2]=AvgMathScore_y1z2
        DF[2,2]=AvgMathScore_y2z2
        DF[3,2]=AvgMathScore_y3z2
        DF[4,2]=AvgMathScore_y4z2
        
        barplot(DF~y, main=paste("Average Math Scores per\n",col_y, "per", col_z),
                xlab=col_y, ylab='Average Math Scores (out of 100)', 
                legend = z, beside=TRUE)}
      
      
      if (length(z)==3)
      { Rows_y1=d3[ d3[,col_y]==y[1] , ]
        Rows_y1z1=Rows_y1[Rows_y1[,col_z]==z[1],] 
        AvgMathScore_y1z1=mean(Rows_y1z1$Score)
        
        Rows_y1z2=Rows_y1[Rows_y1[,col_z]==z[2],]
        AvgMathScore_y1z2=mean(Rows_y1z2$Score)
        
        Rows_y1z3=Rows_y1[Rows_y1[,col_z]==z[3],]
        AvgMathScore_y1z3=mean(Rows_y1z3$Score)
        
        
        Rows_y2=d3[ d3[,col_y]==y[2] , ]
        Rows_y2z1=Rows_y2[Rows_y2[,col_z]==z[1],]
        AvgMathScore_y2z1=mean(Rows_y2z1$Score)
        
        Rows_y2z2=Rows_y2[Rows_y2[,col_z]==z[2],]
        AvgMathScore_y2z2=mean(Rows_y2z2$Score)
        
        Rows_y2z3=Rows_y2[Rows_y2[,col_z]==z[3],]
        AvgMathScore_y2z3=mean(Rows_y2z3$Score)
        
        
        Rows_y3=d3[ d3[,col_y]==y[3] , ]
        Rows_y3z1=Rows_y3[Rows_y3[,col_z]==z[1],]
        AvgMathScore_y3z1=mean(Rows_y3z1$Score)
        
        Rows_y3z2=Rows_y3[Rows_y3[,col_z]==z[2],]
        AvgMathScore_y3z2=mean(Rows_y3z2$Score)
        
        Rows_y3z3=Rows_y3[Rows_y3[,col_z]==z[3],]
        AvgMathScore_y3z3=mean(Rows_y3z3$Score)
        
        
        Rows_y4=d3[ d3[,col_y]==y[4] , ]
        Rows_y4z1=Rows_y4[Rows_y4[,col_z]==z[1],]
        AvgMathScore_y4z1=mean(Rows_y4z1$Score)
        
        Rows_y4z2=Rows_y4[Rows_y4[,col_z]==z[2],]
        AvgMathScore_y4z2=mean(Rows_y4z2$Score)
        
        Rows_y4z3=Rows_y4[Rows_y4[,col_z]==z[3],]
        AvgMathScore_y4z3=mean(Rows_y4z3$Score)
        
        
        DF=table(d3$Ethnic,d3$Teacher)
        #                  Ruger Smith Wesson
        #Asian               17    20     16
        #African-American    18    16     18
        #Hispanic            25    18     23
        #Caucasian           11    15     19
        
        DF[1,1]=AvgMathScore_y1z1
        DF[2,1]=AvgMathScore_y2z1
        DF[3,1]=AvgMathScore_y3z1
        DF[4,1]=AvgMathScore_y4z1
        
        DF[1,2]=AvgMathScore_y1z2
        DF[2,2]=AvgMathScore_y2z2
        DF[3,2]=AvgMathScore_y3z2
        DF[4,2]=AvgMathScore_y4z2
          
        DF[1,3]=AvgMathScore_y1z3
        DF[2,3]=AvgMathScore_y2z3
        DF[3,3]=AvgMathScore_y3z3
        DF[4,3]=AvgMathScore_y4z3
        
        
        barplot(DF~y, main=paste("Average Math Scores per\n", col_y, "per", col_z),
                xlab=col_y, ylab='Average Math Scores (out of 100)', 
                legend = z, beside=TRUE)}
    
    }

      })
 }





shinyApp(ui = ui, server = server)
