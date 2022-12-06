install.packages("shiny")
library(shiny)
library(ggplot2)
library(shinydashboard)
MPI=read.csv("ap.csv")
View(MPI)
MPI1=read.csv("MPI.csv")
View(MPI1)
dfdata = data.frame(MPI1)
View(dfdata)
install.packages("formattable")                            
library("formattable")
library(dplyr)
dfdata1 =t(dfdata)
rownames(dfdata1) <- colnames(dfdata)
colnames(dfdata1) <- rownames(dfdata)
View(dfdata1)
dataframe2=as.data.frame(dfdata1)
dataframe2
dataframe2=dfdata1[1:10,]
colnames(dataframe2)=dfdata$States
dataframe2=as.data.frame(dataframe2)
dataframe2$index=rownames(dataframe2)
View(dataframe2)

d=read.csv("new.csv")
dk=d[(d$Country=="India"),]
dk1=dk[1,4:6]
str(dk1)

library(dplyr)
df_t =t(dk)
rownames(df_t) <- colnames(dk)
colnames(df_t) <- rownames(dk)
dk$States
dataframe1=as.data.frame(df_t)
d1=df_t[4:6,]
colnames(d1)=dk$States
d1=as.data.frame(d1)
d1$index=rownames(d1)
d1$Indicators=rownames(d1)
View(d1)

library(ggplot2)
library(shinydashboard)

x=c("","Assam","Bihar","Jharkhand","Madhya Pradesh","Uttar Pradesh")
z=c("","Kerala","Lakshadweep","Puducherry","Delhi","Sikkim")
y=c("","MPI",'HCR',"IPP")
p=c("","Deprivation Percentage in Living Standards","Deprivation Percentage in Education","Deprivation Percentage in Health","Headcount Ratio","Intensity of Poverty")


ui = dashboardPage(
  dashboardHeader(title="India's MPI Report"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction",tabName="plot"),
      menuItem("A study across all States",tabName = "plot1"),
      menuItem("state Wise Comparison",tabName = "plot2"),
      menuItem("Relation b/w MPI and Others",tabName = "plot3"))),
  dashboardBody(
    tabItems(
      tabItem("plot",
              fluidPage(
                box(
                  h1("A report based on India's Multidimensional Poverty Index based on 2015-16 MPI survey results"),
                  h3("Abstract:"),
                  p("The report aims to compare the Multidimensional Poverty levels across various states in India and analyse various social indicators to understand the level of deprivation and causes of poverty in each state."),
                  h3("Introduction"),
                  p("Over the last four decades,many techniques, procedures, theories and indices in Welfare Economics to understand,measure,analyze and compare the concepts of overall development and inequalities,poverty etc.The Multidimensional Poverty Index is basically a deprivation index, where we try to find people are deprived from some basic rights.MPI as a whole takes into account both the proportion of poor and intensity of poverty.HCR is the headcount ratio, which represents the proportion of poor in the society and “IPP” is the total deprivation in the region divided by the total number of poor in the region.After that I showed the percentage of people who are poor and deprived in Health,Education and Living Standards across various states.I tried to show that through indicators like Child Mortality Rate and Nutrition of Health Dimension,Years of Schooling and School Attendance of Education Dimension and finally cooking fuel,sanitation,drinking water,electricity, housing and assets of Living Standard dimension.Lastly how the percentage different dimensions like Health,Education and Living Standards contribute to the level of multidimensional poverty.It will be described in the pie-chart format.The scatterplot was shown to draw the correlation between Multidimensional Poverty index and others"),
                  h3("About the Dataset:"),
                  p("I have collected the data from the Oxford Poverty and Human Development Index based on Sub-national Results MPI 2021 based on 2015-16 survey results.It contains files with data of all states levels in India,related to the MPI of the regions, Censored Headcount Region and Contributions of the regions to the poverty."),
                  h2("The Variables used in the Project:"),
                  h3("MPI=Multidimensional Poverty Index"),
                  h3("HCR=Headcount Ratio"),
                  h3("IPP=Total Deprivation in the region divided by the total number of poor"),
                  h3("CMR=Child Mortality Rate"),
                  h3("NRT=Nutrition"),
                  h3("YS=Years Of Schooling"),
                  h3("ATR=Attendance Of Education"),
                  h3("CF=Cooking fuel"),
                  h3("SNT=Sanitation"),
                  h3("DW=Drinking Water"),
                  h3("ELC=Electricty"),
                  h3("HS=Housing"),
                  h3("AS=Assets"),
                  h3("HTH=Health"),
                  h3("EDU=Education"),
                  h3("Living Standard= LVS"),
                  h2("Project Analysis"),
                  
                  
                  
                  h3("1.The Multidimensional Poverty Index Scores scores across all states of India."),
                  h3("2.The Headcount Ratio showing the percentage of population in Multidimensional Poverty across all states in India"),
                  h3("3.The Intensity of Deprivation among the Poor across all States in India."),
                  h3("4.The Statewise Comparison between 5 states with highest and lowest MPI scores."),
                  h3("5.Scatter Plot to show the relationship between the Multidimensional Poverty Index and others."),
                  width=12
                  ))),
      tabItem("plot1",
              fluidPage(
                box(title='A Study of Poverty Levels & Scores acrros all the States in India ',width=8,solidHeader=TRUE,selectInput("select1",label = h3("Select box"),choices=y),actionButton("go","Click to see"),plotOutput('my_plot1')
                ))),
      tabItem("plot2",
            tabsetPanel(
              tabPanel("HEALTH ",
                         fluidPage(
                           fluidRow(
                             column(
                               
                              width=6,selectInput("select2",label = h3("5 States with Highest MPI scores"),choices=x),actionButton("go","Click to see")),
                             column(width=6,selectInput("select3",label = h3("5 States with Highest MPI"),choices=z),actionButton("go","Click to see"))),
                           fluidRow(
                             box(title="Percentage of Deprivations in Health:Nutrition(NTR) and Child Mortality Rate(CMR)",width=6,solidHeader=TRUE,plotOutput('my_plot2')),
                             box(title="Percentage of Deprivations in Health:Nutrition(NTR) and Child Mortality Rate(CMR)",width=6,solidHeader=TRUE,plotOutput('my_plot3'))))),
              tabPanel("EDUCATION",
                         fluidPage(
                           fluidRow(
                             column(
                               width=6,selectInput("select4",label = h3("5 States with Highest MPI scores"),choices=x),actionButton("go","Click to see")),
                             column(width=6,selectInput("select5",label = h3("5 States with Lowest MPI scores"),choices=z),actionButton("go","Click to see"))),
                           fluidRow(
                             box(title="Percentage of Deprivations in Education:Years of Schooling(YS) and School Attendance(ATR)",width=6,solidHeader=TRUE,plotOutput('my_plot4')),
                             box(title="Percentage of Deprivations in Education:Years of Schooling(YS) and School Attendance(ATR)",width=6,solidHeader=TRUE,plotOutput('my_plot5'))))),
              tabPanel("STANDARD OF LIVING",
                         fluidPage(
                           fluidRow(
                             column(
                               width=6,selectInput("select6",label = h3("5 States with Highest MPI scores"),choices=x),actionButton("go","Click to see")),
                             column(width=6,selectInput("select7",label = h3("5 States with Lowest MPI scores"),choices=z),actionButton("go","Click to see"))),
                           fluidRow(
                             box(title="Percentage of Deprivations in Living Standards:Cooking Fuel(CF),Santisation(SNT),Drinking water(DW),Electricity(ELC),Housing(HS),Assets(AS):",width=6,solidHeader=TRUE,plotOutput('my_plot6')),
                             box(title="Percentage of Deprivations in Living Standards:Cooking Fuel(CF),Santisation(SNT),Drinking water(DW),Electricity(ELC),Housing(HS),Assets(AS):",width=6,solidHeader=TRUE,plotOutput('my_plot7'))))),
              
              tabPanel("PERCENTAGE OF CONTRIBUTIONS",
                       fluidPage(
                         fluidRow(
                           column(
                             width=6,selectInput("select8",label = h3("5 States with Highest MPI scores"),choices=x),actionButton("go","Click to see")),
                           column(width=6,selectInput("select9",label = h3("5 States with Lowest MPI scores"),choices=z),actionButton("go","Click to see"))),
                         fluidRow(
                           box(title="Pie Chart showing contributions of different deprivations to MPI(in percentage): Education(EDU),Health(HTH),Living Standards(LVS):",width=6,solidHeader=TRUE,plotOutput('my_plot8')),
                           box(title="Pie Chart showing contributions of different deprivations to MPI(in percentage): Education(EDU),Health(HTH),Living Standards(LVS):",width=6,solidHeader=TRUE,plotOutput('my_plot9')))))
              
              
              )),
      tabItem("plot3",
              fluidPage(
                box(title="Scatter Plot to show the relationship between the Multidimensional Poverty Index and others",width=8,solidHeader=TRUE,selectInput("select10",label = h3("Select box"),choices=p),actionButton("go","Click to see"),plotOutput('my_plot10')))))

))


server <- function(input, output){
  output$my_plot1 = renderPlot({
    if (input$select1=='HCR'){
      ggplot(MPI,aes(HCR,reorder(States,HCR),fill=HCR))+geom_col(color='black')+xlab("HCR")+ylab("States")+ggtitle("Headcount Ratio:percentage of population in Multidimenisonal Poverty(HCR)")
    }else if (input$select1=='MPI'){
      ggplot(MPI,aes(MPI,reorder(States,MPI),fill=MPI))+geom_col(color='black')+xlab("MPI")+ylab("States")+ggtitle("Multidimensional Poverty Index(MPI)")
    }else if (input$select1=='IPP'){
      ggplot(MPI,aes(IPP,reorder(States,IPP),fill=IPP))+geom_col(color='black')+xlab("IPP")+ylab("States")+ggtitle("Intensity of deprivation of Poverty among Poor(IPP)")
    }
    
  })
  output$my_plot2 = renderPlot({
    if (input$select2=='Bihar'){
      ggplot(data=dataframe2[2:3,], aes(index,as.numeric(Bihar),fill=index))+geom_col(color='black')+xlab("Child Mortality  and  Nutrition")+ylab("Percentage of Population deprived")+guides(fill=guide_legend(title="Health Dimension"))+scale_y_continuous(labels=percent)
    }else if (input$select2=='Jharkhand'){
      ggplot(data=dataframe2[2:3,], aes(index,as.numeric(Jharkhand),fill=index))+geom_col(color='black')+xlab("Child Mortality  and  Nutrition")+ylab("Percentage of Population deprived")+guides(fill=guide_legend(title="Health Dimension"))+scale_y_continuous(labels=percent)
    }else if (input$select2=='Uttar Pradesh'){
      ggplot(data=dataframe2[2:3,], aes(index,as.numeric(UP),fill=index))+geom_col(color='black')+xlab("Child Mortality  and  Nutrition")+ylab("Percentage of Population deprived")+guides(fill=guide_legend(title="Health Dimension"))+scale_y_continuous(labels=percent)
    }else if (input$select2=='Madhya Pradesh'){
      ggplot(data=dataframe2[2:3,], aes(index,as.numeric(MP),fill=index))+geom_col(color='black')+xlab("Child Mortality  and  Nutrition")+ylab("Percentage of Population deprived")+guides(fill=guide_legend(title="Health Dimension"))+scale_y_continuous(labels=percent)
    }else if (input$select2=='Assam'){
      ggplot(data=dataframe2[2:3,], aes(index,as.numeric(Assam),fill=index))+geom_col(color='black')+xlab("Child Mortality  and  Nutrition")+ylab("Percentage of Population deprived")+guides(fill=guide_legend(title="Health Dimension"))+scale_y_continuous(labels=percent)
    }})
  output$my_plot3 = renderPlot({
    if (input$select3=='Kerala'){
      ggplot(data=dataframe2[2:3,], aes(index,as.numeric(Kerala),fill=index))+geom_col(color='black')+xlab("Child Mortality  and  Nutrition")+ylab("Percentage of Population deprived")+guides(fill=guide_legend(title="Health Dimension"))+scale_y_continuous(labels=percent)
    }else if(input$select3=='Lakshadweep'){
      ggplot(data=dataframe2[2:3,], aes(index,as.numeric(Lakshadweep),fill=index))+geom_col(color='black')+xlab("Child Mortality  and  Nutrition")+ylab("Percentage of Population deprived")+guides(fill=guide_legend(title="Health Dimension"))+scale_y_continuous(labels=percent)
    }else if (input$select3=='Puducherry'){
      ggplot(data=dataframe2[2:3,], aes(index,as.numeric(Puducherry),fill=index))+geom_col(color='black')+xlab("Child Mortality  and  Nutrition")+ylab("Percentage of Population deprived")+guides(fill=guide_legend(title="Health Dimension"))+scale_y_continuous(labels=percent)
    }else if (input$select3=='Delhi'){
      ggplot(data=dataframe2[2:3,], aes(index,as.numeric(Delhi),fill=index))+geom_col(color='black')+xlab("Child Mortality  and  Nutrition")+ylab("Percentage of Population deprived")+guides(fill=guide_legend(title="Health Dimension"))+scale_y_continuous(labels=percent)
    }else if (input$select3=='Sikkim'){
      ggplot(data=dataframe2[2:3,], aes(index,as.numeric(Sikkim),fill=index))+geom_col(color='black')+xlab("Child Mortality  and  Nutrition")+ylab("Percentage of Population deprived")+guides(fill=guide_legend(title="Health Dimension"))+scale_y_continuous(labels=percent)
    }})
  output$my_plot4 = renderPlot({
    if (input$select4=='Bihar'){
      ggplot(data=dataframe2[4:5,], aes(index,as.numeric(Bihar),fill=index))+geom_col(color='black')+xlab("Years of Schooling(YR) and Attendance of Education(AER)")+ylab("Percentage of Population deprived")+guides(fill=guide_legend(title="Education Dimension"))+scale_y_continuous(labels=percent)
    }else if (input$select4=='Jharkhand'){
      ggplot(data=dataframe2[4:5,], aes(index,as.numeric(Jharkhand),fill=index))+geom_col(color='black')+xlab("Years of Schooling(YR) and Attendance of Education(AER)")+ylab("Percentage of Population deprived")+guides(fill=guide_legend(title="Education Dimension"))+scale_y_continuous(labels=percent)
    }else if (input$select4=='Uttar Pradesh'){
      ggplot(data=dataframe2[4:5,], aes(index,as.numeric(UP),fill=index))+geom_col(color='black')+xlab("Years of Schooling(YR) and Attendance of Education(AER)")+ylab("Percentage of Population deprived")+guides(fill=guide_legend(title="Education Dimension"))+scale_y_continuous(labels=percent)
    }else if(input$select4=='Madhya Pradesh'){
      ggplot(data=dataframe2[4:5,], aes(index,as.numeric(MP),fill=index))+geom_col(color='black')+xlab("Years of Schooling(YR) and Attendance of Education(AER)")+ylab("Percentage of Population deprived")+guides(fill=guide_legend(title="Education Dimension"))+scale_y_continuous(labels=percent)
    }else if (input$select4=='Assam'){
      ggplot(data=dataframe2[4:5,], aes(index,as.numeric(Assam),fill=index))+geom_col(color='black')+xlab("Years of Schooling(YR) and Attendance of Education(AER)")+ylab("Percentage of Population deprived")+guides(fill=guide_legend(title="Education Dimension"))+scale_y_continuous(labels=percent)
    }})
  output$my_plot5 = renderPlot({
    if (input$select5=='Kerala'){
      ggplot(data=dataframe2[4:5,], aes(index,as.numeric(Kerala),fill=index))+geom_col(color='black')+xlab("Years of Schooling(YR) and Attendance of Education(AER)")+ylab("Percentage of Population deprived")+guides(fill=guide_legend(title="Education Dimension"))+scale_y_continuous(labels=percent)
    }else if (input$select5=='Lakshadweep'){
      ggplot(data=dataframe2[4:5,], aes(index,as.numeric(Lakshadweep),fill=index))+geom_col(color='black')+xlab("Years of Schooling(YR) and Attendance of Education(AER)")+ylab("Percentage of Population deprived")+guides(fill=guide_legend(title="Education Dimension"))+scale_y_continuous(labels=percent)
    }else if (input$select5=='Puducherry'){
      ggplot(data=dataframe2[4:5,], aes(index,as.numeric(Puducherry),fill=index))+geom_col(color='black')+xlab("Years of Schooling(YR) and Attendance of Education(AER)")+ylab("Percentage of Population deprived")+guides(fill=guide_legend(title="Education Dimension"))+scale_y_continuous(labels=percent)
    }else if(input$select5=='Delhi'){
      ggplot(data=dataframe2[4:5,], aes(index,as.numeric(Delhi),fill=index))+geom_col(color='black')+xlab("Years of Schooling(YR) and Attendance of Education(AER)")+ylab("Percentage of Population deprived")+guides(fill=guide_legend(title="Education Dimension"))+scale_y_continuous(labels=percent)
    }else if(input$select5=='Sikkim'){
      ggplot(data=dataframe2[4:5,], aes(index,as.numeric(Sikkim),fill=index))+geom_col(color='black')+xlab("Years of Schooling(YR) and Attendance of Education(AER)")+ylab("Percentage of Population deprived")+guides(fill=guide_legend(title="Education Dimension"))+scale_y_continuous(labels=percent)
    }})
  output$my_plot7 = renderPlot({
    if (input$select7=='Kerala'){
      ggplot(data=dataframe2[6:10,], aes(index,as.numeric(Kerala),fill=index))+geom_col(color='black')+xlab("Cooking fuel,Drinking Water,Housing,Assets,Santation")+ylab("Percentage of Population deprived")+guides(fill=guide_legend(title="Living Standard Dimension"))+scale_y_continuous(labels=percent)
    } else if(input$select7=='Lakshadweep'){
      ggplot(data=dataframe2[6:10,], aes(index,as.numeric(Lakshadweep),fill=index))+geom_col(color='black')+xlab("Cooking fuel,Drinking Water,Housing,Assets,Santation")+ylab("Percentage of Population deprived")+guides(fill=guide_legend(title="Living Standard Dimension"))+scale_y_continuous(labels=percent)
    } else if(input$select7=='Puducherry'){
      ggplot(data=dataframe2[6:10,], aes(index,as.numeric(Puducherry),fill=index))+geom_col(color='black')+xlab("Cooking fuel,Drinking Water,Housing,Assets,Santation")+ylab("Percentage of Population deprived")+guides(fill=guide_legend(title="Living Standard Dimension"))+scale_y_continuous(labels=percent)
    } else if(input$select7=='Delhi'){
      ggplot(data=dataframe2[6:10,], aes(index,as.numeric(Delhi),fill=index))+geom_col(color='black')+xlab("Cooking fuel,Drinking Water,Housing,Assets,Santation")+ylab("Percentage of Population deprived")+guides(fill=guide_legend(title="Living Standard Dimension"))+scale_y_continuous(labels=percent)
    } else if(input$select7=='Sikkim'){
      ggplot(data=dataframe2[6:10,], aes(index,as.numeric(Sikkim),fill=index))+geom_col(color='black')+xlab("Cooking fuel,Drinking Water,Housing,Assets,Santation")+ylab("Percentage of Population deprived")+guides(fill=guide_legend(title="Living Standard Dimension"))+scale_y_continuous(labels=percent)
    }})
  output$my_plot6 = renderPlot({
    if (input$select6=='Bihar'){
      ggplot(data=dataframe2[6:10,], aes(index,as.numeric(Bihar),fill=index))+geom_col(color='black')+xlab("Cooking fuel,Drinking Water,Housing,Assets,Santation")+ylab("Percentage of Population deprived")+guides(fill=guide_legend(title="Living Standard Dimension"))+scale_y_continuous(labels=percent)
    } else if(input$select6=='Jharkhand'){
      ggplot(data=dataframe2[6:10,], aes(index,as.numeric(Jharkhand),fill=index))+geom_col(color='black')+xlab("Cooking fuel,Drinking Water,Housing,Assets,Santation")+ylab("Percentage of Population deprived")+guides(fill=guide_legend(title="Living Standard Dimension"))+scale_y_continuous(labels=percent)
    } else if(input$select6=='uttar Pradesh'){
      ggplot(data=dataframe2[6:10,], aes(index,as.numeric(UP),fill=index))+geom_col(color='black')+xlab("Cooking fuel,Drinking Water,Housing,Assets,Santation")+ylab("Percentage of Population deprived")+guides(fill=guide_legend(title="Living Standard Dimension"))+scale_y_continuous(labels=percent)
    } else if(input$select6=='Madhya Pradesh'){
      ggplot(data=dataframe2[6:10,], aes(index,as.numeric(MP),fill=index))+geom_col(color='black')+xlab("Cooking fuel,Drinking Water,Housing,Assets,Santation")+ylab("Percentage of Population deprived")+guides(fill=guide_legend(title="Living Standard Dimension"))+scale_y_continuous(labels=percent)
    } else if(input$select6=='Assam'){
      ggplot(data=dataframe2[6:10,], aes(index,as.numeric(Assam),fill=index))+geom_col(color='black')+xlab("Cooking fuel,Drinking Water,Housing,Assets,Santation")+ylab("Percentage of Population deprived")+guides(fill=guide_legend(title="Living Standard Dimension"))+scale_y_continuous(labels=percent)
    }})
  output$my_plot8 = renderPlot({
    if (input$select8=='Bihar'){
      ggplot(d1,aes(x='', y =Bihar,fill=Indicators)) +
        geom_col(color = "black") +
        geom_text(aes(label=Bihar),
                  position=position_stack(vjust = 0.5))+coord_polar(theta = "y") +
        theme_void()+scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+guides(fill=guide_legend(title=))
    } else if(input$select8=='Jharkhand'){
      ggplot(d1,aes(x='', y =Jharkhand,fill=Indicators)) +
        geom_col(color = "black") +
        geom_text(aes(label=Jharkhand),
                  position=position_stack(vjust = 0.5))+coord_polar(theta = "y") +
        theme_void()+scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+guides(fill=guide_legend(title=))
    } else if (input$select8=='Uttar Pradesh'){
      ggplot(d1,aes(x='', y =UP,fill=Indicators)) +
        geom_col(color = "black") +
        geom_text(aes(label=UP),
                  position=position_stack(vjust = 0.5))+coord_polar(theta = "y") +
        theme_void()+scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+guides(fill=guide_legend(title=))
    }else if (input$select8=='Madhya Pradesh'){
      ggplot(d1,aes(x='', y =MP,fill=Indicators)) +
        geom_col(color = "black") +
        geom_text(aes(label=MP),
                  position=position_stack(vjust = 0.5))+coord_polar(theta = "y") +
        theme_void()+scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+guides(fill=guide_legend(title=))
    }else if (input$select8=='Assam'){
      ggplot(d1,aes(x='', y =Assam,fill=Indicators)) +
        geom_col(color = "black") +
        geom_text(aes(label=Assam),
                  position=position_stack(vjust = 0.5))+coord_polar(theta = "y") +
        theme_void()+scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+guides(fill=guide_legend(title=))
    }})
  output$my_plot9 = renderPlot({
    if (input$select9=='Kerala'){
      ggplot(d1,aes(x='', y =Kerala,fill=Indicators)) +
        geom_col(color = "black") +
        geom_text(aes(label=Kerala),
                  position=position_stack(vjust = 0.5))+coord_polar(theta = "y") +
        theme_void()+scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+guides(fill=guide_legend(title=))
    } else if(input$select9=='Lakshadweep'){
      ggplot(d1,aes(x='', y =Lakshadweep,fill=Indicators)) +
        geom_col(color = "black") +
        geom_text(aes(label=Lakshadweep),
                  position=position_stack(vjust = 0.5))+coord_polar(theta = "y") +
        theme_void()+scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+guides(fill=guide_legend(title=))
    } else if (input$select9=='Puducherry'){
      ggplot(d1,aes(x='', y =Puducherry,fill=Indicators)) +
        geom_col(color = "black") +
        geom_text(aes(label=Puducherry),
                  position=position_stack(vjust = 0.5))+coord_polar(theta = "y") +
        theme_void()+scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+guides(fill=guide_legend(title=))
    }else if (input$select9=='Delhi'){
      ggplot(d1,aes(x='', y =Delhi,fill=Indicators)) +
        geom_col(color = "black") +
        geom_text(aes(label=Delhi),
                  position=position_stack(vjust = 0.5))+coord_polar(theta = "y") +
        theme_void()+scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+guides(fill=guide_legend(title=))
    }else if (input$select9=='Sikkim'){
      ggplot(d1,aes(x='', y =Sikkim,fill=Indicators)) +
        geom_col(color = "black") +
        geom_text(aes(label=Sikkim),
                  position=position_stack(vjust = 0.5))+coord_polar(theta = "y") +
        theme_void()+scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+guides(fill=guide_legend(title=))
    }})
  output$my_plot10 = renderPlot({
    if (input$select10=='Headcount Ratio'){
      plot(x = MPI$MPI, y =MPI$HCR,xlab = "Multidimensional Poverty Index",ylab = "Headcount Ratio",xlim = c(0,1),ylim = c(0,50),main = "Multidimensional Poverty Index V/S Headcount Ratio",grid(3, 5))
    } else if(input$select10=='Intensity of Poverty'){
      plot(x = MPI$MPI, y =MPI$IPP,xlab = "Multidimensional Poverty Index",ylab = "Intensity of Poverty",xlim = c(0,1),ylim = c(0,50),main = "Multidimensional Poverty Index V/S Intensity of Poverty",grid(3, 5))
    } else if(input$select10=='Deprivation Percentage in Health'){
      plot(x = MPI$MPI, y =MPI$H,xlab = "Multidimensional Poverty Index",ylab = "Deprivation in Health",xlim = c(0,1),ylim = c(0,50),main = "Multidimensional Poverty v/s Level of deprivation in Health",grid(3, 5))
    } else if(input$select10=='Deprivation Percentage in Education'){
      plot(x = MPI$MPI, y =MPI$E,xlab = "Multidimensional Poverty Index",ylab = "Deprivation Percentage in Education",xlim = c(0,1),ylim = c(0,50),main = "Multidimensional Poverty v/s Level of deprivation in Education",grid(3, 5))
    } else if(input$select10=='Deprivation Percentage in Living Standards'){
      plot(x = MPI$MPI, y =MPI$L,xlab = "Multidimensional Poverty Index",ylab = "Deprivation Percentage in Living Standards",xlim = c(0,1),ylim = c(0,50),main = "Multidimensional Poverty Index V/S Level of deprivation in Living Standards ",grid(3, 5))
    }
      
    })

  
}

shinyApp(ui = ui,server = server)

