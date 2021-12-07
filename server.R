library(shiny)
library(shinythemes)
library(shinydashboard)
library(gdata)
library(rintrojs)
library(shinyjs)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(leaflet)
library(DT)




shinyServer(function(input,output) {
  pl<- read.csv2("pl.csv", sep=",")
  head(pl)
  pl<-pl[-c(1,2),]#On enlève la 2ème ligne car ce sont des titres des colonnes
  colnames(pl)<-c("ID","Joueur","Nation","Poste","Equipe","Age","Naissance","MJ","Titulaire","MinutesJouees","MinJ/90","Buts","PDT","ButsSansPenT","PenaltysMarT","PenaltysTen","CJT","CRT","Buts/90","PD/90","ButsSansPen/90","PenaltysMar/90","CJ/90","CR/90","xGT","sPenxGT","xAT","sPenxGT+xAT","xG/90","xA/90","xG+xA/90","sPenxG/90")
  pl<-pl[,-33] #La dernière colonne n'a pas d'interet car seulement écrit "match"
  head(pl)
  pl$Joueur<-gsub("\\", ":", pl$Joueur, fixed=TRUE) # On change le \\ en : car on a un  soucis pour utiliser separate avec
  pl<-separate(pl, Joueur, c("Joueur","A enlever"), sep = ":")
  pl<-pl[,-3] #On enleve la colonne nom en doublon "A enlever"
  pl<-separate(pl, Nation, c("A enlever","Nation"), sep = " ")
  pl<-pl[,-3] #On enleve nation en doublon "A enlever"
  pl$ID<-as.integer(pl$ID)
  
  for (i in 1:13){
    pl[,i+5]<-as.integer(pl[,i+5])
  }#Transformation  des colonnes souhaitées en integer au lieu de character
  
  for (i in 19:32){
    pl[,i]<-as.double(pl[,i])
  }
  
  stade <- read.csv2("Stadium.csv", sep=",")
  stade[,2]<-as.numeric(stade[,2])
  stade[,3]<-as.numeric(stade[,3])
 

  output$playerdata <- renderDataTable({
    subset(pl, pl$Equipe == input$inclub, select=c("Joueur","Poste", "MJ", "Buts","PDT")) 
    
  })
  
  output$stade <- renderDataTable(
    stade %>%
      datatable(editable="row")
  )
  
  sd1 <- reactive({
    pl[, c(input$x, input$y)]
  })
  
  sd2 <- reactive({
    pl[, c(input$xcol, input$ycol, "Equipe")]
  })
  
  sd3 <- reactive({
    pl2<-pl[, c(input$xcol, input$ycol)]
    pl2<- subset(pl2, pl$Equipe == input$inclub2)
  })
  
  output$plot1 <- renderPlot({
    ggplot(pl, aes(x=MJ, y=Buts))+
      geom_point()+
      geom_point(aes(x=input$x,y=input$y), colour="blue")+
      labs(x="Matchs Joués", y="Buts marqués")
  })
  
  output$plot2 <-  renderPlot({
    ggplot(sd2(),aes_string(x=input$xcol, y=input$ycol))+
      geom_point(aes(color=Equipe))
  })
  
  output$plot3<- renderPlot({
    ggplot(sd3(),aes_string(x=input$xcol, y=input$ycol))+
      geom_point()})
  
  output$plot4<- renderPlot({ggplot(sd3(), aes(x=input$inclub2)) + 
    geom_boxplot(aes_string(y=input$xcol))+
    labs(x="Equipe choisie")})
  
  se<-subset(pl, select=c("Joueur", "Equipe", "Poste", "MJ", "Titulaire", "MinutesJouees", "PDT", "ButsSansPenT", "Buts") )
  output$click_info <- renderPrint({
    nearPoints(se, input$plot2_click, addDist = F)
  })
  
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addMarkers(data = stade, ~Longitude, ~Latitude, popup = paste0("<b> Equipe : </b>"
                                                                     , stade$Equipe
                                                                     , "<br>"
                                                                     ,"<b> Stade : </b>"
                                                                     , stade$Stade
                                                                     ) )
  
    
  })
  
})
