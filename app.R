library(shiny)
library(shinythemes)
library(shinydashboard)
library(gdata)
library(tidyr)
library(stringr)
library(dplyr)
library(ggplot2)
library(leaflet)
library(DT)

#Optimisation de la base de données
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
  
  
ui<-function(request){
  
  sidebar<-dashboardSidebar(
    hr(),
    sidebarMenu(id="tabs",
                menuItem("Accueil", tabName = "acc", icon=icon("home"), selected=TRUE),
                menuItem("Stade", tabName="st", icon=icon("map")),
                menuItem("Effectif", tabName="eff", icon=icon("users")),
                menuItem("Graphiques", icon=icon("chart-line"),
                         menuSubItem("Graphique 1", tabName = "g1"),
                         menuSubItem("Graphiques 2", tabName = "g2"))
    ))
  body<- dashboardBody(
    tabItems(tabItem(tabName = "eff",
                     fluidPage(
                       h1("Effectif par équipe", style="font-family:monospace;text-align:center" ),
                       selectInput("inclub","Selectionnez un club", choices= sort(pl$Equipe)),
                       dataTableOutput("playerdata")
                     )),
             tabItem(tabName = "acc",
                     fluidPage( fluidRow(column(12,align="center", h1("Première League saison 2020-2021", style="font-family:monospace" ))),
                                fluidRow(column(12, align="center", tags$img(src="pl.png",width="500px",height="200px", align ="center"))), 
                                
                                
                                br(),
                                p(strong("Bienvenue sur cette application"),", elle présente différents aspects de la", strong("Première League"), "qui a eu lieu en", strong("2020-2021"), ". Cette compétition regroupe les 20 meilleurs équipes anglaises. À la fin de la saison, les 6 premères équipes sont qualifiées dans différentes compétions européennes (Champion's League, Europa League), la 7ème passe un tour de barrage pour la Conference League. Enfin les 3 derniers du championnat sont relégués à l'échellon inférieur (Championship). Voici ci dessous, l'introduction de la première league :  
                                     ",style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:20px"),
                                br(),
                                
                                fluidRow(column(12, align= "center",HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/59LP0ofNT5I" frameborder="0" allow="accelerometer; autoplay; encrypted-media; picture-in-picture" allowfullscreen></iframe>'))),
                                
                                br(),
                                p("Dans cette application, nous n'allons pas nous concentré sur le classement final de la saison. Nous allons regardé différentes dimensions de cette saison. Nous allons utiliser 2 bases de données : une qui est concentré sur les joueurs de chaque équipe. On peut y voir les buts, passes décisives, leur nationalité, leur poste... Mais nous n'allons pas utiliser toutes les colonnes de celle ci. La deuxième base de données nommée stadiums.csv regroupe les stades ainsi que leur localisation. On peut parler des différentes sections présentent dans cetta application :", 
                                  br() , strong("Stade"), "qui regroupe une map avec tous les stades de la league" ,
                                  br(), strong("Effectif"),"qui regroupe les effectifs de chaque équipe",
                                  br(), strong("Graphiques"), "où on peut retrouver 2 plots interactifs",
                                  style="text-align;color:black;background-color:lavender;padding:15p;border-radius=20px"),
                                br(),
                                p(em("Application R shiny développée par :"),"Bouhamidi Yacine et Gévaudan Bastien",style="text-align:center"),
                                p(em("La base de données sur les ont été trouvé sur ce "),
                                  a(href="https://fbref.com/fr/comps/9/10728/Stats-2020-2021-Premier-League", "site",target="_blank"),style="text-align:center;color:black")
                                
                     )),
             tabItem(tabName = "st",
                     fluidPage(
                       h1("Map des stades", style="font-family:monospace;text-align:center"),
                       leafletOutput("mymap"),
                       dataTableOutput("stade")
                     )),
             tabItem(tabName = "g1",
                     fluidPage(
                       h1("Graphique numéro 1", style="font-family:monospace;text-align:center"),
                       box(solidHeader= TRUE,
                           plotOutput("plot1")),
                       box(title="Inputs",statuts="info",solidHeader = TRUE,
                           numericInput('x', 'Nombre de matchs joués', 3, min = 0, max = 38),
                           numericInput('y', 'Nombre de buts', 3, min = 0, max = 38))
                     )
                     
             ),
             tabItem(tabName = "g2",
                     fluidPage(h1("Graphiques numéro 2", style="font-family:monospace;text-align:center"),
                               p("Légende : MJ = matchs joués, PDT=Passes décisives",
                                 style="text-align:center;color:black;background-color:lavender;padding:15px;border-radius:20px"),
                               box(title =  "Graphique général", status="primary", solidHeader = TRUE,
                                   selectInput("xcol", "Selectionnez le x du graphique", choices = c("MJ","Titulaire", "MinutesJouees")),
                                   selectInput("ycol", "Selectionnez le y du graphique", choices = c("Buts", "PDT", "ButsSansPenT")),
                                   p(strong("Graphique du x en fonction du y avec toutes les équipes "),style="text-align:center"),
                                   br(),
                                   plotOutput("plot2", click = "plot2_click"),
                                   p("Cliquez sur un point, voici le (ou les) joueur(s) représenté(s) par ce point :", style="text-align:justify"),
                                   verbatimTextOutput("click_info")),
                               box(title="Graphique selon l'équipe", status="primary", solidHeader = TRUE,height = "700px",
                                   tabBox(height = "250px", width="450",
                                          tabPanel("Nuage de points",p(strong("Graphique du x en fonction du y en fonction d'une équipe choisie "),style="text-align:center"),
                                                   br(),
                                                   selectInput("inclub2","Selectionnez un club", choices= sort(pl$Equipe), selected= (pl$Equipe=="Arsenal")),
                                                   plotOutput("plot3")),
                                          tabPanel("Boxplot",p(strong("Boxplot de la variable x en fonction de l'équipe choisie"), style="text-align:center"),
                                                   plotOutput("plot4"))))
                               
                               
                     )
                     
             )
    ))
  
  
  
  
  
  dashboardPage(
    dashboardHeader(title = "Première League"),
    sidebar,
    body
  )
}


server = function(input, output, session) {
 
  
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
  
}


shinyApp(ui, server)
