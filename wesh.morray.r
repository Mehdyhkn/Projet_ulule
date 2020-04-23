library(shiny)
library(markdown)
library(shinythemes)
library(ggplot2)

#__________________________________________________________________________________________________
#______________________________________________FONCTIONS___________________________________________
#__________________________________________________________________________________________________
erreur <- function(){
  Mes1 <- "Malheureusement il n'y a pas de données pour le graphique demandé."
  Mes2 <- "Il semblerait que vous ayez trouvé une faille importante de notre site."
  Mes3 <- "In the future everything is dockerized."
  Mes4 <- "..."
  Mes <- sample(c(Mes1,Mes2,Mes3,Mes4),1,replace=T,prob = c(1/4,1/4,1/4,1/4))
  stop(Mes)
}

campfinan <- function(ulule,dan,dcat){
  
  
  if(dcat != "Toutes"){
    #On filtre par rapport à la catégorie choisie
    an21 <-  ulule %>% filter(category == dcat)
  }
  else{an21 <- ulule}  #Sinon on ne filtre pas 
  
  if(dan != "Toutes"){
    #On filtre par rapport à la catégorie choisie
    an21 <- an21 %>% filter(annee_depart == dan)
    xdescription <- "Mois"
    titre <- str_c("Nombres de campagnes au cours de l'année"  ,toString(dan),sep=" ")
    x=count(an21, mois_depart)$mois_depart
    y =count(an21, mois_depart)$n
  }
  else{
  titre <- "Nombres de campagnes au cours des années"
  xdescription <- "Années"
  x=count(an21, annee_depart)$annee_depart
  y =count(an21, annee_depart)$n
  }
  
  if (length(x)==0 || length(y)==0){
    erreur()
    }
  else{
  plot(x,y,type = "b",col = "blue", xlab=xdescription , ylab="Nombres de campagnes", lwd = 2, main = titre)
  }
}


propfinan <- function(ulule,dan,dcat){
  
  if(dcat != "Toutes"){
    #On filtre par rapport à la catégorie choisie
    propor <-  ulule %>% filter(category == dcat)
  }
  else{propor <- ulule}  #Sinon on ne filtre pas  
  
  if(dan=="Toutes"){
    x <- na.omit(unique(year(propor$date_start)))
    table <- table(propor$goal_raised,propor$annee_depart)
    xdescription <- "Années"
    title <- "Proportion de campagnes financées au cours des année"
    xlimite <- c(2011,2018)
  }
  else{
    propor <- propor %>% filter( year(date_start) == dan)
    x <- sort(unique(month(propor$date_start)))
    table <- table(propor$goal_raised,propor$mois_depart)
    xdescription <- "Mois"
    title <- str_c("Proportion de campagnes financées au cours de l'année"  ,toString(dan),sep=" ")
    xlimite <- c(1,12)
  }
  
  if(sum(table)<=10){
    erreur()
  }
  else{
    propor <- table[2,]/colSums(table)
    plot(x,propor , type = "b" , col = "red" , xlab=xdescription,main=title,
         ylab="Proportion",ylim=c(0,1),xlim=xlimite)
  }
  
  
 
}



montantfinan <- function(ulule,dan,dcat){
  
  ulule %>% filter(goal_raised == TRUE)
  if(dcat != "Toutes"){
    #On filtre par rapport à la catégorie choisie
    propor <-  ulule %>% filter(category == dcat)
  }
  else{propor <- ulule}  #Sinon on ne filtre pas  
  
  if(dan=="Toutes"){
    x <- na.omit(unique(year(propor$date_start)))
    y <- propor %>%   group_by(year(date_start))  %>% summarise(moyenne = mean(Mont_conv_euro,na.rm = TRUE)) %>% drop_na()
    xdescription <- "Années"
    title <- "Moyenne des montants des campagnes financées en euros au cours des année"
    xlimite <- c(2011,2018)
  }
  else{
    propor <- propor %>% filter( year(date_start) == dan)
    x <- sort(unique(month(propor$date_start)))
    y <- propor %>%   group_by(month(date_start))  %>% summarise(moyenne = mean(Mont_conv_euro,na.rm = TRUE)) %>% drop_na()
    xdescription <- "Mois"
    title <- str_c("Moyenne des montants des campagnes financées en euros au cours de l'année"  ,toString(dan),sep=" ")
    xlimite <- c(1,12)
  }
  
  if (length(x)==0 || length(y)==0){
    erreur()
  }
  else{
    plot(x,y$moyenne , type = "b" , col = "red" , xlab=xdescription,main=title,,xlim=xlimite,ylab=" ")
    }
  
}









cashflow <- function(num){
  catt <-  c("Mode & Design", "Edition & Journal.","Musique" ,"Enfance & Educ.",  "Film et Vidéo","Spectacle vivant","Solidaire & Citoyen","Sports","Art & Photo"  ,"BD" ,"Autres projets", "Technologie" , "Artisanat & Cuisine" ,"Patrimoine" ,"Jeux", "Santé & Bien-etre")
  moyen_cash= rep(0,length(catt))
  for (i in 1:length(catt)) {
    ae <- ulule %>% filter(category==catt[i] & goal_raised==TRUE)
    moyen_cash[i] = mean(ae$amount_raised)
  }
  tabcash <- data.frame(Catégorie=catt,Montant_moyen_obtenu = moyen_cash) %>% arrange(desc(Montant_moyen_obtenu))
  tabcash[1:num,]
}


topi<- function(num){
  ca<- ulule %>% filter(goal_raised == TRUE) %>% count(category) %>% arrange(desc(n))
  names(ca)[2]<- "Nombres de campagnes financées"
  top3 <- ca[1:num,]$category
  plot(x=count(filter(ulule , category==top3[1]), annee_depart)$annee_depart, y =count(filter(ulule , category==top3[1]), annee_depart)$n,xlab="Années de suivis des campagnes", ylab="Nombres de campagnes")
  couleur <- c("red","orange","blue","black","purple")
  
  for (i in 1:num) {
    lines(x=count(filter(ulule , category==top3[i]), annee_depart)$annee_depart, y =count(filter(ulule , category==top3[i]), annee_depart)$n,
          type="b",pch=19,col = sample(couleur,1),xlab="Années de suivis des campagnes", ylab="Nombres de campagnes", lwd = 2)}
}







#____________________________________________________________________________________________________________________
#________________________________________________________UI__________________________________________________________
#____________________________________________________________________________________________________________________
ui <- shinyUI(navbarPage("App",theme = shinytheme("flatly"),
                         tabPanel("Détails du projet",
                                  tags$div(
                                    tags$h4("Les Données"), 
                                    h6(),
                                    "Les données (apercu et téléchargement dans l'onglet Data) proviennent du site web d'ulule  une plateforme de financement participatif au cours de l'année 2019 que nous vous invitons à regarder : ",tags$br(),
                                    tags$a(href="https://fr.ulule.com", "Make good things happen"),tags$br(),
                
                                    "Au soutien de ce jeu de donnée, nous avons pour mission de concevoir et déployer une application Shiny, que voici, permettant de suivre l’évolution des campagnes de financement participatif du site Ulule",tags$br(),
                                    "Le jeu de données est composé de ",tags$br(),
                                    
                                    tags$br(),tags$h4("Background"), 
                                    "Avant tout cela nous avons eu à effectuer des transformations sur le jeu de données: Une épuration . Notre étude se fera donc en fonction de plusieurs restrictions qui permettent une meilleure analyse du jeu de donnée.
                                    A savoir nous ne prendrons en compte que les campagnes qui n'ont pas été annulées et nous allons nous restreindre au périmètre des 8 pays ayant le plus de campagnes au total.",
                                    tags$br(),tags$br(),
                                    tags$h4("Code"),
                                    "Les codes utilisé pour générer cette application Shiny sont disponible sur",tags$a(href="https://github.com/eparker12/nCoV_tracker", "Github."),
                                    tags$br(),tags$br(),tags$h4("Authors"),
                                    "HOUNKONNOU Mehdy, Master 1 Modélisation Statistique et Stochastique, Université de Bordeaux",tags$br(),
                                    "LABARBARIE Pol, Master 1 Modélisation Statistique et Stochastique, Université de Bordeaux",tags$br(),
                                    tags$br(),tags$br(),tags$h4("Contact"),
                                    "mehdyhkn@yahoo.com",tags$br(),tags$br(),
                                    "**adresse mail de pol",tags$br(),tags$br()
                                    )
                         )
                         
                         
                         
                         ,
                         tabPanel("Data",
                                  numericInput("maxrows", "Rows to show", 25),
                                  verbatimTextOutput("rawtable"),
                                  downloadButton("downloadCsv", "Download as CSV"),tags$br(),tags$br(),
                                  "Adapted from timeline data published by ", tags$a(href="lien github", "Pol Labarbarie et Mehdy
                                                                                     HOUNKONNOU"
                                                                                  )
                         ),
                         
                         tabPanel("Visualtions",  
                                  sidebarLayout(
                                    sidebarPanel(  
                                      
                                      selectInput("type", "Choissisez la visualisation",
                                                  choices = c("Nombre total de campagnes crées"=1,"Proportion de campagnes financées"=2,
                                                              "Montant moyen des campagnes financés"=3)),
                                      selectInput("an", "Années",
                                                  choices = c("Toutes","2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018")),
                                      selectInput("cat" , "Choix de la catégorie", 
                                                  choices = c("Toutes","Mode & Design", "Edition & Journal.","Musique" ,"Enfance & Educ.",        
                                                              "Film et Vidéo","Spectacle vivant","Solidaire & Citoyen","Sports",               
                                                              "Art & Photo"  ,"BD" ,"Autres projets", "Technologie" ,          
                                                              "Artisanat & Cuisine" ,"Patrimoine" ,"Jeux",                   
                                                              "Santé & Bien-etre") )),
                                      mainPanel( 
                                      headerPanel("Graphe"), 
                                      plotOutput("Toto")))),
                         
                         tabPanel("Analyses Brièves",
                                  p("Dans cette partie nous allons présenter les meilleurs catégories  ;)", style = "font-family: 'times'; font-si19pt"),
                                  fluidRow(
                                    column(6,offset = 3 ,plotOutput('plottop'))),
                                  hr(),
                                  fluidRow(
                                    column(3,offset=6,
                                           tableOutput("tab")),
                                    hr(),
                                    
                                    fluidRow(
                                      column(4, offset = 3,
                                             selectInput("typ", "Choissisez la visualisation",
                                                         choices = c("Nombre total de campagnes crées",
                                                                     "Montant moyen des campagnes financés"))
                                      ),
                                      column(4, 
                                             selectInput("Top", "Choisissez votre top",
                                                         choices = c("Top 3","Top 5"))
                                      )
                                    )
                                  ))
                         
))

#____________________________________________________________________________________________________________________
#________________________________________________________Server______________________________________________________
#____________________________________________________________________________________________________________________

server <- function(input, output,session){
  output$Toto <- renderPlot({
    dtype <- input$type
    dan <- input$an
    dcat <- input$cat
    if (dtype == 1){
      campfinan(ulule,dan,dcat)
    }
    if (dtype == 2){
      propfinan(ulule,dan,dcat)
    }
    if(dtype==3){
      montantfinan(ulule,dan,dcat)
    }
 
 })
  
  output$plottop <- renderPlot({
    dTop <- input$Top
    dtyp <- input$typ
    if(dtyp=="Nombre total de campagnes crées"){
      if(dTop=="Top 3"){
        topi(3)}
      else{
        topi(5)}}
    #else{
    
    #}
  })
  #affichage de la partie data
  output$rawtable <- renderPrint({
    orig <- options(width = 1000)
    print(tail(ulule %>% select(c(absolute_url,amount_raised,Mont_conv_euro,finished ,category,date_end,date_start,goal_raised )), input$maxrows), row.names = FALSE)
    options(orig)
  })
  #bouton download
  output$downloadCsv <- downloadHandler(
    filename = function() {
      paste("Ulule_custom_2019_", ".csv", sep="")
    },
    content = function(file) {
      write.csv(ulule %>% select(c(absolute_url,amount_raised,Mont_conv_euro,finished ,category,date_end,date_start,goal_raised )), file)
    }
  )
  
  
  output$tab <- renderTable({
    dTop <- input$Top
    dtyp <- input$typ
    if(dtyp=="Nombre total de campagnes crées"){
      if(dTop=="Top 3"){
        ca<- ulule %>% filter(goal_raised == TRUE) %>% count(category) %>% arrange(desc(n))
        names(ca)[2] <- "Nombres de campagnes financées"
        head(ca[1:3,])}
      
      else{
        ca<- ulule %>% filter(goal_raised == TRUE) %>% count(category) %>% arrange(desc(n))
        names(ca)[2] <- "Nombres de campagnes financées"
        head(ca[1:5,])}
    }
    else{
      if(dTop=="Top 3"){
        cashflow(3)
      }
      else{
        cashflow(5)
      }

    }
    
    
    }
)
}
# Run the application 
shinyApp(ui = ui, server = server)
