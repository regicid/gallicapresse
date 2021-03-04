library(shiny)
library(ggplot2)
library(plotly)
library(stringr)
library(purrr)
library(dplyr)
library(xml2)
library(markdown)
library(shinythemes)
library(tidyr)
data = list()


Plot1 <- function(data,input){
  presse = data[["presse"]]
  rownames(presse)<-NULL
  presse$freq<-presse$count/sum(presse$count)
  presse1<-slice_head(presse,n=10)
  presse1<-add_row(presse1,titre="Autre",count=sum(presse$count)-sum(presse1$count),freq=1-sum(presse1$freq))
  plot1<-plot_ly(x=~presse1$count,y=reorder(presse1$titre,presse1$count),type="bar")
  plot1<-layout(plot1, title="Origine des occurrences",xaxis=list(title="Nombre d'occurrences par journal"))
  return(plot1)
}
Plot2 <- function(data,input){
  presse = data[["presse"]]
  rownames(presse)<-NULL
  presse$freq<-presse$count/sum(presse$count)
  presse1<-slice_head(presse,n=10)
  presse1<-add_row(presse1,titre="Autre",count=sum(presse$count)-sum(presse1$count),freq=1-sum(presse1$freq))
  plot2<-plot_ly(x=~presse1$freq,y=reorder(presse1$titre,presse1$freq),type="bar")
  plot2<-layout(plot2, title="Origine des occurrences",xaxis=list(title="Proportion d'occurrences par journal"))
  return(plot2)
}
Plot3 <- function(data,input){
  tableau = data[["tableau"]]
  plot3<-plot_ly(x=~tableau$date,type="histogram",color = tableau$principaux_titres,colors="Dark2")
  plot3<-layout(plot3, title="Distribution des mentions dans la presse française \nselon le journal d'origine", xaxis=list(title="Date"),yaxis=list(title="Nombre de mentions"),barmode="stack")
  return(plot3)
}
Plot4 <- function(data,input){
  tableau = data[["tableau"]]
  plot4<-plot_ly(x=~tableau$date,type="histogram",color = tableau$principaux_titres,colors="Dark2")
  plot4<-layout(plot4, title="Distribution des mentions dans la presse française \nselon le journal d'origine", xaxis=list(title="Date"),yaxis=list(title="Part des mentions pour chaque période"),barmode="stack",barnorm="percent")
  return(plot4)
}
Plot5 <- function(data,input){
  tableau = data[["tableau2"]]
  plot5<-plot_ly(x=~tableau$date,type="histogram",color = tableau$principales_villes,colors="Dark2")
  plot5<-layout(plot5, title="Distribution des mentions dans la presse française \nselon la ville d'édition", xaxis=list(title="Date"),yaxis=list(title="Nombre de mentions"),barmode="stack")
  return(plot5)
}
Plot6 <- function(data,input){
  tableau = data[["tableau2"]]
  plot6<-plot_ly(x=~tableau$date,type="histogram",color = tableau$principales_villes,colors="Dark2")
  plot6<-layout(plot6, title="Distribution des mentions dans la presse française \nselon la ville d'édition", xaxis=list(title="Date"),yaxis=list(title="Part des mentions pour chaque période"),barmode="stack",barnorm="percent")
  return(plot6)
}
get_data <- function(mot,from,to){
  progress <- shiny::Progress$new()
  on.exit(progress$close())
  progress$set(message = "Patience...", value = 0)

      mot2 = str_replace_all(mot," ","%20")
      ###
      or<-""
      or_end<-""
      if(str_detect(mot2,"[+]")){
      mots_or = str_split(mot2,"[+]")[[1]]
      or1<-NA
      or1_end<-NA
      for (j in 2:length(mots_or)) {
        
        or1[j]<-str_c("or%20text%20adj%20%22",mots_or[j],"%22%20")
        or1_end[j]<-str_c("%20",mots_or[j])
        or<-str_c(or,or1[j])
        or_end<-str_c(or_end,or1_end[j])
      }
      mot1<-mots_or[1]} else{mot1=mot2}
      ###
      i=1
      page<-function(i)
      {
        if(doc_type==1)
        {
          beginning=str_c(from,"/01/01")
          end=str_c(to,"/12/31")
          url<-str_c("https://gallica.bnf.fr/SRU?operation=searchRetrieve&exactSearch=true&maximumRecords=50&startRecord=",i,"&collapsing=false&version=1.2&query=(dc.language%20all%20%22fre%22)%20and%20(text%20adj%20%22",mot1,"%22%20",or,")%20%20and%20(dc.type%20all%20%22fascicule%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(gallicapublication_date%3E=%22",beginning,"%22%20and%20gallicapublication_date%3C=%22",end,"%22)&suggest=10&keywords=",mot1,or_end)
        }
        
        if(doc_type==2){
          url<-str_c("https://gallica.bnf.fr/SRU?operation=searchRetrieve&version=1.2&startRecord=0&maximumRecords=50&startRecord=",i,"&collapsing=true&exactSearch=true&query=(dc.language%20all%20%22fre%22)%20and%20(text%20adj%20%22",mot1,"%22%20",or,")%20%20and%20(dc.type%20all%20%22monographie%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(gallicapublication_date%3E=%22",from,"%22%20and%20gallicapublication_date%3C=%22",to,"%22)&suggest=10&keywords=",mot1,or_end)
          
        }
        read_xml(url)
      }
      
      tot <- page(1)
      te <- xml2::as_list(tot)
      nmax <- as.integer(unlist(te$searchRetrieveResponse$numberOfRecords))
      for (j in seq(51, nmax, by = 50)){
        temp <- page(j)
        for (l in xml2::xml_children(temp)){
          xml2::xml_add_child(tot, l)
        }
        progress$inc(j/nmax, message = paste("Téléchargement en cours...",as.integer((j/nmax)*100),"%"))
      }
      progress$set(message = "Patience...", value = 0)
      xml_to_df <- function(doc, ns = xml_ns(doc)) {
        split_by <- function(.x, .f, ...) {
          vals <- map(.x, .f, ...)
          split(.x, simplify_all(transpose(vals)))
        }
        node_to_df <- function(node) {
          # Filter the attributes for ones that aren't namespaces
          # x <- list(.index = 0, .name = xml_name(node, ns))
          x <- list(.name = xml_name(node, ns))
          # Attributes as column headers, and their values in the first row
          attrs <- xml_attrs(node)
          if (length(attrs) > 0) {attrs <- attrs[!grepl("xmlns", names(attrs))]}
          if (length(attrs) > 0) {x <- c(x, attrs)}
          # Build data frame manually, to avoid as.data.frame's good intentions
          children <- xml_children(node)
          if (length(children) >= 1) {
            x <- 
              children %>%
              # Recurse here
              map(node_to_df) %>%
              split_by(".name") %>%
              map(bind_rows) %>%
              map(list) %>%
              {c(x, .)}
            attr(x, "row.names") <- 1L
            class(x) <- c("tbl_df", "data.frame")
          } else {
            x$.value <- xml_text(node)
          }
          x
        }
        node_to_df(doc)
      }
      x = 1:3
      parse_gallica <- function(x){
        xml2::xml_find_all(tot, ".//srw:recordData")[x] %>% 
          xml_to_df() %>% 
          select(-.name) %>% 
          .$`oai_dc:dc` %>% 
          .[[1]] %>% 
          mutate(recordId = 1:nrow(.)) %>% 
          #    tidyr::unnest() %>% 
          tidyr::gather(var, val, - recordId) %>% 
          group_by(recordId, var) %>% 
          mutate(value = purrr::map(val, '.value') %>% purrr::flatten_chr() %>% paste0( collapse = " -- ")) %>% 
          select(recordId, var, value) %>% 
          ungroup() %>% 
          mutate(var = stringr::str_remove(var, 'dc:')) %>% 
          tidyr::spread(var, value) %>% 
          select(-.name)
      }
      for (h in 1:nmax) {
        ligne<-h%>%parse_gallica
        tot_df<-bind_rows(tot_df,ligne)
        progress$inc((h/nmax), detail = paste("Traitement des données",as.integer((h/nmax)*100),"%"))
      }
      
    
      total<-tot_df
      total<-total[,c(3,5,7,11)]
      total$title<-str_replace(total$title,"-"," ")
      total$title<-str_remove_all(total$title,"[\\p{Punct}&&[^']].+")
      total$publisher<-str_replace_all(total$publisher,"-"," ")
      total$publisher<-gsub("[\\(\\)]", "", regmatches(total$publisher, gregexpr("\\(.*?\\)", total$publisher)))
      total$publisher<-str_remove_all(total$publisher,'c"')
      total$publisher<-str_remove_all(total$publisher,'"')
      total$publisher<-str_remove_all(total$publisher,'character0')
      total$identifier<-str_remove_all(total$identifier,"([:blank:].+)")
      
      total$ark=str_extract(total$identifier,"12148/[:alnum:]+")
      total$ark=str_remove_all(total$ark,"12148/")
      
      
      
      #####COMPTAGE PAR TITRE DE PRESSE
      presse<-as.data.frame(unique(total$title))
      colnames(presse)<-c("titre")
      presse$titre<-as.character(presse$titre)
      presse$count<-NA
      for (i in 1:length(presse$titre)) 
      {
        presse$count[i]<-sum(str_count(total$title,presse$titre[i]))
      }
      presse<-presse[order(presse$count,decreasing = TRUE),]
      total$title<-as.factor(total$title)
      
      total$date<-as.Date.character(total$date)
      borne1=as.Date.character(str_c(from,"-01-01"))
      borne2=as.Date.character(str_c(to,"-12-31"))
      total<-total[total$date>=borne1 & total$date<=borne2,]
      total<-total[is.na(total$date)==FALSE,]
      #####DETERMINATION DES PRINCIPAUX TITRES DE PRESSE
      top_titres<-slice_head(presse,n=10)
      
      total$principaux_titres<-"Autre"
      for (i in 1:length(total$title)) 
      {
        if(sum(as.numeric(total$title[i]==top_titres$titre))==1)
        {
          total$principaux_titres[i]<-as.character(total$title[i])
        }
      }
      total$principaux_titres<-as.factor(total$principaux_titres)
      
      #####COMPTAGE PAR VILLES
      total_bis<-total
      total_bis<-total_bis %>%
        mutate(publisher = strsplit(as.character(publisher), ",")) %>%
        unnest() %>%
        filter(publisher != "") %>%
        select(date,identifier,publisher,title,ark,principaux_titres)
      total_bis$publisher<-str_remove(total_bis$publisher,"^ ")
      villes<-as.data.frame(unique(total_bis$publisher))
      colnames(villes)<-c("city")
      villes$city<-as.character(villes$city)
      villes$count<-NA
      for (i in 1:length(villes$city)) 
      {
        villes$count[i]<-sum(str_count(total_bis$publisher,villes$city[i]))
      }
      villes<-villes[order(villes$count,decreasing = TRUE),]
      total_bis$publisher<-as.factor(total_bis$publisher)
      
      
      #####DETERMINATION DES PRINCIPALES VILLES
      top_villes<-slice_head(villes,n=10)
      
      total_bis$principales_villes<-"Autre"
      for (i in 1:length(total_bis$publisher)) 
      {
        if(sum(as.numeric(total_bis$publisher[i]==top_villes$city))==1)
        {
          total_bis$principales_villes[i]<-as.character(total_bis$publisher[i])
        }
      }
      total_bis$principales_villes<-as.factor(total_bis$principales_villes)
      

      data=list(total,presse,total_bis,villes)
      names(data) = c("tableau","presse","tableau2","villes")
  return(data)}


ui <- navbarPage("Gallicapresse",
                 tabPanel("Graphique",fluidPage(),
                          tags$head(
                            tags$style(HTML(".shiny-output-error-validation{color: red;}"))),
                          pageWithSidebar(headerPanel(''),
                                          sidebarPanel(
                                            textInput("mot","Terme(s) à chercher","Clemenceau"),
                                            p('Utiliser "a+b" pour rechercher a OU b'),
                                            numericInput("beginning","Début",1914),
                                            numericInput("end","Fin",1920),
                                            actionButton("do","Générer le graphique"),
                                            checkboxInput("relative", "Afficher les résultats en valeurs relatives", value = FALSE),
                                            downloadButton('downloadData', 'Télécharger les données'),
                                            downloadButton('downloadPlot', 'Télécharger le graphique interactif')
                                          ),
                                          
                                          mainPanel(plotlyOutput("plot1"),
                                                    p(""),
                                                    plotlyOutput("plot2"),
                                                    p(""),
                                                    plotlyOutput("plot3"),
                                                    p(""),
                                                    plotlyOutput("plot4"),
                                                    p(""),
                                                    plotlyOutput("plot5"),
                                                    p(""),
                                                    plotlyOutput("plot6")))),
                 tabPanel("Notice",shiny::includeMarkdown("Notice.md"))
)



# Define server logic required to draw a histogram
server <- function(input, output){
  observeEvent(input$do,{
    datasetInput <- reactive({
      data$tableau})
    df = get_data(input$mot,input$beginning,input$end)
    # if(input$relative){
    output$plot2 <- renderPlotly({Plot2(df,input)})
    output$plot4 <- renderPlotly({Plot4(df,input)})
    output$plot6 <- renderPlotly({Plot6(df,input)})
    # }
    # else{
      output$plot1 <- renderPlotly({Plot1(df,input)})
      output$plot3 <- renderPlotly({Plot3(df,input)})
      output$plot5 <- renderPlotly({Plot5(df,input)})
    # }
   
    output$downloadData <- downloadHandler(
      filename = function() {
        paste('data-', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        write.csv(df$tableau, con)
      })
    output$downloadPlot <- downloadHandler(
      filename = function() {
        paste('plot-', Sys.Date(), '.html', sep='')
      },
      content = function(con) {
        htmlwidgets::saveWidget(as_widget(Plot(df,input)), con)
      })
  })
  
  
}

shinyApp(ui = ui, server = server)
