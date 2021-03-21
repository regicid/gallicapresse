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
library(leaflet.extras)
library(htmltools)
library(lubridate)
library(httr)
data = list()


Plot1 <- function(data,input){
  presse = data[["presse"]]
  rownames(presse)<-NULL
  presse$freq<-presse$count/sum(presse$count)
  presse1<-slice_head(presse,n=10)
  presse1<-add_row(presse1,titre="Autre",count=sum(presse$count)-sum(presse1$count),freq=1-sum(presse1$freq))
  plot1<-plot_ly(x=~presse1$count,y=reorder(presse1$titre,presse1$count),type="bar")
  plot1<-layout(plot1, title="Origine des occurrences : les dix principaux journaux d'origine",xaxis=list(title="Nombre d'occurrences par journal"))
  return(plot1)
}
Plot2 <- function(data,input){
  presse = data[["presse"]]
  rownames(presse)<-NULL
  presse$freq<-presse$count/sum(presse$count)
  presse1<-slice_head(presse,n=10)
  presse1<-add_row(presse1,titre="Autre",count=sum(presse$count)-sum(presse1$count),freq=1-sum(presse1$freq))
  plot2<-plot_ly(x=~presse1$freq,y=reorder(presse1$titre,presse1$freq),type="bar")
  plot2<-layout(plot2, title="Origine des occurrences : les dix principaux journaux d'origine",xaxis=list(title="Proportion d'occurrences par journal",tickformat = ".1%"))
  return(plot2)
}
Plot3 <- function(data,input){
  tableau = data[["tableau"]]
  plot3<-plot_ly(x=~tableau$date,type="histogram",color = tableau$principaux_titres,colors="Dark2")
  plot3<-layout(plot3, title="Distribution des mentions dans la presse française \nselon le journal d'origine", xaxis=list(title="Date",type="date",tickformat = "%b %Y",tickangle="-45"),yaxis=list(title="Nombre de mentions"),barmode="stack")
  return(plot3)
}
Plot4 <- function(data,input){
  tableau = data[["tableau"]]
  plot4<-plot_ly(x=~tableau$date,type="histogram",color = tableau$principaux_titres,colors="Dark2")
  plot4<-layout(plot4, title="Distribution des mentions dans la presse française \nselon le journal d'origine", xaxis=list(title="Date",type="date",tickformat = "%b %Y",tickangle="-45"),yaxis=list(title="Part des mentions pour chaque période"),barmode="stack",barnorm="percent")
  return(plot4)
}
Plot5 <- function(data,input){
  villes = data[["villes"]]
  rownames(villes)<-NULL
  villes$freq<-villes$count/sum(villes$count)
  villes1<-slice_head(villes,n=10)
  villes1<-add_row(villes1,city="Autre",count=sum(villes$count)-sum(villes1$count),freq=1-sum(villes1$freq))
  plot5<-plot_ly(x=~villes1$count,y=reorder(villes1$city,villes1$count),type="bar")
  plot5<-layout(plot5, title="Origine géographique des occurrences : les dix principales villes de publication",xaxis=list(title="Nombre d'occurrences par ville"))
  return(plot5)
}
Plot6 <- function(data,input){
  villes = data[["villes"]]
  rownames(villes)<-NULL
  villes$freq<-villes$count/sum(villes$count)
  villes1<-slice_head(villes,n=10)
  villes1<-add_row(villes1,city="Autre",count=sum(villes$count)-sum(villes1$count),freq=1-sum(villes1$freq))
  plot6<-plot_ly(x=~villes1$freq,y=reorder(villes1$city,villes1$freq),type="bar")
  plot6<-layout(plot6, title="Origine géographique des occurrences : les dix principales villes de publication",xaxis=list(title="Proportion d'occurrences par ville",tickformat = ".1%"))
  return(plot6)
}
Plot7 <- function(data,input){
  tableau = data[["tableau2"]]
  plot7<-plot_ly(x=~tableau$date,type="histogram",color = tableau$principales_villes,colors="Dark2")
  plot7<-layout(plot7, title="Distribution des mentions dans la presse française \nselon la ville d'édition", xaxis=list(title="Date",type="date",tickformat = "%b %Y",tickangle="-45"),yaxis=list(title="Nombre de mentions"),barmode="stack")
  return(plot7)
}
Plot8 <- function(data,input){
  tableau = data[["tableau2"]]
  plot8<-plot_ly(x=~tableau$date,type="histogram",color = tableau$principales_villes,colors="Dark2")
  plot8<-layout(plot8, title="Distribution des mentions dans la presse française \nselon la ville d'édition", xaxis=list(title="Date",type="date",tickformat = "%b %Y",tickangle="-45"),yaxis=list(title="Part des mentions pour chaque période"),barmode="stack",barnorm="percent")
  return(plot8)
}
Plot9 <- function(data,input){
  theme = data[["theme"]]
  rownames(theme)<-NULL
  theme$freq<-theme$count/sum(theme$count)
  theme1<-slice_head(theme,n=10)
  theme1<-add_row(theme1,thematique="Autre",count=sum(theme$count)-sum(theme1$count),freq=1-sum(theme1$freq))
  plot9<-plot_ly(x=~theme1$count,y=reorder(theme1$thematique,theme1$count),type="bar")
  plot9<-layout(plot9, title="Thématique des journaux : les dix principaux thèmes (Dewey)",xaxis=list(title="Nombre d'occurrences par thématique"))
  return(plot9)
}
Plot10 <- function(data,input){
  theme = data[["theme"]]
  rownames(theme)<-NULL
  theme$freq<-theme$count/sum(theme$count)
  theme1<-slice_head(theme,n=10)
  theme1<-add_row(theme1,thematique="Autre",count=sum(theme$count)-sum(theme1$count),freq=1-sum(theme1$freq))
  plot10<-plot_ly(x=~theme1$freq,y=reorder(theme1$thematique,theme1$freq),type="bar")
  plot10<-layout(plot10, title="Thématique des journaux : les dix principaux thèmes (Dewey)",xaxis=list(title="Proportion d'occurrences par thématique",tickformat = ".1%"))
  return(plot10)
}
Plot11 <- function(data,input){
  tableau = data[["tableau"]]
  plot11<-plot_ly(x=~tableau$date,type="histogram",color = tableau$principaux_themes,colors="Dark2")
  plot11<-layout(plot11, title="Distribution des mentions dans la presse française \nselon le thème du journal d'origine", xaxis=list(title="Date",type="date",tickformat = "%b %Y",tickangle="-45"),yaxis=list(title="Nombre de mentions"),barmode="stack")
  return(plot11)
}
Plot12 <- function(data,input){
  tableau = data[["tableau"]]
  plot12<-plot_ly(x=~tableau$date,type="histogram",color = tableau$principaux_themes,colors="Dark2")
  plot12<-layout(plot12, title="Distribution des mentions dans la presse française \nselon le thème du journal d'origine", xaxis=list(title="Date",type="date",tickformat = "%b %Y",tickangle="-45"),yaxis=list(title="Part des mentions pour chaque période"),barmode="stack",barnorm="percent")
  return(plot12)
}
Plot13<-function(data,input){
  tableau<-data[["tableau2"]]
  
  leaflet(tableau) %>% addProviderTiles(providers$Stamen.Toner) %>%
    setView( lng=2.25, lat=47.15, 5 ) %>% addMarkers(
    clusterOptions = markerClusterOptions(), popup = ~htmlEscape(paste(title," ", date,"\n",identifier)))
}

Plot14 <- function(data,input){
  quotidiens = data[["quotidiens"]]
  rownames(quotidiens)<-NULL
  quotidiens$freq<-quotidiens$count/sum(quotidiens$count)
  plot14<-plot_ly(x=~quotidiens$count,y=reorder(quotidiens$periodicite,quotidiens$count),type="bar")
  plot14<-layout(plot14, title="Périodicité des journaux",xaxis=list(title="Nombre d'occurrences par catégorie"))
  return(plot14)
}
Plot15 <- function(data,input){
  quotidiens = data[["quotidiens"]]
  rownames(quotidiens)<-NULL
  quotidiens$freq<-quotidiens$count/sum(quotidiens$count)
  plot15<-plot_ly(x=~quotidiens$freq,y=reorder(quotidiens$periodicite,quotidiens$freq),type="bar")
  plot15<-layout(plot15, title="Périodicité des journaux",xaxis=list(title="Proportion d'occurrences par catégorie",tickformat = ".1%"))
  return(plot15)
}
Plot16 <- function(data,input){
  tableau = data[["tableau"]]
  plot16<-plot_ly(x=~tableau$date,type="histogram",color = tableau$is_quotidien,colors="Dark2")
  plot16<-layout(plot16, title="Distribution des mentions dans la presse française \nselon la périodicité du journal d'origine", xaxis=list(title="Date",type="date",tickformat = "%b %Y",tickangle="-45"),yaxis=list(title="Nombre de mentions"),barmode="stack")
  return(plot16)
}
Plot17 <- function(data,input){
  tableau = data[["tableau"]]
  plot17<-plot_ly(x=~tableau$date,type="histogram",color = tableau$is_quotidien,colors="Dark2")
  plot17<-layout(plot17, title="Distribution des mentions dans la presse française \nselon la périodicité du journal d'origine", xaxis=list(title="Date",type="date",tickformat = "%b %Y",tickangle="-45"),yaxis=list(title="Part des mentions pour chaque période"),barmode="stack",barnorm="percent")
  return(plot17)
}

temps_traitement<-function(mot,dateRange){
  from<-min(dateRange)
  to<-max(dateRange)
  from<-str_replace_all(as.character(from),"-","/")
  to<-str_replace_all(as.character(to),"-","/")
  
  
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
    
    beginning=from
    end=to
    url<-str_c("https://gallica.bnf.fr/SRU?operation=searchRetrieve&exactSearch=true&maximumRecords=50&startRecord=",i,"&collapsing=false&version=1.2&query=(dc.language%20all%20%22fre%22)%20and%20(text%20adj%20%22",mot1,"%22%20",or,")%20%20and%20(dc.type%20all%20%22fascicule%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(gallicapublication_date%3E=%22",beginning,"%22%20and%20gallicapublication_date%3C=%22",end,"%22)&suggest=10&keywords=",mot1,or_end)
    
    read_xml(url)
  }
  
  tot <- page(1)
  te <- xml2::as_list(tot)
  nmax <- as.integer(unlist(te$searchRetrieveResponse$numberOfRecords))
  traitement<-as.integer(nmax*(0.013+0.0225))
  traitement=seconds_to_period(traitement)
  traitement=as.character(traitement)
  traitement=str_replace_all(traitement,"M"," minutes")
  traitement=str_replace_all(traitement,"S"," secondes")
  traitement=str_replace_all(traitement,"H"," heures")
  message<-str_c(nmax," numéros de presse trouvés. \nDélai de traitement estimé : ",traitement,".")
  return(message)
}

prepare_data <- function(mot,dateRange){
  progress <- shiny::Progress$new()
  on.exit(progress$close())
  progress$set(message = "Patience...", value = 0)
  
  
  from<-min(dateRange)
  to<-max(dateRange)
  from<-str_replace_all(as.character(from),"-","/")
  to<-str_replace_all(as.character(to),"-","/")

  
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
    
    beginning=from
    end=to
    url<-str_c("https://gallica.bnf.fr/SRU?operation=searchRetrieve&exactSearch=true&maximumRecords=50&startRecord=",i,"&collapsing=false&version=1.2&query=(dc.language%20all%20%22fre%22)%20and%20(text%20adj%20%22",mot1,"%22%20",or,")%20%20and%20(dc.type%20all%20%22fascicule%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(gallicapublication_date%3E=%22",beginning,"%22%20and%20gallicapublication_date%3C=%22",end,"%22)&suggest=10&keywords=",mot1,or_end)
    
    read_xml(RETRY("GET",url,times = 6))
  }
  
  tot <- page(1)
  te <- xml2::as_list(tot)
  nmax <- as.integer(unlist(te$searchRetrieveResponse$numberOfRecords))
  for (j in seq(51, nmax, by = 50)){
    temp <- page(j)
    for (l in xml2::xml_children(temp)){
      xml2::xml_add_child(tot, l)
    }
    progress$inc(50/nmax, message = paste("Téléchargement en cours...",as.integer((j/nmax)*100),"%"))
  }
  progress$set(message = "Traitement en cours ; cette étape va être longue...", value = 100)
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
  # h=1
  # tot_df<-h%>%parse_gallica
  # for (h in 2:nmax) {
  #   ligne<-h%>%parse_gallica
  #   tot_df<-bind_rows(tot_df,ligne)
  #   progress$inc((1/nmax), detail = paste("Traitement des données",as.integer((h/nmax)*100),"%"))
  # }
  tot_df <- 1:nmax %>% 
    parse_gallica %>% 
    bind_rows()
  return(tot_df)
}


get_data<-function (tot_df,mot,dateRange,mois_pub){
  from<-as.character(min(dateRange))
  to<-as.character(max(dateRange))
  
  total<-tot_df
  total<-select(total,date,identifier,publisher,title,relation)
  total$relation<-str_remove_all(total$relation,".+-- ")
  total$relation<-str_remove_all(total$relation,"https://gallica.bnf.fr/ark:/12148/")
  total$relation<-str_replace_all(total$relation,"/","_")
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
  presse<-total %>% 
    group_by(title) %>%
    summarise(count = length(title))
  colnames(presse)<-c("titre","count")
  presse<-presse[presse$titre!="",]
  presse<-presse[presse$titre!=" ",]
  presse<-presse[is.na(presse$titre)==FALSE,]
  presse<-presse[order(presse$count,decreasing = TRUE),]
  total$title<-as.factor(total$title)
  total$date<-as.character(total$date)  
  
  total$date[str_length(total$date)==7]<-str_c(total$date[str_length(total$date)==7],"-01")
  if(mois_pub==FALSE){total$date[str_length(total$date)==4]<-str_c(total$date[str_length(total$date)==4],"-01-01")}
  
  
  total<-total[str_length(total$date)==10,]
  
  total$date<-as.Date.character(total$date)
  borne1=from
  borne2=to
  total<-total[total$date>=borne1 & total$date<=borne2,]
  total<-total[is.na(total$date)==FALSE,]
  total<-total[order(total$date),]
  #####DETERMINATION DES PRINCIPAUX TITRES DE PRESSE
  top_titres<-slice_head(presse,n=10)
  total$principaux_titres<-"Autre"
  total$principaux_titres[is.element(total$title,top_titres$titre)]<-as.character(total$title[is.element(total$title,top_titres$titre)])
  total$principaux_titres<-as.factor(total$principaux_titres)
  
  #####COMPTAGE PAR VILLES
  total_bis<-total
  total_bis<-total_bis %>%
    mutate(publisher = strsplit(as.character(publisher), ",")) %>%
    unnest() %>%
    filter(publisher != "") %>%
    select(date,identifier,publisher,title,ark,principaux_titres)
  total_bis<-total_bis %>%
    mutate(publisher = strsplit(as.character(publisher), " puis")) %>%
    unnest() %>%
    filter(publisher != "") %>%
    select(date,identifier,publisher,title,ark,principaux_titres)
  total_bis$publisher<-str_remove_all(total_bis$publisher,"^ ")
  total_bis$publisher<-str_remove_all(total_bis$publisher,"$ ")
  total_bis$publisher<-str_remove_all(total_bis$publisher,"^([:alpha:] )")
  total_bis$publisher<-str_remove_all(total_bis$publisher,"\\]")
  total_bis$publisher<-str_remove_all(total_bis$publisher,"\\[")
  total_bis$publisher<-str_remove_all(total_bis$publisher,"@")
  total_bis$publisher<-str_remove_all(total_bis$publisher,"\\?")
  total_bis$publisher<-str_remove_all(total_bis$publisher,"^[:punct:]")
  total_bis$publisher<-str_remove_all(total_bis$publisher,"$[:punct:]")
  total_bis$publisher<-iconv(total_bis$publisher,from="UTF-8",to="ASCII//TRANSLIT")
  total_bis$publisher<-str_remove_all(total_bis$publisher,"^ ")
  total_bis$publisher<-str_remove_all(total_bis$publisher,"$ ")
  total_bis<-total_bis[total_bis$publisher!="",]
  total_bis<-distinct(total_bis)
  villes<-total_bis %>% 
    group_by(publisher) %>%
    summarise(count = length(publisher))
  colnames(villes)<-c("city","count")
  villes<-villes[order(villes$count,decreasing = TRUE),]
  total_bis$publisher<-as.factor(total_bis$publisher)
  
  
  #####DETERMINATION DES PRINCIPALES VILLES
  top_villes<-slice_head(villes,n=10)
  total_bis$principales_villes<-"Autre"
  total_bis$principales_villes[is.element(total_bis$publisher,top_villes$city)]<-as.character(total_bis$publisher[is.element(total_bis$publisher,top_villes$city)])
  total_bis$principales_villes<-as.factor(total_bis$principales_villes)
  total_bis<-distinct(total_bis)
  
  #####THEMES DES JOURNAUX
  liste<-read.csv("liste_journaux_gallica_quotidiens.csv",encoding = "UTF-8")
  liste<-select(liste,ark,sdewey_nom,sdewey_nom2,is_quotidien)
  colnames(liste)<-c("relation","sdewey_nom","sdewey_nom2","is_quotidien")
  total<-left_join(total,liste,"relation")
  total$sdewey_nom<-as.character(total$sdewey_nom)
  total$sdewey_nom[total$sdewey_nom==""]<-"N/A"
  total$sdewey_nom2[total$sdewey_nom2==""]<-"N/A"
  total$sdewey_nom[is.na(total$sdewey_nom)]<-"N/A"
  total$sdewey_nom2[is.na(total$sdewey_nom2)]<-"N/A"
  total$sdewey_nom<-as.factor(total$sdewey_nom)
  total$sdewey_nom2<-as.factor(total$sdewey_nom2)
  #####COMPTAGE PAR THEMATIQUE
  theme<-total %>% 
    group_by(sdewey_nom) %>%
    summarise(count = length(sdewey_nom))
  colnames(theme)<-c("thematique","count")
  theme<-theme[order(theme$count,decreasing = TRUE),]
  #####DETERMINATION DES PRINCIPAUX THEMES
  top_themes<-slice_head(theme,n=10)
  total$principaux_themes<-"Autre"
  total$principaux_themes[is.element(total$sdewey_nom,top_themes$thematique)]<-as.character(total$sdewey_nom[is.element(total$sdewey_nom,top_themes$thematique)])
  total$principaux_themes<-as.factor(total$principaux_themes)
  
  #Ajout du géocodage
  geocodage<-read.csv("villes_geocodes.csv",encoding="UTF-8")
  total_bis<-left_join(total_bis,geocodage,"publisher")
  
  #Périodicité
  total$is_quotidien<-as.character(total$is_quotidien)
  total$is_quotidien[total$is_quotidien=="TRUE"]<-"Quotidien"
  total$is_quotidien[total$is_quotidien=="FALSE"]<-"Autre périodicité"
  total$is_quotidien[is.na(total$is_quotidien)]<-"Autre périodicité"
  total$is_quotidien<-as.factor(total$is_quotidien)
  #####COMPTAGE DES QUOTIDIENS
  quotidiens<-total %>% 
    group_by(is_quotidien) %>%
    summarise(count = length(is_quotidien))
  colnames(quotidiens)<-c("periodicite","count")
  
  quotidiens<-quotidiens[order(quotidiens$count,decreasing = TRUE),]
  
  data=list(total,presse,theme,quotidiens,total_bis,villes)
  names(data) = c("tableau","presse","theme","quotidiens","tableau2","villes")
  return(data)}

# compteur<-read.csv("/home/benjamin/Bureau/compteur_gallicapresse.csv",encoding = "UTF-8")
# a<-as.data.frame(cbind(as.character(Sys.Date()),1))
# colnames(a)=c("date","count")
# compteur<-rbind(compteur,a)
# write.csv(compteur,"/home/benjamin/Bureau/compteur_gallicapresse.csv",fileEncoding = "UTF-8",row.names = FALSE)

ui <- navbarPage("Gallicapresse",
                 tabPanel("Graphique",fluidPage(),
                          tags$head(
                            tags$style(HTML(".shiny-output-error-validation{color: red;}"))),
                          pageWithSidebar(headerPanel(''),
                                          sidebarPanel(
                                            textInput("mot","Terme(s) à chercher","Abel Bonnard"),
                                            p('Utiliser "a+b" pour rechercher a OU b'),
                                            dateRangeInput('dateRange',
                                                           label = '\n',
                                                           start = as.Date.character("1913-01-01"), end = as.Date.character("1914-12-31"),
                                                           separator="à", startview = "century"),
                                            p(textOutput("message")),
                                            actionButton("do","Générer les graphiques"),
                                            checkboxInput("relative", "Afficher les résultats en valeurs relatives", value = FALSE),
                                            checkboxInput("mois_pub", "Supprimer les numéros sans mois de publication enregistré", value = FALSE),
                                            radioButtons("structure", "Données à analyser :",choices = list("Titre de presse" = 1, "Ville de publication" = 2,"Classement thématique de Dewey" = 3,"Périodicité" = 4),selected = 1),
                                            downloadButton('downloadData', 'Télécharger les données')
                                          ),
                                          
                                          mainPanel(
                                                    plotlyOutput("plot1"),
                                                    downloadButton('downloadPlot1', 'Télécharger le graphique interactif'),
                                                    fluidRow(textOutput("legende1"),align="right"),
                                                    fluidRow(textOutput("legende2"),align="right"),
                                                    p(""),
                                                    plotlyOutput("plot2"),
                                                    downloadButton('downloadPlot2', 'Télécharger le graphique interactif'),
                                                    fluidRow(textOutput("legende3"),align="right"),
                                                    fluidRow(textOutput("legende4"),align="right"),
                                                    p(""),
                                                    conditionalPanel(condition="input.structure==2",leafletOutput("plot7")),
                                                    conditionalPanel(condition="input.structure==2",downloadButton('downloadPlot7', 'Télécharger la carte interactive')),
                                                    conditionalPanel(condition="input.structure==2",fluidRow(textOutput("legende5"),align="right")),
                                                    conditionalPanel(condition="input.structure==2",fluidRow(textOutput("legende6"),align="right"))
                                          ))),
                 tabPanel("Notice",shiny::includeMarkdown("Notice.md")),
                 tabPanel(title=HTML("<li><a href='http://gallicagram.hopto.org:3838/gallicagram_app/' target='_blank'>Gallicagram"))
)



# Define server logic required to draw a histogram
server <- function(input, output){

  
  #Fonction d'affichage :
  display<-function(df)
  {observeEvent(input$relative,
                {observeEvent(input$structure,
                  {
                  if(input$relative & input$structure==1){
                  output$plot1 <- renderPlotly({Plot2(df,input)})
                  output$downloadPlot1 <- downloadHandler(
                    filename = function() {
                      paste('plot-', Sys.Date(), '.html', sep='')
                    },
                    content = function(con) {
                      htmlwidgets::saveWidget(as_widget(Plot2(df,input)), con)
                    })
                  
                  output$plot2 <- renderPlotly({Plot4(df,input)})
                  output$downloadPlot2 <- downloadHandler(
                    filename = function() {
                      paste('plot-', Sys.Date(), '.html', sep='')
                    },
                    content = function(con) {
                      htmlwidgets::saveWidget(as_widget(Plot4(df,input)), con)
                    })
                  }
                    else if(input$relative & input$structure==2){
                  output$plot1 <- renderPlotly({Plot6(df,input)})
                  output$downloadPlot1 <- downloadHandler(
                    filename = function() {
                      paste('plot-', Sys.Date(), '.html', sep='')
                    },
                    content = function(con) {
                      htmlwidgets::saveWidget(as_widget(Plot6(df,input)), con)
                    })
                  
                  output$plot2 <- renderPlotly({Plot8(df,input)})
                  output$downloadPlot2 <- downloadHandler(
                    filename = function() {
                      paste('plot-', Sys.Date(), '.html', sep='')
                    },
                    content = function(con) {
                      htmlwidgets::saveWidget(as_widget(Plot8(df,input)), con)
                    })
                    }
                    else if(input$relative & input$structure==3){
                  output$plot1 <- renderPlotly({Plot10(df,input)})
                  output$downloadPlot1 <- downloadHandler(
                    filename = function() {
                      paste('plot-', Sys.Date(), '.html', sep='')
                    },
                    content = function(con) {
                      htmlwidgets::saveWidget(as_widget(Plot10(df,input)), con)
                    })
                  
                  output$plot2 <- renderPlotly({Plot12(df,input)})
                  output$downloadPlot2 <- downloadHandler(
                    filename = function() {
                      paste('plot-', Sys.Date(), '.html', sep='')
                    },
                    content = function(con) {
                      htmlwidgets::saveWidget(as_widget(Plot12(df,input)), con)
                    })
                    }
                  else if(input$relative & input$structure==4){
                  output$plot1 <- renderPlotly({Plot15(df,input)})
                  output$downloadPlot1 <- downloadHandler(
                    filename = function() {
                      paste('plot-', Sys.Date(), '.html', sep='')
                    },
                    content = function(con) {
                      htmlwidgets::saveWidget(as_widget(Plot15(df,input)), con)
                    })
                  
                  output$plot2 <- renderPlotly({Plot17(df,input)})
                  output$downloadPlot2 <- downloadHandler(
                    filename = function() {
                      paste('plot-', Sys.Date(), '.html', sep='')
                    },
                    content = function(con) {
                      htmlwidgets::saveWidget(as_widget(Plot17(df,input)), con)
                    })
                }
                else if(input$relative==FALSE & input$structure==1){
                  output$plot1 <- renderPlotly({Plot1(df,input)})
                  output$downloadPlot1 <- downloadHandler(
                    filename = function() {
                      paste('plot-', Sys.Date(), '.html', sep='')
                    },
                    content = function(con) {
                      htmlwidgets::saveWidget(as_widget(Plot1(df,input)), con)
                    })
                  
                  output$plot2 <- renderPlotly({Plot3(df,input)})
                  output$downloadPlot2 <- downloadHandler(
                    filename = function() {
                      paste('plot-', Sys.Date(), '.html', sep='')
                    },
                    content = function(con) {
                      htmlwidgets::saveWidget(as_widget(Plot3(df,input)), con)
                    })
                }
                else if(input$relative==FALSE & input$structure==2){
                  output$plot1 <- renderPlotly({Plot5(df,input)})
                  output$downloadPlot1 <- downloadHandler(
                    filename = function() {
                      paste('plot-', Sys.Date(), '.html', sep='')
                    },
                    content = function(con) {
                      htmlwidgets::saveWidget(as_widget(Plot5(df,input)), con)
                    })
                  
                  output$plot2 <- renderPlotly({Plot7(df,input)})
                  output$downloadPlot2 <- downloadHandler(
                    filename = function() {
                      paste('plot-', Sys.Date(), '.html', sep='')
                    },
                    content = function(con) {
                      htmlwidgets::saveWidget(as_widget(Plot7(df,input)), con)
                    })
                  output$plot7 <- renderLeaflet({Plot13(df,input)})
                  output$downloadPlot7 <- downloadHandler(
                    filename = function() {
                      paste('plot-', Sys.Date(), '.html', sep='')
                    },
                    content = function(con) {
                      htmlwidgets::saveWidget(as_widget(Plot13(df,input)), con)
                    })
                }
                else if(input$relative==FALSE & input$structure==3){ 
                  output$plot1 <- renderPlotly({Plot9(df,input)})
                  output$downloadPlot1 <- downloadHandler(
                    filename = function() {
                      paste('plot-', Sys.Date(), '.html', sep='')
                    },
                    content = function(con) {
                      htmlwidgets::saveWidget(as_widget(Plot9(df,input)), con)
                    })
                  
                  output$plot2 <- renderPlotly({Plot11(df,input)})
                  output$downloadPlot2 <- downloadHandler(
                    filename = function() {
                      paste('plot-', Sys.Date(), '.html', sep='')
                    },
                    content = function(con) {
                      htmlwidgets::saveWidget(as_widget(Plot11(df,input)), con)
                    })
                }
                else if(input$relative==FALSE & input$structure==4){
                  output$plot1 <- renderPlotly({Plot14(df,input)})
                  output$downloadPlot1 <- downloadHandler(
                    filename = function() {
                      paste('plot-', Sys.Date(), '.html', sep='')
                    },
                    content = function(con) {
                      htmlwidgets::saveWidget(as_widget(Plot14(df,input)), con)
                    })
                  
                  output$plot2 <- renderPlotly({Plot16(df,input)})
                  output$downloadPlot2 <- downloadHandler(
                    filename = function() {
                      paste('plot-', Sys.Date(), '.html', sep='')
                    },
                    content = function(con) {
                      htmlwidgets::saveWidget(as_widget(Plot16(df,input)), con)
                    })
                }
                
                
                })
  })}
  
  #Affichage au démarrage :
  tot_df<-read.csv("exemple.csv",encoding = "UTF-8")
  observeEvent(input$mois_pub,{
    df_exemple = reactive({get_data(tot_df,input$mot,input$dateRange,input$mois_pub)})
    display(df_exemple())
  })
  output$legende1=renderText("Source : gallica.bnf.fr")
  output$legende2=renderText("Affichage : Gallicapresse par Benjamin Azoulay et Benoît de Courson")
  output$legende3=renderText("Source : gallica.bnf.fr")
  output$legende4=renderText("Affichage : Gallicapresse par Benjamin Azoulay et Benoît de Courson")
  output$legende5=renderText("Source : gallica.bnf.fr")
  output$legende6=renderText("Affichage : Gallicapresse par Benjamin Azoulay et Benoît de Courson")
  
  recherche<-reactive({input$mot})
  duree<-reactive({input$dateRange})
  output$message<-renderText(temps_traitement(recherche(),duree()))
  
  observeEvent(input$do,{
    datasetInput <- reactive({
      data$tableau})
    tot_df=prepare_data(input$mot,input$dateRange)
    observeEvent(input$mois_pub,{
    df = get_data(tot_df,input$mot,input$dateRange,input$mois_pub)
    display(df)
    
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste('data-', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        write.csv(df$tableau, con, fileEncoding = "UTF-8",row.names = F)
      })
  })})
  
  
  
}

shinyApp(ui = ui, server = server)
