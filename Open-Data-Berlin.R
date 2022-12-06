####### Wichtige Informationen ####### 
# Das Folgende Skript behandelt die Prüfungsaufgabe des Kurses Data Science
# an der DHBW Stuttgart im Sutdiengang B. Sc. Informatik.
#
# Das Skript ist wie folgt aufegeilt:
# - zu Beginn werden benötigte Bibliotheken geladen
# - Danach werden die Daten geladen und bereinigt, aggregiert bzw. Duplikate entfernt und um Informationen ergänzt
# - Zum Schluss werden die einzelnen Aufgaben bearbeitet und im Lösungsabschnitt beantwortet
# 
# Hinweis: Aufgabe c) wird nicht in diesem Skript beantwortet. Hierfür wird ein extra
# Shiny skript gestartet (TODO: SKRIPT). Die Shiny App benutzt aber die hier definierten
# Funktionen zum bereinigen und aggregieren der Daten.


####### Start des Skripts####### 
rm(list = ls())
setwd("C:/Users/tomfr/OneDrive/Studium/UNI/5. Semester/Data Sience/Prüfung")
library(tidyverse)
library(ggplot2)
library(tidyr)
library(reshape2)

# Read the data from csv with the ";" as separator
loadData <- function(){
  data <- read.csv("./open-data_berlin_Nutzerdaten.csv", sep = ";")
  # str(data)
  # View(data)
  return(data)
}

# After looking into the data:
# 1. The Names of the pages with ä,ö,ü,ß are escaped --> First step is to remove the escape
# 2. Every Month has two columns: pi (Page impressions), v (Page Visits)
# 3. Many NA's

####### REMOVE ESCAPED LETTERS AND CORRUPT DATA START #######
substituteData <- function(data) {
  
  # Next: Start with 1. step and remove escaped letters
  # - √º = ü
  # - √∂ = ö
  # - √§ = ä
  # - √ü = ß
  
  # ü <- data[grep('√º', data$page), ]
  # ö <- data[grep('√∂', data$page), ]
  # ä <- data[grep('√§', data$page), ]
  ü <- grep('√º', data$page)
  ö <- grep('√∂', data$page)
  ä <- grep('√§', data$page)
  
  # Search for 'Straße' --> √ü = ß
  # View(data[grep('stra', data$page), ])
  ß <- grep('√ü', data$page)
  
  # Thesis: Every escaped char starts with '√'. Search for not found escaped chars:
  # escaped <- data[grep('√', data$page)]
  escaped <- grep('√', data$page)
  others <- setdiff(setdiff(setdiff(setdiff(escaped, ß), ü), ö), ä)
  # View(data[others, ])
  # There exist more:
  # √É≈∏ = ß
  # √É∆í√Ü‚Äô√É‚Ä¶√Ç¬∏ = ß
  
  # Ö:
  # √É¬∂ = ö
  # √É∆í√Ü‚Äô√É‚Ä†√¢‚Ç¨‚Ñ¢√É∆í√¢‚Ç¨ √É¬¢√¢‚Äö¬¨√¢‚Äû¬¢√É∆í√Ü‚Äô√É¬¢√¢‚Äö¬¨√Ö¬°√É∆í√¢‚Ç¨≈°√É‚Äö√Ç¬∂ = ö
  # √É∆í√Ü‚Äô√É‚Äö√Ç¬∂ = ö
  
  # Ü:
  # √É¬º = ü
  # √É∆í√Ü‚Äô√É‚Ä†√¢‚Ç¨‚Ñ¢√É∆í√¢‚Ç¨ √É¬¢√¢‚Äö¬¨√¢‚Äû¬¢√É∆í√Ü‚Äô√É¬¢√¢‚Äö¬¨√Ö¬°√É∆í√¢‚Ç¨≈°√É‚Äö√Ç¬º = ü
  # √É∆í√Ü‚Äô√É‚Ä†√¢‚Ç¨‚Ñ¢√É∆í√¢‚Ç¨≈°√É‚Äö√Ç¬º = ü
  # √É∆í√Ü‚Äô√É‚Äö√Ç¬º = ü
  # √É¬É¬º = ü
  
  # Ä:
  # √É¬§A4 = ä
  # √É∆í√Ü‚Äô√É‚Äö√Ç¬§ = ä
  # √É¬§ = ä
  # √É∆í√Ç¬§ = ä
  
  others <- setdiff(others, grep('√É≈∏', data$page))
  others <- setdiff(others, grep('√É∆í√Ü‚Äô√É‚Ä†√¢‚Ç¨‚Ñ¢√É∆í√¢‚Ç¨ √É¬¢√¢‚Äö¬¨√¢‚Äû¬¢√É∆í√Ü‚Äô√É¬¢√¢‚Äö¬¨√Ö¬°√É∆í√¢‚Ç¨≈°√É‚Äö√Ç¬', data$page))
  others <- setdiff(others, grep('√É¬∂', data$page))
  others <- setdiff(others, grep('√É∆í√Ü‚Äô√É‚Äö√Ç¬∂', data$page))
  others <- setdiff(others, grep('√É¬º', data$page))
  others <- setdiff(others, grep('√É∆í√Ü‚Äô√É‚Äö√Ç¬º', data$page))
  others <- setdiff(others, grep('√É¬§A4', data$page))
  others <- setdiff(others, grep('√É∆í√Ü‚Äô√É‚Äö√Ç¬§', data$page))
  others <- setdiff(others, grep('√É¬§', data$page))
  others <- setdiff(others, grep('√É∆í√Ç¬§', data$page))
  others <- setdiff(others, grep('√É∆í√Ü‚Äô√É‚Ä†√¢‚Ç¨‚Ñ¢√É∆í√¢‚Ç¨≈°√É‚Äö√Ç¬º', data$page))
  others <- setdiff(others, grep('√É¬É¬º', data$page))
  
  # Thesis is a pattern for the most escaped letters:
  # - √(something)º = ü Problem:
  #                     √É∆í√Ü‚Äô√É‚Ä†√¢‚Ç¨‚Ñ¢√É∆í√¢‚Ç¨≈°√É‚Äö√Ç¬º because 2 times °
  #                     √É∆í√Ü‚Äô√É‚Ä†√¢‚Ç¨‚Ñ¢√É∆í√¢‚Ç¨ √É¬¢√¢‚Äö¬¨√¢‚Äû¬¢√É∆í√Ü‚Äô√É¬¢√¢‚Äö¬¨√Ö¬°√É∆í√¢‚Ç¨≈°√É‚Äö√Ç¬º
  #                     Solution: ° is different to º and º only one time at the end!
  # - %C3%BC = ü
  # - √(something)∂ = ö Problem: none
  # - %C3%B6 = ö
  # - √(something)§ = ä Problem: √É¬§A4 and because one more letter
  # - %C3%A4 = ä
  # - A4 = ä
  # - "¬® " = ä
  # - √(something)ü or √(something)∏ = ß Problem: should be parsed before ü because it uses ü
  # - %C3%9F = ß
  #
  # Replace letters:
  temp <- data[others, "page"]
  data[ß, "page"]
  data_sub <- data
  data_sub$page <- gsub("(√[^A-Za-z]*?[ü∏])|%C3%9F", "ß", data_sub$page) # ß
  data_sub$page <- gsub("(√[^A-Za-z]*?º)|%C3%BC", "ü", data_sub$page) # ü
  data_sub$page <- gsub("(√[^A-Za-z]*?∂)|%C3%B6", "ö", data_sub$page) # ö
  data_sub$page <- gsub("(√[^A-Za-z]*?§(A4)?)|(%C3%)?(A4)|(¬® a)|(&auml;)|(ÔøΩ)|(aÃà)|( Ãà)", "ä", data_sub$page) # ä
  data_sub$page <- gsub("¬Æ", "®", data_sub$page) # ®
  data_sub$page <- gsub("¬ß", "§", data_sub$page) # §
  data_sub$page <- gsub("eurm¬≤", "EUR/m²", data_sub$page) # €
  
  # No sense -> delete chars
  data_sub$page <- gsub("-?‚Äì-?", "-", data_sub$page)
  data_sub$page <- gsub(")[.]?", "", data_sub$page)
  data_sub$page <- gsub(",|(¬¨)|!|&| Óåì", "", data_sub$page)
  data_sub$page <- gsub("‚Äã|‚Äû|‚Äú", "", data_sub$page)
  
  # The following code was used for finding strange signs
  # invalid <- gsub("[a-z\\._äöüß®§€]|-","",data_sub$page, ignore.case = T)
  # data_sub$page[invalid != ""]
  # invalid[invalid != ""]
  # data_sub$page[invalid %in% ""]
  
  # correct malicious data
  data_sub$page[data_sub$page == "koordinaten-der-zugangsmöglichkeiten-zu-stationen-0 Orignalquelle: daten.berlin.de Urheber der Daten auch VBB aber Version Mai 2018 Zuletzt aktualisiert 18. September 2018 10:58"] <- "koordinaten-der-zugangsmöglichkeiten-zu-stationen-0"
  data_sub$page[data_sub$page == "einwohnerinnen-texttt{und-einwohner-den-ortsteilen-berlins-am-30062016"] <- "einwohnerinnen-text-und-einwohner-den-ortsteilen-berlins-am-30062016"
  data_sub$page[data_sub$page == "vbb-fahrplandaten-gtfsxid=17259157000231570018615700190157002561570025915700262usg=ALkJrhgrmAYC4EL0Dhh2k3oF-zprTxEuCA"] <- "vbb-fahrplandaten-gtfs"
  data_sub$page[data_sub$page == "volkswirtschaftliche-gesamtrechnungen-berlin-und-brandenburg abgerufen"] <- "volkswirtschaftliche-gesamtrechnungen-berlin-und-brandenburg"
  data_sub$page[data_sub$page == "vbb-fahrplandaten-gtfs laden Sie die GTFS.zip"] <- "vbb-fahrplandaten-gtfs"
  data_sub$page[data_sub$page == "vbb-fahrplandaten-gtfs (z.B."] <- "vbb-fahrplandaten-gtfs"
  data_sub$page[data_sub$page == "träger-von-kindertagesstätten-marzahn-hellersdorfalice saloman schule"] <- "träger-von-kindertagesstätten-marzahn-hellersdorf-alice-saloman-schule"
  data_sub$page[data_sub$page == "straßenverzeichnisberliner strasen"] <- "straßenverzeichnis-berliner-straßen"
  data_sub$page[data_sub$page == "statistische-einheiten-im-inspire-datenmodell-rbs- blöcke-atom-1"] <- "statistische-einheiten-im-inspire-datenmodell-rbs-blöcke-atom-1"
  data_sub$page[data_sub$page == "liste-der-häufigen-vornamen-2017rashad idris berlin"] <- "liste-der-häufigen-vornamen-2017"
  data_sub$page[data_sub$page == "liste-der-badestellenbrutto netto rechner"] <- "liste-der-badestellen-brutto-netto-rechner"
  data_sub$page[data_sub$page == "koordinaten-der-zugangsmöglichkeiten-zu-stationen-0 von Berlin Open Data (daten.berlin.de Urheber der Daten VBB (API CSV-Datei (Originalbezeichnung: UMBW.csvOriginal Datensatz beinhaltet 20046 Dateneinträge Koordinaten der"] <- "koordinaten-der-zugangsmöglichkeiten-zu-stationen-0"
  data_sub$page[data_sub$page == "koordinaten-der- zugangsmöglichkeiten-zu-stationen-0"] <- "koordinaten-der-zugangsmöglichkeiten-zu-stationen-0"
  data_sub$page[data_sub$page == "kitas-berlin-steglitz zehlendorf"] <- "kitas-berlin-steglitz-zehlendorf"
  data_sub$page[data_sub$page == "grünanlagenbestand-berlin-einschließlich-der-öffentlichen-spielplätze- wms-2"] <- "grünanlagenbestand-berlin-einschließlich-der-öffentlichen-spielplätze-wms-2"
  data_sub$page[data_sub$page == "grünanlagenbestand-berlin-einschl-der-öffentlichen-spielplätze-spielplätze-wfs-4usg=ALkJrhhpBeHpvpm-qzrnGTkp0z1qTwQldQ"] <- "grünanlagenbestand-berlin-einschl-der-öffentlichen-spielplätze-spielplätze-wfs-4"
  data_sub$page[data_sub$page == "geometrien-der-wahlbezirke-für-die-wahl-zum- abgeordnetenhaus-von-berlin-2021"] <- "geometrien-der-wahlbezirke-für-die-wahl-zum-abgeordnetenhaus-von-berlin-2021"
  data_sub$page[data_sub$page == "gebiete§ 9 AG BauGB-von-außergewöhnlicher-stadtpolitischer-bedeutung-nach-§-9-ag-baugb-wms"] <- "gebiete-von-außergewöhnlicher-stadtpolitischer-bedeutung-nach-§-9-ag-baugb-wms"
  data_sub$page[data_sub$page == "erhaltungsverordnungsgebiete-erhaltung-der-zusammensetzung -der-wohnbevölkerung-wfs"] <- "erhaltungsverordnungsgebiete-erhaltung-der-zusammensetzung-der-wohnbevölkerung-wfs"
  data_sub$page[data_sub$page == "einwohnerinnen-und-einwohner-berlin-lor-planungsräumen-am-31122018 bereit"] <- "einwohnerinnen-und-einwohner-berlin-lor-planungsräumen-am-31122018"
  data_sub$page[data_sub$page == "einwohnerinnen-und-einwoh- ner-mit-migrationshintergrund-berlin-lor-planungsräumen-nach-6."] <- "einwohnerinnen-und-einwohner-mit-migrationshintergrund-berlin-lor-planungsräumen"
  data_sub$page[data_sub$page == "einschulbereiche-geometrien-schuljahr-20192020 ."] <- "einschulbereiche-geometrien-schuljahr-20192020"
  data_sub$page[data_sub$page == "digitale-farbige-orthophotos-2018-dop20rgbWeb Map Service Digitale Orthophotos - Bodenauflösung 20 cm on sg.geodatenzentrum.de"] <- "digitale-farbige-orthophotos-2018-dop20rgb"
  data_sub$page[data_sub$page == "brandenburger-straßen-und-vo lksfeste-2020"] <- "brandenburger-straßen-und-volksfeste-2020"
  data_sub$page[data_sub$page == "bodenrichtwert 12305 Berlin"] <- "bodenrichtwert-12305-berlin"
  data_sub$page[data_sub$page == "bücherschränke-im bezirk-lichtenberg"] <- "bücherschränke-im-bezirk-lichtenberg"
  data_sub$page[data_sub$page == "¬üenhandel"] <- "aus-und-einfuhr-außenhandel"
  data_sub$page <- gsub("-ua-", "-umweltatlas-", data_sub$page)
  data_sub$page <- gsub("-v-2$", "-v2", data_sub$page) # Make different versions equal
  data_sub$page <- gsub("-[0-9]{1,2}$", "", data_sub$page) # Filter different page pages
  data_sub$page <- gsub("[a-f]+[0-9]+[0-9a-f]*$", "", data_sub$page) # No hex data
  data_sub$page <- gsub("-0$", "", data_sub$page) # Filter Zeros
  data_sub <- data_sub[-which(data_sub$page == 'a'), ] # No sense
  data_sub <- data_sub[-which(data_sub$page == '1c35e89f-5725-4d46-bc5a-229'), ] # No sense
  
  return(data_sub)
}
####### REMOVE ESCAPED LETTERS AND CORRUPT DATA END ####### 




####### AGGREGATE DATA START #######
aggregateData <- function(data_sub) {
  # Sum the amount of visits and replace the 0's with NA
  data_agg <- aggregate(x = data_sub[ , colnames(data_sub) != "page"], # Mean by group
            by = list(data_sub$page),
            FUN = sum,
            na.rm = TRUE
            )
  colnames(data_agg)[colnames(data_agg) == 'Group.1'] <- 'page'
  data_agg[data_agg == 0] <- NA
  
  return(data_agg)
}
####### AGGREGATE DATA END #######






####### PREPARE SUMS START#######
enrichData <- function(data_agg) {
  data_enr <- data_agg
  
  # Add the sum of visits | sum of impressions
  month_cols_pi <- names(data_enr)[grep("X[0-9]*[.][0-9][0-9][.]pi", names(data_enr))]
  month_cols_v <- names(data_enr)[grep("X[0-9]*[.][0-9][0-9][.]v", names(data_enr))]
  
  # Calculate the sums
  sum_pi <- apply(data_enr[,month_cols_pi], c(1), function(x) {sum(x, na.rm = T)})
  sum_v <- apply(data_enr[,month_cols_v], c(1), function(x) {sum(x, na.rm = T)})
  sum <- sum_pi + sum_v
  data_enr['sum_pi'] <- sum_pi
  data_enr['sum_v'] <- sum_v
  data_enr['sum'] <- sum
  
  return(data_enr)
}
####### PREPARE SUMS END ####### 





####### TASK A) 10 most used pages ALTRNATIVE START#######
# Function for chart
fun_bar_chart <- function(data_enr_temp, number_to_display, decreasing = T, orderBy = "sum_pi") {
  # Order by sum of visits and page impressions
  data_enr_temp['sum_pi_v'] <- data_enr_temp$sum_pi - data_enr_temp$sum_v
  data_enr_temp <- data_enr_temp[order(data_enr_temp[orderBy], decreasing = decreasing), ]
  
  # Get the 10 highest/lowest per class
  top10 <- data_enr_temp[which(data_enr_temp$page %in% head(data_enr_temp, number_to_display)$page),]
  top10 <- top10[order(top10[orderBy], decreasing = T), ]
  # Make two fields for every variable of each page
  melted <- melt(top10[,c("page", "sum_pi", "sum_v")], id="page")
  
  # Make a factor to order the data
  melted$page <- factor(melted$page, levels = unique(melted$page),ordered = T)

  # Define Title
  title <- ""
  if(decreasing) {
    title <- paste("Die", number_to_display ,"meist genutzten Seiten nach Page Imrpessions", sep = " ")
  } else {
    title <- paste("Die", number_to_display ,"am wenigsten genutzten Seiten nach Page Imrpessions", sep = " ")
  }
  
  # Make plot
  plot <- ggplot(melted, aes(value, page, label=value)) +   
    geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
    geom_text( size = 4, hjust=1.1, position=position_dodge2(0.9), color="white") +
    ggtitle(title) +
    ylab("Seite") + xlab("Summe") +
    labs(fill='') +
    scale_fill_manual(values=c("#824f8c", "#e64823"), labels=c('Impressions', 'Visits'))+
    theme_linedraw()+
    theme(
      legend.position = "top",
      text = element_text(size = 14), 
      plot.title = element_text(hjust = 0.5, size=18),
      axis.title=element_text(size=14,face="bold"),
      axis.text = element_text(size = 14)
    )
  return(plot)
}

task_A <- function(data_enr){
  p1 <- fun_bar_chart(data_enr, 10, T, "sum_pi")
  return(p1)
}
####### TASK A) 10 most used pages END#######




####### TASK B) 10 least used pages START#######
# Use the function of a)
task_B <- function (data_enr) {
  # Was the first idea but not used in the final solution
  # p2 <- fun_bar_chart(data_enr, 10, F, "sum_pi")
  
  num_of_zero_visits <- sum(data_enr$sum_v <= 0)
  
  # Histogram for the page impressions < 100
  histogram <- ggplot(data_enr[data_enr$sum_pi < 100, ]) +
    geom_histogram(aes(sum_pi), fill = "#e64823", color="black", binwidth=1) +
    ylab("Anzahl an Seiten") + xlab("Summe der Page Impressions") +
    ggtitle("Histogram über die Summe der Page Impressions kleiner 100") +
    labs(fill='') +
    theme_linedraw()+
    theme(
      legend.position = "top",
      text = element_text(size = 14), 
      plot.title = element_text(hjust = 0.5, size=18),
      axis.title=element_text(size=14,face="bold"),
      axis.text = element_text(size = 14)+
      theme(
        text = element_text(size = 14), 
        plot.title = element_text(hjust = 0.5, size=18),
        axis.title=element_text(size=14,face="bold"),
        axis.text = element_text(size = 14)
      )
    )
  
  # List of all pages with less than 10 page impressions
  less_than_10_impressions <- data_enr[data_enr$sum_pi < 10, ]
  
  # Example page for the argumentation
  example_page <- less_than_10_impressions[less_than_10_impressions$page %in% c("anzahl-arbeitsloser-frauen-berlin-1995-2010", "arbeitslose-veränderung-2013-2014-wms"), c("page", "sum_v", "sum_pi")]
  
  # List of elements with one impression
  list_of_elements <- data_enr[data_enr$sum_pi == 1, 'page']
  
  return(list("list_of_elements"=list_of_elements, "num_of_zero_visits"=num_of_zero_visits, "histogram"=histogram, "less_than_10_impressions" = less_than_10_impressions, "example_page" = example_page))
}
####### TASK B) 10 least used pages END#######




####### TASK D)  impressions ~ visits#######
task_D <-function(data_enr) {
  # Test if there are in one data set more impressions than visits
  pi_smaller_v = sum(data_enr$sum_pi < data_enr$sum_v)
  
  # Test how many pages have the same amount of impressions and visits
  pi_equal_v = sum(data_enr$sum_pi == data_enr$sum_v)
  
  # Look for a relation ship
  p3 <- ggplot(data_enr, aes(log(sum_v), log(sum_pi))) +
    geom_jitter(alpha=0.05) +
    geom_smooth(method = 'loess', formula = "y ~ x") +
    labs(title = "Verhältnis zwischen Page Impressions und Visits") + 
    xlab("log(Summe von Page Visits)") + ylab("log(Summe von Page Impressions)")
  return(list("plot" = p3, "pi_smaller_v"=pi_smaller_v, "pi_equal_v"=pi_equal_v))
}
####### TASK D) visits ~ impressions#######




####### TASK E)  Started pages#######
# From TASK D) we know that every page with at least one impression have one visit.
# So we can filter through the first date
task_E <- function(data_enr){
  start_pages <- data_enr[!(is.na(data_enr$X2019.02.v)), "page"]
  return (start_pages)
}
# Answer: The in start_pages listed pages are start pages.
####### TASK E) visits ~ impressions#######


###### SOLUTIONS #####
# Load all data
data_enr <- loadData() %>% substituteData() %>% aggregateData() %>% enrichData()

# a) Die 10 meist benutzen Datensätze: 
solution_A <- task_A(data_enr); solution_A
# Die Grafik zeigt die
# 10 meist genutzten Seiten/Dienste von Open Data Berlin. Dabei sind
# die Page Visits und die Anzahl der Page Impressions dargestellt.
# 
# Unter einer Page Impression versteht man die Aufrufe einer Seite.
# Jedes nicht automatische neuladen der Seite wird dabei getrackt.
# Dazu zählt das erste mal öffnen, erneute laden, oder öffnen nach
# einem lägneren Zeitraum [1-3]. Die Page Visits geben zwar auch
# Seitenaufrufe an aber nur der erste in einer Session. Wobei eine
# Session nach einer Inaktivität von 30 Minuten aufhört [1-3]. Somit
# ist ein Visit der erste Besuch einer Seite. Wenn der Nutzer
# innerhalb von der Session (endet nach 30 Minuten inaktivität) die
# Seite erneut öffnet bzw. lädt werden keine visits erzeugt sondern
# nur Impressions. Schließlich lässt sich sagen, dass ein Page Visit
# auch immer eine Page Impression auslöst [1-3]. 
#
# Aufgrund dieser Definition von Page Imrpessions und Visits wurden in
# Plot 1 (p1) die Seiten nach ihrer Summe von page impressions
# geordnet. Weil Seiten mit mehr page impressions wurden häufiger
# geöffnet und somit potentiell mehr benutzt.
#
# Außerdem wurden für die Grafik kaputte Datensätze entfernt und
# escapete Buchstaben ersetzt. Abgesehen davon wurden Seitennamen die
# mit einer Forlaufenden Nummer Enden als eine Seite Betrachtet, da
# nach einer Recherche keine fortlaufenden nummern in Seitennamen bei
# Open Data Berlin gefunden wurden. Es ist also von einem Skriptfehler
# auszugehen. Hingegen Namen die mit einem Datum oder einer Jahreszahl
# enden wurden beibehalten.
#
# Referenzen Webtrekk, page visits und impression: 
# [1] https://documentation.mapp.com/1.0/en/basic-metrics-page-impressions-visits-visitors-7211156.html (letzer Aufruf: 05.12.2022)
# [2] https://engel-zimmermann.de/blog/visits-views-und-page-impressions-eine-kleine-fuehrung-durch-den-zahlendschungel/ (letzer Aufruf: 05.12.2022)
# [3] https://www.beyond-media.de/blog/artikel/page-impressions-definition-und-erklaerung-der-kennzahl/  (letzer Aufruf: 05.12.2022)



# Solution of b) Auskunft über die 10 am wenigsten benutzten Dienste
solution_B <- task_B(data_enr)

# Als ersten Gedanken über die (zehn) am wenigsten benutzten Dienste denkt man,
# dass verschiedene Dienste 0 visits und damit 0 impressions haben müssen. Doch
# der Datensatz enthält keinen Dienst der nicht besucht oder aufgerufen wurde.
# Volgende Abfrage bestätigt das:
solution_B["num_of_zero_visits"]
# Das kann darauf zurückzuführen sein, dass das Skript einen Dienst zur Liste 
# nur dann hinzufügt, wenn er aufgerufen wird. Es ist folglich schwierig
# auf Basis der vorhandenen Daten die 10 wenigsten Datensätze zu finden, weil
# man nicht davon ausgehen kann, dass man alle vorhandenen Dienste hat.
#
# Ein weiterer Punkt ist, dass viele Daten existieren, die nur sehr wenige Page
# Impressions haben. Das folgende Histogram zeigt, die Anzahl an Seiten für die
# Summe der Page Impressions kleiner als 100.
solution_B["histogram"]
# Man sieht, dass  sich im Bereich von 0 bis 10 Page Impressions sehr viele Seiten
# sammeln. Es existieren knapp über 600 Datensätze, die nur eine  und 
# ca. 350, die zwei Page Impression haben. Das ist fast ein drittel der bereinigten
# und danach aggregierten Daten. Weil dieser Datensatz nicht die komplette Zeit
# von Open Data Berlin abdeckt und erst im Januar 2019 anfängt, könnten diese Seiten
# schon früher existiert haben und öfters aufgerufen worden sein. Zum Beispiel die
# Seite "arbeitslose-veränderung-2013-2014-wms" hat 2 Page Impressions und Visits und
# wurde am 31.12.2015 veröffentlicht und auch das letzte mal aktualisiert [4].
# Andere Datensätze, wie zum Beispiel "anzahl-arbeitsloser-frauen-berlin-1995-2010"
# existieren hingegen heutzutage garnicht mehr (bzw. haben vielleicht nie existiert)
# und können deswegen nicht (mehr) aufgerufen werden [5].
solution_B["example_page"]

# Zusammenfasseng kann gesagt werden, dass es im Datensatz Seiten gibt, die
# eine Page Visit und Page Impression habe (siehe nächste Ausgabe).
solution_B["list_of_elements"]
# Sie sind im Datensatz die am wenigst benutzten Seiten. Aber müssen nicht der 
# realität entsprechen, weil das Tracking Tool gewechselt wurde und nicht alle
# Seiten enthalten sind.
# 
# Referenzen zu den Datensätzen:
# [4] https://daten.berlin.de/datensaetze/arbeitslose-ver%C3%A4nderung-2013-2014-wms  (letzer Aufruf: 06.12.2022)
# [5] https://daten.berlin.de/search/node/anzahl%20arbeitsloser%20frauen%20berlin   (letzer Aufruf: 06.12.2022)




# Solution of c)
# Please start the shiny app




# Solution of d) Verhältnis von Page Impressions zu Page Visits
solution_D <- task_D(data_enr)

# Das Verhältnis von Page Impressions zu Page Visits lässt sich wie folgt beschreiben.
# Aus Aufgabe a) kann entnommen werden, dass beim tracken eines Page Visit eine Page Impression 
# mit getrackt wird aber nicht umgekehrt. Somit muss es mindestens so viele Page Impressions, wie
# Page Visits geben. Folgende Abfrage sucht im Datensatz nach einer Seite mit mehr
# Visits als Impressions. Das Ergebnis ist 0.
solution_D["pi_smaller_v"]
# Die nächste Ausgabe betrachtet, wie viele Seiten gleich viel Page Impressions
# wie Visits haben:
solution_D["pi_equal_v"]
nrow(data_enr)
# Daraus folgt, dass knapp die Hälfte der Seiten genau gleich viele Page Impressions und
# Visits hat. Das liegt vermutlich mit unter daran, dass knapp ein drittel der Daten
# genau einen Page Visitt und eine Page Impression haben. Man könnte also vermuten,
# dass mit mehr Visits mehr Page Impressions kommen so, dass es mehr Impressions als 
# Visits gibt. Der nächste Plot gibt das Verhältnis von Page Impressions zu Visits aus.
solution_D["plot"]
# Daraus folgt, dass durch den Zusammenhang, dass Visits auch eine Impression triggern, die 
# Page Impressions proportional zu den Visits wachsen.


# Solution of e) Mit welchen Daten wurde begonnen?
task_E(data_enr)
# TODO: Write Answer
# start_pages
###### SOLUTIONS END #####
