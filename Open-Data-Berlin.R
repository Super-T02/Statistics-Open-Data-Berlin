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
  # View(data[others,])
  
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
  
  # Used for finding strange signs
  # invalid <- gsub("[a-z\\._äöüß®§€]|-","",data_sub$page, ignore.case = T)
  # data_sub$page[invalid != ""]
  # invalid[invalid != ""]
  # data_sub$page[invalid %in% ""]
  
  # View(data_sub)
  # Unsolved Problem: ¬ is sometimes in the data but don't know what it does
  
  # correct malicious data
  # data_sub$page[data_sub$page == ""] <- ""
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
  
  # Equalize Services
  # data_sub$page <- gsub("-wms(-[0-9]*)?", "", data_sub$page)
  # data_sub$page <- gsub("-atom(-[0-9]*)?", "", data_sub$page)
  # data_sub$page <- gsub("-wfs(-[0-9]*)?", "", data_sub$page)
  # data_sub$page <- gsub("-gtfs(-[0-9]*)?", "", data_sub$page)
  # data_sub$page <- gsub("[.]rdf", "", data_sub$page)
  # data_sub$page <- gsub("[.]xml", "", data_sub$page)
  data_sub$page <- gsub("-v-2$", "-v2", data_sub$page) # Make different versions equal
  data_sub$page <- gsub("-[0-9]{1,2}$", "", data_sub$page) # Filter different page pages
  data_sub$page <- gsub("[a-f]+[0-9]+[0-9a-f]*$", "", data_sub$page) # No hex data
  data_sub$page <- gsub("-0$", "", data_sub$page) # Filter Zeros
  
  # Remove corrupt data
  data_sub <- data_sub[-which(data_sub$page == 'a'), ]
  data_sub <- data_sub[-which(data_sub$page == '1c35e89f-5725-4d46-bc5a-229'), ]
  
  return(data_sub)
}
####### REMOVE ESCAPED LETTERS AND CORRUPT DATA END ####### 




####### FIND DUPLICATES START #######
aggregateData <- function(data_sub) {
  # Find the duplicates
  data_agg <- data_sub[(duplicated(data_sub$page) | duplicated(data_sub$page, fromLast = T)),]
  # Sum the amount of visits and replace the 0's with NA
  data_agg <- aggregate(x = data_agg[ , colnames(data_agg) != "page"],             # Mean by group
            by = list(data_agg$page),
            FUN = sum,
            na.rm = TRUE
            )
  colnames(data_agg)[colnames(data_agg) == 'Group.1'] <- 'page'
  data_agg[data_agg == 0] <- NA
  
  return(data_agg)
}
####### FIND DUPLICATES END #######






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
  # View(data_enr[,c('page','sum_v', 'sum_pi')])
  
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
  
  # melted <- melted[order(-melted$variable, -melted$value, decreasing = F), ]
  
  # Make a factor to order the data
  melted$page <- factor(melted$page, levels = unique(melted$page),ordered = T)
  # View(melted)
  # Define Title
  title <- ""
  if(decreasing) {
    title <- paste("Most", number_to_display ,"used pages by page impressions", sep = " ")
  } else {
    title <- paste("Least", number_to_display ,"used pages by page impressions", sep = " ")
  }
  
  # Make plot
  plot <- ggplot(melted, aes(value, page, label=value)) +   
    geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
    geom_text( size = 4, hjust=1.1, position=position_dodge2(0.9)) +
    ggtitle(title) +
    ylab("Sum") + xlab("Page") +
    scale_fill_discrete(labels=c('Impressions', 'Visits')) +
    labs(fill='') +
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
  p2 <- fun_bar_chart(data_enr, 10, F, "sum_pi")
  return(p2)
}

# TODO: Write some queries for searching for pages with zero visits
# TODO: Write some queries for searching how many pages have one visit

# View(melted)
####### TASK B) 10 least used pages END#######




####### TASK D)  impressions ~ visits#######
task_D <-function(data_enr) {
  # Test if there are in one data set more impressions than visits
  sum(data_enr$sum_pi < data_enr$sum_v) # 0
  
  # Test how many pages have the same amount of impressions and visits
  sum(data_enr$sum_pi == data_enr$sum_v) # 2147
  
  # Look for a relation ship
  p3 <- ggplot(data_enr[,], aes(sum_v, sum_pi)) +
    geom_point() +
    geom_smooth(method = 'loess', formula = "y ~ x") +
    labs(title = "Relationship between impressions and visits") + 
    xlab("Sum of visits by page") + ylab("Sum of page impressions by page")
  
  # Answer: There are always more or equal impressions than visits. That means
  # every visit is a impression, but a visit can have multiple impressions. Looking
  # at the relationship, we can see that more visits result in more impressions.
  
  return(p3)
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
# a) Die 10 meist benutzen Datensätze: (Plot: p1) Die Grafik zeigt die
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

# Solution of b)
# p2
# TODO: Write Answer
# Answer: 

# Solution of c)
# Please start the shiny app

# Solution of d)
# TODO: New plot for the other shiny app and write Answer
# p3 # and Answer: There are always more or equal impressions than visits. That means
# every visit is a impression, but a visit can have multiple impressions. Looking
# at the relationship, we can see that more visits result in more impressions.

# Solution of e)
# TODO: Write Answer
# start_pages


###### SOLUTIONS END #####
