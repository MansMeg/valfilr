# Functions to get data from XML
library(XML)
library(plyr)

# Functions
NullToNone <-function(x){
  ifelse(is.null(x),"",x)
}


GetPartier <- function(val_xml){
  # Funktion som genererar ett dataset med partibeteckningar
  # fr?n en valfil fr?n valmyndighetens XML-filer
  # Libraries:
  library(XML)
  
  # Test av att det ?r ett XML-objekt
  if(class(val_xml)[1] != "XMLInternalDocument"){print("Ej ett XMLNode-objekt!");break}

  # XPaths?kning
  nodes = xpathApply(val_xml, "/VAL/PARTI")
  
  # Skapa dataset
  partiData = data.frame("Abbr"=sapply(nodes, xmlGetAttr,"F?RKORTNING"),
                         "Name"=sapply(nodes, xmlGetAttr,"BETECKNING"),stringsAsFactors=FALSE)
  return(partiData)
}


GetKrets <- function(val_xml){
  # Funktion som genererar ett dataset med information om kretsar beteckningar
  # fr?n valmyndighetens XML-filer
  # Libraries:
  library(XML)
  
  # Test av att det ?r ett XML-objekt
  if(class(val_xml)[1] != "XMLInternalDocument"){print("Ej ett XMLNode-objekt!");break}
  
  # XPaths?kning
  nodes = xpathApply(val_xml, "//KRETS_KOMMUN")
  
  # Skapa dataset
  kretsData = as.data.frame(t(sapply(nodes, xmlAttrs)))
  return(kretsData)
}

GetValdistrikt <- function(val_xml){
  # Beskrivning:
  # 
  # Libraries:
  library(XML)
  library(plyr)
  
  # Test av att det ?r ett XML-objekt
  if(class(val_xml)[1] != "XMLInternalDocument"){print("Ej ett XMLNode-objekt!");break}
  
  # XPaths?kning
  nodes = xpathApply(val_xml, "//VALDISTRIKT")
  
  # Skapa dataset
  valdirData = data.frame("KOD"=as.character(sapply(nodes, xmlGetAttr,"KOD")),
                          "NAMN"=as.character(sapply(nodes, xmlGetAttr,"NAMN")),
                          "INDELNING"=unlist(lapply(sapply(nodes, xmlGetAttr,"INDELNING"),NullToNone)),
                          "ROSTER"=as.numeric(sapply(nodes, xmlGetAttr,"R?STER")))                          
  # L?gger till ?vriga variabler
  # kodStr<-valdirData$KOD[1]
  valdirData <- cbind(valdirData,rbind.fill(lapply(as.character(valdirData$KOD),AddToValdistrikt)))
  return(valdirData)
}

# fil <- "slutresultat_0115K.xml"
# kodStr<-"01150511"

AddToValdistrikt <- function(kodStr){
  # Beskrivning:
  # 
  # Libraries:
  # Get nodes
  nodevaldistrikt <- xpathApply(val_xml, paste("//VALDISTRIKT[@KOD='",kodStr,"']/VALDELTAGANDE",sep=""))
  nodeparti <- xpathApply(val_xml, paste("//VALDISTRIKT[@KOD='",kodStr,"']/GILTIGA",sep=""))
  nodepartiOtherXMLv16 <- xpathApply(val_xml, paste("//VALDISTRIKT[@KOD='",kodStr,"']/?VRIGA_GILTIGA/GILTIGA",sep=""))
  nodepartiOtherXMLv13 <- xpathApply(val_xml, paste("//VALDISTRIKT[@KOD='",kodStr,"']/GILTIGA[@PARTI='?VR']/VARAV_?VRIGA",sep=""))
  nodekrets <- xpathApply(val_xml, paste("//KRETS_KOMMUN[VALDISTRIKT/@KOD='",kodStr,"']/@KOD",sep=""))
  
  # Parse XML
  partiname <- unlist(lapply(nodeparti, xmlGetAttr,"PARTI"))
  partinameOtherXMLv16 <- unlist(lapply(nodepartiOtherXMLv16, xmlGetAttr,"PARTI"))
  partinameOtherXMLv13 <- unlist(lapply(nodepartiOtherXMLv13, xmlGetAttr,"PARTI")) 
  particount <- as.numeric(unlist(lapply(nodeparti, xmlGetAttr,"R?STER")))
  particountOtherXMLv16 <- as.numeric(unlist(lapply(nodepartiOtherXMLv16, xmlGetAttr,"R?STER")))
  particountOtherXMLv13 <- as.numeric(unlist(lapply(nodepartiOtherXMLv13, xmlGetAttr,"R?STER")))
  kretscode <- as.character(nodekrets[[1]])
  voterscount <- as.numeric(lapply(nodevaldistrikt, xmlAttrs)[[1]][1])

  particountOther<-c(particountOtherXMLv16,particountOtherXMLv13)
  partinameOther<-c(partinameOtherXMLv16,partinameOtherXMLv13)
  
  output <- as.data.frame(matrix(c(voterscount,kretscode,particount,particountOther),
                                1,length(partiname)+length(partinameOther)+2))
  colnames(output) <- c("ROSTBERATTIGADE", "KRETS_KOMMUN_KOD",partiname,partinameOther)

  # Remove part ?VR (the parties in this category has already been parsed)
  if(length(particountOtherXMLv13)>0) output <- output[-which(colnames(output)=="?VR")]
    
  return(output)
}


GetOnsdistrikt <- function(val_xml){
  # Beskrivning:
  # 
  # Libraries:
  library(XML)
  library(plyr)
  
  # Test av att det ?r ett XML-objekt
  if(class(val_xml)[1] != "XMLInternalDocument"){print("Ej ett XMLNode-objekt!");break}
  
  # XPaths?kning
  nodes = xpathApply(val_xml, "//ONSDAGSDISTRIKT")
  
  # Skapa dataset
  valdirData = data.frame("KOD"=as.character(sapply(nodes, xmlGetAttr,"KOD")),
                          "NAMN"=as.character(sapply(nodes, xmlGetAttr,"NAMN")),
                          "INDELNING"=unlist(lapply(sapply(nodes, xmlGetAttr,"INDELNING"),NullToNone)),
                          "ROSTER"=as.numeric(sapply(nodes, xmlGetAttr,"R?STER")))                          
  # L?gger till ?vriga variabler
  valdirData <- cbind(valdirData,rbind.fill(lapply(as.character(valdirData$KOD),AddToOnsdistrikt)))
  return(valdirData)
}

AddToOnsdistrikt <- function(kodStr){
  # Beskrivning:
  # 
  # Libraries:
  nodeparti <- xpathApply(val_xml, paste("//ONSDAGSDISTRIKT[@KOD='",kodStr,"']/GILTIGA",sep=""))
  nodepartiOtherXMLv16 <- xpathApply(val_xml, paste("//ONSDAGSDISTRIKT[@KOD='",kodStr,"']/?VRIGA_GILTIGA/GILTIGA",sep=""))
  nodepartiOtherXMLv13 <- xpathApply(val_xml, paste("//ONSDAGSDISTRIKT[@KOD='",kodStr,"']/GILTIGA[@PARTI='?VR']/VARAV_?VRIGA",sep=""))
  nodekrets <- xpathApply(val_xml, paste("//KRETS_KOMMUN[ONSDAGSDISTRIKT/@KOD='",kodStr,"']/@KOD",sep=""))
  
  partiname <- unlist(lapply(nodeparti, xmlGetAttr,"PARTI"))
  partinameOtherXMLv16 <- unlist(lapply(nodepartiOtherXMLv16, xmlGetAttr,"PARTI"))
  partinameOtherXMLv13 <- unlist(lapply(nodepartiOtherXMLv13, xmlGetAttr,"PARTI"))
  particount <- as.numeric(unlist(lapply(nodeparti, xmlGetAttr,"R?STER")))
  particountOtherXMLv16 <- as.numeric(unlist(lapply(nodepartiOtherXMLv16, xmlGetAttr,"R?STER")))
  particountOtherXMLv13 <- as.numeric(unlist(lapply(nodepartiOtherXMLv13, xmlGetAttr,"R?STER")))
  kretscode <- as.character(nodekrets[[1]])
  
  particountOther<-c(particountOtherXMLv16,particountOtherXMLv13)
  partinameOther<-c(partinameOtherXMLv16,partinameOtherXMLv13)
  
  output <- as.data.frame(matrix(c(as.numeric(NA),kretscode,particount,particountOther),
                                 1,length(partiname)+length(partinameOther)+2))
  colnames(output) <- c("ROSTBERATTIGADE", "KRETS_KOMMUN_KOD",partiname,partinameOther)

  # Remove part ?VR (the parties in this category has already been parsed)
  if(length(particountOtherXMLv13)>0) output <- output[-which(colnames(output)=="?VR")]
  
  return(output)
}


GetRiksdagsKrets <- function(val_xml){
  # Funktion som genererar ett dataset med information om kretsar beteckningar
  # fr?n valmyndighetens XML-filer
  # Libraries:
  library(XML)
  
  # Test av att det ?r ett XML-objekt
  if(class(val_xml)[1] != "XMLInternalDocument"){print("Ej ett XMLNode-objekt!");break}
  
  # XPaths?kning kodStr <- as.character(kretsData$KOD)[1]
  nodes = xpathApply(val_xml, "//KRETS_RIKSDAG")
  
  # Skapa dataset
  RkretsData = as.data.frame(t(sapply(nodes, xmlAttrs)))
  RkretsData<-RkretsData[,c(1:2,4)]
  colnames(RkretsData)<-c("KRETS_RIKSDAG_KOD","KRETS_RIKSDAG_NAMN","KRETS_RIKSDAG_MANDAT")
  
  kretsKommun <- rbind.fill(lapply(as.character(RkretsData$KRETS_RIKSDAG_KOD),AddKommunRiksdagsKrets))
  kretsKommun <- kretsKommun[,c("KOD","NAMN","R_VALKRETS")]
  colnames(kretsKommun)<-c("KOMMUN_KOD","KOMMUN_NAMN","KRETS_RIKSDAG_KOD")
  
  Rkrets<-merge(RkretsData,kretsKommun,by="KRETS_RIKSDAG_KOD",all.y=TRUE)
  return(Rkrets)
}

AddKommunRiksdagsKrets <- function(kodStr){
  # Libraries:
  library(XML)
  
  # Test av att det ?r ett XML-objekt
  nodes = xpathApply(val_xml, paste("//KRETS_RIKSDAG[@KOD='",kodStr,"']/KOMMUN",sep=""))
  
  # Skapa dataset
  kommunKrets = as.data.frame(t(sapply(nodes, xmlAttrs)))
  kommunKrets$R_VALKRETS<-kodStr
  return(kommunKrets)
}

GetLandstingKrets <- function(val_xml){
  # Funktion som genererar ett dataset med information om kretsar beteckningar
  # fr?n valmyndighetens XML-filer
  # Libraries:
  library(XML)
  
  # Test av att det ?r ett XML-objekt
  if(class(val_xml)[1] != "XMLInternalDocument"){print("Ej ett XMLNode-objekt!");break}
  
  # XPaths?kning kodStr <- as.character(kretsData$KOD)[1]
  nodes = xpathApply(val_xml, "//KRETS_LANDSTING")
  
  # Skapa dataset mode(LkretsData[[1]])
  LkretsData <- lapply(nodes, xmlAttrs)
  LkretsData <- rbind.fill(lapply(LkretsData, function(X) as.data.frame(t(X))))

  # St?dar i filen
  LkretsData<-LkretsData[,c(1:2,4)]
  colnames(LkretsData)<-c("KRETS_LANDSTING_KOD","KRETS_LANDSTING_NAMN","KRETS_LANDSTING_MANDAT")

  return(LkretsData)
}

cleanConvertToNumeric<-function(fileValdistrikt){
  toConvertNames<- c("ROSTER","ROSTBERATTIGADE",alla_partier$Abbr)
  toConvertIndex<-which(colnames(fileValdistrikt)%in%toConvertNames)
  for (i in toConvertIndex){
    fileValdistrikt[,i]<-as.numeric(as.character(fileValdistrikt[,i]))
  }
  return(fileValdistrikt)
}

cleanParties<-function(fileValdistrikt,parties,limit=500){
  # Remove to small parties (less than limit)
  aggrIndex<-which(colnames(fileValdistrikt)%in%alla_partier$Abbr)
  toRemove<-apply(fileValdistrikt[,aggrIndex],MARGIN=2,FUN=sum,na.rm=TRUE) < limit
  toRemoveIndex<-which(colnames(fileValdistrikt)%in%names(toRemove[toRemove]))
  fileValdistrikt<-fileValdistrikt[,-toRemoveIndex]

  # Calculate Other party counts
  otherPartyCol<-colnames(fileValdistrikt)%in%alla_partier$Abbr & !colnames(fileValdistrikt)%in%parties
  
  # Remove/change NA for parties to be analyzed 
  fileValdistrikt[,colnames(fileValdistrikt)%in%parties][is.na(fileValdistrikt[,colnames(fileValdistrikt)%in%parties])]<-0  

  # Put the file together
  output<-fileValdistrikt[,!colnames(fileValdistrikt)%in%alla_partier$Abbr]
  output<-cbind(output,fileValdistrikt[,colnames(fileValdistrikt)%in%parties])
  output$Other <- apply(fileValdistrikt[,otherPartyCol],MARGIN=1,sum,na.rm=TRUE)  
  output<-cbind(output,fileValdistrikt[,otherPartyCol])
  
  return(output)
}
