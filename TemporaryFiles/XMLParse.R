##### Preparations #####
# Libraries
library(stringr)
library(plyr)
rm(list=ls())


##### Parameters #####
year <- "2010"
# Data directory
dataDir<-"slutresultat2010/"

# Parties to output (the rest is turned into Other)
parties<-c("M","C","FP","KD","S","V","MP","SD")
# Data and code directory
code.path <- "~/code"
data.path <- "~/RawData"


# Read files
source.with.encoding(file.path(code.path,'XMLGetFunctions.R'), encoding='ISO-8859-1')
setwd(file.path(data.path,"DataValmyndigheten"))

# Include files to parse
data = dir(dataDir)
data<- data[!str_length(data)<22]





##### XMLParse: Valdistrikdata #####
for (fil in data){
  cat(fil) 
  val_typ <- str_sub(string=fil,start=18,end=18)
  val_xml <- xmlInternalTreeParse(paste(dataDir,fil,sep=""),encoding="latin1")
  cat(".")

  # Fil: Partier
  partier <- GetPartier(val_xml)
  if (exists("alla_partier")){
    in_file <- partier$Abbr %in% alla_partier$Abbr
    if(!all(in_file)){
    alla_partier <- rbind.fill(alla_partier,partier[!in_file,])
    }
  }else{
    alla_partier <- partier
  }
  cat(".")
  
  # Fil: Valkretsar
  if (val_typ=="L"){
    kretsar <- GetKrets(val_xml)
    if (exists("valkretsarL")){
      valkretsarL <- rbind.fill(valkretsarL,kretsar)
    }else{
      valkretsarL <- kretsar
    }
  }else if(val_typ=="K"){
    kretsar <- GetKrets(val_xml)
    if (exists("valkretsarK")){
      valkretsarK <- rbind.fill(valkretsarK,kretsar)
    }else{
      valkretsarK <- kretsar
    }    
  }
  cat(".")
  
  # Fil: Valdistrikt
  objectname <- paste("valdistrikt",val_typ,sep="")
  distrikt <- GetValdistrikt(val_xml)
  distrikt <- rbind.fill(distrikt,GetOnsdistrikt(val_xml))
  if (exists(objectname)){
    eval(parse(text=paste(objectname,"<- rbind.fill(",objectname,",distrikt)",sep="")))
  }else{
    assign(objectname,distrikt)
  }
  cat(".")
  
  # Fil: Lägga till onsdagsvaldistrikt (ej implementerat)
  
  cat("Done.\n") 
}


##### XMLParse: Riksdags- och landstingsvalkretsar ####
data <- dir(dataDir)
data <- data[str_length(data)==20]

for (fil in data){
  cat(fil) 
  val_typ <- str_sub(string=fil,start=16,end=16)
  val_xml <- xmlInternalTreeParse(paste(dataDir,fil,sep=""),encoding="latin1")
  cat(".")
  
  # Riksdagsvalkretsar
  if(val_typ=="R"){
    valkretsR <- GetRiksdagsKrets(val_xml)
  }
  # Landstingsvalkretsar
  if(val_typ=="L"){
    valkretsL <- GetLandstingKrets(val_xml)
  }
  cat(".")
  cat("Done.\n") 
}

##### Cleanup #####
# Cleanup valkretsar
if(exists("valkretsarK") && exists("valkretsarL")){
  valkretsK <- merge(x=valkretsarK,y=valkretsarL[c("KOD","KRETS_LANDSTING")],by="KOD",all.x=TRUE)
  rm(list=c("valkretsarL","valkretsarK"))
}

if(exists("valkretsK")){
  # Renaming column names
  nameIndex<-which(colnames(valkretsK) %in% c("KOD","NAMN","MANDAT_VALKRETS","KRETS_LANDSTING"))
  colnames(valkretsK)[nameIndex] <- c("KRETS_KOMMUN_KOD","KRETS_KOMMUN_NAMN","KRETS_KOMMUN_MANDAT","KRETS_LANDSTING_KOD")

  # Adding Gotland kommunresultat till Landsting (Gotland don't have landstingsval)
  distriktToAdd <- valdistriktK[substr(x=valdistriktK$KOMMUNKRETSKOD,1,4)=="0980",]
  distriktToAdd <- distriktToAdd[,!apply(is.na(distriktToAdd),2,all)]
  valdistriktL <- rbind.fill(valdistriktL,distriktToAdd)
  rm(distriktToAdd)
}

# Converting numbers to numeric, putting other parties together rename to year
toConvert<-c("ROSTER","ROSTBERATTIGADE",parties)
if(exists("valkretsK")) {
  objNames<-paste("valdistrikt",c("K","L","R"),sep="")
}else{
  objNames<-"valdistriktE"
}

for(objName in objNames){
  eval(parse(text=paste(objName,year,"<-cleanConvertToNumeric(",objName,")",sep="")))
  eval(parse(text=paste(objName,year,"<-cleanParties(",objName,year,",parties)",sep="")))
}
eval(parse(text=paste("alla_partier",year,"<-alla_partier",sep="")))

##### Save #####
if(exists("valkretsK")) {
  filesToSave<-c(paste(objNames,year,sep=""),paste("valkrets",c("K","L","R"),sep=""),paste("alla_partier",year,sep=""))
}else{
  filesToSave<-c(paste(objNames,year,sep=""),paste("alla_partier",year,sep=""))
}
save(list=filesToSave,file=paste("Valdata",year,".Rdata",sep=""))
