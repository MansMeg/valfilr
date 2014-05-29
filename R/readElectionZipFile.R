#' @title 
#' readElectionZipFile
#'
#' @description 
#' Read in the zipfiles from valmyndigheten to an R-object to parse.
#' 
#' @param file The zip file from valmyndigheten. (Här kan vi lägga till en internetadress)
#' 
#' @return
#' A ValXML class object.
#' 
#' @export
#' 

readElectionZipFile <- function(zipfile){
  require(XML)
  require(stringr)
  
  # Unzip to temporary directory
  unzipFolder <- paste(tempdir(), "/electFiles", sep="")
  unzip(zipfile, exdir=unzipFolder)
  
  # List unpacked files
  xmlFiles <- dir(unzipFolder)
  xmlFiles <- xmlFiles[str_detect(string=tolower(xmlFiles),"xml")]

  xmlFileList <- list()
  dtdVector <- character(length(xmlFiles))
  for(i in seq_along(xmlFiles)){ 
    fileName <- paste(unzipFolder, xmlFiles[i], sep="/")
    fileHead <- readLines(con=fileName,n=3)
    dtdVector[i] <- unlist(str_split(fileHead[3],"\""))[4]
    xmlFileList[[i]] <- 
      xmlInternalTreeParse(paste(unzipFolder, xmlFiles[i], sep="/"),
                           encoding="latin1")
  }
  names(xmlFileList) <- xmlFiles  
  class(xmlFileList) <- "ValXML"
  attr(xmlFileList, which="DTD") <- dtdVector
  return(xmlFileList)
}


#' @title 
#' print.ValXML
#'
#' @description 
#' Prints the ValXML class.
#' 
#' @param x A ValXML object.
#' 
#' @export
#' 
print.ValXML <- function(x){
  stopifnot(class(x)=="ValXML")
  headAttr <- xmlAttrs(xmlRoot(x[[1]]))
  cat(headAttr[1],"\n", headAttr[3], "\nRapporttidpunkt: ", headAttr[7], sep="")
}

