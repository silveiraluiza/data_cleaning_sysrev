library(readr)
require(plyr)
require(stringr)
library("dplyr")

rm_accent <- function(str,pattern="all") {
  if(!is.character(str))
    str <- as.character(str)
  
  pattern <- unique(pattern)
  
  if(any(pattern=="Ç"))
    pattern[pattern=="Ç"] <- "ç"
  
  symbols <- c(
    acute = "áéíóúÁÉÍÓÚýÝ",
    grave = "àèìòùÀÈÌÒÙ",
    circunflex = "âêîôûÂÊÎÔÛ",
    tilde = "ãõÃÕñÑ",
    umlaut = "äëïöüÄËÏÖÜÿ",
    cedil = "çÇ"
  )
  
  nudeSymbols <- c(
    acute = "aeiouAEIOUyY",
    grave = "aeiouAEIOU",
    circunflex = "aeiouAEIOU",
    tilde = "aoAOnN",
    umlaut = "aeiouAEIOUy",
    cedil = "cC"
  )
  
  accentTypes <- c("´","`","^","~","¨","ç")
  
  if(any(c("all","al","a","todos","t","to","tod","todo")%in%pattern)) # opcao retirar todos
    return(chartr(paste(symbols, collapse=""), paste(nudeSymbols, collapse=""), str))
  
  for(i in which(accentTypes%in%pattern))
    str <- chartr(symbols[i],nudeSymbols[i], str) 
  
  return(str)
}

pes_loc = 'pubmed_result_string f_com filtro_474.csv'
scop_loc = 'scopus_string f_com filtro_967.csv'
doc_loc = '/Users/lsilvei6/Documents'
setwd(doc_loc)

resultados_pes <- read_csv(pes_loc)
resultado_scop <-read_csv(scop_loc)

resultado_scop <- resultado_scop[,c('Title','Authors')]
resultados_pes <- resultados_pes[,c('Title','Description')]              

cols = c('key','Authors')
names(resultado_scop) <- cols
names(resultados_pes) <- cols

resultado_scop$Title_Scop = resultado_scop$key
resultado_scop$Authors_Scop = resultado_scop$Authors

resultados_pes$Title_PMed = resultados_pes$key
resultados_pes$Authors_PMed = resultados_pes$Authors

resultado_scop[cols] <- lapply(resultado_scop[cols], rm_accent)
resultado_scop[cols] <- lapply(resultado_scop[cols], tolower)
resultado_scop[cols] <- colwise(str_trim)(resultado_scop[cols])
resultado_scop$key <- gsub('[[:punct:] ]+','',resultado_scop$key)

resultados_pes[cols] <- lapply(resultados_pes[cols], rm_accent)
resultados_pes[cols] <- lapply(resultados_pes[cols], tolower)
resultados_pes[cols] <- colwise(str_trim)(resultados_pes[cols]) 
resultados_pes$key <- gsub('[[:punct:] ]+','',resultados_pes$key)

drops <- c("Authors")
resultados_pes <- resultados_pes[ , !(names(resultados_pes) %in% drops)]
resultado_scop <- resultado_scop[ , !(names(resultado_scop) %in% drops)]

total <- merge(resultado_scop,resultados_pes,by=c("key"))
total$Source <- 'Scopus/PubMed'

resultado_scop$Source <- 'Scopus'
resultados_pes$Source <- 'PubMed'

resultado_scop <- anti_join(resultado_scop,total, by = "key")
resultados_pes <- anti_join(resultados_pes,total, by = "key")

CombinedTable <- bind_rows(total,resultado_scop,resultados_pes)

write.csv2(CombinedTable, file = "Resultados_finais_ScopPMed.csv",row.names=FALSE)
