#' @title The Frequon (Frequency Analysis) Game
#'
#' @description
#' The \code{frequon} function is used for solving problems in the data-based game ,,The Frequon Game''.
#'
#' @param ... \code{frequon} function is called by different arguments, which vary depending
#' on a problem that Bit is trying to solve. See \code{Details} in order to learn more about the list of possible arguments.
#'
#' @details Every time when some additional hints are needed one should add
#' \code{hint=TRUE} argument to the \code{proton} function.
#'
#' In order to get more information about a user on the Proton server
#' one should pass \code{action = "login"}, \code{login="XYZ"} arguments
#' to the \code{proton} function.
#'
#' In order to log into the Proton server one should pass \code{action = "login"},
#' \code{login="XYZ"}, \code{password="ABC"} arguments to the \code{proton} function.
#' If the password matches login, then one will receive a message about successful login.
#'
#' In order to log into a server different from Proton one should pass
#' \code{action = "server"}, \code{host="XYZ"} arguments to the \code{proton} function.
#'
#' ,,The Frequon Game'' is a free of charge, educational project of the SmarterPoland.pl Foundation.
#'
#' @author
#' Katarzyna Fak - the idea and the implementation,
#' Przemyslaw Biecek - comments and the integration with this package.
#'
#' @examples
#' \dontrun{
#' frequon()
#' frequon(hint=TRUE)
#' }
#' @rdname frequon
#' @export
frequon <- function(...) {
 args <- list(...)

 hintf <- function(level){
   cat(txt[[paste0("hint",level)]])
 }
 taskf <- function(level){
   cat(txt[[paste0("task",level)]])
 }

 txt <- as.list(dcode(.pouch$f))

 # plain start
 if( length(args) == 0 ){
   cat(txt$intro)
   .pouch$level <- 0
   return()
 }

 subjects <- c('0' = 're: interested?',
               '1' = 're: frequencies',
               '2' = 're: transcription',
               '3' = 're: key',
               '4' = 're: next text: and',
               '5' = 're: lengths in the text',
               '6' = 're: language in and message',
               '7' = 're: password')

 if ("subject" %in% names(args)) {
   if( tolower(args$subject) == subjects[1] ){
     if( digest(args$content) == "6ba57621bc24fe10f21bc8ff7c178b39" ){
       .pouch$level <- 1
       taskf(.pouch$level)
     } else if(.pouch$level == 0) cat(txt$errorIntro)
   }

   # 1. podaj wektor częstości
   if( tolower(args$subject) == subjects[2] ){
     if(is.null(names(args$content))){
       cat(txt$errorNoNAMES)
     } else{
       if( any(is.na(args$content)) || any(is.na(names(args$content))) )
         cat(txt$errorNAs) else{
           if(!all(names(args$content[letters])==letters))
             cat(txt$errorLetters)
           if( digest(args$content[letters]) == 'cf9f65b80d343b29860fb6cf10b644d4' ){
             .pouch$level <- 2  # przypisanie podwójne, żeby w każdej chwili można było
             # wywołać hinta dla odpowiedniego poziomu gry.
             taskf(.pouch$level )
           } else cat(txt$errorFrequencies)
         }
     }
   }
   ## 2. podstaw literki z EnglishLetterFrequency
   if( tolower(args$subject) == subjects[3] ){
     In <- gsub("[^A-Z]","", toupper(args$content))
     if( digest(In) == 'd5bc6f3d64e0199e08d029ee25d835a2' ){
       .pouch$level <- 3
       taskf(.pouch$level )
     } else cat(txt$errorDecipher)
   }
   ## 3. przetłumacz WSZYSTKIE literki (znajdź pełny klucz)
   if( tolower(args$subject) ==  subjects[4] ){
     if( any(names(args$content) != c("old","new")) | length(args$content) != 2 )
       cat(txt$errorKey1) else{
         if( any(nchar(args$content) != c(26,26)) )
           cat(txt$errorKey2) else{
             ## dopuszczam, że gracz może nie podać liter we wskazanej kolejności "abc...xyz"
             ## jeśli tylko podaje je poprawnie.
             ord <- order( strsplit(args$content['old'], "")[[1]] )
             In <- strsplit(args$content['new'], "")[[1]][ord]
             if( digest(In) == '277d35cce1043607f15f83fbc36b75ff' ){
               .pouch$level  <- 4
               taskf(.pouch$level )
             } else cat(txt$errorBadKey)
           }
       }
   }
   ## 4. znajdź kolejny szyfr
   if( tolower(args$subject) == subjects[5] ){
     if( strsplit(args$subject, " ")[[1]][3] %in% c('and', 'guns') ){
       cat(txt$errorBadX)
     } else {
       if( strsplit(args$subject, " ")[[1]][3] == "guns" ){
         cat(txt$errorBadX2)
       }
     }
     In <- gsub("[^A-Z]","", toupper(args$content))
     if( digest(In) == 'e876c26d9d8bad8028cefb95eb54df21' ){
       .pouch$level  <- 5
       taskf(.pouch$level )
     } else cat(txt$errorBadAND)
   }
   ## 6. zlicz długości słów
   if( tolower(args$subject) == subjects[6] ){
     if( ! any(names(args) %in% "attachment") ){
       cat(txt$errorAttachment)
     } else {
       if( !all(names(args$attachment) %in% names(wikiquotes)) ){
         cat(txt$errorWikiNames)
       }
       if( digest(args$content[as.character(1:13)]) == "0aac2628993a796da4b5408f8f9d0ef1" &&
           digest(args$attachment[names(wikiquotes)]) == "6e6b01872252d622733fcc3e0d29eee6" ){
         .pouch$level  <- 6
         taskf(.pouch$level )
       } else cat(txt$errorLengths)
     }
   }
   ## 7.  podanie języka
   if( tolower(args$subject) == subjects[7] ){
     if( digest(tolower(args$content)) == '939ad54fa39990afd160056465120f72'){
       .pouch$level  <- 7
       taskf(.pouch$level )
     } else cat(txt$errorLanguage)
   }
   ## 8. podanie hasła
   if( tolower(args$subject) == subjects[8] ){
     if( digest(args$content) == 'ba61e613c2207f0a81e0697914f3dc96' )
       cat(txt$outro) else cat(txt$errorEnd)
   }
 }

 if( length(args)>0 && !("content" %in% names(args)) && !('hint' %in% names(args)))
   cat(txt$errorContent)
 if( length(args)>0 && !("subject" %in% names(args)) && !('hint' %in% names(args)))
   cat(txt$errorSubject)

 if(!is.null(args$hint) && args$hint == TRUE)
   hintf(.pouch$level)
}

