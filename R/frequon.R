#' @title The Frequon (Frequency Analysis) Game
#'
#' @description
#' The \code{frequon} function is used for solving problems in the data-based game ,,The Frequon Game''.
#'
#' @param ... \code{frequon} function is called by different arguments, which vary depending
#' on a problem that Bit is trying to solve. See \code{Details} in order to learn more about the list of possible arguments.
#'
#' @details Every time when some additional hints are needed one should add
#' \code{hint=TRUE} argument to the \code{frequon} function.
#'
#' In this game you are in contact with a group of people that are going to stop terrorists.
#' You can communicate with them through \code{frequon} function.
#'
#' In each call add \code{subject} parameter that will indicate which message you are answering.
#' Add \code{content} parameter. It's value should match the request.
#'
#' ,,The Frequon Game'' is a free of charge, educational project of the SmarterPoland.pl Foundation.
#'
#' @author
#' \itemize{
#'   \item{Katarzyna Fak - the idea and the implementation,}
#'   \item{Przemyslaw Biecek - comments and the integration with the `BetaBit` package.}
#' }
#'
#' @examples
#' frequon()
#' frequon(hint=TRUE)
#' @rdname frequon
#' @importFrom stats filter
#' @importFrom stats na.omit
#' @export
frequon <- function(...) {
 args <- list(...)

 hintf <- function(level){
   cat(txt[[paste0("hint",level)]])
 }
 taskf <- function(level){
   cat(txt[[paste0("task",level)]])
 }

 txt <- as.list(dcode(.frequon.))

 # plain start
 if( length(args) == 0 ){
   cat(txt$intro)
   .pouch$level <- 0
   return(invisible(TRUE))
 }

 subjects <- c('0' = 're: interested?',
               '1' = 're: frequencies',
               '2' = 're: transcription',
               '3' = 're: key',
               '4' = 're: next text',
               '5' = 're: lengths in the text',
               '6' = 're: language in and message',
               '7' = 're: password')

 if ("subject" %in% names(args)) {
   if( tolower(args$subject) == subjects[1] ){
     if( digest(args$content) == digest(BetaBit::roses) ){
       .pouch$level <- 1
       taskf(.pouch$level)
     } else if(.pouch$level == 0) cat(txt$errorIntro)
   }

   if (!(tolower(args$subject) %in% subjects)) {
     cat("Check the mail subject. Something is wrong there!")
     return(invisible(FALSE))
   }

  # 1. give a vector of frequencies
   if( tolower(args$subject) == subjects[2] ){
     if(is.null(names(args$content))){
       cat(txt$errorNoNAMES)
     } else{
       if( any(is.na(args$content)) || any(is.na(names(args$content))) )
         cat(txt$errorNAs) else{
           if((length(na.omit(names(args$content[letters]))) != length(letters)) ||
              (!all(names(args$content[letters])==letters)))
             cat(txt$errorLetters)
           if( digest(as.numeric(args$content[letters])) == '8a37375e11927ab4564a62598ae764dd' ){
             .pouch$level <- 2  # a double assignement to be able to call hint on the
             # proper level of the game.
             taskf(.pouch$level )
             return(invisible(TRUE))
           } else cat(txt$errorFrequencies)
         }
     }
   }
   ## 2. substitute letters from EnglishLetterFrequency
   if( tolower(args$subject) == subjects[3] ){
     In <- gsub("[^A-Z]","", toupper(args$content))
     if( digest(In) == 'd5bc6f3d64e0199e08d029ee25d835a2' ){
       .pouch$level <- 3
       taskf(.pouch$level )
       return(invisible(TRUE))
     } else cat(txt$errorDecipher)
   }
   ## 3. translate ALL of the letters (find a complete key)
   if( tolower(args$subject) ==  subjects[4] ){
     if( length(args$content) != 2 || any(sort(names(args$content)) != c("new","old")) )
       cat(txt$errorKey1) else{
         if( any(nchar(args$content) != c(26,26)) )
           cat(txt$errorKey2) else{
             ## I assume that a player may not give the letters in the indicated order "abc...xyz"
             ## if only they are correct.
             ord <- order( strsplit(args$content['old'], "")[[1]] )
             In <- strsplit(args$content['new'], "")[[1]][ord]
             if( digest(In[-c(2, 21, 9, 11)]) == 'e81104a8409e08ab2eaaa41fe6645056' ){
               .pouch$level  <- 4
               taskf(.pouch$level )
               return(invisible(TRUE))
             } else cat(txt$errorBadKey)
           }
       }
   }
   ## 4. find another cipher
   if( tolower(args$subject) == subjects[5] ){
     if( strsplit(args$subject, " ")[[1]][3] %in% c('and', 'guns') ){
       cat(txt$errorBadX)
     } else {
       if( strsplit(args$subject, " ")[[1]][3] == "guns" ){
         cat(txt$errorBadX2)
       }
     }
     In <- gsub("[^A-Z]","", toupper(args$content))
     if( digest(In) %in% c('e876c26d9d8bad8028cefb95eb54df21', '54bac78ea14b3ddd536318edbd629ad4', digest(gsub("[^A-Z]","", toupper(BetaBit::pcs)))) ){
       .pouch$level  <- 5
       taskf(.pouch$level )
       return(invisible(TRUE))
     } else cat(txt$errorBadAND)
   }
   ## 5. count the words' lengths
   if( tolower(args$subject) == subjects[6] ){
     if( ! any(names(args) %in% "attachment") ){
       cat(txt$errorAttachment)
     } else {
       if( !all(names(args$attachment) %in% names(BetaBit::wikiquotes)) ){
         cat(txt$errorWikiNames)
       }
       if( digest(as.numeric(args$content[as.character(6:10)])) == "a99031c077f98cd93b351882400b7dbd"){
         if (digest(as.numeric(args$attachment[["Czech"]][as.character(2:10)])) == "a976e39ca9e1eb15418cce4606575002") {
           .pouch$level  <- 6
           taskf(.pouch$level )
           return(invisible(TRUE))
         } else cat(txt$errorLengths0)
       } else cat(txt$errorLengths)
     }
   }
   ## 6. give a language
   if( tolower(args$subject) == subjects[7] ){
     if( digest(tolower(args$content)) == '4f808eee5fb8c3a585d76daf132e3990'){
       .pouch$level  <- 7
       taskf(.pouch$level )
       return(invisible(TRUE))
     } else cat(txt$errorLanguage)
   }
   ## 7. give a password
   if( tolower(args$subject) == subjects[8] ){
     if( digest(args$content) == 'ba61e613c2207f0a81e0697914f3dc96' ) {
       cat(txt$outro)
       return(invisible(TRUE))
     } else {
         cat(txt$errorEnd)
         return(invisible(FALSE))
       }
   }
 }

 if( length(args)>0 && !("content" %in% names(args)) && !('hint' %in% names(args))) {
   cat(txt$errorContent)
   return(invisible(FALSE))
 }
 if( length(args)>0 && !("subject" %in% names(args)) && !('hint' %in% names(args))) {
   cat("Did you send this message without subject?\n")
   return(invisible(FALSE))
 }

 if(!is.null(args$hint) && args$hint == TRUE) {
   hintf(.pouch$level)
   return(invisible(TRUE))
 }

 return(invisible(FALSE))
}

