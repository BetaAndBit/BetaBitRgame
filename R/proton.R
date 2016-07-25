#' @title The Proton Game
#'
#' @description
#' The \code{proton} function is used for solving problems in the data-based game ,,The Proton Game''.
#' Solve four data-based puzzles in order to crack into Pietraszko's account!
#'
#' @param ... \code{proton} function is called by different arguments, which vary depending
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
#' ,,The Proton Game'' is a free of charge, educational project of the SmarterPoland.pl Foundation.
#'
#' @author
#' Przemyslaw Biecek, \email{przemyslaw.biecek@@gmail.com}, SmarterPoland.pl Foundation.
#'
#' @examples
#' proton()
#' proton(hint=TRUE)
#' @rdname proton
#' @importFrom digest digest
#' @export
proton <- function(...) {
 args <- list(...)
 texts <- dcode(.proton.)

 # plain start
 if (length(args) == 0) {
    cat(texts["proton.init"])
    return(invisible(NULL))
 }
 if (length(args) == 1 && !is.null(args$hint) && args$hint) {
   cat(texts["proton.init"], "\n\nHINT:\n",texts["proton.init.w"], sep = "")
   return(invisible(NULL))
 }

 # action = server
 if(length(args)>0 && !is.null(args$action) && args$action == "server") {
  if (!is.null(args$host) && digest(args$host) == "94265570be658d9fafa4861d7252afa9") {
    cat(texts["proton.host.instr"])
    if (!is.null(args$hint) && args$hint) {
      cat("\n\nHINT:\n",texts["proton.host.instr.w"], sep = "")
    }
    return(invisible(NULL))
  } else {
    cat("Bit spent some time to infiltrate this workstation. \nBut there is nothing interesting here.\nFind the workstation which Pietraszko is using most often to log into the Proton server and try again.")
  }
 }

 # action = login
 if(length(args)>0 && !is.null(args$action) && args$action == "login") {
   # only user is set to johnins
   if (!is.null(args$login) && args$login == texts["log.1"] && is.null(args$password)) {
     cat(texts["proton.login.init"])
     if (!is.null(args$hint) && args$hint) {
       cat("\nHINT:\n",texts["proton.login.init.w"], sep = "")
     }
     return(invisible(NULL))
   }
   if(is.null(args$login)) {
     cat("\nIf action='login' argument is set then one should also set login=. argument \n")
     return(invisible(NULL))
   }

   # user is set to janie and password is provided
   if (!is.null(args$login) && args$login == texts["log.1"] && !is.null(args$password)) {
     if (digest(args$password) == "bbfb4a474b61b80225fd49d7c67e5a01") {
       cat(texts["proton.login.pass.instr"])
       if (!is.null(args$hint) && args$hint) {
         cat("\nHINT:\n",texts["proton.login.pass.instr.w"], sep = "")
       }
       return(texts["proton.login.pass"])
     } else {
       return(texts["proton.login.fail"])
     }
   }
   # user is set to sl and password is provided
   if (!is.null(args$login) && args$login == texts["log.2"] && !is.null(args$password)) {
     if (digest(args$password) == "ce3494fef4545c1b6160e5430d7efe66") {
       cat(texts["proton.final"])
       return(texts["proton.login.pass"])
     } else {
       return(texts["proton.login.fail"])
     }
   }

   # only user is set
   if (!is.null(args$login) && args$login != texts["log.1"]) {
     cat(texts["proton.login.weak"])
     return(invisible(NULL))
   }

 }
}
