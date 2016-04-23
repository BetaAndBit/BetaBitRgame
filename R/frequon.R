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


}

