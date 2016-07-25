#' @title The Regression Game
#'
#' @description
#' The \code{regression} function is used for solving problems in the data-based
#' game ,,The regression Game''.
#'
#' @param ... \code{regression} function is called with different arguments, which
#' vary depending on a problem that Beta and Bit are trying to solve. See
#' \code{Details} in order to learn more about the list of possible arguments.
#'
#' @details Every time when some additional hints are needed one should add
#' \code{hint = TRUE} or \code{techHint = TRUE} argument to the \code{regression} function.
#' Technical hints will point out R packages and/or functions which might help
#' you to solve the task while "normal" hints provide you with methodological
#' advices.
#'
#' In this game you are helping a Professor Pearson.
#' You can communicate with him through the \code{regression} function.
#'
#' In each call include the \code{subject} parameter (indicating which task
#' you are trying to answer) and the \code{content} parameter (providing
#' information professor Pearson is asking you for in a given task).
#'
#' Data used in the game comes from the study of Polish upper-secondary
#' schools first grade students. It was conducted together with the
#' PISA 2009 study using the same cognitive tests and questionnaires as
#' in PISA 2009 but on a different group of students (in Poland most of the
#' students in a PISA sample attends lower-secondary schools). The students who
#' participated in the first wave of the study were followed in the 2nd grade of
#' upper-secondary school within the research program \emph{Our further study
#' and work} (\emph{Nasza Dalsza Nauka i Praca}). Both studies were conducted by
#' the Institute of Philosophy and Sociology Polish Academy of Sciences.
#' \strong{The original data was changed a little, to better fit the purpose of
#' the game.}
#'
#' ,,The Regression Game'' is a free of charge, educational project of the
#' SmarterPoland.pl Foundation.
#' @return
#' Function returns one of three possible values:
#' \itemize{
#'   \item{\code{TRUE} if you provided correct answer to a task,}
#'   \item{\code{FALSE} if you provided wrong answer to a task,}
#'   \item{\code{NULL} if function can't identify task you wanted to answer.}
#' }
#' @author
#' \itemize{
#'   \item{Tomasz Zoltak - the idea and the implementation,}
#'   \item{Mateusz Zoltak - comments, contribution to hints,}
#'   \item{Zuzanna Brzozowska - proofreading,}
#'   \item{Przemyslaw Biecek - comments and the integration with the `BetaBit` package.}
#' }
#' @examples
#' regression()
#' regression(hint = TRUE)
#' regression(techHint = TRUE)
#' @rdname regression
#' @importFrom stats lm deviance formula
#' @export
regression <- function(...) {
  args = list(...)
  textsRegression = dcode(.regression.)

  if (length(args) == 0) {
    cat(textsRegression$regressionInit)
    return(invisible(NULL))
  }
  if ("subject" %in% names(args)) {
    args$subject = tolower(args$subject)
    if (args$subject == "summer internship") {
      if ("hint" %in% names(args)) {
        cat(textsRegression$hint0)
        return(invisible(FALSE))
      }
      if ("techHint" %in% names(args)) {
        cat(textsRegression$techHint0)
        return(invisible(FALSE))
      }
      cat(textsRegression$task1)
      return(invisible(TRUE))
    } else if (!any(c("content", "hint") %in% names(args))) {
      cat("You forgot to send the results!\nUse argument `content` of the `regression()` function to send the data.\n")
      return(invisible(FALSE))
    }
    # zadanie 1. - korelacje
    if (args$subject == "correlations") {
      if ("hint" %in% names(args)) {
        cat(textsRegression$hint1)
        return(invisible(FALSE))
      }
      if ("techHint" %in% names(args)) {
        cat(textsRegression$techHint1)
        return(invisible(FALSE))
      }
      if (!is.vector(args$content)) {
        cat("Argument `content` must be a vector of mode `numeric` and length of 3.\n")
        return(invisible(FALSE))
      } else if (!is.numeric(args$content) | length(args$content) != 3) {
        cat("Argument `content` must be a vector of mode `numeric` and length of 3.\n")
        return(invisible(FALSE))
      }
      if (!is.null(names(args$content)) & is.vector(args$content)) {
        args$content = args$content[order(names(args$content))]
      }
      if (all.equal(unname(args$content),
                    unname(zadaniaRegresja::answers[[1]]))[1] %in% TRUE) {
        cat(textsRegression$task2)
        return(invisible(TRUE))
      } else {
        cat(textsRegression$wrongAnswer)
        return(invisible(FALSE))
      }
    # zadanie 2. - diagnostyka liniowosci
    } else if (args$subject == "name of the variable") {
      if ("hint" %in% names(args)) {
        cat(textsRegression$hint2)
        return(invisible(FALSE))
      }
      if ("techHint" %in% names(args)) {
        cat(textsRegression$techHint2)
        return(invisible(FALSE))
      }
      if (!is.vector(args$content)) {
        cat("Argument `content` must be a character vector of length 1.\n")
        return(invisible(FALSE))
      } else if (!is.character(args$content) | length(args$content) != 1) {
        cat("Argument `content` must be a character vector of length 1.\n")
        return(invisible(FALSE))
      }
      if (all.equal(unname(args$content),
                    zadaniaRegresja::answers[[2]])[1] %in% TRUE) {
        cat(textsRegression$task3)
        return(invisible(TRUE))
      }
      else {
        cat(textsRegression$wrongAnswer)
        return(invisible(FALSE))
      }
    # zadanie 3. - przeksztalcenie zmiennej niezaleznej
    } else if (args$subject == "transformation") {
      if ("hint" %in% names(args)) {
        cat(textsRegression$hint3)
        return(invisible(FALSE))
      }
      if ("techHint" %in% names(args)) {
        cat(textsRegression$techHint3)
        return(invisible(FALSE))
      }
      if (!is.vector(args$content)) {
        cat("Argument `content` must contain an expression.\n")
        return(invisible(FALSE))
      } else if (!is.expression(args$content[1])) {
        cat("Argument `content` must contain an expression.\n")
        return(invisible(FALSE))
      } else if (!all(all.vars(args$content[1]) %in% "income")) {
        cat("There should be no variables other than `income` in your expression.\n")
        return(invisible(FALSE))
      }
      incomeTr = tryCatch(
        eval(args$content[1], zadaniaRegresja::FSW),
        error = function(e) {
          cat(
            "Trying to evaluate your expression: `",
            as.character(args$content)[1],
            "` causes an error:\n\n",
            sep = ""
          )
          print(e)
          return(NULL)
        }
      )
      if (is.null(incomeTr)) {
        return(invisible(FALSE))
      }
      mTemp = with(zadaniaRegresja::FSW, lm(READ_2009 ~ cultpos + incomeTr))
      if (summary(mTemp)$coef[3, 4] <= 0.05) {
        functionsUsed = setdiff(all.names(args$content[1]), "income")
        if (
          length(functionsUsed) == 1 &
          all(functionsUsed %in% c("log2", "log10"))
        ) {
          commentReplace =
                paste0("It's nice you decided to use logarithmic transformation. The slope parameter for transformed income has clear interpretation: that's the change in prediction when the value of income rises ",
                       ifelse(functionsUsed == "log2", "twice", "ten times"), ".")
        } else if (length(functionsUsed) == 1 &
                   all(functionsUsed %in% "log")) {
          commentReplace = "It's nice you decided to use logarithmic transformation. However if you used base 2 or 10 instead of e, it would be a little easier to interpret the slope parameter coefficient value. It looks quite good, but there is still a little problem. Perhaps there is something wrong with a way you determine value of SCHOOL_ID on the basis of the names (or perhaps order) of the model contrasts (dummie variables). Please, check what values of SCHOOL_ID appear in the dataset and compare with how they are described in names of the model contrasts."
        } else {
          commentReplace = paste0(
            "Note however that if you used base 2 logarithm to ",
            "transform `income`, then the slope parameter would be ",
            "more easily interpretable."
          )
        }
        textsRegression$task4 = sub(
          "comment on 3rd task",
          commentReplace,
          textsRegression$task4
        )
        # trzeba zbadac rozwiazanie i podmienic komentarz do niego
        cat(textsRegression$task4)
        return(invisible(TRUE))
      } else {
        cat("Ufortunately after this transformation `income` is still insignifficant.\n")
        return(invisible(FALSE))
      }
    # zadanie 4. - wspolliniowosc
    } else if (args$subject == "collinearity") {
      if ("hint" %in% names(args)) {
        cat(textsRegression$hint4)
        return(invisible(FALSE))
      }
      if ("techHint" %in% names(args)) {
        cat(textsRegression$techHint4)
        return(invisible(FALSE))
      }
      varsTemp = all.vars(
        ~ SEX + SCHOOL_TYPE + log(income) + homepos + hisei +
        csesi + RAVEN_WYN + STAI_C_WYN +  STAI_S_WYN + SES_WYN + ZAMPS_WYN
      )
      if (!is.vector(args$content)) {
        cat("Argument `content` must be a vector of mode `character`.\n")
        return(invisible(FALSE))
      } else if (!is.character(args$content)) {
        cat("Argument `content` must be a vector of mode `character`.\n")
        return(invisible(FALSE))
      } else if (!all(args$content %in% varsTemp)) {
        cat("Some of variables you gave don't appear in the model. Check variable names.\n")
        return(invisible(FALSE))
      } else if (
        any(sapply(
          zadaniaRegresja::answers[[4]],
          function(x, y) {return(all(y %in% x))},
          y = args$content
        ))
      ) {
        cat(textsRegression$task5)
        return(invisible(TRUE))
      }
      varsTemp = setdiff(varsTemp, args$content)
      varsTemp = sub("income", "log(income)", varsTemp)
      varsTemp = formula(paste("READ_2009 ~ ", paste(varsTemp, collapse = "+")))
      mTemp = lm(varsTemp, zadaniaRegresja::FSW)
      if (any(summary(mTemp)$coef[-1, 4] > 0.05)) {
        cat("Unfortunately, there is/are still some insignificant parameter(s) in the model.\n")
        print(summary(mTemp))
        return(invisible(FALSE))
      } else {
        cat("All variables in the model are statistically significant, but you removed more variables than in the optimal solution. Try removing other variables.")
      }
    # zadanie 5. - regresja w ramach grup (interakacje I)
    } else if (args$subject == "groups") {
      if ("hint" %in% names(args)) {
        cat(textsRegression$hint5)
        return(invisible(FALSE))
      }
      if ("techHint" %in% names(args)) {
        cat(textsRegression$techHint5)
        return(invisible(FALSE))
      }
      if (!is.data.frame(args$content)) {
        cat("Argument `content` must be a data frame with two columns: `SCHOOL_ID`and `par_hisei`.\n")
        return(invisible(FALSE))
      } else if (!(all(names(args$content) %in% c("SCHOOL_ID", "par_hisei")))) {
        cat("Argument `content` must be a data frame with two columns: `SCHOOL_ID`and `par_hisei`.\n")
        return(invisible(FALSE))
      }
      args$content = args$content[order(args$content$SCHOOL_ID), ]
      if (
        all.equal(
          args$content$par_hisei,
          zadaniaRegresja::answers[[5]],
          tolerance = 0.001
        )[1] %in% TRUE
      ) {
        cat(textsRegression$task6)
        return(invisible(TRUE))
      } else {
        cat(textsRegression$wrongAnswer)
        return(invisible(FALSE))
      }
    # zadanie 6. - istotnosc roznic (interakcje II)
    } else if (args$subject == "significant differences") {
      if ("hint" %in% names(args)) {
        cat(textsRegression$hint6)
        return(invisible(FALSE))
      }
      if ("techHint" %in% names(args)) {
        cat(textsRegression$techHint6)
        return(invisible(FALSE))
      }
      if (!is.vector(args$content)) {
        cat("Argument `content` must be a vector of mode `numeric`.\n")
        return(invisible(FALSE))
      } else if (!is.numeric(args$content)) {
        cat("Argument `content` must be a vector of mode `numeric`.\n")
        return(invisible(FALSE))
      } else if (!all(args$content %in% zadaniaRegresja::FSW$SCHOOL_ID)) {
        cat("Some values you gave don't appear in `FSW$SCHOOL_ID`.\n")
        return(invisible(FALSE))
      } else if (
        any(sapply(
          zadaniaRegresja::answers[[6]][1:2],
          function(x, y) {return(all(y %in% x))},
          y = args$content
        ))
      ) {
        if (length(args$content) == length(zadaniaRegresja::answers[[6]][[1]])) {
          commentReplace = "Note however, that you treated the mean value of slope parameters as it was estimated without any error. Do you know what can you do to account for this error while checking significance of the differences?"
        } else {
          commentReplace = "That's nice you took into account that the mean value of slope parameters is also estimated with error."
        }
        textsRegression$task7 = sub(
          "comment on 6th task",
          commentReplace,
          textsRegression$task7
        )
        cat(textsRegression$task7)
        return(invisible(TRUE))
      } else if (
        any(sapply(
          zadaniaRegresja::answers[[6]][3:4],
          function(x, y) {return(all(y %in% x))},
          y = args$content
        ))
      ) {
        cat("You are close to the right solution but it looks like you messed up SCHOOL_ID values/labels. Please compare SCHOOL_ID values from the dataset and the contrasts variables names in the model. If you use `contr.sum`, note that it does not use names (labels) of factor levels to construct names of contrasts (and this is behaviour different to what `contr.treatment` does).\n")
        return(invisible(FALSE))
      } else {
        cat(textsRegression$wrongAnswer)
        return(invisible(FALSE))
      }
    # zadanie 7. - modelowanie wieku
    } else if (args$subject == "age") {
      if ("hint" %in% names(args)) {
        cat(textsRegression$hint7)
        return(invisible(FALSE))
      }
      if ("techHint" %in% names(args)) {
        cat(textsRegression$techHint7)
        return(invisible(FALSE))
      }
      if (!("formula" %in% class(args$content))) {
        cat("Argument `content` must be a model formula.\n")
        return(invisible(FALSE))
      } else if (args$content[[2]] != "READ_2009") {
        cat("There should be simply `READ_2009` on the left side of the model formula given by `content`.\n")
        return(invisible(FALSE))
      }
      if ("vars" %in% names(args)) {
        if (!is.list(args$vars)) {
          cat("Argument `vars` must be a list of expressions.\n")
          return(invisible(FALSE))
        } else if (!all(sapply(args$vars, is.expression))) {
          cat("Argument `vars` must be a list of expressions.\n")
          return(invisible(FALSE))
        } else if (
          !all(sapply(args$vars, function(x) {all(all.vars(x) %in% "RAVEN_AGE")}))
        ) {
          cat("No other variable than `RAVEN_AGE` can appear in expressions given in the `vars` argument.\n")
          return(invisible(FALSE))
        } else if (
          !all(all.vars(args$content) %in% c("READ_2009", "RAVEN_AGE", names(args$vars)))
        ) {
          cat("Expressions defining some variables that appear in model formula given by argument `content` do not appear in argument `vars`. Check your formula and names of elements of the list of expressions.\n")
          return(invisible(FALSE))
        }
        varsTemp = lapply(args$vars, function(x) {
          return(tryCatch(
            eval(x, zadaniaRegresja::FSW),
            error = function(e) {
              cat(
                "Trying to evaluate your expression: `",
                as.character(x),
                "` causes an error:\n\n",
                sep = ""
              )
              print(e)
              return(NULL)
            }
          ))
        })
        if (any(sapply(varsTemp, is.null))) {
          return(invisible(FALSE))
        }
        dataTemp = cbind(zadaniaRegresja::FSW, as.data.frame(varsTemp))
      } else {
        if (!all(all.vars(args$content[[3]]) %in% "RAVEN_AGE")) {
          cat("No other variable than `RAVEN_AGE` can appear on the right side the model formula unless you provide expressions describing how to compute them by specifying the `vars` argument.\n")
          return(invisible(FALSE))
        }
        dataTemp = zadaniaRegresja::FSW
      }
      mTemp = tryCatch(
        lm(args$content, dataTemp),
        error = function(e) {
          cat("Trying to estimate regression model caused an error. Probably there's something wrong with a model formula you provided.\n\n")
          print(e)
          return(NULL)
        }
      )
      if (is.null(mTemp)) {
        return(invisible(FALSE))
      }
      if (deviance(mTemp) <= zadaniaRegresja::answers[[7]]) {
        cat(textsRegression$congratulations)
        return(invisible(TRUE))
      } else {
        cat("Try to change something - your model should fit the data better.")
        return(invisible(FALSE))
      }
    # niepoprawny `subject`
    } else {
      cat("Please check the subject. Something is wrong there!")
      return(invisible(NULL))
    }
  } else if ("hint" %in% names(args)) {
    cat("Just type `regression()` into the console and hit `enter` :)")
    return(invisible(FALSE))
  }

  return(invisible(NULL))
}

