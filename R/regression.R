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
#' In this game you are helping Professor Pearson.
#' You can communicate with him through the \code{regression} function.
#'
#' In each call include the \code{subject} parameter (indicating which task
#' you are trying to answer) and the \code{content} parameter (providing
#' information Professor Pearson is asking you for in a given task).
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

  textsRegression = as.list(dcode(.regression.))

  if (length(args) == 0) {
    cat(textsRegression$regressionInit)
    return(invisible(NULL))
  }
  if ("subject" %in% names(args)) {
    args$subject = tolower(args$subject)
    . = list(structure(c(-0.143966665283887, -0.137825041716732, -0.157386920578125), .Names = c("MATH_2009", "READ_2009", "SCIE_2009")), "highconf", NULL, list(c("RAVEN_WYN", "STAI_C_WYN", "ZAMPS_WYN"), c("csesi", "STAI_C_WYN", "ZAMPS_WYN")), c(-0.6375, 1.1661, 1.5848, 0.2752, 1.26, 0.2914, 0.4058, 0.2478, 0.7394, 0.8916, 0.0043, 0.7219, -0.282, -1.2267, 1.5022, 0.8074, 1.1414, 0.8687, -1.3389, -0.0902, 4.945, 0.5476, -0.2839, -0.1818, -0.4485, -0.074, 0.2379, 0.249, 2.4501, -0.1493, -0.6062, -0.4345, -0.6615, 0.1084, 0.4113, 1.2611, -0.3029, 0.5105, 0.3736, 0.1127, -0.1324, 0.8799, -1.9888, 1.1488, -0.1969, 0.3077, 0.8966, -0.3707, 1.8101, 0.0376, 0.622, 1.0357, -0.8864, 1.3858, 0.8317, -0.4309, 0.4432, 0.4627, -0.0403, 0.4621, -1.5229, 0.7644, 0.0242, 1.0579, 0.4841, -0.9998, 0.9073, -0.0962, 0.4208, 1.1767, 0.5754, 1.4375, -1.6987, 1.8664, 0.5893, 0.837, 0.0181, -0.1621, -0.0472, 0.5972, -0.1009, -0.0312, -1.5385, 0.825, -0.9025, 0.9718, 0.0498, 0.4716, 0.9132, 2.2402, 1.6773, 0.3976, 0.9948, 3.1998, -2.0272, 2.1335, 1.1378, 0.7727, -0.9487, 1.6824, 0.8196, -0.9819, -1.1, -0.8967, -1.7019, -0.7351, 2.1406, -0.7031, 0.7061, 1.4704, 0.575, -2.271, -0.2446, 0.677, 1.0731, 0.5211, -1.3844, -0.3239, 1.9043, 1.6658,-1.4841, 2.8874, 2.064, -0.4097, 1.582, -0.5039, 1.2635, 0.4873, -0.2557, -0.4544, -0.4648, 1.3875, -1.7898, 1.2693, 2.4197, 3.1531, 0.3177, -1.7153, -1.4094, 0.2531, 0.2441, -0.4066, 0.0512, -0.4713, -0.0557, 5.334, 0.0388, 0.0423, 1.3605, -0.2314, 1.4575, 0.5591, -0.5954, 0.2199, 3.2709, 0.1701, -1.3674, -0.3077, -0.1179, -0.7117, -1.2782, -0.3209, -0.983, -1.298, 1.6415, 3.7454, 1.2008, 1.6935, 0.1726, -0.2236, -0.4053, 0.1983, -0.1504, 4.4081, 0.3223, 0.4518, -1.0513, -0.2394, 3.6358, 0.9655, 0.2501, -0.0744, -1.3324, 1.8961, -0.4012, 1.5268, 4.609, -0.3739, 0.7202, -0.5722, 0.3523, 1.1486, 1.5051, 1.7594, 2.9057, 1.0941, -1.1194), list(c(43, 61, 73, 83, 94, 95, 96, 105, 117, 133, 136, 146, 155, 190), c(43, 61, 73, 83, 94, 95, 96, 105, 112, 117, 133, 136, 146, 155, 190)), 24446455.2489998)
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
                    unname(.[[1]]))[1] %in% TRUE) {
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
                    .[[2]])[1] %in% TRUE) {
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
        eval(args$content[1], BetaBit::FSW),
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
      mTemp = with(BetaBit::FSW, lm(READ_2009 ~ cultpos + incomeTr))
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
          commentReplace = "It's nice you decided to use logarithmic transformation. However if you used base 2 or 10 instead of e, it would be a little easier to interpret the slope parameter coefficient value. It looks quite good, but there is still a little problem. Perhaps there is something wrong with a way you determine value of SCHOOL_ID on the basis of the names (or perhaps order) of the model contrasts (dummy variables). Please, check what values of SCHOOL_ID appear in the dataset and compare with how they are described in names of the model contrasts."
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
        cat("Unfortunately after this transformation `income` is still insignifficant.\n")
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
          .[[4]],
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
      mTemp = lm(varsTemp, BetaBit::FSW)
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
          .[[5]],
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
      } else if (!all(args$content %in% BetaBit::FSW$SCHOOL_ID)) {
        cat("Some values you gave don't appear in `FSW$SCHOOL_ID`.\n")
        return(invisible(FALSE))
      } else if (
        any(sapply(
          .[[6]][1:2],
          function(x, y) {return(all(y %in% x))},
          y = args$content
        ))
      ) {
        if (length(args$content) == length(.[[6]][[1]])) {
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
          .[[6]][3:4],
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
            eval(x, BetaBit::FSW),
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
        dataTemp = cbind(BetaBit::FSW, as.data.frame(varsTemp))
      } else {
        if (!all(all.vars(args$content[[3]]) %in% "RAVEN_AGE")) {
          cat("No other variable than `RAVEN_AGE` can appear on the right side the model formula unless you provide expressions describing how to compute them by specifying the `vars` argument.\n")
          return(invisible(FALSE))
        }
        dataTemp = BetaBit::FSW
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
      if (deviance(mTemp) <= .[[7]]) {
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

