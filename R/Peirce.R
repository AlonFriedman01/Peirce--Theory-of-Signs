#' @description regression forumla
#' @title Regression
#' @name Regression
newnewreg <- function (formula, data, subset, weights, na.action,
                       method = "qr", model = TRUE, contrasts = NULL, offset, ...)
{
  matchcalltrue = match.call()

  ## keep only the arguments which should go into the model frame
  matchfalse = match.call(expand.dots = FALSE)
  match = match(c("formula", "data", "subset", "weights", "na.action",
                  "offset"), names(matchfalse), 0)
  matchfalse = matchfalse[c(1, match)]

  #below drops the unselected values from the line above
  matchfalse$drop.unused.levels = TRUE
  matchfalse[[1]] = quote(stats::model.frame)

  # was as.name("model.frame"), but
  ##    need "stats:: ..." for non-standard evaluation

  matchfalse = eval.parent(matchfalse)
  if (method == "model.frame") return(matchfalse)

  ## 1) allow model.frame to update the terms object before saving it.
  matchattr = attr(matchfalse, "terms")
  y = model.response(matchfalse, "numeric")

  ## 2) retrieve the weights and offset from the model frame so
  ## they can be functions of columns in arg data.
  w = model.weights(matchfalse)
  offset = model.offset(matchfalse)
  x = model.matrix(matchattr, matchfalse, contrasts)
  ## if any subsetting is done, retrieve the "contrasts" attribute here.

  ## create object z where we store the values from the fit. lm.fit accepts matrix.
  ## x = model.matrx() sets the values to a matrix for lm.fit to accept.
  z <- lm.fit(x, y, offset = offset, ...)
  class(z) = c(if(is.matrix(y)) "mlm", "lm")

  ## 3) return the na.action info
  z$na.action = attr(matchfalse, "na.action")
  z$offset = offset

  ## 4) return the contrasts used in fitting: possibly as saved earlier.
  z$contrasts = attr(x, "contrasts")

  ## 5) return the levelsets for factors in the formula
  z$xlevels = .getXlevels(matchattr, matchfalse)
  z$call = matchcalltrue
  z$terms = matchattr
  if (model)  z$model = matchfalse
  z
}
