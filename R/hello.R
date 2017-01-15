


#' pull a vector or scalar (vector of length one) from a data frame
#'
#' Sometimes we want to get a single value from a dplyr pipeline so we can use it with inline R code in R markdown. This function is an efficient method of extracting a column from a tibble as a vector. From \url{http://stackoverflow.com/a/24730843/1036500}
#'
#' @param x a data frame
#' @param y a column name
#'
#' @return a vector or scalar
#' @export
#'
#' @examples
#'
#' pull(iris, Species)
#'
pull <- function(x,y) {
  x[,if(is.name(substitute(y))) deparse(substitute(y)) else y, drop = FALSE][[1]]
}

#' Search and replace in a vector using a vector of patterns
#'
#' This is a small variation on the standard methods for searching and replacing on matches in a vector, the grep/grepl/sub/gsub functions. Those functions do not natively handle a vector of patterns for matching. This function allows you to convienently pass a vector of patterns to search for matches in a vector.
#'
#'
#' @param x a vector, such as a vector of patterns that you want to search for in a vector
#' @param y a vector, the one that you want to search in
#' @param ... other arguments for \code{\link[base]{grep}}
#'
#' @author Ben Marwick \email{benmarwick@gmail.com}
#'
#' @return a vector
#' @export
#'
#' @examples
#'
#' x <- letters[1:3]
#' y <- letters[1:10]
#'
#' grepv(x, y)
#'
#' # compare with
#' # grep(x, y)
#'
#'
grepv <- function(x, y, ...){
  grep(paste0(x, collapse = "|"), y, ...)
}

#' @rdname grepv
#' @export
greplv <- function(x, y, ...){
  grepl(paste0(x, collapse = "|"), y, ...)
}


#' @rdname grepv
#' @export
subv <- function(x, y, ...){
  sub(paste0(x, collapse = "|"), y, ...)
}

#' @rdname grepv
#' @export
gsubv <- function(x, y, ...){
  sub(paste0(x, collapse = "|"), y, ...)
}


#' Return the current date and time, nicely formatted
#'
#' Convenience function that returns date and time formatted as I like them. From \url{https://github.com/pascal-niklaus/pascal/}
#'
#' @param fmt A string specifying the format of date and time. Defaults to \code{\%d-\%b-\%Y \%H:\%M:\%S}.
#'          Check \code{\link{strftime}} for format specifications.
#' @return Current data and time as character string, formatted as specified
#' @examples
#' current_time()
#' @seealso \code{\link{Sys.time}}, \code{\link{strftime}}
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @export
current_time <- function(fmt="%d-%b-%Y %H:%M:%S") {
  format(Sys.time(), fmt)
}


#' Close all text sinks and graphics devices
#'
#' Close all graphics devices if any are open, issuing a warning.
#' From \url{https://github.com/pascal-niklaus/pascal/}
#'
#' @return none

#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @importFrom grDevices dev.list dev.off
#' @export
close_all <- function()
{
  while(!is.null(dev.list()))
  {
    warning("some devices are open -- closing !!!");
    dev.off();
  }
}

#' Replace NA and NaN with zero
#'
#' Convenience function that replaces NA and NaN in vectors, lists, and matrices by zero
#'
#' Note that +Inf/-Inf values are preserved.
#'
#' From \url{https://github.com/pascal-niklaus/pascal/}
#'
#' @param x vector of data
#' @return vector with non-finite (NaN, Inf) and missing data (NA) replaced by zero
#' @examples
#' x <- 1:5
#' x[3]<-NA
#' x[4]<-1/0
#' x
#' ## [1]   1   2  NA Inf   5
#' NAtozero(x)
#' ## [1] 1 2 0 0 5
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @export
NAtozero <- function(x)
{
  if(is.list(x)) {
    lapply(x,NAtozero);
  } else if(is.matrix(x)) {
    apply(x,1:2,NAtozero);
  } else
    ifelse(is.na(x) | is.nan(x),0,x);
}


#' Robust detection of extreme values
#'
#' Detect extreme values based on interval defined by median absolute deviation
#'
#' The function \code{xtreme} checks which values are more than \code{f} median absolute deviations
#' (scaled to standard deviation equivalents) away from the median.
#' With the default value of f = 3.5 and a perfect normal distribution, only 1 in ca. 2000
#' values would be classified as extremes. With f = 4, the fraction of extremes reduces to 1 in 16000 values.
#'
#'  From \url{https://github.com/pascal-niklaus/pascal/}
#'
#' @param x vector of data
#' @param f number of sd-equivalents defining extreme values
#' @keywords misc utilities
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @seealso \code{\link{mad}}
#' @examples
#' pnorm(-3.5)*2; # fraction of data more than 3.5 sd away from mean
#' y <- rnorm(1e6)
#' sum(xtreme(y))/1e6;
#' @importFrom stats mad median
#' @export
xtreme <- function(x,f=3.5)
{
  tmp<-abs(x-median(x,na.rm=TRUE))>f*mad(x,na.rm=TRUE);
  !is.na(tmp) & tmp;
}



#' Human Numbers
#'
#' Format numbers so they're legible for humans. Use this in ggplot for labels where you might use the comma or percent functions from the scales package.
#'
#' Checks whether numbers are positive or negative.
#' Allows up to 1 significant figure
#' sapply used for element-wise application of the humanity function as a vector may include
#' numbers where billions, millions or thousands are appropriate.
#'
#' From \url{https://github.com/fdryan}
#'
#' @return a character vector the same length as the input vector
#' @param x a numeric vector to format
#' @param smbl a symbol you'd like to prefix your numbers by
#' @examples
#' human_numbers(c(1000000 , 1500000, 10000000000))
#' human_numbers(c(1.200000e+05, -2.154660e+05, 2.387790e+05, 4.343500e+04 ,5.648675e+12), "$")
#' ggplot2 + scale_y_continuous(labels = human_numbers)
#' ggplot2 + scale_x_continuous(labels = human_numbers)
#' ggplot2 + scale_x_continuous(labels = human_gbp)
#' # custom function for large currency values:
#' human_usd   <- function(x){human_numbers(x, smbl = "$")}

human_numbers <- function(x = NULL, smbl =""){
  humanity <- function(y){

    if (!is.na(y)){

      b <- round_any(abs(y) / 1e9, 0.1)
      m <- round_any(abs(y) / 1e6, 0.1)
      k <- round_any(abs(y) / 1e3, 0.1)

      if ( y >= 0 ){
        y_is_positive <- ""
      } else {
        y_is_positive <- "-"
      }

      if ( k < 1 ) {
        paste0(y_is_positive, smbl, y )
      } else if ( m < 1){
        paste0 (y_is_positive, smbl,  k , "k")
      } else if (b < 1){
        paste0 (y_is_positive, smbl, m ,"m")
      } else {
        paste0 (y_is_positive, smbl,  comma(b), "b")
      }
    }
  }

  sapply(x,humanity)
}

#' Human versions of large currency numbers - extensible via the smbl argument.


#' Uppercase the First Character of Each Word in a String
#'
#' This function takes in a string of arbitrary length and returns a string of the
#' same length with the first character in each word capitalized, assuming that
#' character is alphabetic. From \url{https://github.com/michaeltoth/}
#'
#' @param string String to modify
#'
#' @return Returns a string with the first character of each word capitalized, if that character is alphabetic.
#'
#' @examples
#' toupper_first('new york')
#' ## -> [1] "New York"
#'
#' toupper_first('NEW YORK')
#' ## -> [1] "New York"
#'
#' toupper_first('5th avenue')
#' ## -> [1] "5th Avenue"
#'
#' @export
toupper_first <- function(string) {
  s <- tolower(strsplit(string, " ")[[1]])
  paste0(toupper(substring(s, 1, 1)), substring(s, 2), collapse = " ")
}

# add_commas
#' Add commas to a large number
#'
#' Convert a number to a string, with commas every 3rd digit. From \url{https://github.com/kbroman}
#'
#' @param numbers Vector of non-negative numbers (will be rounded to integers)
#'
#'  @author Karl Broman
#'
#' @export
#' @return Character string with numbers written like \code{"7,547,085"}.
#'
#' @examples
#' add_commas(c(231, 91310, 2123, 9911001020, 999723285))
add_commas <-
  function(numbers)
  {
    format(numbers, big.mark=",", scientific=FALSE, trim=TRUE)
  }

#' Numbers spelled out in English
#'
#' The numbers 1-20 spelled out in English, for use in reports. From \url{https://github.com/kbroman}
#'
#' @name numbers
#'
#' @docType data
#'
#' @aliases Numbers
#'
#' @author Karl Broman
#'
#' @details
#' \itemize{
#' \item \code{numbers} - lower case
#' \item \code{Numbers} - Capitalized
#' }
#'
#' @format A vector of character strings
#'
#' @keywords datasets
#'
#' @examples
#' numbers[5]
#' Numbers[5]
NULL

#' @name Numbers
#' @rdname numbers
NULL

#' List objects by size
#'
#' Lists objects in memory in the current session, and shows how much memory they're using. From \url{http://stackoverflow.com/q/1358003/1036500}
#'
#' @param pos .
#' @param pattern .
#' @param order.by .
#' @param decreasing sort by descending?
#' @param head .
#' @param n Number of rows to show
#'
#' @return a data frame
#' @export
#'
#' @examples
#' x <- 1e5
#' object_sizes()
#'
object_sizes <- function(..., n=10) {

.object_sizes <- function (pos = 1,
                           pattern,
                           order.by,
                           decreasing=FALSE,
                           head=FALSE,
                           n=5) {
  napply <- function(names, fn) sapply(names, function(x)
    fn(get(x, pos = pos)))
  names <- ls(pos = pos, pattern = pattern)
  obj.class <- napply(names, function(x) as.character(class(x))[1])
  obj.mode <- napply(names, mode)
  obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
  obj.prettysize <- napply(names, function(x) {
    capture.output(format(utils::object.size(x), units = "auto")) })
  obj.size <- napply(names, object.size)
  obj.dim <- t(napply(names, function(x)
    as.numeric(dim(x))[1:2]))
  vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
  obj.dim[vec, 1] <- napply(names, length)[vec]
  out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
  names(out) <- c("Type", "Size", "PrettySize", "Rows", "Columns")
  if (!missing(order.by))
    out <- out[order(out[[order.by]], decreasing=decreasing), ]
  if (head)
    out <- head(out, n)
  out
}

.object_sizes(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)

}




