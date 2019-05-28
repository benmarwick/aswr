#' @name df_from_vector
#'
#' @description Takes a vector and converts it into a data frame of a given number of columns.
#'
#' @param vec A vector
#' @param n_col int The number of columns that the resulting table should have.
#'
#' @return A data frame.
#' @export
#'
#' @importFrom tibble as_tibble
#' @importFrom readr type_convert
#'
#' @examples
#'
#' vec <- rep(1:10)
#'
#' df_from_vector(vec, 2)
#'
df_from_vector <- function(vec, n_col) {

  # check that lenth of vector is divisible by ncol
  # so we can make columns of equal length

  if(length(vec) %% n_col != 0) stop("The number of elements in the vector is not an even multiple of the number of columns that you have requested")

    col_list <- vector("list", length = n_col)
    for(j in seq_len(n_col)) {
      chr_vec <-  vec
      col_list[[j]] <- chr_vec[seq(j, length(chr_vec), n_col)]
    }

    df_out <- tibble::as.tibble(do.call("cbind", col_list))
    df_out <- readr::type_convert(df_out)

    return(df_out)

  }





#' @name split_at
#'
#' @description Split a vector at specific positions defined by indices. From https://stackoverflow.com/a/16358095/1036500
#'
#' @param x A vector to split, e.g. a character vector
#' @param pos int An integer vector of indices to split at
#'
#' @return a list
#' @export
#'
#' @examples
#'
#' split_at(c("a", "b", "c", "d", "e", "f", "g"), 1)
#' split_at(c("a", "b", "c", "d", "e", "f", "g"), c(3, 5))
#'
split_at <- function(x, pos){

  unname(split(x, cumsum(seq_along(x) %in% pos)))

}




#  bround
#'
#' Round a number, preserving extra 0's
#'
#' Round a number, preserving extra 0's.
#'
#' From \url{https://github.com/kbroman/broman}
#'
#' #' @author Karl Broman \email{kbroman@gmail.com}
#'
#' @param x Number to round.
#'
#' @param digits Number of digits past the decimal point to keep.
#'
#' @details
#' Uses \code{\link[base]{sprintf}} to round a number, keeping extra 0's.
#'
#' @export
#' @return
#' A vector of character strings.
#'
#' @examples
#' bround(51.01, 3)
#' bround(0.199, 2)
#'
#' @seealso
#' \code{\link[base]{round}}, \code{\link[base]{sprintf}}
#'
#' @keywords
#' utilities
bround <-
  function(x, digits=1)
  {
    if(digits < 1)
      stop("This is intended for the case digits >= 1.")

    if(length(digits) > 1) {
      digits <- digits[1]
      warning("Using only digits[1]")
    }

    tmp <- sprintf(paste("%.", digits, "f", sep=""), x)

    # deal with "-0.00" case
    zero <- paste0("0.", paste(rep("0", digits), collapse=""))
    tmp[tmp == paste0("-", zero)] <- zero

    tmp
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
#' @export
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


#' A precise & pristine [ggplot2] theme with opinionated defaults and an emphasis on typography, from boB Rudis' https://github.com/hrbrmstr/hrbrthemes
#'
#' @md
#' @section Why Arial Narrow?:
#' First and foremost, Arial Narrow is generally installed by default or readily
#' available on any modern system, so it's "free"-ish; plus, it is a condensed font
#' with solid default kerning pairs and geometric numbers.
#'
#' @section Building upon `theme_ipsum`:
#' The function is setup in such a way that you can customize your own one by just
#' wrapping the call and changing the parameters. See source for examples.
#'
#' @section Gotchas:
#' There are distinctions between font names and various devices. Names that work
#' for display graphics devices and bitmap ones such as `png` may not work well
#' for PostScript or PDF ones. You may need two versions of a font-based
#' theme function for them to work in a particular situation. This situation
#' usually only arises when using a newer font with many weights but somewhat
#' irregular internal font name patterns.
#'
#' @md
#' @param base_family,base_size base font family and size
#' @param plot_title_family,plot_title_face,plot_title_size,plot_title_margin plot tilte family, face, size and margi
#' @param subtitle_family,subtitle_face,subtitle_size plot subtitle family, face and size
#' @param subtitle_margin plot subtitle margin bottom (single numeric value)
#' @param strip_text_family,strip_text_face,strip_text_size facet label font family, face and size
#' @param caption_family,caption_face,caption_size,caption_margin plot caption family, face, size and margin
#' @param axis_title_family,axis_title_face,axis_title_size axis title font family, face and size
#' @param axis_title_just axis title font justificationk one of `[blmcrt]`
#' @param plot_margin plot margin (specify with [ggplot2::margin])
#' @param grid panel grid (`TRUE`, `FALSE`, or a combination of `X`, `x`, `Y`, `y`)
#' @param axis add x or y axes? `TRUE`, `FALSE`, "`xy`"
#' @param ticks ticks if `TRUE` add ticks
#' @export
#' @examples \dontrun{
#' library(ggplot2)
#' library(dplyr)
#'
#' # seminal scatterplot
# ggplot(mtcars, aes(mpg, wt)) +
#   geom_point() +
#   labs(x="Fuel use (mpg)", y="Weight (tons)",
#        title="Seminal ggplot2 scatterplot example",
#        subtitle="A plot that is only useful for demonstration purposes",
#        caption="Brought to you by the letter 'g'") +
#   theme_arial()
#'
#' # seminal bar chart
#'
#' update_geom_font_defaults()
#'
#' count(mpg, class) %>%
#'   ggplot(aes(class, n)) +
#'   geom_col() +
#'   geom_text(aes(label=n), nudge_y=3) +
#'   labs(x="Fuel use (mpg)", y="Weight (tons)",
#'        title="Seminal ggplot2 bar chart example",
#'        subtitle="A plot that is only useful for demonstration purposes",
#'        caption="Brought to you by the letter 'g'") +
#'   theme_arial(grid="Y") +
#'   theme(axis.text.y=element_blank())
#' }
theme_arial <- function(base_family="Arial Narrow",
                        base_size = 11,
                        plot_title_family=base_family,
                        plot_title_size = 18,
                        plot_title_face="bold",
                        plot_title_margin = 10,
                        subtitle_family=base_family,
                        subtitle_size = 12,
                        subtitle_face = "plain",
                        subtitle_margin = 15,
                        strip_text_family = base_family,
                        strip_text_size = 12,
                        strip_text_face = "plain",
                        caption_family = base_family,
                        caption_size = 9,
                        caption_face = "italic",
                        caption_margin = 10,
                        axis_title_family = subtitle_family,
                        axis_title_size = 9,
                        axis_title_face = "plain",
                        axis_title_just = "mc",
                        plot_margin = margin(30, 30, 30, 30),
                        grid = TRUE,
                        axis = FALSE,
                        ticks = FALSE) {

  ret <- ggplot2::theme_minimal(base_family=base_family, base_size=base_size)

  ret <- ret + theme(legend.background=element_blank())
  ret <- ret + theme(legend.key=element_blank())

  if (inherits(grid, "character") | grid == TRUE) {

    ret <- ret + theme(panel.grid=element_line(color="#2b2b2bdd", size=0.10))
    ret <- ret + theme(panel.grid.major=element_line(color="#2b2b2b99", size=0.10))
    ret <- ret + theme(panel.grid.minor=element_line(color="#2b2b2b99", size=0.05))

    if (inherits(grid, "character")) {
      if (regexpr("X", grid)[1] < 0) ret <- ret + theme(panel.grid.major.x=element_blank())
      if (regexpr("Y", grid)[1] < 0) ret <- ret + theme(panel.grid.major.y=element_blank())
      if (regexpr("x", grid)[1] < 0) ret <- ret + theme(panel.grid.minor.x=element_blank())
      if (regexpr("y", grid)[1] < 0) ret <- ret + theme(panel.grid.minor.y=element_blank())
    }

  } else {
    ret <- ret + theme(panel.grid=element_blank())
  }

  if (inherits(axis, "character") | axis == TRUE) {
    ret <- ret + theme(axis.line=element_line(color="#2b2b2b", size=0.15))
    if (inherits(axis, "character")) {
      axis <- tolower(axis)
      if (regexpr("x", axis)[1] < 0) {
        ret <- ret + theme(axis.line.x=element_blank())
      } else {
        ret <- ret + theme(axis.line.x=element_line(color="#2b2b2b", size=0.15))
      }
      if (regexpr("y", axis)[1] < 0) {
        ret <- ret + theme(axis.line.y=element_blank())
      } else {
        ret <- ret + theme(axis.line.y=element_line(color="#2b2b2b", size=0.15))
      }
    } else {
      ret <- ret + theme(axis.line.x=element_line(color="#2b2b2b", size=0.15))
      ret <- ret + theme(axis.line.y=element_line(color="#2b2b2b", size=0.15))
    }
  } else {
    ret <- ret + theme(axis.line=element_blank())
  }

  if (!ticks) {
    ret <- ret + theme(axis.ticks = element_blank())
    ret <- ret + theme(axis.ticks.x = element_blank())
    ret <- ret + theme(axis.ticks.y = element_blank())
  } else {
    ret <- ret + theme(axis.ticks = element_line(size=0.15))
    ret <- ret + theme(axis.ticks.x = element_line(size=0.15))
    ret <- ret + theme(axis.ticks.y = element_line(size=0.15))
    ret <- ret + theme(axis.ticks.length = grid::unit(5, "pt"))
  }

  xj <- switch(tolower(substr(axis_title_just, 1, 1)), b=0, l=0, m=0.5, c=0.5, r=1, t=1)
  yj <- switch(tolower(substr(axis_title_just, 2, 2)), b=0, l=0, m=0.5, c=0.5, r=1, t=1)

  ret <- ret + theme(axis.text.x=element_text(margin=margin(t=0)))
  ret <- ret + theme(axis.text.y=element_text(margin=margin(r=0)))
  ret <- ret + theme(axis.title=element_text(size=axis_title_size, family=axis_title_family))
  ret <- ret + theme(axis.title.x=element_text(hjust=xj, size=axis_title_size,
                                               family=axis_title_family, face=axis_title_face))
  ret <- ret + theme(axis.title.y=element_text(hjust=yj, size=axis_title_size,
                                               family=axis_title_family, face=axis_title_face))
  ret <- ret + theme(strip.text=element_text(hjust=0, size=strip_text_size,
                                             face=strip_text_face, family=strip_text_family))
  ret <- ret + theme(panel.spacing.x=grid::unit(2, "lines"))
  ret <- ret + theme(panel.spacing.y=grid::unit(2, "lines"))
  ret <- ret + theme(plot.title=element_text(hjust=0, size=plot_title_size,
                                             margin=margin(b=plot_title_margin),
                                             family=plot_title_family, face=plot_title_face))
  ret <- ret + theme(plot.subtitle=element_text(hjust=0, size=subtitle_size,
                                                margin=margin(b=subtitle_margin),
                                                family=subtitle_family, face=subtitle_face))
  ret <- ret + theme(plot.caption=element_text(hjust=1, size=caption_size,
                                               margin=margin(t=caption_margin),
                                               family=caption_family, face=caption_face))
  ret <- ret + theme(plot.margin=plot_margin)

  ret

}

#' Update matching font defaults for text geoms
#'
#' Updates [ggplot2::geom_label] and [ggplot2::geom_text] font defaults
#'
#' @param family,face,size font family name, face and size
#' @export
update_geom_font_defaults <- function(family="Arial Narrow", face="plain", size=3.5) {
  update_geom_defaults("text", list(family=family, face=face, size=size))
  update_geom_defaults("label", list(family=family, face=face, size=size))
}

#' @rdname ArialNarrow
#' @md
#' @title Arial Narrow font name R variable aliases
#' @description `font_an` == "`Arial Narrow`"
#' @format length 1 character vector
#' @export
font_an <- "Arial Narrow"


#' theme_rotate_x_text
#'
#' Shortcut for rotating the x-axis tick mark labels, because I find it hard to remember the full code for it.
#'
#' @param angle
#' @param hjust
#' @param vjust
#'
#' @return
#' @export
#'
#' @examples
theme_rotate_x_text <- function(angle = -90,
                                hjust = 0,
                                vjust = 0.25){

  ggplot2::theme(axis.text.x = element_text(angle = angle,
                                 hjust = hjust,
                                 vjust = vjust))
}


#' Clean up a character vector to make it numeric
#'
#' Remove commas & whitespace, primarily, from https://github.com/hrbrmstr/hrbrmisc
#'
#' @param x character vector to process
#' @return numeric vector
#' @export
make_numeric <- function(x) { as.numeric(gsub(",|[^0-9.]", "", trimws(x))) }

#' Clean up a character vector to make it a percent
#'
#' Remove "%" primarily, convert to numeric & divide by 100, from https://github.com/hrbrmstr/hrbrmisc
#'
#' @param x character vector to process
#' @return numeric vector
#' @export
make_percent <- function(x) { as.numeric(gsub("%", "", trimws(x))) / 100 }
