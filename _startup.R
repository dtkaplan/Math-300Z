# Common formatting for the Blog posts

library(knitr)
library(kableExtra)

kill_row_names <- function(x) {
  row.names(x) <- NULL
  x
}

tbl_style_html <- function(tbl, ...) {
  columns <- 1:ncol(tbl)
  kable(tbl) %>%
    kable_styling(bootstrap_options = c("striped", "condensed"),
                  full_width = FALSE,
                  html_font = "Courier", position="left",
                  ...) #|> column_spec(columns, width="3cm")
}

tbl_style_tex <- function(tbl, ...) {
  paste("\\ttfamily", kable(tbl, booktabs=TRUE, format="latex"), "\\normalfont\n\\bigskip")
}

library(knitr)

knit_print.data.frame <- function (x, options, ...) {

  tbl_style <- ifelse(knitr::is_html_output(), tbl_style_html, tbl_style_tex)

  if ("displayrows" %in% names(options)) {
    # rmarkdown::paged_table(x, options) |>
    #   rmarkdown:::print.paged_df()
    x <- head(x, options$displayrows)
  }

  if ("digits" %in% names(options)) {
    if (length(options$digits) > 1) {
      if (length(options$digits) != ncol(x)) {

        stop("There are", ncol(x), " columns and ",
             length(options$digits), " digits. digits argument must have one component for each column.")
      } else {
        for (k in 1:length(options$digits)) {
          if (!is.na(options$digits[k]))
            x[,k] <- signif(x[,k], digits=as.numeric(options$digits[k]))
        }
      }
    } else {
      fix_signif <- function(x) {
        signif(x, digits=as.numeric(options$digits))
      }
      x <- x |>
        mutate_if(is.numeric, fix_signif)
    }
  }


  tbl_style(x) |> asis_output()

}



registerS3method("knit_print", "data.frame", knit_print.data.frame)

