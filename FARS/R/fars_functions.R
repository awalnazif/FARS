#' Read data from the "Fatality Analysis Reporting System (FARS)"

#' This is a function that reads in data from the US National Highway Traffic Safety Administration's
#' Fatality Analysis Reporting System. It checks if the the data file exists and if so reads in the csv data,
#' (using the \code{fars_read} argument). If the file does not exists, it prints "file "filename" does not exist".
#' The data is turned into a dataframe table. The function requires functions from the packages readr and dyplr.
#'
#' @param filename The name of the file that is read in.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @return This function reads in csv data, if the specified filename exists in the working directory,
#'    and turns it into a tibble data. If the filename does not exist, "file "filename" does not exists" is printed.
#'
#' @examples
#' \dontrun{fars_read("accident_2013.csv")}
#'
#' @export
fars_read <- function(filename) {
    if(!file.exists(filename))
        stop("file '", filename, "' does not exist")
    data <- suppressMessages({
        readr::read_csv(filename, progress = FALSE)
    })
    dplyr::tbl_df(data)
}


#' Create a filename based on the input year
#'
#' This is a function that returns a filename in the format "accident_year.csv.bz2",
#' where you can specify the year.
#'
#' @param year The year which you want in your filename.
#'
#' @return This function creates a filename that shows the specified year in the filename.
#'
#' @examples
#' \dontrun{make_filename(2013)}
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Read in "FARS" data by specifying the year
#'
#' This is a function that reads in FARS data by using as an input the year.
#' It returns the months and year as a list. An warning message is thrown, when an invalid year is chosen
#' as an input.
#'
#' @param years The input year. If the input year does not exist,
#'    a warning message is thrown: invalid year "year".
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @return This function creates a list of the months and year for data specified with the input year.
#'
#' @examples
#' \dontrun{fars_read_years(2013)}
#'
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' Summarize number per month in a specified year
#'
#' This function summarizes "FARS" data per month in a given year, that
#' is specifid in the input
#'
#' @param years The input year for which to summarize the data
#'
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom tidyr spread
#'
#' @return This function returns a summary of the data grouped by months in
#'    a given year.
#'
#' @examples
#' \dontrun{fars_summarize_years(2013)}
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Plot a map of a givne state with distribution of accidents
#'
#' This function plots the geographical boundaries of a state and the distribution of reported accidents within the state.
#' State number and reporting years are required for the input.
#'
#' @param state.num The number of the state. If an invalid state number is chosen,
#'    an error message ("invalid STATE number: ") is thrown.
#' @param year The year. If there are no accidents in a given year and state,
#'    the message "no accidents to plot" is thrown.
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @return This function returns a map of the a given state, showing the distribution of accidents in a given year.
#'
#'  @examples
#' \dontrun{fars_map_state(18, 2013)}
#'
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}

