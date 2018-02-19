year = NULL
STATE = NULL
MONTH = NULL
n = NULL

#' Read Data
#'
#' This function reads data in compressed .csv format and creates a data frame. It throws an error if the filename in the argument does not exist.
#'
#' @param filename A character string giving the filename of the data
#'
#' @return This function returns a data frame
#'
#' @examples
#' \dontrun{fars_read("Data.csv.bz2")}
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
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
#'
#' Make filenames
#'
#' This function creates filenames with extensions with the year input
#'
#' @param year An integer giving the year
#'
#' @return This function returns a string with the year
#'
#' @examples
#' \dontrun{make_filename(2013)}
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}
#' Extract a subset of data
#'
#' This function creates a data list with the month and year columns for each year in the input
#'
#' @param years A list of years
#'
#' @return This function returns a list of data frames for each year in the input and none if there is no data available for a year. If there is no data available for a year, the function throws a warning.
#'
#' @examples
#' \dontrun{fars_read_years(list(2013,2014,2015))}
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(system.file(file))
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}
#' Data operations
#'
#' This function groups data by year and month and summarizes the total number of accidents for each year
#'
#' @param years A list of years
#'
#' @return This returns a data frame with each column for each year in the input argument and the corresponding number of accidents for that year.
#'
#' @examples
#' \dontrun{fars_summarize_years(list(2013,2014,2015))}
#'
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom tidyr spread
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}
#' Data plotting
#'
#' This function plots the accidents for the state provided in the input on the map. The function throws an error if the state number in the input argument does not exist in the fars data.If there are no accidents reported for the state of interest, a message is displayed.
#'
#' @param state.num An integer representing the corresponding state
#' @param year An integer representing the year for which the plot is made
#'
#' @return This returns a map with the accidents plotted for the state in the input
#'
#' @examples
#' \dontrun{fars_map_state(3,2013)}
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
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
