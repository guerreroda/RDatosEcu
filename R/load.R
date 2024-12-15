#' RDatosEcu
#' @title RDatosEcu
#' @name RDatosEcu
#' @description
#' Author: Diego Guerrero
#' \code{RDatosEcu} downloads selected data.
#' `RDatosEcu()` returns df with time series from the RDatosEcu github folder.
#'
#' @import readr
#' @import dplyr
#' @import curl
#' @import lubridate
#' @import openxlsx
#' @param Ticket: string with all tickets.
#' @param retry: connection attempts default is 10.
#' @param real: TRUE, FALSE, or base year. Transforms nominal variables into real. default is FALSE
#' @param export.path: specify path and file name with extension (csv of xlsx)
#' @returns DataFrame.
#' @examples
#' RDatosEcu("RGDP0000 UNTL1007")
#' RDatosEcu("RGDP0000 UNTL1007", retry=10)
#' RDatosEcu("RGDP0000 UNTL1007", retry=10, export.path="FileName.csv")
#' @export
RDatosEcu <- function(ticket, real=FALSE, retry = 10, export.path=FALSE) {
  main <- "http://raw.githubusercontent.com/guerreroda/PublicEcuador/refs/heads/main/files/"
  TARGET <- unlist(strsplit(ticket, " "))

  merged_data <- data.frame()

  for (t in 1:length(TARGET)) {
    # Download the data
    mydata <- get_data(TARGET[t] , max.attempts = retry)

    #If it is GDP/Quarterly, transform into last day of the quarter:
    if TARGET[t]=="RGDP0000"{
      mydata$date <- ceiling_date(mydata$date, "quarter") - days(1)
    }

    if (nrow(merged_data) == 0) {
      merged_data <- mydata
    } else {
      merged_data <- merge(merged_data, mydata, by = "date", all = TRUE)
    }
  }

  rm(main, TARGET)
  print("Data downloaded.")

  if (real!=FALSE){
    ###########################
    # Download the dictionary
    dict <- get_dict("dict", url="https://raw.githubusercontent.com/guerreroda/RDatosEcu/refs/heads/main/")

    # Select observations that are available in the data.
    dict <- dict %>%
      select(ticket, nominal)
    mycols <- intersect(dict$ticket, colnames(merged_data))
    dict <- dict %>%
      filter( ticket %in% mycols )  %>%
      bind_rows(tibble(ticket = "date", nominal = 1)) %>%
      bind_rows(tibble(ticket = "date", nominal = 0))

    #break merged_data into those that are nominal, and those that are real
    nominal_df <- merged_data %>%
      select(all_of(dict$ticket[dict$nominal == 1]))

    real_df <- merged_data %>%
      select(all_of(dict$ticket[dict$nominal == 0]))

    ###########################
    # Download CPI
    cpi <- get_data("PCPI0000")
    colnames(cpi)[colnames(cpi) == "PCPI0000"] <- "cpi"

    # now apply inflation adjustment
    adj_df <- adjust_inflation( nominal_df , cpi, base=real )

    # merge both df
    merged_data <- merge( real_df, adj_df, by = "date", all = TRUE)
  }

  if ( export.path != FALSE ) {
    save_file( merged_data, export.path)
  }
  return(merged_data)
}

#' @title get_data
#' @name get_data
#'
get_data <- function(
    FileName,
    url = "https://raw.githubusercontent.com/guerreroda/PublicEcuador/refs/heads/main/files/",
    max.attempts = 5
){

  name_file = paste0( FileName , ".csv")
  #print(name_file)
  GET <- paste0(
    url ,
    name_file
  )

  attempt <- 0
  success <- FALSE

  while(attempt < max.attempts && !success) {
    tryCatch({
      attempt <- attempt + 1
      h <- new_handle()
      handle_setopt( h , ssl_verifypeer = FALSE)
      myCsv <- curl_fetch_memory(GET, handle = h)
      success <- TRUE
    }, error = function(e) {
      message(paste("Connection attempt", attempt, "failed:", e$message))
      if (attempt <= max.attempts) {
        Sys.sleep(3)
      }
    })
  }

  csvContent <- rawToChar(myCsv$content)
  mydata <- read.csv(text = csvContent)
# mydata <- read.csv(textConnection(myCsv))
  mydata <- mydata[-nrow(mydata), ]

  mydata$date <- as.Date(mydata$date, format = "%Y-%m-%d")
  column_name <- FileName
  mydata[[column_name]] <- as.numeric(mydata[[column_name]])

  Sys.sleep(1)
  return(mydata)
}

#' @title get_dict
#' @name get_dict
#'
get_dict <- function(
    FileName,
    url = "https://raw.githubusercontent.com/guerreroda/PublicEcuador/refs/heads/main/files/"
){

  name_file = paste0( FileName , ".csv")
  #print(name_file)
  GET <- paste0(
    url ,
    name_file
  )

  #print(GET)
  #myCsv <- getURL(GET)
  h <- new_handle()
  handle_setopt( h , ssl_verifypeer = FALSE)
  myCsv <- curl_fetch_memory( GET, handle=h )
  csvContent <- rawToChar(myCsv$content)
  mydata <- read.csv(text = csvContent)
  #mydata <- read.csv(textConnection(myCsv))
  mydata <- mydata[-nrow(mydata), ]
  return(mydata)
}

#' @title save_file
#' @name save_file
#'
save_file <- function( df, file_path) {
  # Extract the file extension
  file_ext <- tools::file_ext(file_path)

  # Save based on the file extension
  if (file_ext == "csv") {
    write.csv(df, file = file_path, row.names = FALSE)
  } else if (file_ext == "xlsx") {
    write.xlsx(df, file = file_path)
  } else if (file_ext == "rds") {
    saveRDS(df, file = file_path)
  } else if (file_ext == "txt") {
    write.table(df, file = file_path, row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)
  } else {
    stop("Unsupported file extension. Please use '.csv', '.xlsx', '.rds', or '.txt'.")
  }
}

#' @title adjust_inflation
#' @name adjust_inflation
#' @description
#' Author: Diego Guerrero
#' adjust_inflation() transform series into real values
#' @param data: data frame with series to be adjusted.
#' @param index: "cpi", "ipp", or "gdp" deflactor
#' @returns DataFrame.
#' @examples
#' adjust_inflation(df, index="cpi"
#' adjust_inflation(df, index="cpi", base=2024)
#' adjust_inflation(df, index="gdp")
#' @export
adjust_inflation <- function( data, index, base = 1) {

  # Define CPI on base year
  if (base!=1){
    base = mean(index$cpi[format(index$date, "%Y") == base], na.rm = TRUE)
  }
  index$adjust <- base/index$cpi

  # Merge by date column
  merged_data <- merge( data , index , by = "date")

  # Apply adjustment only to selected columns
  target_cols <- setdiff(colnames(merged_data), c("date", "adjust"))
  merged_data[ , target_cols ] <- merged_data[ , target_cols ] * merged_data$adjust

  # Keep only original columns
  merged_data <- merged_data[, names(data)]
  return(merged_data)
}


#' @title flatten_data
#' @name flatten_data
#' @description
#' Author: Diego Guerrero
#' flatten_data() transforms data shape to columns per lag.
#' @param data: data frame with series to be adjusted.
#' @param lags: number of lags to introduce.
#' @param exclude: exclude columns
#' @returns DataFrame.
#' @examples
#' TBD
#' @export
flatten_data <- function( data, lags, exclude) {
}
