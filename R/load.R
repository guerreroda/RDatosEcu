#' Author: Diego Guerrero
#' This function downloads from the RDatosEcu github folder.
#' Ticket: input a string with all tickets required
#' Real: transforms nominal variables into real.
#' Real takes arguments TRUE, FALSE, or a new base year. The default is FALSE.
#' Output: DataFrame
#' @import readr
#' @import RCurl
#' @import lubridate
#' @import dplyr
#' @examples
#' RDatosEcu("RGDP0000 UNTL1007")

#' @export
RDatosEcu <- function(ticket, real=FALSE) {
  main <- "http://raw.githubusercontent.com/guerreroda/PublicEcuador/refs/heads/main/files/"
  TARGET <- unlist(strsplit(ticket, " "))

  merged_data <- data.frame()

  for (t in 1:length(TARGET)) {
    mydata <- get_data(TARGET[t])

    if (nrow(merged_data) == 0) {
      merged_data <- mydata
    } else {
      merged_data <- merge(merged_data, mydata, by = "date", all = TRUE)
    }
    Sys.sleep(0.5)
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
    dict <- dict %>%
      filter( ticket %in% intersect(dict$ticket, colnames(merged_data)) )

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

  return(merged_data)
}

get_data <- function(
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
  myCsv <- getURL(GET)
  mydata <- read.csv(textConnection(myCsv))
  mydata <- mydata[-nrow(mydata), ]

  mydata$date <- as.Date(mydata$date, format = "%Y-%m-%d")
  column_name <- FileName
  mydata[[column_name]] <- as.numeric(mydata[[column_name]])

  return(mydata)
}


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
  myCsv <- getURL(GET)
  mydata <- read.csv(textConnection(myCsv))
  mydata <- mydata[-nrow(mydata), ]
  return(mydata)
}



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
  numeric_cols <- sapply( data , is.numeric)
  merged_data[ , numeric_cols ] <- merged_data[ , numeric_cols ] * merged_data$adjust

  # Keep only original columns
  merged_data <- merged_data[, names(data)]
  return(merged_data)
}

