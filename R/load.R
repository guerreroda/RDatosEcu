#' Author: Diego Guerrero
#' This function downloads from the RDatosEcu github folder.
#' Ticket: input a string with all tickets required
#' Real: transforms nominal variables into real. TRUE, FALSE, or base year. Default is FALSE.
#' Output: DataFrame file
#' @export
#' @import readr
#' @import zoo
#' @import RCurl
#' @import lubridate
#' @examples
#' RDatosEcu("RGDP0000 UNTL1007")


RDatosEcu <- function(ticket, real=FALSE) {
  main <- "https://raw.githubusercontent.com/guerreroda/PublicEcuador/refs/heads/main/files/"
  TARGET <- unlist(strsplit(ticket, " "))

  merged_data <- data.frame()

  for (t in 1:length(TARGET)) {

    mydata <- get_data(TARGET[t])

    if (nrow(merged_data) == 0) {
      merged_data <- mydata
    } else {
      merged_data <- merge(merged_data, mydata, by = "date", all = TRUE)
    }
    Sys.sleep(0.1)
  }

  rm(main, TARGET)
  print("Data downloaded.")

  if (real!=FALSE){
    cpi <- get_data("PIND0000")
    colnames(cpi)[colnames(cpi) == "PIND0000"] <- "cpi"

    #break merged_data into those that are nominal, and those that are real

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



adjust_inflation <- function( data, index, base = 1) {

  # Define CPI on base year
  if (base!=1){
    base = mean(index$cpi[format(index$date, "%Y") == base], na.rm = TRUE)
  }
  index$adjust <- base/index$cpi

  # Merge by date column
  merged_data <- merge( data , index , by = "date")

  # Apply adjustment only to selected columnsgood
  numeric_cols <- sapply( data , is.numeric)
  merged_data[ , numeric_cols ] <- merged_data[ , numeric_cols ] * merged_data$adjust

  # Keep only original columns
  merged_data <- merged_data[, names(data)]
  return(merged_data)
}

