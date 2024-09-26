# Author: Diego Guerrero
# This function downloads from the RDatosEcu github folder
# Ticket: input a string with all tickets required
# Output: DataFrame file

RDatosEcu <- function(ticket, bulk_file=1) {
  require(readr)
  require(zoo)
  require(RCurl)
  main <- "https://raw.githubusercontent.com/guerreroda/PublicEcuador/refs/heads/main/files/"
  TARGET <- unlist(strsplit(ticket, " "))
  merged_data <- data.frame()

  for (t in 1:length(TARGET)) {

    name_file = paste0(TARGET[t], ".csv")
    #print(name_file)
    GET <- paste0(
      main ,
      name_file
    )

    #print(GET)
    myCsv <- getURL(GET)
    mydata <- read.csv(textConnection(myCsv))

    mydata <- mydata[-nrow(mydata), ]
    mydata$date <- as.yearmon(mydata$date, format = "%Y-%m-%d")
    column_name <- TARGET[t]
    mydata[[column_name]] <- as.numeric(mydata[[column_name]])

    #if (bulk_file==1) {
    if (nrow(merged_data) == 0) {
      merged_data <- mydata
    } else {
      merged_data <- merge(merged_data, mydata, by = "date", all = TRUE)
    }
    #else {
    #assign(TARGET[t], mydata)
    #}

    rm(GET, myCsv, t, name_file, mydata, column_name)
    Sys.sleep(0.1)
  }

  rm(main, TARGET)
  #if (bulk_file==1) {
  print("Data downloaded.")
  return(merged_data)
  #}
}
