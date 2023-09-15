# This function downloads data from FRED. It returns quarterly data.
# User must provide the FRED url.
"getFRED" <- function(url, freq = "Quarterly") {
  # Download the data from FRED
  # download.file(url, destfile = 'FREDtemp.txt', method = "wget")
  # FREDraw <- readLines('FREDtemp.txt')
  txt.file.name <- paste0("rawData/", substr(url, regexpr("[a-zA-z0-9]*.txt", url), 1000))
  if (!file.exists(txt.file.name)) {
    # Download the data from FRED
    # download.file(url, destfile = 'FREDtemp.txt', method = "wget")
    system(paste0('wget --no-check-certificate "', url, '"'))
    system(paste("mv", substr(url, regexpr("[a-zA-z0-9]*.txt", url), 1000), txt.file.name))
  }

  FREDraw <- readLines(txt.file.name)

  # Frequency
  freq.FRED <- gsub(" ", "", substr(
    FREDraw[which(regexpr("Frequency", FREDraw) == 1)],
    (nchar("Frequency") + 2), 100
  ))

  # Where does the data start
  datastart <- which(gsub(" ", "", FREDraw) == "DATEVALUE") - 2

  # data <- read.table('FREDtemp.txt', skip = datastart, header = TRUE)
  data <- read.table(txt.file.name, skip = datastart, header = TRUE)

  first.year <- as.numeric(format(as.Date(data$DATE[1]), "%Y"))

  first.month <- as.numeric(format(as.Date(data$DATE[1]), "%m"))

  #  Adjust frequency
  if (freq.FRED == "Quarterly") {
    first.q <- (first.month - 1) / 3 + 1
    data.tis <- tis(data$VALUE, start = c(first.year, first.q), tif = "quarterly")
  } else if (freq.FRED == "Monthly") {
    data.tis <- tis(data$VALUE, start = c(first.year, first.month), tif = "monthly")
  }

  # Convert frequency

  if (freq.FRED == "Monthly" & freq == "Quarterly") {
    data.tis <- convert(data.tis, tif = "quarterly", method = "constant", observed. = "averaged")
  }

  return(data.tis)
}

# END