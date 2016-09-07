# readCsv() in readCsv.R
# Howard Seltman, August 2016
# Goal: read a csv file from the current directory, another absolute
#       directory, or a sub-directory; convert columns with less than
#       'maxCatToFactor' categories to a factor; and remove empty
#       rows and columns.
# INPUT:
#  fname: a filename
#  dir: location of file.  Use NULL for current directory, or
#       use an absolute or relative directory.
#  maxCatToFactor: maximum number of categories for any column
#       to consider it to be categorical and therefore converted
#       to a factor (only applies to character columns)
#  ...: other options for read.csv()
#
# OUTPUT:
#  a data frame
#
# DETAILS:
#   Front and back whitespace are trimmed from character columns.
#   Blank strings are converted to NA.
#   Only then are the number of non-NA categories counted to compare
#   to 'maxCatToFactor'.

readCsv = function(fname, dir=NULL, maxCatToFactor=5, ...) {
  # construct full file name from 'fname' and 'dir'
  if (is.null(dir)) {
    fname = file.path(getwd(), fname)
  } else {
    # determine if 'dir' is absolute or relative
    absPath = regexpr(":", dir, fixed=TRUE) != -1 ||
              regexpr("^[\\/]", dir) != -1
    if (absPath) {
      fname = file.path(dir, fname)
    } else {
      fname = file.path(getwd(), dir, fname)
    }
  }
  
  # Verify file exists (show full location if not)
  if (file.exists(fname) == FALSE) {
    stop(fname, " not found")
  }
  
  # Try to read the file as a csv file
  dtf = try(read.csv(fname, as.is=TRUE, ...))
  if (is(dtf, "try-error")) {
    stop(fname, " is not in a valid csv format")
  }
  
  # Fix-up leading and trailing blanks and convert "" to NA
  charCol = sapply(dtf,  is.character)
  if (any(charCol)) {
    for (ii in which(charCol)) {
      dtf[, ii] = trimws(dtf[, ii])
      dtf[!is.na(dtf[, ii]) & nchar(dtf[, ii])==0, ii] = NA
    }
  }

  # Remove any blank rows
  blank = apply(dtf, MARGIN=1, function(x) all(is.na(x)))
  dtf = dtf[!blank, , drop=FALSE]
  
  # Remove any blank columns
  blank = apply(dtf, MARGIN=2, function(x) all(is.na(x)))
  dtf = dtf[, !blank, drop=FALSE]

  # Convert columns to factor if appropriate
  charCol = apply(dtf,  MARGIN=2, is.character)
  if (any(charCol)) {
    catCount = apply(dtf[, charCol, drop=FALSE],  MARGIN=2, 
                     function(x) length(table(x)))
    toFactor = names(catCount)[catCount <= maxCatToFactor]
    for (ii in seq_along(toFactor)) {
      dtf[, toFactor[ii]] = factor(dtf[, toFactor[ii]])
    }
  }

  return(dtf)
}

if (exists("testingReadCsv")) {
  # Check error messages
  try(readCsv("nothing.csv"))
  try(readCsv("nothing.csv", dir="c:"))
  try(readCsv("nothing.csv", dir="sub"))
  try(readCsv("readCsv.R"))
  
  # Check various functionalities
  cat("\nNo missing data, 2 factors, 8x4\n")
  str(readCsv("basic.csv"))
  cat("\n2 factors, 7x4\n")
  str(readCsv("twoFactors7x4.csv"))
  cat("\n2 factors, 6x3 ('name' is rowname)\n")
  str(readCsv("twoFactors6x3.csv"))
  cat("\n1 factor, 6x3\n")
  str(readCsv("twoFactors6x3.csv", maxCatToFactor=2))
  cat("\nOne column\n")
  str(readCsv("oneColumn.csv"))
  cat("\nNo blanks\n")
  str(readCsv("noBlanks.csv"))
  cat("\nNo character columns\n")
  str(readCsv("noChar.csv"))
}
