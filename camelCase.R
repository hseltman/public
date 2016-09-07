# Howard Seltman, July 2016
# Function to make camel case from a string vector
# Result is like https://en.wikipedia.org/wiki/CamelCase.
# Spaces or punctuation (e.g., "." or "_") are dropped and the next character is made upper case.
# The first character is always lower case.

# Arguments:
#  sv: a string vector with spaces or punctuation between words
#  upper: default to FALSE; TRUE makes first work upper case, too
# Output:
#  a string vector in camelcase
#
camelCase = function(sv, upper=FALSE, capIsNew=FALSE, alreadyTrimmed=FALSE) {
  if (!is.character(sv)) stop("'sv' must be a string vector")
  if (!alreadyTrimmed) sv = gsub("[[:space:]]*$", "", gsub("^[[:space:]]*", "", sv))
  if (capIsNew) sv = gsub("([A-Z])", " \\1", sv)
  apart = strsplit(sv, split="[[:space:][:punct:]]")
  apart = lapply(apart, tolower)
  capitalize = function(x) paste0(toupper(substring(x,1,1)), substring(x,2))
  if (upper) {
    apart = lapply(apart, capitalize)
  } else {
    apart = lapply(apart, function(x) c(x[1], capitalize(x[-1])))
  }
  return(sapply(apart, paste, collapse=""))
}

# Optional testing
if (exists("testingCamelCase")) {
  vec = c("Oh, Here I am,  John", "Oh, that_is_OK, John", "Oh, fine, John")
  print(camelCase(vec))
  if (!exists("dropSame")) source("dropFrontSame.R")
  print(camelCase(dropSame(vec)))
  print(camelCase(dropSame(vec), upper=TRUE))
  vec2 = c("OhHereIAm", "ThatIsOK", "fine_with-me")
  print(camelCase(vec2))
  print(camelCase(vec2, capIsNew=TRUE))
  vec3 = c(" all_three", "  two-to-1 ", "terminal ", "termi inal   ")
  print(camelCase(vec3))
  print(camelCase(vec3, alreadyTrimmed=TRUE))
}
