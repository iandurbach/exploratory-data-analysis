# Write a function that given your birthday (as a date), 
# returns how old you are in years.

age <- function(bday) {
  lubridate::interval(bday, today()) / years(1)
}
age(ymd("1979-05-23"))
