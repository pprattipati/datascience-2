# %d = Day
# %m = numeric month
# %b = abbreviated month
# %B = full month
# %y = 2-digit year
# %Y = 4-digit year

dates1 = c('1/10/1998', '4/25/2006')
class(dates1)

dates1_std = as.Date(dates, format="%m/%d/%Y") # stating the format of our input
dates1_std
class(dates1_std)

dates2 = c('10-1-98', '25-4-06')
dates2_std = as.Date(dates2, format="%d-%m-%y") # stating the format of our input
dates2_std

# get the weekday of a date
weekdays(dates1_std)
# get the month of a date
months(dates1_std)

# difference in days between two dates
dates1_std[1] - dates1_std[2]

# current date
Sys.Date()

# Extract day, month, year from Date
dates1_std
