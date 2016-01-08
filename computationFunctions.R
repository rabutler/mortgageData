# nMonths is number of months to look back, and xx is the time series data
# returns range, so biggest drop in nMonths, and biggest increase in nMonths
findMaxChange <- function(nMonths, xx)
{
  zz <- xx[(nMonths + 1):length(xx)] - xx[1:(length(xx) - nMonths)]
  range(zz)
}

findChange <- function(nMonths, xx)
{
  data.frame(value = xx[(nMonths + 1):length(xx)] - xx[1:(length(xx) - nMonths)], 
             nMonths = nMonths)
}