# Summary

A simple time series analysis, in R, of the Great Salt Lake water levels.  Data is from the USGS National Water Information System.

The data shows statistically significant autocorrelation of lake water levels with a 12-13 year period, since the mid-1980s.  Potential causes other than random variation are unclear.

# Sources

## USGS National Water Information System
Accessed 2024-05-24

### North GSL: Saline, UT (Site 10010100)
https://waterdata.usgs.gov/nwis/dv?referred_module=sw&site_no=10010100
https://waterdata.usgs.gov/nwis/dv?cb_62614=on&format=rdb&site_no=10010100&legacy=&referred_module=sw&period=&begin_date=1966-04-15&end_date=2024-05-23

### South GSL: Saltair Boat Harbor (site 10010000)
https://waterdata.usgs.gov/nwis/dv?referred_module=sw&site_no=10010000
https://waterdata.usgs.gov/nwis/dv?cb_62614=on&format=rdb&site_no=10010000&legacy=&referred_module=sw&period=&begin_date=1847-10-18&end_date=2024-05-23

### Other
hat tip: http://www.nytimes.com/2024/05/16/climate/great-salt-lake-water-levels.html

# Results

Plot of Great Salt Lake water level since 1867

Plot of Great Salt Lake water level since 1966 (Modern daily record keeping for northern and southern portions)

Autocorrelation plot (lag in years vs. correlation)

Partial autocorrelation plot

Note, augmented Dickey-Fuller tests suggest mean reversion tendencies for most recent time series windows.

# Statement of Originality

This code is an original product of the author (a human).  AI models were NOT used in the development of this code.
