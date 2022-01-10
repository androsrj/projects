# This script webscrapes the Ted Williams data 
# from Basebal References, filters out the seasons he didn't play (due to WWII),
# and keeps only the year, games, and batting average columns.

# Import pandas for the html reader and dataframe functions
import pandas as pd

# Read in HTML data
url = "https://www.baseball-reference.com/players/w/willite01-bat.shtml"
rawdata = pd.read_html(url)[0]

# Cleanup and remove columns
removenull = rawdata.loc[rawdata["Age"].notna(),['Year','G','BA']]
final = removenull.iloc[0:22,:]

# Print without row numbers
print(final.to_string(index=False))

