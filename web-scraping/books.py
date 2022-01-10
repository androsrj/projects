# I use the URL (below) from goodreads.com to
# web scrape the first page (100 books) of top-ranked young adult fiction novels.
# The final data frame contains each book's title, author, score, and number of votes.

# Bring in libraries
import pandas as pd
import re
from bs4 import BeautifulSoup
import requests

# Set URL and use the BeautifulSoup module to read in the HTML code
url = "https://www.goodreads.com/list/show/141.Best_Young_Adult_Fiction"
page = requests.get(url)
soup = BeautifulSoup(page.text,"html.parser")

# Split up HTML code by each book (length of books list is 100)
books = soup.find_all('tr', attrs={'itemtype':"http://schema.org/Book"})

# Initialize an empty vector of length n for each variable
n = len(books)
titles = [' '] * n
authors = titles.copy()
scores = titles.copy()
votes = titles.copy()

# Iterate through the books list and pull out each variable
for i in range(n):
   titles[i] = books[i].find_all('a')[1].get_text() 
   authors[i] = books[i].find_all('a')[2].get_text()
   scores[i] = books[i].find_all('a')[3].get_text()
   votes[i] = books[i].find_all('a')[4].get_text()

   # Some of the variables need a bit of manipulation
   titles[i] = titles[i].replace('\n', ' ').replace('\r', '')    # Get rid of all the \n's in the titles
   scores[i] = scores[i].replace(',','')                         # Make the scores numeric and get rid of the word 'scores'
   scores[i] = re.findall('\d+', scores[i])[0]                   #
   votes[i] = votes[i].replace(',','')                           # Get rid of the 'voted for' phrase and make it numeric
   votes[i] = re.findall('\d+', votes[i])[0]

# Compile final dataset
books_final = pd.DataFrame(data = {'Book Title':titles, 'Author':authors, 'Score':scores, 'Votes':votes})

# Print final data without row numbers
print(books_final.to_string(index=False))
