import re
import html
import requests
import os

genres = ['action', 'romance']
nMoviesPages = 20 # 50 movies per page (20*50==1000 movies)
nReviews = 100 # 10 reviews per page (100*10==1000 reviews/movie)

for genre in genres:
#    print(genre)
    p = 1
    r = requests.get('http://www.imdb.com/search/title?genres=%s&title_type=feature&sort=moviemeter,asc&page=%d&ref_=adv_nxt' % (genre, p))
    if r.status_code != 200:
        break
    movies = set(re.findall('href=\"/title/(.*?)/', r.text.replace('\n',''), re.IGNORECASE))
#    print(movies)
    if not os.path.exists(genre):
        os.makedirs(genre)
    for movie in movies:
#        print(movie)
        i = 0
        reviews = []
        while True:
            r = requests.get('http://www.imdb.com/title/%s/reviews?start=%d' % (movie, i))
            if r.status_code != 200:
                break
            reviews = re.findall('</div><p>(.*?)</p>', r.text.replace('\n',''), re.IGNORECASE)
            if len(reviews) == 0:
                break
            n = 0
            for review in reviews:
                with open("%s/%s_%s.txt" % (genre, movie, i+n), 'w+') as file:
                    file.write(html.unescape(re.sub('<[^<]+?>', '', review)).encode(errors='replace').decode(errors='ignore'))
                n += 1
            i += 10
            if i == nReviews:
                break
    p += 1
    if p == nMoviesPages:
        continue
