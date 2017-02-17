from bs4 import BeautifulSoup
from urllib2 import urlopen
from pyspark import SparkContext
import pandas as pd
sc = SparkContext("local", "Simple App")

if __name__=="__main__":
	local=[]
	for key in ["data+scientist","data+analyst"]:
		for i in range(20):
			url="https://ca.indeed.com/jobs?q=%s&l=canada&start=%d"%(key,i*10)
			r=urlopen(url)
			soup=BeautifulSoup(r)
			tmp=soup.find_all('span',attrs={'itemprop':'addressLocality'})
			for j in range(len(tmp)):
					local.append(tmp[j].text.split(',')[0])
			if i%4==0:
				print "Process: "+key
				print "<"+"=="*(i/4)+">"
				print "<"+"=="*5+">"
	results = sc.parallelize(local).map(lambda x: (x,1)).reduceByKey(lambda x,y:x+y)
	top10=results.sortBy(lambda x: x[1],ascending=False).take(10)
	print top10
	sc.stop()