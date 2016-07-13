for type in data fare
do
	
    	for month in 1 2 3 4 5 6 7 8 9 10 11 12
        do
		url=https://nyctaxitrips.blob.core.windows.net/data/trip_${type}_${month}.csv.zip
	  	curl -O $url
		unzip trip_${type}_${month}.csv.zip
		rm trip_${type}_${month}.csv.zip
	done
done

