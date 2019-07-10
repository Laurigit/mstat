#aja ja mäppää
docker run -ti -p 80:3838 -v C:/Users/Lauri/Documents/R/mstat2:/srv/shiny-server/app beepr:latest
docker run -ti -p 80:3838 -v C:/Users/Lauri/Documents/R/combine/stat_client:/srv/shiny-server/app -v C:/Users/Lauri/Documents/R/combine/mstat2:/srv/shiny-server/srv -v C:/Users/Lauri/Documents/R/combine/common_data:/srv/shiny-server/common_data beepr:latest

#join existing running image
docker exec -it reverent_meninsky bash


#buildaa
docker build -t beepr .
