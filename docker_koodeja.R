#aja ja mäppää
docker run -ti -p 80:3838 -v C:/Users/Lauri/Documents/R/mstat2:/srv/shiny-server/app beepr:latest
docker run -ti -p 80:3838 -v C:/Users/Lauri/Documents/R/stat_client:/srv/shiny-server/app -v C:/Users/Lauri/Documents/R/mstat2:/srv/shiny-server/srv beepr:latest

#join existing running image
docker exec -it reverent_meninsky bash
