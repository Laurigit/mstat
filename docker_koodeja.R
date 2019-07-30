

docker network create  my_network
#aja ja mäppää
docker run -ti -p 80:3838 -v C:/Users/Lauri/Documents/R/mstat2:/srv/shiny-server/app beepr:latest
docker run -ti -p 80:3838 -v C:/Users/Lauri/Documents/R/combine/betmtg:/srv/shiny-server/betmtg -v C:/Users/Lauri/Documents/R/combine/stat_client:/srv/shiny-server/app -v C:/Users/Lauri/Documents/R/combine/mstat2:/srv/shiny-server/srv -v C:/Users/Lauri/Documents/R/combine/common_data:/srv/shiny-server/common_data --network my_network beepr:latest


docker run -ti -p 80:3838 -link mysql -v /betmtg:/srv/shiny-server/betmtg -v /client:/srv/shiny-server/app -v /mstat:/srv/shiny-server/srv -v /common_data:/srv/shiny-server/common_data beepr:latest


#network
sudo docker run -ti -p 80:3838 -v /overlay:/srv/shiny-server/overlay -v /betmtg:/srv/shiny-server/betmtg -v /client:/srv/shiny-server/app -v /mstat:/srv/shiny-server/srv -v /common_data:/srv/shiny-server/common_data --network my_network beepr:latest
docker run -ti -p 80:3838  beepr:latest
docker run -ti -p 80:3838 -v /overlay:/srv/shiny-server/overlay -v /betmtg:/srv/shiny-server/betmtg -v /client:/srv/shiny-server/app -v /mstat:/srv/shiny-server/srv -v /common_data:/srv/shiny-server/common_data --network my_network beepr:latest
docker run -ti -p 80:3838 -v /mstat:/srv/shiny-server/srv  --network my_network beepr:latest


#local network
docker run -ti -p 80:3838 -v C:/Users/Lauri/Documents/R/combine/overlay:/srv/shiny-server/overlay -v C:/Users/Lauri/Documents/R/combine/betmtg:/srv/shiny-server/betmtg -v C:/Users/Lauri/Documents/R/combine/stat_client:/srv/shiny-server/app -v C:/Users/Lauri/Documents/R/combine/mstat2:/srv/shiny-server/srv -v C:/Users/Lauri/Documents/R/combine/common_data:/srv/shiny-server/common_data --network my_network beepr:latest

C:/Users/Lauri/Documents/R/combine
#join existing running image
docker exec -it reverent_meninsky bash


#buildaa
docker build -t beepr .


#start mysql
docker-compose run --service-ports db


#copy. pitää tehdä kansiossa C:\Users\Lauri> cd .\Documents\
scp -i "betmtg_europe.pem" C:\Users\Lauri\Documents\R\mysql\conf\my.cnf ubuntu@ec2-13-53-105-10.eu-north-1.compute.amazonaws.com:/mysql/conf
