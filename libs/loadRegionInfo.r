regions = "outputs/full_biome_realms.nc"

region_names =  c("Australasia\nTropical Forest",        
                 "South East\nAustralia Woodland",
                 "Australia\nSavanna/grassland",
                 "Australia\nMediterranean",
                 "Australia\nDesert/shrubland",
                 "Southern\nAfrica Tropical Forest",
                 "Southern\nAfrica Savanna/grassland",
                 "Southern\nAfrica Desert",
                 "Northern\nAfrica Tropical Forests",
                 "Northern\nAfrica Savanna/grassland",
                 "Northern\nAfrica Desert",
                 "Indo-Malay\nTropical Forest",
                 "Indo-Malay\nDry tropical forest",
                 "Southern\nAmerica Tropical Forest",
                 "Southern\nAmerica Savanna/grassland",
                 "Southern\nAmerica Desert",
                 "Northern\nAmerica Tropical forest",
                 "Northern\nAmerica temperate woodland",
                 "Northern\nAmerica Savanna/grassland",
                 "Northern\nAmerica Desert",
                 "Northern\nAmerica Boreal Forest",
                 "Northern\nAmerica tundra",
                 "Eurasia\nTemperate forest/woodland",
                 "Eurasia\nparkland/grass",
                 "Mediterranean",
                 "Eurasia\nDesert/scrub",
                 "Eurasian\nBoreal forest",
                 "Eurasian\nTundra")



vegTypes = c("Tropical evergreen forest/wood",  #1
             "Seasonal/temperate forest/wood",  #2 
             "Savanna/Parkland/grassland",      #3
             "Mediterranean",                   #4       
             "Desert/shrubland",                #5
             "Boreal Forest",                   #6
             "Tundra")                          #7

region_vegTypes = c("Australasia\nTropical Forest" = 1,        
                    "South East\nAustralia Woodland" = 2,
                    "Australia\nSavanna/grassland" = 3,
                    "Australia\nMediterranean" = 4,
                    "Australia\nDesert/shrubland" = 5,
                    "Southern\nAfrica Tropical Forest" = 1,
                 "Southern\nAfrica Savanna/grassland" = 3,
                 "Southern\nAfrica Desert" = 5,
                 "Northern\nAfrica Tropical Forests" = 1,
                 "Northern\nAfrica Savanna/grassland" = 3,
                 "Northern\nAfrica Desert" = 5,
                 "Indo-Malay\nTropical Forest" = 1,
                 "Indo-Malay\nDry tropical forest" = 2,
                 "Southern\nAmerica Tropical Forest" = 1,
                 "Southern\nAmerica Savanna/grassland" =3,
                 "Southern\nAmerica Desert" = 5,
                 "Northern\nAmerica Tropical forest" = 1,
                 "Northern\nAmerica temperate woodland" = 2,
                 "Northern\nAmerica Savanna/grassland" = 3,
                 "Northern\nAmerica Desert" = 5,
                 "Northern\nAmerica Boreal Forest" = 6,
                 "Northern\nAmerica tundra" = 7,
                 "Eurasia\nTemperate forest/woodland" = 2,
                 "Eurasia\nparkland/grass" = 3,
                 "Mediterranean" = 4,
                 "Eurasia\nDesert/scrub" = 5,
                 "Eurasian\nBoreal forest" = 6,
                 "Eurasian\nTundra" = 7)

regions = raster(regions)
