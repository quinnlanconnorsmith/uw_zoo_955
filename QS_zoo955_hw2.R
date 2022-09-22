#Quinn and Ian ZOO955 Homework number 2

library(tidyverse)

view(iris)
#This dataset has sepal width, sepal length, petal width, and petal length 

# 1. filter
# filter iris data by each species 
setosa <- filter(iris, Species == "setosa")
versicolor <- filter(iris, Species == "versicolor")
virginica <- filter(iris, Species == "virginica")

# 2. mutate
# create 2 new columns in iris data that is petal/sepal length x width
#Remember that you now need to call 'iris_area' in future code 
iris_area <- mutate(
  iris, 
  petal.area = Petal.Length * Petal.Width,
  sepal.area = Sepal.Length * Sepal.Width
)

view(iris_area)

# plot petal area ~ length - is the relationship linear? Why?
#This relationship is not linear, most likely because we're plotting 3 different species with different ratios of petal length/area
ggplot(data = iris_area, aes(x = Petal.Length, y = petal.area)) + geom_point()

# 3. summarize
# compute the mean petal length of each species dataset from above
setosa_mean <- mean(setosa$Petal.Length)
versicolor_mean <- mean(versicolor$Petal.Length)
virginica_mean <- mean(virginica$Petal.Length)

# now do it using summarize
#This gives the same value, but is a 'dataset' instead of a value 
setosa_summarize_mean <- summarize(setosa, mean.petal.length = mean(Petal.Length))
versicolor_summarize_mean <- summarize(versicolor, mean.petal.length = mean(Petal.Length))
virginica_summarize_mean <- summarize(virginica, mean.petal.length = mean(Petal.Length))

# 4. group by
#With a little extra work, we can create a dataset with the mean values 
# we can do the above summarize so much easier when combined with group_by
iris_means <- summarize(group_by(iris, Species), mn.petal.length = mean(Petal.Length))

# 5. pipes
# the above can get unwieldy - rearrange iris_means from 4 using pipes
#I've added a count to this as well

iris_means2 <- iris_area %>% 
  group_by(Species) %>% 
  summarise(
    count = n(),
    mean.petal.length = mean(Petal.Length, na.rm = TRUE)
  ) 


## On Your Own #1 
# now compute mean petal area for each species - how would you go about it using dplyr
iris_petal_means2 <- iris_area %>% 
  group_by(Species) %>% 
  summarise(
    count = n(),
    mean.petal.length = mean(petal.area, na.rm = TRUE)
  ) 
view(iris_petal_means2)
# Q: What is the mean petal area for each species

#Setosa - 0.3656
#Versicolor - 5.7204
#Virginica - 11.2962

# 6. arrange/select/count
# determine which species has the longest mean petal length
#Virginica!
iris_size <- 
  iris %>% 
  select(Species, Petal.Length) %>% # only selects these 2 columns
  group_by(Species) %>%
  summarize(mean.petal.length = mean(Petal.Length)) %>%
  arrange(desc(mean.petal.length))

# On Your Own #2
# do the same for the other measurements (i.e. petal.width, sepal.length, etc)
# Q: What is the mean petal and sepal lengths and widths for each species
#I think we can do this all at once
#Answers should be shown after running view()

iris_sp_means <-
  iris %>%
  group_by(Species) %>%
  summarize(
    mean.petal.length = mean(Petal.Length),
    mean.sepal.length = mean(Sepal.Length),
    mean.petal.width = mean(Petal.Width),
    mean.sepal.width = mean(Sepal.Width)
    )
view(iris_sp_means)

# count the number of records for each species
(iris_spp_n <- count(iris, Species))
#You can also add this in the summarize! 

# On Your Own #3
# count the number of samples that are >= mean.petal.length for each species
# Q: How many samples where Petal.Length >= mean.petal.length does each species have
#Answer should be shown after running view()

iris_means_3 <- 
  iris_area %>%
  group_by(Species) %>%
  mutate(mean.petal.length = mean(Petal.Length)) %>%
  filter(Petal.Length >= mean.petal.length) %>%
  count()

view(iris_means_3)

# 7. joins
#I believe these are random each time you run this 
set.seed(123)
ht <- data.frame(level = LETTERS[1:5], height = sample(40:80, 5, replace = TRUE))
wt <- data.frame(level = LETTERS[1:6], weight = sample(50:500, 6, replace = TRUE))
#We have differing sample sizes

# join together height and weight by level
# what happens when you reverse ht and wt (i.e. put wt first, then ht)
ht_wt <- left_join(ht, wt, by = "level")
ht_wt <- left_join(wt, ht, by = "level")
#We lose sample F unless we put wt first!  

# On Your Own #4 - Extra Credit
# work with the nycflights13 data set to determine what airport had the 
#     most departing flights in 2013
# must use combination of dplyr verbs
# data.frames are airports, flights, planes and weather
# HINT: faa column in airports links to origin column in flights
library(nycflights13)

# Q1: Which airport had the most departing flights in 2013? 
# Q2: Which airport (name) had the greatest number of arriving flights in 2013?
# Q3: Which airport (name) had the greatest number of delayed arriving flights?
# Q4: What is the manufacturer, model, year, and type of airplane that flew the 
#    most flights in 2013 (only include planes with all relevant information)?
#    How man flights was it?

view(flights)
view(airports)
#This takes a long time to compute 

nycflights13::flights %>% 
  View()


#Q1 Newark Liberty International has the most departing flights 
flights_depart <- flights %>% 
count(origin) %>% 
arrange(desc(n))

#You need the info from 'airports' for more information about the airport!
flights_depart <- flights %>% 
  count(origin) %>% 
  arrange(desc(n)) %>% 
  left_join(airports, by = c("origin" = "faa")) 
view(flights_depart)

flights_depart$name[1]

#Q2 Chicago O'hare has the most flights arriving 

flights_arrive <- flights %>% 
  count(dest) %>% 
  arrange(desc(n)) %>% 
  left_join(airports, by = c("dest" = "faa")) 

flights_arrive$name[1]

view(flights_arrive)

#Q3 Hartsfield Jackson Atlanta International airport has the most flights with arrival delays 

flights_delay <- flights %>% 
  filter(!is.na(arr_delay >0)) %>%
  count(dest) %>% 
  arrange(desc(n)) %>%
  left_join(airports, by = c("dest" = "faa"))

# I also added the total & mean time delay for each airport. ATL still has the most hours of delay, but Columbia Metropolitan has the longest average delays

flights_delay2 <- flights %>% 
  filter(!is.na(arr_delay >0)) %>%
  group_by(dest) %>% 
  summarize(mn.delay = mean(arr_delay),
            sum.delay = sum(arr_delay)) %>% 
  left_join(flights_delay, by = "dest")

#Q4 Tailnumber N711MQ, Manufacturer Gulfstream Aerospace, Model G1159B, Year 1976, Type Fixed wing multi engine, 486 Flights 
#Only include the closest plane to the top with all the details 
#You must use left_join first, I tired to use it at the tail end and it wouldn't work 
flights_most <- flights %>% 
  left_join(planes, by = "tailnum") %>%
  count(tailnum, manufacturer, model, year.y, type) %>%
  arrange(desc(n))
view(flights_most)   
