
library(ggplot2)
ny = read.csv('new_york_city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')



head(ny)

head(wash)

head(chi)

#adding a city column to each table so they will be identifiable when joined in one dataset
chi$City <- 'Chicago'
ny$City <- 'New York'
wash$City <- 'Washington'
#adding missing columns with null values to Washington data set for consistency when joining
wash$Gender <- NA
wash$Birth.Year <- NA

# combining the 3 datasets into one known as city_data
city_data <- rbind(ny, wash)
city_data <- rbind(city_data, chi)
head(city_data)



# Convert Trip.Duration to minutes
city_data$Trip.Duration_Minutes <- city_data$Trip.Duration / 60

# Plot the bar chart for average trip duration.
ggplot(city_data, aes(x = City, y = Trip.Duration_Minutes)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge", fill = "blue") +
  labs(title = "Average Trip Duration by City",
       x = "City",
       y = "Average Trip Duration (minutes)")




# Calculate and display average trip duration for each city.
trip_summary <- with(city_data, aggregate(list(Trip.Duration), by = list(City), 
                                   FUN = function(x) mean(x / 60, na.rm = TRUE)))
colnames(trip_summary) <- c('City', 'Average_Trip_Duration_Minutes')

trip_summary


                                        


# creating dataframe composed of only Chicago and New York called 'two_city'
two_city <- rbind(chi, ny)
head(two_city)

summary(two_city) 
#inspecting new dataframe, we see a 'mystery' count that is untitled under gender.

# Filtering gender column to only male and female
filtered_gen <- two_city[two_city$Gender %in% c("Female", "Male"), ]

#plotting bar chart
ggplot(aes(x = City, fill = as.factor(Gender)), data = filtered_gen) +
  geom_bar(position = 'dodge', colour = "black") +
  ggtitle('Riders by Gender') +
  scale_x_discrete(labels = c('Chicago', 'New York City')) +
  labs(y = 'Count of Riders', x = 'City') +
  scale_fill_manual(values = c("Female" = "blue", "Male" = "orange")) +
  guides(fill = guide_legend(title = "Gender"))


#Show total population of dataset broken down by gender(whole number and percent)
gen_total = sort(table(two_city$Gender))
print(gen_total)
round((gen_total / length(two_city$Gender) * 100), digits =2)

#Create function to calculate percent of riders per city by gender
gender_percentage <- function(data, city_column, gender_column, target_gender) {
 
  gender_table <- table(data[[city_column]], data[[gender_column]])

  gender_counts <- gender_table[, target_gender]

  total_counts <- rowSums(gender_table)

  percentage_data <- data.frame(
    City = names(gender_counts),
    Percentage_Target = (gender_counts / total_counts) * 100
  )

  return(percentage_data)
}

#Finding male rider percentage within each city
gender_percentage_data <- gender_percentage(two_city, "City", "Gender", "Male")
print(gender_percentage_data)

#Percentage of male riders below

#Using function to find female rider percentage within each city
gender_percentage_data <- gender_percentage(two_city, "City", "Gender", "Female")
print(gender_percentage_data)
#Percentage of female riders below

# inspecting data, we see another 'mystery' count under User.Type that is not subscriber/customer.
summary(city_data)

# Filter the data for 'Subscriber' and 'Customer'
bike_user_type <- city_data[city_data$User.Type %in% c("Subscriber", "Customer"), ]

#Plot data in bar chart
ggplot(aes(x = City, fill = as.factor(User.Type)), data = bike_user_type) +
  geom_bar(position = 'dodge', colour = "black") +
  ggtitle('User Type by City') +
  labs(y = 'Count of Users', x = 'City') +
  scale_fill_manual(values = c("Subscriber" = "blue", "Customer" = "orange")) +
  guides(fill = guide_legend(title = "User Type"))



#Create function to calculate percentage of subscribers/customers per city
calculate_percentage_user_type <- function(data, user_type) {
  
  type_data <- data[data$User.Type == user_type, ]

  total_counts <- table(data$City)

  user_type_counts <- table(type_data$City)

  percentage <- (user_type_counts / total_counts) * 100

  result <- data.frame(City = names(percentage), Percentage = as.numeric(percentage))

  return(result)
}


# Calculate percentage for 'Subscriber' using function and display
subscriber_percentage <- calculate_percentage_user_type(city_data, 'Subscriber')
print(subscriber_percentage)
#percentage for subscribers below

# Calculate percentage for 'Customer' using function and display
customer_percentage <- calculate_percentage_user_type(city_data, 'Customer')
print(customer_percentage)
#percentage for customers below

system('python -m nbconvert Explore_bikeshare_data.ipynb')

