
### Feature engineering -----------------------------------------------------------------------------------

# Train set
train %<>% mutate(length_of_donation = Months_since_First_Donation - Months_since_Last_Donation,
                  frequency_of_donation = length_of_donation / Number_of_Donations,
                  average_volume_of_donation = Total_Volume_Donated / Number_of_Donations
)

# average_volume_of_donation is constant. That is the Total_Volume_Donated is a linear function of 
# Number_of_Donations

train %<>% select(-average_volume_of_donation, -Total_Volume_Donated)

# Validation set
validate %<>% mutate(length_of_donation = Months_since_First_Donation - Months_since_Last_Donation,
                  frequency_of_donation = length_of_donation / Number_of_Donations,
)

# Test set
test %<>% mutate(length_of_donation = Months_since_First_Donation - Months_since_Last_Donation,
                     frequency_of_donation = length_of_donation / Number_of_Donations,
)
