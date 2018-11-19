
library("tidyverse")
library("countrycode")

# dput(sort(unique(events$country)))
countries <- c("Aeolian Islands", "Afghanistan", "Albania", "Algeria", "American Samoa",
               "Andorra", "Angola", "Anguilla", "Antarctica", "Antigua and Barbuda",
               "Argentina", "Armenia", "Aruba", "Australia", "Austria", "Azerbaijan",
               "Bahamas", "Bahrain", "Bangladesh", "Barbados", "Bavaria", "Belarus",
               "Belgium", "Belize", "Benin", "Bermuda", "Bhutan", "Bolivia",
               "Bonaire", "Bosnia and Herzegovina", "Botswana", "Brazil", "British Indian Ocean Territory",
               "British Virgin Islands", "Brunei", "Bulgaria", "Burkina Faso",
               "Burundi", "Cambodia", "Cameroon", "Canada", "Cape Verde", "Cayman Islands",
               "Central African Republic", "Chad", "Chile", "China", "Christmas Island",
               "Cocos (Keeling) Islands", "Colombia", "Comoros", "Congo", "Cook Island",
               "Costa Rica", "Cote d'Ivoire", "Croatia", "Cuba", "Curaçao",
               "Cyprus", "Czech Republic", "Democratic Republic of Congo", "Denmark",
               "Djibouti", "Dominica", "Dominican Republic", "Ecuador", "Egypt",
               "El Salvador", "Equatorial Guinea", "Eritrea", "Estonia", "Ethiopia",
               "Falkland Islands", "Faroe Islands", "Fiji", "Finland", "France",
               "French Guiana", "French Polynesia", "French Southern Territories",
               "Gabon", "Gambia", "Georgia", "Germany", "Ghana", "Gibraltar",
               "Greece", "Greenland", "Grenada", "Guadeloupe", "Guam", "Guatemala",
               "Guernsey", "Guinea", "Guinea-Bissau", "Guyana", "Haiti", "Heard Island and McDonald Islands",
               "Holy See", "Honduras", "Hong Kong", "Hungary", "Iceland", "India",
               "Indonesia", "Iran", "Iraq", "Ireland", "Isle Of Man", "Israel",
               "Italy", "Jamaica", "Japan", "Jersey", "Jordan", "Kazakhstan",
               "Kenya", "Kiribati", "Kosovo", "Kuwait", "Kyrgyzstan", "Laos",
               "Latvia", "Lebanon", "Lesotho", "Liberia", "Libya", "Liechtenstein",
               "Lithuania", "Luxembourg", "Macao", "Madagascar", "Malawi", "Malaysia",
               "Maldives", "Mali", "Malta", "Marshall Islands", "Martinique",
               "Mauritania", "Mauritius", "Mayotte", "Mexico", "Micronesia",
               "Moldova, Republic of", "Monaco", "Mongolia", "Montenegro", "Montserrat",
               "Morocco", "Mozambique", "Myanmar", "Namibia", "Nauru", "Nepal",
               "Netherlands", "Netherlands Antilles", "New Caledonia", "New Zealand",
               "Nicaragua", "Niger", "Nigeria", "Niue", "Norfolk Island", "North Korea",
               "Northern Mariana Islands", "Norway", "Occupied Palestinian Territory",
               "Oman", "Pakistan", "Palau", "Panama", "Papua New Guinea", "Paraguay",
               "Peru", "Philippines", "Pitcairn", "Poland", "Portugal", "Puerto Rico",
               "Qatar", "Reunion", "Romania", "Russian Federation", "Rwanda",
               "Saint Barthelemy", "Saint Helena", "Saint Kitts and Nevis",
               "Saint Lucia", "Saint Martin (French part)", "Saint Pierre and Miquelon",
               "Saint Vincent and the Grenadines", "Samoa", "San Marino", "Sao Tome and Principe",
               "Saudi Arabia", "Senegal", "Serbia", "Seychelles", "Sierra Leone",
               "Singapore", "Sint Maarten", "Slovakia", "Slovenia", "Solomon Islands",
               "Somalia", "South Africa", "South Georgia and the South Sandwich Islands",
               "South Korea", "South Sudan", "Spain", "Sri Lanka", "Sudan",
               "Suriname", "Svalbard and Jan Mayen", "Swaziland", "Sweden",
               "Switzerland", "Syria", "Taiwan", "Tajikistan", "Tanzania", "Thailand",
               "the former Yugoslav Republic of Macedonia", "Timor-Leste", "Togo",
               "Tokelau", "Tonga", "Trinidad and Tobago", "Tunisia", "Turkey",
               "Turkmenistan", "Turks and Caicos Islands", "Tuvalu", "U.S. Virgin Islands",
               "Uganda", "Ukraine", "United Arab Emirates", "United Kingdom",
               "United States", "United States Minor Outlying Islands", "Uruguay",
               "Uzbekistan", "Vanuatu", "Venezuela", "Vietnam", "Wallis and Futuna",
               "Western Sahara", "Yemen", "Zambia", "Zimbabwe")

df <- tibble(country = countries) %>%
  mutate(cowcode = countrycode(country, "country.name", "cown", warn = FALSE))
df <- df %>%
  mutate(gwcode = case_when(
    cowcode==255L ~ 260L,
    cowcode==679L ~ 678L,
    cowcode==947L ~ 973L,
    cowcode==955L ~ 972L,
    cowcode==970L ~ 971L,
    cowcode==946L ~ 970L,
    country %in% c("American Samoa", "Guam", "Northern Mariana Islands", "Puerto Rico", "U.S. Virgin Islands", "United States Minor Outlying Islands") ~ 2L,
    # UK
    country %in% c("Anguilla", "Bermuda", "British Indian Ocean Territory", "British Virgin Islands", "Cayman Islands", "Gibraltar", "Guernsey", "Falkland Islands", "Isle Of Man", "Jersey", "Montserrat", "Pitcairn", "Saint Helena", "South Georgia and the South Sandwich Islands", "Turks and Caicos Islands") ~ 200L,
    # Netherlands
    country %in% c("Aruba", "Bonaire", "Curaçao", "Netherlands Antilles", "Sint Maarten") ~ 210L,
    # France
    country %in% c("French Guiana", "French Polynesia", "French Southern Territories", "Guadeloupe", "Martinique", "Mayotte", "New Caledonia", "Reunion", "Saint Barthelemy", "Saint Martin (French part)", "Saint Pierre and Miquelon", "Wallis and Futuna") ~ 220L,
    country %in% c("Aeolian Islands") ~ 325L,
    country %in% c("Svalbard and Jan Mayen") ~ 385L,
    country %in% c("Greenland", "Faroe Islands") ~ 390L,
    country %in% c("Western Sahara") ~ 600L,
    country %in% c("Christmas Island") ~ 900L,
    country %in% c("Cocos (Keeling) Islands", "Cook Island", "Heard Island and McDonald Islands", "Norfolk Island") ~ 920L,
    TRUE ~ cowcode
  ))

country_to_gwcode <- df %>% select(country, gwcode)
usethis::use_data(country_to_gwcode, internal = TRUE, overwrite = TRUE)


# remainder
df %>%
  filter(is.na(gwcode)) %>%
  View("leftover-cnames")
