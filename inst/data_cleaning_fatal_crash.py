#load the file to ensure all details are accurate and include row names in the output
import pandas as pd

# Define the file path
file_path = "/inst/ext_data/fatal_crashes_2013_2022_nhtsa.txt"
output_path = "/inst/ext_data/cleaned_fatal_crashes.txt"

data = pd.read_csv(file_path, sep="\t", header=1, index_col=0)

# Retain only the columns named '#' and keep row names intact
cleaned_data_with_rownames = data.filter(like='#')

# Save the cleaned dataset back to a file with row names
cleaned_data_with_rownames.to_csv(output_path, sep="\t", index=True)

output_path
