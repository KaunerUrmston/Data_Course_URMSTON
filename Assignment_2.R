# Step 4: List all .csv files in the Data/ directory

csv_files <- list.files("Data", pattern = "\\.csv$")

# Step 5: Count how many .csv files were found

length(csv_files)

# Step 6: Read wingspan_vs_mass.csv into an object called "df"

df <- read.csv("Data/wingspan_vs_mass.csv")

# Step 7: Inspect the first 5 lines of the data frame

head(df, 5)

# Step 8: Find files (recursively) in Data/ that begin with "b"

b_files <- list.files(
  "Data",
  pattern = "^b",
  recursive = TRUE,
  full.names = TRUE)


# Step 9: Display the first line of each "b" file

for (file in b_files) {
  cat("File:", file, "\n")
  print(readLines(file, n = 1))
  cat("\n")}

# Step 10: Display the first line of every .csv file

csv_full_paths <- list.files(
  "Data",
  pattern = "\\.csv$",
  recursive = TRUE,
  full.names = TRUE)

for (file in csv_full_paths) {
  cat("File:", file, "\n")
  print(readLines(file, n = 1))
  cat("\n")}
