anonymize_string <- function(input_string, word_mapping) {
  # Split the input string into words
  words <- unlist(strsplit(input_string, " "))

  # Initialize an empty vector to store the anonymized words
  anonymized_words <- character(0)

  # Loop through each word in the input string
  for (word in words) {
    # Check if the word is in the word_mapping
    if (word %in% names(word_mapping)) {
      # If it's in the mapping, replace with a random word from the mapping
      anonymized_word <- sample(word_mapping[[word]], 1)
    } else {
      # If not in the mapping, keep the original word
      anonymized_word <- word
    }
    # Append the anonymized word to the vector
    anonymized_words <- c(anonymized_words, anonymized_word)
  }

  # Combine the anonymized words back into a string
  anonymized_string <- paste(anonymized_words, collapse = " ")
  return(anonymized_string)
}


generate_word_mapping <- function(input_string) {
  words <- unlist(strsplit(input_string, " "))
  unique_words <- unique(words)

  replacement_words <- lapply(unique_words, function(word) {
    replacement <- sample(unique_words, 1)
    while (replacement == word) {
      replacement <- sample(unique_words, 1)
    }
    return(replacement)
  })

  word_mapping <- setNames(replacement_words, unique_words)
  return(word_mapping)
}

# Example input text
input_text <- "John and Mary went to the store. John bought apples, and Mary bought oranges."

# Generate the word mapping
word_mapping <- generate_word_mapping(input_text)

# Anonymize the input text using the generated word mapping
anonymized_text <- anonymize_string(input_text, word_mapping)

cat("Original text: ", input_text, "\n")
cat("Anonymized text: ", anonymized_text, "\n")



# Example word mapping
word_mapping <- list(
  "John" = c("Alice", "Bob", "Charlie"),
  "Mary" = c("Eve", "Grace", "Hannah")
)

# Example input string
input_text <- "John and Mary went to the store."

# Anonymize the input string
anonymized_text <- anonymize_string(input_text, word_mapping)

cat("Original text: ", input_text, "\n")
cat("Anonymized text: ", anonymized_text, "\n")
