#' Abbreviate Statements in Concept Map Data
#'
#' This function processes the "statement" column of a data frame containing concept map data by converting each
#' statement to lowercase, removing stopwords, and truncating the statement to a specified maximum length.
#' It allows for the abbreviation of long statements while maintaining their core meaning by removing unnecessary words.
#'
#' @param CMData A data frame containing concept map data. The data should have at least the following column:
#'   - `statement`: The text of the statement to be abbreviated.
#'
#' @param max_length An integer specifying the maximum number of characters for the abbreviated statement. Default is 30.
#'
#' @details This function performs several preprocessing steps on the "statement" column:
#'   - Converts statements to lowercase.
#'   - Removes punctuation and stopwords from the statements.
#'   - Truncates statements to a specified maximum length.
#'   - Removes any rows with empty statements after processing.
#'
#' Stopwords are predefined and include common English words (e.g., "the", "and", "is", "a", etc.) that do not contribute
#' much meaning to the core idea of the statement.
#'
#' @return A data frame with the same structure as the input, but with an updated "statement" column containing the
#' abbreviated statements.
#'
#' @importFrom stringr str_to_lower str_replace_all str_squish str_remove_all
#'
#' @examples
#' # Create a sample data frame with concept map data
#' conceptMapData <- data.frame(
#'    id = c(1, 2, 3),
#'     statement = c(
#'        "The quick brown fox jumps over the lazy dog",
#'        "This is a simple concept map example",
#'        "Data science involves analyzing datasets"
#'     )
#' )
#'
#' # Apply the abbreviateStatements function with a maximum length of 20
#' result <- abbreviateStatements(conceptMapData, max_length = 20)
#'
# Display the resulting data frame
#' print(result)
#'
#' @export
abbreviateStatements <- function(CMData, max_length = 30) {

  # Step 1: Define the list of stopwords directly in the script
  stopwords <- structure(list(
    "la", "ns", "ef", "al", "lol", "shit", "fucking", "fuck", "c", "im",
    "gt", "s", "t", "u", "k", "us", "d", "m", "n", "j", "h", "e", "re",
    "may", "ive", "r", "ese", "aj", "y", "hes", "shes", "doesnt", "didnt",
    "dont", "isnt", "arent", "wasnt", "werent", "hasnt", "havent",
    "hadnt", "wont", "wouldnt", "cant", "couldnt", "shouldnt", "might",
    "mightnt", "mustnt", "whats", "whos", "wheres", "whens", "whys",
    "hows", "youre", "youll", "rulescarefully", "please", "read", "join",
    "can", "will", "ve", "just", "like", "make", "think", "know", "get",
    "got", "one", "time", "going", "see", "say", "said", "want", "also",
    "way", "even", "take", "saying", "still", "really", "much", "many",
    "back", "go", "well", "look", "good", "need", "first", "two", "say",
    "said", "let", "right", "gonna", "something", "thing", "things", "lot",
    "sure", "maybe", "yes", "no", "okay", "hello", "hi", "hey", "thanks",
    "thank", "sorry", "ok", "yeah", "yes", "nope", "new", "old", "great",
    "better", "best", "awesome", "amazing", "cool", "ll", "reddit", "op",
    "sub", "ing", "etc", "don", "thats", "wasn", "saw", "went", "didn",
    "bring", "nk", "f", "theyre", "ese", "i", "me", "my", "myself", "we",
    "our", "ours", "ourselves", "you", "your", "yours", "yourself",
    "yourselves", "he", "him", "his", "himself", "she", "her", "hers",
    "herself", "it", "its", "itself", "they", "them", "their", "theirs",
    "themselves", "what", "which", "who", "whom", "this", "that", "these",
    "those", "am", "is", "are", "was", "were", "be", "been", "being",
    "have", "has", "had", "having", "do", "does", "did", "doing", "a",
    "an", "the", "and", "but", "if", "or", "because", "as", "until",
    "while", "of", "at", "by", "for", "with", "about", "against",
    "between", "into", "through", "during", "before", "after", "above",
    "below", "to", "from", "up", "down", "in", "out", "on", "off",
    "over", "under", "again", "further", "then", "once", "here",
    "there", "when", "where", "why", "how", "all", "any", "both",
    "each", "few", "more", "most", "other", "some", "such", "no",
    "nor", "not", "only", "own", "same", "so", "than", "too", "very",
    "s", "t", "can", "will", "just", "don", "should", "now", "ever",
    "used", "never", "told", "st", "nd", "rd", "th"), .Names = NULL)

  # Step 2: Convert the stopwords into a regular expression pattern
  stopwords_pattern <- paste0("\\b(", paste(stopwords, collapse = "|"), ")\\b")

  # Step 3: Process the "statement" column of CMData
  # Step-by-step processing without pipes
  CMData$oriStatement <- CMData$statement  # Keep the original statement as "oriStatement"

  # Convert the statement to lowercase
  CMData$statement <- stringr::str_to_lower(CMData$statement)

  # Remove punctuation from the statement
  CMData$statement <- stringr::str_replace_all(CMData$statement, "[[:punct:]]", "")

  # Remove stopwords and extra spaces
  CMData$statement <- stringr::str_squish(stringr::str_remove_all(CMData$statement, stopwords_pattern))

  # Restrict the length to max_length characters
  CMData$statement <- substr(CMData$statement, 1, max_length)

  # Remove rows with empty statements
  CMData <- CMData[CMData$statement != "", ]
  # Step 4: Return the modified dataframe
  return(CMData)
}
