#' Simulate Card Sorting Data
#'
#' This function simulates card sorting data based on user-specified parameters,
#' such as the number of sorters, the probability of correct sorting, and the
#' weights for different card attributes.
#'
#' @param nSorters An integer specifying the number of sorters to simulate. Default is 40.
#' @param pCorrect A numeric value between 0 and 1 specifying the probability that
#'   a card is sorted correctly. Default is 0.95.
#' @param attributeWeights A numeric vector of length 4 specifying the weights
#'   for the card attributes (e.g., \code{color}, \code{suit}, \code{rank_picture},
#'   and \code{odd_even_picture}). Default is \code{c(1, 1, 1, 1)}.
#' @param verbose Logical, if \code{TRUE}, additional information about the processing
#'  steps is printed to the console.
#'
#' @details The function simulates a card sorting experiment where cards are
#'   sorted by multiple sorters based on one of four attributes. The probability
#'   of sorting a card correctly is determined by \code{pCorrect}, and errors are
#'   introduced randomly for each sorter by selecting a different category.
#'   Attribute weights determine how many sorters focus on each attribute.
#'   Whole-number allocations use the largest remainders to preserve the requested
#'   total; ties are resolved in attribute order.
#'
#'   The function returns a data frame containing simulated card sorting data
#'   for all sorters, including sorter IDs, card IDs, and assigned stacks.
#'
#' @return A data frame with columns:
#'   \describe{
#'     \item{\code{sorterID}}{A unique identifier for each sorter.}
#'     \item{\code{statement}}{The ID of the card being sorted.}
#'     \item{\code{stackID}}{The stack assigned to the card by the sorter.}
#'   }
#'
#' @examples
#' # Simulate data with default parameters:
#' set.seed(1)
#' myCMData <- simulateCardData()
#'
#' # Simulate data with custom parameters:
#' set.seed(1)
#' myCMData <- simulateCardData(nSorters=40, pCorrect=.90, attributeWeights=c(1,1,1,1))
#'
#' @seealso \code{\link[stats]{rbinom}}, \code{\link[base]{sample}}
#'
#' @export
simulateCardData <- function(nSorters = 40,
                             pCorrect = .95,
                             attributeWeights = c(1, 1, 1, 1),
                             verbose = TRUE) {

  cardDeck <- structure(list(ID = c("Club2", "Club3", "Club4", "Club5", "Club6",
                                    "Club7", "Club8", "Club9", "Club10", "ClubJ", "ClubQ", "ClubK",
                                    "ClubA", "Spades2", "Spades3", "Spades4", "Spades5", "Spades6",
                                    "Spades7", "Spades8", "Spades9", "Spades10", "SpadesJ", "SpadesQ",
                                    "SpadesK", "SpadesA", "Diamonds2", "Diamonds3", "Diamonds4",
                                    "Diamonds5", "Diamonds6", "Diamonds7", "Diamonds8", "Diamonds9",
                                    "Diamonds10", "DiamondsJ", "DiamondsQ", "DiamondsK", "DiamondsA",
                                    "Hearts2", "Hearts3", "Hearts4", "Hearts5", "Hearts6", "Hearts7",
                                    "Hearts8", "Hearts9", "Hearts10", "HeartsJ", "HeartsQ", "HeartsK",
                                    "HeartsA", "Wildcard1", "Wildcard2"),
                             color = structure(as.integer(c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                                                 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 3, 3, 3, 3, 3,
                                                 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
                                                 3, 3, 3, 2, 2)), levels = c("Black", "Wildcard", "Red"), class = "factor"),
                             suit = structure(as.integer(c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                                                1, 1, 1, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
                                                5, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3,
                                                3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4)), levels = c("Club",
                                                                                                                    "Diamonds", "Hearts", "Wildcard", "Spades"), class = "factor"),
                             rank_picture = structure(as.integer(c(2, 3, 4, 5, 6, 7, 8, 9,
                                                        1, 10, 10, 10, 11, 2, 3, 4, 5, 6, 7, 8, 9, 1,
                                                        10, 10, 10, 11, 2, 3, 4, 5, 6, 7, 8, 9, 1, 10,
                                                        10, 10, 11, 2, 3, 4, 5, 6, 7, 8, 9, 1, 10, 10,
                                                        10, 11, 10, 10)), levels = c("10", "2", "3", "4", "5",
                                                                                        "6", "7", "8", "9", "Picture", "Ace"), class = "factor"),
                             odd_even_picture = structure(as.integer(c(1, 2, 1, 2, 1, 2, 1,
                                                            2, 1, 3, 3, 3, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1,
                                                            3, 3, 3, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 3, 3,
                                                            3, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 3, 3, 3, 2,
                                                            3, 3)), levels = c("Even", "Odd", "Picture"), class = "factor")), row.names = as.integer(c(NA,
                                                                                                                                             -54)), class = "data.frame")

  if (!is.numeric(nSorters) || length(nSorters) != 1 ||
        !is.finite(nSorters) || nSorters < 1 || nSorters != floor(nSorters)) {
    stop("nSorters must be one positive integer.")
  }

  if (!is.numeric(attributeWeights) || length(attributeWeights) != 4 ||
        any(!is.finite(attributeWeights)) || any(attributeWeights < 0) || !any(attributeWeights > 0)) {
    stop("attributeWeights must contain four finite non-negative weights with positive sum.")
  }

  if (!is.numeric(pCorrect) || length(pCorrect) != 1 || !is.finite(pCorrect) || pCorrect < 0 || pCorrect > 1) {
    stop("pCorrect must be one number in [0,1].")
  }

  if (verbose) {
    cat(sprintf("Simulating cardData.\n"))
    cat(sprintf("  Number of sorters: %d\n", nSorters))
    cat(sprintf("  Correct sorting probability: %f\n", pCorrect))
    cat(sprintf("  Attribute weights: %s\n", paste(attributeWeights, collapse = ", ")))
  }

  #get attribute labels from the cardDeck
  attributeLabels <- colnames(cardDeck)[-1]

  #determine number of sorters per attribute
  weights <- attributeWeights / max(attributeWeights)
  quota <- nSorters * (weights / sum(weights))
  numbersOfSortersPerAttribute <- floor(quota)
  remaining <- as.integer(nSorters - sum(numbersOfSortersPerAttribute))

  if (remaining > 0) {
    orderRemainder <- order(-(quota - numbersOfSortersPerAttribute), seq_along(quota))
    extra <- orderRemainder[seq_len(remaining)]
    numbersOfSortersPerAttribute[extra] <- numbersOfSortersPerAttribute[extra] + 1
  }

  sorterChoice <- rep(attributeLabels, times = numbersOfSortersPerAttribute)

  #report sorter's choices
  if (verbose) {
    cat(sprintf("  Number of sorters per sorting attribute:\n"))
    cat(table(sorterChoice))
    cat("\n")
  }

  #run simulated data creation
  mySimulatedStack <- lapply(1:nSorters, function(jj) {
    #sort according to their choice
    myStackID <- cardDeck[, sorterChoice[jj]] #+1 is to point to correct column

    #we need to add some errors
    theSortErrors <- which(stats::rbinom(n = nrow(cardDeck), size = 1, prob = (1 - pCorrect)) == 1)

    #change some stackID's randomly (this means cards are misplaced)
    categories <- levels(cardDeck[, sorterChoice[jj]])
    for (row in theSortErrors) {
      alternatives <- setdiff(categories, as.character(myStackID[row]))

      if (!length(alternatives)) stop("A sorting error requires at least two categories.")
      
      myStackID[row] <- sample(alternatives, size = 1)
    }

    #store this sorter's work
    data.frame(sorterID = sprintf("Sorter%.2d", jj), statement = cardDeck[, 1], stackID = myStackID)
  })

  #unpack all the stack from all sorters and create one data set
  theSimulatedCardData <- do.call(rbind, mySimulatedStack)

  return(theSimulatedCardData)
}
