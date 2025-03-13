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
#'   introduced randomly for each sorter. The attribute weights determine how many
#'   sorters focus on each attribute, and a warning is issued if the weights do
#'   not align with the total number of sorters.
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
                             attributeWeights = c(1,1,1,1),
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
                             color = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
                                                 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 3L, 3L, 3L, 3L, 3L, 3L, 3L,
                                                 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L,
                                                 3L, 3L, 3L, 2L, 2L), levels = c("Black", "Wildcard", "Red"), class = "factor"),
                             suit = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
                                                1L, 1L, 1L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L,
                                                5L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L,
                                                3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 4L, 4L), levels = c("Club",
                                                                                                                    "Diamonds", "Hearts", "Wildcard", "Spades"), class = "factor"),
                             rank_picture = structure(c(2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L,
                                                        1L, 10L, 10L, 10L, 11L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 1L,
                                                        10L, 10L, 10L, 11L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 1L, 10L,
                                                        10L, 10L, 11L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 1L, 10L, 10L,
                                                        10L, 11L, 10L, 10L), levels = c("10", "2", "3", "4", "5",
                                                                                        "6", "7", "8", "9", "Picture", "Ace"), class = "factor"),
                             odd_even_picture = structure(c(1L, 2L, 1L, 2L, 1L, 2L, 1L,
                                                            2L, 1L, 3L, 3L, 3L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L,
                                                            3L, 3L, 3L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 3L, 3L,
                                                            3L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 3L, 3L, 3L, 2L,
                                                            3L, 3L), levels = c("Even", "Odd", "Picture"), class = "factor")), row.names = c(NA,
                                                                                                                                             -54L), class = "data.frame")

  if (verbose) {
    cat(sprintf("Simulating cardData.\n"))
    cat(sprintf("  Number of sorters: %d\n", nSorters))
    cat(sprintf("  Error probability: %f\n", pCorrect))
    cat(sprintf("  Attribute weights: %s\n", paste(attributeWeights, collapse = ", ")))
  }

  #get attribute labels from the cardDeck
  attributeLabels <- colnames(cardDeck)[-1]

  #determine number of sorters per attribute
  numbersOfSortersPerAttribute <- attributeWeights*(nSorters/sum(attributeWeights))

  #create sorterChoice vector
  sorterChoice <- unlist(as.vector(sapply(1:4, function(i) {
    rep(attributeLabels[i], numbersOfSortersPerAttribute[i])
  })))

  #report sorter's choices
  if (verbose) {
    cat(sprintf("  Number of sorters per sorting attribute:\n"))
    cat(table(sorterChoice))
    cat("\n")
  }

  #show warning in case number of sorters does not match with attribute weights
  if (sum(table(sorterChoice)) != nSorters) {
    warning(paste0("The number of requested sorters (", nSorters, ") is incompatible with the attribute weights.\nSetting number of sorters to ", sum(table(sorterChoice)), "."))
    nSorters <- sum(table(sorterChoice))
  }

  #run simulated data creation
  mySimulatedStack <- lapply(1:nSorters, function(jj) {
    #sort according to their choice
    myStackID <- cardDeck[, sorterChoice[jj]] #+1 is to point to correct column

    #we need to add some errors
    theSortErrors <- which(stats::rbinom(n=nrow(cardDeck), size=1, prob = (1-pCorrect))==1)

    #change some stackID's randomly (this means cards are misplaced)
    myStackID[theSortErrors] <- sample(levels(cardDeck[, sorterChoice[jj]]),
                                       size=length(theSortErrors), replace=TRUE)

    #store this sorter's work
    data.frame(sorterID=sprintf("Sorter%.2d", jj), statement=cardDeck[, 1], stackID=myStackID)
  })

  #unpack all the stack from all sorters and create one data set
  theSimulatedCardData <- do.call(rbind, mySimulatedStack)

  return(theSimulatedCardData)
}
