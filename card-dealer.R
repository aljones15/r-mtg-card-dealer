library(dplyr)
library(purrr)
library(httr)
library(jsonlite)

"get the deck from a \n sep file"
deck <- read.delim(
  file.choose(),
  header = FALSE,
  blank.lines.skip = FALSE,
  col.names = c('cards'),
  stringsAsFactors = FALSE,
  colClasses = c("character")
)

"Sideboard index is either a string Sideboard or an empty string for mtg goldfish"
sideboardStart <- grep("(^\\s*Sideboard\\s*$)|(^$)", deck$cards, perl = TRUE)

"get the count and name"
"count really should be first numbers in the string until a space"
deckParser <- function(d) {
  d %>%
    select(cards) %>%
    mutate(
      count = lapply(cards, function(x) { strsplit(x, ' ', perl = TRUE)[[1]][1] }),
      name = lapply(cards, function(x) {
        cardNames <- strsplit(x, ' ', perl = TRUE)[[1]]
        "join the words in the card name that were split apart"
        trimws(paste(cardNames[2:length(cardNames)], collapse = ' '))
      })
    )
}

fetchCard <- function(name) {
  "turn the spaces into +s"
  search <- gsub(" ", "+", name)
  card <- GET(paste("https://api.scryfall.com/cards/named?exact=", search, sep=""))
  fromJSON(content(card, "text"))
}

"we need to get oracle_text, mana_cost, and keywords, type_line"

"separate deck and sideboard"
main <- deckParser(slice(deck, n = 1:(sideboardStart - 1)))
sideboard <- deckParser(slice(deck, n = (sideboardStart + 1):length(deck$cards)))

"turn the data frame into a repeating list"
makeDeck <- function(deck) {
  apply(deck, 1, function(row) {
    card <- fetchCard(row$name)
    "flip cards are missing text and cost"
    rows <- list(
      name = card$name,
      text = card$oracle_text,
      cost = card$mana_cost,
      type = card$type_line
    )
    replicate(rows, n = row$count)
  })
}

"transpose after reducing the matrixies to a single matrix"
deckList <- t(reduce(makeDeck(main), cbind))

"get a sample of 7 cards"
deckList[sample(nrow(deckList), 7, replace = FALSE), ]

"this reduces a deck size with each draw"
draw <- function(count, deck, hand = list()) {
  draws <- deck[sample(nrow(deck), count, replace = FALSE), ]
  nextDeck <- reduce(draws, function(acc, nextCard) {
    m <- match(nextCard, acc)
    if(m == 0 || is.na(m)) {
      return(acc)
    }
    print(m)
    acc[-m, ]
  }, .init = deck)
  list(deck = nextDeck, drawn = c(draws, hand))
}

init <- draw(7, deckList)