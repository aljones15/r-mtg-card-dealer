library(dplyr)

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
deckParser <- function(d) {
  d %>%
    select(cards) %>%
    mutate(
      count = lapply(cards, function(x) { strsplit(x, ' ', perl = TRUE)[[1]][1] }),
      name = lapply(cards, function(x) {
        cardNames <- strsplit(x, ' ', perl = TRUE)[[1]]
        trimws(paste(cardNames[2:length(cardNames)], collapse = ' '))
      })
    )
}

"separate deck and sideboard"
main <- deckParser(slice(deck, n = 1:(sideboardStart - 1)))
sideboard <- deckParser(slice(deck, n = (sideboardStart + 1):length(deck$cards)))

"turn the data frame into a repeating list"
deckList <- unlist(apply(main, 1, function(row) {
  rep(row$name, times = row$count)
}))

length(deckList)

"get a sample of 7 cards"
sample(deckList, 7, replace = FALSE)

"this needs to reduce a deck size each time"
draw <- function(count, deck) {
  s <- sample(deck, count, replace = FALSE)
  matches <- match(s, deck)
  nextDeck <- deck[-matches]
  nextDeck
}