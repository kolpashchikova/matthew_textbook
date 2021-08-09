text_v <- scan("data/text/melville.txt", what = "character", sep = "\n")
start_v <- which(text_v == "CHAPTER 1. Loomings.") # where does the actual text begin
metadata_v <- text_v[1:start_v - 1] # the metadata before the text
novel_lines_v <- text_v[start_v:length(text_v)] # the actual novel
novel_v <- paste(novel_lines_v, collapse = " ")
novel_lower_v <- tolower(novel_v)
moby_word_l <- strsplit(novel_lower_v, "\\W")
moby_word_v <- unlist(moby_word_l)
not_blanks_v <- which(moby_word_v != "")
not_blanks_v
moby_word_v <- moby_word_v[not_blanks_v] # getting rid of blanks
which(moby_word_v == "whale") # positions of this word
moby_word_v[which(moby_word_v == "whale")]
moby_word_v[which(moby_word_v == "why")]
whale_hits_v <-length(moby_word_v[which(moby_word_v == "whale")]) 
total_words_v <- length(moby_word_v) # the length of the novel
whale_hits_v / total_words_v # whales percentage
length(unique(moby_word_v)) # unique words count
moby_freqs_t <- table(moby_word_v) # frequencies table
moby_freqs_t[1:1000]
sorted_moby_freqs_t <- sort(moby_freqs_t, decreasing = TRUE)
head(sorted_moby_freqs_t)
top_ten_t <- sorted_moby_freqs_t[1:10]
plot(top_ten_t)
sorted_moby_freqs_t <- sort(moby_freqs_t, decreasing = TRUE)
sorted_moby_freqs_t["he"]
sorted_moby_freqs_t["she"]
sorted_moby_freqs_t["him"]
sorted_moby_freqs_t["her"]
sorted_moby_freqs_t["him"]/sorted_moby_freqs_t["her"]
sorted_moby_freqs_t["he"]/sorted_moby_freqs_t["she"]
sum(sorted_moby_freqs_t)
moby_length_v <- sum(sorted_moby_freqs_t)
sorted_moby_rel_freqs_t <- 100*(sorted_moby_freqs_t/moby_length_v)
sorted_moby_rel_freqs_t["the"] # сколько раз на каждые 100 слов встречается the
plot(
  sorted_moby_rel_freqs_t[1:10], type = "b",
  xlab = "Top Ten Words in Moby Dick", # подпись к оси х
  ylab = "Percentage of Full Text", # подпись к оси у
  xaxt = "n"
)
axis(
  1, 1:10,
  labels = names(sorted_moby_rel_freqs_t [1:10])
)

text2_v <- scan("data/text/austen.txt", what = "character", sep = "\n")
text2_v[1:10]
metadata2_v <- text2_v[1:2]
novel2_v <- text2_v[3:(length(text2_v))]
novel2_v <- paste(novel2_v, collapse = " ")
novel2_v <- tolower(novel2_v)
novel2_l <- strsplit(novel2_v, "\\W")
str(novel2_l)
words2_v <- unlist(novel2_l)
not_blanks2_v <- which(words2_v != "")
words2_v <- words2_v[not_blanks2_v]
freqs2_t <- table(words2_v)
sorted_freqs2_t <- sort(freqs2_t, decreasing = TRUE)
length2_v <- sum(sorted_freqs2_t)
sorted_rel_freqs2_t <- 100*(sorted_freqs2_t/length2_v)
plot(
  sorted_rel_freqs2_t[1:10], type = "b",
  xlab = "Top Ten Words in Sense & Sensibility", 
  ylab = "Percentage of Full Text", 
  xaxt = "n"
)
axis(
  1, 1:10,
  labels = names(sorted_rel_freqs2_t [1:10])
)

joined_freqs <- unique(c(names(sorted_moby_rel_freqs_t[1:10])), names(sorted_rel_freqs2_t [1:10]))

same_freqs <- which(names(sorted_moby_rel_freqs_t[1:10]) %in% names(sorted_rel_freqs2_t [1:10]))

names(sorted_rel_freqs2_t[
  which(names(sorted_rel_freqs2_t[1:10])
        %in% names(sorted_moby_rel_freqs_t[1:10]))]) # words present in both texts

presentSense <- which(names(sorted_rel_freqs2_t[1:10])
                      %in% names(sorted_moby_rel_freqs_t[1:10]))
names(sorted_rel_freqs2_t[1:10])[-presentSense]
presentMoby <- which(names(sorted_moby_rel_freqs_t[1:10])
                     %in% names(sorted_rel_freqs2_t[1:10]))
names(sorted_moby_rel_freqs_t[1:10])[-presentMoby] # unique words for each text

n_time_v <- seq(from = 1, to = length(moby_word_v))
whales_v <- which(moby_word_v == "whale")
w_count_v <- rep(NA, times = length(n_time_v))
w_count_v[whales_v] <- 1
plot(
  w_count_v,
  main = "Dispersion Plot of 'whale' in Moby Dick",
  xlab = "Novel Time",
  ylab = "whale",
  type = "h",
  ylim = c(0, 1), yaxt = 'n'
)

whale_hits <- grep(
  "whale|whales|whale's|monster|leviathan",
  moby_word_v
)
ahab_hits <- grep(
  "ahab|ahabs|ahab's|captain",
  moby_word_v
)
whale_hits_new <- grep(
  "whale|whales|whale's|monster|leviathan",
  moby_word_v
)
ahab_hits_new <- grep(
  "ahab|ahabs|ahab's|captain",
  moby_word_v
)
whale_varients_v <- moby_word_v[whale_hits_new]
ahab_varients_v <- moby_word_v[ahab_hits_new]
sort(table(whale_varients_v), decreasing = TRUE)
length(which(whale_varients_v == "whale"))
length(grep("whale", whale_varients_v))
length(grep("^whale$", whale_varients_v))

#n2_time_v <- seq(from = 1, to = length(moby_word_v))
#whales2_v <- which(moby_word_v %in% whale_varients_v)
#w2_count_v <- rep(NA, times = length(n2_time_v))
#w2_count_v[whales2_v] <- 1
#plot(
# w2_count_v,
#main = "Dispersion Plot of 'whale' varients in Moby Dick",
#xlab = "Novel Time",
#ylab = "whale",
#type = "h",
#ylim = c(0, 1), yaxt = 'n'
#)

w_varient_v <- rep(NA, length(n_time_v))
whale_hits <- grep(
  "whale|whales|whale's|monster|leviathan",
  moby_word_v
)
w_varient_v[whale_hits] <- 1
plot(
  w_varient_v,
  main ="Dispersion Plot of 'whale' variants in Moby Dick",
  xlab = "Novel Time",
  ylab = "whale(s)",
  type = "h",
  ylim = c(0,1),
  yaxt = 'n'
)
table(moby_word_v[grep("^w...e$", moby_word_v)])
table(moby_word_v[grep("^wh..e$", moby_word_v)])
adverbs_t <- sort(table(moby_word_v[grep("ly$", moby_word_v)]), decreasing = TRUE)

start_v <- which(text_v == "CHAPTER 1. Loomings.")
novel_lines_v <- text_v[start_v:length(text_v)]
chap_positions_v <- grep("^CHAPTER \\d", novel_lines_v) # \\d = any digit
novel_lines_v[chap_positions_v]
tail(novel_lines_v)
chap_positions_v <- c(chap_positions_v, length(novel_lines_v))
chapter_raws_l <- list()
chapter_freqs_l <- list()

for(i in 1:length(chap_positions_v)){
  if(i != length(chap_positions_v)){
    chapter_title <- novel_lines_v[chap_positions_v[i]]
    start <- chap_positions_v[i] + 1
    end <- chap_positions_v[i + 1] - 1
    chapter_lines_v <- novel_lines_v[start:end]
    chapter_words_v <- tolower(paste(chapter_lines_v, collapse = " "))
    chapter_words_l <- strsplit(chapter_words_v, "\\W")
    chapter_word_v <- unlist(chapter_words_l)
    chapter_word_v <- chapter_word_v[which(chapter_word_v != "")]
    chapter_freqs_t <- table(chapter_word_v)
    chapter_raws_l[[chapter_title]] <- chapter_freqs_t
    chapter_freqs_t_rel <- 100*(chapter_freqs_t/sum(chapter_freqs_t))
    chapter_freqs_l[[chapter_title]] <- chapter_freqs_t_rel
  }
  