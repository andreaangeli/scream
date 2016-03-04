#' ---
#'   output: github_document
#'   title: Get replies and quotes of a tweet
#' ---

#' ![](troubleshooting-tips-tweet-smaller.png)

#' I [tweeted some #rstats troubleshooting
#' tips](https://twitter.com/JennyBryan/status/704779515558400000), that were at
#' least semi-serious. It seemed to strike a chord. As [Clint Weathers aka
#' \@zenrhino](https://twitter.com/zenrhino/status/704791544054722564) pointed
#' out, there is solace in "shared suffering". The replies are pretty funny and
#' wise, so this was a good excuse to make my first -- and possibly last! --
#' foray into the Twitter API, in order to get them.

#' Load some packages.

library(twitteR)
library(purrr)
suppressMessages(library(dplyr))
library(stringr)
library(googlesheets)

#' I used the `twitteR` package
#' ([CRAN](https://cran.r-project.org/web/packages/twitteR/index.html),
#' [GitHub](https://github.com/geoffjentry/twitteR)) to access the [Twitter REST
#' API](https://dev.twitter.com/rest/public). The
#' [vignette](http://geoffjentry.hexdump.org/twitteR.pdf) contains some setup
#' information. FWIW: I found that was necessary to set the callback URL for the
#' app to `http://127.0.0.1:1410`. I put the various pieces of secret
#' information in a file to keep them out of this script.

source("secrets.R")
setup_twitter_oauth(consumer_key = to$ck, consumer_secret = to$cs,
                    access_token = to$at, access_secret = to$as)

#' Hi, me!
jenny <- getUser("jennybryan")
(jenny_id <- jenny$getId())

#' Find the [tweet of
#' interest](https://twitter.com/JennyBryan/status/704779515558400000).
zz <- searchTwitter('from:jennybryan+troubleshooting', n = 5)
(target_tweet <- zz[[1]])
(target_tweet_id <- target_tweet$getId())

#' How do I get all the replies? Jeroen put me onto this [SO
#' thread](http://stackoverflow.com/questions/2693553/replies-to-a-particular-tweet-twitter-api),
#' which suggests the API doesn't really support that. But it does contain
#' constructive advice for a workaround:
#'
#'   * get the user's id
#'   * get tweets from that user's mentions_timeline
#'   * get the id of tweet of interest
#'   * filter user's mentions for tweets where `in_reply_to_status_id` matches
#'   this id
#'
#' Let's try that.
mt <- mentions(n = 200, sinceID = target_tweet_id)
length(mt)
tail(mt)

#' A couple of helper functions. Nothing to see here.
map_chr2 <- function(x, .f, ...) {
  map(x, .f, ...) %>% map_if(is_empty, ~ NA_character_) %>% flatten_chr()
}
ellipsize <- function(x, n = 20) {
  ifelse(str_length(x) > n,
         paste0(str_sub(x, end = n - 1), "\u2026"),
         str_sub(x, end = n)) %>%
    str_pad(n)
}

#' Put the mention tweets in a data frame.
#' Pull out `replyToSID`.
#' Filter for the target tweet.
df <- data_frame(mt = mt) %>%
  mutate(replyToSID = mt %>% map_chr2("replyToSID")) %>%
  filter(replyToSID == target_tweet_id)
df %>%
  mutate(id = mt %>% map_chr2("id"),
         screenName = mt %>% map_chr2("screenName"),
         text = mt %>% map_chr2("text")) %>%
  mutate(text = text %>% substr(13, 140) %>% trimws() %>% ellipsize(30)) %>%
  select(-replyToSID, -mt)

#' That filter may be too draconian. Some of the tweets I want to include are
#' replies to replies. Those *should* still show up in my mentions so I think
#' I'll just keep all mentions that are recent enough and manually curate.
df <- data_frame(mt = mt) %>%
  mutate(replyToSID = mt %>% map_chr2("replyToSID"))
mentions <- df %>%
  mutate(id = mt %>% map_chr2("id"),
         screenName = mt %>% map_chr2("screenName"),
         text = mt %>% map_chr2("text")) %>%
  select(-replyToSID, -mt)
mentions %>%
  mutate(text = text %>% substr(13, 140) %>% trimws() %>% ellipsize(37))

#' Clearly I will be in for some pain if I need to work with text with emoji.
#' Hopefully those will get filtered out and I can ignore this problem!
#'
#' Write `mentions` to a Google Sheet for manual curation. I tried with Excel
#' locally but it mangled the tweet ids and line endings, as usual, whereas
#' Google did not.

# initial creation
# ss <- gs_new("mentions_for_manual_curation", trim = TRUE,
#              input = mentions %>%
#                mutate(keep = FALSE) %>%
#                select(id, screenName, keep, text))
ss <- gs_title("mentions_for_manual_curation")
ss %>% gs_browse()
(n_curated <- ss$ws$row_extent - 1) # there's a header row
if (nrow(mentions) > n_curated && interactive()) {
  mentions_curated <- ss %>%
    gs_read(col_types = "cclc")
  mentions_for_curation <- mentions %>%
    left_join(mentions_curated %>% select(id, keep)) %>%
    select(id, screenName, keep, text)
  ss <- ss %>%
    gs_edit_cells(input = mentions_for_curation)
  message("Tweets needing a keep decision: ",
          sum(is.na(mentions_for_curation$keep)))
  ## I manually worked on the `keep` column in the browser at this point
  mentions <- ss %>%
    gs_read(col_types = "cclc") %>%
    filter(keep)
}

#' I also noticed that anyone who *quoted* the tweet wasn't showing up in the
#' mentions. How do I get those tweets? Because the added comments are basically
#' the same as these replies. Back to
#' [stackoverflow](http://stackoverflow.com/questions/31373259/how-to-find-all-retweet-with-comments-for-a-particular-tweet-using-api)!
#' More API disappointment, more constructive workarounds:
#'
#'   * Get the "short url" for the original tweet.
#'   * Search for tweets containing that.
#'   * I may also need to search/filter for tweets that have the target tweet in
#'   `in_reply_to_status_id`?
#'
#' What is this "short url" for my target tweet?
str(target_tweet)
target_tweet$getUrls()
#' It doesn't seem like I have it.
#'
#' Is `twitteR` really returning everything the API gives us? Let's `curl --get`
#' this tweet and leave `twitteR` out of it. I used the twitter API OAuth Tool
#' to compose this beauty. Accessible by selecting your app in the OAuth
#' Signature Generator drop down
#' [here](https://dev.twitter.com/rest/reference/get/statuses/show/%3Aid).

## curl --get 'https://api.twitter.com/1.1/statuses/show.json' --data
## 'id=704779515558400000' --header 'Authorization: OAuth
## oauth_consumer_key="???",
## oauth_nonce="???",
## oauth_signature="???",
## oauth_signature_method="HMAC-SHA1", oauth_timestamp="1457043820",
## oauth_token="???",
## oauth_version="1.0"' --verbose > target_tweet.json
target_tweet_curl <- jsonlite::fromJSON("target_tweet.json")
names(target_tweet_curl)

#' I'm not having any better luck than before.
target_tweet_curl$entities$url

#' Ok there's this but pretty sure it's just for the accompanying image.
target_tweet_media_url <- target_tweet_curl$entities$media$url
if (interactive()) browseURL(target_tweet_media_url)

#' I will `curl --get` [a tweet that quoted
#' mine](https://twitter.com/ByerlyElizabeth/status/705032096838844417) and see
#' if I get my own short url there. Used same approach as above.
quote_tweet_curl <- jsonlite::fromJSON("example_quote.json")
names(quote_tweet_curl)
quote_tweet_curl$quoted_status_id_str
identical(quote_tweet_curl$quoted_status_id_str, target_tweet_id)

#' Here is my target tweet's short url: <https://t.co/ehskwqtZPf>. Yes this
#' seems to link to my target tweet. Good.
(target_tweet_short_url <- quote_tweet_curl$entities$urls$url)
if (interactive()) browseURL(target_tweet_short_url)

#' How do I search for tweets whose text contains my short url?
(st <- searchTwitter(target_tweet_short_url))

#' Ugh, I only get that one tweet. Note from the future: It turns out these
#' short urls are unique to the quoting tweet, so this approach doesn't actually
#' work. I proved this by looking another quote tweet and it had an entirely
#' different short URL for the target tweet.
#'
#' The [Streaming
#' API](https://twittercommunity.com/t/quote-tweet-events-in-the-streaming-api/38457)
#' *does* seem to offer usable information on quoted tweets. That is wrapped by
#' yet another package:
#' [`streamR`](https://cran.r-project.org/web/packages/streamR/). But it was
#' last updated in January 2014 and [this blog
#' post](http://bogdanrau.com/blog/collecting-tweets-using-r-and-the-twitter-streaming-api/)
#' mentions a bunch of packages that suggest it may not be terribly current.
#' There is also no vignette. For now, I will just capture the URLs and
#' therefore ids of these quote tweets manually. They show up in Mentions in
#' Tweetbot and are easy to pick out because of the image.
qt <- readLines("quote_tweet_urls.txt") %>%
  basename() %>%
  lookup_statuses()
quotes <- data_frame(qt = qt) %>%
  mutate(id = qt %>% map_chr2("id"),
         screenName = qt %>% map_chr2("screenName"),
         text = qt %>% map_chr2("text")) %>%
  select(-qt)
quotes %>%
  mutate(text = text %>% ellipsize(37))

#' Combine true replies and quotes.
tweets <- mentions %>%
  bind_rows(quotes)
tweets %>%
  mutate(text = text %>% ellipsize(37))

#' Write them out.
write.csv(tweets, "tweets.csv", row.names = FALSE)
saveRDS(tweets, "tweets.rds")
