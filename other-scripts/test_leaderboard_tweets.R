library(rtweet)

## store api keys
twitter_creds <- config::get("twitter", file = "/home/balco/my_rconfig.yml")

## authenticate via web browser
token <- create_token(
  app = twitter_creds$app_name,
  consumer_key = twitter_creds$api_key,
  consumer_secret = twitter_creds$secret_api_key,
  access_token = twitter_creds$access_token,
  access_secret = twitter_creds$secret_access_token)

tokenn = get_token()

x = post_tweet(
  status = 'test tweet please ignore',
  #media = '/home/balco/dev/lor-meta-report/templates/tweet-plots/leaderboard.png',
  token = tokenn
)

#You currently have Essential access which includes access to Twitter API v2 endpoints only. 
#If you need access to this endpoint, you’ll need to apply for Elevated access via the Developer Portal. 
#You can learn more here: https://developer.twitter.com/en/docs/twitter-api/getting-started/about-twitter-api#v2-access-leve

# make main tweet with top 10
# reply with 3 biggest increases, and decks played in past 24 hours