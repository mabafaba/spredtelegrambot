# to run from terminal in background:
# nohup Rscript main.R &

library(telegram.bot, warn.conflicts = F)
library(magrittr, warn.conflicts = F)
library(purrr, warn.conflicts = F)
library(dplyr, warn.conflicts = F)
library(geosphere,warn.conflicts = F)
library(stringr,warn.conflicts = F)

invisible(lapply(list.files("./functions",full.names = T),source))


# NETWORK SETUP -----------------------------------------------------------

token <- get_spred_token()

network_name = "Kritische Männlichkeit"
start_message = paste0("Hi, this is the *",network_name,"* local network! \n I'll forward Messages you send me to people near your homebase (and the other way around!).\nTo start, send me the location of a public place (cafe, bar, library, ...). That will be your homebase (you can change it anytime by sending me another location).\nIf you send messages here, other users in this network can see the message, along with your user name and your current homebase.")
rules = "Community rules:\n\n 1. Be respectful\n 2. No discrimination \n 3. No spam or off topic advertisement \n 4. no hate\n\nIf someone violates these rules, please /flag them."
command_explanations = "/rules - show network rules\n/flag - flag a user to the moderators\n/feedback - send feedback to moderators"
help = paste(start_message,"\n\n",command_explanations)



network_messages <- new_network_messages(
  network_name = network_name,
  start_message = start_message,
  rules = rules,
  command_explanations = command_explanations,
  help = help
)

start_bot<-function(){
# handling pressed buttons:
query_handler <- CallbackQueryHandler(answer_cb)

handlers<-list(
  handle_command_listen,
  query_handler,
  CommandHandler_text("start", help),
  CommandHandler_text("help",help),
  CommandHandler('flag',callback = flag_user),
  CommandHandler_text('rules',rules),
  CommandHandler('feedback',receive_feeback),
  CommandHandler("block",block_user),
  CommandHandler("unblock", unblock_user),
  MessageHandler(set_homebase,MessageFilters$location),
  MessageHandler(callback = handle_text_share_nearby, MessageFilters$text),
  ErrorHandler(error_callback)
)

commands <- read.csv("data/commands.csv",stringsAsFactors = FALSE) %>% tibble

if(!exists("network")){
  network <- new_network(
                     token = get_spred_token(),
                     chats = new_chats(),
                     sysadmin_user_id = "624612504",
                     network_messages = network_messages,
                     handlers = handlers,
                     sysadmin_chat_id =  "-557669018", # not yet standard for network object  
                     commands = commands,  # not yet standard for network object 
                     blocked_users = character(),
                     update_log = list() # not yet standard for network object        
  )
}

network$handlers<-handlers
updater<-Updater(token = token)
updater <- purrr::reduce(c(updater,network$handlers), `+`)
updater$start_polling()

}


start_bot()


