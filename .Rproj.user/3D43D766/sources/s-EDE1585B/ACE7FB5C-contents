

keyboard_listen <- InlineKeyboardMarkup(
  inline_keyboard = list(
    list(InlineKeyboardButton("only my homebase", callback_data = "command_listen 1")),
    list(
      InlineKeyboardButton("10", callback_data = "command_listen 10"),
      InlineKeyboardButton("100", callback_data = "command_listen 100"),
      InlineKeyboardButton("1000", callback_data = "command_listen 1000")
    ),
    list(InlineKeyboardButton("everywhere", callback_data = "command_listen Inf"))
  )
)


keyboard_shared_nearby <- function(message_identifier){
  #   
  InlineKeyboardMarkup(
    inline_keyboard = 
      list(
        list(
          InlineKeyboardButton("share here", callback_data = paste0("s_n share --- ",message_identifier)),
        InlineKeyboardButton("more", callback_data = paste0("s_n more --- ",message_identifier))
      )
  )
  )
}

keyboard_shared_nearby_more <- function(message_identifier, user_role){
  #   
  InlineKeyboardMarkup(
    inline_keyboard = 
      list(
        list(
          InlineKeyboardButton("share here", callback_data = paste0("s_n share --- ",message_identifier)),
          InlineKeyboardButton("flag", callback_data = paste0("s_n flg --- ",message_identifier))
        )
      )
  )
}



# inline_keyboard = list(
#   list(InlineKeyboardButton("share here", callback_data = paste0("s_n share --- ",message_identifier))),
#   list(InlineKeyboardButton("flag", callback_data = paste0("s_n flg --- ",message_identifier))
#   )
# )


answer_cb <- function(bot, update) {
  
  user_response <- update$callback_query$data
  if(grepl("^command_listen ",user_response)){
    answer_cb_command_listen(bot,update)
  }
  if(grepl("^s_n ",user_response)){
    answer_cb_command_share_nearby(bot, update)
  }
  tryCatch({bot$answerCallbackQuery(callback_query_id = update$callback_query$id)})
  
}

answer_cb_command_listen<-function(bot,update){
  data <- gsub("command_listen ","",update$callback_query$data)
  chat_id<-match(update$effective_chat()$id,network$chats$chat.id)
  network$chats$listening_radius[chat_id]<<-as.numeric(data)
  network$chats
  
  bot$sendMessage(chat_id = update$effective_chat()$id,
                  text = paste0("done!"))
  tryCatch({bot$answerCallbackQuery(callback_query_id = update$callback_query$id)},
           error = function(e){
             bot$sendMessage(chat_id = update$effective_chat()$id,
                             text = paste0("couldn't set listening distances, please send /listen again."))
             
           })
}






answer_cb_command_share_nearby<- function(bot, update){
  if(grepl("s_n share",update$callback_query$data)){
    global_identifier <- gsub("s_n share --- ","", update$callback_query$data)
    send_to_chats<-nodes_in_radius(update$effective_chat()$id)
    send_formatted_shared_message(bot,
                                  send_to_chats = send_to_chats,
                                  from_chat_id = update$effective_chat()$id,
                                  original_global_message_id = global_identifier)
    share_nearby_inform_sender(bot,update,send_to_chats)
  }
  
  if(grepl("s_n flg",update$callback_query$data)){
    # flag_user(bot,update) doesnt work because message not orginal message.
    bot$sendMessage(update$effective_chat()$id,text = "please type /flag and the name of the user you want to flag.")
    
  }
  
  if(grepl("s_n more",update$callback_query$data)){
    global_identifier <- gsub("s_n more --- ","", update$callback_query$data)
    bot$edit_message_reply_markup(chat_id = update$from_chat_id(),
                                  message_id = update$callback_query$message$message_id,
                                  reply_markup =  keyboard_shared_nearby_more(global_identifier))
    
  }
}








