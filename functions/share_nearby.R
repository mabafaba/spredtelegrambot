handle_text_share_nearby <- function(bot, update) {
  
  if(accidentall_flag_as_message(update$message$text)){
    bot$send_message(chat_id = update$message$chat_id,
                     text = "It looks like you may have tried to flag a user. If you want to flag a user, make sure to start the message with /flag. we did not share this message with the network.")
    
    return(invisible(NULL))
  }
  
  global_message_id <- accept_message_to_share_nearby(bot, update)
  
  
  from_chat_id<-update$effective_chat()$id
  message_text <- update$effective_message()
  original_message_id <- update$message$message_id
  
  send_to_chats <- nodes_in_radius(from_chat_id)
  
  
  
  
  if (is.null(send_to_chats) | nrow(send_to_chats) < 1) {
    if(!(update$effective_chat()$id %in% network$chats$chat.id)){
      bot$send_message(chat_id = update$message$chat_id,
                       text = "Please first set your homebase by sharing the location of a public place.")
    }else{
      bot$send_message(chat_id = update$message$chat_id,
                       text = "Noone is close enough to your homebase to hear your message. You can change your hombase by sharing the location of a public place.")
      # "At the moment, noone is close enough to your homebase to hear your message. Do you want to move your homebase to the nearest location with active users?"
    }
    
    return(NULL)
    
  }
  # bot$sendMessage(chat_id = update$message$chat_id,
  #                 reply_to_message_id = update$message$message_id,
  #                 text = paste('your chat id:', update$message$chat_id)
  #                 # reply_markup = spred_keyboard
  # )
  
  
  
  
  
  send_formatted_shared_message(bot,send_to_chats = send_to_chats,from_chat_id = from_chat_id,original_global_message_id = global_message_id)
  # map2(send_to_chats[, c('chat.id')],send_to_chats[,'distances'], send_formatted_shared_message, keyboard_shared_nearby)
  
  # bot$forwardMessage(chat.id,update$message$chat_id,update$message$message_id)
  
  share_nearby_inform_sender(bot,update,send_to_chats)
  
  
}

# MessageHandler(callback = share_nearby, MessageFilters$text)

global_message_identifier<-function(update){
  paste0("chat::",update$effective_chat()$id," user::", update$effective_user()$username," message::",update$message$message_id)
}


new_shared_messages <- function(
  update = list(),
  global_identifier = character(),
  received_by_chats = character(length(global_identifier)),
  shared_by_users = character(length(global_identifier)),
  time = Sys.time(),
  add_to = NULL
){
  if("Update" %in% class(update)){global_identifier = global_message_identifier(update)}
  
  new_shared_message<-tibble(global_identifier = global_identifier,
                             update = list(update),
                             received_by_chats = received_by_chats,
                             shared_by_users = shared_by_users,
                             time = time
  )
  
  if(nrow(new_shared_message)>1){stop("cant add more than one new_shared_message at once")}
  return(bind_rows(add_to,new_shared_message))
}



accept_message_to_share_nearby<-function(bot,update){
  if(!(global_message_identifier(update) %in% network$shared_messages$global_identifier)){
    network$shared_messages <<- new_shared_messages(update,add_to = network$shared_messages,received_by_chats = as.character(update$effective_chat()$id))  
  }
  return(global_message_identifier(update))
}


share_nearby_inform_sender<-function(bot,update,send_to_chats){
  bot$sendMessage(
    chat_id = update$effective_chat()$id,
    reply_to_message_id = update$message$message_id,
    # text = paste("`shared to:",
    #              collapse_commas_and(c(
    #                as.character(get_this_node(update)$venue.title),
    #                as.character(send_to_chats$venue.title)
    #              )),
    #              "`"),
    text = paste0("`shared with ", nrow(send_to_chats), "`"),
    parse_mode = 'Markdown'
    # reply_markup = spred_keyboard
  )
}




# share_nearby_send_message <- function(.........) # <- no update parameter


send_formatted_shared_message<-function(bot,send_to_chats, from_chat_id, original_global_message_id) {
  
  original_update <- network$shared_messages$update[network$shared_messages$global_identifier==original_global_message_id][[1]]
  
  already_received <- strsplit(network$shared_messages$received_by_chats[network$shared_messages$global_identifier == original_global_message_id]," ") %>% unlist %>% as.numeric
  send_to_chats <- send_to_chats %>% filter(!(chat.id %in% already_received))
  
  if(from_chat_id!=original_update$effective_chat()$id){
    via_prefix <- paste0("@",original_update$effective_user()$username, " `via` ") 
  }else{via_prefix <- ""}
  
  map2(send_to_chats$chat.id,send_to_chats$distances, function(to_chat_id,distance){
    
    bot$sendMessage(
      chat_id = to_chat_id,
      text = paste0(via_prefix, "@",username_from_chat_id(from_chat_id),
                    ' `at ',
                    venue_title_from_chat_id(from_chat_id),
                    " (",format_distance(distance),"):`\n",
                    gsub("`","\\`",original_update$message$text,fixed = TRUE)
      ),
      reply_markup = keyboard_shared_nearby(original_global_message_id),
      parse_mode = 'Markdown'
    )
    
  })
  network$shared_messages$received_by_chats[network$shared_messages$global_identifier==original_global_message_id] <<- 
    paste(
      network$shared_messages$received_by_chats[network$shared_messages$global_identifier==original_global_message_id],
      " ",
      paste(send_to_chats$chat.id,collapse = " ")
    )
  return(invisible(NULL))
}


