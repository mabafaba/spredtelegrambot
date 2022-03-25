

flag_user<-function(bot, update){
  log_update(update,"receive_feedback")  
  
  flagging_failed <- function(message){
    bot$sendMessage(
      update$message$chat_id,
      text = message
    )
  }
  
  flagged_successfully <- function(username){
    bot$sendMessage(
      update$message$chat_id,
      text = paste("the user", username, "has been flagged to the moderators of this network. If their messages violate the community /rules, they will be banned.")
    )
  }
  
  flag_username <- str_extract_all(update$message$text,"@[A-z0-9_]*") %>% unlist
  
  if(length(flag_username)==0){
    flagging_failed("please mention the name of the user you want to flag. Your message should look like this:\n /flag @username optional message to network moderators")
  return(invisible(NULL))
    }
  
  if(length(flag_username)>1){
    flagging_failed("Please mention only one user that you want to flag. Your message should look like this:\n /flag @username optional message to network moderators")
    return(invisible(NULL))
    }
  
  if(!(
    (flag_username %>% gsub("@","",.) %in% network$chats$username)
  )
  ){
    
    flagging_failed(paste("can not flag user", flag_username,"because they have not sent any messages on this network."))
    return(invisible(NULL))
    
  }

  message_text<-update$message$text %>% gsub("/flag","",.,fixed = T) %>% gsub(flag_username,"",.,fixed = T)
  
    send_to_admin_inbox(bot,update,type = 'flag',about_username = flag_username)
  print(network$admin_inbox)
  flagged_successfully(flag_username)
  invisible(NULL)
}






