
user_name<-function(update){
  username<-update$effective_user()$username
  if(is.null(username)){
    username<-paste(update$effective_chat()$first_name,update$effective_chat()$last_name)
  }
  username
}

new_network<-function(name = "network",
                      token = get_spred_token(),
                      chats=new_chats(),
                      admin_inbox = new_admin_inbox(),
                      network_messages = new_network_messages(),
                      handlers,
                      sysadmin_user_id = "624612504",
                      sysadmin_chat_id = "624612504", 
                      sysadmin_username = "@mmmatin",...){
  
  # network object

  thisnetwork <- list(
    name = name,
    token = token,
    chats = chats,
    admin_inbox = admin_inbox,
    network_messages = network_messages,
    handlers = handlers,
    sysadmin_username = sysadmin_username,
    ...)
  
  # environment
  
  
}



new_network_messages<-function(...){
  messages<-list(...)
  if(length(messages)==0){return(tibble(id = NULL, text = NULL))}
  tibble(id = names(messages),text = unlist(unname(messages)))

}

network_message <- function(id,network){
  
  network$network_messages$text[match(id,network$network_messages$id)]
  
}




new_chats <- function(chat.id = numeric(),
                      location.latitude = numeric(),
                      location.longitude = numeric(),
                      venue.title = character(),
                      username = character(),
                      user_id = character(),
                      blocked = logical(),
                      role= character(),
                      listening_radius = numeric(),
                      add_to = NULL){

  chats <- tibble(
    chat.id,
    location.latitude,
    location.longitude,
    venue.title,
    username,
    user_id,
    blocked,
    role,
    listening_radius = listening_radius)
  if(is.null(chats$username)){chats$username<-chats$user_id}
  # class(chats)<-c(class(chats),"spred_chats")
  
  if(!is.null(add_to)){
    chats <- bind_rows(add_to,chats)
    
  }
  
  chats <- chats %>% group_by(chat.id, user_id) %>% summarise(
    location.latitude = last(location.latitude),
    location.longitude = last(location.longitude),
    venue.title = last(venue.title),
    blocked = last(blocked),
    role = last(role),
    username = last(username),
    listening_radius = first(listening_radius)
  ) %>% ungroup
  
  return(chats)
}



new_admin_inbox <- function(from_chat_id = character(),
                            about_username = character(),
                            message=character(),
                            type = character(),
                            time = character(),
                            add_to = NULL){
  
  inbox<-tibble(from_chat_id,
                about_username,
                message,
                type,
                time)
  
  
  if(!is.null(add_to)){
    inbox <- bind_rows(add_to,inbox)
  }
  
  
  return(inbox)
  
}
