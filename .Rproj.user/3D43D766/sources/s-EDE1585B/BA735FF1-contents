

# COMMANDS ----------------------------------------------------------------
CommandHandler_text <- function(command,message){
  CommandHandler(command, function(bot, update) {
    log_update(update,"CommandHandler_text")
    bot$sendMessage(
      update$message$chat_id,
      text = message
    )
  })
}


# ERRORS ------------------------------------------------------------------
error_callback <- function(bot, error) {
  warning(simpleWarning(conditionMessage(error), call = "Updates polling"))
}


error_callback<-error_callback


# SET CHAT LOCATION ---------------------------------------------------------------
# assumes that a 'network' object exists
# stores a homebase location set by user
set_homebase <- function(bot, update) {
  log_update(update,"set_homebase")
  # check inputs
  
  # if(is.null(update$effective_user()$username)){
  #   bot$sendMessage(update$effective_chat()$id,"To use this bot, you need to set a Telegram username in Setting -> Username")
  # }
  if(!exists("network")){stop("there must be a global object called 'network'")}
  if (is.null(update$message$venue$title)) {
    bot$sendMessage(chat_id = update$message$chat_id, 
                    text = "you have to click on a public place shown on the telegram app when sharing a location. You might have to click the 'show nearby places' button first.")
    return(NULL)
    
  }
  
  # update global chats
  network$chats <<- new_chats(
    chat.id = as.numeric(update$message$chat$id),
    location.latitude = update$message$location$latitude,
    location.longitude = update$message$location$longitude,
    venue.title = update$message$venue$title,
    username = update$message$from$username,
    user_id = as.character(update$effective_user()$id),
    blocked = FALSE,
    role = "regular",
    listening_radius = 10,
    add_to = network$chats
    
  )
  
  # give feedback to user
  
  bot$sendMessage(
    chat_id = update$message$chat_id,
    text = paste(
      "OK,",
      update$message$venue$title,
      "is set as your homebase. You can change it anytime if you send me another location. Send a message here that you want to spread to people near your homebase!"
    )
  )
  print(network$chats)
}

# object that stores and manages chat locations



# USE LOCATIONS --------------------------------------------------------


get_this_node <- function(update) {
  this_index <- match(update$message$chat_id, network$chats$chat.id)
  if (is.na(this_index)) {
    return(NULL)
  }
  network$chats[this_index, ]
}




make_location_id<-function(distances,venue.title){
  paste(round(distances,3),"---------",venue.title)
}


nodes_in_radius<-function(chat_id){
  
  this_index <- match(chat_id, network$chats$chat.id)
  if (is.na(this_index)) {
    return(new_chats())
  }
  this_lat  <- network$chats$location.latitude[this_index]
  this_lon <- network$chats$location.longitude[this_index]
  allchats<-network$chats
  allchats$distances <-
    distm(
      c(this_lon, this_lat),
      cbind(network$chats$location.longitude, network$chats$location.latitude),
      fun = distHaversine
    ) %>% as.vector
  allchats$location_id<-make_location_id(allchats$distances,allchats$venue.title)
  places<-allchats %>%
    group_by(location_id) %>%
    summarise(distances = first(distances)) %>% 
    ungroup %>% 
    mutate(distance_rank = rank(distances))
  
  allchats$distance_rank <- places$distance_rank[match(allchats$location_id,places$location_id)]
  chats_reached<-allchats %>% filter(distance_rank <= listening_radius)
  chats_reached$reached <- NULL
  chats_reached$distance_rank <- NULL
  chats_reached$location_id <- NULL
  return(chats_reached %>% filter(chat.id != chat_id) )
}

# nodes_in_radius_distance_meters <- function(update) {
#   # everyone (for testing)
#   this_index <- match(update$message$chat_id, network$chats$chat.id)
#   if (is.na(this_index)) {
#     return(NULL)
#   }
#   this_lat  <- network$chats$location.latitude[this_index]
#   this_lon <- network$chats$location.longitude[this_index]
#   distances <-
#     distm(
#       c(this_lon, this_lat),
#       cbind(network$chats$location.longitude, network$chats$location.latitude),
#       fun = distHaversine
#     ) %>% t
#   locationswithdistance <-
#     cbind(network$chats[, c('chat.id', 'venue.title')], distances)
#   
#   max_dist<-listening_radius_to_meters(network$chats$listening_radius[this_index])
#   locationswithdistance %>% filter(chat.id != update$message$chat_id) %>% head(max_dist) 
#   
# }

venue_title_from_chat_id <- function(x) {
  this_index <- match(x, network$chats$chat.id)[1]
  network$chats$venue.title[this_index] %>% gsub("\"", "", .)
}

username_from_chat_id <- function(x) {
  this_index <- match(x, network$chats$chat.id)[1]
  network$chats$username[this_index] %>% gsub("\"", "", .)
}

accidentall_flag_as_message<-function(x){
  print(x)
  if(grepl("^flag",x)){return(TRUE)}
  if(grepl("\\\\flag",x)){return(TRUE)}
  return(FALSE)
  
}



send_to_admin_inbox<-function(bot,update,type,about_username = NA, time = as.character(Sys.time())){
  if(!exists("network")){stop("global object admin_inbox must exist")}
  network$admin_inbox <<- new_admin_inbox(from_chat_id = as.character(update$effective_chat()$id),
                                          about_username = about_username,
                                          message =  update$message$text,
                                          type = type,time = time,
                                          add_to = network$admin_inbox)
  
  bot$sendMessage(
    network$sysadmin_chat_id,
    text = paste0(type,' from user ',update$message$from$username,":\n\n",update$message$text))
  
  
  return(invisible(NULL))
  
}

# feedback ----------------------------------------------------------------

receive_feeback<-function(bot,update){
  log_update(update,"receive_feedback")
  return_message <- "Thanks! We review all feedback that we receive."
  if(nchar(update$message$text)<29){
    return_message<-"Feedback needs to be at least 20 characters."
  }else{
    send_to_admin_inbox(bot,update,"feedback")
  }
  bot$sendMessage(
    update$message$chat_id,
    text = return_message
  )
}




# listen ------------------------------------------------------------------

handle_command_listen<-CommandHandler("listen",callback = function(bot,update){
  
  
  bot$sendMessage(update$message$chat$id, "From how many nearby places do you want to receive messages?", reply_markup = keyboard_listen,parse_mode = "Markdown")
})



# log ---------------------------------------------------------------------
log_update<-function(update,handler_name){
  network$update_log <<- 
    c(network$update_log,
      list(list(update=update,
           handler=handler_name
           ))
      )
  }






