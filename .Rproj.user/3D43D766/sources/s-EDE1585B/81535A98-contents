library(R6)

network_initialiser <- 
  function(name = "network",
           token = get_spred_token(),
           chats=new_chats(),
           admin_inbox = new_admin_inbox(),
           bot_texts = new_network_messages(),
           shared_updates = new_shared_messages(),
           handlers = NULL,
           sysadmin_username = "@mmmatin",
           ...){
    
    self$name = name
    self$token = token
    self$chats = chats
    self$admin_inbox = admin_inbox
    self$bot_texts = bot_texts
    self$shared_udpates = shared_updates
    self$handlers = handlers
    self$sysadmin_username = sysadmin_username
    self$other <- list(...)
    
  }





get_venue.title<-function(username){
  self$chats$venue.title[self$chats$username==username]
}
get_role<-function(username){
  self$chats$role[self$chats$username==username]
}

get_blocked<-function(username){
  self$chats$blocked[self$chats$username==username]
}

get_shared_message<-function(global_message_id){
  network$shared_messages  
}



Network <- R6Class("Network",
                   public = list(
                     name = NULL,
                     token = NULL,
                     chats = new_chats(),
                     admin_inbox = new_admin_inbox(),
                     network_messages = new_network_messages(),
                     handlers = NULL,
                     sysadmin_username = NULL,
                     other = NULL,
                     initialize = network_initialiser,
                     show = function() {
                       print(self$chats)
                     },
                     get = function(attribute){
                       self[[attribute]]
                     },
                     greet = function() {
                       cat(paste0("Hello, my name is ", self$name, ".\n"))
                     }
                   )
)


Network$new()


