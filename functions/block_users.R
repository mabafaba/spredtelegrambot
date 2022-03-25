


# check command access

tier_user_id<-function(user_id){
  if(user_id == network$sysadmin_user_id){return(10)}
  role <- network$chats$role[match(user_id,network$chats$user_id)]
  tier<-1:4
  names(tier)<-c("regular","moderator","admin","sysadmin")
  user_tier<-tier[role]
  user_tier
}

sender_tier<-function(update){
  tier_user_id(as.character(update$effective_user()$id))
}

mentioned_tier <- function(update){
  mentioned_users<-extract_mentioned_username(update) %>% gsub("@","",.)
  lapply(mentioned_users,tier_username) %>% unlist
}

tier_username<-function(username){
  if(username == network$sysadmin_username){return(10)}
  role <- network$chats$role[match(username,network$chats$username)]
  tier<-1:4
  names(tier)<-c("regular","moderator","admin","sysadmin")
  user_tier<-tier[role]
  user_tier
}

command_allowed<-function(user_id,command){
  if(user_id %in% network$sysadmin_user_id){return(TRUE)}
  if(user_id %in% network$blocked_users){return(FALSE)}
  command_tier<-network$commands$tier[match(command,network$commands$command)]
  return(user_tier >= command_tier)
  
}


block_user <- function(bot,update){
  log_update(update,"receive_feedback")
  if(!command_allowed(update$effective_chat()$id,"blockuser")){
    bot$sendMessage(update$message$chat$id,text = "Command not allowed.")
    return(invisible(NULL))
  }
  users_to_block<-extract_mentioned_username(update)
  
  cant_block <- users_to_block[sender_tier(update) <= mentioned_tier(update)]
  if(length(cant_block)>0){
    bot$sendMessage(update$message$chat$id,text = paste0("cannot block users with higher or equal access rights: ",paste0(cant_block, collapse = ", "))) 
    users_to_block<-users_to_block[!(users_to_block%in% cant_block)]
  }
  
  network$blocked_users <<- c(network$blocked_users, gsub("@","",users_to_block)) %>% unique
  bot$sendMessage(update$message$chat$id,text = paste0("blocked users: ",paste0(users_to_block, collapse = ", ")))  
  print(network$blocked_users)
  return(invisible(NULL))
}


unblock_user <- function(bot,update){
  log_update(update,"receive_feedback")
  command_allowed(update$message$from$username,"unblockuser")
  
  if(!command_allowed(update$message$from$username,"blockuser")){
    bot$sendMessage(update$message$chat$id,text = "Command not allowed.")
    return(invisible(NULL))
  }
  
  users_to_unblock<-extract_mentioned_username(update)
  network$blocked_users<-network$blocked_users[!network$blocked_users %in% gsub("@","",users_to_unblock)]
  network$blocked_users <<- c(network$blocked_users, gsub("@","",users_to_unblock)) %>% unique
  bot$sendMessage(update$message$chat$id,text = paste0("UNblocked users: ",paste0(users_to_unblock, collapse = ", ")))  
  print(network$blocked_users)
  return(invisible(NULL))
}


# everyone 1

# /help
# /rules
# /flag
# /feedback

# moderators 2
# /moderatorhelp
# /becomeuser

# /blockuser
# /unblockuser
# /hidemessage
# /unhidemessage
# /showuser

# /tomoderators
# /toadmins

# admin commands 3

# /adminhelp

# /becomoderator
# /becomeuser

# /makemoderator (not admins or sysadmins)
# /makeuser
# /setrules
# /setname
# /sethelp
# /setadminhelp  
# /setstart

# /tomoderators
# /toeveryone
# /toadmins
# /tomodsandadmins

# sysadmin 4

# /makeadmin (anyone but self)
# /makemoderator (anyone but self)
