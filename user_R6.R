library(R6)

user_initialiser <- 
  function(effective_user, blocked = FALSE, tier = 1, listening_radius = 10){
    self$id <- effective_user$id
    self$is_bot <- effective_user$is_bot
    self$first_name <- effective_user$first_name
    self$last_name <- effective_user$last_name
    self$username <- effective_user$username
    self$language_code <-effective_user$language_code
    self$blocked <- blocked
    self$tier <- tier
    self$listening_radius <- listening_radius
    
    
  }

User <- R6Class("tlg_user",
                   public = list(
                     id = NULL,
                     is_bot = FALSE,
                     first_name = NULL,
                     last_name = NULL,
                     username = NULL,
                     language_code = NULL,
                     blocked = NULL,
                     tier = 1,
                     listening_radius = 10,
                     
                     show = function() {
                       print(self)
                     },
                     display_label = function(){ifelse(is.null(self$username),paste0("@",self$id),paste0("@",self$username))},
                     block = function(){self$blocked <- TRUE},
                     unblock = function(){self$blocked <- FALSE},
                     set_tier = function(tier){
                       if(!is.numeric(tier)){stop("tier must be numeric")}
                       self$tier<-tier
                       },
                     set_listening_radius = function(listening_radius){
                       if(!is.numeric(listening_radius)){stop("tier must be numeric")}
                       self$listening_radius<-listening_radius
                     },
                     get = function(attribute){
                       self[[attribute]]
                     }
                   ,
                initialize = user_initialiser)
)


