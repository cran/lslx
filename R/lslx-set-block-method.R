## \code{$free_block()} / \code{$penalize_block()} / \code{$fix_block()} sets all target parameters as FREE / PENALIZED / FIXED. ##
lslx$set("private",
         "set_block",
         function(block,
                  group,
                  action,
                  type,
                  verbose = TRUE) {
           if (!all(
             block %in% c(
               "y<-1",
               "f<-1",
               "y<-f",
               "y<-y",
               "f<-y",
               "f<-f",
               "y<->f",
               "f<->y",
               "y<->y",
               "f<->f"
             )
           )) {
             stop("Argument 'block' contains invalid relation. Please check.")
           }
           
           if (missing(group)) {
             group <-  private$model$name_group
           } else if (!all(group %in% private$model$name_group)) {
             stop(
               "Argument 'group' contains unknown group name.",
               "\n  Group name(s) currently recognized by 'lslx' is ",
               do.call(paste, as.list(private$model$name_group)),
               ".",
               "\n  Group name specified in 'group' is ",
               do.call(paste, as.list(group)),
               "."
             )
           }
           if (missing(type)) {
             type <- c("free", "pen")
             type_fixed <- c("fixed")
           } else if ("fixed" %in% type) {
             type <- type[!(type %in% "fixed")]
             type_fixed <- c("fixed")
           } else {
             type <- type
             type_fixed <- NULL
           }
           
           if (length(type) > 0) {
             relation <-
               private$model$specification[(private$model$specification$block %in% block) &
                                             (private$model$specification$group %in% group) &
                                             (private$model$specification$type %in% type), ]
             name_relation <- row.names(relation)
           } else {
             name_relation <- NULL
           }
           
           if (length(type_fixed) > 0) {
             relation_all <- lapply(block, function(i_block) {
               variable_type <-
                 substring(block, 1:nchar(i_block), 1:nchar(i_block))[c(1, nchar(i_block))]
               operator <-
                 do.call(paste0, as.list(substring(
                   i_block, 1:nchar(i_block), 1:nchar(i_block)
                 )[-c(1, nchar(i_block))]))
               right_variable <- switch(
                 variable_type[1],
                 "y" = private$model$name_response,
                 "f" = private$model$name_factor,
                 "1" = 1
               )
               left_variable <- switch(
                 variable_type[2],
                 "y" = private$model$name_response,
                 "f" = private$model$name_factor,
                 "1" = 1
               )
               relation <-
                 paste0(
                   expand.grid(right_variable, operator, left_variable)[, 1],
                   expand.grid(right_variable, operator, left_variable)[, 2],
                   expand.grid(right_variable, operator, left_variable)[, 3]
                 )
               return(relation)
             })
             relation_all <-
               paste0(expand.grid(unlist(relation_all), group)[, 1],
                      "|",
                      expand.grid(unlist(relation_all), group)[, 2])
             
             to_be_fixed <-
               private$model$specification[(private$model$specification$block %in% block) &
                                             (private$model$specification$group %in% group) &
                                             (private$model$specification$type == "fixed"), ]
             fixed_not_zero <-
               to_be_fixed[(to_be_fixed$type == "fixed") &
                             (to_be_fixed$start != 0), ]
             name_fixed <-
               union(setdiff(relation_all, row.names(private$model$specification)),
                     setdiff(rownames(to_be_fixed), rownames(fixed_not_zero)))
           } else {
             name_fixed <- NULL
           }
           
           name <- c(name_relation, name_fixed)
           
           if (length(name) == 0) {
             stop(
               "No valid relation or type",
               " under group ",
               do.call(paste, as.list(group)),
               " is found. Please check the settings."
             )
           } else {
             private$set_coefficient(name = name,
                                     action = action,
                                     verbose = verbose)
           }
         })

lslx$set("public",
         "free_block",
         function(block,
                  group,
                  type,
                  verbose = TRUE) {
           private$set_block(
             block = block,
             group = group,
             type = type,
             action = "free",
             verbose = verbose
           )
         })

lslx$set("public",
         "fix_block",
         function(block,
                  group,
                  type,
                  verbose = TRUE) {
           private$set_block(
             block = block,
             group = group,
             type = type,
             action = "fix",
             verbose = verbose
           )
         })


lslx$set("public",
         "penalize_block",
         function(block,
                  group,
                  type,
                  verbose = TRUE) {
           private$set_block(
             block = block,
             group = group,
             type = type,
             action = "penalize",
             verbose = verbose
           )
         })