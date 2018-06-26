script_name <- "r_script.R"
msg_subj <- script.alert::initial_alert(script_name = script_name)
tryCatch(
  expr = {
    source("path/to/r_script.R", echo = T) ## put the script here
    script.alert::finish_alert(script_name = script_name, message_subject = msg_subj)
  },
  error = function(err){
    message(err)
    script.alert::error_alert(script_name = script_name, message_subject = msg_subj)
  })
