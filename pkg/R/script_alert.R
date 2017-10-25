#' Set up script alert
#' 
#' Creates a file in the users home directory for future use.
#'
#' @return \code{NULL}
#' @export
set_up_script_alert <- function(){
  message(paste("The security of this procedure is somewhat dubious.",
          "It is therefore recommended that you use a special email", 
          "address for sending alerts that is NOT for personal or",
          "other professional."))
  address_from <- readline(prompt = "Enter full email address where messages will originate (unsecure): ")
  pwd <- readline(prompt = "Enter password (unsecure): ")
  address_to <- readline(prompt = "Enter full email address where messages will go (secure, I think): ")
  writeLines(text = paste(address_from, ":", pwd, "\n", address_to, sep = ""), con = "~/.secret_stuff")
  message(paste("A file called ~/.secret_stuff has been created that stores",
          "the provided information for future use. This is why this",
          "isn't super secure."))
}

#' Initialization email alert
#'
#' @param text Optional text to include in email.
#' @param address_to email address where message will be sent. Default is that provided when running \code{set_up_script_alert()}.
#' @param script_name character string giving name of script.
#' @param message_subject subject of email message. Default is job name, start time, and server hostname.
#'
#' @return message subject
#' @export
initial_alert <- function(text = NULL, address_to = NULL, script_name = NULL, message_subject = NULL){
  start_time <- Sys.time()
  initial_text <- paste('Your script ', script_name, 
                        'began running on ', hostname, 
                        ' at ', start_time, '.\n', text, 
                        sep = '')
  msg_subj <- gen_alert(text = initial_text, address_to = address_to, 
                        script_name = script_name, message_subject = message_subject)
  return(msg_subj)
}

#' Error email alert
#'
#' @param text Optional text to include in email.
#' @param address_to email address where message will be sent. Default is that provided when running \code{set_up_script_alert()}.
#' @param script_name character string giving name of script.
#' @param message_subject subject of email message. Default is job name and server hostname.
#'
#' @return message subject
#' @export
error_alert <- function(text = NULL, address_to = NULL, script_name = NULL, message_subject = NULL){
  crash_time <- Sys.time()
  error_text <-  paste('Your script ', script_name, 
                       'has thrown an error on ', hostname, 
                       ' at ', crash_time, '. Sorry.\n\n', text, 
                       sep = '')
  msg_subj <- gen_alert(text = error_text, address_to = address_to, 
                        script_name = script_name, message_subject = message_subject)
  return(msg_subj)
}

#' Completion email alert
#'
#' @param text Optional text to include in email.
#' @param address_to email address where message will be sent. Default is that provided when running \code{set_up_script_alert()}.
#' @param script_name character string giving name of script.
#' @param message_subject subject of email message. Default is job name, start time, and server hostname.
#'
#' @return message subject
#' @export
finish_alert <- function(text = NULL, address_to = NULL, script_name = NULL, message_subject = NULL){
  finish_time <- Sys.time()
  finish_text <- paste('Your script ', script_name, 
                'has completed on ', hostname, 
                ' at ', finish_time, '.\n\n', text, 
                sep = '')
  msg_subj <- gen_alert(text = finish_text, address_to = address_to, 
                        script_name = script_name, message_subject = message_subject)
  return(msg_subj)
}

#' Generic email alert
#'
#' @param text Optional text to include in email.
#' @param address_to email address where message will be sent. Default is that provided when running \code{set_up_script_alert()}.
#' @param script_name character string giving name of script.
#' @param message_subject subject of email message. Default is job name, start time, and server hostname.
#'
#' @return message subject
#' @export
gen_alert <- function(text = NULL, address_to = NULL, script_name = NULL, message_subject = NULL){
  info <- readLines(con = "~/.secret_stuff")[1]
  if(is.null(address_to)) address_to <- readLines(con = "~/.secret_stuff")[2]
  address_from <- as.character(strsplit(info, ":")[[1]][1])
  hostname <- system(command = 'hostname', intern = T)
  username <- system(command = 'id -un', inter = T)
  if(!is.null(script_name)) script_name <- paste(script_name, " ", sep = "")
  finish_time <- Sys.time()
  if(is.null(message_subject)){
    message_subject <- paste('job ', script_name, 'on ', hostname, sep = '')
  }
  if(is.null(text)) text <- "No text supplied to gen_alert()."
  writeLines(text = paste('From: "', username, '@', hostname, '" <', address_from, '>\n', 
                          'To: <', address_to, '>\n',
                          'Subject: ', message_subject, '\n',
                          'Message-ID: <', username, '@', hostname, '>\n\n',
                          text, sep = ''), con = 'message.txt')
  system(
    paste("curl",
          "--url 'smtps://smtp.gmail.com:465'", 
          "--ssl-reqd", 
          "--mail-from", address_from,
          "--mail-auth", address_from,
          "--mail-rcpt", address_to,
          "--upload-file message.txt",
          "--user", info,
          "--insecure")
  )
  system('rm message.txt')
  return(message_subject)
}