#' Create Remote Driver
#'
#' Create remote driver for navigation
#'
#' @param browser Browser (default is set to 'firefox')
#' @param port Port (default is set to 4445)
#'
#' @export

create_remotedriver <- function(browser = "firefox", port = 4445){

	# system('docker run -d -p 4445:4444 selenium/standalone-firefox')

	remdr <- RSelenium::remoteDriver(
		remoteServerAddr = "localhost",
		port = as.integer(port),
		browserName = browser
	)
}
