#Examples
library(RSelenium)
## Not run: 
# start the server if one isnt running
driver <- rsDriver()

# use default server initialisation values
#remDr <- remoteDriver$new()

# send request to server to initialise session
#remDr$open()

# navigate to R home page
driver$client$navigate("http://www.r-project.org")

# navigate to www.bbc.co.uk notice the need for http://
driver$client$navigate("http://www.bbc.co.uk")

# go backwards and forwards
driver$client$goBack()

driver$client$goForward()

driver$client$goBack()

# Examine the page source
frontPage <- driver$client$getPageSource()

# The R homepage contains frames
webElem <- driver$client$findElements(value = "//frame")
sapply(webElem, function(x) {x$getElementAttribute('name')})

# The homepage contains 3 frames: logo, contents and banner
# switch to the `contents` frame
webElem <- driver$client$findElement(using = 'name', value = 'contents')
remDr$switchToFrame(webElem$elementId)

# re-examine the page source

contentPage <- remDr$getPageSource()
identical(contentPage, frontPage) # false we hope!!

# Find the link for the search page on R homepage. Use xpath as default.
webElem <- driver$client$findElement(value = '//a[@href = "search.html"]')
webElem$getElementAttribute('href')
# http://www.r-project.org/search.html

# click the search link
webElem$clickElement()

# FILL OUT A GOOGLE SEARCH FORM
driver$client$navigate("http://www.google.com")

# show different methods of accessing DOM components

webElem1 <- driver$client$findElement(using = 'name', value = 'q')
webElem2 <- driver$client$findElement(
  using = 'id',
  value = webElem1$getElementAttribute('id')[[1]])
webElem3 <- remDr$findElement(using = 'xpath',
                              value = '//input[@name = "q"]')

# Enter some text in the search box

webElem1$sendKeysToElement(list('RSelenium was here'))

# clear the text previously entered

webElem1$clearElement()

# show an example of sending a key press
webElem1$sendKeysToElement(list('R', key = 'enter'))

# Collate the results for the `R` search
googLinkText <- remDr$findElements(value = "//h3[@class = 'r']")
linkHeading <- sapply(googLinkText, function(x) x$getElementText())
googLinkDesc <- remDr$findElements(value = "//div[@class = 's']")
linkDescription <- sapply(googLinkDesc, function(x) x$getElementText())
googLinkHref <- remDr$findElements(value = "//h3[@class = 'r']/a")
linkHref <- sapply(googLinkHref,
                   function(x) x$getElementAttribute('href'))

data.frame(heading = linkHeading,
           description = linkDescription, href = linkHref)

# Example of javascript call
remDr$executeScript("return arguments[0] + arguments[1];", args = 1:2)
# Example of javascript async call
jsscript <-
  "arguments[arguments.length - 1](arguments[0] + arguments[1]);"
remDr$executeAsyncScript(jsscript, args = 1:2)

# EXAMPLE INJECTING INTO PHANTOMJS using phantomExecute
require(RSelenium)
pJS <- wdman::phantomjs(port = 4932L)
remDr <- remoteDriver(browserName = "phantomjs", port = 4932L)
remDr$open(silent = TRUE)
remDr$navigate("http://ariya.github.com/js/random/")
# returns a set of random numbers
remDr$findElement("id", "numbers")$getElementText()[[1]]
result = remDr$phantomExecute("var page = this;
                               page.onInitialized = function () {
                               page.evaluate(function () {
                               Math.random = function() {return 42/100}
                               })
                               }", list());
remDr$navigate("http://ariya.github.com/js/random/")
# Math.random returns our custom function
remDr$findElement("id", "numbers")$getElementText()[[1]]
remDr$close()
pJS$stop()

## End(Not run)

