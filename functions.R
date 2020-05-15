
library(cicerone)

download.file(
  url = 'https://cdn.jsdelivr.net/npm/js-cookie@2/src/js.cookie.min.js',
  destfile = 'www/js.cookie.js'
)

addResourcePath("js", "www")


guide <- Cicerone$
  new()$ 
  step(
    el = "geneName",
    title = "Gene Input",
    description = "This is where you enter a gene name to explore"
  )$
  step(
    "explore",
    "Explore gene list",
    "Instead of searching a gene, here you can explore our gene list with scores."
  )


jsCode <- '
  shinyjs.getcookie = function(params) {
    var cookie = Cookies.get("id");
    if (typeof cookie !== "undefined") {
      Shiny.onInputChange("jscookie", cookie);
    }
  }
  shinyjs.setcookie = function(params) {
    Cookies.set("id", escape(params), { expires: 0.5 });
    Shiny.onInputChange("jscookie", params);
  }
  shinyjs.rmcookie = function(params) {
    Cookies.remove("id");
    Shiny.onInputChange("jscookie", "");
  }
  shinyjs.reload = function() {
    history.go(0);
  }
'