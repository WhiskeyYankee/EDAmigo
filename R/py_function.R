#' Title
#'
#' @return
#' @export
#'
#' @examples
py_function = function(){
  reticulate::source_python(system.file("python/my_function.py", package = "EDAmigo"))
  my_function()
}
