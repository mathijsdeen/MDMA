#' Retrieve Unicode character for a currency
#'
#' @param currency character string. Supported values are accounting sign, afghani,
#' armenian dram, austral, baht, bitcoin, boliviano, cent, cedi, currency, dollar,
#' dong, drachma, dutch guilder, euro, franc, georgian lari, german penny, hryvnia,
#' indian rupee, iranian rial, kip, lari, lira, livre tournois, manat, mark, new shekel,
#' pakistani rupee, peso, pound, quetzal, real, rial, ruble, shekel, spesmilo, syrian pound,
#' tenge, tugrik, turkish lira, won, yen, yuan.
#' @param ... other options (for troubleshooting only).
#'
#' @return \code{currency2unicode} the unicode character for a given currency.
#' @details
#' The input is evaluated case insensitive. In case the input is not supported, the function
#' will return the original input.
#'
#' @export
#'
#' @examples
#' currency2unicode("dollar")
#'
#' cat(sprintf("%s5 is all my mom allows me to spend.", currency2unicode("dollar")))
#'
#' @author Mathijs Deen
currency2unicode <- function(currency, ...) {
  if(!is.character(currency) | length(currency) != 1) stop("Currently, you can only have 1 currency input.")
  currencyLC <- tolower(currency)
  currency_map <- list(
    "accounting sign"  = "\u20A1",       # ₡
    "afghani"          = "\u060B",       # ؋
    "armenian dram"    = "\u058F",       # ֏
    "austral"          = "\u20B3",       # ₳ (historical)
    "baht"             = "\u0E3F",       # ฿
    "bitcoin"          = "\u20BF",       # ₿
    "boliviano"        = "\u20B2",       # ₲
    "cent"             = "\u00A2",       # ¢
    "cedi"             = "\u20B5",       # ₵
    "currency"         = "\u00A4",       # ¤ (generic)
    "dollar"           = "\u0024",       # $
    "dong"             = "\u20AB",       # ₫
    "drachma"          = "\u20AF",       # ₯ (historical)
    "dutch guilder"    = "\u0192",       # ƒ (historical)
    "euro"             = "\u20AC",       # €
    "franc"            = "\u20A3",       # ₣ (historical)
    "georgian lari"    = "\u20BE",       # ₾
    "german penny"     = "\u20B0",       # ₰ (historical)
    "hryvnia"          = "\u20B4",       # ₴
    "indian rupee"     = "\u20B9",       # ₹
    "iranian rial"     = "\uFDFC",       # ﷼
    "kip"              = "\u20AD",       # ₭
    "lari"             = "\u20BE",       # ₾
    "lira"             = "\u20A4",       # ₤ (historical)
    "livre tournois"   = "\u20B6",       # ₶ (historical)
    "manat"            = "\u20BC",       # ₼
    "mark"             = "\u2133",       # ℳ (historical)
    "new shekel"       = "\u20AA",       # ₪
    "pakistani rupee"  = "\u20A8",       # ₨
    "peso"             = "\u20B1",       # ₱
    "pound"            = "\u00A3",       # £
    "quetzal"          = "\u20B1",       # ₱
    "real"             = "\u20B4",       # ₴
    "rial"             = "\uFDFC",       # ﷼
    "ruble"            = "\u20BD",       # ₽
    "shekel"           = "\u20AA",       # ₪
    "spesmilo"         = "\u20B7",       # ₷ (historical)
    "syrian pound"     = "\u20A3",       # ₣
    "tenge"            = "\u20B8",       # ₸
    "tugrik"           = "\u20AE",       # ₮
    "turkish lira"     = "\u20BA",       # ₺
    "won"              = "\u20A9",       # ₩
    "yen"              = "\u00A5",       # ¥
    "yuan"             = "\u5143"        # 元
  )

  if(hasArg("returnSupportedCurrencies")) return(paste(names(currency_map), collapse = ", "))
  if(currencyLC %in% names(currency_map)) {
    return(currency_map[[currencyLC]])
  } else {
    #stop("Unknown currency. Available are: ", paste(names(currency_map), collapse = ", "))
    #warning(sprintf("'%s' is not a supported currency, returning value '%s' instead.", currency, currency),call. = FALSE)
    return(currency)
  }
}
